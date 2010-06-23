{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit network;

interface

uses
   baseunix, unixtype, sockets, exceptions;

const
   timeoutForever = -1;

type
   TBaseSocket = class;
   TListenerSocket = class;

   TNewConnectionEvent = procedure (ListenerSocket: TListenerSocket) of object;
   TDisconnectEvent = procedure (Socket: TBaseSocket) of object;

   TBaseSocket = class
    protected
      FConnected: Boolean;
      FOnDisconnect: TDisconnectEvent;
      FSocketNumber: cint;
    public
      procedure Disconnect(); virtual; abstract;
      function Read(): Boolean; virtual; abstract;
      property Connected: Boolean read FConnected;
      property OnDisconnect: TDisconnectEvent read FOnDisconnect write FOnDisconnect;
   end;

   TListenerSocket = class(TBaseSocket)
    protected
      FOnNewConnection: TNewConnectionEvent;
    public
      constructor Create(APort: Word);
      destructor Destroy(); override;
      procedure Disconnect(); override;
      function Read(): Boolean; override;
      property OnNewConnection: TNewConnectionEvent read FOnNewConnection write FOnNewConnection;
   end;

   TNetworkSocket = class(TBaseSocket)
    protected
      FAddr: TINetSockAddr;
      function InternalRead(c: Byte): Boolean; virtual; abstract;
    public
      constructor Create(AListener: TListenerSocket);
      destructor Destroy(); override;
      procedure Disconnect(); override;
      function Read(): Boolean; override;
      procedure Write(s: AnsiString); { Do not pass the empty string }
      procedure Write(s: array of byte);
   end;

   PSocketListItem = ^TSocketListItem;
   TSocketListItem = record
     Next: PSocketListItem;
     Value: TBaseSocket;
   end;

   TNewSocketConnectionEvent = procedure (Socket: TNetworkSocket) of object;

   TNetworkServer = class
    protected
      FList: PSocketListItem;
      FFileDescriptorSet: TFDSet;
      FMaxSocketNumber: cint;
      FHavePendingDisconnects: Boolean;
      procedure Add(Socket: TBaseSocket);
      procedure Remove(Socket: TBaseSocket);
      procedure Empty();
      procedure HandleNewConnection(ListenerSocket: TListenerSocket);
      procedure HandleDisconnect(Socket: TBaseSocket);
      function CreateNetworkSocket(ListenerSocket: TListenerSocket): TNetworkSocket; virtual; abstract;
    public
      constructor Create(APort: Word);
      destructor Destroy(); override;
      procedure Select(Timeout: cint);
   end;

implementation

uses
   sysutils;

constructor TListenerSocket.Create(APort: Word);
var
   Addr: TINetSockAddr;
begin
   inherited Create();
   FSocketNumber := fpSocket(AF_INET, SOCK_STREAM, 0);
   if (FSocketNumber < 0) then
      raise ESocketError.Create(SocketError);
   Addr.sin_family := AF_INET;
   Addr.sin_port := htons(APort);
   Addr.sin_addr.s_addr := htonl(INADDR_ANY);
   if (fpBind(FSocketNumber, @Addr, SizeOf(Addr)) <> 0) then
      raise ESocketError.Create(SocketError);
   if (fpListen(FSocketNumber, 32) <> 0) then // allow up to 32 pending connections at once
      raise ESocketError.Create(SocketError);
   FConnected := True;
end;

destructor TListenerSocket.Destroy();
begin
   Assert(not FConnected);
   inherited;
end;

procedure TListenerSocket.Disconnect();
begin
   if (FConnected) then
   begin
      if (Assigned(FOnDisconnect)) then
         FOnDisconnect(Self);
      if (fpClose(FSocketNumber) <> 0) then
         raise EKernelError.Create(fpGetErrNo);
   end;
   FConnected := False;
end;

function TListenerSocket.Read(): Boolean;
begin
   Assert(FConnected);
   Assert(Assigned(FOnNewConnection));
   FOnNewConnection(Self);
   Result := True;
end;


constructor TNetworkSocket.Create(AListener: TListenerSocket);
var
   Options: cint;
   AddrLen: PSockLen;
begin
   inherited Create();
   New(AddrLen);
   AddrLen^ := SizeOf(FAddr);
   FSocketNumber := fpAccept(AListener.FSocketNumber, @FAddr, AddrLen);
   Dispose(AddrLen);
   if (FSocketNumber < 0) then
      raise ESocketError.Create(SocketError);
   Options := FpFcntl(FSocketNumber, F_GETFL);
   if (Options < 0) then
      raise EKernelError.Create(fpGetErrNo);
   Options := FpFcntl(FSocketNumber, F_SETFL, Options or O_NONBLOCK);
   if (Options < 0) then
      raise EKernelError.Create(fpGetErrNo);
   FConnected := True;
end;

destructor TNetworkSocket.Destroy();
begin
   Assert(not FConnected);
   inherited;
end;

procedure TNetworkSocket.Disconnect();
var
   Error: Integer;
begin
   if (FConnected) then
   begin
      if (Assigned(FOnDisconnect)) then
         FOnDisconnect(Self);
      Error := fpShutdown(FSocketNumber, 2);
      if ((Error <> 0) and (SocketError <> 107)) then // 107 = already disconnected
         raise ESocketError.Create(SocketError);
   end;
   FConnected := False;
end;

function TNetworkSocket.Read(): Boolean;
var
   Data: Byte;
   Received: ssize_t;
begin
   Assert(FConnected);
   Received := fpRecv(FSocketNumber, @Data, SizeOf(Data), 0);
//   if (Received < 0) then
//      raise ESocketError.Create(SocketError);
   Result := (Received = SizeOf(Data)) and InternalRead(Data);
end;

procedure TNetworkSocket.Write(s: AnsiString);
var
   Sent: ssize_t;
begin
   Assert(FConnected);
   Assert(Length(s) > 0);
   Sent := fpSend(FSocketNumber, Pointer(s), Length(s), 0);
   if (Sent < Length(s)) then
      raise ESocketError.Create(SocketError);
end;

procedure TNetworkSocket.Write(s: array of byte);
var
   Sent: ssize_t;
begin
   Assert(FConnected);
   Sent := fpSend(FSocketNumber, @s, Length(s), 0);
   if (Sent < Length(s)) then
      raise ESocketError.Create(SocketError);
end;


constructor TNetworkServer.Create(APort: Word);
var
   Listener: TListenerSocket;
begin
   inherited Create();
   fpFD_ZERO(FFileDescriptorSet);
   Listener := TListenerSocket.Create(APort);
   Listener.OnNewConnection := @Self.HandleNewConnection;
   Add(Listener);
end;

destructor TNetworkServer.Destroy();
begin
   Empty();
   inherited;
end;

procedure TNetworkServer.Add(Socket: TBaseSocket);
var
   Item: PSocketListItem;
begin
   New(Item);
   Item^.Next := FList;
   Item^.Value := Socket;
   FList := Item;
   if (FMaxSocketNumber < Socket.FSocketNumber) then
      FMaxSocketNumber := Socket.FSocketNumber;
   fpFD_SET(Socket.FSocketNumber, FFileDescriptorSet);
   Socket.OnDisconnect := @Self.HandleDisconnect;
end;

procedure TNetworkServer.Remove(Socket: TBaseSocket);
var
   Item: PSocketListItem;
   Last: ^PSocketListItem;
begin
   fpFD_CLR(Socket.FSocketNumber, FFileDescriptorSet);
   Last := @FList;
   Item := FList;
   while (Assigned(Item)) do
   begin
      if (Item^.Value = Socket) then
      begin
         Last^ := Item^.Next;
         Dispose(Item);
         Item := nil;
      end
      else
      begin
         Last := @Item^.Next;
         Item := Item^.Next;
      end;
   end;
end;

procedure TNetworkServer.Empty();
var
   Item: PSocketListItem;
begin
   while (Assigned(FList)) do
   begin
      Item := FList;
      FList := FList^.Next;
      Item^.Value.Disconnect();
      Item^.Value.Destroy();
      Dispose(Item);
   end;
   fpFD_ZERO(FFileDescriptorSet);
end;

procedure TNetworkServer.HandleNewConnection(ListenerSocket: TListenerSocket);
begin
   Add(CreateNetworkSocket(ListenerSocket));
end;

procedure TNetworkServer.HandleDisconnect(Socket: TBaseSocket);
begin
   FHavePendingDisconnects := True;
end;

procedure TNetworkServer.Select(Timeout: cint);
var
   fdsRead, fdsExcept: TFDSet;
   Pending, IsSet: cint;
   Item: PSocketListItem;
   Last: ^PSocketListItem;
   Disconnect: Boolean;
begin
   fdsRead := FFileDescriptorSet;
   fdsExcept := FFileDescriptorSet;
   Pending := fpSelect(FMaxSocketNumber+1, @fdsRead, nil, @fdsExcept, Timeout);
   if (Pending < 0) then
   begin
      case fpGetErrNo of
         ESysEIntr: Exit; { probably received ^C or an alarm }
      else
         raise EKernelError.Create(fpGetErrNo);
      end;
   end;
   Item := FList;
   while ((Pending > 0) and Assigned(Item)) do
   begin
      Disconnect := False;
      IsSet := fpFD_ISSET(Item^.Value.FSocketNumber, fdsRead);
      Assert(IsSet >= 0);
      if (IsSet > 0) then
      begin
         Dec(Pending);
         try
            if (not Item^.Value.Read()) then
               Disconnect := True;
         except
            on E: Exception do
            begin
               Writeln('When reading:');
               ReportException(E);
               Disconnect := True;
            end;
         end;
      end;
      IsSet := fpFD_ISSET(Item^.Value.FSocketNumber, fdsExcept);
      Assert(IsSet >= 0);
      if (IsSet > 0) then
      begin
         Dec(Pending);
         Disconnect := True;
      end;
      if (Disconnect) then
         Item^.Value.Disconnect();
      Item := Item^.Next;
   end;
   Assert(Pending = 0);
   if (FHavePendingDisconnects) then
   begin
      Last := @FList;
      Item := FList;
      while (Assigned(Item)) do
      begin
         if (not Item^.Value.Connected) then
         begin
            Assert(Last^ = Item);
            fpFD_CLR(Item^.Value.FSocketNumber, FFileDescriptorSet);
            Item^.Value.Destroy();
            Last^ := Item^.Next;
            Dispose(Item);
            Item := Last^;
         end
         else
         begin
            Last := @Item^.Next;
            Item := Item^.Next;
         end;
      end;
      FHavePendingDisconnects := False;
   end;
end;

end.