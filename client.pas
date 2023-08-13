{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit client;

interface

uses
   corenetwork, corewebsocket, world, player;

type
   TCuddlyWorldClient = class(TWebSocket)
    protected
      FPlayer: TPlayer;
      FWorld: TWorld;
      procedure HandleMessage(Message: UTF8String); override;
      procedure HandleAvatarMessage(Message: UTF8String);
      procedure HandleForceDisconnect();
      procedure TryLogin(Message: UTF8String);
      procedure TryCommand(Message: UTF8String);
    public
      constructor Create(AListener: TListenerSocket; AWorld: TWorld);
      destructor Destroy(); override;
    end;

   TCuddlyWorldServer = class(TNetworkServer)
    protected
      FOrigin: UTF8String;
      FHostName: UTF8String;
      FResource: UTF8String;
      FWorld: TWorld;
      function CreateNetworkSocket(AListenerSocket: TListenerSocket): TNetworkSocket; override;
    public
      constructor Create(APort: Word; AWorld: TWorld);
   end;

{$IFOPT C+}
procedure TestClient();
{$ENDIF}

implementation

uses
   sysutils;

const
   ResponseFrameHeader = #$01; // first byte of sequences of frames that represent responses; first frame is echo.
   EndOfResponse = #$02; // contents of trailing frame after response

constructor TCuddlyWorldClient.Create(AListener: TListenerSocket; AWorld: TWorld);
begin
   inherited Create(AListener);
   FWorld := AWorld;
end;

destructor TCuddlyWorldClient.Destroy();
begin
   if (Assigned(FPlayer)) then
      FPlayer.Abandon();
   inherited;
end;

procedure TCuddlyWorldClient.HandleMessage(Message: UTF8String);
begin
   if (not Assigned(FPlayer)) then
      TryLogin(Message)
   else
      TryCommand(Message);
end;

procedure TCuddlyWorldClient.TryLogin(Message: UTF8String);
var
   PotentialPlayer: TPlayer;
   Index: Cardinal;
   Username, Password: UTF8String;
begin
   Index := Pos(' ', Message); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   if (Index = 0) then
   begin
      WriteFrame('First frame must be username and password, separated by a space character.');
      Disconnect();
      Exit;
   end;
   Username := Copy(Message, 1, Index-1);
   if (Username = '') then
   begin
      WriteFrame('Username cannot be blank.');
      Disconnect();
      Exit;
   end;
   Password := Copy(Message, Index+1, Length(Message)-Index);
   try
      PotentialPlayer := FWorld.GetPlayer(Username) as TPlayer;
      if (Assigned(PotentialPlayer)) then
      begin
         if (PotentialPlayer.GetPassword() = Password) then
         begin
            FPlayer := PotentialPlayer;
            FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
            WriteFrame('Welcome back to Cuddly World, ' + Username + '!');
            WriteFrame('');
            FPlayer.DoLook();
            FPlayer.DoInventory();
            WriteFrame('');
         end
         else
         begin
            WriteFrame('Wrong password.');
            Disconnect();
            Exit;
         end;
      end
      else
      begin
         FPlayer := TPlayer.Create(Username, Password, pSingularThey);
         FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
         FWorld.AddPlayer(FPlayer); { this puts it into the world }
         FPlayer.AnnounceAppearance();
         WriteFrame('Welcome to Cuddly World, ' + Username + '!');
         WriteFrame('');
         FPlayer.DoLook();
         WriteFrame('');
      end;
   except
      on E: EExternal do raise;
      on E: Exception do
      begin // for debugging
         WriteFrame('You feel a disturbance in the force that whispers "' + E.Message + '".');
         WriteFrame('');
         Writeln('');
         Writeln('While connecting user "' + Username + '", the following exception was raised:');
         Writeln(E.Message);
         DumpExceptionBackTrace(Output);
      end;
   end;
end;

procedure TCuddlyWorldClient.TryCommand(Message: UTF8String);
begin
   WriteFrame(ResponseFrameHeader + '> ' + Message);
   try
      FWorld.SetDirty();
      FWorld.Perform(Message, FPlayer);
   except
      on EExternal do raise;
      on E: Exception do
      begin // for debugging
         WriteFrame('You feel a disturbance in the force that whispers "' + E.Message + '".');
         Writeln('');
         Writeln('While processing "' + Message + '", the following exception was raised:');
         Writeln(E.Message);
         DumpExceptionBackTrace(Output);
      end;
   end;
   WriteFrame(EndOfResponse);
end;

procedure TCuddlyWorldClient.HandleAvatarMessage(Message: UTF8String);
begin
   WriteFrame(Message);
end;

procedure TCuddlyWorldClient.HandleForceDisconnect();
begin
   WriteFrame('Switching to new connection...');
   FPlayer.Abandon();
   FPlayer := nil;
   Disconnect();
end;


constructor TCuddlyWorldServer.Create(APort: Word; AWorld: TWorld);
begin
   inherited Create(APort);
   FWorld := AWorld;
end;

function TCuddlyWorldServer.CreateNetworkSocket(AListenerSocket: TListenerSocket): TNetworkSocket;
begin
   Result := TCuddlyWorldClient.Create(AListenerSocket, FWorld);
end;

{$IFOPT C+}
procedure TestClient();
begin
end;
{$ENDIF}

end.
