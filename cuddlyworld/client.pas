{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit client;

interface

uses
   network, websocket, world, player;

type
   TCuddlyWorldClient = class(TWebSocket)
    protected
      FPlayer: TPlayer;
      FWorld: TWorld;
      function HandleMessage(Message: AnsiString): Boolean; override;
      procedure HandleAvatarMessage(Message: AnsiString);
      procedure HandleForceDisconnect();
      procedure TryLogin(Message: AnsiString);
      procedure TryCommand(Message: AnsiString);
    public
      constructor Create(AListener: TListenerSocket; AWorld: TWorld);
      destructor Destroy(); override;
    end;

   TCuddlyWorldServer = class(TNetworkServer)
    protected
      FOrigin: AnsiString;
      FHostName: AnsiString;
      FResource: AnsiString;
      FWorld: TWorld;
      function CreateNetworkSocket(AListenerSocket: TListenerSocket): TNetworkSocket; override;
    public
      constructor Create(APort: Word; AWorld: TWorld);
   end;

implementation

uses
   sysutils;

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

function TCuddlyWorldClient.HandleMessage(Message: AnsiString): Boolean;
begin
   if (not Assigned(FPlayer)) then
      TryLogin(Message)
   else
      TryCommand(Message);
   Result := True;
end;

procedure TCuddlyWorldClient.TryLogin(Message: AnsiString);
var
   PotentialPlayer: TPlayer;
   Index: Cardinal;
   Username, Password: AnsiString;
begin
   Index := Pos(' ', Message); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   if (Index = 0) then
   begin
      WriteFrame('First frame must be username and password, separated by a space character.');
      Disconnect();
      Exit;
   end;
   Username := Copy(Message, 1, Index-1);
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
         FPlayer := TPlayer.Create(Username, Password, gOrb);
         FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
         FWorld.AddPlayer(FPlayer); { this puts it into the world }
         FPlayer.AnnounceAppearance();
         WriteFrame('Welcome to Cuddly World, ' + Username + '!');
         WriteFrame('');
         FPlayer.DoLook();
         WriteFrame('');
      end;
      FPlayer.ReportFailedCommands := True;
   except
      on E: EExternal do raise;
      on E: Exception do
      begin // for debugging
         WriteFrame('You feel a disturbance in the force that whispers "' + E.Message + '".');
         Writeln('');
         Writeln('While connecting user "' + Username + '", the following exception was raised:');
         Writeln(E.Message);
         DumpExceptionBackTrace(Output);
      end;
   end;
end;

procedure TCuddlyWorldClient.TryCommand(Message: AnsiString);
begin
   WriteFrame('> ' + Message);
   try
      FWorld.SetDirty();
      FPlayer.Perform(Message);
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
end;

procedure TCuddlyWorldClient.HandleAvatarMessage(Message: AnsiString);
begin
   WriteFrame(Message);
end;

procedure TCuddlyWorldClient.HandleForceDisconnect();
begin
   WriteFrame('Switching to new connection...');
   FPlayer.Abandon();
   FPlayer.ReportFailedCommands := False;
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

end.
