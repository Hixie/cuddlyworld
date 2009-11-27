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
      procedure Handshake(); override;
    public
      constructor Create(AListener: TListenerSocket; AOrigin: String; ALocation: String; AWorld: TWorld);
      destructor Destroy(); override;
    end;

   TCuddlyWorldServer = class(TNetworkServer)
    protected
      FOrigin: String;
      FLocation: String;
      FWorld: TWorld;
      function CreateNetworkSocket(AListenerSocket: TListenerSocket): TNetworkSocket; override;
    public
      constructor Create(APort: Word; AOrigin: String; ALocation: String; AWorld: TWorld);
   end;

implementation

uses
   sysutils;

constructor TCuddlyWorldClient.Create(AListener: TListenerSocket; AOrigin: String; ALocation: String; AWorld: TWorld);
begin
   inherited Create(AListener, AOrigin, ALocation);
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
   WriteFrame('> ' + Message);
   try
      FPlayer.Perform(Message);
   except
      on E: EExternal do raise;
      on E: Exception do
      begin // for debugging
         WriteFrame('You feel a disturbance in the force that whispers "' + E.Message + '".');
         Writeln('');
         Writeln('While processing "' + Message + '", the following exception was raised:');
         Writeln(E.Message);
         DumpExceptionBackTrace(Output);
      end;
   end;
   Result := True;
end;

procedure TCuddlyWorldClient.HandleAvatarMessage(Message: AnsiString);
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

procedure TCuddlyWorldClient.Handshake();
var
   PotentialPlayer: TPlayer;
begin
   inherited;
   try
      PotentialPlayer := FWorld.GetPlayer(FUsername) as TPlayer;
      if (Assigned(PotentialPlayer)) then
      begin
         if (PotentialPlayer.GetPassword() = FPassword) then
         begin
            FPlayer := PotentialPlayer;
            FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
            WriteFrame('Welcome back to CuddlyWorld, ' + FUsername + '!');
            WriteFrame('');
            FPlayer.Look();
            FPlayer.Inventory();
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
         FPlayer := TPlayer.Create(FUsername, FPassword);
         FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
         FWorld.AddPlayer(FPlayer); { this puts it into the world }
         FPlayer.AnnounceAppearance();
         WriteFrame('Welcome to CuddlyWorld, ' + FUsername + '!');
         WriteFrame('');
         FPlayer.Look();
         WriteFrame('');
      end;
   except
      on E: EExternal do raise;
      on E: Exception do
      begin // for debugging
         WriteFrame('You feel a disturbance in the force that whispers "' + E.Message + '".');
         Writeln('');
         Writeln('While connecting user "' + FUsername + '", the following exception was raised:');
         Writeln(E.Message);
         DumpExceptionBackTrace(Output);
      end;
   end;
end;


constructor TCuddlyWorldServer.Create(APort: Word; AOrigin: String; ALocation: String; AWorld: TWorld);
begin
   inherited Create(APort);
   FOrigin := AOrigin;
   FLocation := ALocation;
   FWorld := AWorld;
end;

function TCuddlyWorldServer.CreateNetworkSocket(AListenerSocket: TListenerSocket): TNetworkSocket;
begin
   Result := TCuddlyWorldClient.Create(AListenerSocket, FOrigin, FLocation, FWorld);
end;

end.
