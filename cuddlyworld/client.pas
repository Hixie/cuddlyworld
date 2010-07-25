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
      constructor Create(AListener: TListenerSocket; AOrigin: AnsiString; AHostName: AnsiString; APort: Word; AResource: AnsiString; AWorld: TWorld);
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
      constructor Create(APort: Word; AOrigin: AnsiString; AHostName: AnsiString; AResource: AnsiString; AWorld: TWorld);
   end;

implementation

uses
   sysutils;

constructor TCuddlyWorldClient.Create(AListener: TListenerSocket; AOrigin: AnsiString; AHostName: AnsiString; APort: Word; AResource: AnsiString; AWorld: TWorld);
begin
   inherited Create(AListener, AOrigin, AHostName, APort, AResource);
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
            WriteFrame('Welcome back to Cuddly World, ' + FUsername + '!');
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
         FPlayer := TPlayer.Create(FUsername, FPassword, gOrb);
         FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
         FWorld.AddPlayer(FPlayer); { this puts it into the world }
         FPlayer.AnnounceAppearance();
         WriteFrame('Welcome to Cuddly World, ' + FUsername + '!');
         WriteFrame('');
         FPlayer.DoLook();
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


constructor TCuddlyWorldServer.Create(APort: Word; AOrigin: AnsiString; AHostName: AnsiString; AResource: AnsiString; AWorld: TWorld);
begin
   inherited Create(APort);
   FOrigin := AOrigin;
   FHostName := AHostName;
   FResource := AResource;
   FWorld := AWorld;
end;

function TCuddlyWorldServer.CreateNetworkSocket(AListenerSocket: TListenerSocket): TNetworkSocket;
begin
   Result := TCuddlyWorldClient.Create(AListenerSocket, FOrigin, FHostName, FPort, FResource, FWorld);
end;

end.
