{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
program cuddlyworld;
uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils, baseunix, client, corenetwork, exceptions,
   storable,
   physics, world, locations, player,
   cuddlycamp;

type
   TMain = class
      constructor Create();
      destructor Destroy(); override;
      procedure Run(); virtual; abstract;
      procedure ReportException(E: Exception);
    protected
      FWorld: TWorld;
      FOldReportExceptionMethod: TReportExceptionEvent;
      procedure InitialiseWorld();
      procedure SaveWorld();
      {$IFDEF DEBUG}
      procedure ReportStatus(Perspective: TPlayer);
      {$ENDIF}
   end;

   TNetworkMain = class(TMain)
      constructor Create(Port: Word);
      destructor Destroy(); override;
      procedure Run(); override;
    protected
      FServer: TCuddlyWorldServer;
   end;

   TCommandLineMain = class(TMain)
      constructor Create(Username, Password: UTF8String);
      destructor Destroy(); override;
      procedure Run(); override;
      procedure HandleForceDisconnect();
      procedure HandleAvatarMessage(Message: UTF8String);
    protected
      FPlayer: TPlayer;
   end;

var
   Aborted: Boolean = False;

procedure SigIntHandler(Signal: Longint; Info: PSigInfo; Context: PSigContext); cdecl;
begin
   Aborted := True;
end;

constructor TMain.Create();
var
   NewAction: PSigActionRec;
begin
   Writeln('CuddlyWorld initializing...');
   inherited;
   {$IFDEF DEBUG}
   Writeln('CuddlyWorld debugging enabled.');
   player.StatusReport := @Self.ReportStatus;
   {$ENDIF}
   InitialiseWorld();
   New(NewAction);
   if (not Assigned(NewAction)) then
      OutOfMemoryError();
   try
      NewAction^.sa_handler := @SigIntHandler;
      NewAction^.sa_flags := SA_ONESHOT;
      {$ifdef Linux} NewAction^.sa_restorer := nil; {$endif}
      FillByte(NewAction^.sa_mask, SizeOf(NewAction^.sa_mask), 0);
      if (fpSigAction(SIGINT, NewAction, nil) <> 0) then
         raise EKernelError.Create(fpGetErrNo);
   finally
      Dispose(NewAction);
   end;
   FOldReportExceptionMethod := SetReportExceptionMethod(@Self.ReportException);
end;

procedure TMain.InitialiseWorld();
begin
   FWorld := ReadObjectFromFile(kWorldFileName) as TWorld;
   if (not Assigned(FWorld)) then
      raise Exception.Create('World not found. Run genesis to create ' + kWorldFileName + ' before running cuddlyworld.');
end;

procedure TMain.SaveWorld();
begin
   StoreObjectToFile(kWorldFileName, FWorld, kSaveDataVersion);
   FWorld.Saved();
end;

destructor TMain.Destroy();
begin
   {$IFDEF DEBUG}
   player.StatusReport := nil;
   {$ENDIF}
   FWorld.Free();
   SetReportExceptionMethod(FOldReportExceptionMethod);
   inherited;
end;

procedure TMain.ReportException(E: Exception);
begin
   Writeln(E.Message);
   Dump_Stack(Output, Get_Frame);
end;

{$IFDEF DEBUG}
procedure TMain.ReportStatus(Perspective: TPlayer);
begin
   Perspective.SendRawMessage('Players: ' + IntToStr(FWorld.GetPlayerCount()));
end;
{$ENDIF}


constructor TNetworkMain.Create(Port: Word);
begin
   inherited Create();
   FServer := TCuddlyWorldServer.Create(Port, FWorld);
end;

destructor TNetworkMain.Destroy();
begin
   FServer.Free();
   inherited;
end;

procedure TNetworkMain.Run();
begin
   Writeln('CuddlyWorld running...');
   repeat
      FServer.Select(timeoutForever);
      FWorld.CheckForDisconnectedPlayers();
      EmptyDisposalQueue();
      if (FWorld.Dirty) then
         SaveWorld();
   until Aborted;
   Writeln('CuddlyWorld aborted');
end;


constructor TCommandLineMain.Create(Username, Password: UTF8String);
begin
   inherited Create();
   FPlayer := FWorld.GetPlayer(Username) as TPlayer;
   if (Assigned(FPlayer)) then
   begin
      if (FPlayer.GetPassword() = Password) then
      begin
         FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
         Writeln('Logged in as ' + FPlayer.GetDefiniteName(nil) + '.');
         Writeln('');
         FPlayer.DoLook();
         FPlayer.DoInventory();
         Writeln('');
      end
      else
      begin
         raise Exception.Create('Wrong Password');
      end;
   end
   else
   begin
      FPlayer := TPlayer.Create(Username, Password, pSingularThey);
      FPlayer.Adopt(@Self.HandleAvatarMessage, @Self.HandleForceDisconnect);
      FWorld.AddPlayer(FPlayer); { this puts it into the world }
      FPlayer.AnnounceAppearance();
      Writeln('Welcome to CuddlyWorld, ' + FPlayer.GetDefiniteName(nil) + '.');
      Writeln('');
      FPlayer.DoLook();
      Writeln('');
   end;
end;

procedure TCommandLineMain.HandleAvatarMessage(Message: UTF8String);
begin
   Writeln(Message);
end;

procedure TCommandLineMain.HandleForceDisconnect();
begin
end;

destructor TCommandLineMain.Destroy();
begin
   FPlayer.Abandon();
   inherited;
end;

procedure TCommandLineMain.Run();
var
   S: UTF8String;
begin
   repeat
      Write('> ');
      Readln(S);
      FWorld.Perform(S, FPlayer);
      EmptyDisposalQueue();
      SaveWorld();
   until Aborted;
end;

const
   PortVariable = 'CUDDLYWORLDPORT';
   DefaultPort = 10000;

var
   Main: TMain;
   Port, Error: Word;
begin
   {$IFDEF DEBUG}
      RegisterStorableClassSynonym('TTestWorld', TWorld);
   {$ENDIF}
   try
      if (ParamCount() > 0) then
      begin
         if (ParamCount() <> 2) then
         begin
            Writeln('Usage: cuddlyworld [username password]');
            Writeln('Without arguments, runs a WebSocket server on the port given by the environment variable ', PortVariable ,'.');
            Writeln('The default port if the variable is not set, is ', DefaultPort, '.');
            Writeln('With arguments, runs locally in single-user mode using the given username and password.');
         end
         else
         begin
            Main := TCommandLineMain.Create(ParamStr(1), ParamStr(2));
         end;
      end
      else
      begin
         Val(GetEnvironmentVariable(PortVariable), Port, Error);
         if (Error <> 0) then
         begin
            Port := DefaultPort;
         end;
         Main := TNetworkMain.Create(Port);
         Writeln('Server active on port ', Port, '.');
      end;
      try
         Main.Run();
      finally
         Main.Free();
      end;
   except
     on E: Exception do
     begin
        Writeln(E.Message);
        DumpExceptionBackTrace(Output);
     end;
   end;
end.
