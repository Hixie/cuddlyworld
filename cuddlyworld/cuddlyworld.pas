{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
program cuddlyworld;
uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils, baseunix, client, network, exceptions,
   storable, world, locations,
   {$IFDEF DEBUG} player, {$ENDIF}
   cuddlycamp;

type
   TMain = class
      constructor Create();
      destructor Destroy(); override;
      procedure Run();
      procedure ReportException(E: Exception);
    protected
      FWorld: TWorld;
      FServer: TCuddlyWorldServer;
      FOldReportExceptionMethod: TReportExceptionEvent;
      procedure InitialiseWorld();
      procedure SaveWorld();
      {$IFDEF DEBUG}
      procedure ReportStatus(Perspective: TAvatar);
      {$ENDIF}
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
   FServer := TCuddlyWorldServer.Create(10000, 'http://software.hixie.ch', 'ws://damowmow.com:10000/cuddlyworld', FWorld);
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
end;

destructor TMain.Destroy();
begin
   {$IFDEF DEBUG}
   player.StatusReport := nil;
   {$ENDIF}
   FServer.Free();
   FWorld.Free();
   SetReportExceptionMethod(FOldReportExceptionMethod);
   inherited;
end;

procedure TMain.Run();
begin
   Writeln('CuddlyWorld running...');
   repeat
      FServer.Select(timeoutForever);
      FWorld.CheckForDisconnectedPlayers();
      FWorld.CheckDisposalQueue();
      SaveWorld();
   until Aborted;
   Writeln('CuddlyWorld aborted');
end;

procedure TMain.ReportException(E: Exception);
begin
   Writeln(E.Message);
   Dump_Stack(Output, Get_Frame);
end;

{$IFDEF DEBUG}
procedure TMain.ReportStatus(Perspective: TAvatar);
begin
   Perspective.AvatarMessage('Players: ' + IntToStr(FWorld.GetPlayerCount()));
end;
{$ENDIF}

begin
   try
      with (TMain.Create()) do
      begin
         try
            Run();
         finally
            Free();
         end;
      end;
   except
     on E: Exception do
     begin
        Writeln(E.Message);
        DumpExceptionBackTrace(Output);
     end;
   end;
end.