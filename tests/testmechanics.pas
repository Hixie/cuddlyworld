{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics;

interface

//{$DEFINE VERBOSE}

uses
   sysutils, storable, matcher, physics, player, locations, things, grammarian, cuddlycamp, world, threshold;

type
   TExpectationKind = (ekString, ekSubstring, ekNoSubstring, ekSkip, ekDisconnected, ekRecordingStart);

   PExpectation = ^TExpectation;
   TExpectation = record
      Kind: TExpectationKind;
      Target: UTF8String;
      AdvanceWhenFound: Boolean;
      SkipUntilFound: Boolean;
      AlsoCheckNext: Boolean;
   end;

   TTestProxy = class
    private
      FPosition: Cardinal;
      FExpectations: array of TExpectation;
      FRecording: Boolean;
      FTest: UTF8String;
      function AddExpectation(): PExpectation;
      procedure Next();
      procedure HandleAnything(Debug: UTF8String);
      procedure HandleLine(Message: UTF8String);
    public
      procedure WaitUntilString(Message: UTF8String);
      procedure WaitUntilSubstring(Message: UTF8String);
      procedure SkipLine();
      procedure SkipEverything();
      procedure StopSkipping();
      procedure ExpectString(Message: UTF8String);
      procedure ExpectSubstring(Message: UTF8String);
      procedure ExpectNoSubstring(Message: UTF8String);
      procedure ExpectDisconnect(Eventually: Boolean);
      procedure AndAlso();
      procedure StartRecording();
      procedure ExpectRecorded();
      procedure HandleAvatarMessage(Message: UTF8String);
      procedure HandleForceDisconnect();
      procedure ExpectDone();
      procedure Clear();
      procedure Test(Name: UTF8String);
   end;

   ETestError = class(Exception)
   end;

   TTestWorld = class(TWorld)
     FStartLocation: TAtom;
     procedure AddPlayer(Player: TPlayer); override;
     procedure Perform(Command: UTF8String; Player: TPlayer); override;
   end;

type
   TInitWorld = function(): TWorld is nested;
   TRunWorld = procedure (TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy) is nested;

procedure RunMechanicsHarness(InitTest: TInitWorld; RunTest: TRunWorld; SaveWorld: Boolean);

implementation

function TTestProxy.AddExpectation(): PExpectation;
begin
   SetLength(FExpectations, Length(FExpectations)+1);
   Result := @FExpectations[Length(FExpectations)-1];
   Assert(Result^.Kind = ekString);
   Assert(Result^.Target = '');
   Assert(Result^.AdvanceWhenFound = False);
   Assert(Result^.SkipUntilFound = False);
   Assert(Result^.AlsoCheckNext = False);
   Result^.AdvanceWhenFound := True;
end;

procedure TTestProxy.Next();
begin
   if (FExpectations[FPosition].AdvanceWhenFound) then
   begin
      Inc(FPosition);
      if (FPosition >= Length(FExpectations)) then
         Clear();
   end;
end;

procedure TTestProxy.HandleAnything(Debug: UTF8String);
begin
   if (FPosition >= Length(FExpectations)) then
      raise ETestError.Create('Failed in test ' + FTest + ': expected nothing but got ' + Debug + '.');
end;

procedure TTestProxy.HandleLine(Message: UTF8String);
var
   Found, Done: Boolean;
   Error: UTF8String;
begin
{$IFDEF VERBOSE}   Writeln('# ' + Message); {$ENDIF}
   repeat
      HandleAnything('line "' + Message + '"');
      if (FRecording) then
         ExpectString(Message);
      Found := False;
      case FExpectations[FPosition].Kind of
        ekString: begin
           if (FExpectations[FPosition].Target = Message) then
              Found := True
           else
              Error := 'expected "' + FExpectations[FPosition].Target + '"' + #10 + ' but got "' + Message + '"';
        end;
        ekSubstring: begin
           if (Pos(FExpectations[FPosition].Target, Message) > 0) then
              Found := True
           else
              Error := 'expected "' + FExpectations[FPosition].Target + '" substring but got only "' + Message + '"';
        end;
        ekNoSubstring: begin
           if (Pos(FExpectations[FPosition].Target, Message) <= 0) then
              Found := True
           else
              Error := 'expected to not find "' + FExpectations[FPosition].Target + '" substring but got "' + Message + '"';
        end;
        ekSkip: Found := True;
        ekDisconnected: Error := 'expected to be disconnected but got "' + Message + '"';
        ekRecordingStart: Error := 'expected to start seeing a previously recorded session but got "' + Message + '"';
      end;
      if (not Found) then
      begin
         if (FExpectations[FPosition].SkipUntilFound) then
         begin
//{$IFDEF VERBOSE}            Writeln('not testing the validity of: ' + Message); {$ENDIF}
            Exit;
         end;
         raise ETestError.Create('Failed in test ' + FTest + ':' + #10 + Error + '.');
      end;
      Done := not FExpectations[FPosition].AlsoCheckNext;
      Next();
   until Done;
end;

procedure TTestProxy.WaitUntilString(Message: UTF8String);
begin
   with AddExpectation()^ do
   begin
      Target := Message;
      SkipUntilFound := True;
   end;
end;

procedure TTestProxy.WaitUntilSubstring(Message: UTF8String);
begin
   with AddExpectation()^ do
   begin
      Target := Message;
      Kind := ekSubstring;
      SkipUntilFound := True;
   end;
end;

procedure TTestProxy.SkipLine();
begin
   with AddExpectation()^ do
   begin
      Kind := ekSkip;
      Target := '(skip one line)';
   end;
end;

procedure TTestProxy.SkipEverything();
begin
   with AddExpectation()^ do
   begin
      Kind := ekSkip;
      Target := '(skip everything)';
      AdvanceWhenFound := False;
   end;
end;

procedure TTestProxy.StopSkipping();
begin
   if ((FPosition >= Length(FExpectations)) or
       (FExpectations[FPosition].Kind <> ekSkip) or
       (FExpectations[FPosition].Target <> '(skip everything)') or
       (FExpectations[FPosition].AdvanceWhenFound <> False) or
       (FExpectations[FPosition].SkipUntilFound <> False)) then
   begin
      {$IFDEF VERBOSE}
      Writeln('expectation kind was ', FExpectations[FPosition].Kind);
      Writeln('target was ', FExpectations[FPosition].Target);
      Writeln('advance was ', FExpectations[FPosition].AdvanceWhenFound);
      Writeln('skip was ', FExpectations[FPosition].SkipUntilFound);
      {$ENDIF}
      raise ETestError.Create('Failed in test ' + FTest + ': wasn''t skipping when told to stop skipping.');
   end;
   FExpectations[FPosition].AdvanceWhenFound := True;
   Next();
end;

procedure TTestProxy.ExpectString(Message: UTF8String);
begin
   with AddExpectation()^ do
   begin
      Target := Message;
   end;
end;

procedure TTestProxy.ExpectSubstring(Message: UTF8String);
begin
   with AddExpectation()^ do
   begin
      Kind := ekSubstring;
      Target := Message;
   end;
end;

procedure TTestProxy.ExpectNoSubstring(Message: UTF8String);
begin
   with AddExpectation()^ do
   begin
      Kind := ekNoSubstring;
      Target := Message;
   end;
end;

procedure TTestProxy.ExpectDisconnect(Eventually: Boolean);
begin
   with AddExpectation()^ do
   begin
      Kind := ekDisconnected;
      Target := '(disconnected)';
      SkipUntilFound := Eventually;
   end;
end;

procedure TTestProxy.AndAlso();
begin
   Assert(Length(FExpectations) > 0);
   FExpectations[Length(FExpectations)-1].AlsoCheckNext := True;
end;

procedure TTestProxy.StartRecording();
begin
   with AddExpectation()^ do
   begin
      Kind := ekRecordingStart;
      Target := '(recording start)';
   end;
   FRecording := True;
end;

procedure TTestProxy.ExpectRecorded();
begin
   Assert(FRecording);
   FRecording := False;
   HandleAnything('told to start expecting recording');
   if (not (FExpectations[FPosition].Kind = ekRecordingStart)) then
   begin
      raise ETestError.Create('Failed in test ' + FTest + ': expected "' + FExpectations[FPosition].Target + '" but was told to start expecting recording.');
   end;
   Next();
end;

procedure TTestProxy.HandleAvatarMessage(Message: UTF8String);
var
   A, B: Cardinal;
begin
   A := 1;
   for B := 1 to Length(Message) do // $R-
   begin
      if (Message[B] = #10) then
      begin
         HandleLine(Message[A..B-1]);
         A := B+1; // $R-
      end;
   end;
   if (A <= Length(Message)) then
      HandleLine(Message[A..Length(Message)])
   else
      HandleLine('');
end;

procedure TTestProxy.HandleForceDisconnect();
begin
   HandleAnything('disconnected');
   if (not (FExpectations[FPosition].Kind = ekDisconnected)) then
   begin
      if (FExpectations[FPosition].SkipUntilFound) then
         Exit;
      raise ETestError.Create('Failed in test ' + FTest + ': expected "' + FExpectations[FPosition].Target + '" but got disconnected.');
   end;
   Next();
end;

procedure TTestProxy.ExpectDone();
begin
   if (FPosition < Length(FExpectations)) then
      raise ETestError.Create('Failed in test ' + FTest + ': expected "' + FExpectations[FPosition].Target + '" but got nothing.');
end;

procedure TTestProxy.Clear();
begin
   SetLength(FExpectations, 0);
   FPosition := 0;
end;

procedure TTestProxy.Test(Name: UTF8String);
begin
   FTest := Name;
{$IFDEF VERBOSE}   Writeln('* ' + FTest); {$ENDIF}
end;


procedure TTestWorld.AddPlayer(Player: TPlayer);
begin
   FStartLocation.Add(Player, tpOn);
   inherited;
end;

procedure TTestWorld.Perform(Command: UTF8String; Player: TPlayer);
begin
{$IFDEF VERBOSE}   Writeln('# > ' + Message); {$ENDIF}
   inherited;
end;

procedure RunMechanicsHarness(InitTest: TInitWorld; RunTest: TRunWorld; SaveWorld: Boolean);
var
   TestWorld: TWorld;
   TestPlayer: TPlayer;
   Proxy: TTestProxy;
   Failed: Boolean;
begin
   Proxy := TTestProxy.Create();
   if (SaveWorld) then
   begin
      TestWorld := InitTest();
      TestWorld.AddPlayer(TPlayer.Create('Flathead', 'zorkmid', pHe));
      StoreObjectToFile(kWorldFileName, TestWorld, kSaveDataVersion);
      TestWorld.Free();
      Writeln('** SAVED TEST WORLD TO ', kWorldFileName, '! Log in as "Flathead", password "zorkmid".');
   end;
   TestWorld := InitTest();
   Failed := False;
   RaiseMaxFrameCount := maxint;
   try
      try
         TestPlayer := TPlayer.Create('Tester', '', pIt);
         TestPlayer.Adopt(@Proxy.HandleAvatarMessage, @Proxy.HandleForceDisconnect);
         TestWorld.AddPlayer(TestPlayer);
         TestPlayer.AnnounceAppearance();
         RunTest(TestWorld, TestPlayer, Proxy);
         Proxy.ExpectDone();
      except
         on E: ETestError do
         begin
            Writeln('TEST ERROR');
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            Writeln('ABORTING');
            Proxy.Clear();
            Failed := True;
         end;
         on E: Exception do
         begin
            Writeln('UNEXPECTED INTERNAL ERROR');
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            Writeln('RERAISING');
            raise;
         end;
      end;
      try
         Proxy.ExpectDisconnect(True);
      except
         on E: ETestError do
         begin
            if (not Failed) then
            begin
               Writeln('TEST ERROR AT END');
               Writeln(E.Message);
               DumpExceptionBackTrace(Output);
               Writeln('ABORTING');
               Failed := True;
            end;
         end;
         on E: Exception do
         begin
            Writeln('UNEXPECTED INTERNAL ERROR');
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            Writeln('RERAISING');
            raise;
         end;
      end;
   finally
      try
         EmptyDisposalQueue();
         TestWorld.Free();
         Proxy.Free();
      except
         on E: Exception do
         begin
            Writeln('UNEXPECTED INTERNAL ERROR WHILE RELEASING MEMORY');
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            Writeln('IGNORING ERROR');
         end;
      end;
   end;
   if (Failed) then
   begin
      Writeln('FAILURE DETECTED');
      Halt(1);
   end;
end;

end.