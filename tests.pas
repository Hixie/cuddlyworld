{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
program tests;
uses
   sysutils, storable, world, player, cuddlycamp;

type
   TExpectationKind = (ekString, ekSubstring, ekNoSubstring, ekSkip, ekDisconnected, ekRecordingStart);

   PExpectation = ^TExpectation;
   TExpectation = record
      Kind: TExpectationKind;
      Target: AnsiString;
      AdvanceWhenFound: Boolean;
      SkipUntilFound: Boolean;
   end;

   TTestProxy = class
    private
      FPosition: Cardinal;
      FExpectations: array of TExpectation;
      FRecording: Boolean;
      FTest: AnsiString;
      function AddExpectation(): PExpectation;
      procedure Next();
      procedure HandleAnything(Debug: AnsiString);
      procedure HandleLine(Message: AnsiString);
    public
      procedure WaitUntilString(Message: AnsiString);
      procedure SkipLine();
      procedure SkipEverything();
      procedure StopSkipping();
      procedure ExpectString(Message: AnsiString);
      procedure ExpectSubstring(Message: AnsiString);
      procedure ExpectNoSubstring(Message: AnsiString);
      procedure ExpectDisconnect(Eventually: Boolean);
      procedure StartRecording();
      procedure ExpectRecorded();
      procedure HandleAvatarMessage(Message: AnsiString);
      procedure HandleForceDisconnect();
      procedure Clear();
      procedure Test(Name: AnsiString);
   end;

   ETestError = class(Exception)
   end;

function TTestProxy.AddExpectation(): PExpectation;
begin
   SetLength(FExpectations, Length(FExpectations)+1);
   Result := @FExpectations[Length(FExpectations)-1];
   Assert(Result^.Kind = ekString);
   Assert(Result^.Target = '');
   Assert(Result^.AdvanceWhenFound = False);
   Assert(Result^.SkipUntilFound = False);
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

procedure TTestProxy.HandleAnything(Debug: AnsiString);
begin
   if (FPosition >= Length(FExpectations)) then
      raise ETestError.Create('Failed in test ' + FTest + ': expected nothing but got ' + Debug + '.');
end;

procedure TTestProxy.HandleLine(Message: AnsiString);
var
   Found: Boolean;
   Error: AnsiString;
begin
   HandleAnything('line "' + Message + '"');
   if (FRecording) then
      ExpectString(Message);
   Found := False;
   case (FExpectations[FPosition].Kind) of
     ekString: begin
        if (FExpectations[FPosition].Target = Message) then
           Found := True
        else
           Error := 'expected "' + FExpectations[FPosition].Target + '" but got "' + Message + '"';
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
     else Error := 'Unknown expectation';
   end;
   if (not Found) then
   begin
      if (FExpectations[FPosition].SkipUntilFound) then
         Exit;
      raise ETestError.Create('Failed in test ' + FTest + ': ' + Error + '.');
   end;
   Next();
end;

procedure TTestProxy.WaitUntilString(Message: AnsiString);
begin
   with AddExpectation()^ do
   begin
      Target := Message;
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
      raise ETestError.Create('Failed in test ' + FTest + ': wasn''t skipping when told to stop skipping.');
   end;
   FExpectations[FPosition].AdvanceWhenFound := True;
   Next();
end;

procedure TTestProxy.ExpectString(Message: AnsiString);
begin
   with AddExpectation()^ do
   begin
      Target := Message;
   end;
end;

procedure TTestProxy.ExpectSubstring(Message: AnsiString);
begin
   with AddExpectation()^ do
   begin
      Kind := ekSubstring;
      Target := Message;
   end;
end;

procedure TTestProxy.ExpectNoSubstring(Message: AnsiString);
begin
   with AddExpectation()^ do
   begin
      Kind := ekNoSubstring;
      Target := Message;
      AdvanceWhenFound := False;
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

procedure TTestProxy.HandleAvatarMessage(Message: AnsiString);
var
   A, B: Cardinal;
begin
   A := 1;
   for B := 1 to Length(Message) do
   begin
      if (Message[B] = #10) then
      begin
         HandleLine(Message[A..B-1]);
         A := B+1;
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

procedure TTestProxy.Clear();
begin
   SetLength(FExpectations, 0);
   FPosition := 0;
end;

procedure TTestProxy.Test(Name: AnsiString);
begin
   FTest := Name;
end;


var
   TestWorld: TWorld;
   TestPlayer: TPlayer;
   Proxy: TTestProxy;
   Failed: Boolean;
begin
   Writeln('CuddlyWorld Tests initializing...');
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   Proxy := TTestProxy.Create();
   TestWorld := InitEden();
   Failed := False;
   try
      try
         TestPlayer := TPlayer.Create('Tester', '');
         TestPlayer.Adopt(@Proxy.HandleAvatarMessage, @Proxy.HandleForceDisconnect);
         TestWorld.AddPlayer(TestPlayer);
         TestPlayer.AnnounceAppearance();

         // Basic look test
         Proxy.Test('Login');
         Proxy.ExpectString('Camp Cuddlyfort');
         Proxy.SkipEverything();
         Proxy.StartRecording();
         TestPlayer.DoLook();
         Proxy.StopSkipping();
         Proxy.ExpectRecorded();
         Proxy.ExpectString('');
         TestPlayer.Perform('look');

         // Dig and cover test
         Proxy.Test('Digging');
         Proxy.WaitUntilString('');
         Proxy.ExpectString('Foot of Cliff Face');
         Proxy.WaitUntilString('');
         Proxy.ExpectString('(the ground with the spade)');
         Proxy.ExpectString('(first taking the spade)');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('With much effort, you dig a huge hole.');
         Proxy.ExpectString('');
         Proxy.ExpectString('(first taking the balloon)');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('Placed in the hole.');
         Proxy.ExpectString('');
         Proxy.WaitUntilString('');
         Proxy.SkipLine();
         Proxy.ExpectSubstring('On the hole is a pile');
         Proxy.ExpectString('');
         TestPlayer.Perform('move all n; n; dig; put balloon in hole; move all onto hole; x hole');

         Writeln('Tests done successfully.');
      except
         on E: ETestError do
         begin
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            Proxy.Clear();
            Failed := True;
         end;
         on E: Exception do
         begin
            Writeln(E.Message);
            DumpExceptionBackTrace(Output);
            raise;
         end;
      end;
      Proxy.ExpectDisconnect(True);
   finally
      TestWorld.Free();
      Proxy.Free();
   end;
   Writeln('CuddlyWorld Tests complete.');
   if (Failed) then
   begin
      Writeln('FAILURE DETECTED');
      Halt(1);
   end;
end.