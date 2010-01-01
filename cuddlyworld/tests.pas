{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
{-$DEFINE VERBOSE}
program tests;
uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils, storable, world, player, locations, things, thingdim, grammarian, cuddlycamp;

type
   TExpectationKind = (ekString, ekSubstring, ekNoSubstring, ekSkip, ekDisconnected, ekRecordingStart);

   PExpectation = ^TExpectation;
   TExpectation = record
      Kind: TExpectationKind;
      Target: AnsiString;
      AdvanceWhenFound: Boolean;
      SkipUntilFound: Boolean;
      AlsoCheckNext: Boolean;
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
      procedure WaitUntilSubstring(Message: AnsiString);
      procedure SkipLine();
      procedure SkipEverything();
      procedure StopSkipping();
      procedure ExpectString(Message: AnsiString);
      procedure ExpectSubstring(Message: AnsiString);
      procedure ExpectNoSubstring(Message: AnsiString);
      procedure ExpectDisconnect(Eventually: Boolean);
      procedure AndAlso();
      procedure StartRecording();
      procedure ExpectRecorded();
      procedure HandleAvatarMessage(Message: AnsiString);
      procedure HandleForceDisconnect();
      procedure ExpectDone();
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

procedure TTestProxy.HandleAnything(Debug: AnsiString);
begin
   if (FPosition >= Length(FExpectations)) then
      raise ETestError.Create('Failed in test ' + FTest + ': expected nothing but got ' + Debug + '.');
end;

procedure TTestProxy.HandleLine(Message: AnsiString);
var
   Found, Done: Boolean;
   Error: AnsiString;
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
         begin
{$IFDEF VERBOSE}            Writeln('not testing the validity of: ' + Message); {$ENDIF}
            Exit;
         end;
         raise ETestError.Create('Failed in test ' + FTest + ': ' + Error + '.');
      end;
      Done := not FExpectations[FPosition].AlsoCheckNext;
      Next();
   until Done;
end;

procedure TTestProxy.WaitUntilString(Message: AnsiString);
begin
   with AddExpectation()^ do
   begin
      Target := Message;
      SkipUntilFound := True;
   end;
end;

procedure TTestProxy.WaitUntilSubstring(Message: AnsiString);
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

procedure TTestProxy.Test(Name: AnsiString);
begin
   FTest := Name;
   Writeln('* ' + FTest);
end;


type
   TTestPlayer = class(TPlayer)
     procedure Perform(Message: AnsiString); override;
   end;

procedure TTestPlayer.Perform(Message: AnsiString);
begin
{$IFDEF VERBOSE}   Writeln('# > ' + Message); {$ENDIF}
   inherited;
end;


type
   TTestWorld = class(TWorld)
     procedure AddPlayer(Avatar: TAvatar); override;
   end;

procedure TTestWorld.AddPlayer(Avatar: TAvatar);
begin
   FLocations^.Next^.Value.GetSurface().Add(Avatar, tpOn);
   inherited;
end;


procedure TestMechanics();

   function InitTestEden: TWorld;
   var
      World: TTestWorld;
      Camp, Cliff: TLocation;
      CampMountain, CampForest: TThing;
      CliffMountain, CliffForest, CliffCamp, CliffCliff: TThing;
   begin
      World := TTestWorld.Create();

      { Locations }
      Camp := TFeaturelessOutdoorLocation.Create('Camp Cuddlyfort', 'Camp Cuddlyfort', 'a camp', 'This is a camp nestled in a forest, under the shadow of a mountain to the north.');
      Cliff := TFeaturelessOutdoorLocation.Create('Foot of Cliff Face', 'the foot of the cliff face', 'a foot of a cliff face', 'The south side of a mountain rises out of the ground here, in a clear and well-defined way, as if to say "this far, no farther" to an enemy whose nature you cannot fathom. ' + 'The cliff is a sheer rock face, essentially unclimbable. Conspicuous is the absence of any vegetation anywhere on the cliff, at least as far as you can see. At the base of the cliff to the east and west is a dense forest.');

      { Camp }
      CampMountain := TDistantScenery.Create('mountain', cdNorth);
      Camp.Add(CampMountain, tpAroundImplicit);
      CampForest := TScenery.Create(['forest', 'trees', 'tree', 'of'], 'the forest of trees', 'The forest is dense and impassable.');
      Camp.Add(CampForest, tpAroundImplicit);
      Camp.GetSurface().Add(TStaticThing.Create('MacGuffin', 'The MacGuffin displays outward signs of being avian in nature.', tmHeavy, tsBig), tpOn);
      Camp.GetSurface().Add(TStaticThing.Create('penny', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);
      Camp.GetSurface().Add(TPile.Create(['leaves', 'leaf'], 'It appears someone has collected fallen leaves from the forest. Possibly the entire forest, given how big the pile is.', tmLight, tsGigantic), tpOn);
      Camp.ConnectCardinals(Cliff, CampForest, CampForest, CampForest);
      Camp.ConnectDiagonals(CampForest, CampForest, CampForest, CampForest);
      World.AddLocation(Camp);

      { Cliff }
      CliffMountain := TScenery.Create('mountain', 'From here you cannot get a good sense of the size of the mountain. Its cliff face dominates your view.');
      (CliffMountain as TScenery).FindDescription := 'The mountain towers high above you.';
      Cliff.Add(CliffMountain, tpAtImplicit);
      CliffCliff := TScenery.Create('cliff', 'The cliff consists of a sheer rock face.');
      CliffMountain.Add(CliffCliff, tpPartOfImplicit);
      CliffForest := TScenery.Create(['forest', 'trees', 'tree', 'of'], 'the forest of trees', 'The forest is dense and impassable.');
      Cliff.Add(CliffForest, tpAroundImplicit);
      CliffCamp := TDistantScenery.Create('camp', cdSouth);
      Cliff.Add(CliffCamp, tpAroundImplicit);
      Cliff.GetSurface().Add(TSpade.Create(), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large red balloon', 'huge', 'massive'], 'the large red balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured red.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large orange balloon', 'huge', 'massive'], 'the large orange balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured orange.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large yellow balloon', 'huge', 'massive'], 'the large yellow balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured yellow.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large green balloon', 'huge', 'massive'], 'the large green balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured green.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large blue balloon', 'huge', 'massive'], 'the large blue balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured blue.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large violet balloon', 'purple', 'huge', 'massive'], 'the large violet balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured violet.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large white balloon', 'huge', 'massive'], 'the large white balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured white.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large black balloon', 'huge', 'massive'], 'the large black balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured black.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large grey balloon', 'gray', 'huge', 'massive'], 'the large grey balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured grey.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create(['large pink balloon', 'huge', 'massive'], 'the large pink balloon', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured pink.', tmLight, tsMassive), tpOn);
      Cliff.ConnectCardinals(CliffCliff, CliffForest, Camp, CliffForest);
      Cliff.ConnectDiagonals(nil, CliffForest, CliffForest, nil);
      World.AddLocation(Cliff);

      Result := World;
   end;

var
   TestWorld: TWorld;
   TestPlayer: TPlayer;
   Proxy: TTestProxy;
   Failed: Boolean;
begin
   Writeln('MECHANICS');
   Proxy := TTestProxy.Create();
   TestWorld := InitTestEden();
   Failed := False;
   try
      try
         TestPlayer := TTestPlayer.Create('Tester', '', gRobot);
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
         Proxy.ExpectDone();

         // Dig and cover test
         Proxy.Test('Digging');
         Proxy.ExpectNoSubstring('I can''t see anything to move.');
         Proxy.WaitUntilString('');
         Proxy.ExpectString('Foot of Cliff Face');
         Proxy.ExpectNoSubstring('hole');
         Proxy.AndAlso();
         Proxy.ExpectNoSubstring('leaves');
         Proxy.WaitUntilString('');
         Proxy.ExpectString('(the ground with the spade)');
         Proxy.ExpectString('(first taking the spade)');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('With much effort, you dig a huge hole.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Foot of Cliff Face');
         Proxy.ExpectSubstring('hole');
         Proxy.AndAlso();
         Proxy.ExpectNoSubstring('leaves');
         Proxy.WaitUntilString('');
         Proxy.ExpectSubstring('(first taking the '); Proxy.AndAlso(); Proxy.ExpectSubstring(' penny)');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('Dropped on the hole.');
         Proxy.ExpectString('The penny falls into the hole.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Placed on the hole.');
         Proxy.ExpectString('The spade falls into the hole.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Moved onto the hole.');
         Proxy.ExpectString('The MacGuffin falls into the hole.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Moved onto the hole.');
         Proxy.ExpectString('');
         Proxy.SkipLine();
         Proxy.ExpectSubstring('On the hole is a pile of leaves');
         Proxy.ExpectString('');
         Proxy.ExpectString('Foot of Cliff Face');
         Proxy.ExpectNoSubstring('hole');
         Proxy.AndAlso();
         Proxy.ExpectSubstring('leaves');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('move all n; n; dig; l; drop penny onto hole; move spade on to hole; push macguffin on hole; move leaves over hole; x hole; l');
         Proxy.ExpectDone();

         // overfill test
         Proxy.Test('Overfilling');
         Proxy.ExpectString('Moved onto the ground.');
         Proxy.ExpectString('');
         Proxy.ExpectNoSubstring('overflowing');
         Proxy.WaitUntilString('  A spade.');
         Proxy.WaitUntilString('');
         Proxy.ExpectString('The hole is in the ground.');
         Proxy.ExpectString('');
         Proxy.ExpectSubstring('Moved');
         Proxy.ExpectSubstring('falls');
         Proxy.ExpectSubstring('Moved');
         Proxy.ExpectSubstring('falls');
         Proxy.WaitUntilString(''); { finish moving balloons }
         Proxy.ExpectSubstring('overflowing');
         Proxy.AndAlso();
         Proxy.ExpectNoSubstring('spade');
         Proxy.AndAlso();
         Proxy.ExpectSubstring('balloon');
         Proxy.WaitUntilString('  A spade.');
         Proxy.WaitUntilString('');
         Proxy.ExpectSubstring('Moved');
         Proxy.ExpectString('');
         Proxy.ExpectString('There is a hole under the pile of earth.');
         Proxy.ExpectString('');
         Proxy.ExpectString('(off the hole)');
         Proxy.ExpectString('Moved onto the ground.');
         Proxy.ExpectString('');
         Proxy.ExpectString('(out of the hole)');
         Proxy.ExpectString('Moved onto the ground.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Moved onto the hole.');
         Proxy.ExpectString('You fill the hole with the pile of earth.');
         Proxy.ExpectString('');
         Proxy.ExpectString('I can''t find anything like a "hole" here.');
         Proxy.ExpectString('');
         TestPlayer.Perform('move leaves onto ground; x hole; find hole; push balloon on hole; x hole; move earth on hole; find hole; move earth off; move pink out; get spade; move earth onto hole; find hole');
         Proxy.ExpectDone();

         // and/then/etc tests
         Proxy.Test('Continuation and Joins');
         Proxy.ExpectString('You say hello.');
         Proxy.ExpectString('');
         TestPlayer.Perform('say hello');
         Proxy.ExpectString('You say a.');
         Proxy.ExpectString('');
         Proxy.ExpectString('You say b.');
         Proxy.ExpectString('');
         TestPlayer.Perform('say a and say b');
         Proxy.ExpectString('You say a.');
         Proxy.ExpectString('');
         Proxy.ExpectString('You say b.');
         Proxy.ExpectString('');
         TestPlayer.Perform('say a and, then, say b');
         Proxy.ExpectString('You say a.');
         Proxy.ExpectString('');
         Proxy.ExpectString('You say b.');
         Proxy.ExpectString('');
         TestPlayer.Perform('say a; then say b');
         Proxy.ExpectString('You say a.');
         Proxy.ExpectString('');
         Proxy.ExpectString('You say b.');
         Proxy.ExpectString('');
         TestPlayer.Perform('say a; say b');
         Proxy.ExpectString('You say a.');
         Proxy.ExpectString('');
         Proxy.ExpectString('You say b.');
         Proxy.ExpectString('');
         TestPlayer.Perform('say a then, say b');
         Proxy.WaitUntilSubstring('Taken');
         Proxy.WaitUntilString('');
         Proxy.WaitUntilSubstring('dig');
         Proxy.WaitUntilString('');
         Proxy.WaitUntilSubstring('Dropped');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('take all and dig and drop all in hole');
         Proxy.ExpectDone();
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
      Proxy.ExpectDone();
      Proxy.ExpectDisconnect(True);
   finally
      TestWorld.CheckDisposalQueue();
      TestWorld.Free();
      Proxy.Free();
   end;
   if (Failed) then
   begin
      Writeln('FAILURE DETECTED');
      Halt(1);
   end;
end;

procedure TestPlot();
var
   TestWorld: TWorld;
   TestPlayer: TPlayer;
   Proxy: TTestProxy;
   Failed: Boolean;
begin
   Writeln('PLOT');
   Proxy := TTestProxy.Create();
   TestWorld := InitEden();
   Failed := False;
   try
      try
         TestPlayer := TTestPlayer.Create('Tester', '', gRobot);
         TestPlayer.Adopt(@Proxy.HandleAvatarMessage, @Proxy.HandleForceDisconnect);
         TestWorld.AddPlayer(TestPlayer);
         TestPlayer.AnnounceAppearance();

         // Starting room test
         Proxy.Test('Starting location');
         Proxy.ExpectString('On the pedestal (at the arrivals circle)');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('look');
         Proxy.ExpectDone();

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
      Proxy.ExpectDone();
      Proxy.ExpectDisconnect(True);
   finally
      TestWorld.CheckDisposalQueue();
      TestWorld.Free();
      Proxy.Free();
   end;
   if (Failed) then
   begin
      Writeln('FAILURE DETECTED');
      Halt(1);
   end;
end;

begin
   Writeln('CuddlyWorld Tests initializing...');
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   TestMechanics();
   TestPlot();
   Writeln('CuddlyWorld Tests complete.');
end.