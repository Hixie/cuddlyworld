{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
program tests;
uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils, storable, matcher, world, player, locations, things, thingdim, grammarian, cuddlycamp;

//{$DEFINE VERBOSE}

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
{$IFDEF VERBOSE}   Writeln('* ' + FTest); {$ENDIF}
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
      CampForest := TScenery.Create('forest', '((forest/forests (of trees)?) tree/trees)@', 'The forest is dense and impassable.');
      Camp.Add(CampForest, tpAroundImplicit);
      Camp.GetSurface().Add(TStaticThing.Create('penny', 'penny/pennies', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);
      Camp.GetSurface().Add(TStaticThing.Create('MacGuffin', 'MacGuffin/MacGuffins', 'The MacGuffin displays outward signs of being avian in nature.', tmHeavy, tsBig), tpOn);
      Camp.GetSurface().Add(TPile.Create(['leaf'], ['leaves'], 'It appears someone has collected fallen leaves from the forest. Possibly the entire forest, given how big the pile is.', tmLight, tsGigantic), tpOn);
      Camp.ConnectCardinals(Cliff, CampForest, CampForest, CampForest);
      Camp.ConnectDiagonals(CampForest, CampForest, CampForest, CampForest);
      World.AddLocation(Camp);

      { Cliff }
      CliffMountain := TScenery.Create('mountain', 'From here you cannot get a good sense of the size of the mountain. Its cliff face dominates your view.');
      (CliffMountain as TScenery).FindDescription := 'The mountain towers high above you.';
      Cliff.Add(CliffMountain, tpAtImplicit);
      CliffCliff := TScenery.Create('cliff', 'The cliff consists of a sheer rock face.');
      CliffMountain.Add(CliffCliff, tpPartOfImplicit);
      CliffForest := TScenery.Create('forest', '((forest/forests (of trees)?) tree/trees)@', 'The forest is dense and impassable.');
      Cliff.Add(CliffForest, tpAroundImplicit);
      CliffCamp := TDistantScenery.Create('camp', cdSouth);
      Cliff.Add(CliffCamp, tpAroundImplicit);
      Cliff.GetSurface().Add(TSpade.Create(), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large red balloon', '((large huge massive)@ red balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured red.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large orange balloon', '((large huge massive)@ orange balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured orange.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large yellow balloon', '((large huge massive)@ yellow balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured yellow.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large green balloon', '((large huge massive)@ green balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured green.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large blue balloon', '((large huge massive)@ blue balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured blue.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large violet balloon', '((large huge massive)@ (violet purple)@ balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured violet.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large white balloon', '((large huge massive)@ white balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured white.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large black balloon', '((large huge massive)@ black balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured black.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large grey balloon', '((large huge massive)@ (grey gray)@ balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured grey.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large pink balloon', '((large huge massive)@ pink balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured pink.', tmLight, tsMassive), tpOn);
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

         // Basic parsing of things
         Proxy.Test('Parsing of things');
         Proxy.ExpectString('I can''t see any "xyzzy" here to examine.');
         Proxy.ExpectString('');
         TestPlayer.Perform('x xyzzy');

         Proxy.ExpectString('I can''t see any "xyzzy" here to examine.');
         Proxy.ExpectString('');
         TestPlayer.Perform('x the xyzzy');

         Proxy.ExpectString('I was with you until you said "but xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but xyzzy');

         Proxy.ExpectString('I was with you until you said "but the xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but the xyzzy one');

         Proxy.ExpectString('I was with you until you said "all that is xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all that is xyzzy');

         Proxy.ExpectString('I was with you until you said "that is xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag that is xyzzy');

         Proxy.ExpectString('All 1234 what?');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all 1234');

         Proxy.ExpectString('I was with you until you said "but but".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but but');

         Proxy.ExpectString('I don''t understand your use of commas.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take ,');

         Proxy.ExpectString('I can''t see any "," here to take.');
         Proxy.ExpectString('');
         TestPlayer.Perform('TAKE THE,');

         Proxy.ExpectString('Penny: Taken.');
         Proxy.ExpectString('MacGuffin: Taken.');
         Proxy.ExpectSubstring('Pile of leaves: ');
         Proxy.ExpectSubstring('Bag of holding: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all, and bag');

         Proxy.ExpectString('Penny: Dropped.');
         Proxy.ExpectString('MacGuffin: Dropped.');
         Proxy.ExpectString('');
         TestPlayer.Perform('drop all but bag');

         Proxy.ExpectString('The bag has the name "Tester" embroidered around its rim.');
         Proxy.ExpectString('');
         TestPlayer.Perform('x bag');

         Proxy.ExpectString('Around the bag''s rim is embroidered the name "Tester".');
         Proxy.ExpectString('');
         TestPlayer.Perform('x rim');

         Proxy.ExpectString('Around the bag''s rim is embroidered the name "Tester".');
         Proxy.ExpectString('');
         TestPlayer.Perform('x bag rim');

         Proxy.ExpectString('I don''t understand how to examine things "bag".');
         Proxy.ExpectString('');
         TestPlayer.Perform('x rim bag');

         Proxy.ExpectString('Pile of leaves: You shake the pile of leaves.');
         Proxy.ExpectString('MacGuffin: You shake the MacGuffin.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the pile, some leaves, and a macguffin');

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take pile that is the leaf ones');

         Proxy.ExpectString('The pile of leaves slips through your fingers.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take pile that is the leaf one');
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
         Proxy.ExpectSubstring('On the hole is a pile of leaves'); // examine hole
         Proxy.ExpectString('');
         Proxy.ExpectString('The hole is covered by a pile of leaves.'); // look in hole
         Proxy.ExpectString('');
         Proxy.ExpectString('To look in the ground, you first need to dig a hole in it.'); // look in ground
         Proxy.ExpectString('');
         Proxy.ExpectString('Foot of Cliff Face');
         Proxy.ExpectNoSubstring('hole');
         Proxy.AndAlso();
         Proxy.ExpectSubstring('leaves');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('move all n; n; dig; l; drop penny onto hole; move spade on to hole; push macguffin on hole; move leaves over hole; x hole; l in hole; look in ground; l');
         Proxy.ExpectDone();

         // complex parsing
         Proxy.Test('Thing Seeker');
         Proxy.ExpectSubstring('It is coloured pink.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine the balloon that is pink');

         Proxy.ExpectSubstring('It is coloured pink.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine the pink that is a balloon');

         Proxy.ExpectSubstring('Which "balloon that is large" do you mean, ');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine the balloon that is large');

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine the balloon that is pink that is pink');

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine balloons that is pink');

         Proxy.ExpectString('I don''t know how to examine multiple things at once.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine all that is balloon');

         Proxy.ExpectString('(the large pink balloon)');
         Proxy.ExpectSubstring('It is coloured pink.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine all that is pink');

         Proxy.ExpectSubstring(' balloon)');
         Proxy.ExpectSubstring('It is coloured');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine a balloon that is large');

         Proxy.ExpectSubstring(' balloon)');
         Proxy.ExpectSubstring('It is coloured');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine any of the balloons');

         Proxy.ExpectSubstring(' balloon)');
         Proxy.ExpectSubstring('It is coloured');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine one balloon');

         Proxy.ExpectSubstring(' balloon)');
         Proxy.ExpectSubstring('It is coloured');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine some balloon');

         Proxy.ExpectString('I don''t know how to examine multiple things at once.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine some balloons');

         Proxy.ExpectString('(the large pink balloon)');
         Proxy.ExpectSubstring('It is coloured pink.');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine all that is pink');
         Proxy.ExpectDone();

         // counting and parsing with numbers
         Proxy.Test('Counting');
         Proxy.ExpectSubstring('(');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take three balloons');

         Proxy.ExpectSubstring('(');
         Proxy.ExpectSubstring('balloon: (first');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectSubstring('balloon: Dropped.');
         Proxy.ExpectSubstring('balloon: (first');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectSubstring('balloon: Dropped.');
         Proxy.ExpectString('');
         TestPlayer.Perform('drop any two balloons');

         Proxy.ExpectString('About "balloons"... I count 10, not two.');
         Proxy.ExpectString('');
         TestPlayer.Perform('drop the two balloons');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all balloons');
         Proxy.StopSkipping();

         // insert tests for 'but' here
         Proxy.Test('"But"');
         Proxy.ExpectSubstring('Pile of earth:');
         Proxy.ExpectSubstring('Pile of leaves:');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but balloons');

         Proxy.ExpectString('You are carrying:');
         Proxy.ExpectString('  A bag of holding.');
         Proxy.ExpectString('');
         TestPlayer.Perform('i');

         Proxy.ExpectSubstring('green');
         Proxy.ExpectSubstring('blue');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but piles and red and orange and yellow and balloon that is pink and violet and purple and white and black and gray');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all balloons');
         Proxy.StopSkipping();

         Proxy.ExpectSubstring('violet');
         Proxy.ExpectSubstring('white');
         Proxy.ExpectSubstring('black');
         Proxy.ExpectSubstring('red');
         Proxy.ExpectSubstring('orange');
         Proxy.ExpectSubstring('yellow');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all balloons but pink ones, blue ones, the green one that is large, and the gray one');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all balloons');
         Proxy.StopSkipping();

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
         TestPlayer.Perform('move leaves onto ground; x hole; find hole; push balloons on hole; x hole; move earth on hole; find hole; move earth off; move pink out; get spade; move earth onto hole; find hole');
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

         Proxy.ExpectString('You say a.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Then what?');
         Proxy.ExpectString('');
         TestPlayer.Perform('say a then say b then');

         Proxy.ExpectString('You say a.');
         Proxy.ExpectString('');
         Proxy.ExpectString('And what?');
         Proxy.ExpectString('');
         TestPlayer.Perform('say a and say b and');

         Proxy.WaitUntilSubstring('Taken');
         Proxy.WaitUntilString('');
         Proxy.WaitUntilSubstring('dig');
         Proxy.WaitUntilString('');
         Proxy.WaitUntilSubstring('Dropped');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('take all and dig and drop all in hole');

         Proxy.ExpectString('And what?');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag and spade and');

         Proxy.ExpectString('And what?');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all and');

         Proxy.ExpectString('Then what?');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all and then');

         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('');
         Proxy.ExpectString('I don''t understand how to "and".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take spade and and');

         Proxy.ExpectString('I don''t understand your use of commas.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag, spade, ');
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
   TestWorld, TestWorld2: TWorld;
   TestPlayer: TPlayer;
   Proxy: TTestProxy;
   Failed: Boolean;
begin
   Writeln('PLOT');
   Proxy := TTestProxy.Create();
   Proxy.Test('InitEden()');
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
         Proxy.ExpectString('On the stone pedestal (at the arrivals circle)');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('look');
         Proxy.ExpectDone();

         Proxy.ExpectString('(the bag of holding)');
         Proxy.ExpectString('You already have that.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all, and bag');

         // test round-tripping
         Proxy.Test('Round-tripping');
         RegisterStorableClass(TTestPlayer, 19);
         StoreObjectToFile('/tmp/world.dat.$$$', TestWorld, kSaveDataVersion);
         TestWorld2 := ReadObjectFromFile('/tmp/world.dat.$$$') as TWorld;
         TestWorld2.Free();

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

procedure TestMatcher();
var
   TestID: Cardinal;

   procedure RunMatcherTest(TestMatcher: TMatcher; Candidate: TTokens; Start: Cardinal; Pass: Cardinal);
   var
      Result: Cardinal;
   begin
      Inc(TestID);
      Result := TestMatcher.Matches(Candidate, Start);
      if (Result <> Pass) then
         Writeln('FAILED matcher test ', TestID, '; expected to match ', Pass, ' tokens but matched ', Result);
   end;

   procedure RunCanonicalMatchTest(TestMatcher: TMatcher; Pass: AnsiString);
   var
      Result: AnsiString;
   begin
      Inc(TestID);
      Result := TestMatcher.GetCanonicalMatch(' ');
      if (Result <> Pass) then
         Writeln('FAILED matcher test ', TestID, '; expected to find longest match "', Pass, '" but got "', Result, '"');
   end;

var
   TestMatcher, OtherMatcher: TMatcher;
   Strings: TTokens;
begin
   Writeln('PATTERN COMPILER');

   SetLength(Strings, 4);
   Strings[0] := 'the';
   Strings[1] := 'green';
   Strings[2] := 'lantern';
   Strings[3] := 'glowing';

   TestID := 0;

   CompilePattern('the', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the) green', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the green');
   RunMatcherTest(TestMatcher, Strings, 0, 2);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(((the green lantern glowing)))', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the green lantern glowing');
   RunMatcherTest(TestMatcher, Strings, 0, 4);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the? (green lantern)? glowing', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the green lantern glowing');
   RunMatcherTest(TestMatcher, Strings, 0, 4);
   RunMatcherTest(TestMatcher, Strings, 1, 3);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the? ((glowing green)# lantern)&', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing green lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 3);
   RunMatcherTest(TestMatcher, Strings, 1, 2);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(glowing lantern)@', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'glowing');
   RunMatcherTest(TestMatcher, Strings, 0, 0);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)*', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 2);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)#', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 2);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)%', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('(the glowing lantern)&', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 1);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 1);
   RunMatcherTest(TestMatcher, Strings, 3, 1);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the+ glowing? lantern+', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 0);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the/fail FAIL?/fail green lantern/fail (FAIL FAIL)?/glowing', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the FAIL green lantern FAIL FAIL');
   RunMatcherTest(TestMatcher, Strings, 0, 3);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   RunCanonicalMatchTest(OtherMatcher, 'fail fail green fail glowing');
   RunMatcherTest(OtherMatcher, Strings, 0, 0);
   RunMatcherTest(OtherMatcher, Strings, 1, 0);
   RunMatcherTest(OtherMatcher, Strings, 2, 0);
   RunMatcherTest(OtherMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('the (glowing)?/(yellow green blue red)& lantern/(glowing lantern)*', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'the glowing lantern');
   RunMatcherTest(TestMatcher, Strings, 0, 0);
   RunMatcherTest(TestMatcher, Strings, 1, 0);
   RunMatcherTest(TestMatcher, Strings, 2, 0);
   RunMatcherTest(TestMatcher, Strings, 3, 0);
   RunCanonicalMatchTest(OtherMatcher, 'the yellow green blue red glowing lantern');
   RunMatcherTest(OtherMatcher, Strings, 0, 4);
   RunMatcherTest(OtherMatcher, Strings, 1, 0);
   RunMatcherTest(OtherMatcher, Strings, 2, 0);
   RunMatcherTest(OtherMatcher, Strings, 3, 0);
   TestMatcher.Free();
   OtherMatcher.Free();

   CompilePattern('((((navy blue)& (wooden wood)@)# ((archway/archways arch/arches)@ (to the (north n)@)?))& (((navy blue)& (wooden wood)@ (north northern n)@)# (archway/archways arch/arches)@)&)@', TestMatcher, OtherMatcher);
   RunCanonicalMatchTest(TestMatcher, 'navy blue wooden archway to the north');
   RunCanonicalMatchTest(OtherMatcher, 'navy blue wooden archways to the north');
   TestMatcher.Free();
   OtherMatcher.Free();
end;

begin
   Writeln('CuddlyWorld Tests initializing...');
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   TestMatcher();
   TestMechanics();
   TestPlot();
   Writeln('CuddlyWorld Tests complete.');
end.