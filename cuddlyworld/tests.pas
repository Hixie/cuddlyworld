{$MODE OBJFPC} { -*- text -*- }

{$DEFINE VERBOSE}
{$DEFINE PLAY_IN_TEST_EDEN}

{$INCLUDE settings.inc}
program tests;
uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils, storable, matcher, lists, world, player, locations, things, thingdim, grammarian, cuddlycamp;

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
        else Error := 'Unknown expectation';
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
     FStartLocation: TLocation;
     procedure AddPlayer(Avatar: TAvatar); override;
   end;

procedure TTestWorld.AddPlayer(Avatar: TAvatar);
begin
   FStartLocation.GetSurface().Add(Avatar, tpOn);
   inherited;
end;


procedure TestMechanics();

   function InitTestEden: TWorld;
   var
      World: TTestWorld;
      Camp, Cliff, Cave1, Cave2, Cave3, Cave4, FlowerRoom, Kitchen: TLocation;
      CampMountain, CampForest: TThing;
      CliffMountain, CliffForest, CliffCamp, CliffCliff: TThing;
      Thing, Thing2: TThing;
   begin
      World := TTestWorld.Create();

      { Locations }
      Camp := TFeaturelessOutdoorLocation.Create('Camp Cuddlyfort', 'Camp Cuddlyfort', 'a camp', 'This is a camp nestled in a forest, under the shadow of a mountain to the north.');
      Cliff := TFeaturelessOutdoorLocation.Create('Foot of Cliff Face', 'the foot of the cliff face', 'a foot of a cliff face', 'The south side of a mountain rises out of the ground here, in a clear and well-defined way, as if to say "this far, no farther" to an enemy whose nature you cannot fathom. ' + 'The cliff to the north is a sheer rock face, essentially unclimbable, with a cave entrance. ' + ' Conspicuous is the absence of any vegetation anywhere on the cliff, at least as far as you can see. At the base of the cliff to the east and west is a dense forest.');
      Cave1 := TFeaturelessOutdoorLocation.Create('Cave one', 'the first cave', 'a cave', 'The cave is very well-lit from the south.');
      Cave2 := TFeaturelessOutdoorLocation.Create('Cave two', 'the second cave', 'a cave', 'The cave is somewhat well-lit from the south.');
      Cave3 := TFeaturelessOutdoorLocation.Create('Cave three', 'the third cave', 'a cave', 'The cave is lit from the south.');
      Cave4 := TFeaturelessOutdoorLocation.Create('Cave four', 'the fourth cave', 'a cave', 'The cave is brightly lit from an entrace to a white room to the west. There is also some dim light coming from the south.');
      FlowerRoom := TFeaturelessOutdoorLocation.Create('Flower room', 'the flower room', 'a flower room', 'The room has bright ambient lighting for no apparent reason. It is a bright white room, almost clinical in nature, but it unexpectedly conveys a sense of floweriness. An exit to the east appears to lead to a dimly lit cave. Another exit leads south.');
      Kitchen := TFeaturelessOutdoorLocation.Create('Kitchen', 'the kitchen', 'a kitchen', 'The room has bright ambient lighting for no apparent reason. It is a bright white room, almost clinical in nature, but it unexpectedly conveys a sense of being, or having once been, used for food preparation. An exit leads north.');

      { Camp }
      CampMountain := TDistantScenery.Create('mountain', 'mountain/mountains', cdNorth);
      Camp.Add(CampMountain, tpAroundImplicit);
      CampForest := TScenery.Create('forest', '((forest/forests (of trees)?) tree/trees)@', 'The forest is dense and impassable.');
      Camp.Add(CampForest, tpAroundImplicit);
      Camp.GetSurface().Add(TPile.Create(['leaf'], ['leaves'], 'It appears someone has collected fallen leaves from the forest. Possibly the entire forest, given how big the pile is.', tmLight, tsGigantic), tpOn);
      Camp.GetSurface().Add(TStaticThing.Create('MacGuffin', 'MacGuffin/MacGuffins', 'The MacGuffin displays outward signs of being avian in nature.', tmHeavy, tsBig), tpOn);
      Camp.GetSurface().Add(TStaticThing.Create('penny', 'penny/pennies', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);

      { Cliff }
      CliffMountain := TScenery.Create('mountain', 'mountain/mountains', 'From here you cannot get a good sense of the size of the mountain. Its cliff face dominates your view.');
      (CliffMountain as TScenery).FindDescription := 'The mountain towers high above you.';
      Cliff.Add(CliffMountain, tpAtImplicit);
      CliffCliff := TScenery.Create('cliff', '((sheer rock cliff/cliffs)& (rock face/faces))@', 'The cliff consists of a sheer rock face with a cave entrace in it.');
      CliffMountain.Add(CliffCliff, tpPartOfImplicit);
      CliffForest := TScenery.Create('forest', '((forest/forests (of trees)?) tree/trees)@', 'The forest is dense and impassable.');
      Cliff.Add(CliffForest, tpAroundImplicit);
      CliffCamp := TDistantScenery.Create('camp', 'camp/camps', cdSouth);
      Cliff.Add(CliffCamp, tpAroundImplicit);
      Cliff.GetSurface().Add(TStaticThing.Create('large pink balloon', '((large huge massive)@ pink balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured pink.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large grey balloon', '((large huge massive)@ (grey gray)@ balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured grey.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large black balloon', '((large huge massive)@ black balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured black.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large white balloon', '((large huge massive)@ white balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured white.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large violet balloon', '((large huge massive)@ (violet purple)@ balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured violet.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large blue balloon', '((large huge massive)@ blue balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured blue.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large green balloon', '((large huge massive)@ green balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured green.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large yellow balloon', '((large huge massive)@ yellow balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured yellow.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large orange balloon', '((large huge massive)@ orange balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured orange.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TStaticThing.Create('large red balloon', '((large huge massive)@ red balloon/balloons)&', 'This balloon is as wide as your arm span, making it difficult to handle. It is coloured red.', tmLight, tsMassive), tpOn);
      Cliff.GetSurface().Add(TSpade.Create(), tpOn);


      { Cave 1 }

      Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

      Thing := TStaticThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Cave1.GetSurface().Add(TSpade.Create(), tpOn);

      Thing := TPile.Create(['rock'], ['rocks'], 'The pile of rocks is boring and uninteresting.', tmHeavy, tsBig);
      Cave1.GetSurface().Add(Thing, tpOn);
      Thing.Add(TPile.Create(['diamond'], ['diamonds'], 'The pile of diamonds is the tiniest pile of diamonds you have ever seen.', tmLight, tsSmall), tpEmbedded);


      { Cave 2 }

      Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

      Thing := TStaticThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Cave2.GetSurface().Add(TSpade.Create(), tpOn);

      Thing := TPile.Create(['rock'], ['rocks'], 'The pile of rocks is boring and uninteresting.', tmHeavy, tsBig);
      Cave2.GetSurface().Add(Thing, tpOn);
      Thing.Add(TPile.Create(['diamond'], ['diamonds'], 'The pile of diamonds is the tiniest pile of diamonds you have ever seen.', tmLight, tsSmall), tpEmbedded);


      { Cave 3 }

      Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

      Cave3.GetSurface().Add(TSpade.Create(), tpOn);

      Thing := TStaticThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Thing := TPile.Create(['rock'], ['rocks'], 'The pile of rocks is boring and uninteresting.', tmHeavy, tsBig);
      Cave3.GetSurface().Add(Thing, tpOn);
      Thing.Add(TPile.Create(['diamond'], ['diamonds'], 'The pile of diamonds is the tiniest pile of diamonds you have ever seen.', tmLight, tsSmall), tpEmbedded);


      { Cave 4 }

      Cave4.GetSurface().Add(TStaticThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('plastic table', '(plastic table/tables)&', 'The table is made of plastic.', tmPonderous, tsMassive);
      Cave4.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('stainless steel table', '(stainless steel table/tables)&', 'The table is made of stainless steel.', tmPonderous, tsMassive);
      Cave4.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('silver table', '(silver table/tables)&', 'The table is made of silver.', tmPonderous, tsMassive);
      Cave4.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('silver knife', '(silver (knife/knives utensil/utensils)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver fork', '(silver (fork/forks utensil/utensils)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
      Thing.Add(TStaticThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);

      Cave4.Add(TScenery.Create('cave paintings', 'cave? painting/paintings', 'The cave paintings are non-descript.'), tpAt);


      { Flower Room}

      Thing := TStaticThing.Create('red table', '(red table/tables)&', 'The table is red.', tmHeavy, tsBig);
      FlowerRoom.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('red vase', '(red vase/vases)&', 'The vase is red.', tmLight, tsSmall), tpOn);

      Thing := TStaticThing.Create('blue table', '(blue table/tables)&', 'The table is blue.', tmHeavy, tsBig);
      FlowerRoom.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('blue vase', '(blue vase/vases)&', 'The vase is blue.', tmLight, tsSmall), tpOn);


      { Kitchen }

      Thing := TStaticThing.Create('wooden table', '(brown wooden table/tables)&', 'The table is made of brown wood.', tmHeavy, tsBig);
      Kitchen.GetSurface().Add(Thing, tpOn);

      Thing2 := TStaticThing.Create('fruit plate', '(fruit plate/plates)&', 'The plate is intended to hold fruit.', tmLight, tsSmall);
      Thing.Add(Thing2, tpOn);
      Thing2.Add(TStaticThing.Create('red grapes', '(red (grape/grapes fruit)@)&', 'The grapes are red.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('green grapes', '(green (grape/grapes fruit)@)&', 'The grapes are green.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('red apple', '(red (apple/apples fruit)@)&', 'The apple is red.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('green apple', '(green (apple/apples fruit)@)&', 'The apple is green.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('yellow apple', '(yellow (apple/apples fruit)@)&', 'The apple is yellow.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('red pepper', '(red (pepper/peppers fruit)@)&', 'The pepper is red.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('green pepper', '(green (pepper/peppers fruit)@)&', 'The pepper is green.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('yellow pepper', '(yellow (pepper/peppers fruit)@)&', 'The pepper is yellow.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('orange', '(orange (orange/oranges fruit)@)&', 'The orange is orange.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('apricot', '(orange (apricot/apricots fruit)@)&', 'The apricot is orange.', tmLight, tsSmall), tpOn);
      Thing2.Add(TStaticThing.Create('banana', '(yellow (banana/bananas fruit)@)&', 'The banana is yellow.', tmLight, tsSmall), tpOn);

      Thing2 := TContainer.Create('box', '((berry fruit)@ (box/boxes container/containers)@)&', 'The box is intended to hold berries.', tmLight, tsSmall);
      Thing.Add(Thing2, tpOn);
      Thing2.Add(TStaticThing.Create('strawberries', '(red (strawberry/strawberries berry/berries fruit)@)&', 'The strawberries are red.', tmLight, tsSmall), tpIn);
      Thing2.Add(TStaticThing.Create('blueberries', '(blue (blueberry/blueberries berry/berries fruit)@)&', 'The blueberries are blue.', tmLight, tsSmall), tpIn);

      Thing2 := TContainer.Create('crate', '(fruit (crate/crates container/containers)@)&', 'The crate is intended to hold fruit.', tmLight, tsSmall);
      Thing.Add(Thing2, tpOn);
      Thing2.Add(TStaticThing.Create('pineapple', '((prickly brown)# (pineapple/pineapples fruit)@)&', 'The pineapple is brown and prickly.', tmLight, tsSmall), tpIn);
      Thing2.Add(TStaticThing.Create('kiwi', '((furry brown)# (kiwi/kiwis fruit)@)&', 'The kiwi is brown and furry.', tmLight, tsSmall), tpIn);

      Thing := TBag.Create('garbage bag', '(black ((garbage bag/bags)& (trash bag/bags) trashbag/trashbags)@)&', 'The garbage bag is black.', tsBig);
      Kitchen.GetSurface().Add(Thing, tpOn);
      Thing.Add(TStaticThing.Create('rotten grapes', '((rotten red)# (grape/grapes fruit)@)&', 'The grapes is rotten.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('rotten kiwi', '((rotten furry brown)# (kiwi/kiwis fruit)@)&', 'The kiwi is rotten.', tmLight, tsSmall), tpIn);
      Thing.Add(TStaticThing.Create('rotten pineapple', '((rotten prickly brown)# (pineapple/pineapples fruit)@)&', 'The pineapple is rotten.', tmLight, tsSmall), tpIn);


      { Connections }

      Camp.ConnectCardinals(Cliff, CampForest, CampForest, CampForest);
      Camp.ConnectDiagonals(CampForest, CampForest, CampForest, CampForest);
      Cliff.ConnectCardinals(Cave1, CliffForest, Camp, CliffForest);
      Cliff.ConnectDiagonals(nil, CliffForest, CliffForest, nil);
      Cave1.ConnectCardinals(Cave2, nil, Cliff, nil);
      Cave2.ConnectCardinals(Cave3, nil, Cave1, nil);
      Cave3.ConnectCardinals(Cave4, nil, Cave2, nil);
      Cave4.ConnectCardinals(nil, nil, Cave3, FlowerRoom);
      FlowerRoom.ConnectCardinals(nil, Cave4, Kitchen, nil);
      Kitchen.ConnectCardinals(FlowerRoom, nil, nil, nil);

      World.AddLocation(Camp);
      World.AddLocation(Cliff);
      World.AddLocation(Cave1);
      World.AddLocation(Cave2);
      World.AddLocation(Cave3);
      World.AddLocation(Cave4);
      World.AddLocation(FlowerRoom);
      World.AddLocation(Kitchen);
      World.FStartLocation := Camp;

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
   {$IFDEF PLAY_IN_TEST_EDEN}
      RegisterStorableClassAsSynonym(TTestWorld, TWorld);
      TestWorld.AddPlayer(TPlayer.Create('Flathead', 'zorkmid', gMale));
      StoreObjectToFile(kWorldFileName, TestWorld, kSaveDataVersion);
      TestWorld.Free();
      TestWorld := InitTestEden();
   {$ENDIF}
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

         Proxy.ExpectString('I was with you until you said "that is xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all that is xyzzy');

         Proxy.ExpectString('I was with you until you said "that is xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag that is xyzzy');

         Proxy.ExpectString('You used the term "and that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag and that is bag');

         Proxy.ExpectString('I was with you until you said "that are xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all that are xyzzy');

         Proxy.ExpectString('I was with you until you said "that are xyzzy".');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag that are xyzzy');

         Proxy.ExpectString('You used the term "and that are" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag and that are bag');

         Proxy.ExpectString('You used the term "and on" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take bag and on ground');

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

         Proxy.ExpectSubstring('Pile of leaves: ');
         Proxy.ExpectString('MacGuffin: Taken.');
         Proxy.ExpectString('Penny: Taken.');
         Proxy.ExpectSubstring('Bag of holding: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all, and bag');

         Proxy.ExpectString('MacGuffin: Dropped.');
         Proxy.ExpectString('Penny: Dropped.');
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

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take piles that is the leaf ones');

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take piles that is the leaf one');

         Proxy.ExpectString('You used the term "that are" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take piles that are the leaf one');

         Proxy.ExpectString('(the pile of leaves)');
         Proxy.ExpectString('The pile of leaves slips through your fingers.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take piles that are the leaf ones');

         Proxy.ExpectString('You used the term "that are" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take pile that are the leaf one');

         Proxy.ExpectString('You used the term "that are" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take pile that are the leaf ones');
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
         Proxy.ExpectString('(the pile of leaves)');
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

         Proxy.ExpectSubstring('Which balloon that is large do you mean, ');
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

         Proxy.Test('"that is" and "and that is" and so on');
         Proxy.ExpectString('(the large pink balloon)');
         Proxy.ExpectString('You shake the large pink balloon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake balloons that are large and that are pink and that are not blue');

         Proxy.Test('"that is" and "and that is" and so on');
         Proxy.ExpectString('Large grey balloon: You shake the large grey balloon.');
         Proxy.ExpectString('Large black balloon: You shake the large black balloon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake balloons that are not purple and that are not red and that are not green and that are not blue and that are not pink and that are not orange and that are not yellow and that are not white');

         // counting and parsing with numbers
         Proxy.Test('Counting');
         Proxy.ExpectSubstring('(');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectSubstring('balloon: Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take three balloons');

//         Proxy.ExpectSubstring('(');
//         Proxy.ExpectSubstring('pink balloon: Dropped.');
//         Proxy.ExpectSubstring('grey balloon: Dropped.');
//         Proxy.ExpectString('');
//         TestPlayer.Perform('drop any two balloons');

         Proxy.ExpectString('About the two balloons... I count 10, not two.');
         Proxy.ExpectString('');
         TestPlayer.Perform('drop the two balloons');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all balloons; look');
         Proxy.StopSkipping();

         Proxy.Test('"But"');
         Proxy.ExpectSubstring('Pile of leaves:');
         Proxy.ExpectSubstring('Pile of earth:');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but balloons');

         Proxy.ExpectString('You are carrying:');
         Proxy.ExpectString('  A bag of holding.');
         Proxy.ExpectString('');
         TestPlayer.Perform('i');

         Proxy.ExpectSubstring('blue');
         Proxy.ExpectSubstring('green');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but piles and red and orange and yellow and balloon that is pink and violet and purple and white and black and gray');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all balloons; look');
         Proxy.StopSkipping();

         Proxy.ExpectSubstring('white');
         Proxy.ExpectSubstring('violet');
         Proxy.ExpectSubstring('yellow');
         Proxy.ExpectSubstring('orange');
         Proxy.ExpectSubstring('red');
         Proxy.ExpectSubstring('black');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all balloons but pink ones, blue ones, the green one that is large, and the gray one');

         Proxy.ExpectString('(the pile of earth)');
         Proxy.ExpectSubstring('slips');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all piles but the pile that is the pile of leaves');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all balloons');
         Proxy.StopSkipping();

         Proxy.Test('"From"');
         Proxy.ExpectSubstring('Hole: ');
         Proxy.ExpectSubstring('Pile of earth: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all from ground but balloons');

         Proxy.ExpectSubstring('(the pile of leaves)');
         Proxy.ExpectSubstring('slips');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all from hole but balloons');

         Proxy.ExpectSubstring('Hole: ');
         Proxy.ExpectSubstring('Pile of earth: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but balloons from ground');

         Proxy.ExpectSubstring('(the pile of leaves)');
         Proxy.ExpectSubstring('slips');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but balloons from hole');

         Proxy.ExpectSubstring('Hole: ');
         Proxy.ExpectSubstring('Pile of earth:');
         Proxy.ExpectSubstring('Pile of leaves:');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but balloons from ground and all but earth from hole');

         Proxy.ExpectDone();

         // overfill test
         Proxy.Test('Overfilling');
         Proxy.ExpectString('(the pile of leaves)');
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

         Proxy.SkipEverything();
         TestPlayer.Perform('inventory; look');
         Proxy.StopSkipping();

         Proxy.ExpectString('Pile of leaves: The pile of leaves slips through your fingers.');
         Proxy.ExpectString('Large pink balloon: Taken.');
         Proxy.ExpectString('');
         Proxy.ExpectString('(the ground with the spade)');
         Proxy.ExpectString('With much effort, you dig a huge hole.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Bag of holding: Dropped in the hole.');
         Proxy.ExpectString('Spade: Dropped in the hole.');
         Proxy.ExpectString('Large pink balloon: Dropped in the hole.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all and dig and drop all in hole in ground');

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

         // more parsing tests
         Proxy.Test('More thingseeker tests');
         Proxy.SkipEverything();
         TestPlayer.Perform('north');
         Proxy.StopSkipping();

         Proxy.ExpectString('Which utensil that is not a fork from a table that is not plastic and that is not a desk do you mean, the plastic knife or the plastic spoon?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the utensil that is not a fork from a table that is not plastic and that is not a desk');

         Proxy.ExpectString('(the plastic knife and the plastic spoon)');
         Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all utensils that are not forks from a table that is not plastic and that is not a desk');

         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all of the knives but fork from desk and all of the steel but fork from desk');

         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all of the knives from desk and all of the steel but fork from desk');

         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all of the knives but fork and all of the steel but fork from desk');

         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all of the knives and all of the steel but fork from desk');

         Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Silver knife: You shake the silver knife.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all utensils but some spoons and some forks');

         Proxy.ExpectString('(the stainless steel knife, the stainless steel fork, and the stainless steel spoon)');
         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from one of the tables THAT IS NOT the kitchen table');

         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver knife: You shake the silver knife.');
         Proxy.ExpectString('Silver fork: You shake the silver fork.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from all of the tables THAT ARE NOT the kitchen table');

         Proxy.ExpectString('Plastic fork: Taken.');
         Proxy.ExpectString('Silver fork: Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take forks from the table that is kitchen and from the table that is dining');

         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake spoons and all forks from desk');

         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all spoons and all forks from desk');

         Proxy.ExpectString('Foot of Cliff Face');
         Proxy.WaitUntilString('');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Cave one');
         Proxy.WaitUntilString('');
         Proxy.ExpectString('You say "bring it on!".');
         Proxy.ExpectString('');
         TestPlayer.Perform('south; take bag; north; say "bring it on!"');
         Proxy.ExpectDone();

         Proxy.ExpectString('Placed in the brown sack.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Placed in the bag of holding.');
         Proxy.ExpectString('');
         Proxy.ExpectString('(first taking the brown sack)');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('Placed on the desk.');
         Proxy.ExpectString('');
         TestPlayer.Perform('put plastic fork in sack and put silver fork in bag of holding; then put sack on desk');

         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take fork from sack from table');

         Proxy.ExpectString('Placed in the brown sack.');
         Proxy.ExpectString('');
         TestPlayer.Perform('put plastic fork in sack on table');

         Proxy.ExpectString('You shake the plastic fork.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake fork from bag from table');

         Proxy.ExpectString('You shake the plastic fork.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake fork from bag on table');

         Proxy.ExpectString('You shake the plastic fork.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake fork in bag from table');

         Proxy.ExpectString('You shake the plastic fork.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake fork in bag on table');

         Proxy.ExpectString('The stainless steel fork is on the desk.');
         Proxy.ExpectString('');
         TestPlayer.Perform('find steel fork');

         Proxy.ExpectString('The silver fork is in the bag of holding.');
         Proxy.ExpectString('');
         TestPlayer.Perform('find silver fork');

         Proxy.ExpectString('The plastic fork is in the brown sack.');
         Proxy.ExpectString('');
         TestPlayer.Perform('find plastic fork');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all then look then north');
         Proxy.StopSkipping();

         Proxy.ExpectString('(the plastic knife)');
         Proxy.ExpectString('You shake the plastic knife.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all knives but silver and stainless steel');

         Proxy.ExpectString('(the plastic knife)');
         Proxy.ExpectString('You shake the plastic knife.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all knives but all silver and stainless steel');

         Proxy.ExpectString('(the plastic knife)');
         Proxy.ExpectString('You shake the plastic knife.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all knives but all silver and all stainless steel');

         Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all knives but silver and all stainless steel');

         Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Silver knife: You shake the silver knife.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake knives and all steel from desk but fork');

         Proxy.ExpectString('Plastic knife: You shake the plastic knife.');
         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Silver knife: You shake the silver knife.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake knives and all steel but fork from desk');

         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all knives but wooden plus all steel but fork from desk');

         Proxy.ExpectString('Stainless steel knife: Taken.');
         Proxy.ExpectString('Stainless steel spoon: Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all knives and all steel but fork from desk');

         Proxy.ExpectString('(the plastic knife, the plastic fork, and the plastic spoon)');
         Proxy.ExpectString('Plastic knife: Taken.');
         Proxy.ExpectString('Plastic fork: Taken.');
         Proxy.ExpectString('Plastic spoon: Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all from one of the tables');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all then look then north');
         Proxy.StopSkipping();

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the utensil that is all but fork and knife');

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the utensil that is all but some fork and a knife');

         Proxy.ExpectString('You used the term "that is" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the utensil that is all but some fork and two knives');

         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the utensils that are all but fork and knife');

         Proxy.ExpectString('(the wooden spoon, the plastic spoon, the stainless steel knife, the stainless steel fork, the stainless steel spoon, the silver knife, the silver fork, and the silver spoon)');
         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel knife: You shake the stainless steel knife.');
         Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver knife: You shake the silver knife.');
         Proxy.ExpectString('Silver fork: You shake the silver fork.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the utensils that are all but some fork and a knife');

         Proxy.ExpectString('(the wooden spoon, the plastic spoon, the stainless steel fork, the stainless steel spoon, the silver knife, the silver fork, and the silver spoon)');
         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel fork: You shake the stainless steel fork.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver knife: You shake the silver knife.');
         Proxy.ExpectString('Silver fork: You shake the silver fork.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the utensils that are all but some fork and two knives');

         Proxy.ExpectString('(the plastic knife, the plastic fork, the plastic spoon, and the stainless steel knife)');
         Proxy.ExpectString('Plastic knife: Taken.');
         Proxy.ExpectString('Plastic fork: Taken.');
         Proxy.ExpectString('Plastic spoon: Taken.');
         Proxy.ExpectString('Stainless steel knife: Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take four utensils from two of the tables');

         Proxy.ExpectString('About the four forks from two tables... I can only find two: the stainless steel fork and the silver fork.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take four forks from two of the tables');

         Proxy.ExpectSubstring('Clove of garlic: ');
         Proxy.ExpectSubstring('Lunch: ');
         Proxy.ExpectSubstring('Wooden spoon: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from all but table');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all then look');
         Proxy.StopSkipping();

         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('');
         Proxy.ExpectSubstring('You are carrying:');
         Proxy.ExpectString('  A silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take the spoon from the table that is dining then inventory');

         Proxy.ExpectSubstring('(the silver knife and the stainless steel fork)');
         Proxy.ExpectSubstring('shake');
         Proxy.ExpectSubstring('shake');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake some utensil from the dining room table and some utensil from the desk');
 
         Proxy.SkipEverything();
         TestPlayer.Perform('drop all then look then north');
         Proxy.StopSkipping();

         Proxy.ExpectSubstring('Silver table: ');
         Proxy.ExpectSubstring('Silver knife: ');
         Proxy.ExpectSubstring('Silver spoon: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all that is silver but fork');

         Proxy.SkipEverything();
         TestPlayer.Perform('put silver fork on plastic table');
         TestPlayer.Perform('put plastic fork on steel table');
         TestPlayer.Perform('put steel fork on silver table');
         Proxy.StopSkipping();

         Proxy.ExpectSubstring('Silver fork: ');
         Proxy.ExpectSubstring('Plastic fork: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all forks from all tables but silver');

         Proxy.ExpectSubstring('Plastic fork: ');
         Proxy.ExpectSubstring('Stainless steel fork: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all forks from tables but silver');

         Proxy.ExpectSubstring('Silver fork: ');
         Proxy.ExpectSubstring('Plastic fork: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake forks from all tables but silver');

         Proxy.ExpectSubstring('You used the term "but" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake forks from tables but silver');

         Proxy.SkipEverything();
         TestPlayer.Perform('put spoons on plastic table');
         Proxy.StopSkipping();

         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake spoons from silver table and from plastic table');

         Proxy.SkipEverything();
         TestPlayer.Perform('take all the spoons; push everything but the spoons south; drop the spoons; s; s; s; take a spade; n; n; n; l');
         Proxy.StopSkipping();

         Proxy.ExpectString('With much effort, you dig a huge hole.');
         Proxy.ExpectString('');
         TestPlayer.Perform('dig the ground with the spade that is metal');

         Proxy.ExpectString('Wooden spoon: Moved into the hole.');
         Proxy.ExpectString('Plastic spoon: Moved into the hole.');
         Proxy.ExpectString('Stainless steel spoon: Moved into the hole.');
         Proxy.ExpectString('Silver spoon: Moved into the hole.');
         Proxy.ExpectString('');
         Proxy.ExpectString('Moved into the hole.');
         Proxy.ExpectString('You fill the hole with the pile of earth.');
         Proxy.ExpectString('');
         TestPlayer.Perform('push all the spoons into the hole, then push the pile of earth into the hole');

         Proxy.ExpectString('With much effort, you dig a huge hole.');
         Proxy.ExpectString('');
         TestPlayer.Perform('dig the ground with the spade that is metal');

         Proxy.ExpectString('I don''t see anything to take here.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take all but pile');

         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all spoons');

         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from pile');

         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from all that is earth');

         Proxy.ExpectString('It''s not clear to what you are referring.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from all that is earth but pile');

         Proxy.ExpectString('(the wooden spoon)');
         Proxy.ExpectString('(on the ground)');
         Proxy.ExpectString('(first taking the wooden spoon)');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('Placed.');
         Proxy.ExpectString('');
         TestPlayer.Perform('put a spoon from pile of earth');

         Proxy.ExpectString('(the wooden spoon)');
         Proxy.ExpectString('You shake the wooden spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon on ground');

         Proxy.ExpectString('It''s not clear to what you are referring.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon in ground');

         Proxy.ExpectString('(the plastic spoon)');
         Proxy.ExpectString('You shake the plastic spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon in pile');

         Proxy.ExpectString('It''s not clear to what you are referring.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon on pile');

         Proxy.ExpectString('It''s not clear to what you are referring.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon on ground in pile');

         Proxy.ExpectString('(the plastic spoon)');
         Proxy.ExpectString('You shake the plastic spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon in pile on ground');

         Proxy.SkipEverything();
         TestPlayer.Perform('inventory; look; look in hole; look in pile');
         Proxy.StopSkipping();

         Proxy.ExpectString('Plastic spoon: You shake the plastic spoon.');
         Proxy.ExpectString('Stainless steel spoon: You shake the stainless steel spoon.');
         Proxy.ExpectString('Silver spoon: You shake the silver spoon.');
         Proxy.ExpectString('Wooden spoon: You shake the wooden spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake spoons in pile and on ground');

         Proxy.ExpectString('I don''t see any "bag".');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon in ground and on pile and from bag');

         Proxy.ExpectString('I don''t see any "bag".');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon on ground and in pile and from bag');

         Proxy.ExpectString('It''s not clear to what you are referring.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon in ground and on pile and from the cave paintings');

         Proxy.ExpectString('(the plastic spoon)');
         Proxy.ExpectString('You shake the plastic spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake a spoon on ground and in pile and from the cave paintings');

         Proxy.ExpectString('Which spoon on ground and in pile and from the cave paintings do you mean, the plastic spoon, the stainless steel spoon, the silver spoon, or the wooden spoon?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the spoon on ground and in pile and from the cave paintings');

         Proxy.ExpectString('Which spoon in pile and on ground and from the cave paintings do you mean, the plastic spoon, the stainless steel spoon, the silver spoon, or the wooden spoon?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the spoon in pile and on ground and from the cave paintings');

         Proxy.ExpectString('About the two spoons but the stainless steel spoon... I count three, not two.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all two spoons but the stainless steel spoon');

         Proxy.ExpectString('(the plastic spoon)');
         Proxy.ExpectString('You shake the plastic spoon.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake plastic spoon in pile and from a spoon');

         Proxy.SkipEverything();
         TestPlayer.Perform('drop all then look then west');
         Proxy.StopSkipping();

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vase on table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vase on the table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vase on the table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vase on table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vases on table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vases on the table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vases on the table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vases on table');

         Proxy.ExpectString('Which vase on tables do you mean, the red vase or the blue vase?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vase on tables');

         Proxy.ExpectString('Which vase on the tables do you mean, the red vase or the blue vase?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vase on the tables');

         Proxy.ExpectString('Which vase on the tables do you mean, the red vase or the blue vase?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vase on the tables');

         Proxy.ExpectString('Which vase on tables do you mean, the red vase or the blue vase?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vase on tables');

         Proxy.ExpectString('Red vase: You shake the red vase.');
         Proxy.ExpectString('Blue vase: You shake the blue vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vases on tables');

         Proxy.ExpectString('Red vase: You shake the red vase.');
         Proxy.ExpectString('Blue vase: You shake the blue vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vases on the tables');

         Proxy.ExpectString('Red vase: You shake the red vase.');
         Proxy.ExpectString('Blue vase: You shake the blue vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the vases on the tables');

         Proxy.ExpectString('Red vase: You shake the red vase.');
         Proxy.ExpectString('Blue vase: You shake the blue vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vases on tables');

         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the red vase on table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake red vase on the table');

         Proxy.ExpectString('Which table do you mean, the red table or the blue table?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the red vase on the table');

         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake red vase on table');

         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the red vase on tables');

         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake red vase on the tables');

         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the red vase on the tables');

         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake red vase on tables');

         Proxy.ExpectString('(the red vase)');
         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake vases that are red');

         Proxy.ExpectString('(the red vase)');
         Proxy.ExpectString('You shake the red vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all that are red and that are vases');

         Proxy.ExpectString('(the blue vase)');
         Proxy.ExpectString('You shake the blue vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from any of the tables THAT ARE NOT the red ones');

         Proxy.ExpectString('(first taking the blue vase)');
         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('Placed on the red table.');
         Proxy.ExpectString('');
         TestPlayer.Perform('put blue vase on red table');

         Proxy.ExpectString('You shake the blue vase.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake blue from red THAT IS NOT vase');

         Proxy.SkipEverything();
         TestPlayer.Perform('south');
         Proxy.StopSkipping();

         Proxy.ExpectString('Red grapes: You shake the red grapes.');
         Proxy.ExpectString('Green grapes: You shake the green grapes.');
         Proxy.ExpectString('Rotten grapes: You shake the rotten grapes.');
         Proxy.ExpectString('Red apple: You shake the red apple.');
         Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake grapes and all apples THAT ARE NOT green from plate on table');

         Proxy.ExpectString('Red grapes: You shake the red grapes.');
         Proxy.ExpectString('Green grapes: You shake the green grapes.');
         Proxy.ExpectString('Red apple: You shake the red apple.');
         Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake grapes and apples THAT ARE NOT green from plate on table');

         Proxy.ExpectString('Kitchen');
         Proxy.WaitUntilSubstring('On the fruit plate are green grapes.');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('look');
         Proxy.ExpectDone();

         Proxy.ExpectString('Red grapes: You shake the red grapes.');
         Proxy.ExpectString('Green grapes: You shake the green grapes.');
         Proxy.ExpectString('Rotten grapes: You shake the rotten grapes.');
         Proxy.ExpectString('Red apple: You shake the red apple.');
         Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
         Proxy.ExpectString('Banana: You shake the banana.');
         Proxy.ExpectString('Blueberries: You shake the blueberries.');
         Proxy.ExpectString('Pineapple: You shake the pineapple.');
         Proxy.ExpectString('Kiwi: You shake the kiwi.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake grapes and all apples from plate but green plus bananas and all berries THAT ARE NOT strawberries from box and pineapple and kiwi from crate');

         Proxy.ExpectString('Red grapes: You shake the red grapes.');
         Proxy.ExpectString('Green grapes: You shake the green grapes.');
         Proxy.ExpectString('Rotten grapes: You shake the rotten grapes.');
         Proxy.ExpectString('Red apple: You shake the red apple.');
         Proxy.ExpectString('Yellow apple: You shake the yellow apple.');
         Proxy.ExpectString('Banana: You shake the banana.');
         Proxy.ExpectString('Blueberries: You shake the blueberries.');
         Proxy.ExpectString('Pineapple: You shake the pineapple.');
         Proxy.ExpectString('Kiwi: You shake the kiwi.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake grapes, and all of the apples from plate but green, plus bananas, and all of the berries that are not strawberries from box, and pineapple from crate, and kiwi from crate');

         Proxy.WaitUntilString('Taken.');
         Proxy.WaitUntilString('Kitchen');
         Proxy.WaitUntilString('');
         TestPlayer.Perform('north and east and south and take sack and south and south and take bag of holding and north and north and north and west and south');
         Proxy.ExpectDone();

         Proxy.SkipEverything();
         TestPlayer.Perform('inventory; look');
         Proxy.StopSkipping();

         Proxy.ExpectString('Which bag do you mean, the black garbage bag, the elongated brown sack, or the embroidered bag of holding labeled Tester?');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine bag');

         Proxy.ExpectString('Placed in the garbage bag.');
         Proxy.ExpectString('');
         TestPlayer.Perform('put brown sack in black bag');

         Proxy.ExpectString('Which bag do you mean, the black garbage bag, the elongated brown sack, or the embroidered bag of holding labeled Tester?');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine bag');

         Proxy.ExpectString('Taken.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take garbage bag');

         Proxy.ExpectString('Which bag do you mean, the embroidered bag of holding labeled Tester, the black garbage bag, or the elongated brown sack?');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine bag');

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
         TestWorld.CheckDisposalQueue();
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

         Proxy.ExpectString('(the south archway)');
         Proxy.ExpectSubstring('pink');
         Proxy.ExpectString('');
         TestPlayer.Perform('examine all that is pink and that is arch');

         Proxy.ExpectString('The diamonds are part of the south archway.');
         Proxy.ExpectString('');
         TestPlayer.Perform('take diamonds from arch');

         Proxy.ExpectSubstring('Which arch do you mean, ');
         Proxy.ExpectString('');
         TestPlayer.Perform('take diamonds from the arch');

         Proxy.ExpectString('(the gold inlay and the diamonds)');
         Proxy.ExpectSubstring('Gold inlay: ');
         Proxy.ExpectSubstring('Diamonds: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from one of the arches that is not the north arch');

         Proxy.ExpectSubstring('Small white geometric shapes: ');
         Proxy.ExpectSubstring('Wood: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from one of the arches that is the north arch');

         Proxy.ExpectSubstring('Small white geometric shapes: ');
         Proxy.ExpectSubstring('Wood: ');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from one of the arches that is the north arch and is wood');

         Proxy.ExpectString('You used the term "and that are" in a way I don''t understand.');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake all from one of the arches that is an arch and are pink');

         Proxy.ExpectString('Which diamond from one of the arches that is an arch and that is pink do you mean, the diamond-studded gold inlay of the south archway or the circle-and-cross diamonds of the south archway?');
         Proxy.ExpectString('');
         TestPlayer.Perform('shake the one diamond from one of the arches that is an arch and is pink');

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
   Failed: Boolean;

   procedure RunMatcherTest(TestMatcher: TMatcher; Candidate: TTokens; Start: Cardinal; Pass: Cardinal);
   var
      Result: Cardinal;
   begin
      Inc(TestID);
      Result := TestMatcher.Matches(Candidate, Start);
      if (Result <> Pass) then
      begin
         Writeln('FAILED matcher test ', TestID, '; expected to match ', Pass, ' tokens but matched ', Result);
         Failed := True;
      end;
   end;

   procedure RunCanonicalMatchTest(TestMatcher: TMatcher; Pass: AnsiString);
   var
      Result: AnsiString;
   begin
      Inc(TestID);
      Result := TestMatcher.GetCanonicalMatch(' ');
      if (Result <> Pass) then
      begin
         Writeln('FAILED matcher test ', TestID, '; expected to find longest match "', Pass, '" but got "', Result, '"');
         Failed := True;
      end;
   end;

var
   TestMatcher, OtherMatcher: TMatcher;
   Strings: TTokens;
begin
   Writeln('PATTERN COMPILER');

   Failed := False;

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

   if (Failed) then Halt(1);
end;

type
   TMolecule = class(TSpade)
   end;

type
   TMoleculeEnumerator = specialize TGenericStorableListEnumerator<TMolecule>;
   TMoleculeList = specialize TStorableList<TMolecule, TMoleculeEnumerator>;

procedure TestLists();
var
   List1, List2: TMoleculeList;
   Mol1, Mol2, Mol3: TMolecule;
   Enum1, Enum2: TMoleculeEnumerator;
   Failed: Boolean;
   TestCount: Cardinal;

   procedure Check(Success: Boolean; S: AnsiString);
   begin
      Inc(TestCount);
      if (not Success) then
      begin
         Writeln('Test #', TestCount, ': ', S);
         Failed := True;
      end;
   end;

begin
   Writeln('LISTS');
   Failed := False;
   TestCount := 0;
   Mol1 := TMolecule.Create();
   Mol2 := TMolecule.Create();
   Mol3 := TMolecule.Create();
   List1 := TMoleculeList.Create([slOwner]);
   List1.AppendItem(Mol1);
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List2 := TMoleculeList.Create([slOwner]);
   Enum1 := List2.GetEnumerator();
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   List2.AdoptItem(Enum1);
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List1.GetEnumerator();
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List2.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List1.AppendItem(Mol2);   
   List2.AdoptList(List1);
   Enum1 := List1.GetEnumerator();
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Enum1 := List2.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol2, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List2.AppendItem(Mol3);
   Enum1 := List2.GetEnumerator(tdForward);
   Check(Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Check(Enum1.Current = Mol1, 'Test after adding Mol3 failed');
   Check(Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Check(Enum1.Current = Mol2, 'Test after adding Mol3 failed');
   Check(Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Check(Enum1.Current = Mol3, 'Test after adding Mol3 failed');
   Check(not Enum1.MoveNext(), 'Test after adding Mol3 failed');
   Enum1.Free();
   Enum1 := List2.GetEnumerator(tdForward);
   Check(Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.Current = Mol1, 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.Current = Mol2, 'Test for adopting Mol2 from the middle failed');
   List1.AdoptItem(Enum1);
   Check(Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Check(Enum1.Current = Mol3, 'Test for adopting Mol2 from the middle failed');
   Check(not Enum1.MoveNext(), 'Test for adopting Mol2 from the middle failed');
   Enum1.Free();
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test checking Mol2 got adopted failed');
   Check(Enum1.Current = Mol2, 'Test checking Mol2 got adopted failed');
   Check(not Enum1.MoveNext(), 'Test checking Mol2 got adopted failed');
   Enum1.Free();

   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'List adoption test failed - setup');
   Check(Enum1.Current = Mol2, 'List adoption test failed - setup');
   Enum2 := List2.GetEnumerator();
   Check(Enum2.MoveNext(), 'List adoption test failed - setup');
   Check(Enum2.Current = Mol1, 'List adoption test failed - setup');
   Enum1.Free(); { can't have any live enumerators during adoption }
   List2.AdoptList(List1);
   Check(Enum2.MoveNext(), 'List adoption test failed - post adoption, pre traversal');
   Check(Enum2.Current = Mol3, 'List adoption test failed - post adoption, pre traversal: Mol3 missing');
   Check(Enum2.MoveNext(), 'List adoption test failed - post adoption, failed to traverse');
   Check(Enum2.Current = Mol2, 'List adoption test failed - post adoption: Mol2 missing');
   List1.AdoptItem(Enum2);
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol2, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();
   Check(not Enum2.MoveNext(), 'Test failed');
   Enum2.Free();
   Enum2 := List2.GetEnumerator();
   Check(Enum2.MoveNext(), 'Test failed');
   Check(Enum2.Current = Mol1, 'Test failed');
   List1.AdoptItem(Enum2);
   Enum2.Free();
   Enum1 := List1.GetEnumerator();
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol2, 'Test failed');
   Check(Enum1.MoveNext(), 'Test failed');
   Check(Enum1.Current = Mol1, 'Test failed');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   List2.AdoptList(List1);
   Enum1 := List2.GetEnumerator(tdReverse);
   Check(Enum1.MoveNext(), 'Reverse test failed - List2 empty');
   Check(Enum1.Current = Mol1, 'Reverse test failed - Mol1 not at end');
   Check(Enum1.MoveNext(), 'Reverse test failed');
   Check(Enum1.Current = Mol2, 'Reverse test failed - Mol2 not in middle');
   Check(Enum1.MoveNext(), 'Reverse test failed');
   Check(Enum1.Current = Mol3, 'Reverse test failed - Mol3 not at start');
   Check(not Enum1.MoveNext(), 'Test failed');
   Enum1.Free();

   { molecules get freed by lists }
   List1.Free();
   List2.Free();
   if (Failed) then Halt(1);
end;

begin
   Writeln('CuddlyWorld Tests initializing...');
   {$IFDEF DEBUG} Writeln('CuddlyWorld debugging enabled.'); {$ENDIF}
   TestMatcher();
   TestLists();
   TestMechanics();
   TestPlot();
   Writeln('CuddlyWorld Tests complete.');
end.