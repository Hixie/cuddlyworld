{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit cuddlycamp;

interface

uses
   world;

const
   kWorldFileName = 'world.dat';
   kSaveDataVersion = 1;

function InitEden: TWorld; { create the initial locations }

implementation

uses
   storable, grammarian, locations, thingdim, things, player, broadcast, sysutils;

type
   TCuddlyWorld = class(TWorld)
    protected
      FStartingLocation: TAtom;
      FStartingPosition: TThingPosition;
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure AddPlayer(Avatar: TAvatar); override;
      property StartingLocation: TAtom read FStartingLocation write FStartingLocation;
      property StartingPosition: TThingPosition read FStartingPosition write FStartingPosition;
   end;

   TGenderArchway = class(TLocationProxy)
    protected
      FGender: TGender;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Gender: TGender; Destination: TLocation);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); override;
   end;

const
   magicA = #226#152#128;
   magicB = #226#152#129;
   magicC = #226#152#130;
   magicD = #226#152#131;
   magicE = #226#152#132;
   magicF = #226#152#133;
   magicG = #226#152#134;
   magicH = #226#152#135;
   magicI = #226#152#136;
   magicJ = #226#152#137;
   magicK = #226#152#138;
   magicL = #226#152#139;
   magicM = #226#152#140;
   magicN = #226#152#141;
   magicO = #226#152#142;
   magicP = #226#152#143;
   magicQ = #226#152#144;
   magicR = #226#152#145;
   magicS = #226#152#146;
   magicT = #226#152#147;
   magicU = #226#152#148;
   magicV = #226#152#149;
   magicW = #226#152#150;
   magicX = #226#152#151;
   magicY = #226#152#152;
   magicZ = #226#152#153;

constructor TCuddlyWorld.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FStartingLocation));
   FStartingPosition := TThingPosition(Stream.ReadCardinal());
end;

procedure TCuddlyWorld.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FStartingLocation);
   Stream.WriteCardinal(Cardinal(FStartingPosition));
end;

procedure TCuddlyWorld.AddPlayer(Avatar: TAvatar);
begin
   Assert(Assigned(FStartingLocation));
   Assert(FStartingPosition >= Low(TThingPosition));
   Assert(FStartingPosition <= High(TThingPosition));
   FStartingLocation.Add(Avatar, FStartingPosition);
   inherited;
end;


constructor TGenderArchway.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Gender: TGender; Destination: TLocation);
begin
   inherited Create(Name, Pattern, Description, Destination);
   FGender := Gender;
end;

constructor TGenderArchway.Read(Stream: TReadStream);
begin
   inherited;
   FGender := TGender(Stream.ReadCardinal());
end;

procedure TGenderArchway.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FGender));
end;

procedure TGenderArchway.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
var
   Subject: TPlayer;
begin
   inherited;
   if (Traveller is TPlayer) then
   begin
      Subject := (Traveller as TPlayer);
      if (Subject.Gender <> FGender) then
      begin
         Subject.Gender := FGender;
         case FGender of
           gMale: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('becomes'), M('become')), SP, M('a man.')]);
           gFemale: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('becomes'), M('become')), SP, M('a woman.')]);
           gThirdGender: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('becomes'), M('become')), SP, M('a person.')]);
           gRobot: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('becomes'), M('become')), SP, M('a robot.')]);
           gOrb: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('becomes'), M('become')), SP, M('an orb.')]);
           gHive: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('becomes'), M('become')), SP, M('a hive mind.')]);
          else
            Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
         end;
      end;
   end;
end;


function InitEden: TWorld;
var
   World: TCuddlyWorld;
   SkyBox: TLocation;
   Sky, Sun: TThing;

   procedure CreateStartingArea;

      procedure AddTheSlideHole(Path, SlideDispatchRoom: TLocation);
      var
         Thing: TOpening;
      begin
         Thing := TOpening.Create('hole in the ground', '((hole/holes (in the ground)?) (slide/slides opening?) (opening/openings))@', 'The hole looks like the entrance to a fun slide at an amusement park. Some light shines up from inside.', SlideDispatchRoom, tsMassive);
         Thing.CannotMoveExcuse := 'You can''t move a hole.';
         Thing.FindDescription := 'The hole is in the ground.';
         Path.GetSurface().Add(Thing, tpDirectionalOpening);
      end;

      procedure PopulateTrees(Location: TLocation; Directions: TCardinalDirectionSet = cdCompasDirection);
      var
         Direction: TCardinalDirection;
         Tree: TThing;
      begin
         for Direction in Directions do
         begin
            if (not Location.HasLandmark(Direction)) then
            begin
               Tree := TTree.Create('tree', 'tree/trees', 'This tree looks like any other tree.', tmLudicrous, tsGigantic);
               Location.GetSurface().Add(Tree, tpPlantedInImplicit);
               Location.AddLandmark(Direction, Tree, []);
            end;
         end;
      end;

   var
      ArrivalsCircle, MalePath1, MalePath2, FemalePath1, FemalePath2, ThirdGenderPath1, ThirdGenderPath2, HivePath1, HivePath2, RobotPath1, RobotPath2,
      SlideDispatchRoom: TLocation;
      ArrivalsPedestal, PavedPath, Thing: TThing;
      MaleArchway, FemaleArchway, ThirdGenderArchway, HiveArchway, RobotArchway: TGenderArchway;
   begin

      { Locations }
      ArrivalsCircle := TSurfaceNamedLocation.Create('Arrivals Circle', 'the arrivals circle', 'an arrivals circle', 'The arrivals circle is where all the visitors to Cuddly World first appear. Well-worn paths lead to the north and south under decorated archways; a well-paved, but less worn, path leads to the east under a similar archway. '+'Large signs staked into the ground point elaborately to the north and south paths. '+'A large stone serves as a sign next to the east archway. '+'To the west, a smaller sign is staked into the middle of some weeds next to an apparently abandoned archway. '+'To the southwest are another sign and archway, the sign in an even more dilapidated state. Beyond, in all directions, you see an impenetrable forest.', CreateEarthSurface());
      MalePath1 := TSurfaceNamedLocation.Create('Male Path', 'the male path', 'a male path', 'The male path meanders from the south to the west.', CreateEarthSurface());
      MalePath2 := TSurfaceNamedLocation.Create('Male Clearing', 'the male clearing', 'a male clearing', 'The forest thins out, leaving a circular clearing.', CreateStoneSurface());
      FemalePath1 := TSurfaceNamedLocation.Create('Female Path', 'the female path', 'a female path', 'The female path meanders from the north to the east.', CreateEarthSurface());
      FemalePath2 := TSurfaceNamedLocation.Create('Female Clearing', 'the female clearing', 'a female clearing', 'The forest thins out, leaving a circular clearing.', CreateStoneSurface());
      ThirdGenderPath1 := TSurfaceNamedLocation.Create('Third-Gender Path', 'the third-gender path', 'a third-gender path', 'The third-gender path meanders from the east to the north.', CreateEarthSurface());
      ThirdGenderPath2 := TSurfaceNamedLocation.Create('Third-Gender Clearing', 'the third-gender clearing', 'a third-gender clearing', 'The forest thins out, leaving a circular clearing.', CreateStoneSurface());
      HivePath1 := TSurfaceNamedLocation.Create('Hive Path', 'the hive path', 'an hive path', 'The hive path meanders from the west to the south.', CreateEarthSurface());
      HivePath2 := TSurfaceNamedLocation.Create('Hive Clearing', 'the hive clearing', 'an hive clearing', 'The forest thins out, leaving a circular clearing.', CreateStoneSurface());
      RobotPath1 := TSurfaceNamedLocation.Create('Robot Path', 'the robot path', 'a robot path', 'The robot path meanders from the northeast to the southeast.', CreateEarthSurface());
      RobotPath2 := TSurfaceNamedLocation.Create('Robot Clearing', 'the robot clearing', 'a robot clearing', 'The forest thins out, leaving a circular clearing.', CreateStoneSurface());

      SlideDispatchRoom := ArrivalsCircle; // make SlideDispatchRoom here

      { ArrivalsCircle }

      ArrivalsPedestal := TStaticThing.Create('stone pedestal', '((big glowing stone ((twelve-pointed twelve-point (twelve (pointed point)@))@ star)&)# arrivals (pedestal/pedestals slab/slabs)@)&',
                                              'The arrivals pedestal is a big stone slab, in the shape of a twelve-pointed star, over which you materialised when you arrived in Cuddly World. Glowing sigils are engraved at each point of the star, and a circle is engraved around the center.', tmLudicrous, tsGigantic);
      ArrivalsPedestal.Add(TFeature.Create('glowing engraved sigils', '((glowing engraved (deep? magic))# (sigil/sigils engraving/engravings)@)&', 'The sigils are of a deep magic. '+'Each point has a symbol engraved in it. There are eight unique symbols; two are repeated twice, and one presumably important symbol is repeated three times. '+'Starting from the northern-most point of the star and going around the circle in a clockwise direction, you see the following symbols: '+magicA+magicL+magicM+magicA+magicG+magicI+magicC+magicA+magicR+magicR+magicI+magicV), tpPartOfImplicit);
      ArrivalsPedestal.Add(TFeature.Create('engraved circle', '((center engraved)# (circle/circles engraving/engravings)@)&', 'The circle is engraved around the center of the pedestal.'), tpPartOfImplicit);
      ArrivalsCircle.GetSurface().Add(ArrivalsPedestal, tpAt);

      MaleArchway := TGenderArchway.Create('north archway', '(((((navy dark)@ blue)& painted Lancet (wooden wood)@)# ((archway/archways arch/arches)@ (to the (north n)@)?))& '+
                                                             '((((navy dark)@ blue)& painted Lancet (wooden wood)@ (north northern n)@)# (archway/archways arch/arches)@)&)@', 'The north archway is a Lancet arch made of painted wood. It is a predominantly dark blue affair, with small white geometric shapes (primarily circles and arrows, all pointing diagonally upwards and to the right) painted on its tall columns.', gMale, MalePath1);
      MaleArchway.CannotMoveExcuse := 'The archway seems to have been firmly embedded in the ground.';
      MaleArchway.FindDescription := 'The archway is to the north, over a path.';
      MaleArchway.Add(TFeature.Create('small white geometric shapes', '((small white geometric)# shape/shapes)&', 'The geometric shapes are small white circles painted against the blue wooden arch. Each circle has an arrow pointing out of the circle at the top left. Each arrow is the same length as the diameter of the circle from which it extends.'), tpPartOfImplicit);
      MaleArchway.Add(TFeature.Create('wood', '((stiff painted blue)# wood/wood)&', 'The northern archway''s wood is painted blue with small white geometric shapes. The wood is stiff.'), tpPartOfImplicit);
      ArrivalsCircle.GetSurface().Add(MaleArchway, tpAtImplicit);

      FemaleArchway := TGenderArchway.Create('south archway', '(((((light? pink) light-pink)@ thick (semicircular semi-circular)@ crystal)# (archway/archways arch/arches)@ (to the (south s)@))& '+
                                                               '((((light? pink) light-pink)@ thick (semicircular semi-circular)@ crystal (southern south s)@)# (archway/archways arch/arches)@)&)@',
                                             'The south archway is a thick, semicircular arch made of a light-pink crystal. At the top of the arch is a gold inlay, which itself is decorated with a diamond-studded circle above a diamond-studded cross.', gFemale, FemalePath1);
      FemaleArchway.CannotMoveExcuse := 'The archway seems to have been firmly embedded in the ground.';
      FemaleArchway.FindDescription := 'The archway is to the south, over a path.';
      FemaleArchway.Add(TFeature.Create('gold inlay', '(((diamond-studded (diamond studded)&)@ gold)# inlay/inlays)&', 'The gold is inlaid in the crystal archway. Diamonds are studded in the gold inlay, forming a circle and a cross.'), tpPartOfImplicit);
      FemaleArchway.Add(TFeature.Create('diamonds', '(((circle-and-cross (circle and cross))@ diamond/diamonds)& '+
                                                    '((diamond-studded (diamond studded?))@ (circle-and-cross/circles-and-crosses (circle/circles and cross/crosses) circle/circles cross/crosses)@)& '+
                                                    '((circle-and-cross/circles-and-crosses (circle/circles and cross/crosses) circle/circles cross/crosses)@ (of (diamond diamonds)@))&)@',
                                        'The diamonds reflect the light perfectly. They are embedded in the gold inlay, arranged so as to form a perfect circle with a perfect square cross underneath. The top of the top arm of the cross touches the bottom of the circle.'), tpPartOfImplicit);
      ArrivalsCircle.GetSurface().Add(FemaleArchway, tpAtImplicit);

      ThirdGenderArchway := TGenderArchway.Create('east archway', '((three-foiled cusped ((archway/archways arch/arches)@ (of stone)? (to the (east e)@)?))& '+
                                                                  '(((three-foiled cusped)& stone)# ((archway/archways arch/arches)@ (to the (east e)@)?))& '+
                                                                  '(((eastern east e)@ (three-foiled cusped)& stone)# (archway/archways arch/arches)@)& '+
                                                                  '((eastern east e)@ (three-foiled cusped)& ((archway/archways arch/arches)@ (of stone)?))&)@',
                                                  'The east archway is a three-foiled cusped arch made of stone. Carved in the face of the arch are a series of human figures cooking, building huts, hunting, playing, digging, and so forth.', gThirdGender, ThirdGenderPath1);
      ThirdGenderArchway.CannotMoveExcuse := 'The archway seems to have been firmly embedded in the ground.';
      ThirdGenderArchway.FindDescription := 'The archway is to the east, over a well-paved path.';
      ThirdGenderArchway.Add(TFeature.Create('human figures', '((carved human figure/figures)& (human figure carving/carvings)&)@', 'The carvings appear to represent humans partaking in typical day-to-day activities. The recognisable acts depicted are the cooking of a meal, the building of wooden huts, hunting, children playing, the digging of a hole, and fornication.'), tpPartOfImplicit);
      ArrivalsCircle.GetSurface().Add(ThirdGenderArchway, tpAtImplicit);

      HiveArchway := TGenderArchway.Create('west archway', '((((stainless steel)& horseshoe)# ((archway/archways arch/arches)@ (to the (west w)@)?))& ' +
                                                           '(((stainless steel)& horseshoe (western west w)@)# (archway/archways arch/arches)@)&)@',
                                           'The west archway is a horseshoe arch made of a series of adjacent bars of stainless steel, buried under tension.', gHive, HivePath1);
      HiveArchway.CannotMoveExcuse := 'The archway seems to have been firmly embedded in the ground.';
      HiveArchway.FindDescription := 'The archway is to the west.';
      HiveArchway.Add(TFeature.Create('bars', '(((series of)? (buried adjacent)# bars (of (stainless steel)&)?) '+
                                              '(buried stainless steel bar/bars)& '+
                                              '((stainless steel)& bar/bars (buried under tension))&)@',
                                      'The stainless steel bars are bent into a horseshoe shape and kept in place by virtue of being buried in the ground.'), tpPartOfImplicit);
      ArrivalsCircle.GetSurface().Add(HiveArchway, tpAtImplicit);

      RobotArchway := TGenderArchway.Create('southwest archway', '(((shining (silicon metalloid)& (inverted catenary)&)# ((archway/archways arch/arches)@ (to the (southwest (south west) sw)@)?))& '+
                                                                 '(((southwestern (south western) southwest (south west) sw)@ shining (silicon metalloid)& (inverted catenary)&)# (archway/archways arch/arches)@)&)@',
                                            'The southwest archway is an inverted catenary arch precisely carved into a block of shining silicon metalloid. Along the rim of the arch are many dots. '+'The archway itself is in good condition, but the area surrounding it appears to be unmaintained and rarely travelled.', gRobot, RobotPath1);
      RobotArchway.CannotMoveExcuse := 'The archway seems to have been firmly embedded in the ground.';
      RobotArchway.FindDescription := 'The archway is to the southwest.';
      Thing := TFeature.Create('rim', '(shining silicon metalloid)* rim/rims', 'The rim of the carved silicon metalloid is very smooth and shiny. It follows an inverted catenary shape. The rim is decorated with dots.');
      Thing.Add(TFeature.Create('dots', '(((regularly? spaced) (large small (large and small))@)# rim dot/dots)&', 'The dots are spaced regularly along the rim of the arch. Some of the dots are large and some small, but there is no obvious pattern: '#226#131#175#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175#226#131#175#226#131#175#226#131#175#226#128#162#226#128#162#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175#226#131#175#226#131#175#226#131#175#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175), tpPartOfImplicit);
      RobotArchway.Add(Thing, tpAmbiguousPartOfImplicit);
      ArrivalsCircle.GetSurface().Add(RobotArchway, tpAtImplicit);

      Thing := TScenery.Create('weeds', 'weed/weeds', 'The weeds have overrun the archways to the west and southwest.');
      (Thing as TScenery).CannotMoveExcuse := 'The weeds have evolved very strong roots. It would take some sort of inhuman hive-mind/robot alliance to clean them up.';
      (Thing as TScenery).FindDescription := 'The weeds are around the archways to the west and southwest.';
      ArrivalsCircle.GetSurface().Add(Thing, tpAroundImplicit);

      Thing := TScenery.Create('north-leading path', '((well-worn (path/paths (leading (north n)@)?))& ((well-worn (north-leading n-leading ((north n)@ leading?))@)# path/paths)&)@',
                               'The path leading north appears to be oft frequented. It enters the north archway and continues beyond.');
      (Thing as TScenery).FindDescription := 'The path leads north through an archway.';
      ArrivalsCircle.GetSurface().Add(Thing, tpPartOfImplicit);

      Thing := TScenery.Create('south-leading path', '((well-worn (path/paths (leading (south s)@)?))& ((well-worn (south-leading s-leading ((south s)@ leading?))@)# path/paths)&)@',
                               'The path leading south appears to be oft frequented. It enters the south archway and continues beyond.');
      (Thing as TScenery).FindDescription := 'The path leads south through an archway.';
      ArrivalsCircle.GetSurface().Add(Thing, tpPartOfImplicit);

      PavedPath := TScenery.Create('east-leading path', '(((well-paved (well? paved))@ (path/paths (leading (east e)@)?))& (((well-paved (well? paved))@ (east-leading e-leading ((east e)@ leading?))@)# path/paths)&)@',
                                                      'The path leading east is paved with stone. It leads to the east archway. A stone sits to the side of the path.');
      PavedPath.Add(TFeature.Create('paving stones', '(paving stone/stones)&', 'The paving stones of the east-leading path are firmly embedded in the ground.'), tpPartOfImplicit);
      (PavedPath as TScenery).FindDescription := 'The path leads east through an archway.';
      ArrivalsCircle.GetSurface().Add(PavedPath, tpAtImplicit);

      Thing := TSign.Create('north-pointing sign', '((((north-pointing n-pointing ((north n)@ pointing?) northern)@ thick (wooden wood)@)# sign/signs)& '+
                                                    '(((thick (wooden wood)@)# sign/signs)& (to the (north n)@)?))@',
                            'The sign is made of thick wood. Its head is shaped like an arrow; a short word is engraved on its face. It is planted near the northern archway, and points into it.', 'Men', tmHeavy, tsBig);
      (Thing as TSign).UnderDescription := 'There is nothing unusual on the underside of the north-pointing sign.';
      (Thing as TSign).FindDescription := 'The north-pointing sign is by the north archway and the well-worn path that leads into it.';
      (Thing as TSign).CannotMoveExcuse := 'The sign is remarkably firmly planted in the ground.';
      ArrivalsCircle.GetSurface().Add(Thing, tpAtImplicit);

      Thing := TSign.Create('sign to the south', '((((((light? pink) light-pink)@ crystal)# sign/signs)& (to the (south s)@)?) '+
                                                  '(((south-pointing s-pointing ((south s)@ pointing?) southern)@ ((light? pink) light-pink)@ crystal)# sign/signs)&)@',
                            'The sign, a rectangular slab of pink crystal, matches the style of the archway next to which it is planted, to the south of the arrival circle.',
                            'Girls and Ladies are cordially invited to enter through the southern archway.', tmHeavy, tsBig);
      (Thing as TSign).UnderDescription := 'The underside of the sign to the south is as pretty as the rest of the pink crystal sign.';
      (Thing as TSign).FindDescription := 'The sign to the south stands proud next to the path leading through the south archway.';
      (Thing as TSign).CannotMoveExcuse := 'The sign is remarkably firmly planted in the ground.';
      ArrivalsCircle.GetSurface().Add(Thing, tpAtImplicit);

      Thing := TSign.Create('stone to the east', '((((big)# stone/stones)& (to the (east e)@)?) '+
                                                  '(((big stone)# sign/signs)& (to the (east e)@)?) '+
                                                  '((big (east-pointing e-pointing ((east e)@ pointing?) eastern)@ stone)# sign/signs)& '+
                                                  '(big (east eastern e)@ stone/stones)&)@',
                            'Next to the eastern archway is a big stone, with writing carved into it and an arrow pointing at the aforementioned archway.',
                            'Members of the Third Gender', tmPonderous, tsBig);
      (Thing as TSign).UnderDescription := 'You cannot see under the stone to the east, but it appears that the paving continues under the stone.';
      (Thing as TSign).FindDescription := 'The stone to the east sits on the paving leading through the east archway.';
      (Thing as TSign).CannotMoveExcuse := 'The stone won''t budge.'; { it's actually nailed to the ground }
      PavedPath.Add(Thing, tpAtImplicit);

      Thing := TSign.Create('west-pointing sign', '((((west-pointing w-pointing ((west w)@ pointing?) western)@ (stainless-steel (stainless? steel))@)# sign/signs)& '+
                                                   '(((stainless-steel (stainless? steel))@ sign/signs)& (to the (west w)@)?))@',
                            'The sign, made of stainless-steel, is in the form of a simple arrow pointing west, with text printed neatly centered in small print. Weeds surround the sign.',
                            'Hive-minds enter here.', tmLight, tsBig);
      (Thing as TSign).UnderDescription := 'Under the west-pointing sign you find more weeds.';
      (Thing as TSign).FindDescription := 'The west-pointing sign is by the west archway, planted in a patch of weeds.';
      (Thing as TSign).CannotMoveExcuse := 'The sign is remarkably firmly planted in the ground.';
      ArrivalsCircle.GetSurface().Add(Thing, tpAtImplicit);

      Thing := TSign.Create('southwest-pointing sign', '((((southwest-pointing south-west-pointing sw-pointing ((southwest (south west) sw)@ pointing?) southwestern (south western))@ shining (silicon metalloid)&)# sign/signs)& '+
                                                        '(((shining (silicon metalloid)&)# sign/signs)& (to the (southwest (south west) sw)@)?))@',
                                                   'A sign made of shining silicon metalloid stands next to the southwest archway, with text written in an OCR-optimised font. Weeds have overrun the base of the sign.',
                                                   'ROBOTS', tmLight, tsBig);
      (Thing as TSign).UnderDescription := 'Under the southwest-pointing sign you find more weeds.';
      (Thing as TSign).FindDescription := 'The southwest-pointing sign is by the southwest archway amidst rampant weeds.';
      (Thing as TSign).CannotMoveExcuse := 'The sign is remarkably firmly planted in the ground.';
      ArrivalsCircle.GetSurface().Add(Thing, tpAtImplicit);

      ArrivalsCircle.AddLandmark(cdNorth, MaleArchway, [loReachable, loAutoDescribe]);
      ArrivalsCircle.AddLandmark(cdEast, ThirdGenderArchway, [loReachable, loAutoDescribe]);
      ArrivalsCircle.AddLandmark(cdSouth, FemaleArchway, [loReachable, loAutoDescribe]);
      ArrivalsCircle.AddLandmark(cdWest, HiveArchway, [loReachable, loAutoDescribe]);
      ArrivalsCircle.AddLandmark(cdSouthWest, RobotArchway, [loReachable, loAutoDescribe]);
      PopulateTrees(ArrivalsCircle);

      { Slide Dispatch Room }
      // ...

      // XXXX the archways need to be in their own rooms and have sides and stuff

      { Paths }
      ConnectLocations(ArrivalsCircle, cdNorth, MalePath1);
      ConnectLocations(MalePath1, cdWest, MalePath2);
      AddTheSlideHole(MalePath2, SlideDispatchRoom);
      ConnectLocations(ArrivalsCircle, cdSouth, FemalePath1);
      ConnectLocations(FemalePath1, cdEast, FemalePath2);
      AddTheSlideHole(FemalePath2, SlideDispatchRoom);
      ConnectLocations(ArrivalsCircle, cdEast, ThirdGenderPath1);
      ConnectLocations(ThirdGenderPath1, cdNorth, ThirdGenderPath2);
      AddTheSlideHole(ThirdGenderPath2, SlideDispatchRoom);
      ConnectLocations(ArrivalsCircle, cdWest, HivePath1);
      ConnectLocations(HivePath1, cdSouth, HivePath2);
      AddTheSlideHole(HivePath2, SlideDispatchRoom);
      ConnectLocations(ArrivalsCircle, cdSouthWest, RobotPath1);
      ConnectLocations(RobotPath1, cdSouthWest, RobotPath2);
      AddTheSlideHole(RobotPath2, SlideDispatchRoom);

      { Trees and Skies }
      PopulateTrees(ArrivalsCircle);
      ArrivalsCircle.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(MalePath1);
      MalePath1.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(MalePath2);
      MalePath2.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(FemalePath1);
      FemalePath1.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(FemalePath2);
      FemalePath2.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(ThirdGenderPath1);
      ThirdGenderPath1.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(ThirdGenderPath2);
      ThirdGenderPath2.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(HivePath1);
      HivePath1.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(HivePath2);
      HivePath2.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(RobotPath1);
      RobotPath1.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
      PopulateTrees(RobotPath2);
      RobotPath2.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);

      { Register locations }
      World.AddLocation(ArrivalsCircle);
      World.StartingLocation := ArrivalsPedestal;
      World.StartingPosition := tpOn;
      World.AddLocation(MalePath1);
      World.AddLocation(MalePath2);
      World.AddLocation(FemalePath1);
      World.AddLocation(FemalePath2);
      World.AddLocation(ThirdGenderPath1);
      World.AddLocation(ThirdGenderPath2);
      World.AddLocation(HivePath1);
      World.AddLocation(HivePath2);
      World.AddLocation(RobotPath1);
      World.AddLocation(RobotPath2);

      { temporary }
      RobotPath1.GetSurface().Add(TSpade.Create(), tpOn);
      RobotPath1.GetSurface().Add(TStaticThing.Create('penny', '(copper (penny/pennies coin/coins)@)&', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);
      RobotPath1.GetSurface().Add(TStaticThing.Create('penny', '(copper (penny/pennies coin/coins)@)&', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);
      RobotPath1.GetSurface().Add(TStaticThing.Create('penny', '(copper (penny/pennies coin/coins)@)&', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);
      RobotPath1.GetSurface().Add(TStaticThing.Create('farthing', '(brass (farthing/farthings coin/coins)@)&', 'The farthing is a brass coin of little value.', tmLight, tsSmall), tpOn);
      RobotPath1.GetSurface().Add(TStaticThing.Create('farthing', '(brass (farthing/farthings coin/coins)@)&', 'The farthing is a brass coin of little value.', tmLight, tsSmall), tpOn);
      RobotPath1.GetSurface().Add(TStaticThing.Create('farthing', '(brass (farthing/farthings coin/coins)@)&', 'The farthing is a brass coin of little value.', tmLight, tsSmall), tpOn);
   end;

begin
   World := TCuddlyWorld.Create();

   Sky := TScenery.Create('sky', 'blue? sky/skies', 'The sky is clear.');
   Sun := TScenery.Create('sun', '(sun/suns star/stars)@', 'The sun is bright.');
   (Sky as TScenery).Opened := True;
   Sky.Add(Sun, tpEmbedded);
   SkyBox := TBackdrop.Create(Sky, tpAtImplicit);
   World.AddLocation(SkyBox);

   CreateStartingArea();
   Result := World;
end;

initialization
   RegisterStorableClass(TCuddlyWorld,   100000);
   RegisterStorableClass(TGenderArchway, 100001);
end.
