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
      FGender: TGender;
    public
      constructor Create(Name: AnsiString; Description: AnsiString; Gender: TGender; Destination: TLocation);
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
   Stream.ReadReference(@FStartingLocation);
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


constructor TGenderArchway.Create(Name: AnsiString; Description: AnsiString; Gender: TGender; Destination: TLocation);
begin
   inherited Create(Name, Description, Destination);
   FGender := Gender;
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
           gMale: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('become'), M('becomes')), SP, M('a man.')]);
           gFemale: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('become'), M('becomes')), SP, M('a woman.')]);
           gThirdGender: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('become'), M('becomes')), SP, M('a person.')]);
           gRobot: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('become'), M('becomes')), SP, M('a robot.')]);
           gOrb: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('become'), M('becomes')), SP, M('a robot.')]);
           gHive: DoBroadcast([Traveller], nil, [C(M(@Traveller.GetDefiniteName)), SP, MP(Traveller, M('become'), M('becomes')), SP, M('a hive mind.')]);
          else
            raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
         end;
      end;
   end;
end;


function InitEden: TWorld;
var
   World: TCuddlyWorld;
   ArrivalsCircle, MalePath, FemalePath: TLocation;
   Forest, MaleArchway, FemaleArchway, ThirdGenderArchway, HiveArchway, RobotArchway, ArrivalsPedestal,
   Thing: TThing;
begin
   World := TCuddlyWorld.Create();

   { Locations }
   ArrivalsCircle := TFeaturelessOutdoorLocation.Create('Arrivals Circle', 'the arrivals circle', 'an arrivals circle', 'The arrivals circle is where all the visitors to Cuddly World first appear. Well-worn paths lead to the north and south under decorated archways; a well-paved, but less worn, path leads to the west under a similar archway. '+'Large signs staked into the ground point elaborately to these paths. To the west, a smaller sign is staked into the middle of some weeds next to an apparently abandoned archway. '+'To the southwest are another sign and archway, the sign in an even more dilapidated state. In the distance to the '+'west is a mountain. In other directions, you see an impenetrable forest.');
   MalePath := TFeaturelessOutdoorLocation.Create('Male Path', 'the male path', 'a male path', 'The male path meanders from the south to the east.');
   FemalePath := TFeaturelessOutdoorLocation.Create('Female Path', 'the female path', 'a female path', 'The female path meanders from the north to the east.');

   { ArrivalsCircle }
   ArrivalsCircle.Add(TDistantScenery.Create('mountain', cdEast), tpAroundImplicit);
   Forest := TScenery.Create('forest', '((forest/forests (of trees)?) (tree forest/forests)& tree/trees)@', 'The forest is dense and impassable.');
   ArrivalsCircle.Add(Forest, tpAroundImplicit);

   MaleArchway := TGenderArchway.Create('north archway', '(((((navy dark)@ blue)& painted Lancet (wooden wood)@)# ((archway/archways arch/arches)@ (to the (north n)@)?))& ((((navy dark)@ blue)& painted Lancet (wooden wood)@ (north northern n)@)# (archway/archways arch/arches)@)&)@', 'The north archway is a Lancet arch made of painted wood. It is a predominantly dark blue affair, with small white geometric shapes (primarily circles and arrows, all pointing diagonally upwards and to the right) painted on its tall columns.', gMale, MalePath);
   MaleArchway.Add(TScenery.Create('small white geometric shapes', '((small white geometric)# shape/shapes)&', 'The geometric shapes are small white circles painted against the blue wooden arch. Each circle has an arrow pointing out of the circle at the top left. Each arrow is the same length as the diameter of the circle from which it extends.'), tpPartOfImplicit);
   MaleArchway.Add(TScenery.Create('wood', '((stiff painted blue)# wood)&', 'The northern archway''s wood is painted blue with small white geometric shapes. The wood is stiff.'), tpPartOfImplicit);
   ArrivalsCircle.GetSurface().Add(MaleArchway, tpPartOfImplicit);

   FemaleArchway := TGenderArchway.Create('south archway', '(((((light pink)& light-pink)@ thick (semicircular semi-circular)@ crystal)# (archway/archways arch/arches)@ (to the (south s)@))& '+
                                                           '((((light pink)& light-pink)@ thick (semicircular semi-circular)@ crystal (southern south s)@)# (archway/archways arch/arches)@)&)@',
                                          'The south archway is a thick, semicircular arch made of a light-pink crystal. At the top of the arch is a gold inlay, which itself is decorated with a diamond-studded circle above a diamond-studded cross.', gFemale, FemalePath);
   FemaleArchway.Add(TScenery.Create('gold inlay', '(((diamond-studded (diamond studded)&)@ gold)# inlay/inlays)&', 'The gold is inlaid in the crystal archway. Diamonds are studded in the gold inlay, forming a circle and a cross.'), tpPartOfImplicit);
   FemaleArchway.Add(TScenery.Create('diamonds', '(((diamond-studded (diamond studded?))@ (circle-and-cross/circles-and-crosses (circle/circles and cross/crosses) circle/circles cross/crosses)@)& '+
                                                 '((circle-and-cross (circle and cross))@ diamond/diamonds)& '+
                                                 '((circle-and-cross/circles-and-crosses (circle/circles and cross/crosses) circle/circles cross/crosses)@ of (diamond diamonds)@)&)@',
                                     'The diamonds reflect the light perfectly. They are embedded in the gold inlay, arranged so as to form a perfect circle with a perfect square cross underneath. The top of the top arm of the cross touches the bottom of the circle.'), tpPartOfImplicit);
   ArrivalsCircle.GetSurface().Add(FemaleArchway, tpPartOfImplicit);

   ThirdGenderArchway := TGenderArchway.Create('east archway', '((three-foiled cusped ((archway/archways arch/arches)@ (of stone)? (to the (east e)@)?))& '+
                                                               '(((three-foiled cusped)& stone)# ((archway/archways arch/arches)@ (to the (east e)@)?))& '+
                                                               '(((eastern east e)@ (three-foiled cusped)& stone)# (archway/archways arch/arches)@)& '+
                                                               '((eastern east e)@ (three-foiled cusped)& ((archway/archways arch/arches)@ (of stone)?))&)@',
                                               'The east archway is a three-foiled cusped arch made of stone. Carved in the face of the arch are a series of human figures cooking, building huts, hunting, playing, digging, and so forth.', gThirdGender, nil);
   ThirdGenderArchway.Add(TScenery.Create('human figures', '((carved human figure/figures)& (human figure carving/carvings)&)@', 'The carvings appear to represent humans partaking in typical day-to-day activities. The recognisable acts depicted are the cooking of a meal, the building of wooden huts, hunting, children playing, the digging of a hole, and fornication.'), tpPartOfImplicit);
   ArrivalsCircle.GetSurface().Add(ThirdGenderArchway, tpPartOfImplicit);

   HiveArchway := TGenderArchway.Create('west archway', '((((stainless steel)& horseshoe)# ((archway/archways arch/arches)@ (to the (west w)@)?))& ' +
                                                        '(((stainless steel)& horseshoe (western west w)@)# (archway/archways arch/arches)@)&)@',
                                        'The west archway is a horseshoe arch made of a series of adjacent bars of stainless steel, buried under tension.', gHive, nil);
   HiveArchway.Add(TScenery.Create('bars', '(((series of)? (buried adjacent)# bars (of (stainless steel)&)?) '+
                                           '(buried stainless steel bar/bars)& '+
                                           '((stainless steel)& bar/bars (buried under tension))&)@',
                                   'The stainless steel bars are bent into a horseshoe shape and kept in place by virtue of being buried in the ground.'), tpPartOfImplicit);
   ArrivalsCircle.GetSurface().Add(HiveArchway, tpPartOfImplicit);

   RobotArchway := TGenderArchway.Create('southwest archway', '(((shining (silicon metalloid)& (inverted catenary)&)# ((archway/archways arch/arches)@ (to the (southwest (south west) sw)@)?))& '+
                                                              '(((southwestern (south western) southwest (south west) sw)@ shining (silicon metalloid)& (inverted catenary)&)# (archway/archways arch/arches)@)&)@',
                                         'The southwest archway is an inverted catenary arch precisely carved into a block of shining silicon metalloid. Along the rim of the arch are many dots. '+'The archway itself is in good condition, but the area surrounding it appears to be unmaintained and rarely travelled.', gThirdGender, nil);
   Thing := TScenery.Create('rim', '(shining silicon metalloid)* rim/rims', 'The rim of the carved silicon metalloid is very smooth and shiny. It follows an inverted catenary shape. The rim is decorated with dots.');
   Thing.Add(TScenery.Create('dots', '(((regularly? spaced) (large small (large and small))@)# rim dot/dots)&', 'The dots are spaced regularly along the rim of the arch. Some of the dots are large and some small, but there is no obvious pattern: '#226#131#175#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175#226#131#175#226#131#175#226#131#175#226#128#162#226#128#162#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175#226#131#175#226#131#175#226#131#175#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175), tpPartOfImplicit);
   RobotArchway.Add(Thing, tpAmbiguousPartOfImplicit);
   ArrivalsCircle.GetSurface().Add(RobotArchway, tpPartOfImplicit);

   Thing := TScenery.Create('weeds', 'weed/weeds', 'The weeds have overrun the archways to the west and southwest.');
   (Thing as TScenery).CannotMoveExcuse := 'The weeds have evolved very strong roots. It would take some sort of inhuman hive-mind/robot alliance to clean them up.';
   ArrivalsCircle.GetSurface().Add(Thing, tpAroundImplicit);

   ArrivalsPedestal := TStaticThing.Create('stone pedestal', '((big glowing stone ((twelve-pointed twelve-point (twelve (pointed point)@))@ star)&)# arrivals (pedestal/pedestals slab/slabs)@)&',
                                           'The arrivals pedestal is a big stone slab, in the shape of a twelve-pointed star, over which you materialised when you arrived in Cuddly World. Glowing sigils are engraved at each point of the star, and a circle is engraved around the center.', tmLudicrous, tsGigantic);
   ArrivalsPedestal.Add(TScenery.Create('glowing engraved sigils', '((glowing engraved (deep? magic))# (sigil/sigils engraving/engravings)@)&', 'The sigils are of a deep magic. '+'Each point has a symbol engraved in it. There are eight unique symbols; two are repeated twice, and one presumably important symbol is repeated three times. '+'Starting from the northern-most point of the star and going around the circle in a clockwise direction, you see the following symbols: '+magicA+magicL+magicM+magicA+magicG+magicI+magicC+magicA+magicR+magicR+magicI+magicV), tpPartOfImplicit);
   ArrivalsPedestal.Add(TScenery.Create('engraved circle', '((center engraved)# (circle/circles engraving/engravings)@)&', 'The circle is engraved around the center of the pedestal.'), tpPartOfImplicit);
   ArrivalsCircle.GetSurface().Add(ArrivalsPedestal, tpAt);

   // signs

   ArrivalsCircle.ConnectCardinals(MaleArchway, ThirdGenderArchway, FemaleArchway, HiveArchway);
   ArrivalsCircle.ConnectDiagonals(Forest, Forest, RobotArchway, Forest);
   World.AddLocation(ArrivalsCircle);
   World.StartingLocation := ArrivalsPedestal;
   World.StartingPosition := tpOn;

   // temporary
   World.AddLocation(MalePath);
   MalePath.ConnectCardinals(Forest, Forest, ArrivalsCircle, Forest);
   MalePath.Add(TSpade.Create(), tpOn);
   World.AddLocation(FemalePath);
   FemalePath.ConnectCardinals(ArrivalsCircle, Forest, Forest, Forest);

   Result := World;
end;

initialization
   RegisterStorableClass(TCuddlyWorld,   100000);
   RegisterStorableClass(TGenderArchway, 100001);
end.
