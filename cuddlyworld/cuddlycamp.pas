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
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AGender: TGender; ADestination: TLocation; AFlags: TStaticThingFlags = []);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); override;
   end;


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


constructor TGenderArchway.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AGender: TGender; ADestination: TLocation; AFlags: TStaticThingFlags = []);
begin
   inherited Create(ASynonyms, ALongName, ADescription, ADestination, tmLudicrous, tsMassive, AFlags);
   FGender := AGender;
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
   ArrivalsCircle := TFeaturelessOutdoorLocation.Create('Arrivals Circle', 'the arrivals circle', 'an arrivals circle', 'The arrivals circle is where all the visitors to Cuddly World first appear. Well-worn paths lead to the north and south under decorated archways; a well-paved, but less worn, path leads to the west under a similar archway. Large signs staked into the '+'ground point elaborately to these paths. To the west, a smaller sign is staked into the middle of some weeds next to an apparently abandoned archway. To the southwest are another sign and archway, in an even more dilapidated state. In the distance to the '+'west is a mountain. In other directions, you see an impenetrable forest.');
   MalePath := TFeaturelessOutdoorLocation.Create('Male Path', 'the male path', 'a male path', 'The male path meanders from the south to the east.');
   FemalePath := TFeaturelessOutdoorLocation.Create('Female Path', 'the female path', 'a female path', 'The female path meanders from the north to the east.');

   { ArrivalsCircle }
   ArrivalsCircle.Add(TDistantScenery.Create('mountain', cdEast), tpAroundImplicit);
   Forest := TScenery.Create(['forest', 'trees', 'tree', 'of'], 'the forest of trees', 'The forest is dense and impassable.');
   ArrivalsCircle.Add(Forest, tpAroundImplicit);

   MaleArchway := TGenderArchway.Create(['north archway', 'arch', 'blue', 'navy', 'wooden', 'to', 'the', 'n'], 'the blue wooden archway to the north', 'The north archway is a Lancet arch made of painted wood. It is a predominantly dark blue affair, with small white geometric shapes (primarily circles and arrows, all pointing diagonally upwards and to the right) painted on its tall columns.', gMale, MalePath);
   MaleArchway.Add(TScenery.Create('small white geometric shapes', 'The geometric shapes are small white circles painted against the blue wooden arch. Each circle has an arrow pointing out of the circle at the top left. Each arrow is the same length as the diameter of the circle from which it extends.', [stfPlural]), tpPartOfImplicit);
   MaleArchway.Add(TScenery.Create('wood', 'The northern archway's wood is painted blue with small white geometric shapes. The wood is stiff.'), tpPartOfImplicit);
   ArrivalsCircle.Add(MaleArchway, tpPartOfImplicit);

   FemaleArchway := TGenderArchway.Create(['south archway', 'arch', 'light', 'light-pink', 'pink', 'crystal', 'to', 'the', 's'], 'the pink crystal archway to the south', 'The south archway is a thick, semicircular arch made of a light-pink crystal. At the top of the arch is a gold inlay, which itself is decorated with a diamond-studded circle above a diamond-studded cross.', gFemale, FemalePath);
   FemaleArchway.Add(TScenery.Create('gold inlay', 'The gold is inlaid in the crystal archway. Diamonds are studded in the gold inlay, forming a circle and a cross.'), tpPartOfImplicit);
   FemaleArchway.Add(TScenery.Create(['diamonds', 'diamond', 'diamond-studded', 'studded', 'circle', 'cross', 'and'], 'the circle-and-cross diamonds', 'The diamonds reflect the light perfectly. They are embedded in the gold inlay, arranged so as to form a perfect circle with a perfect square cross underneath. The top of the top arm of the cross touches the bottom of the circle.', [stfPlural]), tpPartOfImplicit);
   ArrivalsCircle.Add(FemaleArchway, tpPartOfImplicit);

   ThirdGenderArchway := TGenderArchway.Create(['east archway', 'arch', 'stone', 'to', 'the', 'e'], 'the stone archway to the east', 'The east archway is a three-foiled cusped arch made of stone. Carved in the face of the arch are a series of human figures cooking, building huts, hunting, playing, digging, and so forth.', gThirdGender, nil);
   ThirdGenderArchway.Add(TScenery.Create(['human figures', 'figure', 'carvings', 'carving', 'carved'], 'the carved human figures', 'The carvings appear to represent humans partaking in typical day-to-day activities. The recognisable acts depicted are the cooking of a meal, the building of wooden huts, hunting, children playing, the digging of a hole, and fornication.', [stfPlural]), tpPartOfImplicit);
   ArrivalsCircle.Add(ThirdGenderArchway, tpPartOfImplicit);

   HiveArchway := TGenderArchway.Create(['west archway', 'arch', 'stainless', 'steel', 'horseshoe', 'bar', 'bars', 'to', 'the', 'w'], 'the stainless steel archway to the west', 'The west archway is a horseshoe arch made of a series of adjacent bars of stainless steel, buried under tension.', gHive, nil);
   ArrivalsCircle.Add(HiveArchway, tpPartOfImplicit);

   RobotArchway := TGenderArchway.Create(['southwest archway', 'arch', 'silicon', 'metalloid', 'to', 'the', 'sw', 'dilapidated'], 'the silicon archway to the southwest', 'The southwest archway is an inverted catenary arch precisely carved into a block of shining silicon metaloid. Along the rim of the arch are many dots.', gThirdGender, nil);
   RobotArchway.Add(TScenery.Create(['dots', 'dot'], 'the dots', 'The dots are spaced regularly along the rim of the arch. Some of the dots are large and some small, but there is no obvious pattern: '#226#131#175#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175#226#131#175#226#131#175#226#131#175#226#128#162#226#128#162#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175#226#131#175#226#131#175#226#131#175#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#128#162#226#131#175#226#128#162#226#128#162#226#131#175#226#131#175, [stfPlural]), tpPartOfImplicit);
   ArrivalsCircle.Add(RobotArchway, tpPartOfImplicit);

   Thing := TScenery.Create(['weeds', 'weed'], 'the weeds', 'The weeds have overrun the archways to the west and southwest.', [stfPlural]);
   (Thing as TScenery).CannotMoveExcuse := 'The weeds have evolved very strong roots. It would take some sort of inhuman hive-mind/robot alliance to clean them up.';
   ArrivalsCircle.Add(Thing, tpAroundImplicit);

   ArrivalsPedestal := TStaticThing.Create(['stone pedestal', 'twelve-pointed', 'star'], 'the twelve-pointed star stone pedestal', 'The arrivals pedestal is a big stone slab, in the shape of a twelve-pointed star, over which you materialised when you arrived in Cuddly World. Glowing sigils are engraved at each point of the star, and a circle is engraved around the center.', tmLudicrous, tsMassive);
   ArrivalsPedestal.Add(TScenery.Create(['glowing engraved sigils', 'engravings'], 'the glowing engraved sigils', 'The sigils are of a deep magic.', [stfPlural]), tpPartOfImplicit);
   ArrivalsPedestal.Add(TScenery.Create(['engraved circle', 'engravings'], 'the engraved circle', 'The circle is engraved around the center of the pedestal.'), tpPartOfImplicit);
   ArrivalsCircle.Add(ArrivalsPedestal, tpAt);

   // signs

   ArrivalsCircle.ConnectCardinals(MaleArchway, ThirdGenderArchway, FemaleArchway, HiveArchway);
   ArrivalsCircle.ConnectDiagonals(Forest, Forest, RobotArchway, Forest);
   World.AddLocation(ArrivalsCircle);
   World.StartingLocation := ArrivalsPedestal;
   World.StartingPosition := tpOn;

   // temporary
   World.AddLocation(MalePath);
   World.AddLocation(FemalePath);

   Result := World;
end;

initialization
   RegisterStorableClass(TCuddlyWorld, 10);
   RegisterStorableClass(TGenderArchway, 11);
end.



xxxxxxxxxxxxxxx


   "Ode to Death death wish" - "Ode", "Ode to Death", "death", "death wish", "death-wish", "wish"; NOT "Ode to Death wish", "to", "death death".

   Adjective ['Ode', 'Ode to Death', 'Death']
   Noun ['death wish', 'death-wish']
   Named ['wish']


   "Grotesque Blue Sword of Blood" - "Grotesque Sword", "Sword of Blood", "Grotesque Sword of Blood", "Grotesque", "Sword", "Blood"; NOT "Grotesque Blood", "of"

   Adjective ['Grotesque']
   Adjective ['Blue']
   Noun ['Sword', 'Sword of Blood']
   Named ['Blood']


   "Happy Fun Ball"          - "Ball", "Happy Ball", "Happy Fun Ball", "Fun Ball", "Happy", "Fun"

   Adjective ['Happy']
   Adjective ['Fun']
   Noun ['Ball']


   "the blue wooden archway to the north" - "blue", "navy", "wooden", "archway", "arch", "n", "north", "northern",
                                            "blue wooden archway", "navy wooden archway", "northern blue wooden archway",
                                            "navy wooden", "northern wooden", "n arch", "navy blue arch", "navy-blue arch";
                                            NOT "blue navy arch", "wooden navy", "arch blue", "archway wooden", "arch wooden", "arch n", "archway northern"

   Adjective ['Blue', 'Navy', 'navy blue', 'navy-blue']
   Adjective ['Wooden']
   Adjective ['n', 'north', 'northern']
   Noun ['Archway', 'Arch']
   


   explicit synonyms (must match entire phrase)
   adjectives (must be given once max each, must be given first, any number can be given, some have alternatives that are mutually exclusive)
   nouns (only one can be given, must be at end)
   

xxxxxxxxxxxxxxxxxxxxx
