{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit cuddlycamp;

interface

uses
   physics, world;

const
   kWorldFileName = 'map.dat';
   kSaveDataVersion = 1;

function InitEden: TWorld; { create the initial locations }

implementation

uses
   storable, grammarian, locations, things, player, sysutils, threshold;

type
   TCuddlyWorld = class(TWorld) // @RegisterStorableClass
    protected
      FStartingLocation: TAtom;
      FStartingPosition: TThingPosition;
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure AddPlayer(Player: TPlayer); override;
      property StartingLocation: TAtom read FStartingLocation write FStartingLocation;
      property StartingPosition: TThingPosition read FStartingPosition write FStartingPosition;
   end;

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

procedure TCuddlyWorld.AddPlayer(Player: TPlayer);
begin
   Assert(Assigned(FStartingLocation));
   Assert(FStartingPosition >= Low(TThingPosition));
   Assert(FStartingPosition <= High(TThingPosition));
   FStartingLocation.Add(Player, FStartingPosition);
   inherited;
end;



function InitEden: TWorld;
var
   World: TCuddlyWorld;
   SkyBox, Room1, Room2, ArchwayLocation: TLocation;
   Sky, Sun: TThing;
   Archway: TThresholdThing;
begin
   World := TCuddlyWorld.Create();

   Sky := TScenery.Create('sky', 'blue? sky/skies', 'The sky is clear.');
   Sun := TScenery.Create('sun', '(sun/suns star/stars)@', 'The sun is bright.');
   (Sky as TScenery).Opened := True;
   Sky.Add(Sun, tpEmbedded);
   SkyBox := TBackdrop.Create(Sky, tpAtImplicit);
   World.AddLocation(SkyBox);

   Archway := TThresholdThing.Create('archway', 'unnotable? (arch/arches archway/archways)@', 'The archway has no notable features.', cdSouth);

   Room1 := TGroundLocation.Create('Room', 'a room', 'the room', 'Nothing is particularly noteworthy about this location.', CreateEarthSurface());
   Room1.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);

   Room2 := TGroundLocation.Create('Back Room', 'a back room', 'the back room', 'Nothing is particularly noteworthy about this second location.', CreateEarthSurface());
   Room2.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);

   ArchwayLocation := ConnectThreshold(Room1, Room2, Archway, CreateEarthSurface());
   ArchwayLocation.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);

   World.AddLocation(Room1);
   World.AddLocation(Room2);
   World.AddLocation(ArchwayLocation);
   World.StartingLocation := Room1.GetSurface();
   World.StartingPosition := tpOn;

   Result := World;
end;

initialization
{$INCLUDE registrations/cuddlycamp.inc}
end.
