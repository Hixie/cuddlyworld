{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit cuddlycamp;

interface

uses
   physics, world, thingdim;

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
   SkyBox, Room1, Room2, DoorwayLocation, Tunnel, TunnelEnd, Bedroom, Cave: TLocation;
   Thing, Sky, Sun, Bed, Pillow, Stars, Ceiling: TThing;
   Doorway: TThresholdThing;
begin
   World := TCuddlyWorld.Create();

   Sky := TScenery.Create('sky', 'blue? sky/skies', 'The sky is clear.');
   Sun := TScenery.Create('sun', '(sun/suns star/stars)@', 'The sun is bright.');
   (Sky as TScenery).Opened := True;
   Sky.Add(Sun, tpEmbedded);
   SkyBox := TBackdrop.Create(Sky, tpAtImplicit);
   World.AddLocation(SkyBox);

   Room1 := TGroundLocation.Create('Room', 'a room', 'the room', 'Nothing is particularly noteworthy about this location.', CreateEarthSurface());
   Room1.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);

   Room2 := TGroundLocation.Create('Back Room', 'a back room', 'the back room', 'Nothing is particularly noteworthy about this second location.', CreateEarthSurface());
   Room2.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);
   
   Doorway := TDoorWay.Create('doorway', 'unnotable? doorway/doorways', 'The doorway has no notable features.', cdSouth,
                              TDoor.Create('door', 'wooden? (open:1 closed:2)* door/doors', 
                                          TDoorSide.Create('side', 'varnished? (front:1)? side/sides', 'the door is varnished.'),
                                          TDoorSide.Create('side', '((scratched? (back:1)? side/sides) scratch/scratches)@', 'the door has a big scratch, as if made by a wolf.')));

   DoorwayLocation := ConnectThreshold(Room1, Room2, Doorway);
   DoorwayLocation.AddLandmark(cdUp, Sky, [loVisibleFromFarAway]);

   Tunnel := TGroundLocation.Create('Tunnel Trail', 'a tunnel trail', 'the tunnel trail', 'The tunnel has many turns.', CreateEarthSurface());
   TunnelEnd := TGroundLocation.Create('Tunnel End', 'a tunnel end', 'the tunnel end', 'The tunnel end room has white walls.', CreateEarthSurface());

   Bedroom := TGroundLocation.Create('Bedroom', 'a bedroom', 'the bedroom', 'The bedroom is a large room. On the ceiling are some stars.', CreateStoneSurface());
   Bed := TDescribedPhysicalThing.Create('bed', 'bed/beds', 'The bed is medium-sized bed.', tmPonderous, tsBig);
   Pillow := TDescribedPhysicalThing.Create('pillow', '((car? pillow/pillows) car/cars)@', 'The pillow has drawings of cars on it.', tmLight, tsSmall);
   Stars := TFeature.Create('stars', '(ceiling/ceilings star/stars)#', 'The ceiling has stars on it.');

   Bedroom.GetSurface().Add(Bed, tpOn);
   Bed.GetSurface().Add(Pillow, tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('eat block', '((blue eat word block/blocks)& word/words)@', 'This block is blue and says "eat".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('it block', '((yellow it word block/blocks)& word/words)@', 'This block is yellow and says "it".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('is block', '((orange is word block/blocks)& word/words)@', 'This block is orange and says "is".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('hat block', '((red hat word block/blocks)& word/words)@', 'This block is red and says "hat".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('make block', '((green make word block/blocks)& word/words)@', 'This block is green and says "make".', tmLight, tsSmall), tpOn);
   Bedroom.Add(Stars, tpPartOfImplicit);

   Cave := TGroundLocation.Create('Cave', 'a cave', 'the cave', 'The cave is round and dark.', CreateEarthSurface());
   Ceiling := TScenery.Create('ceiling', '(round dark)* (ceiling/ceilings roof/rooves roof/roofs)@', 'The ceiling is dark and round, just like the rest of the cave.');
   Cave.Add(Ceiling, tpPartOfImplicit);
   Cave.AddLandmark(cdUp, Ceiling, [loPermissibleNavigationTarget, loConsiderDirectionUnimportantWhenFindingChildren]);
   Thing := TBag.Create('brown sack', '(elongated brown (sack/sacks bag/bags)@)&', 'The sack is brown.', tsBig);
   Cave.GetSurface().Add(Thing, tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('clove of garlic', '((clove/cloves of garlic) (garlic clove/cloves)&)@', 'There''s nothing special about the clove of garlic.', tmLight, tsSmall), tpIn);
   Thing.Add(TDescribedPhysicalThing.Create('lunch', 'lunch/lunches', 'There''s nothing special about the lunch.', tmLight, tsSmall), tpIn);
   Thing.Add(TDescribedPhysicalThing.Create('wooden spoon', '((wooden wood)@ (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of wood.', tmLight, tsSmall), tpIn);

   Thing := TDescribedPhysicalThing.Create('kitchen table', '(non-descript kitchen table/tables)&', 'The kitchen table is non-descript.', tmPonderous, tsMassive);
   Cave.GetSurface().Add(Thing, tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('plastic knife', '(plastic (knife/knives utensil/utensils (silverware set/sets)&)@)&', 'The knife is made of plastic.', tmLight, tsSmall), tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('plastic fork', '(plastic (fork/forks utensil/utensils (silverware set/sets)&)@)&', 'The fork is made of plastic.', tmLight, tsSmall), tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('plastic spoon', '(plastic (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of plastic.', tmLight, tsSmall), tpOn);

   Thing := TDescribedPhysicalThing.Create('desk', '(non-descript (desk/desks table/tables)@)&', 'The desk is non-descript.', tmPonderous, tsMassive);
   Cave.GetSurface().Add(Thing, tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('stainless steel knife', '(stainless steel (knife/knives utensil/utensils (silverware set/sets)&)@)&', 'The knife is made of stainless steel.', tmLight, tsSmall), tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('stainless steel fork', '(stainless steel (fork/forks utensil/utensils (silverware set/sets)&)@)&', 'The fork is made of stainless steel.', tmLight, tsSmall), tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('stainless steel spoon', '(stainless steel (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of stainless steel.', tmLight, tsSmall), tpOn);

   Thing := TDescribedPhysicalThing.Create('dining room table', '(non-descript dining room table/tables)&', 'The dining room table is non-descript.', tmPonderous, tsMassive);
   Cave.GetSurface().Add(Thing, tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('silver knife', '(silver (knife/knives utensil/utensils (silverware set/sets)&)@)&', 'The knife is made of silver.', tmLight, tsSmall), tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('silver fork', '(silver (fork/forks utensil/utensils (silverware set/sets)&)@)&', 'The fork is made of silver.', tmLight, tsSmall), tpOn);
   Thing.Add(TDescribedPhysicalThing.Create('silver spoon', '(silver (spoon/spoons utensil/utensils (silverware set/sets)&)@)&', 'The spoon is made of silver.', tmLight, tsSmall), tpOn);
   Cave.GetSurface().Add(TSpade.Create(), tpOn);

   Bedroom.GetSurface().Add(TOpening.Create('stairs', 'stair/stairs', 'The stairs lead down.', Cave, tsGigantic), tpSurfaceOpening);
   Ceiling.Add(TOpening.Create('stairs', 'stair/stairs', 'The stairs lead up.', Bedroom, tsGigantic), tpSurfaceOpening);

   ConnectLocations(Room1, cdSouth, Tunnel, [loPermissibleNavigationTarget, loAutoDescribe]);
   ConnectLocations(Tunnel, cdSouth, TunnelEnd, [loPermissibleNavigationTarget, loAutoDescribe]);
   ConnectLocations(Tunnel, cdWest, Cave, [loPermissibleNavigationTarget, loAutoDescribe]);

   World.AddLocation(Room1);
   World.AddLocation(Room2);
   World.AddLocation(DoorwayLocation);
   World.AddLocation(Tunnel);
   World.AddLocation(TunnelEnd);
   World.AddLocation(Bedroom);
   World.AddLocation(Cave);
   World.StartingLocation := Room1.GetSurface();
   Assert(Assigned(World.StartingLocation));
   World.StartingPosition := tpOn;

   Result := World;
end;

initialization
{$INCLUDE registrations/cuddlycamp.inc}
end.
