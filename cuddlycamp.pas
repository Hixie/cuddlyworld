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
   storable, grammarian, locations, things, player, sysutils, threshold, stairs;

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
   Tunnel, TunnelEnd, Bedroom, Cave: TLocation;
   Thing, Bed, Pillow, Stars, Ceiling: TThing;
   DoorFrame: TDoorWay;
begin
   World := TCuddlyWorld.Create();

   Tunnel := TGroundLocation.Create('Tunnel Trail', 'the tunnel trail', 'a tunnel trail', 'The tunnel has many turns.', CreateEarthSurface());
   Tunnel.Add(TFeature.Create('many tunnel turns', 'many? tunnel? (turn/turns twist/twists)@', 'The tunnel twists in many directions, leading to a cave when going generally westward, and leading to the end of the tunnel in a northern direction.'), tpPartOfImplicit);
   
   TunnelEnd := TGroundLocation.Create('Tunnel End', 'the tunnel end', 'a tunnel end', 'The tunnel end room has white walls.', CreateEarthSurface());
   Tunnel.Add(TStructure.Create('north wall', '(white (north northern)@)* wall/walls', 'The northern wall of the tunnel end room is white.', 'There does not seem to be any way to attach things to the wall.'), tpPartOfImplicit);
   Tunnel.Add(TStructure.Create('east wall', '(white (east eastern)@)* wall/walls', 'The eastern wall of the tunnel end room is white.', 'There does not seem to be any way to attach things to the wall.'), tpPartOfImplicit);
   Tunnel.Add(TStructure.Create('west wall', '(white (west western)@)* wall/walls', 'The western wall of the tunnel end room is white.', 'There does not seem to be any way to attach things to the wall.'), tpPartOfImplicit);

   Bedroom := TGroundLocation.Create('Bedroom', 'the bedroom', 'a bedroom', 'The bedroom is a large room. On the ceiling are some stars.', CreateStoneSurface());
   Bed := TDescribedPhysicalThing.Create('bed', 'bed/beds', 'The bed is medium-sized bed.', tmPonderous, tsBig);
   Pillow := TDescribedPhysicalThing.Create('pillow', '((car? pillow/pillows) car/cars)@', 'The pillow has drawings of cars on it.', tmLight, tsSmall);
   Stars := TFeature.Create('stars', 'pretty? ceiling? star/stars', 'The ceiling has stars on it.');
   Ceiling := TStructure.Create('ceiling', 'pretty? starry? ceiling/ceilings', 'The ceiling has some pretty stars on it.', 'Putting things on a ceiling seems like an exercise in futility.');
   Bedroom.Add(Ceiling, tpPartOfImplicit);
   Ceiling.Add(Stars, tpPartOfImplicit);

   Bedroom.GetSurface().Add(Bed, tpOn);
   Bed.GetSurface().Add(Pillow, tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('eat block', '((blue eat word block/blocks)& word/words)@', 'This block is blue and says "eat".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('it block', '((yellow it word block/blocks)& word/words)@', 'This block is yellow and says "it".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('is block', '((orange is word block/blocks)& word/words)@', 'This block is orange and says "is".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('hat block', '((red hat word block/blocks)& word/words)@', 'This block is red and says "hat".', tmLight, tsSmall), tpOn);
   Bedroom.GetSurface().Add(TDescribedPhysicalThing.Create('make block', '((green make word block/blocks)& word/words)@', 'This block is green and says "make".', tmLight, tsSmall), tpOn);

   Cave := TGroundLocation.Create('Cave', 'the cave', 'a cave', 'The cave is round and dark.', CreateEarthSurface());
   Ceiling := TStructure.Create('ceiling', '(round dark)* (ceiling/ceilings roof/rooves roof/roofs)@', 'The ceiling is dark and round, just like the rest of the cave.', 'Putting things on a ceiling seems like an exercise in futility.');
   Cave.Add(Ceiling, tpPartOfImplicit);
   Cave.AddLandmark(cdUp, Ceiling, [loPermissibleNavigationTarget]);
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

   DoorFrame := TDoorWay.Create('door frame', '(door frame/frames)', 'The door frame is a frame around where a door would go.', cdNorth,
                                TDoor.Create('door', 'flat? door/doors',
                                             TDoorSide.Create('side', '(flat front)* door? side/sides', 'the front side of the door is flat.'),
                                             TDoorSide.Create('side', '(flat back)* door? side/sides', 'the back side of the door is flat.')));
   DoorFrame.Door.Description := 'The door is flat.';
   World.AddLocation(ConnectThreshold(Tunnel, TunnelEnd, DoorFrame));

   World.AddLocation(ConnectStairs(Cave, Bedroom));

   ConnectLocations(Tunnel, cdWest, Cave, [loPermissibleNavigationTarget, loAutoDescribe]);

   World.AddLocation(Tunnel);
   World.AddLocation(TunnelEnd);
   World.AddLocation(Bedroom);
   World.AddLocation(Cave);
   World.StartingLocation := Cave.GetSurface();
   Assert(Assigned(World.StartingLocation));
   World.StartingPosition := tpOn;

   Result := World;
end;

initialization
{$INCLUDE registrations/cuddlycamp.inc}
end.
