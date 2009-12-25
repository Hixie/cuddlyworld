{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit cuddlycamp;

interface

uses
   storable, world, grammarian, locations, thingdim;

const
   kWorldFileName = 'world.dat';
   kSaveDataVersion = 1;

function InitEden: TWorld; { create the initial locations }

implementation

uses
   things;

type
   TCuddlyWorld = class(TWorld)
    protected
      FStartingLocation: TLocation;
      function GetStartingLocation: TLocation; override;
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      property StartingLocation: TLocation read FStartingLocation write FStartingLocation;
   end;

function TCuddlyWorld.GetStartingLocation: TLocation;
begin
   Assert(Assigned(FStartingLocation));
   Result := FStartingLocation;
end;

constructor TCuddlyWorld.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@FStartingLocation);
end;

procedure TCuddlyWorld.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FStartingLocation);
end;


function InitEden: TWorld;
var
   World: TCuddlyWorld;
   Camp, Cliff: TLocation;
   CampMountain, CampForest: TThing;
   CliffMountain, CliffForest, CliffCamp, CliffCliff: TThing;
begin
   World := TCuddlyWorld.Create();

   { Locations }
   Camp := TFeaturelessOutdoorLocation.Create('Camp Cuddlyfort', 'Camp Cuddlyfort', 'a camp', 'This is a camp nestled in a forest, under the shadow of a mountain to the north.');
   Cliff := TFeaturelessOutdoorLocation.Create('Foot of Cliff Face', 'the foot of the cliff face', 'a foot of a cliff face', 'The south side of a mountain rises out of the ground here, in a clear and well-defined way, as if to say "this far, no farther" to an enemy whose nature you cannot fathom. ' + 'The cliff is a sheer rock face, essentially unclimbable. Conspicuous is the absence of any vegetation anywhere on the cliff, at least as far as you can see. At the base of the cliff to the east and west is a dense forest.');

   { Camp }
   CampMountain := TDistantScenery.Create('mountain', cdNorth);
   Camp.Add(CampMountain, tpAroundImplicit);
   CampForest := TScenery.Create(['forest', 'trees', 'tree'], 'The forest is dense and impassable.');
   Camp.Add(CampForest, tpAroundImplicit);
   Camp.GetSurface().Add(TStaticThing.Create('MacGuffin', 'The MacGuffin displays outward signs of being avian in nature.', tmHeavy, tsBig), tpOn);
   Camp.GetSurface().Add(TStaticThing.Create('penny', 'The penny is a copper coin of little value.', tmLight, tsSmall), tpOn);
   Camp.GetSurface().Add(TPile.Create(['leaves', 'leaf'], 'It appears someone has collected fallen leaves from the forest. Possibly the entire forest, given how big the pile is.', tmLight, tsGigantic), tpOn);
   Camp.ConnectCardinals(Cliff, CampForest, CampForest, CampForest);
   Camp.ConnectDiagonals(CampForest, CampForest, CampForest, CampForest);
   World.AddLocation(Camp);
   World.StartingLocation := Camp;

   { Cliff }
   CliffMountain := TScenery.Create('mountain', 'From here you cannot get a good sense of the size of the mountain. Its cliff face dominates your view.');
   (CliffMountain as TScenery).FindDescription := 'The mountain towers high above you.';
   Cliff.Add(CliffMountain, tpAtImplicit);
   CliffCliff := TScenery.Create('cliff', 'The cliff consists of a sheer rock face.');
   CliffMountain.Add(CliffCliff, tpPartOfImplicit);
   CliffForest := TScenery.Create(['forest', 'trees', 'tree'], 'The forest is dense and impassable.');
   Cliff.Add(CliffForest, tpAroundImplicit);
   CliffCamp := TDistantScenery.Create('camp', cdSouth);
   Cliff.Add(CliffCamp, tpAroundImplicit);
   Cliff.GetSurface().Add(TSpade.Create(), tpOn);
   Cliff.GetSurface().Add(TStaticThing.Create(['gigantic balloon', 'huge', 'massive'], 'The balloon is as high as a house! It is coloured red.', tmLight, tsGigantic), tpOn);
   Cliff.ConnectCardinals(CliffCliff, CliffForest, Camp, CliffForest);
   Cliff.ConnectDiagonals(nil, CliffForest, CliffForest, nil);
   World.AddLocation(Cliff);

   Result := World;
end;

initialization
   RegisterStorableClass(TCuddlyWorld, 10);
end.
