{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit threshold;

interface

uses
   locations, things, grammarian, matcher, storable, physics, messages;

type
   TThresholdThing = class abstract(TScenery)
    protected
      FFrontSideFacesDirection: TCardinalDirection;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      property FrontSideFacesDirection: TCardinalDirection read FFrontSideFacesDirection write FFrontSideFacesDirection;
   end;

   generic TDoubleSidedThing <AncestorClass: TThing> = class abstract (AncestorClass)
    protected
      // make sure you implement Read and Write for these in your subclass...
      // http://bugs.freepascal.org/view.php?id=16588
      FFrontSideDescription: UTF8String;
      FBackSideDescription: UTF8String;
    public
      property FrontSideDescription: UTF8String read FFrontSideDescription write FFrontSideDescription;
      property BackSideDescription: UTF8String read FBackSideDescription write FBackSideDescription;
   end;

   TStaticThresholdThing = class(specialize TDoubleSidedThing<TThresholdThing>) // @RegisterStorableClass
    // This is for something that's always traversable, like an archway or something
    // note that we inherit from a class that defines Openable, so IsOpen() might be true or false
    // but that doesn't mean that when IsOpen() is false, we're not traversable
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
   end;

(*
   TDoorWay = class(TThresholdThing) // @ RegisterStorableClass
    // openable if there's a door
    // IsOpen() returns FOpened which we inherit
    protected
      function GetDoor(): TDoor; // or nil if there isn't one
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; Door: TDoor);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
        // XXX defer to the door if possible, else inherited
      property Door: TDoor read GetDoor; // can be nil, if there's no door
        // XXX doorways override everything to do with IsOpen accordingly
   end;

   TDoor = class(TStaticThing) // @ RegisterStorableClass
    // XXX A TDoor is openable if it has a doorway
    // XXX when you tell the TDoor to open, it actually checks if it is in a doorway
    // and if it is, it tells the doorway that it is now open; ditto when closing, in reverse
    // XXX sides:
    // there should be two TFeature objects, one for each side, that are permanently part of this object
    // XXX have the constructor set them
    // XXX have them be saved and restored appropriately
    // XXX defer to the relevant one of those for DescriptionSelf if Locate() returns a direction and we have
    // a doorway, otherwise defer to both, front then back. relevant one depends on doorway front facing
    // XXX only the relevant side should be referencable, when in a doorway
    protected
      FFrontSide, FBackSide: TFeature;
      function GetDoorWay(): TDoorWay; // or nil if the door isn't in a doorway
      // GetLock() could work a similar way
      function GetMatcherFlags(): TMatcherFlags; override;
    public
      const mfOpen: TMatcherFlag = 1;
      const mfClosed: TMatcherFlag = 2;
      constructor Create(Name: UTF8String; Pattern: UTF8String; FrontSide, BackSide: TFeature);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      property DoorWay: TDoorWay read GetDoorWay; // can be nil, if the door is not in a doorway
   end;

// XXX
// open door
// close door
// take door
// put door in doorway
// hang door in doorway
*)
   TThresholdLocation = class(TSurfaceSlavedLocation) // @RegisterStorableClass
    public
      constructor Create(Landmark: TThing; Surface: TThing);
      function GetTitle(Perspective: TAvatar): UTF8String; override;
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; override;
      procedure AddExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter); override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
   end;

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing; Flags: TLocation.TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;
// if you omit loAutoDescribe from the last argument, then the threshold won't be mentioned in descriptions of rooms that contain it

implementation

uses
   thingdim, lists, exceptions;

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing; Flags: TLocation.TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;
begin
   Flags := Flags + [loPermissibleNavigationTarget, loThreshold];
   Result := TThresholdLocation.Create(Threshold, Surface);
   FrontLocation.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], Result, Flags);
   Result.AddLandmark(Threshold.FrontSideFacesDirection, FrontLocation, [loAutoDescribe, loPermissibleNavigationTarget]);
   BackLocation.AddLandmark(Threshold.FrontSideFacesDirection, Result, Flags);
   Result.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], BackLocation, [loAutoDescribe, loPermissibleNavigationTarget]);
end;


constructor TStaticThresholdThing.Read(Stream: TReadStream);
begin
   inherited;
   FFrontSideDescription := Stream.ReadString();
   FBackSideDescription := Stream.ReadString();
end;

procedure TStaticThresholdThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FFrontSideDescription);
   Stream.WriteString(FBackSideDescription);
end;


constructor TThresholdThing.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection);
begin
   inherited Create(Name, Pattern, Description, tmPonderous, tsMassive);
   FFrontSideFacesDirection := FrontFacesDirection;
end;

constructor TThresholdThing.Read(Stream: TReadStream);
begin
   inherited;
   FFrontSideFacesDirection := TCardinalDirection(Stream.ReadCardinal());
end;

procedure TThresholdThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FFrontSideFacesDirection));
end;


function TStaticThresholdThing.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
var
   SubjectiveInformation: TSubjectiveInformation;
   Direction: TCardinalDirection;
begin
   SubjectiveInformation := Perspective.Locate(Self);
   if (PopCnt(Cardinal(SubjectiveInformation.Directions)) <> 1) then
   begin
      // e.g. if you're right there at the archway or whatever
      Result := inherited;
      exit;
   end;
   XXX; // convert SubjectiveInformation.Directions into a single Direction
   Assert(Direction in [FFrontSideFacesDirection, cdReverse[FFrontSideFacesDirection]]);
   if (Direction = FFrontSideFacesDirection) then
   begin
      if (FFrontSideDescription <> '') then
         Result := FFrontSideDescription
      else
         Result := inherited;
   end
   else
   begin
      if (FBackSideDescription <> '') then
         Result := FBackSideDescription
      else
         Result := inherited;
   end;
end;


constructor TThresholdLocation.Create(Landmark: TThing; Surface: TThing);
begin
   inherited Create(Landmark, tpAtImplicit, Surface);
end;

function TThresholdLocation.GetTitle(Perspective: TAvatar): UTF8String;
begin
   Result := GetName(Perspective) + WithSpaceIfNotEmpty(GetContextFragment(Perspective, tpAt, nil));
end;

function TThresholdLocation.GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
begin
   Result := FMaster.GetPresenceStatement(Perspective, psThereIsAThingThere);
end;

function TThresholdLocation.GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String;
begin
   Assert(Assigned(FMaster));
   if (Context <> FMaster) then
      Result := FMaster.GetDescriptionSelf(Perspective) + WithNewlineIfMultiline(inherited)
   else
      Result := inherited;
end;

function TThresholdLocation.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
begin
   Result := FMaster.GetDescriptionDirectional(Perspective, Mode, Direction);
end;

function TThresholdLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
begin
   Result := FMaster.GetDescriptionRemoteDetailed(Perspective, Direction);
end;

function TThresholdLocation.GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String;
const
   kNecessaryOptions = [loAutoDescribe, loPermissibleNavigationTarget];
var
   Direction: TCardinalDirection;
   Index: Cardinal;
   List: TAtomList;
begin
   Assert(Context <> Self);
   List := TAtomList.Create([slDropDuplicates]);
   for Direction in TCardinalDirection do
      if (Length(FDirectionalLandmarks[Direction]) > 0) then
         for Index := Low(FDirectionalLandmarks[Direction]) to High(FDirectionalLandmarks[Direction]) do
         begin
            Assert(Context <> FDirectionalLandmarks[Direction][Index].Atom);
            if (FDirectionalLandmarks[Direction][Index].Options * kNecessaryOptions = kNecessaryOptions) then
               List.AppendItem(FDirectionalLandmarks[Direction][Index].Atom);
         end;
   if (List.Length >= 2) then
      Result := 'between ' + List.GetDefiniteString(Perspective, 'and')
   else
   if (List.Length = 1) then
      Result := 'near ' + List.First.GetDefiniteName(Perspective)
   else
      Result := inherited;
   List.Free();
end;

procedure TThresholdLocation.AddExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter);
begin
   if (Distance > 0) then
      FMaster.AddExplicitlyReferencedThings(Tokens, Start, Perspective, True, Reporter);
   inherited;
end;

function TThresholdLocation.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := FMaster.FindThingTraverser(Thing, Perspective, FromOutside);
end;

procedure TThresholdLocation.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   FMaster.FindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
end;

function TThresholdLocation.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   DisambiguationOpening := FMaster;
   NotificationList.AppendItem(FMaster);
   Result := GetAtomForDirectionalNavigation(Direction);
   if (Assigned(Result)) then
      Result := Result.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
   else
      Result := inherited;
end;

initialization
{$INCLUDE registrations/threshold.inc}
end.
