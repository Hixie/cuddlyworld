{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit threshold;

interface

uses
   locations, things, grammarian, storable, physics, messages;

type
   TThresholdThing = class(TScenery) // @RegisterStorableClass
    protected
      FFrontSideFacesDirection: TCardinalDirection;
      FFrontSideDescription: UTF8String;
      FBackSideDescription: UTF8String;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      property FrontSideFacesDirection: TCardinalDirection read FFrontSideFacesDirection write FFrontSideFacesDirection;
      property FrontSideDescription: UTF8String read FFrontSideDescription write FFrontSideDescription;
      property BackSideDescription: UTF8String read FBackSideDescription write FBackSideDescription;
   end;

   TThresholdLocation = class(TSurfaceSlavedLocation) // @RegisterStorableClass
    public
      constructor Create(Landmark: TThresholdThing; Surface: TThing);
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      procedure AddExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter); override;
      // XXX make this give the landmark thing's description when the place is examined
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
   end;

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing; Flags: TLocation.TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;

implementation

uses
   thingdim;

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing; Flags: TLocation.TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;
begin
   Flags := Flags + [loPermissibleNavigationTarget, loThreshold];
   Result := TThresholdLocation.Create(Threshold, Surface);
   FrontLocation.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], Result, Flags);
   Result.AddLandmark(Threshold.FrontSideFacesDirection, FrontLocation, [loAutoDescribe, loPermissibleNavigationTarget]);
   BackLocation.AddLandmark(Threshold.FrontSideFacesDirection, Result, Flags);
   Result.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], BackLocation, [loAutoDescribe, loPermissibleNavigationTarget]);
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
   FFrontSideDescription := Stream.ReadString();
   FBackSideDescription := Stream.ReadString();
end;

procedure TThresholdThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FFrontSideFacesDirection));
   Stream.WriteString(FFrontSideDescription);
   Stream.WriteString(FBackSideDescription);
end;

function TThresholdThing.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
begin
   Assert(Direction in [FFrontSideFacesDirection, cdReverse[FFrontSideFacesDirection]]);
   Result := 'Looking ' + CardinalDirectionToString(Direction) + ', you see ' + GetIndefiniteName(Perspective) + '. ';
   if (Direction = FFrontSideFacesDirection) then
   begin
      if (FFrontSideDescription <> '') then
         Result := Result + FFrontSideDescription
      else
         Result := Result + FDescription;
   end
   else
   begin
      if (FBackSideDescription <> '') then
         Result := Result + FBackSideDescription
      else
         Result := Result + FDescription;
   end;
end;


constructor TThresholdLocation.Create(Landmark: TThresholdThing; Surface: TThing);
begin
   inherited Create(Landmark, tpAtImplicit, Surface);
end;

function TThresholdLocation.GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
begin
   Result := FMaster.GetPresenceStatement(Perspective, psThereIsAThingThere);
end;

function TThresholdLocation.GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): UTF8String;
begin
   Assert(Assigned(FMaster));
   if (Context <> FMaster) then
      Result := FMaster.GetDescriptionSelf(Perspective) + WithNewlineIfMultiline(inherited)
   else
      Result := inherited;
end;

function TThresholdLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
begin
   Result := FMaster.GetDescriptionRemoteDetailed(Perspective, Direction);
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
