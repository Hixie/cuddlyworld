{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit threshold;

interface

uses
   locations, things, grammarian, storable, world;

type
   TThresholdThing = class(TScenery) // @RegisterStorableClass
    protected
      FFrontSideFacesDirection: TCardinalDirection;
      FFrontSideDescription: AnsiString;
      FBackSideDescription: AnsiString;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; FrontFacesDirection: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      property FrontSideFacesDirection: TCardinalDirection read FFrontSideFacesDirection write FFrontSideFacesDirection;
      property FrontSideDescription: AnsiString read FFrontSideDescription write FFrontSideDescription;
      property BackSideDescription: AnsiString read FBackSideDescription write FBackSideDescription;
   end;

   // Should seriously consider factoring out the Landmark stuff in
   // TLocation just so we don't have to have the overhead here.
   // Just remember to take care of the ground and sky...
   TThresholdLocation = class(TSurfaceSlavedLocation) // @RegisterStorableClass
    protected
      FThreshold: TThresholdThing;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
    public
      constructor Create(Landmark: TThresholdThing; Surface: TThing);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      // GetDescriptionDirectional?
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      procedure AddExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter); override;
      // XXX make this give the landmark thing's description when the place is examined
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: AnsiString; NotificationList: TAtomList): TAtom; override;
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

constructor TThresholdThing.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; FrontFacesDirection: TCardinalDirection);
begin
   inherited Create(Name, Pattern, Description, tmPonderous, tsMassive);
   FFrontSideFacesDirection := FrontFacesDirection;
end;

constructor TThresholdThing.Read(Stream: TReadStream);
begin
   inherited;
   FFrontSideFacesDirection := TCardinalDirection(Stream.ReadCardinal());
   FFrontSideDescription := Stream.ReadAnsiString();
   FBackSideDescription := Stream.ReadAnsiString();
end;

procedure TThresholdThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FFrontSideFacesDirection));
   Stream.WriteAnsiString(FFrontSideDescription);
   Stream.WriteAnsiString(FBackSideDescription);
end;

function TThresholdThing.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
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
   inherited Create(Landmark, tpAt, Surface);
   FThreshold := Landmark;
end;

constructor TThresholdLocation.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FThreshold));
end;

procedure TThresholdLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FThreshold);
end;

function TThresholdLocation.GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := FThreshold.GetPresenceStatement(Perspective, psThereIsAThingThere);
end;

function TThresholdLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := FThreshold.GetDescriptionRemoteDetailed(Perspective, Direction);
end;

procedure TThresholdLocation.AddExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter);
begin
   if (Distance > 0) then
      FThreshold.AddExplicitlyReferencedThings(Tokens, Start, Perspective, True, Reporter);
   inherited;
end;

function TThresholdLocation.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := FThreshold.FindThingTraverser(Thing, Perspective, FromOutside);
end;

procedure TThresholdLocation.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   FThreshold.FindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
end;

function TThresholdLocation.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: AnsiString; NotificationList: TAtomList): TAtom;
begin
   DisambiguationOpening := FThreshold;
   NotificationList.AppendItem(FThreshold);
   Result := GetAtomForDirectionalNavigation(Direction);
   if (Assigned(Result)) then
      Result := Result.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
   else
      Result := inherited;
end;


function ConnectThreshold(SourceLocation: TLocation; Destination: TLocation; Surface: TThing; Threshold: TThresholdThing): TThresholdLocation;
begin
   Result := TThresholdLocation.Create(Threshold, Surface);
   ConnectLocations(Destination, Threshold.FrontSideFacesDirection, Result);
   ConnectLocations(Result, Threshold.FrontSideFacesDirection, SourceLocation);
end;

initialization
{$INCLUDE registrations/threshold.inc}
end.
