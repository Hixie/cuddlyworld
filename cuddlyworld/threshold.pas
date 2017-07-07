{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit threshold;

interface

uses
   locations, things, thingdim, grammarian, matcher, storable, physics, messages;

type
   TRelativePerspectivePosition = (rppFront, rppBack, rppHere);
   TVisibleSide = (vsFront, vsBack);
   TVisibleSides = set of TVisibleSide;

type
   TThresholdThing = class abstract(TScenery) // not sure we should really inherit from TScenery, maybe some other class that only has some of TScenery's baggage...
    protected
     var
      FFrontSideFacesDirection: TCardinalDirection;
      function LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean; virtual;
      property FrontSideFacesDirection: TCardinalDirection read FFrontSideFacesDirection write FFrontSideFacesDirection;
   end;

   TStaticThresholdThing = class(TThresholdThing) // @RegisterStorableClass
    // This is for something that's always traversable, like an archway or something
    // note that we inherit from a class that defines Openable, so IsOpen() might be true or false
    // but that doesn't mean that when IsOpen() is false, we're not traversable
    protected
      FFrontSideDescription: UTF8String;
      FBackSideDescription: UTF8String;
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      property FrontSideDescription: UTF8String read FFrontSideDescription write FFrontSideDescription;
      property BackSideDescription: UTF8String read FBackSideDescription write FBackSideDescription;
   end;

   TDoor = class;

   TDoorWay = class(TThresholdThing) // @RegisterStorableClass
    protected // open state is stored in inherited FOpened boolean
      const tpConsiderForDoorPosition = tpIn; // if you try to use this, it'll turn into tpOfficialDoorPosition
      const tpOfficialDoorPosition = tpInstalledIn; // this must have at most one TDoor that is tpOfficialDoorPosition at any one time
      const tpOnGround = tpOn;
      function GetDoor(): TDoor; // or nil if there isn't one
      function GetCouldBeDoor(Thing: TThing; ThingPosition: TThingPosition): Boolean;
      procedure Removed(Thing: TThing); override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection; Door: TDoor = nil);
      function IsClear(): Boolean; virtual;
      procedure EnumerateObtrusiveObstacles(List: TThingList); override;
      procedure ProxiedEnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter); override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      procedure Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar); override;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); override;
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetDefaultDestination(out ThingPosition: TThingPosition): TThing; override;
      function GetLookIn(Perspective: TAvatar): UTF8String; override;
      function GetLookThrough(Perspective: TAvatar): UTF8String; virtual; // this gives the answer regardless of whether there's a door, it's open, or whatever
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
      function GetDescriptionObstacles(Perspective: TAvatar; NoObstacleMessage: UTF8String = ''): UTF8String; virtual;
      function GetDescriptionEmpty(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionClosed(Perspective: TAvatar): UTF8String; override; // defers to the door
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetFeatures(): TThingFeatures; override;
      function CanSeeIn(): Boolean; override;
      function CanSeeThrough(): Boolean; virtual;
      function Open(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function Close(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean; override;
      property Door: TDoor read GetDoor; // can be nil, if there's no door
   end;

   TDoorSide = class;

   TDoor = class(TDescribedPhysicalThing) // @RegisterStorableClass
    // The description (settable via the .Description property) will override
    // using the sides when both sides are visible. This may be helpful if the
    // sides have identical descriptions (in which case the normal behaviour
    // of including both in the overall description is ugly).
    protected
      FFrontSide, FBackSide: TDoorSide;
      function GetDoorWay(): TDoorWay; // or nil if the door isn't in a doorway
      // GetLock() could work a similar way
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
      function GetMatcherFlags(Perspective: TAvatar): TMatcherFlags; override;
      function LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
      function DetermineVisibleSides(Perspective: TAvatar): TVisibleSides;
    public
      const mfOpen: TMatcherFlag = 1;
      const mfClosed: TMatcherFlag = 2;
      constructor Create(Name: UTF8String; Pattern: UTF8String; FrontSide, BackSide: TDoorSide; AMass: TThingMass = tmHeavy; ASize: TThingSize = tsMassive);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function CanSeeUnder(Perspective: TAvatar): Boolean; virtual;
      function GetCannotSeeUnder(Perspective: TAvatar): UTF8String; virtual;
      function GetDescriptionClosed(Perspective: TAvatar): UTF8String; override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction; override;
      function GetFeatures(): TThingFeatures; override;
      function Open(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function Close(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      property DoorWay: TDoorWay read GetDoorWay; // can be nil, if the door is not in a doorway
      property Description: UTF8String read FDescription write FDescription;
   end;

   TDoorSide = class(TFeature) // @RegisterStorableClass
    // description argument to constructor shouldn't have a capital first letter
    // it gets concatenated to leading clauses like "On the front side, "...
    protected
      function GetMatcherFlags(Perspective: TAvatar): TMatcherFlags; override;
    public
      const mfOtherSideVisible: TMatcherFlag = 1;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionSelfSentenceFragment(Perspective: TAvatar): UTF8String; virtual;
   end;

   TThresholdSurface = class(TSurface) // @RegisterStorableClass
    public
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
   end;

   TThresholdLocation = class(TSurfaceSlavedLocation) // @RegisterStorableClass
    public
      constructor Create(Landmark: TThing; Surface: TThing);
      function GetTitle(Perspective: TAvatar): UTF8String; override;
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; override;
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); override;
      procedure EnumerateExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter); override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
   end;

// XXX wall with a hole in it... wall without a hole in it... wall that can be hit to make a hole in it...

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing = nil; Flags: TLocation.TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;
// if you omit loAutoDescribe from the last argument, then the threshold won't be mentioned in descriptions of rooms that contain it

implementation

uses
   lists, exceptions, broadcast;

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing; Flags: TLocation.TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;
begin
   if (not Assigned(Surface)) then
      Surface := TThresholdSurface.Create('floor', 'flat? (ground/grounds floor/floors)@', 'The floor is flat.');
   Flags := Flags + [loPermissibleNavigationTarget, loThreshold];
   Result := TThresholdLocation.Create(Threshold, Surface);
   FrontLocation.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], Result, Flags);
   Result.AddLandmark(Threshold.FrontSideFacesDirection, FrontLocation, [loAutoDescribe, loPermissibleNavigationTarget, loNotVisibleFromBehind]);
   BackLocation.AddLandmark(Threshold.FrontSideFacesDirection, Result, Flags);
   Result.AddLandmark(cdReverse[Threshold.FrontSideFacesDirection], BackLocation, [loAutoDescribe, loPermissibleNavigationTarget, loNotVisibleFromBehind]);
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

function TThresholdThing.LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
var
   SubjectiveInformation: TSubjectiveInformation;
   Direction, CandidateDirection: TCardinalDirection;
begin
   SubjectiveInformation := Perspective.Locate(Self);
   if (PopCnt(Cardinal(SubjectiveInformation.Directions)) <> 1) then
   begin
      // e.g. if you're right there at the archway or whatever
      // or if there's a trapdoor on some object instead of it being a directional landmark
      // we assume that if we can't find Perspective at all, that we're here somehow
      Result := rppHere;
   end
   else
   begin
      for CandidateDirection in SubjectiveInformation.Directions do
         Direction := CandidateDirection; // there can only be one at this point, so this should be enough
      Assert(Direction in [FFrontSideFacesDirection, cdReverse[FFrontSideFacesDirection]]);
      if (Direction = FFrontSideFacesDirection) then
         Result := rppBack
      else
         Result := rppFront;
   end;
end;

function TThresholdThing.CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean;
begin
   Result := True;
end;


function TStaticThresholdThing.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   case (LocatePerspective(Perspective)) of
      rppFront: 
         begin
            if (FFrontSideDescription <> '') then
               Result := FFrontSideDescription
            else
               Result := inherited;
         end;
      rppBack:
         begin
            if (FBackSideDescription <> '') then
               Result := FBackSideDescription
            else
               Result := inherited;
         end;
      else // rppHere
         Result := inherited;
   end;
end;


constructor TDoorWay.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; FrontFacesDirection: TCardinalDirection; Door: TDoor = nil);
begin
   inherited Create(Name, Pattern, Description, FrontFacesDirection);
   if (Assigned(Door)) then
   begin
      Add(Door, tpOfficialDoorPosition);
      Assert(Door.Position in tpArguablyInside);
   end;
end;

function TDoorWay.GetDoor(): TDoor;
var
   Child: TThing;
begin
   Result := nil;
   for Child in FChildren do
      if (GetCouldBeDoor(Child, Child.Position)) then
      begin
         Assert(Child.Position in tpArguablyInside);
         Assert(not Assigned(Result));
         Result := Child as TDoor;
         {$IFOPT C-} exit; {$ENDIF}
      end;
end;

function TDoorWay.GetCouldBeDoor(Thing: TThing; ThingPosition: TThingPosition): Boolean;
begin
   Result := (Thing is TDoor) and (ThingPosition in [tpOfficialDoorPosition, tpConsiderForDoorPosition]) and ((Thing as TDoor).Size = FSize);
end;

function TDoorWay.IsClear(): Boolean;
var
   List: TThingList;
begin
   List := GetObtrusiveObstacles();
   Result := List.Length = 0;
   List.Free();
end;

procedure TDoorWay.EnumerateObtrusiveObstacles(List: TThingList);
begin
   inherited;
   if (FParent is TThresholdLocation) then
      FParent.EnumerateObtrusiveObstacles(List);
end;

procedure TDoorWay.ProxiedEnumerateExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter);
var
   Obstacles: TThingList;
   Obstacle: TThing;
begin
   inherited;
   Obstacles := GetObtrusiveObstacles();
   try
      for Obstacle in Obstacles do // should we check IsChildTraversable() ?
         Obstacle.ProxiedEnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, FromOutside, Reporter);
   finally
      Obstacles.Free();
   end;      
end;

procedure TDoorWay.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
var
   Obstacles: TThingList;
   Obstacle: TThing;
begin
   inherited;
   Obstacles := GetObtrusiveObstacles();
   try
      for Obstacle in Obstacles do // should we check IsChildTraversable() ?
         Obstacle.ProxiedFindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
   finally
      Obstacles.Free();
   end;      
end;

function TDoorWay.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
var
   Obstacles: TThingList;
   Obstacle: TThing;
begin
   Result := inherited;
   if (Result) then
      exit;
   Obstacles := GetObtrusiveObstacles();
   try
      for Obstacle in Obstacles do // should we check IsChildTraversable() ?
         if (Obstacle.ProxiedFindThingTraverser(Thing, Perspective, FromOutside)) then
         begin
            Result := True;
            exit;
         end;
   finally
      Obstacles.Free();
   end;      
end;

function TDoorWay.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
var
   OldDoor: TDoor;
   CouldBeDoor: Boolean;
   DoorObstacles: TThingList;
begin
   Assert(Message.IsValid);
   OldDoor := GetDoor();
   if (ThingPosition = tpOn) then
   begin
      if (Assigned(OldDoor)) then
         Message := TMessage.Create(mkClosed, '_ can''t put something on _.',
                                   [Capitalise(Perspective.GetDefiniteName(Perspective)), GetIndefiniteName(Perspective)])
      else
         Message := TMessage.Create(mkClosed, '_ can''t put something on _. Did you mean on _?',
                                   [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                    GetIndefiniteName(Perspective),
                                    OldDoor.GetDefiniteName(Perspective)]);
      Result := False;
   end
   else
   if (ThingPosition = tpIn) then
   begin
      CouldBeDoor := GetCouldBeDoor(Thing, ThingPosition) and (Care = psCarefully);
      if (CouldBeDoor) then
      begin
         if (Assigned(OldDoor)) then
         begin
            // can't install a door when there's already a door
            Message := TMessage.Create(mkDuplicate, '_ already _ _.', [Capitalise(GetDefiniteName(Perspective)),
                                                                       TernaryConditional('has', 'have', IsPlural(Perspective)),
                                                                       OldDoor.GetIndefiniteName(Perspective)]);
            Result := False;
         end
         else
         begin
            DoorObstacles := Thing.GetObtrusiveObstacles();
            try
               if (DoorObstacles.Length > 0) then
               begin
                  Message := TMessage.Create(mkBlocked, '_ cannot install _ _ _ while _ _ _ _.',
                                             [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                              Thing.GetIndefiniteName(Perspective),
                                              ThingPositionToString(tpConsiderForDoorPosition),
                                              GetIndefiniteName(Perspective),
                                              DoorObstacles.GetIndefiniteString(Perspective, 'or'),
                                              IsAre(DoorObstacles.IsPlural(Perspective)),
                                              ThingPositionToString(DoorObstacles.First.Position),
                                              Thing.GetObjectPronoun(Perspective)]);
                  Result := False;
               end
               else
               begin
                  Message := TMessage.Create(mkSuccess, '_ _ _ _ _.',
                                             [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                              TernaryConditional('installs', 'install', Perspective.IsPlural(Perspective)),
                                              Thing.GetIndefiniteName(Perspective),
                                              ThingPositionToString(tpConsiderForDoorPosition),
                                              GetDefiniteName(Perspective)]);
                  Result := True;
               end;
            finally
               DoorObstacles.Free();
            end;
         end;
      end
      else
      if (FParent is TThresholdLocation) then
      begin
         // just dump the junk in the threshold location
         Assert(Assigned(FParent.GetSurface()));
         Result := FParent.GetSurface().CanPut(Thing, tpOn, Care, Perspective, Message);
      end
      else
      if (Assigned(OldDoor) and not IsOpen()) then
      begin
         // can't put something inside a closed doorway, whether it could itself be a door or not
         Message := TMessage.Create(mkClosed, GetDescriptionClosed(Perspective));
         Result := False;
      end
      else
      begin
         // just dump the stuff in us
         Result := inherited;
      end;
   end
   else
      Assert(False); // CanPut only supports tpOn and tpIn
end;

procedure TDoorWay.Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar);
var
   Ground: TAtom;
begin
   if (((not GetCouldBeDoor(Thing, ThingPosition)) or (Care <> psCarefully) or (Assigned(GetDoor()))) and (FParent is TThresholdLocation)) then
   begin
      Ground := FParent.GetSurface();
      Assert(Assigned(Ground));
      DoBroadcast([Self, Ground], Perspective,
                  [C(M(@Perspective.GetDefiniteName)), SP, // You
                   MP(Perspective, M('drops'), M('drop')), SP, // drop
                   M(@Thing.GetDefiniteName), SP, // the door
                   M(ThingPositionToString(ThingPosition)), SP, // in
                   M(@GetDefiniteName), // the door way
                   M(', '),
                   M(ThingPositionToString(tpOnGround)), SP, // to
                   M(@Ground.GetDefiniteName), // the ground
                   M('.')]);
      Ground.Put(Thing, tpOnGround, Care, Perspective);
   end
   else
   begin
      Assert(Care = psCarefully);
      Assert(ThingPosition in [tpConsiderForDoorPosition, tpOfficialDoorPosition]);
      inherited Put(Thing, tpOfficialDoorPosition, psCarefully, Perspective);
   end;
end;

procedure TDoorWay.HandleAdd(Thing: TThing; Blame: TAvatar);
begin
   if (Thing = GetDoor()) then
   begin
      FOpened := not IsClear();
      DoBroadcast([Self, Blame], Blame,
                  [C(M(@Blame.GetDefiniteName)), SP, // You
                   MP(Blame, M('installs'), M('install')), SP, // install
                   M(@Thing.GetIndefiniteName), SP, // a thing
                   M(ThingPositionToString(tpConsiderForDoorPosition)), SP, // in
                   M(@GetDefiniteName), // the door way
                   M('. '),
                   C(M(@Thing.GetSubjectPronoun)), SP, // It
                   MP(Thing, M('is'), M('are')), SP, // is 
                   M(TernaryConditional('closed', 'open', IsOpen())),
                   M('.')]);
   end;
   inherited;
end;

procedure TDoorWay.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
      TheDoor.HandlePassedThrough(Traveller, AFrom, ATo, AToPosition, Perspective);
end;

procedure TDoorWay.Removed(Thing: TThing);
begin
   if (not Assigned(GetDoor())) then
      FOpened := True;
   inherited;
end;

function TDoorWay.GetInside(var PositionOverride: TThingPosition): TThing;
begin
   Result := Self;
end;

function TDoorWay.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function TDoorWay.GetDefaultDestination(out ThingPosition: TThingPosition): TThing;
begin
   ThingPosition := tpOn;
   if (FParent is TThresholdLocation) then
      Result := FParent.GetSurface()
   else
      Result := Self;
   Assert(Assigned(Result));
end;

function TDoorWay.GetLookIn(Perspective: TAvatar): UTF8String;
begin
   if (CanSeeThrough()) then
   begin
      Result := GetLookThrough(Perspective);
      if (Result = '') then
         Result := inherited;
   end
   else
      Result := inherited;
end;

function TDoorWay.GetLookUnder(Perspective: TAvatar): UTF8String;
var
   TheDoor: TDoor;
   Directions: TCardinalDirectionSet;
begin
   Result := '';
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
   begin
      Directions := cdAllDirections;
      case (LocatePerspective(Perspective)) of
         rppFront: Exclude(Directions, cdReverse[FFrontSideFacesDirection]);
         rppBack: Exclude(Directions, FFrontSideFacesDirection);
      end;
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' +
                TernaryConditional('contains', 'contain', IsPlural(Perspective)) + ' ' +
                TheDoor.GetIndefiniteName(Perspective) + '.' +
                WithSpaceIfNotEmpty(TheDoor.GetBasicDescription(Perspective, psThereIsAThingHere, Directions)) +
                WithNewlineIfNotEmpty(GetDescriptionObstacles(Perspective,
                         'Other than ' + TheDoor.GetDefiniteName(Perspective) + ', there is nothing in ' + GetDefiniteName(Perspective) + '.'));
   end
   else
   begin
      Result := GetDescriptionObstacles(Perspective, 'There is nothing in ' + GetDefiniteName(Perspective) + '.');
   end;
end;

function TDoorWay.GetLookThrough(Perspective: TAvatar): UTF8String;
begin
   Assert((not Assigned(GetDoor())) or
          IsOpen() or
          CanSeeThrough() or
          GetDoor().CanSeeUnder(Perspective));
   case (LocatePerspective(Perspective)) of
      rppFront: Result := GetLookTowardsDirection(Perspective, cdReverse[FFrontSideFacesDirection]);
      rppBack: Result := GetLookTowardsDirection(Perspective, FFrontSideFacesDirection);
      else
         Result := '';
   end;
end;

function TDoorWay.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   if (not Assigned(TheDoor)) then
   begin
      if (CanSeeThrough()) then
      begin
         // XXX this is terrible
         // XXX this is just a stand-in until we can revamp GetLookThrough/GetDescriptionRemoteDetailed
         // XXX see TODO
         Perspective.AutoDisambiguated('looking through ' + GetDefiniteName(Perspective));
         Result := GetLookThrough(Perspective);
      end
      else
      begin
         // XXX I don't really understand what this branch is trying to do or what output from this branch would look like...
         // XXX but I don't think I like it.
         if ((lpMandatory in Options) or not (lpNamesTarget in Options)) then
            LeadingPhrase := LeadingPhrase + ','
         else
            LeadingPhrase := 'Looking';
         LeadingPhrase := LeadingPhrase + ' through ' + GetDefiniteName(Perspective);
         Include(Options, lpMandatory);
         Result := inherited;
      end;
   end
   else
   begin
      Exclude(Options, lpNamesTarget);
      if (IsOpen()) then
      begin
         if (CanSeeThrough()) then
         begin
            // XXX this is terrible also
            Perspective.AutoDisambiguated('looking through the open ' + TheDoor.GetName(Perspective));
            Result := GetLookThrough(Perspective);
         end
         else
         begin
            Result := TheDoor.GetDescriptionRemoteDetailed(Perspective, Direction, LeadingPhrase, Options);
         end;
      end
      else
         Result := TheDoor.GetDescriptionRemoteDetailed(Perspective, Direction, LeadingPhrase, Options);
   end;
   Result := Result + WithSpaceIfNotEmpty(GetDescriptionObstacles(Perspective));
end;

function TDoorWay.GetDescriptionObstacles(Perspective: TAvatar; NoObstacleMessage: UTF8String = ''): UTF8String;
var
   Obstacles: TThingList;
begin
   Result := '';
   Obstacles := GetObtrusiveObstacles();
   try
      if (Obstacles.Length > 0) then
         Result := 'Blocking ' +
                   GetDefiniteName(Perspective) + ' ' +
                   IsAre(Obstacles.IsPlural(Perspective)) + ' ' +
                   Obstacles.GetIndefiniteString(Perspective, 'and') + '.'
      else
         Result := NoObstacleMessage;
   finally
      Obstacles.Free();
   end;
end;

function TDoorWay.GetDescriptionEmpty(Perspective: TAvatar): UTF8String;
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
      Result := 'Other than ' + TheDoor.GetDefiniteName(Perspective) + ', there is nothing in ' + GetDefiniteName(Perspective) + '.'
   else
      Result := inherited;
end;

function TDoorWay.GetDescriptionClosed(Perspective: TAvatar): UTF8String;
var
   TheDoor: TDoor;
begin
   TheDoor := GetDoor();
   Assert(Assigned(TheDoor) and not IsOpen());
   if (Assigned(TheDoor)) then
      Result := TheDoor.GetDescriptionClosed(Perspective)
   else
      Result := inherited;
end;

function TDoorWay.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   if (IsOpen() and (FParent is TThresholdLocation)) then
   begin
      if (Direction = cdIn) then
      begin
         case (LocatePerspective(Perspective)) of
            rppFront: Direction := cdReverse[FFrontSideFacesDirection];
            rppBack: Direction := FFrontSideFacesDirection;
         end;
      end;
      PositionOverride := tpOn;
      Result := FParent.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
   end
   else
      Result := inherited;
end;

function TDoorWay.CanSeeIn(): Boolean;
begin
   Result := True;
end;

function TDoorWay.CanSeeThrough(): Boolean;
begin
   Result := (not Assigned(GetDoor())) or (IsOpen());
end;

function TDoorWay.Open(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoor: TDoor;
begin
   Assert(Message.IsValid);
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
   begin
      if (IsOpen()) then
      begin
         Message := TMessage.Create(mkRedundant, '_ in _ _ already open.',
                                                 [Capitalise(TheDoor.GetDefiniteName(Perspective)),
                                                  GetDefiniteName(Perspective),
                                                  IsAre(IsPlural(Perspective))]);
         Result := False;
      end
      else
      begin
         Assert(IsClear());
         DoBroadcast([TheDoor, Perspective], Perspective, [C(M(@Perspective.GetDefiniteName)), SP,
                                                           MP(Perspective, M('opens'), M('open')), SP,
                                                           M(@TheDoor.GetDefiniteName), M('.')]);
         FOpened := True;
         Result := True;
      end;
   end
   else
   begin
      Assert(IsOpen());
      Message := TMessage.Create(mkNoDoor, '_ _ wide open.', [Capitalise(GetDefiniteName(Perspective)), IsAre(IsPlural(Perspective))]);
      Result := False;
   end;
end;

function TDoorWay.Close(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoor: TDoor;
   Obstacles, MoreObstacles: TThingList;
begin
   Assert(Message.IsValid);
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
   begin
      if (not IsOpen()) then
      begin
         Message := TMessage.Create(mkRedundant, '_ _ _ _ already closed.',
                                                 [Capitalise(TheDoor.GetDefiniteName(Perspective)),
                                                  ThingPositionToString(TheDoor.Position),
                                                  GetDefiniteName(Perspective),
                                                  IsAre(IsPlural(Perspective))]);
         Result := False;
      end
      else
      begin
         Obstacles := GetObtrusiveObstacles();
         try
            MoreObstacles := TheDoor.GetObtrusiveObstacles();
            try
               Obstacles.AdoptList(MoreObstacles);
            finally
               MoreObstacles.Free();
            end;
            if (Obstacles.Length > 0) then
            begin
               Message := TMessage.Create(mkBlocked, '_ cannot be closed; _ _ in the way.',
                                          [Capitalise(TheDoor.GetDefiniteName(Perspective)),
                                           Obstacles.GetDefiniteString(Perspective, 'and'),
                                           IsAre(Obstacles.IsPlural(Perspective))]);
               Result := False;
               exit;
            end;
         finally
            Obstacles.Free();
         end;      
         DoBroadcast([TheDoor, Perspective], Perspective, [C(M(@Perspective.GetDefiniteName)), SP,
                                                           MP(Perspective, M('closes'), M('close')), SP,
                                                           M(@TheDoor.GetDefiniteName), M('.')]);
         Result := True;
         FOpened := False;
      end;
   end
   else
   begin
      Assert(IsOpen());
      Message := TMessage.Create(mkNoDoor, 'There is nothing in _ with which to close _.', [GetDefiniteName(Perspective), GetObjectPronoun(Perspective)]);
      Result := False;
   end;
end;

function TDoorWay.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfCanHaveThingsPushedIn];
end;

function TDoorWay.CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean;
begin
   Result := (not Assigned(GetDoor())) or (IsOpen());
end;


constructor TDoor.Create(Name: UTF8String; Pattern: UTF8String; FrontSide, BackSide: TDoorSide; AMass: TThingMass = tmHeavy; ASize: TThingSize = tsMassive);
begin
   inherited Create(Name, Pattern, '' { description }, AMass, ASize);
   Assert(Assigned(FrontSide));
   FFrontSide := FrontSide;
   Add(FrontSide, tpAmbiguousPartOfImplicit);
   Assert(Assigned(BackSide));
   FBackSide := BackSide;
   Add(BackSide, tpAmbiguousPartOfImplicit);
end;

constructor TDoor.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FFrontSide));
   Stream.ReadReference(@Pointer(FBackSide));
end;

procedure TDoor.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FFrontSide);
   Stream.WriteReference(FBackSide);
end;

function TDoor.GetDoorWay(): TDoorWay;
begin
   if ((FParent is TDoorWay) and (FPosition = (FParent as TDoorWay).tpOfficialDoorPosition)) then
      Result := FParent as TDoorWay
   else
      Result := nil;
end;

function TDoor.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
var
   VisibleSides: TVisibleSides;
begin
   VisibleSides := DetermineVisibleSides(Perspective);
   if (Child = FBackSide) then
      Result := (vsBack in VisibleSides) and inherited
   else
   if (Child = FFrontSide) then
      Result := (vsFront in VisibleSides) and inherited
   else
      Result := inherited;
end;

function TDoor.GetMatcherFlags(Perspective: TAvatar): TMatcherFlags;
var
   TheDoorWay: TDoorWay;
begin
   Result := inherited;
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      if (TheDoorWay.IsOpen()) then
         Result := Result or (1 shl mfOpen) // $R-
      else
         Result := Result or (1 shl mfClosed); // $R-
   end;
end;

function TDoor.LocatePerspective(Perspective: TAvatar): TRelativePerspectivePosition;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
      Result := TheDoorWay.LocatePerspective(Perspective)
   else
      Result := rppHere;
end;

function TDoor.DetermineVisibleSides(Perspective: TAvatar): TVisibleSides;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay) and not TheDoorWay.IsOpen()) then
   begin
      case (LocatePerspective(Perspective)) of
         rppFront: Result := [vsFront];
         rppBack: Result := [vsBack];
       else
          Result := [vsFront, vsBack];
      end;
   end
   else
      Result := [vsFront, vsBack];
end;

function TDoor.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoorWay: TDoorWay;
begin
   Assert(Message.IsValid);
   if (ThingPosition = tpOn) then
   begin
      TheDoorWay := GetDoorWay();
      if (Assigned(TheDoorWay)) then
      begin
         if (not TheDoorWay.IsOpen()) then
         begin
            Message := TMessage.Create(mkCannotPutOnBecauseInstalled, TheDoorWay.GetDescriptionNoInside(Perspective));
            Result := False;
            exit;
         end;
      end;
   end;
   Result := inherited;
end;

procedure TDoor.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
var
   Obstacles: TThingList;
   TheDoorWay: TDoorWay;
   Ground: TAtom;
   Thing: TThing;
   Message: TMessage;
begin
   TheDoorWay := GetDoorWay();
   Assert(Assigned(TheDoorWay) and TheDoorWay.IsOpen());
   Obstacles := GetObtrusiveObstacles();
   try
      if (Obstacles.Length > 0) then
      begin
         if (TheDoorWay.Parent is TThresholdLocation) then
         begin
            Ground := TheDoorWay.Parent.GetSurface();
            Assert(Assigned(Ground));
            // mkThingsFall
            DoBroadcastAll([Self, Ground, Traveller, Perspective],
                           [C(M(@Obstacles.GetDefiniteString, M('and'))), SP,
                            MP(@Obstacles.IsPlural, M('falls'), M('fall')), SP,
                            M(ThingPositionToDirectionString(TDoorWay.tpOnGround)), SP,
                            M(@Ground.GetDefiniteName), SP,
                            M('as'), SP,
                            M(@Traveller.GetDefiniteName), SP,
                            MP(Traveller, M('passes'), M('pass')), SP,
                            M('through'), SP,
                            M(@GetDefiniteName), M(','), SP,
                            M('barely missing'), SP,
                            M(@Traveller.GetObjectPronoun), SP,
                            M('on'), SP,
                            M(@Obstacles.GetPossessiveAdjective), SP,
                            M('way down.')]);
            // XXX things should actually hit the Traveller on the way down...
            for Thing in Obstacles do
            begin
               {$IFOPT C+}
                  Message := TMessage.Create();
                  Assert(Ground.CanPut(Thing, TDoorWay.tpOnGround, psRoughly, Perspective, Message));
                  Assert(Message.AsKind = mkSuccess);
                  Assert(Message.AsText = '');
               {$ENDIF}
               Ground.Put(Thing, TDoorWay.tpOnGround, psRoughly, Perspective);
            end;
         end
         else
         begin
            // You wade through the foo and the foo on your way through the door.
            DoBroadcastAll([Self, Traveller, Perspective],
                           [C(M(@Traveller.GetDefiniteName)), SP,
                           MP(Traveller, M('wades'), M('wade')), SP,
                           M('through'), SP,
                           M(@Obstacles.GetDefiniteString, M('and')), SP,
                           M('on'), SP,
                           M(@Traveller.GetPossessiveAdjective), SP,
                           M('way through'), SP,
                           M(@GetDefiniteName), M('.')]);
         end;
      end;
   finally
      Obstacles.Free();
   end;
end;

function TDoor.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
var
   VisibleSides: TVisibleSides;
   TheDoorWay: TDoorWay;
begin
   VisibleSides := DetermineVisibleSides(Perspective);
   if (VisibleSides = [vsFront]) then
   begin
      Result := FFrontSide.GetDescriptionSelf(Perspective);
   end
   else
   if (VisibleSides = [vsBack]) then
   begin
      Result := FBackSide.GetDescriptionSelf(Perspective);
   end
   else
   begin
      Assert(VisibleSides = [vsFront, vsBack]);
      if (FDescription <> '') then
         Result := FDescription
      else
         Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('has', 'have', IsPlural(Perspective)) + ' two sides. On the front, ' + FFrontSide.GetDescriptionSelfSentenceFragment(Perspective) + ' On the back, ' + FBackSide.GetDescriptionSelfSentenceFragment(Perspective);
   end;
   Assert(Result <> '');
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      Result := Result + ' ' + Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + TernaryConditional('closed', 'open', TheDoorWay.IsOpen()) + '.';
   end;
end;

function TDoor.GetLookUnder(Perspective: TAvatar): UTF8String;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      if (TheDoorWay.IsOpen()) then
         Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' open.'
      else
      if (CanSeeUnder(Perspective)) then
      begin
         Assert(LocatePerspective(Perspective) in [rppFront, rppBack]);
         Result := TheDoorWay.GetLookThrough(Perspective);
         if (Result = '') then
            Result := inherited;
      end
      else
         Result := GetCannotSeeUnder(Perspective);
   end
   else
      Result := inherited;
end;

function TDoor.CanSeeUnder(Perspective: TAvatar): Boolean;
begin
   Result := True;
end;

function TDoor.GetCannotSeeUnder(Perspective: TAvatar): UTF8String;
begin
   Assert(not CanSeeUnder(Perspective));
   Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' cannot see under ' + GetDefiniteName(Perspective) + '.';
end;

function TDoor.GetDescriptionClosed(Perspective: TAvatar): UTF8String;
var
   TheDoorWay: TDoorWay;
begin
   TheDoorWay := GetDoorWay();
   Assert(Assigned(TheDoorWay) and not TheDoorWay.IsOpen());
   if (Assigned(TheDoorWay) and not TheDoorWay.IsOpen()) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' closed.'
   else
      Result := inherited;
end;

function TDoor.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
var
   TheDoorWay: TThing;
begin
   Assert(Message.IsValid);
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
      Result := TheDoorWay.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
   else
      Result := inherited;
end;

function TDoor.GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction;
var
   TheDoorWay: TThing;
begin
   Assert(Message.IsValid);
   TheDoorWay := GetDoorWay();
   if ((Direction = cdOut) and Assigned(TheDoorWay) and TheDoorWay.IsOpen()) then
      Result := TheDoorWay.GetNavigationInstructions(Direction, Self, Perspective, Message)
   else
      Result := inherited;
end;

function TDoor.GetFeatures(): TThingFeatures;
var
   TheDoorWay: TThing;
begin
   Result := inherited;
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      if (TheDoorWay.IsOpen()) then
         Result := Result + [tfClosable]
      else
         Result := Result + [tfOpenable];
   end;
end;

function TDoor.Open(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoorWay: TDoorWay;
begin
   Assert(Message.IsValid);
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      Result := TheDoorWay.Open(Perspective, Message);
   end
   else
   begin
      Message := TMessage.Create(mkBogus, '_ _ _ _, so there is no way to open _.',
                                 [Capitalise(GetDefiniteName(Perspective)),
                                  IsAre(IsPlural(Perspective)),
                                  ThingPositionToString(FPosition),
                                  FParent.GetDefiniteName(Perspective),
                                  GetObjectPronoun(Perspective)]);
      Result := False;
   end;
end;

function TDoor.Close(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoorWay: TDoorWay;
begin
   Assert(Message.IsValid);
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      Result := TheDoorWay.Close(Perspective, Message);
   end
   else
   begin
      Message := TMessage.Create(mkBogus, '_ _ _ _, so there is no way to close _.',
                                 [Capitalise(GetDefiniteName(Perspective)),
                                  IsAre(IsPlural(Perspective)),
                                  ThingPositionToString(FPosition),
                                  FParent.GetDefiniteName(Perspective),
                                  GetObjectPronoun(Perspective)]);
      Result := False;
   end;
end;


function TDoorSide.GetMatcherFlags(Perspective: TAvatar): TMatcherFlags;
var
   TheDoorWay: TDoorWay;
begin
   Result := inherited;
   if (FParent is TDoor) then
   begin
      TheDoorWay := (FParent as TDoor).GetDoorWay();
      if (Assigned(TheDoorWay) and not TheDoorWay.IsOpen()) then
         exit;
   end;
   Result := Result or (1 shl mfOtherSideVisible); // $R-
end;

function TDoorSide.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := Capitalise(inherited);
end;

function TDoorSide.GetDescriptionSelfSentenceFragment(Perspective: TAvatar): UTF8String;
begin
   Result := inherited GetDescriptionSelf(Perspective);
end;


function TThresholdSurface.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := inherited;
   if (Result and
          Assigned(FParent) and
          (FParent is TThresholdLocation) and
          ((FParent as TThresholdLocation).FMaster is TDoorway) and
          Assigned(((FParent as TThresholdLocation).FMaster as TDoorway).GetDoor()) and
          not ((FParent as TThresholdLocation).FMaster as TDoorway).IsOpen()) then
   begin
      // can't put something inside a closed doorway, whether it could itself be a door or not
      Message := TMessage.Create(mkClosed, ((FParent as TThresholdLocation).FMaster as TDoorway).GetDescriptionNoInside(Perspective));
      Result := False;
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

function TThresholdLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
begin
   Exclude(Options, lpNamesTarget);
   Result := FMaster.GetDescriptionRemoteDetailed(Perspective, Direction, LeadingPhrase, Options);
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

procedure TThresholdLocation.GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass);
var
   Direction: TCardinalDirection;
begin
   for Direction := Low(FDirectionalLandmarks) to High(FDirectionalLandmarks) do
      if (Length(FDirectionalLandmarks[Direction]) > 0) then
         FDirectionalLandmarks[Direction][0].Atom.GetNearbyThingsByClass(List, True, Filter);
end;

procedure TThresholdLocation.EnumerateExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter);
begin
   if (Distance > 0) then
      FMaster.ProxiedEnumerateExplicitlyReferencedThings(Tokens, Start, Perspective, True, Reporter);
   inherited;
end;

function TThresholdLocation.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := FMaster.ProxiedFindThingTraverser(Thing, Perspective, FromOutside);
end;

procedure TThresholdLocation.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   FMaster.ProxiedFindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
end;

function TThresholdLocation.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   Assert(Assigned(FMaster));
   if ((FMaster is TThresholdThing) and (FMaster as TThresholdThing).CanTraverse(Traveller, Direction, Perspective)) then
   begin
      DisambiguationOpening := FMaster;
      NotificationList.AppendItem(FMaster);
      Result := GetAtomForDirectionalNavigation(Direction);
      if (Assigned(Result)) then
         Result := Result.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList)
      else
         Result := inherited;
   end
   else
   begin
      Message := TMessage.Create(mkClosed, FMaster.GetDescriptionNoInside(Perspective));
      Result := nil;
   end;
end;

initialization
{$INCLUDE registrations/threshold.inc}
end.
