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
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetLookIn(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionEmpty(Perspective: TAvatar): UTF8String; override;
      function GetFeatures(): TThingFeatures; override;
      function CanSeeIn(): Boolean; override;
      function CanSeeThrough(): Boolean; virtual;
      function Open(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function Close(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanTraverse(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar): Boolean; override;
      property Door: TDoor read GetDoor; // can be nil, if there's no door
   end;

   TDoorSide = class;

   TDoor = class(TPhysicalThing) // @RegisterStorableClass
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
      function GetFeatures(): TThingFeatures; override;
      function Open(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function Close(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      property DoorWay: TDoorWay read GetDoorWay; // can be nil, if the door is not in a doorway
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

   TThresholdLocation = class(TSurfaceSlavedLocation) // @RegisterStorableClass
    public
      constructor Create(Landmark: TThing; Surface: TThing);
      function GetTitle(Perspective: TAvatar): UTF8String; override;
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Directions: TCardinalDirectionSet = cdAllDirections; Context: TAtom = nil): UTF8String; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; override;
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); override;
      procedure EnumerateExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter); override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
   end;

// XXX wall with a hole in it... wall without a hole in it... wall that can be hit to make a hole in it...

function ConnectThreshold(FrontLocation, BackLocation: TLocation; Threshold: TThresholdThing; Surface: TThing; Flags: TLocation.TLandmarkOptions = [loAutoDescribe]): TThresholdLocation;
// if you omit loAutoDescribe from the last argument, then the threshold won't be mentioned in descriptions of rooms that contain it

implementation

uses
   lists, exceptions, broadcast;

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

   function IsUnencumbered(): Boolean;
   var
      DoorObstacles: TThingList;
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
            Result := True;
         end;
      finally
         DoorObstacles.Free();
      end;
   end;

var
   OldDoor: TDoor;
   CouldBeDoor: Boolean;
begin
   OldDoor := GetDoor();
   CouldBeDoor := GetCouldBeDoor(Thing, ThingPosition);
   if ((ThingPosition in tpContained) and Assigned(GetDoor()) and not IsOpen()) then
   begin
      // can't put something inside a closed doorway
      Message := TMessage.Create(mkClosed, GetDescriptionClosed(Perspective));
      Result := False;
   end
   else
   if ((CouldBeDoor) and (not (FParent is TThresholdLocation))) then
   begin
      // this is a door in another thing or something
      if (Assigned(OldDoor)) then
      begin
         Message := TMessage.Create(mkDuplicate, '_ already _ _.', [Capitalise(GetDefiniteName(Perspective)),
                                                                    TernaryConditional('has', 'have', IsPlural(Perspective)),
                                                                    OldDoor.GetIndefiniteName(Perspective)]);
         Result := False;
      end
      else
         Result := IsUnencumbered();
   end
   else
   if (((not CouldBeDoor) or (Care <> psCarefully) or (Assigned(GetDoor()))) and (FParent is TThresholdLocation)) then
   begin
      // not a door being carefully installed in a doorway
      Result := FParent.GetSurface().CanPut(Thing, tpOn, Care, Perspective, Message);
   end
   else
   begin
      Result := IsUnencumbered();
   end;
end;

procedure TDoorWay.Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar);
var
   Ground: TAtom;
begin
   if (((not GetCouldBeDoor(Thing, ThingPosition)) or (Care <> psCarefully) or (Assigned(GetDoor()))) and (FParent is TThresholdLocation)) then
   begin
      Ground := FParent.GetSurface();
      DoBroadcast([Self, Ground], Perspective,
                  [C(M(@Perspective.GetDefiniteName)), SP, // You
                   MP(Perspective, M('drops'), M('drop')), SP, // drop
                   M(@Thing.GetDefiniteName), SP, // the door
                   M(ThingPositionToString(ThingPosition)), SP, // in
                   M(@GetDefiniteName), // the door way
                   M(', and '),
                   M(@Thing.GetSubjectPronoun), SP, // it
                   MP(Thing, M('falls'), M('fall')), SP, // falls
                   M(ThingPositionToDirectionString(tpOnGround)), SP, // to
                   M(@Ground.GetDefiniteName), // the ground
                   M('.')]);
      Perspective.AvatarMessage(TMessage.Create(mkThingsFall, '_ _ _ _.',
                                               [Capitalise(Thing.GetDefiniteName(Perspective)),
                                                TernaryConditional('falls', 'fall', Thing.IsPlural(Perspective)),
                                                ThingPositionToDirectionString(tpOnGround),
                                                Ground.GetDefiniteName(Perspective)]));
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
      DoBroadcastAll([Self, Blame], [C(M(@Blame.GetDefiniteName)), SP, // You
                                     MP(Blame, M('installs'), M('install')), SP, // install
                                     M(@Thing.GetIndefiniteName), SP, // a thing
                                     M(ThingPositionToString(Thing.Position)), SP, // in
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

function TDoorWay.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Result := Self;
end;

function TDoorWay.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function TDoorWay.GetLookIn(Perspective: TAvatar): UTF8String;
begin
   if (CanSeeThrough()) then
   begin
      case (LocatePerspective(Perspective)) of
         rppFront: Result := GetLookTowardsDirection(Perspective, cdReverse[FFrontSideFacesDirection]);
         rppBack: Result := GetLookTowardsDirection(Perspective, FFrontSideFacesDirection);
         else
            Result := inherited;
      end;
   end
   else
      Result := inherited;
end;

function TDoorWay.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
var
   TheDoor: TDoor;
   Obstacles: TThingList;
begin
   TheDoor := GetDoor();
   if (Assigned(TheDoor)) then
      Result := TheDoor.GetDescriptionRemoteDetailed(Perspective, Direction)
   else
      Result := inherited;
   Obstacles := GetObtrusiveObstacles();
   try
      if (Obstacles.Length > 0) then
         Result := Result + ' Blocking ' + GetDefiniteName(Perspective) + ' ' + IsAre(Obstacles.IsPlural(Perspective)) + ' ' + Obstacles.GetIndefiniteString(Perspective, 'and') + '.';
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
   inherited Create(Name, Pattern, AMass, ASize);
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
   if (ThingPosition = tpOn) then
   begin
      TheDoorWay := GetDoorWay();
      if (Assigned(TheDoorWay)) then
      begin
         if (not TheDoorWay.IsOpen()) then
         begin
            Message := TMessage.Create(mkCannotPutOnBecauseInstalled, TheDoorWay.GetDescriptionClosed(Perspective));
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
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('has', 'have', IsPlural(Perspective)) + ' two sides. On the front, ' + FFrontSide.GetDescriptionSelfSentenceFragment(Perspective) + ' On the back, ' + FBackSide.GetDescriptionSelfSentenceFragment(Perspective)
   end;
   Assert(Result <> '');
   TheDoorWay := GetDoorWay();
   if (Assigned(TheDoorWay)) then
   begin
      Result := Result + ' ' + Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + TernaryConditional('closed', 'open', TheDoorWay.IsOpen()) + '.';
   end;
end;

function TDoor.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   if (Assigned(GetDoorWay())) then
   begin
      if (IsOpen()) then
         Result := Result + [tfClosable]
      else
         Result := Result + [tfOpenable];
   end;
end;

function TDoor.Open(Perspective: TAvatar; var Message: TMessage): Boolean;
var
   TheDoorWay: TDoorWay;
begin
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
      Message := TMessage.Create(mkClosed, FMaster.GetDescriptionClosed(Perspective));
      Result := nil;
   end;
end;

initialization
{$INCLUDE registrations/threshold.inc}
end.
