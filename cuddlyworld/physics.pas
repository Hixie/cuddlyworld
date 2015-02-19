{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit physics;

interface

uses
   storable, lists, grammarian, thingdim, messages;

type
   TAtom = class;
   TInternalAtomList = specialize TStorableList<TAtom>;
   TThing = class;
   TInternalThingList = specialize TStorableList<TThing>;
   TThingClass = class of TThing;
   TLocation = class;
   TLocationList = specialize TStorableList<TLocation>; // @RegisterStorableClass
   TAvatar = class;

type
   PAtomList = ^TAtomList;
   TAtomList = class(TInternalAtomList) // @RegisterStorableClass
      function GetIndefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
      function GetDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
      function GetLongDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
   end;

   TThingList = class(TInternalThingList) // @RegisterStorableClass
      function GetIndefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
      function GetDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
      function GetLongDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
   end;

function GetIndefiniteString(const List: array of TAtom; StartIndex, EndIndex: Cardinal; Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
function GetDefiniteString(const List: array of TAtom; StartIndex, EndIndex: Cardinal; Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
function GetLongDefiniteString(const List: array of TAtom; StartIndex, EndIndex: Cardinal; Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;

type
   TReachablePosition = (rpReachable, rpNotReachable);
   TReachablePositionSet = set of TReachablePosition;
   TSubjectiveInformation = record
      Directions: TCardinalDirectionSet;
      Reachable: TReachablePositionSet;
      procedure Reset();
   end;

type
   TThingFeature = (tfDiggable, tfCanDig, tfExaminingReads,
                    tfCanHaveThingsPushedOn, { e.g. it has a ramp, or a surface flush with its container -- e.g. holes can have things pushed onto them }
                    tfCanHaveThingsPushedIn); { e.g. it has its entrance flush with its base, or has a lip flush with its container -- holes, bags; but not boxes }
   TThingFeatures = set of TThingFeature;
   TFindMatchingThingsOption = (foIncludePerspectiveChildren, // e.g. not used by "take all"
                                foIncludeNonImplicits, // e.g. usd by "debug things" to make the avatars be included in the list; not used by "take all" so that avatars aren't picked up
                                foFromOutside);
   TFindMatchingThingsOptions = set of TFindMatchingThingsOption;

const
   tfEverything = []; { an empty TThingFeatures set, because thing features _limit_ what can be returned }

type
   TGetDescriptionOnOptions = set of (optDeepOn, optPrecise);
   TGetDescriptionChildrenOptions = set of (optDeepChildren, optFar, optThorough, optOmitPerspective); { deep = bag and inside bag; far = door and inside door }
   TGetPresenceStatementMode = (psThereIsAThingHere { look },
                                psThereIsAThingThere { look north },
                                psOnThatThingIsAThing { nested look },
                                psTheThingIsOnThatThing { find },
                                psOnThatSpecialThing { find (something far away) -- only if parent is TThing, not TLocation });

type
   TThingReporter = procedure (Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber) of object;

   TAtom = class(TStorable)
    {$IFDEF DEBUG}
    private
      FIntegritySelf: TAtom; // set to Self by constructors, set to nil by destructor
    {$ENDIF}
    protected
      FChildren: TThingList;
      procedure Removed(Thing: TThing); virtual;
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
      {$IFOPT C+} procedure AssertChildPositionOk(Thing: TThing; Position: TThingPosition); virtual; {$ENDIF}
      function FindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
      function ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
      procedure ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); virtual;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure Add(Thing: TThing; Position: TThingPosition);
      procedure Add(Thing: TThingList.TEnumerator; Position: TThingPosition);
      procedure Remove(Thing: TThing);
      procedure Remove(Thing: TThingList.TEnumerator);
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: TMessage): Boolean; virtual;
      procedure Put(Thing: TThing; Position: TThingPosition; Carefully: Boolean; Perspective: TAvatar); virtual;
      function GetMassManifest(): TThingMassManifest; virtual; { self and children that are not tpScenery }
      function GetOutsideSizeManifest(): TThingSizeManifest; virtual; { external size of the object (e.g. to decide if it fits inside another): self and children that are tpOutside; add tpContained children if container is flexible }
      function GetInsideSizeManifest(): TThingSizeManifest; virtual; { only children that are tpContained }
      function GetSurfaceSizeManifest(): TThingSizeManifest; virtual; { children that are tpSurface (e.g. to decide if something else can be added to the object's surface or if the surface is full already) }
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); virtual;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; virtual;
      function IsPlural(Perspective: TAvatar): Boolean; virtual; abstract;
      function GetName(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetSummaryName(Perspective: TAvatar): AnsiString; virtual;
      function GetLongName(Perspective: TAvatar): AnsiString; virtual; { if you reply to other terms, put as many as possible here; this is shown to disambiguate }
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; virtual;
      function GetLongDefiniteName(Perspective: TAvatar): AnsiString; virtual;
      function GetSubjectPronoun(Perspective: TAvatar): AnsiString; virtual; // I
      function GetObjectPronoun(Perspective: TAvatar): AnsiString; virtual; // me
      function GetReflexivePronoun(Perspective: TAvatar): AnsiString; virtual; // myself
      function GetPossessivePronoun(Perspective: TAvatar): AnsiString; virtual; // mine
      function GetPossessiveAdjective(Perspective: TAvatar): AnsiString; virtual; // my
      function GetTitle(Perspective: TAvatar): AnsiString; virtual;
      function GetContext(Perspective: TAvatar): AnsiString; virtual;
      function GetLook(Perspective: TAvatar): AnsiString; virtual;
      function GetLookAt(Perspective: TAvatar): AnsiString; virtual;
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract;
      function GetBasicDescription(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): AnsiString; virtual;
      function GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString; virtual;
      function GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString; virtual;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetDescriptionState(Perspective: TAvatar): AnsiString; virtual; { e.g. 'The bottle is open.' }
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): AnsiString; virtual; abstract;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions): AnsiString;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString; virtual;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString; virtual; abstract; // used by locations to include their loAutoDescribe landmarks in their Here description
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract; // used for things like "look north"
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); virtual; abstract; { called by avatar children to trigger DoNavigation correctly }
      procedure FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); virtual;
      function FindThing(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean; out SubjectiveInformation: TSubjectiveInformation): Boolean; virtual;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter); virtual;
      function GetDefaultAtom(): TAtom; virtual; { the TAtom that is responsible for high-level dealings for this one (opposite of GetSurface) }
      function GetInside(var PositionOverride: TThingPosition): TAtom; virtual; { returns nil if there's no inside to speak of }
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; virtual;
      function GetSurface(): TAtom; virtual; { the TAtom that is responsible for the minutiae of where things dropped on this one actually go (opposite of GetDefaultAtom) }
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; virtual; abstract;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; virtual; abstract;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); virtual; { use this to fumble things or to cause things to fall off other things (and make CanPut() always allow tpOn in that case) }
      procedure HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar); virtual; { use this for magic doors, falling down tunnels, etc }
      {$IFDEF DEBUG} function Debug(): AnsiString; virtual; {$ENDIF}
   end;

   PThing = ^TThing;
   TThing = class(TAtom)
    protected
      FParent: TAtom;
      FPosition: TThingPosition;
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
      function IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean; virtual;
      function IsExplicitlyReferenceable(Perspective: TAvatar): Boolean; virtual;
      function FindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass); override;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; virtual;
      function CanTake(Perspective: TAvatar; var Message: TMessage): Boolean; virtual;
      function CanShake(Perspective: TAvatar; var Message: TMessage): Boolean; virtual;
      function GetIntrinsicMass(): TThingMass; virtual; abstract;
      function GetIntrinsicSize(): TThingSize; virtual; abstract;
      function GetMassManifest(): TThingMassManifest; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetSummaryName(Perspective: TAvatar): AnsiString; override;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetLongDefiniteName(Perspective: TAvatar): AnsiString; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetTitle(Perspective: TAvatar): AnsiString; override;
      function GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString; override;
      function GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString; override;
      function GetExamine(Perspective: TAvatar): AnsiString; virtual;
      function GetLookUnder(Perspective: TAvatar): AnsiString; virtual;
      function GetLookIn(Perspective: TAvatar): AnsiString; virtual;
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetInventory(Perspective: TAvatar): AnsiString; virtual;
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): AnsiString; override;
      function GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString; virtual;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString; virtual;
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString; virtual;
      function GetDescriptionEmpty(Perspective: TAvatar): AnsiString; virtual; { only called for optThorough searches }
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; virtual; { used both from inside and outside }
      function GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: AnsiString = ''): AnsiString; virtual;
      function GetDescriptionCarriedTitle(Perspective: TAvatar; DeepCarried: Boolean): AnsiString; virtual;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; virtual;
      function GetDescriptionWriting(Perspective: TAvatar): AnsiString; virtual;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      procedure FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; virtual; abstract; // compares Tokens to FName, essentially
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter); override;
      procedure Moved(OldParent: TAtom; Carefully: Boolean; Perspective: TAvatar); virtual;
      procedure Shake(Perspective: TAvatar); virtual;
      procedure Press(Perspective: TAvatar); virtual;
      function GetFeatures(): TThingFeatures; virtual;
      function CanDig(Target: TThing; Perspective: TAvatar; var Message: TMessage): Boolean; virtual;
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: TMessage): Boolean; virtual;
      procedure Dug(Target: TThing; Perspective: TAvatar; var Message: TMessage); virtual;
      function IsOpen(): Boolean; virtual;
      {$IFDEF DEBUG} function Debug(): AnsiString; override; {$ENDIF}
      property Parent: TAtom read FParent;
      property Position: TThingPosition read FPosition write FPosition;
   end;
 
   { Thing that can move of its own volition }
   TAvatar = class(TThing)
    protected
      function IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean; override;
    public
      procedure DoLook(); virtual; abstract;
      procedure AvatarMessage(Message: TMessage); virtual; abstract;
      procedure AnnounceAppearance(); virtual; abstract;
      procedure AnnounceDisappearance(); virtual; abstract;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); virtual; abstract;
      procedure AnnounceDeparture(Destination: TAtom); virtual; abstract;
      procedure AnnounceArrival(Source: TAtom); virtual; abstract;
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); virtual; abstract;
      procedure AutoDisambiguated(Message: AnsiString); virtual; abstract;
      function HasConnectedPlayer(): Boolean; virtual; abstract;
      function IsReadyForRemoval(): Boolean; virtual; abstract;
      procedure RemoveFromWorld(); virtual;
   end;

   {$IFDEF DEBUG} // used by AssertDirectionHasDestination()
   TDummyAvatar = class(TAvatar) // @RegisterStorableClass
    public
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; override;
      procedure DoLook(); override;
      procedure AvatarMessage(Message: TMessage); override;
      procedure AnnounceAppearance(); override;
      procedure AnnounceDisappearance(); override;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceDeparture(Destination: TAtom); override;
      procedure AnnounceArrival(Source: TAtom); override;
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); override;
      procedure AutoDisambiguated(Message: AnsiString); override;
      function HasConnectedPlayer(): Boolean; override;
      function IsReadyForRemoval(): Boolean; override;
   end;
   {$ENDIF}

   TLocation = class(TAtom)
    public
     type
      TLandmarkOptions = set of (loAutoDescribe, // include landmark in descriptions of the room
                                 loPermissibleNavigationTarget, // only the first landmark in each direction is allowed to be loPermissibleNavigationTarget
                                 loThreshold, // whether FindThing and FindMatchingThings should traverse (ExplicitlyReferenced logic doesn't use this since it looks everywhere)
                                 loVisibleFromFarAway, // for e.g. mountains
                                 loNotVisibleFromBehind, // for e.g. surfaces so that they're not visible from below
                                 loConsiderDirectionUnimportantWhenFindingChildren); // so that "find hole" doesn't say "below"
    protected
     type
      PDirectionalLandmark = ^TDirectionalLandmark;
      TDirectionalLandmark = record
        Direction: TCardinalDirection; // in case it was accessed via FImportantLandmarks
        Options: TLandmarkOptions;
        Atom: TAtom;
      end;
     const
      loImportantLandmarks = [loAutoDescribe, loThreshold]; // don't include the same atom in more than one direction if you use these
     var
      FImportantLandmarks: array of PDirectionalLandmark; // pointers into FDirectionalLandmarks with loImportantLandmarks
      FDirectionalLandmarks: array[TCardinalDirection] of array of TDirectionalLandmark;
      function CanFindInDirection(Direction: TCardinalDirection; Atom: TAtom): Boolean;
      function FindThingDirectionalTraverser(Thing: TThing; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; var SubjectiveInformation: TSubjectiveInformation): Boolean; virtual;
      {$IFDEF DEBUG} procedure AssertDirectionHasDestination(Direction: TCardinalDirection; Atom: TAtom); {$ENDIF}
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function HasLandmark(Direction: TCardinalDirection): Boolean;
      procedure AddLandmark(Direction: TCardinalDirection; Atom: TAtom; Options: TLandmarkOptions);
      procedure AddSurroundings(Atom: TAtom; const Directions: TCardinalDirectionSet = cdCompasDirection);
      function GetAtomForDirectionalNavigation(Direction: TCardinalDirection): TAtom;
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): AnsiString; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString; override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      procedure FailNavigation(Direction: TCardinalDirection; Perspective: TAvatar); { also called when trying to dig in and push something in this direction }
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter); override;
      procedure AddExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter); virtual;
      procedure FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function FindThing(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean; out SubjectiveInformation: TSubjectiveInformation): Boolean; override;
   end;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);

{ Navigation works as follows:
    Enter and ClimbOn actions invoke the Position-based DoNavigation() above directly.
    Go actions invoke the player's parent's Navigate() method.
      Navigate() then typicially defers up until you reach the location.
      The TLocation.Navigate() method then looks in the appropriate direction.
      If that returns something, it calls the Direction-based DoNavigation().
    Then, DoNavigate() calls GetSurface() (for ClimbOn) or GetEntrance() (for Enter and Go).
      TThing.GetEntrance() looks for the child that is a tpOpening and defers to it, else defers to GetInside()
      TLocation.GetEntrance() calls GetSurface().
      TThresholdLocation.GetEntrance() fast-forwards you to the other side.
}

procedure QueueForDisposal(Atom: TAtom);

procedure ConnectLocations(SourceLocation: TLocation; Direction: TCardinalDirection; Destination: TLocation); // does not set loAutoDescribe

procedure EmptyDisposalQueue();

implementation

uses
   sysutils, broadcast;

procedure TSubjectiveInformation.Reset();
begin
   Directions := [];
   Reachable := [];
end;


procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
var
   Destination: TAtom;
   Message: TMessage;
   Position: TThingPosition;
   NotificationList: TAtomList;
   NotificationTarget: TAtom;
   DisambiguationOpening: TThing;
begin
   Assert(Assigned(AFrom));
   Assert(Assigned(ATo));
   Assert(Assigned(Perspective));
   Position := tpOn;
   DisambiguationOpening := nil;
   Message := TMessage.Create(mkSuccess, '');
   NotificationList := TAtomList.Create();
   try
      Destination := ATo.GetEntrance(Perspective, Direction, Perspective, Position, DisambiguationOpening, Message, NotificationList);
      if (Assigned(Destination)) then
      begin
         if (Assigned(DisambiguationOpening) and (DisambiguationOpening <> Destination)) then
            Perspective.AutoDisambiguated('through ' + DisambiguationOpening.GetDefiniteName(Perspective));
         Perspective.AnnounceDeparture(ATo, Direction);
         for NotificationTarget in NotificationList do
            NotificationTarget.HandlePassedThrough(Perspective, AFrom, Destination, Position, Perspective);
         Destination.Add(Perspective, Position);
         Perspective.AnnounceArrival(AFrom.GetDefaultAtom(), ReverseCardinalDirection(Direction));
         Perspective.DoLook();
      end
      else
      begin
         Message.PrefaceFailureTopic('_ cannot go _.',
                                    [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                     CardinalDirectionToString(Direction)]);
         Perspective.AvatarMessage(Message);
      end;
   finally
      NotificationList.Free();
   end;
end;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);
var
   Destination: TAtom;
   Message: TMessage;
   Success: Boolean;
   Ancestor: TAtom;
   NotificationList: TAtomList;
   NotificationTarget: TAtom;
   DisambiguationOpening: TThing;
   Direction: TCardinalDirection;
begin
   Assert(Assigned(AFrom));
   Assert(Assigned(ATo));
   Assert(Assigned(Perspective));
   Assert(Perspective.Parent = AFrom);
   Ancestor := ATo;
   while ((Ancestor is TThing) and (Ancestor <> Perspective)) do
      Ancestor := (Ancestor as TThing).Parent;
   if (Ancestor = Perspective) then
   begin
      Perspective.AvatarMessage(TMessage.Create(mkCannotMoveBecauseLocation, 'That would prove rather challenging given where _ _ relative to _.',
                                                [ATo.GetDefiniteName(Perspective),
                                                 IsAre(ATo.IsPlural(Perspective)),
                                                 Perspective.GetReflexivePronoun(Perspective)]));
   end
   else
   if (Position = tpOn) then
   begin
      ATo := ATo.GetSurface();
      Assert(Assigned(ATo));
      Assert(ATo is TThing);
      DisambiguationOpening := nil;
      Message := TMessage.Create(mkSuccess, '');
      Success := (ATo as TThing).CanPut(Perspective, Position, Perspective, Message);
      if (Success) then
      begin
         ATo.Add(Perspective, Position);
         // XXX announcements
         Perspective.DoLook();
      end
      else
      begin
         Message.PrefaceFailureTopic('_ cannot get onto _.', 
                                     [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                      ATo.GetDefiniteName(Perspective)]);
         Perspective.AvatarMessage(Message);
      end;
   end
   else
   if (Position = tpIn) then
   begin
      Assert(ATo is TThing);
      DisambiguationOpening := nil;
      Message := TMessage.Create(mkSuccess, '');
      NotificationList := TAtomList.Create();
      try
         Ancestor := AFrom;
         while ((Ancestor is TThing) and (Ancestor <> ATo)) do
            Ancestor := (Ancestor as TThing).Parent;
         if (Ancestor = ATo) then
            Direction := cdOut
         else
            Direction := cdIn;
         Destination := ATo.GetEntrance(Perspective, Direction, Perspective, Position, DisambiguationOpening, Message, NotificationList);
         if (Assigned(Destination)) then
         begin
            if (Assigned(DisambiguationOpening) and (DisambiguationOpening <> Destination)) then
               Perspective.AutoDisambiguated('through ' + DisambiguationOpening.GetDefiniteName(Perspective));
            Perspective.AnnounceDeparture(ATo);
            for NotificationTarget in NotificationList do
               NotificationTarget.HandlePassedThrough(Perspective, AFrom, Destination, Position, Perspective);
            Destination.Add(Perspective, Position);
            Perspective.AnnounceArrival(AFrom.GetDefaultAtom());
            Perspective.DoLook();
         end
         else
         begin
            Message.PrefaceFailureTopic('_ cannot enter _.',
                                        [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                         ATo.GetDefiniteName(Perspective)]);
            Perspective.AvatarMessage(Message);
         end;
      finally
         NotificationList.Free();
      end;
   end
   else
      Assert(False, 'unexpected position for navigation: ' + IntToStr(Cardinal(Position)));
end;

{$INCLUDE atomlisthelper.inc} // generated by regen.pl

var
   DisposalQueue: TAtomList;

procedure InitDisposalQueue();
begin
   if (not Assigned(DisposalQueue)) then
      DisposalQueue := TAtomList.Create([slOwner]); 
end;

procedure QueueForDisposal(Atom: TAtom);
begin
   Assert(Assigned(DisposalQueue));
   DisposalQueue.AppendItem(Atom);
end;

procedure EmptyDisposalQueue();
begin
   Assert(Assigned(DisposalQueue));
   DisposalQueue.FreeItems();
end;


constructor TAtom.Create();
begin
   {$IFDEF DEBUG}
    FIntegritySelf := Self;
   {$ENDIF}
   inherited;
   FChildren := TThingList.Create([slOwner]);
end;

destructor TAtom.Destroy();
begin
   {$IFDEF DEBUG}
    Assert(Assigned(FIntegritySelf), 'Tried to free an already-freed TAtom');
    Assert(FIntegritySelf = Self);
    FIntegritySelf := nil;
   {$ENDIF}
   FChildren.Free();
   inherited;
end;

constructor TAtom.Read(Stream: TReadStream);
begin
   {$IFDEF DEBUG}
    FIntegritySelf := Self;
   {$ENDIF}
   inherited;
   FChildren := Stream.ReadObject() as TThingList;
end;

procedure TAtom.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteObject(FChildren);
end;

{$IFOPT C+}
procedure TAtom.AssertChildPositionOk(Thing: TThing; Position: TThingPosition);
var
   Child: TThing;
begin
   if (Position in tpOpening) then
      for Child in FChildren do
         Assert(not (Child.FPosition in tpOpening), 'Can''t have two things that are the tpOpening of another thing. See note in grammarian.pas.');
end;
{$ENDIF}

procedure TAtom.Add(Thing: TThing; Position: TThingPosition);
{$IFOPT C+}
var
   TempPosition: TThingPosition;
{$ENDIF}
begin
   {$IFOPT C+}
     TempPosition := tpIn;
     Assert((Position <> tpIn) or (GetInside(TempPosition) = Self), 'tried to put something inside something without an inside');
     AssertChildPositionOk(Thing, Position);
   {$ENDIF}
   if (Assigned(Thing.FParent)) then
      Thing.FParent.Remove(Thing);
   Assert(not Assigned(Thing.FParent));
   FChildren.AppendItem(Thing);
   Thing.FParent := Self;
   Thing.FPosition := Position;
end;

procedure TAtom.Add(Thing: TThingList.TEnumerator; Position: TThingPosition);
var
   OldParent: TAtom;
   ActualThing: TThing;
   {$IFOPT C+} TempPosition: TThingPosition; {$ENDIF}
begin
   Assert(Thing.FList <> FChildren);
   ActualThing := Thing.Current;
   {$IFOPT C+}
     TempPosition := tpIn;
     Assert((Position <> tpIn) or (GetInside(TempPosition) = Self), 'tried to put something inside something without an inside');
     AssertChildPositionOk(ActualThing, Position);
   {$ENDIF}
   Assert(Assigned(ActualThing));
   OldParent := ActualThing.FParent;
   FChildren.AdoptItem(Thing);
   ActualThing.FParent := Self;
   ActualThing.FPosition := Position;
   if (Assigned(OldParent)) then
      OldParent.Removed(ActualThing);
end;

procedure TAtom.Remove(Thing: TThing);
begin
   FChildren.RemoveItem(Thing);
   Thing.FParent := nil;
   Removed(Thing);
end;

procedure TAtom.Remove(Thing: TThingList.TEnumerator);
var
   OldThing: TThing;
begin
   Assert(Thing.FList = FChildren);
   OldThing := Thing.Current;
   Thing.Remove();
   OldThing.FParent := nil;
   Removed(OldThing);
end;

procedure TAtom.Removed(Thing: TThing);
begin
end;

function TAtom.CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   if (ThingPosition = tpOn) then
   begin
      Result := CanSurfaceHold(Thing.GetIntrinsicSize());
      if (not Result) then
         Message := TMessage.Create(mkTooBig, 'There is not enough room on _ for _.',
                                              [GetDefiniteName(Perspective),
                                               Thing.GetDefiniteName(Perspective)]);
   end
   else
   if (ThingPosition = tpIn) then
   begin
      Result := CanInsideHold(Thing.GetOutsideSizeManifest());
      if (not Result) then
      begin
         if ((not Assigned(GetInside(ThingPosition))) and (Self is TThing)) then
            Message := TMessage.Create(mkNoOpening, '_ _ not appear to have an opening.',
                                                    [Capitalise(GetDefiniteName(Perspective)),
                                                     TernaryConditional('does', 'do', IsPlural(Perspective))])
         else
            Message := TMessage.Create(mkTooBig, 'There is not enough room in _ for _.',
                                                 [GetDefiniteName(Perspective),
                                                  Thing.GetDefiniteName(Perspective)]);;
      end;
   end
   else
      Assert(False, 'Unexpected position ' + IntToStr(Cardinal(ThingPosition)));
end;

procedure TAtom.Put(Thing: TThing; Position: TThingPosition; Carefully: Boolean; Perspective: TAvatar);
var
   OldParent: TAtom;
   {$IFOPT C+} ParentSearch: TAtom; {$ENDIF}
begin
   OldParent := Thing.FParent;
   Add(Thing, Position);
   {$IFOPT C+}
   ParentSearch := Thing;
   Assert(ParentSearch is TThing);
   repeat
      ParentSearch := (ParentSearch as TThing).Parent;
      Assert(ParentSearch <> Thing);
   until (not (ParentSearch is TThing));
   {$ENDIF}
   Thing.Moved(OldParent, Carefully, Perspective);
   HandleAdd(Thing, Perspective);
end;

function TAtom.GetMassManifest(): TThingMassManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      if (not (Child.Position in tpScenery)) then
         Result := Result + Child.GetMassManifest();
end;

function TAtom.GetOutsideSizeManifest(): TThingSizeManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      if (Child.Position in tpOutside) then
         Result := Result + Child.GetOutsideSizeManifest();
end;

function TAtom.GetInsideSizeManifest(): TThingSizeManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      if (Child.Position in tpContained) then
         Result := Result + Child.GetOutsideSizeManifest();
end;

function TAtom.GetSurfaceSizeManifest(): TThingSizeManifest;
var
   Child: TThing;
begin
   Zero(Result);
   for Child in FChildren do
      if (Child.Position in tpSurface) then
         Result := Result + Child.GetIntrinsicSize();
end;

procedure TAtom.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
begin
end;

procedure TAtom.HandleAdd(Thing: TThing; Blame: TAvatar);
begin
end;

{ this is used when notifying players of something, e.g. shouting }
{ it should propagate to all of the things that should get the message }
procedure TAtom.GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (FromOutside <> (Child.Position in tpContained)) then { assumes that we are closed (TThing.GetNearbyThingsByClass solves that) }
         Child.GetNearbyThingsByClass(List, True, Filter);
end;

function TAtom.GetSurroundingsRoot(out FromOutside: Boolean): TAtom;
begin
   Result := Self;
   FromOutside := True;
end;

function TAtom.GetSummaryName(Perspective: TAvatar): AnsiString;
begin
   Result := GetName(Perspective);
end;

function TAtom.GetLongName(Perspective: TAvatar): AnsiString;
begin
   Result := GetName(Perspective);
end;

function TAtom.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := 'the ' + GetName(Perspective);
end;

function TAtom.GetLongDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := 'the ' + GetLongName(Perspective);
end;

function TAtom.GetSubjectPronoun(Perspective: TAvatar): AnsiString;
begin
   Result := 'it';
end;

function TAtom.GetObjectPronoun(Perspective: TAvatar): AnsiString;
begin
   Result := 'it';
end;

function TAtom.GetReflexivePronoun(Perspective: TAvatar): AnsiString;
begin
   Result := 'itself';
end;

function TAtom.GetPossessivePronoun(Perspective: TAvatar): AnsiString;
begin
   Result := 'its';
end;

function TAtom.GetPossessiveAdjective(Perspective: TAvatar): AnsiString;
begin
   Result := 'its';
end;

function TAtom.GetTitle(Perspective: TAvatar): AnsiString;
var
   Additional: AnsiString;
begin
   Result := GetName(Perspective);
   Additional := GetContext(Perspective);
   if (Length(Additional) > 0) then
      Result := Result + ' (' + Additional + ')';
end;

function TAtom.GetContext(Perspective: TAvatar): AnsiString;
var
   Thing: TThing;
   Atom: TAtom;
   Position: TThingPosition;
begin
   Result := '';
   Atom := Self;
   while (Atom is TThing) do
   begin
      Thing := Atom as TThing;
      Atom := Thing.Parent;
      Position := Thing.Position;
      if (Atom.GetDefaultAtom() <> Atom) then
      begin
         Atom := Atom.GetDefaultAtom();
         Position := tpAt;
      end;
      if (Length(Result) > 0) then
         Result := Result + ', ';
      Result := Result + ThingPositionToString(Position) + ' ' + Atom.GetDefiniteName(Perspective);
   end;
end;

function TAtom.GetLook(Perspective: TAvatar): AnsiString;
begin
   Result := Capitalise(GetTitle(Perspective)) + #10 +
             GetBasicDescription(Perspective, psThereIsAThingHere) +
             WithNewlineIfNotEmpty(GetHorizonDescription(Perspective, Self)) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optOmitPerspective]));
end;

function TAtom.GetLookAt(Perspective: TAvatar): AnsiString;
begin
   Result := GetBasicDescription(Perspective, psThereIsAThingHere) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optDeepChildren]));
end;

function TAtom.GetBasicDescription(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): AnsiString;
begin
   Result := GetDescriptionSelf(Perspective) +
             WithSpaceIfNotEmpty(GetDescriptionState(Perspective)) +
             WithSpaceIfNotEmpty(GetDescriptionHere(Perspective, Mode, Context));
end;

function TAtom.GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString;
begin
   Result := '';
end;

function TAtom.GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString;
begin
   Result := GetBasicDescription(Perspective, psThereIsAThingHere, Context);
end;

function TAtom.GetDescriptionState(Perspective: TAvatar): AnsiString; { e.g. 'The bottle is open.' }
begin
   Result := '';
end;

function TAtom.GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions): AnsiString;
begin
   Result := GetDescriptionOn(Perspective, Options, '');
end;

function TAtom.GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions; Prefix: AnsiString): AnsiString;

   procedure ProcessBatch(Children: TThingList);
   var
      Mode: TGetPresenceStatementMode;
      Child: TThing;
   begin
      for Child in Children do
      begin
         if ((Child.Position = tpOn) and ((optPrecise in Options) or (Child <> Perspective))) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            if (optPrecise in Options) then
               Mode := psOnThatThingIsAThing
            else
               Mode := psThereIsAThingHere;
            Result := Result + Prefix + Child.GetPresenceStatement(Perspective, Mode) + WithSpaceIfNotEmpty(Child.GetDescriptionState(Perspective));
            if (optDeepOn in Options) then
            begin
               if (Length(Prefix) = 0) then
                  Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionOn(Perspective, Options + [optPrecise], Prefix))
               else
                  Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionOn(Perspective, Options + [optPrecise], Prefix + '  '));
            end
         end;
      end;
   end;

begin
   Result := '';
   ProcessBatch(FChildren);
   if (GetSurface() <> Self) then
      ProcessBatch(GetSurface().FChildren);
end;

function TAtom.GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString;
begin
   Result := '';
end;

function TAtom.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := True;
end;

procedure TAtom.FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (((foIncludePerspectiveChildren in Options) or (Child <> Perspective)) and (IsChildTraversable(Child, Perspective, foFromOutside in Options))) then
         Child.FindMatchingThings(Perspective, Options + [foFromOutside], PositionFilter, PropertyFilter, List);
end;

procedure TAtom.ProxiedFindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   FindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
end;

function TAtom.FindThing(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean; out SubjectiveInformation: TSubjectiveInformation): Boolean;
begin
   SubjectiveInformation.Reset(); {BOGUS Warning: Variable "SubjectiveInformation" does not seem to be initialized}
   Result := FindThingTraverser(Thing, Perspective, FromOutside);
   if (Result) then
      Include(SubjectiveInformation.Reachable, rpReachable);
end;

function TAtom.FindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
var
   Child: TThing;
begin
   for Child in FChildren do
      if (IsChildTraversable(Child, Perspective, FromOutside)) then
         if (Child.FindThingTraverser(Thing, Perspective, True)) then
         begin
            Result := True;
            Exit;
         end;
   Result := False;
end;

function TAtom.ProxiedFindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := FindThingTraverser(Thing, Perspective, FromOutside);
end;

procedure TAtom.AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (IsChildTraversable(Child, Perspective, FromOutside)) then
         Child.AddExplicitlyReferencedThings(Tokens, Start, Perspective, True, Reporter);
end;

function TAtom.GetDefaultAtom(): TAtom;
begin
   Result := Self;
end;

function TAtom.GetInside(var PositionOverride: TThingPosition): TAtom;
var
   Child: TThing;
begin
   Assert(PositionOverride = tpIn);
   Result := nil;
   for Child in FChildren do
   begin
      if (Child.Position in tpOpening) then
      begin
         Assert(not Assigned(Result));
         Result := Child.GetInside(PositionOverride);
         {$IFOPT C-} Exit; {$ENDIF} // no need to go through the list if not checking for assertions anyway
      end;
   end;
end;

function TAtom.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
var
   Inside: TAtom;
   Position: TThingPosition;
begin
   Position := tpIn;
   Inside := GetInside(Position);
   if (Assigned(Inside)) then
   begin
      Assert(Inside <> Self, 'If you make GetInside() return the element proper, then you must override CanInsideHold() also.');
      if (Position = tpIn) then
         Result := Inside.CanInsideHold(Manifest)
      else
      if (Position = tpOn) then
         Result := Inside.CanSurfaceHold(Manifest)
      else
         raise Exception.Create('Unexpected or unknown overriding inside thing position ' + IntToStr(Cardinal(Position)));
   end
   else
      Result := False;
end;

function TAtom.GetSurface(): TAtom;
begin
   Result := Self;
end;

{$IFDEF DEBUG}
function TAtom.Debug(): AnsiString;
var
   Child: TThing;
   ChildResult: AnsiString;
begin
   Result := GetName(nil) + #10 +
             'Long Name: ' + GetLongDefiniteName(nil) + #10 +
             'Class: ' + ClassName + #10 +
             'Children: ';
   ChildResult := '';
   for Child in FChildren do
   begin     
      if (ChildResult <> '') then
         ChildResult := ChildResult + '; ';
      ChildResult := ChildResult + Child.GetLongName(nil) + ' (' + ThingPositionToString(Child.Position) + ')';
   end;
   if (ChildResult = '') then
      ChildResult := 'none';
   Result := Result + ChildResult;
end;
{$ENDIF}


constructor TThing.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FParent));
   FPosition := TThingPosition(Stream.ReadCardinal());
end;

procedure TThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FParent);
   Stream.WriteCardinal(Cardinal(FPosition));
end;

procedure TThing.GetNearbyThingsByClass(List: TThingList; FromOutside: Boolean; Filter: TThingClass);
var
   Child: TThing;
begin
   if (Self is Filter) then
      List.AppendItem(Self);
   if (FromOutside and IsOpen()) then
   begin
      for Child in FChildren do
         if (Child.Position in tpContained) then
            Child.GetNearbyThingsByClass(List, True, Filter);
   end
   else
      inherited;
end;

function TThing.GetSurroundingsRoot(out FromOutside: Boolean): TAtom;
begin
   Assert(Assigned(FParent));
   if (FPosition in tpContained) then
   begin
      Result := FParent;
      FromOutside := False;
   end
   else
      Result := FParent.GetSurroundingsRoot(FromOutside);
end;

function TThing.GetMassManifest(): TThingMassManifest;
begin
   Result := inherited GetMassManifest() + GetIntrinsicMass();
end;

function TThing.GetOutsideSizeManifest(): TThingSizeManifest;
begin
   Result := inherited GetOutsideSizeManifest() + GetIntrinsicSize();
end;

function TThing.CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean;
var
   Surface: TAtom;
begin
   Surface := GetSurface();
   Assert(Assigned(Surface));
   if (Surface <> Self) then
      Result := Surface.CanSurfaceHold(Manifest)
   else
      Result := (GetSurfaceSizeManifest() + Manifest) <= (GetIntrinsicSize());
end;

function TThing.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
var
   Child: TThing;
   RawMessage: AnsiString;
begin
   // first, look for an opening we can use as the entrance, and defer to it if we have one
   Assert(Assigned(FChildren));
   for Child in FChildren do
   begin
      if (Child.Position in tpOpening) then
      begin
         DisambiguationOpening := Child;
         Result := Child.GetEntrance(Traveller, Direction, Perspective, PositionOverride, DisambiguationOpening, Message, NotificationList);
         Exit;
      end;
   end;
   // no opening, so instead try to directly put the thing inside our insides
   PositionOverride := tpIn;
   Result := GetInside(PositionOverride);
   if (Assigned(Result)) then
   begin
      if (Result = Self) then
      begin
         if (not CanPut(Traveller, PositionOverride, Perspective, Message)) then
            Result := nil;
      end
      else
      begin
         NotificationList.AppendItem(Self);
      end;
   end
   else
   if (Perspective.GetIntrinsicSize() > GetOutsideSizeManifest()) then
   begin
      // no entrance, no inside, but the thing is too small anyway
      // "You are bigger than the apple."
      Message := TMessage.Create(mkTooBig, '_ _ bigger than _.',
                                           [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                            IsAre(Perspective.IsPlural(Perspective)),
                                            GetDefiniteName(Perspective)]);
   end
   else
   begin
      // no entrance, no inside
      // try to give a message along the lines of "the bottle is closed", but failing that, the default below
      RawMessage := GetDescriptionState(Perspective);
      if (RawMessage <> '') then
         Message := TMessage.Create(mkNoOpening, RawMessage)
      else
         Message := TMessage.Create(mkNoOpening, '_ _ no discernible entrance.',
                                                 [Capitalise(GetDefiniteName(Perspective)),
                                                  TernaryConditional('has', 'have', IsPlural(Perspective))]);
   end;
end;

function TThing.GetSummaryName(Perspective: TAvatar): AnsiString;
var
   Context: TAtom;
begin
   Result := inherited;
   Assert(Assigned(FParent));
   Context := FParent.GetDefaultAtom();
   if (Context is TThing) then
      case FPosition of
       tpAmbiguousPartOfImplicit: Result := Result + ' of ' + Context.GetSummaryName(Perspective);
      end;
end;

function TThing.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := GetName(Perspective);
   if (not IsPlural(Perspective)) then
      Result := IndefiniteArticle(Result) + ' ' + Result;
end;

function TThing.GetDefiniteName(Perspective: TAvatar): AnsiString;
var
   Context: TAtom;
begin
   Result := inherited;
   Assert(Assigned(FParent));
   Context := FParent.GetDefaultAtom();
   if (Context is TThing) then
      case FPosition of
       tpAmbiguousPartOfImplicit: Result := Result + ' of ' + Context.GetDefiniteName(Perspective);
      end;
end;

function TThing.GetLongDefiniteName(Perspective: TAvatar): AnsiString;
var
   Context: TAtom;
begin
   Result := inherited;
   Assert(Assigned(FParent));
   Context := FParent.GetDefaultAtom();
   if (Context is TThing) then
      case FPosition of
       tpPartOfImplicit, tpAmbiguousPartOfImplicit: Result := Result + ' of ' + Context.GetDefiniteName(Perspective);
       tpOnImplicit: Result := Result + ' on ' + Context.GetDefiniteName(Perspective);
       tpAtImplicit: Result := Result + ' at ' + Context.GetDefiniteName(Perspective);
       tpAroundImplicit: Result := Result + ' near ' + Context.GetDefiniteName(Perspective);
      end;
end;

function TThing.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := False;
end;

function TThing.GetTitle(Perspective: TAvatar): AnsiString;
begin
   Assert(Assigned(FParent));
   if (Perspective.Parent = Self) then
      Result := Capitalise(ThingPositionToString(Perspective.Position)) + ' ' + GetDefiniteName(Perspective) + ' (' + GetContext(Perspective) + ')'
   else
      Result := inherited;
end;

function TThing.GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString;
begin
   if (((Perspective.Parent = Self) and (Perspective.Position in tpContained)) or (FPosition in tpContained)) then
      Result := inherited
   else
      Result := FParent.GetDescriptionForHorizon(Perspective, Context);
end;

function TThing.GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString;
var
   Part1, Part2: AnsiString;
begin
   Assert(Assigned(FParent));
   if (FParent.GetSurface() <> Self) then
      Part1 := inherited
   else
      Part1 := '';
   Part2 := GetHorizonDescription(Perspective, Context);
   if ((Part1 <> '') and (Part2 <> '')) then
      Result := Part1 + #10 + Part2
   else
      Result := Part1 + Part2;
end;

function TThing.GetExamine(Perspective: TAvatar): AnsiString;
var
   Writing: AnsiString;
begin
   if (tfExaminingReads in GetFeatures()) then
      Writing := GetDescriptionWriting(Perspective)
   else
      Writing := '';
   Result := GetBasicDescription(Perspective, psThereIsAThingHere) +
             WithNewlineIfNotEmpty(Writing) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optDeepChildren, optThorough]));
end;

function TThing.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.';
   // Consider actually looking cdDown from here, somehow
end;

function TThing.GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
{ see also Navigate() }

   function DeferToParent(): AnsiString;
   begin
      Assert(Assigned(FParent));
      if (FPosition in tpDeferNavigationToParent) then
         Result := FParent.GetLookTowardsDirection(Perspective, Direction)
      else
         Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' ' + IsAre(Perspective.IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.';
   end;

begin
   Assert(Direction <> cdIn);
   if (Perspective.Parent = Self) then
   begin
      if (not (Direction in cdPhysicalDirections)) then
      begin
         Assert(Direction = cdOut);
         if ((Perspective.Position in tpContained) and (not IsOpen())) then
            Result := GetDescriptionClosed(Perspective)
         else
            Result := FParent.GetDefaultAtom().GetDescriptionRemoteDetailed(Perspective, Direction);
      end
      else
      if (Perspective.Position in tpDeferNavigationToParent) then
      begin
         Result := DeferToParent();
      end
      else
      begin
         Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' ' + IsAre(Perspective.IsPlural(Perspective)) + ' ' + ThingPositionToString(Perspective.Position) + ' ' + GetDefiniteName(Perspective) + '.';
      end;
   end
   else
      Result := DeferToParent();
end;

function TThing.GetLookIn(Perspective: TAvatar): AnsiString;
var
   PositionOverride: TThingPosition;
   Inside: TAtom;
   Contents: AnsiString;
begin
   if (IsOpen() or ((Perspective.Parent = Self) and (Perspective.Position in tpContained))) then
   begin
      PositionOverride := tpIn;
      Inside := GetInside(PositionOverride);
      if (Assigned(Inside)) then
         Result := Inside.GetDefaultAtom().GetDescriptionRemoteDetailed(Perspective, cdIn)
      else
         Result := '';
      Contents := GetDescriptionIn(Perspective, [optDeepChildren, optThorough, optFar]);
      if (Contents = '') then
         Contents := GetDescriptionEmpty(Perspective);
      if (Result = '') then
         Result := Contents
      else
         Result := Result + #10 + Contents;
   end
   else
   begin
      Result := GetDescriptionClosed(Perspective);
   end;
end;

function TThing.GetDescriptionEmpty(Perspective: TAvatar): AnsiString;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' empty.';
end;

function TThing.GetDescriptionClosed(Perspective: TAvatar): AnsiString;
var
   PositionOverride: TThingPosition;
   Inside: TAtom;
begin
   Assert(not IsOpen());
   PositionOverride := tpIn;
   Inside := GetInside(PositionOverride);
   if (not Assigned(Inside)) then
      Result := 'It is not clear how to get inside ' + GetDefiniteName(Perspective) + '.'
   else
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' closed.';
end;

function TThing.GetInventory(Perspective: TAvatar): AnsiString;
begin
   Result := GetDescriptionCarried(Perspective, True);
   if (Result = '') then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' not carrying anything.';
end;

function TThing.GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): AnsiString;
var
   Child: TThing;
begin
   Result := '';
   for Child in FChildren do
   begin
      { we exclude context so that, e.g., we don't say "there's a pedestal here!" when you're on it }
      if ((Child <> Context) and (Child.Position in tpAutoDescribe)) then
      begin
         if (Length(Result) > 0) then
            Result := Result + ' ';
         Result := Result + Child.GetPresenceStatement(Perspective, Mode) + WithSpaceIfNotEmpty(Child.GetDescriptionState(Perspective));
      end;
   end;
end;

function TThing.GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString;
begin
   Result := Capitalise(CardinalDirectionToDirectionString(Direction)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + '.' + WithSpaceIfNotEmpty(GetDescriptionState(Perspective));
end;

function TThing.GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString;
var
   Additional: AnsiString;
begin
   if (IsOpen() or ((Perspective.Parent = Self) and (Perspective.Position in tpContained))) then
      Result := GetDescriptionIn(Perspective, Options, Prefix)
   else
      Result := '';
   Additional := GetDescriptionCarried(Perspective, optDeepChildren in Options, Prefix);
   if ((Length(Result) > 0) and (Length(Additional) > 0)) then
      Result := Result + #10;
   Result := Result + Additional;
end;

function TThing.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString;
var
   Child: TThing;
   S: AnsiString;
begin
   if (Position in tpAutoDescribeDirectional) then
      Result := GetDescriptionDirectional(Perspective, Mode, Direction)
   else
      Result := '';
   for Child in FChildren do
   begin
      if (not (Child.Position in tpContained)) then
         S := Child.GetDescriptionRemoteBrief(Perspective, Mode, Direction);
      if (Length(S) > 0) then
      begin
         if (Length(Result) > 0) then
            Result := Result + ' ';
         Result := Result + S;
      end;
   end;
end;

function TThing.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := 'Looking ' + CardinalDirectionToString(Direction) + ', you see ' + GetIndefiniteName(Perspective) + '. ' +
             GetBasicDescription(Perspective, psThereIsAThingThere);
end;

function TThing.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString;

   procedure ProcessBatch(Children: TThingList; ExpectedPositionFilter: TThingPositionFilter);
   var
      Child: TThing;
   begin
      for Child in Children do
      begin
         if (((Child <> Perspective) or (not (optOmitPerspective in Options))) and (Child.Position in ExpectedPositionFilter)) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            Result := Result + Prefix + '  ' + Capitalise(Child.GetIndefiniteName(Perspective)) + '.';
            if (optDeepChildren in Options) then
               Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionIn(Perspective, Options - [optFar, optThorough], Prefix + '  '));
         end;
      end;
   end;

var
   Inside, Surface: TAtom;
   ExpectedPosition: TThingPosition;
   ExpectedPositionFilter: TThingPositionFilter;
begin
   Result := '';
   if (optThorough in Options) then
      ExpectedPositionFilter := [tpIn, tpEmbedded]
   else
      ExpectedPositionFilter := [tpIn];
   ProcessBatch(FChildren, ExpectedPositionFilter);
   Surface := GetSurface();
   if (Surface <> Self) then
      ProcessBatch(Surface.FChildren, ExpectedPositionFilter);
   if (optFar in Options) then
   begin
      ExpectedPosition := tpIn;
      Inside := GetInside(ExpectedPosition);
      if (Assigned(Inside) and (((Inside <> Self) and (Inside <> Surface)) or (not (ExpectedPosition in ExpectedPositionFilter)))) then
         ProcessBatch(Inside.FChildren, [ExpectedPosition]);
   end;
   if (Length(Result) > 0) then
      Result := Prefix + GetDescriptionInTitle(Perspective, Options) + #10 + Result;
end;

function TThing.GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('contains', 'contain', IsPlural(Perspective)) + ':';
end;

function TThing.GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: AnsiString = ''): AnsiString;

   procedure ProcessBatch(Children: TThingList);
   var
      Child: TThing;
   begin
      for Child in Children do
      begin
         if (Child.Position = tpCarried) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            Result := Result + Prefix + '  ' + Capitalise(Child.GetIndefiniteName(Perspective)) + '.';
            if (DeepCarried) then
               Result := Result + WithNewlineIfNotEmpty(Child.GetDescriptionOn(Perspective, [optDeepOn, optPrecise], Prefix + '  ')) +
                                  WithNewlineIfNotEmpty(Child.GetDescriptionChildren(Perspective, [optDeepChildren], Prefix + '  '));
         end;
      end;
   end;

begin
   Result := '';
   ProcessBatch(FChildren);
   if (GetSurface() <> Self) then
      ProcessBatch(GetSurface().FChildren);
   if (Length(Result) > 0) then
      Result := Prefix + GetDescriptionCarriedTitle(Perspective, DeepCarried) + #10 + Result;
end;

function TThing.GetDescriptionCarriedTitle(Perspective: TAvatar; DeepCarried: Boolean): AnsiString;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is carrying', 'are carrying', IsPlural(Perspective)) + ':';
end;

function TThing.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString;
begin
   if (Mode = psThereIsAThingHere) then
      Result := 'There ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' here.'
   else
   if (Mode = psThereIsAThingThere) then
      Result := 'There ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' there.'
   else
   if (Mode = psOnThatThingIsAThing) then
      Result := Capitalise(ThingPositionToString(FPosition)) + ' ' + FParent.GetDefiniteName(Perspective) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + '.'
   else
   if (Mode = psTheThingIsOnThatThing) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetLongDefiniteName(Perspective) + '.'
   else
   if (Mode = psOnThatSpecialThing) then
   begin
      if (FParent is TThing) then
         Result := ThingPositionToString(FPosition) + ' ' + FParent.GetLongDefiniteName(Perspective)
      else
         Result := '';
   end
   else
      Assert(False, 'unknown mode');
end;

function TThing.GetDescriptionWriting(Perspective: TAvatar): AnsiString;
begin
   Result := 'There is no discernible writing on ' + GetDefiniteName(Perspective) + '.';
end;

function TThing.CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   if ((ThingPosition = tpIn) and (not IsOpen())) then
   begin
      Result := False;
      Message := TMessage.Create(mkClosed, GetDescriptionClosed(Perspective));
   end
   else
   begin
      Result := inherited;
   end;
end;

function TThing.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Result := True;
end;

function TThing.CanTake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Result := CanMove(Perspective, Message);
end;

function TThing.CanShake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Result := CanTake(Perspective, Message);
end;

procedure TThing.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
{ see also GetLookTowardsDirection() }

   procedure DeferToParent();
   begin
      Assert(Assigned(FParent));
      if (FPosition in tpDeferNavigationToParent) then
         FParent.Navigate(Direction, Perspective)
      else
         Perspective.AvatarMessage(TMessage.Create(mkCannotMoveBecauseLocation, '_ cannot go _; _ _ _ _.',
                                                                                [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                                                                 CardinalDirectionToString(Direction),
                                                                                 Perspective.GetSubjectPronoun(Perspective),
                                                                                 TernaryConditional('is', 'are', Perspective.IsPlural(Perspective)),
                                                                                 ThingPositionToString(FPosition),
                                                                                 FParent.GetDefiniteName(Perspective)]));
   end;

var
   EquivalentPosition: TThingPosition;
begin
   if (Perspective.Parent = Self) then
   begin
      if (not (Direction in cdPhysicalDirections)) then
      begin
         Assert(Direction = cdOut); { cdIn is handled in player.pas }
         if (not (Perspective.Position in tpContained) or IsOpen()) then
         begin
            if (FPosition in tpContained) then
               EquivalentPosition := tpIn
            else
               EquivalentPosition := tpOn;
            DoNavigation(Self, FParent, EquivalentPosition, Perspective)
         end
         else
            Perspective.AvatarMessage(TMessage.Create(mkClosed, GetDescriptionClosed(Perspective)));
      end
      else
      if (Perspective.Position in tpDeferNavigationToParent) then
         DeferToParent()
      else
         Perspective.AvatarMessage(TMessage.Create(mkCannotMoveBecauseLocation, '_ cannot go _; _ _ _ _.',
                                                                                [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                                                                 CardinalDirectionToString(Direction),
                                                                                 Perspective.GetSubjectPronoun(Perspective),
                                                                                 TernaryConditional('is', 'are', Perspective.IsPlural(Perspective)),
                                                                                 ThingPositionToString(Perspective.Position),
                                                                                 GetDefiniteName(Perspective)]));
   end
   else
   begin
      Assert(Direction in cdPhysicalDirections);
      DeferToParent();
   end;
end;

function TThing.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := ((not (Child.Position in tpContained)) or (not FromOutside) or (IsOpen()));
end;

function TThing.IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean;
begin
   Result := PropertyFilter <= GetFeatures();
end;

function TThing.IsExplicitlyReferenceable(Perspective: TAvatar): Boolean;
begin
   Result := True;
end;

procedure TThing.FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   Assert(Assigned(FParent));
   if ((FPosition in PositionFilter) and ((foIncludeNonImplicits in Options) or IsImplicitlyReferenceable(Perspective, PropertyFilter))) then
      List.AppendItem(Self);
   inherited;
end;

function TThing.FindThingTraverser(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   if (Thing = Self) then
      Result := True
   else
      Result := inherited;
end;

procedure TThing.AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter);
var
   Count: Cardinal;
   GrammaticalNumber: TGrammaticalNumber;
begin
   if (IsExplicitlyReferencedThing(Tokens, Start, Perspective, Count, GrammaticalNumber)) then
      Reporter(Self, Count, GrammaticalNumber);
   inherited;
end;

procedure TThing.Moved(OldParent: TAtom; Carefully: Boolean; Perspective: TAvatar);
begin
   // XXX should be more specific about where things are going, e.g. 'takes x', 'drops x', 'puts x on y', 'moves x around' (if oldparent=newparent)
   DoBroadcast([OldParent, FParent], Perspective, [C(M(@Perspective.GetDefiniteName)), MP(Perspective, M(' moves '), M(' move ')), M(@GetDefiniteName), M('.')]);
end;

procedure TThing.Shake(Perspective: TAvatar);
begin
end;

procedure TThing.Press(Perspective: TAvatar);
begin
   Perspective.AvatarMessage(TMessage.Create(mkNoOp, 'Nothing happens.'));
end;

function TThing.GetFeatures(): TThingFeatures;
begin
   Result := [];
end;

function TThing.CanDig(Target: TThing; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Message := TMessage.Create(mkInappropriateTool, '_ _ not make a good digging tool.',
                                                   [Capitalise(GetDefiniteName(Perspective)),
                                                    TernaryConditional('does', 'do', IsPlural(Perspective))]);
   Result := False;
end;

function TThing.Dig(Spade: TThing; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Message := TMessage.Create(mkBogus, '_ cannot dig _.',
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetDefiniteName(Perspective)]);
   Result := False;
end;

procedure TThing.Dug(Target: TThing; Perspective: TAvatar; var Message: TMessage);
begin
end;

function TThing.IsOpen(): Boolean;
begin
   Result := False;
end;

{$IFDEF DEBUG}
function TThing.Debug(): AnsiString;
begin
   Result := inherited;
   Result := Result + #10 +
             'IsPlural: ' + TernaryConditional('singular', 'plural', IsPlural(nil)) + #10 +
             'Position: ' + ThingPositionToString(FPosition) + ' ' + FParent.GetName(nil) + #10 +
             'IsOpen: ' + TernaryConditional('closed', 'open', IsOpen()) + #10 +
             'GetIntrinsicSize(): ' + AnsiString(GetIntrinsicSize()) + #10 +
             'GetSurfaceSizeManifest(): ' + AnsiString(GetSurfaceSizeManifest()) + #10;
end;
{$ENDIF}


function TAvatar.IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingFeatures): Boolean;
begin
   if (Perspective = Self) then
      Result := False
   else
      Result := inherited;
end;

procedure TAvatar.RemoveFromWorld();
begin
   FParent.Remove(Self);
end;

{$IFDEF DEBUG}
function TDummyAvatar.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := False;
end;

function TDummyAvatar.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := '';
end;

function TDummyAvatar.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := '';
end;

function TDummyAvatar.CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := False;
end;

function TDummyAvatar.GetIntrinsicMass(): TThingMass;
begin
   Result := tmLight;
end;

function TDummyAvatar.GetIntrinsicSize(): TThingSize;
begin
   Result := tsSmall;
end;

function TDummyAvatar.IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean;
begin
   Result := False;
end;

procedure TDummyAvatar.DoLook();
begin
end;

procedure TDummyAvatar.AvatarMessage(Message: TMessage);
begin
end;

procedure TDummyAvatar.AnnounceAppearance();
begin
end;

procedure TDummyAvatar.AnnounceDisappearance();
begin
end;

procedure TDummyAvatar.AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection);
begin
end;

procedure TDummyAvatar.AnnounceDeparture(Destination: TAtom);
begin
end;

procedure TDummyAvatar.AnnounceArrival(Source: TAtom);
begin
end;

procedure TDummyAvatar.AnnounceArrival(Source: TAtom; Direction: TCardinalDirection);
begin
end;

procedure TDummyAvatar.AutoDisambiguated(Message: AnsiString);
begin
end;

function TDummyAvatar.HasConnectedPlayer(): Boolean;
begin
   Result := False;
end;

function TDummyAvatar.IsReadyForRemoval(): Boolean;
begin
   Result := True;
end;
{$ENDIF}

constructor TLocation.Create();
begin
   inherited;
end;

destructor TLocation.Destroy();
begin
   inherited;
end;

constructor TLocation.Read(Stream: TReadStream);
var
   Direction: TCardinalDirection;
   Index: Integer;
begin
   inherited;
   for Direction := Low(FDirectionalLandmarks) to High(FDirectionalLandmarks) do
   begin
      SetLength(FDirectionalLandmarks[Direction], Stream.ReadCardinal());
      if (Length(FDirectionalLandmarks[Direction]) > 0) then
         for Index := Low(FDirectionalLandmarks[Direction]) to High(FDirectionalLandmarks[Direction]) do
         begin
            FDirectionalLandmarks[Direction][Index].Direction := Direction;
            FDirectionalLandmarks[Direction][Index].Options := TLandmarkOptions(Stream.ReadCardinal());
            Stream.ReadReference(@Pointer(FDirectionalLandmarks[Direction][Index].Atom));
            if (FDirectionalLandmarks[Direction][Index].Options * loImportantLandmarks <> []) then
            begin
               SetLength(FImportantLandmarks, Length(FImportantLandmarks)+1);
               FImportantLandmarks[High(FImportantLandmarks)] := @FDirectionalLandmarks[Direction][Index];
            end;
         end;
   end;
end;

procedure TLocation.Write(Stream: TWriteStream);
var
   Direction: TCardinalDirection;
   Index: Cardinal;
begin
   inherited;
   for Direction := Low(FDirectionalLandmarks) to High(FDirectionalLandmarks) do
   begin
      Stream.WriteCardinal(Cardinal(Length(FDirectionalLandmarks[Direction])));
      if (Length(FDirectionalLandmarks[Direction]) > 0) then
         for Index := Low(FDirectionalLandmarks[Direction]) to High(FDirectionalLandmarks[Direction]) do
         begin
            Stream.WriteCardinal(Cardinal(FDirectionalLandmarks[Direction][Index].Options));
            Stream.WriteReference(FDirectionalLandmarks[Direction][Index].Atom);
         end;
   end;
end;

function TLocation.HasLandmark(Direction: TCardinalDirection): Boolean;
begin
   Result := Length(FDirectionalLandmarks[Direction]) > 0;
end;

procedure TLocation.AddLandmark(Direction: TCardinalDirection; Atom: TAtom; Options: TLandmarkOptions);
begin
   Assert(Assigned(Atom));
   Assert((Atom is TLocation) or (Atom is TThing));
   Assert((Atom is TLocation) or ((Atom is TThing) and Assigned((Atom as TThing).Parent)), 'Cannot add landmark that does not have parent.');
   Assert((Length(FDirectionalLandmarks[Direction]) = 0) or (not (loPermissibleNavigationTarget in Options)));
   SetLength(FDirectionalLandmarks[Direction], Length(FDirectionalLandmarks[Direction])+1);
   FDirectionalLandmarks[Direction][High(FDirectionalLandmarks[Direction])].Direction := Direction;
   FDirectionalLandmarks[Direction][High(FDirectionalLandmarks[Direction])].Options := Options;
   FDirectionalLandmarks[Direction][High(FDirectionalLandmarks[Direction])].Atom := Atom;
   if (Options * loImportantLandmarks <> []) then
   begin
      SetLength(FImportantLandmarks, Length(FImportantLandmarks)+1);
      FImportantLandmarks[High(FImportantLandmarks)] := @FDirectionalLandmarks[Direction][High(FDirectionalLandmarks[Direction])];
   end;
end;

procedure TLocation.AddSurroundings(Atom: TAtom; const Directions: TCardinalDirectionSet = cdCompasDirection);
var
   Direction: TCardinalDirection;
begin
   for Direction in Directions do
      AddLandmark(Direction, Atom, []);
end;

function TLocation.CanFindInDirection(Direction: TCardinalDirection; Atom: TAtom): Boolean;
var
   Index: Cardinal;
begin
   Assert(Assigned(Atom));
   if (Length(FDirectionalLandmarks[Direction]) > 0) then
      for Index := Low(FDirectionalLandmarks[Direction]) to High(FDirectionalLandmarks[Direction]) do
         if (FDirectionalLandmarks[Direction][Index].Atom = Atom) then
         begin
            Result := True;
            Exit;
         end;
   Result := False;
end;

{$IFDEF DEBUG}
procedure TLocation.AssertDirectionHasDestination(Direction: TCardinalDirection; Atom: TAtom);
var
   PositionOverride: TThingPosition;
   DisambiguationOpening: TThing;
   Message: TMessage;
   ActualDestination, DesiredDestination: TAtom;
   NotificationList: TAtomList;
   Traveller: TAvatar;
begin
   Assert(Assigned(Atom));
   Assert(Length(FDirectionalLandmarks[Direction]) > 0);
   Assert(Low(FDirectionalLandmarks[Direction]) = 0);
   Assert(Assigned(FDirectionalLandmarks[Direction][0].Atom));
   Assert(loPermissibleNavigationTarget in FDirectionalLandmarks[Direction][0].Options, FDirectionalLandmarks[Direction][0].Atom.GetDefiniteName(nil) + ' is not reachable, so cannot be a valid way to reach ' + Atom.GetDefiniteName(nil));
   NotificationList := TAtomList.Create();
   try
      Traveller := TDummyAvatar.Create();
      try
         PositionOverride := tpOn;
         DisambiguationOpening := nil;
         Message := TMessage.Create(mkSuccess, '');
         ActualDestination := FDirectionalLandmarks[Direction][0].Atom.GetEntrance(Traveller, Direction, Traveller, PositionOverride, DisambiguationOpening, Message, NotificationList);
         Assert(Message.AsKind = mkSuccess);
         PositionOverride := tpOn;
         DisambiguationOpening := nil;
         Message := TMessage.Create(mkSuccess, '');
         DesiredDestination := Atom.GetEntrance(Traveller, Direction, Traveller, PositionOverride, DisambiguationOpening, Message, NotificationList);
         Assert(Message.AsKind = mkSuccess);
         Assert(Assigned(ActualDestination));
         Assert(Assigned(DesiredDestination));
         Assert(ActualDestination = DesiredDestination);
      finally
         Traveller.Free();
      end;
   finally
      NotificationList.Free();
   end;
end;
{$ENDIF}


function TLocation.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := False; // good default, but override for things like "The Everglades"
   // (needed for things like "_The Everglades_ are out of reach." in response to "take everglades" from a nearby room)
end;

function TLocation.GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
var
   Index: Cardinal;
begin
   if (Length(FDirectionalLandmarks[Direction]) > 0) then
   begin
      Result := FDirectionalLandmarks[Direction][0].Atom.GetDescriptionRemoteDetailed(Perspective, Direction);
      if (Length(FDirectionalLandmarks[Direction]) > 1) then
      begin
         Result := Result + ' Beyond that, you can see ';
         for Index := 1 to High(FDirectionalLandmarks[Direction]) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         begin
            if (Index > 1) then
            begin
               if (Index < High(FDirectionalLandmarks[Direction])) then
                  Result := Result + ', '
               else
               if (Index > 2) then
                  Result := Result + ', and '
               else
                  Result := Result + ' and ';
            end;
            Result := Result + FDirectionalLandmarks[Direction][Index].Atom.GetIndefiniteName(Perspective);
         end;
         Result := Result + '.';
      end;
   end
   else
      Result := GetLookTowardsDirectionDefault(Perspective, Direction);
end;

function TLocation.GetLookTowardsDirectionDefault(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' ' + TernaryConditional('sees', 'see', Perspective.IsPlural(Perspective)) + ' nothing noteworthy when looking ' + CardinalDirectionToString(Direction) + '.';
end;

function TLocation.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := '';
end;

function TLocation.GetDescriptionHere(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Context: TAtom = nil): AnsiString;

   procedure ProcessBatch(Children: TThingList);
   var
      Child: TThing;
   begin
      for Child in Children do
      begin
         if ((Child.Position in tpAutoDescribe) and
             (Child <> Perspective) and
             (Child <> Context)) then
         begin
            if (Length(Result) > 0) then
               Result := Result + ' ';
            Result := Result + Child.GetPresenceStatement(Perspective, Mode);
            Result := Result + WithSpaceIfNotEmpty(Child.GetDescriptionState(Perspective));
         end;
      end;
   end;

var
   Index: Cardinal;
   Atom: TAtom;
   S: AnsiString;
begin
   Assert(Mode in [psThereIsAThingThere, // look north
                   psThereIsAThingHere]); // look
   Result := '';
   if (Length(FImportantLandmarks) > 0) then
      for Index := Low(FImportantLandmarks) to High(FImportantLandmarks) do
      begin
         Assert(Assigned(FImportantLandmarks[Index]));
         Assert(FImportantLandmarks[Index]^.Options * loImportantLandmarks <> []);
         Atom := FImportantLandmarks[Index]^.Atom;
         if ((Atom <> Context) and (loAutoDescribe in FImportantLandmarks[Index]^.Options)) then
         begin
            S := Atom.GetDescriptionRemoteBrief(Perspective, Mode, FImportantLandmarks[Index]^.Direction);
            if (Length(S) > 0) then
            begin
               if (Length(Result) > 0) then
                  Result := Result + ' ';
               Result := Result + S;
            end;
         end;
      end;
   ProcessBatch(FChildren);
   if (GetSurface() <> Self) then
      ProcessBatch(GetSurface().FChildren);
end;

function TLocation.GetDescriptionRemoteBrief(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString;
begin
   Assert(Mode in [psThereIsAThingThere, // look north (called indirectly from TLocation.GetDescriptionRemoteDetailed() below)
                   psThereIsAThingHere]); // if we're an autodescribed thing to the north of where the player is
   Result := Capitalise(CardinalDirectionToDirectionString(Direction)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + GetDefiniteName(Perspective) + '.';
end;

function TLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := 'Looking ' + CardinalDirectionToString(Direction) + ', you see:' + #10 +
             Capitalise(GetName(Perspective)) + #10 +
             GetBasicDescription(Perspective, psThereIsAThingThere);
end;

function TLocation.GetAtomForDirectionalNavigation(Direction: TCardinalDirection): TAtom;
begin
   Assert(Low(FDirectionalLandmarks[Direction]) = 0);
   if ((Length(FDirectionalLandmarks[Direction]) > 0) and (loPermissibleNavigationTarget in FDirectionalLandmarks[Direction][0].Options)) then
      Result := FDirectionalLandmarks[Direction][0].Atom
   else
      Result := nil;
end;

procedure TLocation.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
var
   Destination: TAtom;
begin
   Destination := GetAtomForDirectionalNavigation(Direction);
   if (Assigned(Destination)) then
      DoNavigation(Self, Destination, Direction, Perspective)
   else
      FailNavigation(Direction, Perspective);
end;

procedure TLocation.FailNavigation(Direction: TCardinalDirection; Perspective: TAvatar);
begin
   Perspective.AvatarMessage(TMessage.Create(mkCannotMoveBecauseCustom, '_ can''t go _ from here.',
                                                                        [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                                                         CardinalDirectionToString(Direction)]));
end;

function TLocation.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Result := GetSurface();
end;

function TLocation.CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean;
var
   Surface: TAtom;
begin
   Surface := GetSurface();
   Assert(Assigned(Surface));
   if (Surface <> Self) then
      Result := Surface.CanSurfaceHold(Manifest)
   else
      Result := True;
end;

procedure TLocation.FindMatchingThings(Perspective: TAvatar; Options: TFindMatchingThingsOptions; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
var
   Index: Cardinal;
begin
   Assert(foFromOutside in Options);
   inherited;
   if (Length(FImportantLandmarks) > 0) then
      for Index := Low(FImportantLandmarks) to High(FImportantLandmarks) do
      begin
         Assert(Assigned(FImportantLandmarks[Index]));
         Assert(FImportantLandmarks[Index]^.Options * loImportantLandmarks <> []);
         if (loThreshold in FImportantLandmarks[Index]^.Options) then
         begin
            Assert(Assigned(FImportantLandmarks[Index]^.Atom));
            FImportantLandmarks[Index]^.Atom.ProxiedFindMatchingThings(Perspective, Options, PositionFilter, PropertyFilter, List);
         end;
      end;
end;

function TLocation.FindThing(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean; out SubjectiveInformation: TSubjectiveInformation): Boolean;
var
   Direction: TCardinalDirection;
   Index: Cardinal;
begin
   Assert(FromOutside = True);
   Result := inherited;
   if (Length(FImportantLandmarks) > 0) then
      for Index := Low(FImportantLandmarks) to High(FImportantLandmarks) do
      begin
         Assert(Assigned(FImportantLandmarks[Index]));
         Assert(FImportantLandmarks[Index]^.Options * loImportantLandmarks <> []);
         if (loThreshold in FImportantLandmarks[Index]^.Options) then
         begin
            Assert(Assigned(FImportantLandmarks[Index]^.Atom));
            if (FImportantLandmarks[Index]^.Atom.ProxiedFindThingTraverser(Thing, Perspective, True)) then
            begin
               Result := True;
               if (not (loConsiderDirectionUnimportantWhenFindingChildren in FImportantLandmarks[Index]^.Options)) then
                  Include(SubjectiveInformation.Directions, FImportantLandmarks[Index]^.Direction);
               Include(SubjectiveInformation.Reachable, rpReachable);
            end;
         end;
      end;
   for Direction := Low(FDirectionalLandmarks) to High(FDirectionalLandmarks) do
      if (FindThingDirectionalTraverser(Thing, Perspective, 0, Direction, SubjectiveInformation)) then
         Result := True;
end;

function TLocation.FindThingDirectionalTraverser(Thing: TThing; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; var SubjectiveInformation: TSubjectiveInformation): Boolean;

   procedure Internal(CurrentDirection: TCardinalDirection; MustBeVisibleFromFarAway, Reversed: Boolean);
   var
      Index: Cardinal;
      CandidateAtom: TAtom;
      CandidateOptions: TLandmarkOptions;
   begin
      if ((Length(FDirectionalLandmarks[CurrentDirection]) > 0) and (Distance < High(Distance))) then
         for Index := Low(FDirectionalLandmarks[CurrentDirection]) to High(FDirectionalLandmarks[CurrentDirection]) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         begin
            CandidateOptions := FDirectionalLandmarks[CurrentDirection][Index].Options;
            if ((not (loThreshold in CandidateOptions)) and
                ((not MustBeVisibleFromFarAway) or (loVisibleFromFarAway in CandidateOptions)) and
                ((not Reversed) or (not (loNotVisibleFromBehind in CandidateOptions)))) then
            begin
               CandidateAtom := FDirectionalLandmarks[CurrentDirection][Index].Atom;
               Assert(Assigned(CandidateAtom));
               if (CandidateAtom is TThing) then
               begin
                  if (CandidateAtom.FindThingTraverser(Thing, Perspective, True)) then
                  begin
                     Result := True;
                     if ((Thing = CandidateAtom) or (not (loConsiderDirectionUnimportantWhenFindingChildren in CandidateOptions))) then
                        Include(SubjectiveInformation.Directions, Direction);
                  end;
               end
               else
               if (CandidateAtom is TLocation) then
               begin
                  if ((CandidateAtom as TLocation).FindThingDirectionalTraverser(Thing, Perspective, Distance+1, CurrentDirection, SubjectiveInformation)) then {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
                     Result := True;
               end
               else
                  Assert(False, 'Not sure how to handle directional atom of class ' + CandidateAtom.ClassName);
               if (Result) then
               begin
                  Include(SubjectiveInformation.Reachable, rpNotReachable);
                  Exit;
               end;
            end;
         end;
   end;

begin
   Assert(Distance < High(Distance));
   Result := False;
   if (Distance > 0) then
   begin
      Internal(cdReverse[Direction], Distance > 1, True);
      if (Result) then
         Exit;
   end;
   Internal(Direction, Distance > 0, False);
end;

procedure TLocation.AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Reporter: TThingReporter);
var
   Index: Cardinal;
   Direction: TCardinalDirection;
begin
   inherited;
   if (Length(FImportantLandmarks) > 0) then
      for Index := Low(FImportantLandmarks) to High(FImportantLandmarks) do
      begin
         Assert(Assigned(FImportantLandmarks[Index]));
         Assert(FImportantLandmarks[Index]^.Options * loImportantLandmarks <> []);
         if (loThreshold in FImportantLandmarks[Index]^.Options) then
         begin
            if (FImportantLandmarks[Index]^.Atom is TLocation) then
               (FImportantLandmarks[Index]^.Atom as TLocation).AddExplicitlyReferencedThingsDirectional(Tokens, Start, Perspective, 1, FImportantLandmarks[Index]^.Direction, Reporter)
            else
               FImportantLandmarks[Index]^.Atom.AddExplicitlyReferencedThings(Tokens, Start, Perspective, True, Reporter);
         end;
      end;
   for Direction := Low(FDirectionalLandmarks) to High(FDirectionalLandmarks) do
      AddExplicitlyReferencedThingsDirectional(Tokens, Start, Perspective, 0, Direction, Reporter);
end;

procedure TLocation.AddExplicitlyReferencedThingsDirectional(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; Distance: Cardinal; Direction: TCardinalDirection; Reporter: TThingReporter);

   procedure Internal(CurrentDirection: TCardinalDirection; MustBeVisibleFromFarAway: Boolean; Reversed: Boolean);
   var
      Index: Cardinal;
      CandidateAtom, Ancestor: TAtom;
      CandidateOptions: TLandmarkOptions;
      ShouldInclude: Boolean;
   begin
      if ((Length(FDirectionalLandmarks[CurrentDirection]) > 0) and (Distance < High(Distance))) then
         for Index := Low(FDirectionalLandmarks[CurrentDirection]) to High(FDirectionalLandmarks[CurrentDirection]) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         begin
            CandidateOptions := FDirectionalLandmarks[CurrentDirection][Index].Options;
            if ((not (loThreshold in CandidateOptions)) and
                ((not MustBeVisibleFromFarAway) or (loVisibleFromFarAway in CandidateOptions)) and
                ((not Reversed) or (not (loNotVisibleFromBehind in CandidateOptions)))) then
            begin
               CandidateAtom := FDirectionalLandmarks[CurrentDirection][Index].Atom;
               Assert(Assigned(CandidateAtom));
               if (CandidateAtom is TLocation) then
               begin
                  (CandidateAtom as TLocation).AddExplicitlyReferencedThingsDirectional(Tokens, Start, Perspective, Distance+1, CurrentDirection, Reporter); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
               end
               else
               if (CandidateAtom is TThing) then
               begin
                  if (Distance > 0) then
                  begin
                     // if we're far away, then include all our landmarks
                     ShouldInclude := True;
                  end
                  else
                  begin
                     // otherwise, only include foreign ones; the local ones are taken care of already by TAtom
                     Ancestor := (CandidateAtom as TThing).Parent;
                     while ((Assigned(Ancestor)) and (Ancestor is TThing)) do
                        Ancestor := (Ancestor as TThing).Parent;
                     ShouldInclude := Ancestor <> Self;
                  end;
                  if (ShouldInclude) then
                     CandidateAtom.AddExplicitlyReferencedThings(Tokens, Start, Perspective, True, Reporter);
               end
               else
                  Assert(False, 'Not sure how to handle directional atom of class ' + CandidateAtom.ClassName);
            end;
         end;
   end;

begin
   Assert(Distance < High(Distance));
   if (Distance > 0) then
      Internal(cdReverse[Direction], Distance > 1, True);
   Internal(Direction, Distance > 0, False);
end;


procedure ConnectLocations(SourceLocation: TLocation; Direction: TCardinalDirection; Destination: TLocation);

   procedure ConnectLocationsOneWay(A: TLocation; Direction: TCardinalDirection; B: TLocation);
   begin
      if (Length(A.FDirectionalLandmarks[Direction]) > 0) then
      begin
         {$IFDEF DEBUG}
         A.AssertDirectionHasDestination(Direction, B)
         {$ENDIF}
      end
      else
      begin
         A.AddLandmark(Direction, B, [loPermissibleNavigationTarget]);
      end;
   end;

begin
   ConnectLocationsOneWay(SourceLocation, Direction, Destination);
   ConnectLocationsOneWay(Destination, cdReverse[Direction], SourceLocation);
end;


initialization
   DisposalQueue := nil;
{$INCLUDE registrations/physics.inc}
   InitDisposalQueue();
finalization
   if (Assigned(DisposalQueue)) then
      DisposalQueue.Free();
end.