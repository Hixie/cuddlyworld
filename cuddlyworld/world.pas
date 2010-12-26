{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit world;

interface

uses
   storable, lists, grammarian, thingdim;

type
   TAtom = class;
   TAtomEnumerator = specialize TGenericStorableListEnumerator<TAtom>;
   TAtomList = specialize TStorableList<TAtom, TAtomEnumerator>;
   TThing = class;
   TThingEnumerator = Specialize TGenericStorableListEnumerator<TThing>;
   TInternalThingList = Specialize TStorableList<TThing, TThingEnumerator>;
   TAvatar = class;
   TAvatarEnumerator = Specialize TGenericStorableListEnumerator<TAvatar>;
   TAvatarList = Specialize TStorableList<TAvatar, TAvatarEnumerator>;
   TLocation = class;
   TLocationEnumerator = Specialize TGenericStorableListEnumerator<TLocation>;
   TLocationList = Specialize TStorableList<TLocation, TLocationEnumerator>;

type
   TThingList = class(TInternalThingList)
      function GetIndefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
      function GetDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
      function GetLongDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
   end;

type
   TThingFeature = (tfDiggable, tfCanDig, tfExaminingReads,
                    tfCanHaveThingsPushedOn, { e.g. it has a ramp, or a surface flush with its container -- e.g. holes can have things pushed onto them }
                    tfCanHaveThingsPushedIn); { e.g. it has its entrance flush with its base, or has a lip flush with its container -- holes, bags; but not boxes }
   TThingFeatures = set of TThingFeature;

const
   tfEverything = []; { an empty TThingFeatures set }

type
   TGetDescriptionOnOptions = set of (optDeepOn, optPrecise);
   TGetDescriptionChildrenOptions = set of (optDeepChildren, optFar, optThorough, optOmitPerspective); { deep = bag and inside bag; far = door and inside door }
   TGetPresenceStatementMode = (psThereIsAThingHere { look }, psOnThatThingIsAThing { nested look }, psTheThingIsOnThatThing { find });

   TReferencedCallback = procedure (Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber) of object;

type
   PAtom = ^TAtom; { used for pointers to fields that point to TAtoms }
   TAtom = class(TStorable)
    {$IFDEF DEBUG}
    private
      FIntegritySelf: TAtom;
    {$ENDIF}
    protected
      FChildren: TThingList;
      procedure Removed(Thing: TThing); virtual;
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure Add(Thing: TThing; Position: TThingPosition);
      procedure Add(Thing: TThingEnumerator; Position: TThingPosition);
      procedure Remove(Thing: TThing);
      procedure Remove(Thing: TThingEnumerator);
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      procedure Put(Thing: TThing; Position: TThingPosition; Carefully: Boolean; Perspective: TAvatar);
      function GetMassManifest(): TThingMassManifest; virtual; { self and children that are not tpScenery }
      function GetOutsideSizeManifest(): TThingSizeManifest; virtual; { self and children that are tpOn, tpCarried; add tpContained children if container is flexible }
      function GetInsideSizeManifest(): TThingSizeManifest; virtual; { only children that are tpContained }
      function GetSurfaceSizeManifest(): TThingSizeManifest; virtual; { children that are tpOn }
      procedure GetAvatars(List: TAvatarList; FromOutside: Boolean); virtual;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; virtual;
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
      function GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract;
      function GetBasicDescription(Perspective: TAvatar; Context: TAtom = nil): AnsiString; virtual;
      function GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString; virtual;
      function GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString; virtual;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetDescriptionState(Perspective: TAvatar): AnsiString; virtual; { e.g. 'The bottle is open.' }
      function GetDescriptionHere(Perspective: TAvatar; Context: TAtom = nil): AnsiString; virtual; abstract;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions): AnsiString;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); virtual; abstract; { called by avatar children to trigger DoNavigation correctly }
      procedure FindMatchingThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); virtual;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Callback: TReferencedCallback); virtual;
      function StillReferenceable(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
      function GetDefaultAtom(): TAtom; virtual; { the TAtom that is responsible for high-level dealings for this one (opposite of GetSurface) }
      function GetInside(var PositionOverride: TThingPosition): TAtom; virtual; { returns nil if there's no inside to speak of }
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; virtual;
      function GetSurface(): TAtom; virtual; { the TAtom that is responsible for the minutiae of where things dropped on this one actually go (opposite of GetDefaultAtom) }
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; virtual; abstract;
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; NotificationList: TAtomList): TAtom; virtual; abstract;
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
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure GetAvatars(List: TAvatarList; FromOutside: Boolean); override;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function GetIntrinsicMass(): TThingMass; virtual; abstract;
      function GetIntrinsicSize(): TThingSize; virtual; abstract;
      function GetMassManifest(): TThingMassManifest; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; NotificationList: TAtomList): TAtom; override;
      function GetSummaryName(Perspective: TAvatar): AnsiString; override;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetLongDefiniteName(Perspective: TAvatar): AnsiString; override;
      function IsPlural(Perspective: TAvatar): Boolean; virtual;
      function GetTitle(Perspective: TAvatar): AnsiString; override;
      function GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString; override;
      function GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString; override;
      function GetExamine(Perspective: TAvatar): AnsiString; virtual;
      function GetLookUnder(Perspective: TAvatar): AnsiString; virtual;
      function GetLookIn(Perspective: TAvatar): AnsiString; virtual;
      function GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetInventory(Perspective: TAvatar): AnsiString; virtual;
      function GetDescriptionHere(Perspective: TAvatar; Context: TAtom = nil): AnsiString; override;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString; override;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString = ''): AnsiString; virtual;
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString; virtual;
      function GetDescriptionEmpty(Perspective: TAvatar): AnsiString; virtual; { only called for optThorough searches }
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; virtual; { used both from inside and outside }
      function GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: AnsiString = ''): AnsiString; virtual;
      function GetDescriptionCarriedTitle(Perspective: TAvatar; DeepCarried: Boolean): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; virtual;
      function GetDescriptionWriting(Perspective: TAvatar): AnsiString; virtual;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      procedure FindMatchingThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList); override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; virtual; abstract;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Callback: TReferencedCallback); override;
      function StillReferenceable(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
      procedure Moved(OldParent: TAtom; Carefully: Boolean; Perspective: TAvatar); virtual;
      procedure Shake(Perspective: TAvatar); virtual;
      procedure Press(Perspective: TAvatar); virtual;
      function GetFeatures(): TThingFeatures; virtual;
      function CanDig(Target: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      procedure Dug(Target: TThing; Perspective: TAvatar; var Message: AnsiString); virtual;
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
      procedure GetAvatars(List: TAvatarList; FromOutside: Boolean); override;
      procedure DoLook(); virtual; abstract;
      procedure AvatarMessage(Message: AnsiString); virtual; abstract;
      procedure AvatarBroadcast(Message: AnsiString); virtual; abstract;
      procedure AnnounceAppearance(); virtual; abstract;
      procedure AnnounceDisappearance(); virtual; abstract;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); virtual; abstract;
      procedure AnnounceDeparture(Destination: TAtom); virtual; abstract;
      procedure AnnounceArrival(Source: TAtom); virtual; abstract;
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); virtual; abstract;
      function HasConnectedPlayer(): Boolean; virtual; abstract;
      function IsReadyForRemoval(): Boolean; virtual; abstract;
      procedure RemoveFromWorld(); virtual;
      function GetUsername(): AnsiString; virtual;
   end;

   TLocation = class(TAtom)
    protected
      FNorth, FNorthEast, FEast, FSouthEast, FSouth, FSouthWest, FWest, FNorthWest, FUp, FDown, FOut: TAtom;
      function GetFieldForDirection(Direction: TCardinalDirection): PAtom;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure ConnectCardinals(ANorth, AEast, ASouth, AWest: TAtom);
      procedure ConnectDiagonals(ANorthEast, ASouthEast, ASouthWest, ANorthWest: TAtom);
      procedure ConnectVerticals(AUp, ADown: TAtom);
      procedure ConnectExits(AOut: TAtom);
      function GetAtomForDirection(Direction: TCardinalDirection): TAtom;
      function GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionHere(Perspective: TAvatar; Context: TAtom = nil): AnsiString; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      procedure FailNavigation(Direction: TCardinalDirection; Perspective: TAvatar); { also called when trying to dig in and push something in this direction }
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; NotificationList: TAtomList): TAtom; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
   end;

   TWorld = class(TStorable)
    protected
      FLocations: TLocationList;
      FGlobalThings: TThingList;
      FPlayers: TAvatarList;
      FDirty: Boolean;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure AddLocation(Location: TLocation); { World will free these }
      procedure AddGlobalThing(GlobalThing: TThing); { World will free these }
      procedure AddPlayer(Avatar: TAvatar); virtual; { these must added to the world before this method is called (derived classes can override this method to do that) }
      function GetPlayer(Name: AnsiString): TAvatar;
      function GetPlayerCount(): Cardinal;
      procedure CheckForDisconnectedPlayers();
      procedure CheckDisposalQueue();
      procedure SetDirty();
      procedure Saved();
      property Dirty: Boolean read FDirty;
   end;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);

procedure QueueForDisposal(Atom: TAtom);

implementation

uses
   sysutils, broadcast;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
var
   Destination: TAtom;
   Message: AnsiString;
   Position: TThingPosition;
   NotificationList: TAtomList;
   NotificationTarget: TAtom;
begin
   Assert(Assigned(AFrom));
   Assert(Assigned(ATo));
   Assert(Assigned(Perspective));
   Position := tpOn;
   Message := '';
   NotificationList := TAtomList.Create();
   try
      Destination := ATo.GetEntrance(Perspective, AFrom, Perspective, Position, Message, NotificationList);
      if (Assigned(Destination)) then
      begin
         Perspective.AnnounceDeparture(ATo, Direction);
         for NotificationTarget in NotificationList do
            NotificationTarget.HandlePassedThrough(Perspective, AFrom, Destination, Position, Perspective);
         Destination.Add(Perspective, Position);
         Perspective.AnnounceArrival(AFrom.GetDefaultAtom(), ReverseCardinalDirection(Direction));
         Perspective.DoLook();
      end
      else
      begin
         Perspective.AvatarMessage('You cannot go ' + CardinalDirectionToString(Direction) + '. ' + Message);
      end;
   finally
      NotificationList.Free();
   end;
end;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);
var
   Destination: TAtom;
   Message: AnsiString;
   Success: Boolean;
   Ancestor: TAtom;
   NotificationList: TAtomList;
   NotificationTarget: TAtom;
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
      Assert(ATo is TThing);
      Perspective.AvatarMessage('That would prove rather challenging given where ' + ATo.GetDefiniteName(Perspective) + ' ' + TernaryConditional('is', 'are', (ATo as TThing).IsPlural(Perspective)) + ' relative to yourself.');
   end
   else
   if (Position = tpOn) then
   begin
      ATo := ATo.GetSurface();
      Assert(Assigned(ATo));
      Assert(ATo is TThing);
      Message := '';
      Success := (ATo as TThing).CanPut(Perspective, Position, Perspective, Message);
      if (Success) then
      begin
         ATo.Add(Perspective, Position);
         Perspective.DoLook();
      end
      else
      begin
         Perspective.AvatarMessage('You cannot get onto ' + ATo.GetDefiniteName(Perspective) + '. ' + Message);
      end;
   end
   else
   if (Position = tpIn) then
   begin
      Assert(ATo is TThing);
      NotificationList := TAtomList.Create();
      try
         Destination := ATo.GetEntrance(Perspective, AFrom, Perspective, Position, Message, NotificationList);
         if (Assigned(Destination)) then
         begin
            Perspective.AnnounceDeparture(ATo);
            for NotificationTarget in NotificationList do
               NotificationTarget.HandlePassedThrough(Perspective, AFrom, Destination, Position, Perspective);
            Destination.Add(Perspective, Position);
            Perspective.AnnounceArrival(AFrom.GetDefaultAtom());
            Perspective.DoLook();
         end
         else
         begin
            Perspective.AvatarMessage('You cannot enter ' + ATo.GetDefiniteName(Perspective) + '. ' + Message);
         end;
      finally
         NotificationList.Free();
      end;
   end
   else
      raise EAssertionFailed.Create('unexpected position for navigation: ' + IntToStr(Cardinal(Position)));
end;


// would be nice to find a way to have the next three methods be implemented somehow using a common body

function TThingList.GetIndefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
var
   Count: Cardinal;
   E: TThingEnumerator;
begin
   Result := '';
   Count := 0;
   E := GetEnumerator();
   try
      while (E.MoveNext()) do
      begin
         if (Count > 0) then
         begin
            if (E.HasMore()) then
               Result := Result + ', '
            else
            if (Count > 1) then
               Result := Result + ', ' + Conjunction + ' '
            else
               Result := Result + ' ' + Conjunction + ' ';
         end;
         Result := Result + E.Current.GetIndefiniteName(Perspective);
         Inc(Count);
      end;
   finally
      E.Free();
   end;
end;

function TThingList.GetDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
var
   Count: Cardinal;
   E: TThingEnumerator;
begin
   Result := '';
   Count := 0;
   E := GetEnumerator();
   try
      while (E.MoveNext()) do
      begin
         if (Count > 0) then
         begin
            if (E.HasMore()) then
               Result := Result + ', '
            else
            if (Count > 1) then
               Result := Result + ', ' + Conjunction + ' '
            else
               Result := Result + ' ' + Conjunction + ' ';
         end;
         Result := Result + E.Current.GetDefiniteName(Perspective);
         Inc(Count);
      end;
   finally
      E.Free();
   end;
end;

function TThingList.GetLongDefiniteString(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
var
   Count: Cardinal;
   E: TThingEnumerator;
begin
   Result := '';
   Count := 0;
   E := GetEnumerator();
   try
      while (E.MoveNext()) do
      begin
         if (Count > 0) then
         begin
            if (E.HasMore()) then
               Result := Result + ', '
            else
            if (Count > 1) then
               Result := Result + ', ' + Conjunction + ' '
            else
               Result := Result + ' ' + Conjunction + ' ';
         end;
         Result := Result + E.Current.GetLongDefiniteName(Perspective);
         Inc(Count);
      end;
   finally
      E.Free();
   end;
end;

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

procedure TAtom.Add(Thing: TThing; Position: TThingPosition);
{$IFOPT C+}
var
   TempPosition: TThingPosition;
{$ENDIF}
begin
   {$IFOPT C+}
     TempPosition := tpIn;
     Assert((Position <> tpIn) or (GetInside(TempPosition) = Self), 'tried to put something inside something without an inside');
   {$ENDIF}
   if (Assigned(Thing.FParent)) then
      Thing.FParent.Remove(Thing);
   Assert(not Assigned(Thing.FParent));
   FChildren.AppendItem(Thing);
   Thing.FParent := Self;
   Thing.FPosition := Position;
end;

procedure TAtom.Add(Thing: TThingEnumerator; Position: TThingPosition);
var
   OldParent: TAtom;
   ActualThing: TThing;
   {$IFOPT C+} TempPosition: TThingPosition; {$ENDIF}
begin
   Assert(Thing.FList <> FChildren);
   {$IFOPT C+}
     TempPosition := tpIn;
     Assert((Position <> tpIn) or (GetInside(TempPosition) = Self), 'tried to put something inside something without an inside');
   {$ENDIF}
   ActualThing := Thing.Current;
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

procedure TAtom.Remove(Thing: TThingEnumerator);
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

function TAtom.CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   if (ThingPosition = tpOn) then
   begin
      Result := CanSurfaceHold(Thing.GetIntrinsicSize());
      if (not Result) then
         Message := 'There is not enough room on ' + GetDefiniteName(Perspective) + ' for ' + Thing.GetDefiniteName(Perspective) + '.';
   end
   else
   if (ThingPosition = tpIn) then
   begin
      Result := CanInsideHold(Thing.GetOutsideSizeManifest());
      if (not Result) then
      begin
         if ((not Assigned(GetInside(ThingPosition))) and (Self is TThing)) then
            Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('does', 'do', (Self as TThing).IsPlural(Perspective)) + ' not appear to have an opening.'
         else
            Message := 'There is not enough room in ' + GetDefiniteName(Perspective) + ' for ' + Thing.GetDefiniteName(Perspective) + '.';
      end;
   end
   else
      raise EAssertionFailed.Create('Unexpected position ' + IntToStr(Cardinal(ThingPosition)));
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
procedure TAtom.GetAvatars(List: TAvatarList; FromOutside: Boolean);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (FromOutside <> (Child.Position in tpContained)) then { assumes that we are closed (TThing.GetAvatars solves that) }
         Child.GetAvatars(List, True);
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
             GetBasicDescription(Perspective) +
             WithNewlineIfNotEmpty(GetHorizonDescription(Perspective, Self)) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optOmitPerspective]));
end;

function TAtom.GetLookAt(Perspective: TAvatar): AnsiString;
begin
   Result := GetBasicDescription(Perspective) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optDeepChildren]));
end;

function TAtom.GetBasicDescription(Perspective: TAvatar; Context: TAtom = nil): AnsiString;
begin
   Result := GetDescriptionSelf(Perspective) +
             WithSpaceIfNotEmpty(GetDescriptionState(Perspective)) +
             WithSpaceIfNotEmpty(GetDescriptionHere(Perspective, Context));
end;

function TAtom.GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString;
begin
   Result := '';
end;

function TAtom.GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString;
begin
   Result := GetBasicDescription(Perspective, Context);
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

procedure TAtom.FindMatchingThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (((IncludePerspectiveChildren) or (Child <> Perspective)) and (IsChildTraversable(Child, Perspective, FromOutside))) then
         Child.FindMatchingThings(Perspective, True, IncludePerspectiveChildren, PositionFilter, PropertyFilter, List);
end;

procedure TAtom.AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Callback: TReferencedCallback);
var
   Child: TThing;
begin
   for Child in FChildren do
      if (IsChildTraversable(Child, Perspective, FromOutside)) then
         Child.AddExplicitlyReferencedThings(Tokens, Start, Perspective, True, Callback);
end;

function TAtom.StillReferenceable(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
var
   Child: TThing;
begin
   for Child in FChildren do
   begin
      if (IsChildTraversable(Child, Perspective, FromOutside) and Child.StillReferenceable(Thing, Perspective, True)) then
      begin
         Result := True;
         Exit;
      end;
   end;
   Result := False;
end;

function TAtom.GetDefaultAtom(): TAtom;
begin
   Result := Self;
end;

function TAtom.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Assert(PositionOverride = tpIn);
   Result := nil;
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
begin
   Result := GetName(nil) + #10 +
             'Long Name: ' + GetLongDefiniteName(nil) + #10 +
             'Class: ' + ClassName;
end;
{$ENDIF}


constructor TThing.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@FParent);
   FPosition := TThingPosition(Stream.ReadCardinal());
end;

procedure TThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FParent);
   Stream.WriteCardinal(Cardinal(FPosition));
end;

procedure TThing.GetAvatars(List: TAvatarList; FromOutside: Boolean);
var
   Child: TThing;
begin
   if (FromOutside and IsOpen()) then
   begin
      for Child in FChildren do
         if (Child.Position in tpContained) then
            Child.GetAvatars(List, True);
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
   if (Assigned(Surface) and (Surface <> Self)) then
      Result := Surface.CanSurfaceHold(Manifest)
   else
      Result := (GetSurfaceSizeManifest() + Manifest) <= (GetIntrinsicSize());
end;

function TThing.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; NotificationList: TAtomList): TAtom;
begin
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
      Message := Capitalise(Perspective.GetIndefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', Perspective.IsPlural(Perspective)) + ' bigger than ' + GetDefiniteName(Perspective) + '.';
   end
   else
   begin
      Message := GetDescriptionState(Perspective);
      if (Length(Message) = 0) then
         Message := Capitalise(GetDefiniteName(Perspective)) + ' has no discernible entrance.';
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
   Result := GetBasicDescription(Perspective) +
             WithNewlineIfNotEmpty(Writing) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optDeepChildren, optThorough]));
end;

function TThing.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
    Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.';
end;

function TThing.GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
{ see also Navigate() }

   function DeferToParent(): AnsiString;
   begin
      Assert(Assigned(FParent));
      if (FPosition in tpDeferNavigationToParent) then
         Result := FParent.GetLookDirection(Perspective, Direction)
      else
         Result := 'You are ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.';
   end;

begin
   Assert(Direction <> cdIn);
   if (Perspective.Parent = Self) then
   begin
      if (Direction > cdLastPhysical) then
      begin
         Assert(Direction = cdOut);
         if ((not (Perspective.Position in tpContained)) or IsOpen()) then
            Result := GetDescriptionClosed(Perspective)
         else
            Result := FParent.GetDescriptionRemoteDetailed(Perspective, Direction);
      end
      else
      if (Perspective.Position in tpDeferNavigationToParent) then
      begin
         Result := DeferToParent();
      end
      else
      begin
         Result := 'You are ' + ThingPositionToString(Perspective.Position) + ' ' + GetDefiniteName(Perspective) + '.';
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
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' empty.';
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
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' closed.';
end;

function TThing.GetInventory(Perspective: TAvatar): AnsiString;
begin
   Result := GetDescriptionCarried(Perspective, True);
   if (Result = '') then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' not carrying anything.';
end;

function TThing.GetDescriptionHere(Perspective: TAvatar; Context: TAtom = nil): AnsiString;
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
         Result := Result + Child.GetPresenceStatement(Perspective, psThereIsAThingHere) + WithSpaceIfNotEmpty(Child.GetDescriptionState(Perspective));
      end;
   end;
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
               Result := Result + WithSpaceIfNotEmpty(Child.GetDescriptionIn(Perspective, Options - [optFar, optThorough], Prefix + '  '));
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

function TThing.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := 'Looking ' + CardinalDirectionToString(Direction) + ', you see ' + GetIndefiniteName(Perspective) + '. ' + GetBasicDescription(Perspective);
end;

function TThing.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString;
begin
   if (Mode = psThereIsAThingHere) then
      Result := 'There ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' here.'
   else
   if (Mode = psOnThatThingIsAThing) then
      Result := Capitalise(ThingPositionToString(FPosition)) + ' ' + FParent.GetDefiniteName(Perspective) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + '.'
   else
   if (Mode = psTheThingIsOnThatThing) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.'
   else
      raise EAssertionFailed.Create('unknown mode');
end;

function TThing.GetDescriptionWriting(Perspective: TAvatar): AnsiString;
begin
   Result := 'There is no discernible writing on ' + GetDefiniteName(Perspective) + '.';
end;

function TThing.CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   if ((ThingPosition = tpIn) and (not IsOpen())) then
   begin
      Result := False;
      Message := GetDescriptionClosed(Perspective);
   end
   else
   begin
      Result := inherited;
   end;
end;

function TThing.CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := CanMove(Perspective, Message);
end;

function TThing.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := True;
end;

procedure TThing.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
{ see also GetLookDirection() }

   procedure DeferToParent();
   begin
      Assert(Assigned(FParent));
      if (FPosition in tpDeferNavigationToParent) then
         FParent.Navigate(Direction, Perspective)
      else
         Perspective.AvatarMessage('You cannot go ' + CardinalDirectionToString(Direction) + '; you''re ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.');
   end;

var
   EquivalentPosition: TThingPosition;
begin
   if (Perspective.Parent = Self) then
   begin
      if (Direction > cdLastPhysical) then
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
            Perspective.AvatarMessage(GetDescriptionClosed(Perspective));
      end
      else
      if (Perspective.Position in tpDeferNavigationToParent) then
         DeferToParent()
      else
         Perspective.AvatarMessage('You cannot go ' + CardinalDirectionToString(Direction) + '; you''re ' + ThingPositionToString(Perspective.Position) + ' ' + GetDefiniteName(Perspective) + '.');
   end
   else
   begin
      Assert(Direction <= cdLastPhysical);
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

procedure TThing.FindMatchingThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingFeatures; List: TThingList);
begin
   Assert(Assigned(FParent));
   if ((FPosition in PositionFilter) and IsImplicitlyReferenceable(Perspective, PropertyFilter)) then
      List.AppendItem(Self);
   inherited;
end;

function TThing.StillReferenceable(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   if (Thing = Self) then
      Result := True
   else
      Result := inherited;
end;

procedure TThing.AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Callback: TReferencedCallback);
var
   Count: Cardinal;
   GrammaticalNumber: TGrammaticalNumber;
begin
   if (IsExplicitlyReferencedThing(Tokens, Start, Perspective, Count, GrammaticalNumber)) then
      Callback(Self, Count, GrammaticalNumber);
   inherited;
end;

procedure TThing.Moved(OldParent: TAtom; Carefully: Boolean; Perspective: TAvatar);
begin
   // should be more specific about where things are going, e.g. 'takes x', 'drops x', 'puts x on y', 'moves x around' (if oldparent=newparent)
   DoBroadcast([OldParent, FParent], Perspective, [C(M(@Perspective.GetDefiniteName)), MP(Perspective, M(' moves '), M(' move ')), M(@GetDefiniteName), M('.')]);
end;

procedure TThing.Shake(Perspective: TAvatar);
begin
end;

procedure TThing.Press(Perspective: TAvatar);
begin
   Perspective.AvatarMessage('Nothing happens.');
end;

function TThing.GetFeatures(): TThingFeatures;
begin
   Result := [];
end;

function TThing.CanDig(Target: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('does', 'do', IsPlural(Perspective)) + ' not make a good digging tool.';
   Result := False;
end;

function TThing.Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Message := 'You cannot dig ' + GetDefiniteName(Perspective) + '.';
   Result := False;
end;

procedure TThing.Dug(Target: TThing; Perspective: TAvatar; var Message: AnsiString);
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

procedure TAvatar.GetAvatars(List: TAvatarList; FromOutside: Boolean);
begin
   List.AppendItem(Self);
   inherited;
end;

procedure TAvatar.RemoveFromWorld();
begin
   FParent.Remove(Self);
end;

function TAvatar.GetUsername(): AnsiString;
begin
   Result := '';
end;


constructor TLocation.Create();
begin
   inherited;
end;

destructor TLocation.Destroy();
begin
   inherited;
end;

constructor TLocation.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@FNorth);
   Stream.ReadReference(@FNorthEast);
   Stream.ReadReference(@FEast);
   Stream.ReadReference(@FSouthEast);
   Stream.ReadReference(@FSouth);
   Stream.ReadReference(@FSouthWest);
   Stream.ReadReference(@FWest);
   Stream.ReadReference(@FNorthWest);
   Stream.ReadReference(@FUp);
   Stream.ReadReference(@FDown);
   Stream.ReadReference(@FOut);
end;

procedure TLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FNorth);
   Stream.WriteReference(FNorthEast);
   Stream.WriteReference(FEast);
   Stream.WriteReference(FSouthEast);
   Stream.WriteReference(FSouth);
   Stream.WriteReference(FSouthWest);
   Stream.WriteReference(FWest);
   Stream.WriteReference(FNorthWest);
   Stream.WriteReference(FUp);
   Stream.WriteReference(FDown);
   Stream.WriteReference(FOut);
end;

procedure TLocation.ConnectCardinals(ANorth, AEast, ASouth, AWest: TAtom);
begin
   FNorth := ANorth;
   FEast := AEast;
   FSouth := ASouth;
   FWest := AWest;
end;

procedure TLocation.ConnectDiagonals(ANorthEast, ASouthEast, ASouthWest, ANorthWest: TAtom);
begin
   FNorthEast := ANorthEast;
   FSouthEast := ASouthEast;
   FSouthWest := ASouthWest;
   FNorthWest := ANorthWest;
end;

procedure TLocation.ConnectVerticals(AUp, ADown: TAtom);
begin
   FUp := AUp;
   FDown := ADown;
end;

procedure TLocation.ConnectExits(AOut: TAtom);
begin
   FOut := AOut;
end;

function TLocation.GetFieldForDirection(Direction: TCardinalDirection): PAtom;
begin
   case Direction of
    cdNorth: Result := @FNorth; 
    cdNorthEast: Result := @FNorthEast; 
    cdEast: Result := @FEast; 
    cdSouthEast: Result := @FSouthEast; 
    cdSouth: Result := @FSouth; 
    cdSouthWest: Result := @FSouthWest; 
    cdWest: Result := @FWest; 
    cdNorthWest: Result := @FNorthWest; 
    cdUp: Result := @FUp; 
    cdDown: Result := @FDown; 
    cdOut: Result := @FOut;
    cdIn: raise EAssertionFailed.Create('TLocations don''t have a cdIn field');
    else
      raise Exception.Create('Unknown cardinal direction ' + IntToStr(Ord(Direction)));
   end;
end;

function TLocation.GetAtomForDirection(Direction: TCardinalDirection): TAtom;
begin
   Result := GetFieldForDirection(Direction)^;
end;

function TLocation.GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
var
   RemoteLocation: TAtom;
begin
   Assert(Direction <> cdIn);
   RemoteLocation := GetAtomForDirection(Direction);
   if (Assigned(RemoteLocation)) then
      Result := RemoteLocation.GetDescriptionRemoteDetailed(Perspective, Direction)
   else
      Result := 'You see nothing noteworthy when looking ' + CardinalDirectionToString(Direction) + '.';
end;

function TLocation.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := '';
end;

function TLocation.GetDescriptionHere(Perspective: TAvatar; Context: TAtom = nil): AnsiString;

   procedure ProcessThing(Thing: TThing; PresenceStatement: AnsiString);
   begin
      if (Length(Result) > 0) then
         Result := Result + ' ';
      Result := Result + PresenceStatement + WithSpaceIfNotEmpty(Thing.GetDescriptionState(Perspective));
   end;

   procedure ProcessBatch(Children: TThingList);
   var
      Child: TThing;
   begin
      for Child in Children do
      begin
         if ((Child.Position in tpAutoDescribe) and
             (Child <> Perspective) and
             (Child <> FNorth) and
             (Child <> FNorthEast) and
             (Child <> FEast) and
             (Child <> FSouthEast) and
             (Child <> FSouth) and
             (Child <> FSouthWest) and
             (Child <> FWest) and
             (Child <> FNorthWest) and
             (Child <> FUp) and
             (Child <> FDown) and
             (Child <> Context)) then
            ProcessThing(Child, Child.GetPresenceStatement(Perspective, psThereIsAThingHere));
      end;
   end;

var
   Direction: TCardinalDirection;
   Atom: TAtom;
   Thing: TThing;
begin
   Result := '';
   Direction := Low(Direction);
   while (Direction <= cdLastPhysical) do
   begin
      Atom := GetAtomForDirection(Direction);
      if (Atom <> Context) then
      begin
         if (Atom is TThing) then
         begin
            Thing := Atom as TThing;
            if (Thing.Position in tpAutoDescribe) then
               ProcessThing(Thing, Capitalise(CardinalDirectionToDirectionString(Direction)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Perspective)) + ' ' + Thing.GetIndefiniteName(Perspective) + '.');
         end
         else
         if (Atom is TLocation) then
         begin
            if (Length(Result) > 0) then
               Result := Result + ' ';
            Result := Result + (Atom as TLocation).GetDescriptionRemoteBrief(Perspective, Direction); // should make this optional
         end;
      end;
      Inc(Direction);
   end;
   // could support cdOut by saying "You may also exit the hut to Camp Cuddlyworld."
   ProcessBatch(FChildren);
   if (GetSurface() <> Self) then
      ProcessBatch(GetSurface().FChildren);
end;

function TLocation.GetDescriptionRemoteBrief(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := Capitalise(CardinalDirectionToDirectionString(Direction)) + ' is ' + GetDefiniteName(Perspective) + '.';
end;

function TLocation.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Result := 'Looking ' + CardinalDirectionToString(Direction) + ', you see:' + #10 +
             Capitalise(GetName(Perspective)) + #10 +
             GetBasicDescription(Perspective);
end;

procedure TLocation.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
var
   Destination: TAtom;
begin
   Destination := GetAtomForDirection(Direction);
   if (Assigned(Destination)) then
      DoNavigation(Self, Destination, Direction, Perspective)
   else
      FailNavigation(Direction, Perspective);
end;

procedure TLocation.FailNavigation(Direction: TCardinalDirection; Perspective: TAvatar);
begin
   Perspective.AvatarMessage('You can''t go ' + CardinalDirectionToString(Direction) + ' from here.');
end;

function TLocation.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; NotificationList: TAtomList): TAtom;
begin
   Result := GetSurface();
end;

function TLocation.CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean;
var
   Surface: TAtom;
begin
   Surface := GetSurface();
   if (Assigned(Surface) and (Surface <> Self)) then
      Result := Surface.CanSurfaceHold(Manifest)
   else
      Result := True;
end;


constructor TWorld.Create();
begin
   inherited;
   InitDisposalQueue();
   FLocations := TLocationList.Create([slOwner]);
   FGlobalThings := TThingList.Create([slOwner]);
   FPlayers := TAvatarList.Create();
end;

destructor TWorld.Destroy();
begin
   FLocations.Free();
   FGlobalThings.Free();
   FPlayers.Free();
   inherited;
end;

constructor TWorld.Read(Stream: TReadStream);
begin
   inherited;
   InitDisposalQueue();
   FLocations := Stream.ReadObject() as TLocationList;
   FGlobalThings := Stream.ReadObject() as TThingList;
   FPlayers := Stream.ReadObject() as TAvatarList;
end;

procedure TWorld.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteObject(FLocations);
   Stream.WriteObject(FGlobalThings);
   Stream.WriteObject(FPlayers);
end;

procedure TWorld.AddLocation(Location: TLocation);
begin
   FLocations.AppendItem(Location);
end;

procedure TWorld.AddGlobalThing(GlobalThing: TThing);
begin
   FGlobalThings.AppendItem(GlobalThing);
end;

procedure TWorld.AddPlayer(Avatar: TAvatar);
begin
   Assert(Assigned(Avatar.FParent));
   FPlayers.AppendItem(Avatar);
end;

function TWorld.GetPlayer(Name: AnsiString): TAvatar;
var
   Item: TAvatar;
begin
   Name := LowerCase(Name);
   for Item in FPlayers do
   begin
      Assert(Assigned(Item.FParent));
      if (LowerCase(Item.GetUsername()) = Name) then
      begin
         Result := Item;
         Exit;
      end;
   end;
   Result := nil;
end;

function TWorld.GetPlayerCount(): Cardinal;
begin
   Result := FPlayers.Length;
end;

procedure TWorld.CheckForDisconnectedPlayers();
var
   E: TAvatarEnumerator;
   Item: TAvatar;
begin
   E := FPlayers.GetEnumerator();
   try
      while (E.MoveNext()) do
      begin
         Item := E.Current;
         if ((not Item.HasConnectedPlayer()) and (Item.IsReadyForRemoval())) then
         begin
            E.Remove();
            Item.RemoveFromWorld();
            Item.Free();
         end;
      end;
   finally
      E.Free();
   end;
end;

procedure TWorld.CheckDisposalQueue();
begin
   EmptyDisposalQueue();
end;

procedure TWorld.SetDirty();
begin
   FDirty := True;
end;

procedure TWorld.Saved();
begin
   FDirty := False;
end;

initialization
   DisposalQueue := nil;
   RegisterStorableClass(TWorld, 1);
   RegisterStorableClass(TAtomList, 2);
   RegisterStorableClass(TThingList, 3);
   RegisterStorableClass(TAvatarList, 4);
   RegisterStorableClass(TLocationList, 5);
finalization
   if (Assigned(DisposalQueue)) then
      DisposalQueue.Free();
end.
