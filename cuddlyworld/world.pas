{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit world;

interface

uses
   storable, grammarian, thingdim;

type

   TAtom = class;
   PPAtomItem = ^PAtomItem;
   PAtomItem = ^TAtomItem;
   TAtomItem = record
      Next: PAtomItem;
      Value: TAtom;
   end;

   TThing = class;
   PPThingItem = ^PThingItem;
   PThingItem = ^TThingItem;
   TThingItem = record
      Next: PThingItem;
      Value: TThing;
   end;

   TAvatar = class;
   PPAvatarItem = ^PAvatarItem;
   PAvatarItem = ^TAvatarItem;
   TAvatarItem = record
      Next: PAvatarItem;
      Value: TAvatar;
   end;

   TThingProperty = (tpDiggable, tpCanDig,
                     tpCanHaveThingsPushedOn, { e.g. it has a ramp, or a surface flush with its container -- e.g. holes can have things pushed onto them }
                     tpCanHaveThingsPushedIn); { e.g. it has its entrance flush with its base, or has a lip flush with its container -- holes, bags; but not boxes }
   TThingProperties = set of TThingProperty;

   TGetDescriptionOnOptions = set of (optDeepOn, optPrecise);
   TGetDescriptionChildrenOptions = set of (optDeepChildren, optFar, optThorough, optOmitPerspective); { deep = bag and inside bag; far = door and inside door }
   TGetPresenceStatementMode = (psThereIsAThingHere { look }, psOnThatThingIsAThing { nested look }, psTheThingIsOnThatThing { find });

   TReferencedCallback = procedure (Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber) of object;

   PAtom = ^TAtom;
   TAtom = class(TStorable)
    private
      procedure Empty();
    protected
      FChildren: PThingItem; { Ordered - most recently added first }
      procedure Removed(Thing: TThing); virtual;
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure Add(Thing: TThing; APosition: TThingPosition);
      procedure Add(Thing: TThing; APosition: TThingPosition; Carefully: Boolean; Perspective: TAvatar); // really should call this something else -- it's more than an overloaded Add()
      procedure Remove(Thing: TThing);
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function GetMassManifest(): TThingMassManifest; virtual; { self and children that are not tpScenery }
      function GetOutsideSizeManifest(): TThingSizeManifest; virtual; { self and children that are tpOn, tpCarried; add tpContained children if container is flexible }
      function GetInsideSizeManifest(): TThingSizeManifest; virtual; { only children that are tpContained }
      function GetSurfaceSizeManifest(): TThingSizeManifest; virtual; { children that are tpOn }
      procedure GetAvatars(var List: PAvatarItem; FromOutside: Boolean); virtual;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; virtual;
      function GetName(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetSummaryName(Perspective: TAvatar): AnsiString; virtual;
      function GetLongName(Perspective: TAvatar): AnsiString; virtual; { if you reply to other terms, put as many as possible here; this is shown to disambiguate }
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; virtual;
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
      function GetExamine(Perspective: TAvatar): AnsiString; virtual;
      function GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract;
      function GetBasicDescription(Perspective: TAvatar; Context: TAtom = nil): AnsiString; virtual;
      function GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString; virtual;
      function GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString; virtual;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetDescriptionState(Perspective: TAvatar): AnsiString; virtual; { e.g. 'The bottle is open.' }
      function GetDescriptionHere(Perspective: TAvatar; Context: TAtom = nil): AnsiString; virtual; abstract;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions): AnsiString;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); virtual; abstract; { called by avatar children to trigger DoNavigation correctly }
      procedure AddImplicitlyReferencedThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var List: PThingItem); virtual;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Callback: TReferencedCallback); virtual;
      function StillReferenceable(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; virtual;
      function GetDefaultAtom(): TAtom; virtual;
      function GetInside(var PositionOverride: TThingPosition): TAtom; virtual; { returns nil if there's no inside to speak of }
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; virtual;
      function GetSurface(): TAtom; virtual;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; virtual; abstract;
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; var NotificationListEnd: PPAtomItem): TAtom; virtual; abstract;
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
      function IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingProperties): Boolean; virtual;
      function IsExplicitlyReferenceable(Perspective: TAvatar): Boolean; virtual;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure GetAvatars(var List: PAvatarItem; FromOutside: Boolean); override;
      function GetSurroundingsRoot(out FromOutside: Boolean): TAtom; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function GetIntrinsicMass(): TThingMass; virtual; abstract;
      function GetIntrinsicSize(): TThingSize; virtual; abstract;
      function GetMassManifest(): TThingMassManifest; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; var NotificationListEnd: PPAtomItem): TAtom; override;
      function GetSummaryName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetLongDefiniteName(Perspective: TAvatar): AnsiString; override;
      function IsPlural(Perspective: TAvatar): Boolean; virtual;
      function GetTitle(Perspective: TAvatar): AnsiString; override;
      function GetHorizonDescription(Perspective: TAvatar; Context: TAtom): AnsiString; override;
      function GetDescriptionForHorizon(Perspective: TAvatar; Context: TAtom): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; virtual;
      function GetLookIn(Perspective: TAvatar): AnsiString; virtual;
      function GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetInventory(Perspective: TAvatar): AnsiString; virtual;
      function GetDescriptionHere(Perspective: TAvatar; Context: TAtom = nil): AnsiString; override;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; override;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString; virtual;
      function GetDescriptionEmpty(Perspective: TAvatar): AnsiString; virtual; { only called for optThorough searches }
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; virtual; { used both from inside and outside }
      function GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean): AnsiString;
      function GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionCarriedTitle(Perspective: TAvatar; DeepCarried: Boolean): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; virtual;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      procedure AddImplicitlyReferencedThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var List: PThingItem); override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; virtual; abstract;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Callback: TReferencedCallback); override;
      function StillReferenceable(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
      procedure Moved(OldParent: TAtom; Carefully: Boolean; Perspective: TAvatar); virtual;
      procedure Shake(Perspective: TAvatar); virtual;
      procedure Press(Perspective: TAvatar); virtual;
      function GetProperties(): TThingProperties; virtual;
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
      function IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingProperties): Boolean; override;
    public
      procedure GetAvatars(var List: PAvatarItem; FromOutside: Boolean); override;
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
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; var NotificationListEnd: PPAtomItem): TAtom; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
   end;

   PLocationItem = ^TLocationItem;
   TLocationItem = record
      Next: PLocationItem;
      Value: TLocation;
   end;

   TWorld = class(TStorable)
    protected
      FLocations: PLocationItem;
      FGlobalThings: PThingItem;
      FPlayers: PAvatarItem;
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
   end;

procedure FreeThingList(ThingItem: PThingItem);
procedure FreeAvatarList(AAvatarItem: PAvatarItem);
function MergeAvatarLists(List1, List2: PAvatarItem): PAvatarItem; inline;
procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);

procedure QueueForDisposal(Atom: TAtom);

implementation

uses
   sysutils, broadcast;

procedure FreeThingList(ThingItem: PThingItem);
var
   ThisThingItem: PThingItem;
begin
   while (Assigned(ThingItem)) do
   begin
      ThisThingItem := ThingItem;
      ThingItem := ThingItem^.Next;
      Dispose(ThisThingItem);
   end;
end;

procedure FreeAvatarList(AAvatarItem: PAvatarItem);
var
   ThisAvatarItem: PAvatarItem;
begin
   while (Assigned(AAvatarItem)) do
   begin
      ThisAvatarItem := AAvatarItem;
      AAvatarItem := AAvatarItem^.Next;
      Dispose(ThisAvatarItem);
   end;
end;

function MergeAvatarLists(List1, List2: PAvatarItem): PAvatarItem;
var
   Search, Next: PAvatarItem;
begin
   Result := List1;
   while (Assigned(List2)) do
   begin
      Next := List2^.Next;
      Search := List1;
      while (Assigned(Search) and (Search^.Value <> List2^.Value)) do
         Search := Search^.Next;
      if (Assigned(Search)) then
      begin
         { found duplicate }
         Dispose(List2);
      end
      else
      begin
         { not duplicate; add it on the front (note that only items that were in List1 originally are searched) }
         List2^.Next := Result;
         Result := List2;
      end;
      List2 := Next;
   end;
end;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
var
   Destination: TAtom;
   Message: AnsiString;
   Position: TThingPosition;
   NotificationList, LastNotificationList: PAtomItem;
   NotificationListEnd: PPAtomItem;
begin
   Assert(Assigned(AFrom));
   Assert(Assigned(ATo));
   Assert(Assigned(Perspective));
   Position := tpOn;
   Message := '';
   NotificationList := nil;
   NotificationListEnd := @NotificationList;
   try
      Destination := ATo.GetEntrance(Perspective, AFrom, Perspective, Position, Message, NotificationListEnd);
      if (Assigned(Destination)) then
      begin
         Perspective.AnnounceDeparture(ATo, Direction);
         while (Assigned(NotificationList)) do
         begin
            NotificationList^.Value.HandlePassedThrough(Perspective, AFrom, Destination, Position, Perspective);
            LastNotificationList := NotificationList;
            NotificationList := NotificationList^.Next;
            Dispose(LastNotificationList);
         end;
         Destination.Add(Perspective, Position);
         Perspective.AnnounceArrival(AFrom.GetDefaultAtom(), ReverseCardinalDirection(Direction));
         Perspective.DoLook();
      end
      else
      begin
         Perspective.AvatarMessage('You cannot go ' + CardinalDirectionToString(Direction) + '. ' + Message);
      end;
   finally
      while (Assigned(NotificationList)) do
      begin
         LastNotificationList := NotificationList;
         NotificationList := NotificationList^.Next;
         Dispose(LastNotificationList);
      end;
   end;
end;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);
var
   Destination: TAtom;
   Message: AnsiString;
   Success: Boolean;
   Ancestor: TAtom;
   NotificationList, LastNotificationList: PAtomItem;
   NotificationListEnd: PPAtomItem;
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
      NotificationList := nil;
      NotificationListEnd := @NotificationList;
      try
         Destination := ATo.GetEntrance(Perspective, AFrom, Perspective, Position, Message, NotificationListEnd);
         if (Assigned(Destination)) then
         begin
            Perspective.AnnounceDeparture(ATo);
            while (Assigned(NotificationList)) do
            begin
               NotificationList^.Value.HandlePassedThrough(Perspective, AFrom, Destination, Position, Perspective);
               LastNotificationList := NotificationList;
               NotificationList := NotificationList^.Next;
               Dispose(LastNotificationList);
            end;
            Destination.Add(Perspective, Position);
            Perspective.AnnounceArrival(AFrom.GetDefaultAtom());
            Perspective.DoLook();
         end
         else
         begin
            Perspective.AvatarMessage('You cannot enter ' + ATo.GetDefiniteName(Perspective) + '. ' + Message);
         end;
      finally
         while (Assigned(NotificationList)) do
         begin
            LastNotificationList := NotificationList;
            NotificationList := NotificationList^.Next;
            Dispose(LastNotificationList);
         end;
      end;
   end
   else
      raise EAssertionFailed.Create('unexpected position for navigation: ' + IntToStr(Cardinal(Position)));
end;

var
   DisposalQueue: PAtomItem = nil;

procedure QueueForDisposal(Atom: TAtom);
var
   AtomItem: PAtomItem;
begin
   New(AtomItem);
   AtomItem^.Value := Atom;
   AtomItem^.Next := DisposalQueue;
   DisposalQueue := AtomItem;
end;

procedure EmptyDisposalQueue();
var
   ThisAtomItem: PAtomItem;
begin
   while (Assigned(DisposalQueue)) do
   begin
      ThisAtomItem := DisposalQueue;
      DisposalQueue := DisposalQueue^.Next;
      ThisAtomItem^.Value.Destroy();
      Dispose(ThisAtomItem);
   end;
end;


constructor TAtom.Create();
begin
   inherited;
end;

destructor TAtom.Destroy();
begin
   Empty();
   inherited;
end;

constructor TAtom.Read(Stream: TReadStream);
var
   Child: TThing;
   Last: PPThingItem;
   Current: PThingItem;
begin
   inherited;
   Last := @FChildren;
   Child := Stream.ReadObject() as TThing;
   while (Assigned(Child)) do
   begin
      New(Current);
      Current^.Value := Child;
      Current^.Next := nil;
      Last^ := Current;
      Last := @Current^.Next;
      Child := Stream.ReadObject() as TThing;
   end;
end;

procedure TAtom.Write(Stream: TWriteStream);
var
   Item: PThingItem;
begin
   inherited;
   Item := FChildren;
   while (Assigned(Item)) do
   begin
      Stream.WriteObject(Item^.Value);
      Item := Item^.Next;
   end;
   Stream.WriteObject(nil);
end;

procedure TAtom.Add(Thing: TThing; APosition: TThingPosition);
var
   Item: PThingItem;
   {$IFOPT C+} Position: TThingPosition; {$ENDIF}
begin
   {$IFOPT C+}
     Position := tpIn;
     Assert((APosition <> tpIn) or (GetInside(Position) = Self), 'tried to put something inside something without an inside');
   {$ENDIF}
   if (Assigned(Thing.FParent)) then
      Thing.FParent.Remove(Thing);
   Assert(not Assigned(Thing.FParent));
   New(Item);
   Item^.Next := FChildren;
   Item^.Value := Thing;
   FChildren := Item;
   Thing.FParent := Self;
   Thing.FPosition := APosition;
end;

procedure TAtom.Add(Thing: TThing; APosition: TThingPosition; Carefully: Boolean; Perspective: TAvatar);
var
   OldParent: TAtom;
   {$IFOPT C+} ParentSearch: TAtom; {$ENDIF}
begin
   OldParent := Thing.FParent;
   Add(Thing, APosition);
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

procedure TAtom.Remove(Thing: TThing);
var
   Item: PThingItem;
   Last: ^PThingItem;
begin
   Last := @FChildren;
   Item := FChildren;
   while (Assigned(Item)) do
   begin
      if (Item^.Value = Thing) then
      begin
         Thing.FParent := nil;
         Last^ := Item^.Next;
         Dispose(Item);
         Item := nil;
      end
      else
      begin
         Last := @Item^.Next;
         Item := Item^.Next;
      end;
   end;
   Removed(Thing);
end;

procedure TAtom.Removed(Thing: TThing);
begin
end;

procedure TAtom.Empty();
var
   Item: PThingItem;
begin
   while (Assigned(FChildren)) do
   begin
      Item := FChildren;
      FChildren := FChildren^.Next;
      {$IFOPT C+} Item^.Value.FParent := nil; {$ENDIF} { The ThingItem.Destroy destructor checks this }
      Item^.Value.Destroy();
      Dispose(Item);
   end;
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

function TAtom.GetMassManifest(): TThingMassManifest;
var
   Child: PThingItem;
begin
   Zero(Result);
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (not (Child^.Value.Position in tpScenery)) then
         Result := Result + Child^.Value.GetMassManifest();
      Child := Child^.Next;
   end;
end;

function TAtom.GetOutsideSizeManifest(): TThingSizeManifest;
var
   Child: PThingItem;
begin
   Zero(Result);
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (Child^.Value.Position in [tpOn, tpCarried]) then
         Result := Result + Child^.Value.GetOutsideSizeManifest();
      Child := Child^.Next;
   end;
end;

function TAtom.GetInsideSizeManifest(): TThingSizeManifest;
var
   Child: PThingItem;
begin
   Zero(Result);
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (Child^.Value.Position in (tpContained - tpScenery)) then
         Result := Result + Child^.Value.GetOutsideSizeManifest();
      Child := Child^.Next;
   end;
end;

function TAtom.GetSurfaceSizeManifest(): TThingSizeManifest;
var
   Child: PThingItem;
begin
   Zero(Result);
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (Child^.Value.Position in (tpEverything - (tpContained + tpScenery + [tpCarried]))) then { i.e. tpOn }
         Result := Result + Child^.Value.GetIntrinsicSize();
      Child := Child^.Next;
   end;
end;

procedure TAtom.HandlePassedThrough(Traveller: TThing; AFrom, ATo: TAtom; AToPosition: TThingPosition; Perspective: TAvatar);
begin
end;

procedure TAtom.HandleAdd(Thing: TThing; Blame: TAvatar);
begin
end;

procedure TAtom.GetAvatars(var List: PAvatarItem; FromOutside: Boolean);
var
   Child: PThingItem;
begin
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (FromOutside or (Child^.Value.Position in tpContained)) then
         Child^.Value.GetAvatars(List, True);
      Child := Child^.Next;
   end;
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

function TAtom.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := GetName(Perspective);
   Result := IndefiniteArticle(Result) + ' ' + Result;
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

function TAtom.GetExamine(Perspective: TAvatar): AnsiString;
begin
   Result := GetBasicDescription(Perspective) +
             WithNewlineIfNotEmpty(GetDescriptionOn(Perspective, [optDeepOn, optPrecise])) +
             WithNewlineIfNotEmpty(GetDescriptionChildren(Perspective, [optDeepChildren, optThorough]));
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

   procedure ProcessBatch(Child: PThingItem);
   var
      Mode: TGetPresenceStatementMode;
   begin
      while (Assigned(Child)) do
      begin
         if ((Child^.Value.Position = tpOn) and ((optPrecise in Options) or (Child^.Value <> Perspective))) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            if (optPrecise in Options) then
               Mode := psOnThatThingIsAThing
            else
               Mode := psThereIsAThingHere;
            Result := Result + Prefix + Child^.Value.GetPresenceStatement(Perspective, Mode) + WithSpaceIfNotEmpty(Child^.Value.GetDescriptionState(Perspective));
            if (optDeepOn in Options) then
            begin
               if (Length(Prefix) = 0) then
                  Result := Result + WithNewlineIfNotEmpty(Child^.Value.GetDescriptionOn(Perspective, Options + [optPrecise], Prefix))
               else
                  Result := Result + WithNewlineIfNotEmpty(Child^.Value.GetDescriptionOn(Perspective, Options + [optPrecise], Prefix + '  '));
            end
         end;
         Child := Child^.Next;
      end;
   end;

begin
   Result := '';
   ProcessBatch(FChildren);
   if (GetSurface() <> Self) then
      ProcessBatch(GetSurface().FChildren);
end;

function TAtom.GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
begin
   Result := GetDescriptionChildren(Perspective, Options, '');
end;

function TAtom.GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString;
begin
   Result := '';
end;

function TAtom.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := True;
end;

procedure TAtom.AddImplicitlyReferencedThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var List: PThingItem);
var
   Child: PThingItem;
begin
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (((IncludePerspectiveChildren) or (Child^.Value <> Perspective)) and (IsChildTraversable(Child^.Value, Perspective, FromOutside))) then
         Child^.Value.AddImplicitlyReferencedThings(Perspective, True, IncludePerspectiveChildren, PositionFilter, PropertyFilter, List);
      Child := Child^.Next;
   end;
end;

procedure TAtom.AddExplicitlyReferencedThings(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; FromOutside: Boolean; Callback: TReferencedCallback);
var
   Child: PThingItem;
begin
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (IsChildTraversable(Child^.Value, Perspective, FromOutside)) then
         Child^.Value.AddExplicitlyReferencedThings(Tokens, Start, Perspective, True, Callback);
      Child := Child^.Next;
   end;
end;

function TAtom.StillReferenceable(Thing: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
var
   Child: PThingItem;
begin
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (IsChildTraversable(Child^.Value, Perspective, FromOutside) and Child^.Value.StillReferenceable(Thing, Perspective, True)) then
      begin
         Result := True;
         Exit;
      end;
      Child := Child^.Next;
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


constructor TThing.Create();
begin
   inherited;
end;

destructor TThing.Destroy();
begin
   Assert(not Assigned(FParent));
   inherited;
end;

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

procedure TThing.GetAvatars(var List: PAvatarItem; FromOutside: Boolean);
var
   Child: PThingItem;
begin
   if (FromOutside and (not IsOpen())) then
   begin
      Child := FChildren;
      while (Assigned(Child)) do
      begin
         if (not (Child^.Value.Position in tpContained)) then
            Child^.Value.GetAvatars(List, True);
         Child := Child^.Next;
      end;
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

function TThing.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; var NotificationListEnd: PPAtomItem): TAtom;
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
         New(NotificationListEnd^);
         NotificationListEnd^^.Value := Self;
         NotificationListEnd^^.Next := nil;
         NotificationListEnd := @NotificationListEnd^^.Next;
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
begin
   Result := inherited;
   Assert(Assigned(FParent));
   case FPosition of
    tpAmbiguousPartOfImplicit: Result := Result + ' of ' + FParent.GetSummaryName(Perspective);
   end;
end;

function TThing.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := inherited;
   Assert(Assigned(FParent));
   case FPosition of
    tpAmbiguousPartOfImplicit: Result := Result + ' of ' + FParent.GetDefiniteName(Perspective);
   end;
end;

function TThing.GetLongDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := inherited;
   Assert(Assigned(FParent));
   case FPosition of
    tpPartOfImplicit, tpAmbiguousPartOfImplicit: Result := Result + ' of ' + FParent.GetDefiniteName(Perspective);
    tpOnImplicit: Result := Result + ' on ' + FParent.GetDefiniteName(Perspective);
    tpInImplicit: Result := Result + ' in ' + FParent.GetDefiniteName(Perspective);
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
   Child: PThingItem;
begin
   Result := '';
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      { we exclude context so that, e.g., we don't say "there's a pedestal here!" when you're on it }
      if ((Child^.Value <> Context) and (Child^.Value.Position in tpAutoDescribe)) then
      begin
         if (Length(Result) > 0) then
            Result := Result + ' ';
         Result := Result + Child^.Value.GetPresenceStatement(Perspective, psThereIsAThingHere) + WithSpaceIfNotEmpty(Child^.Value.GetDescriptionState(Perspective));
      end;
      Child := Child^.Next;
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

function TThing.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
begin
   Result := GetDescriptionIn(Perspective, Options, '');
end;

function TThing.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString;

   procedure ProcessBatch(Child: PThingItem; ExpectedPosition: TThingPosition);
   begin
      while (Assigned(Child)) do
      begin
         if (((Child^.Value <> Perspective) or (not (optOmitPerspective in Options))) and (Child^.Value.Position = ExpectedPosition)) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            Result := Result + Prefix + '  ' + Capitalise(Child^.Value.GetIndefiniteName(Perspective)) + '.';
            if (optDeepChildren in Options) then
               Result := Result + WithSpaceIfNotEmpty(Child^.Value.GetDescriptionIn(Perspective, Options - [optFar, optThorough], Prefix + '  '));
         end;
         Child := Child^.Next;
      end;
   end;

var
   Inside, Surface: TAtom;
   ExpectedPosition: TThingPosition;
begin
   Result := '';
   ProcessBatch(FChildren, tpIn);
   Surface := GetSurface();
   if (Surface <> Self) then
      ProcessBatch(Surface.FChildren, tpIn);
   if (optFar in Options) then
   begin
      ExpectedPosition := tpIn;
      Inside := GetInside(ExpectedPosition);
      if (Assigned(Inside) and (((Inside <> Self) and (Inside <> Surface)) or (ExpectedPosition <> tpIn))) then
         ProcessBatch(Inside.FChildren, ExpectedPosition);
   end;
   if (Length(Result) > 0) then
      Result := Prefix + GetDescriptionInTitle(Perspective, Options) + #10 + Result;
end;

function TThing.GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('contains', 'contain', IsPlural(Perspective)) + ':';
end;

function TThing.GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean): AnsiString;
begin
   Result := GetDescriptionCarried(Perspective, DeepCarried, '');
end;

function TThing.GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: AnsiString): AnsiString;

   procedure ProcessBatch(Child: PThingItem);
   begin
      while (Assigned(Child)) do
      begin
         if (Child^.Value.Position = tpCarried) then
         begin
            if (Length(Result) > 0) then
               Result := Result + #10;
            Result := Result + Prefix + '  ' + Capitalise(Child^.Value.GetIndefiniteName(Perspective)) + '.';
            if (DeepCarried) then
               Result := Result + WithNewlineIfNotEmpty(Child^.Value.GetDescriptionOn(Perspective, [optDeepOn, optPrecise], Prefix + '  ')) +
                                  WithNewlineIfNotEmpty(Child^.Value.GetDescriptionChildren(Perspective, [optDeepChildren], Prefix + '  '));
         end;
         Child := Child^.Next;
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

function TThing.IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingProperties): Boolean;
begin
   Result := PropertyFilter <= GetProperties();
end;

function TThing.IsExplicitlyReferenceable(Perspective: TAvatar): Boolean;
begin
   Result := True;
end;

procedure TThing.AddImplicitlyReferencedThings(Perspective: TAvatar; FromOutside: Boolean; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var List: PThingItem);
var
   Item: PThingItem;
begin
   inherited;
   Assert(Assigned(FParent));
   if ((FPosition in PositionFilter) and IsImplicitlyReferenceable(Perspective, PropertyFilter)) then
   begin
      New(Item);
      Item^.Value := Self;
      Item^.Next := List;
      List := Item;
   end;
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

function TThing.GetProperties(): TThingProperties;
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
             'Position: ' + ThingPositionToString(FPosition) + ' ' + FParent.GetName(nil) + #10 +
             'GetIntrinsicSize(): ' + AnsiString(GetIntrinsicSize()) + #10 +
             'GetSurfaceSizeManifest(): ' + AnsiString(GetSurfaceSizeManifest()) + #10;
end;
{$ENDIF}


function TAvatar.IsImplicitlyReferenceable(Perspective: TAvatar; PropertyFilter: TThingProperties): Boolean;
begin
   if (Perspective = Self) then
      Result := False
   else
      Result := inherited;
end;

procedure TAvatar.GetAvatars(var List: PAvatarItem; FromOutside: Boolean);
var
   Item: PAvatarItem;
begin
   New(Item);
   Item^.Value := Self;
   Item^.Next := List;
   List := Item;
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

   procedure ProcessBatch(Child: PThingItem);
   begin
      while (Assigned(Child)) do
      begin
         if ((Child^.Value.Position in tpAutoDescribe) and
             (Child^.Value <> Perspective) and
             (Child^.Value <> FNorth) and
             (Child^.Value <> FNorthEast) and
             (Child^.Value <> FEast) and
             (Child^.Value <> FSouthEast) and
             (Child^.Value <> FSouth) and
             (Child^.Value <> FSouthWest) and
             (Child^.Value <> FWest) and
             (Child^.Value <> FNorthWest) and
             (Child^.Value <> FUp) and
             (Child^.Value <> FDown) and
             (Child^.Value <> Context)) then
            ProcessThing(Child^.Value, Child^.Value.GetPresenceStatement(Perspective, psThereIsAThingHere));
         Child := Child^.Next;
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
            Result := Result + (Atom as TLocation).GetDescriptionRemoteBrief(Perspective, Direction);
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

function TLocation.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString; var NotificationListEnd: PPAtomItem): TAtom;
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
end;

destructor TWorld.Destroy();
var
   Item: PLocationItem;
begin
   while (Assigned(FLocations)) do
   begin
      Item := FLocations;
      FLocations := FLocations^.Next;
      Item^.Value.Destroy();
      Dispose(Item);
   end;
   FreeAvatarList(FPlayers);
   inherited;
end;

constructor TWorld.Read(Stream: TReadStream);
var
   Location: TLocation;
   LocationItem: PLocationItem;
   Thing: TThing;
   ThingItem: PThingItem;
   AvatarItem: PAvatarItem;
begin
   inherited;
   Location := Stream.ReadObject() as TLocation;
   while (Assigned(Location)) do
   begin
      New(LocationItem);
      LocationItem^.Value := Location;
      LocationItem^.Next := FLocations;
      FLocations := LocationItem;
      Location := Stream.ReadObject() as TLocation;
   end;
   Thing := Stream.ReadObject() as TThing;
   while (Assigned(Thing)) do
   begin
      New(ThingItem);
      ThingItem^.Value := Thing;
      ThingItem^.Next := FGlobalThings;
      FGlobalThings := ThingItem;
      Thing := Stream.ReadObject() as TThing;
   end;
   repeat
      New(AvatarItem);
      AvatarItem^.Next := FPlayers;
      FPlayers := AvatarItem;
   until not Stream.ReadReference(@AvatarItem^.Value);
   FPlayers := AvatarItem^.Next;
   Dispose(AvatarItem);
end;

procedure TWorld.Write(Stream: TWriteStream);
var
   LocationItem: PLocationItem;
   ThingItem: PThingItem;
   AvatarItem: PAvatarItem;
begin
   inherited;
   { Locations }
   LocationItem := FLocations;
   while (Assigned(LocationItem)) do
   begin
      Stream.WriteObject(LocationItem^.Value);
      LocationItem := LocationItem^.Next;
   end;
   Stream.WriteObject(nil);
   { Things }
   ThingItem := FGlobalThings;
   while (Assigned(ThingItem)) do
   begin
      Stream.WriteObject(ThingItem^.Value);
      ThingItem := ThingItem^.Next;
   end;
   Stream.WriteObject(nil);
   { Players }
   AvatarItem := FPlayers;
   while (Assigned(AvatarItem)) do
   begin
      Stream.WriteReference(AvatarItem^.Value);
      AvatarItem := AvatarItem^.Next;
   end;
   Stream.WriteReference(nil);
end;

procedure TWorld.AddLocation(Location: TLocation);
var
   Item: PLocationItem;
begin
   New(Item);
   Item^.Next := FLocations;
   Item^.Value := Location;
   FLocations := Item;
end;

procedure TWorld.AddGlobalThing(GlobalThing: TThing);
var
   Item: PThingItem;
begin
   New(Item);
   Item^.Next := FGlobalThings;
   Item^.Value := GlobalThing;
   FGlobalThings := Item;
end;

procedure TWorld.AddPlayer(Avatar: TAvatar);
var
   Item: PAvatarItem;
begin
   Assert(Assigned(Avatar.Parent));
   New(Item);
   Item^.Next := FPlayers;
   Item^.Value := Avatar;
   FPlayers := Item;
end;

function TWorld.GetPlayer(Name: AnsiString): TAvatar;
var
   Item: PAvatarItem;
begin
   Item := FPlayers;
   Name := LowerCase(Name);
   while ((Assigned(Item)) and (LowerCase(Item^.Value.GetUsername()) <> Name)) do
      Item := Item^.Next;
   if (Assigned(Item)) then
      Result := Item^.Value
   else
      Result := nil;
end;

function TWorld.GetPlayerCount(): Cardinal;
var
   Item: PAvatarItem;
begin
   Result := 0;
   Item := FPlayers;
   while (Assigned(Item)) do
   begin
      Inc(Result);
      Item := Item^.Next;
   end;
end;

procedure TWorld.CheckForDisconnectedPlayers();
var
   Item: PAvatarItem;
   Last: PPAvatarItem;
begin
   Item := FPlayers;
   Last := @FPlayers;
   while (Assigned(Item)) do
   begin
      Assert(Assigned(Item^.Value));
      if ((not Item^.Value.HasConnectedPlayer()) and (Item^.Value.IsReadyForRemoval())) then
      begin
         Last^ := Item^.Next;
         Item^.Value.RemoveFromWorld();
         Item^.Value.Destroy();
         Dispose(Item);
         Item := Last^;
      end
      else
         Item := Item^.Next;
   end;
end;

procedure TWorld.CheckDisposalQueue();
begin
   EmptyDisposalQueue();
end;

end.
