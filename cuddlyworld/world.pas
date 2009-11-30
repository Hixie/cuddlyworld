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

   TTokens = array of AnsiString;

   TThingProperty = (tpDiggable, tpCanDig,
                     tpCanHaveThingsPushedOn, { e.g. it has a ramp, or a surface flush with its container -- holes can have things pushed onto them }
                     tpCanHaveThingsPushedIn); { e.g. it has a lip flush with its container -- holes can have things pushed into them }
   TThingProperties = set of TThingProperty;

   TGetDescriptionOnOptions = set of (optDeepOn, optPrecise);
   TGetDescriptionChildrenOptions = set of (optDeepChildren, optFar, optThorough, optOmitPerspective); { deep = bag and inside bag; far = door and inside door }
   TGetPresenceStatementMode = (psThereIsAThingHere { look }, psOnThatThingIsAThing { nested look }, psTheThingIsOnThatThing { find });

   PAtom = ^TAtom;
   TAtom = class(TStorable)
    private
      procedure Empty();
    protected
      FChildren: PThingItem;
      procedure Removed(Thing: TThing); virtual;
      function AreChildrenExplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean; virtual; { "take camp" }
      function AreChildrenImplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean; virtual; { "take all" }
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
      function GetOutsideSizeManifest(): TThingSizeManifest; virtual; { self and children that are tpOn, tpCarried; add tpIn children if container is flexible }
      function GetInsideSizeManifest(): TThingSizeManifest; virtual; { only children that are tpIn }
      function GetSurfaceSizeManifest(): TThingSizeManifest; virtual; { children that are tpOn }
      procedure HandleAdd(Thing: TThing); virtual; { use this to fumble things or to cause things to fall off other things (and make CanPut() always allow tpOn in that case) }
      function GetAvatars(Perspective: TAvatar): PAvatarItem; virtual;
      function GetName(Perspective: TAvatar): AnsiString; virtual; abstract; { if you ever return more than one word here, override IsMatchingWord() also }
      function GetDefiniteName(Perspective: TAvatar): AnsiString; virtual;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; virtual;
      function GetTitle(Perspective: TAvatar): AnsiString; virtual;
      function GetContext(Perspective: TAvatar): AnsiString; virtual;
      function GetLook(Perspective: TAvatar): AnsiString; virtual;
      function GetLookAt(Perspective: TAvatar): AnsiString; virtual;
      function GetExamine(Perspective: TAvatar): AnsiString; virtual;
      function GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract;
      function GetBasicDescription(Perspective: TAvatar): AnsiString; virtual;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetDescriptionState(Perspective: TAvatar): AnsiString; virtual; { e.g. 'The bottle is open.' }
      function GetDescriptionHere(Perspective: TAvatar): AnsiString; virtual; abstract;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions): AnsiString;
      function GetDescriptionOn(Perspective: TAvatar; Options: TGetDescriptionOnOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual; abstract;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); virtual; abstract;
      procedure AddImplicitlyReferencedThings(Perspective: TAvatar; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var ListEnd: PPThingItem); virtual;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar; var ListEnd: PPThingItem); virtual;
      function StillReferenceable(Thing: TThing; Perspective: TAvatar): Boolean; virtual;
      function GetDefaultAtom(): TAtom; virtual;
      function GetInside(var PositionOverride: TThingPosition): TAtom; virtual; { returns nil if there's no inside to speak of }
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; virtual;
      function GetSurface(): TAtom; virtual;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; virtual; abstract;
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString): TAtom; virtual; abstract;
   end;

   PThing = ^TThing;
   TThing = class(TAtom)
    protected
      FParent: TAtom;
      FPosition: TThingPosition;
      function IsExplicitlyReferenceable(Perspective: TAvatar): Boolean; virtual; { "take camp" }
      function IsImplicitlyReferenceable(Perspective: TAvatar): Boolean; virtual; { "take all" }
      function AreChildrenExplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean; override;
      function AreChildrenImplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean; override;
      function AreMatchingWords(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar): Boolean; virtual;
      function IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean; virtual;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function GetIntrinsicMass(): TThingMass; virtual; abstract;
      function GetIntrinsicSize(): TThingSize; virtual; abstract;
      function GetMassManifest(): TThingMassManifest; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function CanSurfaceHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString): TAtom; override;
      function IsPlural(Perspective: TAvatar): Boolean; virtual;
      function GetTitle(Perspective: TAvatar): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; virtual;
      function GetLookIn(Perspective: TAvatar): AnsiString; virtual;
      function GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetInventory(Perspective: TAvatar): AnsiString; virtual;
      function GetDescriptionHere(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; override;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString; virtual;
      function GetDescriptionEmpty(Perspective: TAvatar): AnsiString; virtual; { only called for optThorough searches }
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; virtual;
      function GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean): AnsiString;
      function GetDescriptionCarried(Perspective: TAvatar; DeepCarried: Boolean; Prefix: AnsiString): AnsiString; virtual;
      function GetDescriptionCarriedTitle(Perspective: TAvatar; DeepCarried: Boolean): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; virtual;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      procedure AddImplicitlyReferencedThings(Perspective: TAvatar; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var ListEnd: PPThingItem); override;
      procedure AddExplicitlyReferencedThings(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar; var ListEnd: PPThingItem); override;
      function StillReferenceable(Thing: TThing; Perspective: TAvatar): Boolean; override;
      procedure Moved(OldParent: TAtom; Carefully: Boolean; Perspective: TAvatar); virtual;
//      procedure Shake(Perspective: TAvatar); virtual;
      function GetProperties(): TThingProperties; virtual;
      function CanDig(Target: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; virtual;
      procedure Dug(Target: TThing; Perspective: TAvatar; var Message: AnsiString); virtual;
      function IsOpen(): Boolean; virtual;
      property Parent: TAtom read FParent;
      property Position: TThingPosition read FPosition write FPosition;
   end;
 
   { Thing that can move of its own volition }
   TAvatar = class(TThing)
    public
      procedure DoLook(); virtual; abstract;
      procedure AvatarMessage(Message: AnsiString); virtual; abstract;
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
      function GetDescriptionHere(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionRemoteBrief(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; virtual;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      procedure FailNavigation(Direction: TCardinalDirection; Perspective: TAvatar); { also called when trying to dig in and push something in this direction }
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString): TAtom; override;
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
      procedure AddPlayer(Avatar: TAvatar); { these must have a location as an ancestor }
      function GetPlayer(Name: AnsiString): TAvatar;
      procedure CheckForDisconnectedPlayers();
      procedure CheckDisposalQueue();
   end;

procedure FreeThingList(ThingItem: PThingItem);
procedure FreeAvatarList(AAvatarItem: PAvatarItem);
function MergeAvatarLists(List1, List2: PAvatarItem): PAvatarItem;
procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Direction: TCardinalDirection; Perspective: TAvatar);
procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);

procedure QueueForDisposal(Atom: TAtom);

implementation

uses
   sysutils;

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
begin
   Assert(Assigned(AFrom));
   Assert(Assigned(ATo));
   Assert(Assigned(Perspective));
   Position := tpOn;
   Message := '';
   Destination := ATo.GetEntrance(Perspective, AFrom, Perspective, Position, Message);
   if (Assigned(Destination)) then
   begin
      Perspective.AnnounceDeparture(ATo, Direction);
      AFrom.Remove(Perspective);
      Destination.Add(Perspective, Position);
      Perspective.AnnounceArrival(AFrom.GetDefaultAtom(), ReverseCardinalDirection(Direction));
      Perspective.DoLook();
   end
   else
   begin
      Perspective.AvatarMessage('You cannot go ' + CardinalDirectionToString(Direction) + '. ' + Message);
   end;
end;

procedure DoNavigation(AFrom: TAtom; ATo: TAtom; Position: TThingPosition; Perspective: TAvatar);
var
   Destination: TAtom;
   Message: AnsiString;
   Success: Boolean;
   Ancestor: TAtom;
begin
   Assert(Assigned(AFrom));
   Assert(Assigned(ATo));
   Assert(Assigned(Perspective));
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
      Destination := ATo.GetEntrance(Perspective, AFrom, Perspective, Position, Message);
      if (Assigned(Destination)) then
      begin
         Perspective.AnnounceDeparture(ATo);
         AFrom.Remove(Perspective);
         Destination.Add(Perspective, Position);
         Perspective.AnnounceArrival(AFrom.GetDefaultAtom());
         Perspective.DoLook();
      end
      else
      begin
         Perspective.AvatarMessage('You cannot enter ' + ATo.GetDefiniteName(Perspective) + '. ' + Message);
      end;
   end
   else
      raise EAssertionFailed.Create('unexpected position for navigation: ' + IntToStr(Cardinal(Position)));
      // tpCarried would be for "jump into player's arms"? :-)
end;

const
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
   Item: PThingItem;
begin
   inherited;
   Child := Stream.ReadObject() as TThing;
   while (Assigned(Child)) do
   begin
      New(Item);
      Item^.Value := Child;
      Item^.Next := FChildren;
      FChildren := Item;
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
   {$IFOPT C+} Position := tpIn; {$ENDIF}
   Assert((APosition <> tpIn) or (GetInside(Position) = Self), 'tried to put something inside something without an inside');
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
   HandleAdd(Thing);
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
      if (Child^.Value.Position in [tpIn]) then
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
      if (Child^.Value.Position in [tpOn]) then
         Result := Result + Child^.Value.GetIntrinsicSize();
      Child := Child^.Next;
   end;
end;

procedure TAtom.HandleAdd(Thing: TThing);
begin
end;

function TAtom.GetAvatars(Perspective: TAvatar): PAvatarItem;
var
   Item: PAvatarItem;
   Child: PThingItem;
begin
   Result := nil;
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if ((Child^.Value <> Perspective) and (Child^.Value is TAvatar)) then
      begin
         New(Item);
         Item^.Next := Result;
         Item^.Value := Child^.Value as TAvatar;
         Result := Item;
      end;
      Child := Child^.Next;
   end;
   if ((Self <> Perspective) and (Self is TAvatar)) then
   begin
      New(Item);
      Item^.Next := Result;
      Item^.Value := Self as TAvatar;
      Result := Item;
   end;
end;

function TAtom.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := 'the ' + GetName(Perspective);
end;

function TAtom.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := GetName(Perspective);
   Result := IndefiniteArticle(Result) + ' ' + Result;
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
      Atom := Thing.FParent;
      Position := Thing.FPosition;
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

function TAtom.GetBasicDescription(Perspective: TAvatar): AnsiString;
begin
   Result := GetDescriptionSelf(Perspective) +
             WithSpaceIfNotEmpty(GetDescriptionState(Perspective)) +
             WithSpaceIfNotEmpty(GetDescriptionHere(Perspective));
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

function TAtom.AreChildrenExplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean;
begin
   Result := True;
end;

function TAtom.AreChildrenImplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean;
begin
   Result := True;
end;

procedure TAtom.AddImplicitlyReferencedThings(Perspective: TAvatar; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var ListEnd: PPThingItem);
var
   Child: PThingItem;
   LocalFilter: TThingPositionFilter;
begin
   LocalFilter := tpEverything;
   if (AreChildrenImplicitlyReferenceable(Perspective, LocalFilter)) then
   begin
      Child := FChildren;
      while (Assigned(Child)) do
      begin
         if ((Child^.Value.Position in LocalFilter) and ((IncludePerspectiveChildren) or (Child^.Value <> Perspective))) then
         begin
            Child^.Value.AddImplicitlyReferencedThings(Perspective, IncludePerspectiveChildren, PositionFilter, PropertyFilter, ListEnd);
         end;
         Child := Child^.Next;
      end;
   end;
end;

procedure TAtom.AddExplicitlyReferencedThings(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar; var ListEnd: PPThingItem);
var
   Child: PThingItem;
   LocalFilter: TThingPositionFilter;
begin
   LocalFilter := tpEverything;
   if (AreChildrenExplicitlyReferenceable(Perspective, LocalFilter)) then
   begin
      Child := FChildren;
      while (Assigned(Child)) do
      begin
         if (Child^.Value.Position in LocalFilter) then
            Child^.Value.AddExplicitlyReferencedThings(Tokens, Start, Count, Perspective, ListEnd);
         Child := Child^.Next;
      end;
   end;
end;

function TAtom.StillReferenceable(Thing: TThing; Perspective: TAvatar): Boolean;
var
   Child: PThingItem;
   LocalFilter: TThingPositionFilter;
begin
   LocalFilter := tpEverything;
   if (AreChildrenExplicitlyReferenceable(Perspective, LocalFilter)) then
   begin
      Child := FChildren;
      while (Assigned(Child)) do
      begin
         if (Child^.Value.Position in LocalFilter) then
         begin
            if (Child^.Value.StillReferenceable(Thing, Perspective)) then
            begin
               Result := True;
               Exit;
            end;
         end;
         Child := Child^.Next;
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

function TThing.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString): TAtom;
begin
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
         Result := Result.GetEntrance(Traveller, AFrom, Perspective, PositionOverride, Message);
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

function TThing.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
    Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.';
end;

function TThing.GetLookDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   Assert(Direction <> cdIn);
   Assert(Perspective.Parent = Self);
   if (Direction = cdOut) then
      Result := FParent.GetDescriptionRemoteDetailed(Perspective, Direction)
   else
      Result := 'You are ' + ThingPositionToString(Perspective.Position) + ' ' + GetDefiniteName(Perspective) + '.';
end;

function TThing.GetLookIn(Perspective: TAvatar): AnsiString;
var
   PositionOverride: TThingPosition;
   Inside: TAtom;
   Contents: AnsiString;
begin
   if (IsOpen() or ((Perspective.Parent = Self) and (Perspective.Position = tpIn))) then
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

function TThing.GetDescriptionHere(Perspective: TAvatar): AnsiString;
var
   Child: PThingItem;
begin
   Result := '';
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (Child^.Value.Position = tpAt) then
         Result := Result + Child^.Value.GetPresenceStatement(Perspective, psThereIsAThingHere) + WithSpaceIfNotEmpty(Child^.Value.GetDescriptionState(Perspective));
      Child := Child^.Next;
   end;
end;

function TThing.GetDescriptionChildren(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString;
var
   Additional: AnsiString;
begin
   if (IsOpen() or ((Perspective.Parent = Self) and (Perspective.Position = tpIn))) then
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
begin
   Assert(Perspective.Parent = Self);
   if (Direction > cdLastPhysical) then
      DoNavigation(Self, FParent, FPosition, Perspective)
   else
      Perspective.AvatarMessage('You cannot go ' + CardinalDirectionToString(Direction) + '; you''re ' + ThingPositionToString(Perspective.FPosition) + ' ' + GetDefiniteName(Perspective) + '.');
end;

function TThing.IsExplicitlyReferenceable(Perspective: TAvatar): Boolean;
begin
   Result := True;
end;

function TThing.IsImplicitlyReferenceable(Perspective: TAvatar): Boolean;
begin
   Result := True;
end;

function TThing.AreChildrenExplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean;
begin
   if (not IsOpen()) then
      PositionFilter := PositionFilter - [tpIn];
   Result := inherited;
end;

function TThing.AreChildrenImplicitlyReferenceable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean;
begin
   if (not IsOpen()) then
      PositionFilter := PositionFilter - [tpIn];
   Result := inherited;
end;

procedure TThing.AddImplicitlyReferencedThings(Perspective: TAvatar; IncludePerspectiveChildren: Boolean; PositionFilter: TThingPositionFilter; PropertyFilter: TThingProperties; var ListEnd: PPThingItem);
var
   Item: PThingItem;
begin
   inherited;
   if ((FPosition in PositionFilter) and (PropertyFilter <= GetProperties()) and (Self <> Perspective) and IsImplicitlyReferenceable(Perspective)) then
   begin
      New(Item);
      Item^.Next := nil;
      Item^.Value := Self;
      ListEnd^ := Item;
      ListEnd := @Item^.Next;
   end;
end;

procedure TThing.AddExplicitlyReferencedThings(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar; var ListEnd: PPThingItem);
var
   Item: PThingItem;
begin
   inherited;
   if (IsExplicitlyReferenceable(Perspective) and AreMatchingWords(Tokens, Start, Count, Perspective)) then
   begin
      New(Item);
      Item^.Next := nil;
      Item^.Value := Self;
      ListEnd^ := Item;
      ListEnd := @Item^.Next;
   end;
end;

function TThing.StillReferenceable(Thing: TThing; Perspective: TAvatar): Boolean;
begin
   if (Thing = Self) then
      Result := True
   else
      Result := inherited;
end;

function TThing.AreMatchingWords(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar): Boolean;
var
   Index: Cardinal;
begin
   Result := True;
   { No overflow possible on the next line because there's no way for Start and Count to point beyond the end of Tokens }
   for Index := Start to Start+Count-1 do
   begin
      if (not IsMatchingWord(Tokens[Index], Perspective)) then
      begin
         Result := False;
         Exit;
      end;
   end;
end;

function TThing.IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean;
begin
   Assert(Pos(' ', GetName(Perspective)) = 0);
   Result := Word = LowerCase(GetName(Perspective));
end;

procedure TThing.Moved(OldParent: TAtom; Carefully: Boolean; Perspective: TAvatar);
var
   Avatars, LastAvatar: PAvatarItem;
begin
   Assert(Assigned(FParent));
   Avatars := MergeAvatarLists(FParent.GetAvatars(Perspective), OldParent.GetAvatars(Perspective));
   while (Assigned(Avatars)) do
   begin
      Avatars^.Value.AvatarMessage(Capitalise(Perspective.GetDefiniteName(Avatars^.Value)) + ' moves ' + GetDefiniteName(Avatars^.Value) + '.'); // should be more specific about where things are going, e.g. 'takes x', 'drops x', 'puts x on y', 'moves x around' (if oldparent=newparent)
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
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
   Message := 'You cannot dig here.';
   Result := False;
end;

procedure TThing.Dug(Target: TThing; Perspective: TAvatar; var Message: AnsiString);
begin
end;

function TThing.IsOpen(): Boolean;
begin
   Result := False;
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

function TLocation.GetDescriptionHere(Perspective: TAvatar): AnsiString;

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
         if ((Child^.Value.Position = tpAt) and
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
             (Child^.Value <> FDown)) then
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
      if (Atom is TThing) then
      begin
         Thing := Atom as TThing;
         if (Thing.Position = tpAt) then
            ProcessThing(Thing, Capitalise(CardinalDirectionToDirectionString(Direction)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Perspective)) + ' ' + Thing.GetIndefiniteName(Perspective) + '.');
      end
      else
      if (Atom is TLocation) then
      begin
         if (Length(Result) > 0) then
            Result := Result + ' ';
         Result := Result + (Atom as TLocation).GetDescriptionRemoteBrief(Perspective, Direction);
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

function TLocation.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var Message: AnsiString): TAtom;
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
   New(Item);
   Item^.Next := FPlayers;
   Item^.Value := Avatar;
   FPlayers := Item;
   Assert(Assigned(FLocations));
   FLocations^.Value.GetSurface().Add(Avatar, tpOn);
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

initialization
   RegisterStorableClass(TWorld, 1);
end.
