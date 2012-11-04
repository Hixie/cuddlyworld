{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit things;

interface

uses
   storable, world, thingdim, grammarian, matcher;

type
   TNamedThing = class(TThing)
    protected
      FName, FLongName: AnsiString;
      FSingularPattern, FPluralPattern: TMatcher;
      FPlural: Boolean;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetSummaryName(Perspective: TAvatar): AnsiString; override;
      function GetLongName(Perspective: TAvatar): AnsiString; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; override;
      {$IFDEF DEBUG} function Debug(): AnsiString; override; {$ENDIF}
      property LongName: AnsiString read FLongName write FLongName;
      property Plural: Boolean read FPlural write FPlural;
   end;

   TStaticThing = class(TNamedThing) { "static" in the sense of unchanging }
    protected
      FDescription: AnsiString;
      FMass: TThingMass;
      FSize: TThingSize;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
   end;

   TFeature = class(TStaticThing)
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString);
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
   end;

   TScenery = class(TStaticThing)
    protected
      FUnderDescription: AnsiString;
      FFindDescription: AnsiString;
      FCannotMoveExcuse: AnsiString;
      FOpened: Boolean;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function IsOpen(): Boolean; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      property UnderDescription: AnsiString read FUnderDescription write FUnderDescription;
      property FindDescription: AnsiString read FFindDescription write FFindDescription;
      property CannotMoveExcuse: AnsiString read FCannotMoveExcuse write FCannotMoveExcuse;
      property Opened: Boolean read FOpened write FOpened;
   end;

   TLocationProxy = class(TScenery)
    protected
      FDestination: TLocation;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Destination: TLocation; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString; override;
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: AnsiString; NotificationList: TAtomList): TAtom; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function IsOpen(): Boolean; override;
   end;

   TOpening = class(TLocationProxy)
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Destination: TLocation; Size: TThingSize);
      function GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: AnsiString; NotificationList: TAtomList): TAtom; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanShake(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetFeatures(): TThingFeatures; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
   end;

   TSurface = class(TStaticThing)
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      function GetDefaultAtom(): TAtom; override;
   end;

   THole = class;

   TEarthGround = class(TSurface)
    protected
      FHole: THole;
      {$IFOPT C+} procedure AssertChildPositionOk(Thing: TThing; APosition: TThingPosition); override; {$ENDIF}
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetLookIn(Perspective: TAvatar): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function GetFeatures(): TThingFeatures; override;
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; override;
      procedure Removed(Thing: TThing); override;
   end;

   TContainer = class(TStaticThing)
     public
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function IsOpen(): Boolean; override;
      function GetFeatures(): TThingFeatures; override;
   end;

   TSpade = class(TStaticThing)
    public
      constructor Create();
      function GetFeatures(): TThingFeatures; override;
      function CanDig(Target: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
   end;

   TBag = class(TNamedThing)
    protected
      FDescription: AnsiString;
      FMaxSize: TThingSize;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; MaxSize: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function IsOpen(): Boolean; override;
      function GetFeatures(): TThingFeatures; override;
   end;

   TPileClass = class of TPile;

   THole = class(TNamedThing)
    protected
      FDescription: AnsiString;
      FSize: TThingSize;
      FPileClass: TPileClass;
    public
      constructor Create(Description: AnsiString; Size: TThingSize; PileClass: TPileClass);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetTitle(Perspective: TAvatar): AnsiString; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionState(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanShake(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      function IsOpen(): Boolean; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      function GetBiggestCoverer: TThing; { only call if IsOpen() is false meaning there is a coverer in the first place }
      function GetFeatures(): TThingFeatures; override;
   end;

   TPile = class(TNamedThing)
    protected
      FIngredient: AnsiString;
      FDescription: AnsiString;
      FMass: TThingMass;
      FSize: TThingSize;
    public
      constructor Create(SingularIngredients: array of AnsiString; PluralIngredients: array of AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize); { ingredient must be canonical plural form }
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; override;
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString; override;
      function GetDescriptionEmpty(Perspective: TAvatar): AnsiString; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function IsOpen(): Boolean; override;
   end;

   TEarthPile = class(TPile)
    public
      constructor Create(Size: TThingSize);
   end;

   TSign = class(TScenery)
    protected
      FWriting: AnsiString;
    public
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Writing: AnsiString; Mass: TThingMass; Size: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetFeatures(): TThingFeatures; override;
      function GetDescriptionWriting(Perspective: TAvatar): AnsiString; override;
   end;

   TTree = class(TStaticThing)
   end;

implementation

uses
   sysutils, broadcast;

constructor TNamedThing.Create(Name: AnsiString; Pattern: AnsiString);
var
   TokenisedName: TTokens;
   NameIsSingular: Boolean;
   {$IFOPT C+} NameIsPlural: Boolean; {$ENDIF}
begin
   inherited Create();
   FName := Name;
   Assert(HasSingularVsPluralAnnotation(Pattern), 'The ' + Name + ' needs explicit singular and plural patterns (no slashes found in "' + Pattern + '").');
   CompilePattern(Pattern, FSingularPattern, FPluralPattern);
   TokenisedName := TokeniseCanonically(Name);
   NameIsSingular := FSingularPattern.Matches(TokenisedName, 0) = Length(TokenisedName);
   {$IFOPT C+}
     NameIsPlural := FPluralPattern.Matches(TokenisedName, 0) = Length(TokenisedName);
     if ((not NameIsSingular) and (not NameIsPlural)) then
        Assert(False, 'Default name ("' + Name + '") does not match given pattern ("' + Pattern + '")');
   {$ENDIF}
   FPlural := not NameIsSingular;
   if (FPlural) then
      FLongName := FPluralPattern.GetCanonicalMatch(' ')
   else
      FLongName := FSingularPattern.GetCanonicalMatch(' ');
end;

destructor TNamedThing.Destroy();
begin
   FSingularPattern.Free();
   FPluralPattern.Free();
   inherited;
end;

constructor TNamedThing.Read(Stream: TReadStream);
begin
   inherited;
   FName := Stream.ReadAnsiString();
   FLongName := Stream.ReadAnsiString();
   FSingularPattern := Stream.ReadObject() as TMatcher;
   FPluralPattern := Stream.ReadObject() as TMatcher;
   FPlural := Stream.ReadBoolean();
end;

procedure TNamedThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FName);
   Stream.WriteAnsiString(FLongName);
   Stream.WriteObject(FSingularPattern);
   Stream.WriteObject(FPluralPattern);
   Stream.WriteBoolean(FPlural);
end;

function TNamedThing.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := FName;
end;

function TNamedThing.GetSummaryName(Perspective: TAvatar): AnsiString;
begin
   Result := FName;
   Assert(Assigned(FParent));
   case FPosition of
    tpAmbiguousPartOfImplicit: Result := Result + ' of ' + FParent.GetSummaryName(Perspective);
   end;
end;

function TNamedThing.GetLongName(Perspective: TAvatar): AnsiString;
begin
   Result := FLongName;
end;

function TNamedThing.IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean;
var
   PossibleCount: Cardinal;
begin
   Result := False;
   GrammaticalNumber := [];
   Count := FSingularPattern.Matches(Tokens, Start);
   if (Count > 0) then
   begin
      Result := True;
      GrammaticalNumber := [gnSingular];
   end;
   PossibleCount := FPluralPattern.Matches(Tokens, Start);
   if ((PossibleCount > 0) and (PossibleCount >= Count)) then
   begin
      if (Count = PossibleCount) then
      begin
         GrammaticalNumber := gnAmbiguous;
      end
      else
      begin
         Result := True;
         Count := PossibleCount;
         GrammaticalNumber := [gnPlural];
      end;
   end;
end;

function TNamedThing.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := FPlural;
end;

{$IFDEF DEBUG}
function TNamedThing.Debug(): AnsiString;
begin
   Result := inherited;
   Result := Result + #10 +
             'Singular pattern as dot file: ' + #10 + FSingularPattern.GetPatternDotFileLabels();
end;
{$ENDIF}


constructor TStaticThing.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
begin
   inherited Create(Name, Pattern);
   FDescription := Description;
   FMass := Mass;
   FSize := Size;
end;

constructor TStaticThing.Read(Stream: TReadStream);
begin
   inherited;
   FDescription := Stream.ReadAnsiString();
   FMass := TThingMass(Stream.ReadCardinal());
   FSize := TThingSize(Stream.ReadCardinal());
end;

procedure TStaticThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FDescription);
   Stream.WriteCardinal(Cardinal(FMass));
   Stream.WriteCardinal(Cardinal(FSize));
end;

function TStaticThing.GetIntrinsicMass(): TThingMass;
begin
   Result := FMass;
end;

function TStaticThing.GetIntrinsicSize(): TThingSize;
begin
   Result := FSize;
end;

function TStaticThing.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;


constructor TFeature.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString);
begin
   inherited Create(Name, Pattern, Description, tmLudicrous, tsLudicrous);
end;

function TFeature.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.';
end;


constructor TScenery.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   { needed for default values }
   inherited;
end;

constructor TScenery.Read(Stream: TReadStream);
begin
   inherited;
   FUnderDescription := Stream.ReadAnsiString();
   FFindDescription := Stream.ReadAnsiString();
   FCannotMoveExcuse := Stream.ReadAnsiString();
   FOpened := Stream.ReadBoolean();
end;

procedure TScenery.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FUnderDescription);
   Stream.WriteAnsiString(FFindDescription);
   Stream.WriteAnsiString(FCannotMoveExcuse);
   Stream.WriteBoolean(FOpened);
end;

function TScenery.IsOpen(): Boolean;
begin
   Result := FOpened;
end;

function TScenery.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   if (Length(FCannotMoveExcuse) > 0) then
      Message := FCannotMoveExcuse
   else
   if (FPosition = tpPartOfImplicit) then
      Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' part of ' + FParent.GetDefiniteName(Perspective) + '.'
   else
   if (FPosition in tpOpening) then
      Message := 'Moving ' + GetIndefiniteName(Perspective) + ' doesn''t even make sense.'
   else
      Message := Capitalise(GetDefiniteName(Perspective)) + ' cannot be moved.';
end;

function TScenery.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString;
begin
   if ((Mode = psTheThingIsOnThatThing) and (Length(FFindDescription) > 0)) then
      Result := FFindDescription
   else
      Result := inherited;
end;

function TScenery.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   if (FPosition in tpScenery) then
   begin
      if (Length(FUnderDescription) > 0) then
         Result := FUnderDescription
      else
         Result := 'You cannot see under ' + GetDefiniteName(Perspective) + '.';
   end
   else
      Result := inherited;
end;


constructor TLocationProxy.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Destination: TLocation; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   Assert(Assigned(Destination));
   inherited Create(Name, Pattern, Description, tmLudicrous, tsLudicrous);
   FDestination := Destination;
end;

constructor TLocationProxy.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FDestination));
end;

procedure TLocationProxy.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FDestination);
end;

function TLocationProxy.GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): AnsiString;
begin
   Result := Capitalise(GetIndefiniteName(Perspective)) + ' ' + TernaryConditional('leads', 'lead', IsPlural(Perspective)) + ' ' + CardinalDirectionToString(Direction) + '.';
end;

function TLocationProxy.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: AnsiString; NotificationList: TAtomList): TAtom;
begin
   DisambiguationOpening := Self;       
   Result := inherited;
end;

function TLocationProxy.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Assert(Assigned(FDestination));
   Result := FDestination.GetInside(PositionOverride);
   if (not Assigned(Result)) then
   begin
      Result := FDestination.GetSurface();
      PositionOverride := tpOn;
   end;
end;

function TLocationProxy.IsOpen(): Boolean;
begin
   Result := True;
end;


constructor TOpening.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Destination: TLocation; Size: TThingSize);
begin
   inherited Create(Name, Pattern, Description, Destination, tmLudicrous, Size);
end;

function TOpening.GetEntrance(Traveller: TThing; AFrom: TAtom; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: AnsiString; NotificationList: TAtomList): TAtom;
begin
   Assert(Assigned(Traveller));
   if (not CanInsideHold(Traveller.GetOutsideSizeManifest())) then
   begin
      Result := nil;
      Message := Traveller.GetDefiniteName(Perspective) + ' ' + IsAre(Traveller.IsPlural(Perspective)) + ' too big to fit in ' + GetDefiniteName(Perspective) + '.';
   end
   else
      Result := inherited; // defers to GetInside(); TLocation.GetInside() gets the FDestination surface
end;

function TOpening.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := GetLookIn(Perspective);
end;

function TOpening.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := Capitalise(Perspective.GetDefiniteName(Perspective) + ' cannot move ' + GetIndefiniteName(Perspective) + '. That does not make sense.');
end;

function TOpening.CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := Capitalise(Perspective.GetDefiniteName(Perspective) + ' cannot physically pick up ' + GetIndefiniteName(Perspective) + '. That is ludicrous.');
end;

function TOpening.CanShake(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := Capitalise(Perspective.GetDefiniteName(Perspective) + ' cannot shake ' + GetIndefiniteName(Perspective) + '. That is just silly.');
end;

function TOpening.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfCanHaveThingsPushedOn, tfCanHaveThingsPushedIn];
end;

function TOpening.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;


constructor TSurface.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   { needed for default values }
   inherited;
end;

function TSurface.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Assert(Assigned(FParent));
   Result := False;
   Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' part of ' + FParent.GetDefiniteName(Perspective) + '.';
end;

procedure TSurface.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
begin
   // always defer to parent, so that "get out" doesn't try to get out of the TSurface and into the TLocation!
   Assert(Assigned(FParent));
   FParent.Navigate(Direction, Perspective);
end;

function TSurface.GetDefaultAtom(): TAtom;
begin
   Assert(Assigned(FParent));
   Result := FParent;
end;


constructor TEarthGround.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FHole));
end;

procedure TEarthGround.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FHole);
end;

{$IFOPT C+}
procedure TEarthGround.AssertChildPositionOk(Thing: TThing; APosition: TThingPosition);
begin
   if (Thing <> FHole) then
   begin
      Assert(not (APosition in tpOpening));
   end
   else
   begin
      Assert(Assigned(FHole));
      Assert(APosition in tpOpening);
   end;
   inherited;
end;
{$ENDIF}

function TEarthGround.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   // we intentionally override seeing the ground if there's a hole, because we want to see what's in the hole
   if ((Direction = cdDown) and (Assigned(FHole)) and (FHole.IsOpen())) then
      Result := FHole.GetDescriptionRemoteDetailed(Perspective, Direction)
   else
      Result := inherited;
end;

function TEarthGround.GetLookIn(Perspective: TAvatar): AnsiString;
var
   Contents: AnsiString;
begin
   if (Assigned(FHole) and (FHole.IsOpen())) then
   begin
      Result := FHole.GetDescriptionRemoteDetailed(Perspective, cdIn);
      Contents := FHole.GetDescriptionIn(Perspective, []);
      if (Length(Contents) > 0) then
         Result := Result + #10 + Contents;
   end
   else
      Result := 'To look in ' + GetDefiniteName(Perspective) + ', you first need to dig a hole in it.';
end;

function TEarthGround.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   if (Assigned(FHole) and (FHole.IsOpen())) then
      Result := 'If the hole is any guide, there is a lot of earth under ' + GetDefiniteName(Perspective) + '.'
   else
      Result := 'To look under ' + GetDefiniteName(Perspective) + ', you first need to get below it.';
end;

function TEarthGround.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfDiggable];
end;

function TEarthGround.Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean;
const
   Size = tsGigantic;
var
   Pile: TPile;
   E: TThingEnumerator;
begin
   if (Assigned(FHole)) then
   begin
      Result := False;
      if (FHole.IsOpen()) then
      begin
         Message := 'There is already a hole here.';
      end
      else
      begin
         Message := 'There''s not really much room here to dig a hole. In particular, ' + FHole.GetBiggestCoverer().GetDefiniteName(Perspective) + ' takes up a lot of room.';
      end;
   end
   else
   begin
      Pile := TEarthPile.Create(Size);
      E := FChildren.GetEnumerator();
      try
         while (E.MoveNext()) do
         begin
            if (E.Current.Position in tpContained) then
            begin
               Assert(not (E.Current is TEarthPile));
               Pile.Add(E, tpEmbedded);
            end;
         end;
      finally
         E.Free();
      end;
      FHole := THole.Create('The hole is quite dirty.', Size, TEarthPile);
      Add(FHole, tpSurfaceOpening);
      Add(Pile, tpOn);
      Result := True;
      Message := 'With much effort, ' + Perspective.GetDefiniteName(Perspective) + ' dig a huge hole.';
   end;
end;

function TEarthGround.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   if (Assigned(FHole) and (FHole.IsOpen())) then { if it's not open then the hole isn't visible, so we pretend it's not there }
      Result := FHole
   else
      Result := Self;
end;

function TEarthGround.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   if (Assigned(FHole)) then
      Result := FHole.CanInsideHold(Manifest)
   else
      Result := False;
end;

function TEarthGround.GetDescriptionClosed(Perspective: TAvatar): AnsiString;
begin
   Result := 'To put things in ' + GetDefiniteName(Perspective) + ', you''ll have to dig a hole to put them in.';
end;

procedure TEarthGround.Removed(Thing: TThing);
begin
   if (Thing = FHole) then
      FHole := nil;
end;


function TContainer.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Result := Self;
end;

function TContainer.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function TContainer.IsOpen(): Boolean;
begin
   Result := True;
end;

function TContainer.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfCanHaveThingsPushedIn];
end;


constructor TSpade.Create();
begin
   inherited Create('spade', '(metal (spade/spades shovel/shovels)@)&', 'The spade is a small handheld tool apparently shaped from a single piece of metal.', tmLight, tsSmall);
end;

function TSpade.GetFeatures(): TThingFeatures;
begin
   Result := inherited GetFeatures() + [tfCanDig];
end;

function TSpade.CanDig(Target: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := True;
end;


constructor TBag.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; MaxSize: TThingSize);
begin
   inherited Create(Name, Pattern);
   FDescription := Description;
   FMaxSize := MaxSize;
end;

constructor TBag.Read(Stream: TReadStream);
begin
   inherited;
   FDescription := Stream.ReadAnsiString();
   FMaxSize := TThingSize(Stream.ReadCardinal());
end;

procedure TBag.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FDescription);
   Stream.WriteCardinal(Cardinal(FMaxSize));
end;

function TBag.GetIntrinsicMass(): TThingMass;
begin
   Result := tmLight;
end;

function TBag.GetIntrinsicSize(): TThingSize;
begin
   Result := tsSmall;
end;

function TBag.GetOutsideSizeManifest(): TThingSizeManifest;
begin
   Result := inherited GetOutsideSizeManifest() + GetInsideSizeManifest();
end;

function TBag.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;

function TBag.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Result := Self;
end;

function TBag.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FMaxSize;
end;

function TBag.IsOpen(): Boolean;
begin
   Result := True;
end;

function TBag.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfCanHaveThingsPushedIn];
end;


constructor THole.Create(Description: AnsiString; Size: TThingSize; PileClass: TPileClass);
begin
   inherited Create('hole', 'hole/holes');
   FDescription := Description;
   FSize := Size;
   FPileClass := PileClass;
end;

constructor THole.Read(Stream: TReadStream);
var
   PileClass: TClass;
begin
   inherited;
   FDescription := Stream.ReadAnsiString();
   FSize := TThingSize(Stream.ReadCardinal());
   {$IFOPT C-} {$HINT This could be optimised further in non-debug builds.} {$ENDIF}
   PileClass := Stream.ReadClass();
   Assert((PileClass = TPile) or (PileClass.InheritsFrom(TPile)));
   FPileClass := TPileClass(PileClass);
end;

procedure THole.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FDescription);
   Stream.WriteCardinal(Cardinal(FSize));
   Stream.WriteClass(FPileClass);
end;

function THole.GetTitle(Perspective: TAvatar): AnsiString;
begin
   Result := 'hole in ' + FParent.GetDefiniteName(Perspective);
end;

function THole.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString;
var
   Child: TThing;
begin
   // This is a little dubious, IMHO
   Result := '';
   for Child in FChildren do
   begin
      if (Child.Position = tpOn) then
      begin
         if (Mode = psTheThingIsOnThatThing) then
         begin
            Result := 'There ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' under ' + Child.GetDefiniteName(Perspective) + { ', ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + } '.';
            Exit;
         end
         else
         if (Mode = psOnThatSpecialThing) then
         begin
            Result := 'under ' + Child.GetDefiniteName(Perspective);
            Exit;
         end
         else
         begin
            if (Length(Result) > 0) then
               Result := Result + ' ';
            Result := Result + Child.GetPresenceStatement(Perspective, psThereIsAThingHere);
         end;
      end;
   end;
   if (Length(Result) = 0) then
      Result := inherited;
end;

function THole.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;

function THole.GetDescriptionState(Perspective: TAvatar): AnsiString;
var
   Child: TThing;
   ContentsSize: TThingSizeManifest;
   OverflowedChildren: TThingList;
begin
   Result := '';
   if (IsOpen()) then
   begin
      Zero(ContentsSize);
      OverflowedChildren := nil;
      try
         for Child in FChildren do
         begin
            if (Child.Position = tpIn) then
            begin
               ContentsSize := ContentsSize + Child.GetOutsideSizeManifest();
               if (ContentsSize > FSize) then
               begin
                  if (not Assigned(OverflowedChildren)) then
                     OverflowedChildren := TThingList.Create();
                  OverflowedChildren.AppendItem(Child);
               end;
            end;
         end;
         if (Assigned(OverflowedChildren)) then
         begin
            Assert(OverflowedChildren.Length > 0);
            Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' full to overflowing; ' +
                      'at the top of it ' + IsAre((OverflowedChildren.Length > 1) or (OverflowedChildren.First.IsPlural(Perspective))) + ' ' +
                      OverflowedChildren.GetIndefiniteString(Perspective, 'and') + '.';
         end;
      finally
         if (Assigned(OverflowedChildren)) then // not strictly necessary
            OverflowedChildren.Free();
      end;
   end;
end;

function THole.GetDescriptionClosed(Perspective: TAvatar): AnsiString;
var
   Cover: TThing;
begin
   Assert(not IsOpen());
   Cover := GetBiggestCoverer();
   Assert(Assigned(Cover));
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' covered by ' + Cover.GetIndefiniteName(Perspective) + '.';
end;

function THole.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := 'To look under ' + GetDefiniteName(Perspective) + ', you first need to get below it.';
end;

function THole.GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   if (Direction = cdUp) then
   begin
      Assert(Assigned(FParent));
      Result := FParent.GetLookTowardsDirection(Perspective, Direction);
   end
   else
      Result := inherited;
end;

function THole.GetIntrinsicMass(): TThingMass;
begin
   Result := tmLudicrous;
end;

function THole.GetIntrinsicSize(): TThingSize;
begin
   Result := FSize;
end;

function THole.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Result := Self;
end;

function THole.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function THole.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := Capitalise(Perspective.GetDefiniteName(Perspective) + ' cannot move ' + GetIndefiniteName(Perspective) + '. That does not make sense. Maybe try filling this hole in and digging a new one in the new location.');
end;

function THole.CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := Capitalise(Perspective.GetDefiniteName(Perspective) + ' cannot physically pick up ' + GetIndefiniteName(Perspective) + '. That is ludicrous.');
end;

function THole.CanShake(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := Capitalise(Perspective.GetDefiniteName(Perspective) + ' cannot shake ' + GetIndefiniteName(Perspective) + '. That is just silly.');
end;

function THole.CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   if (ThingPosition = tpOn) then
   begin
      if (IsOpen()) then
      begin
         { things will either fall in or cover the hole }
         Result := True;
      end
      else
      begin
         { can't cover hole if it's already covered }
         Result := False;
         Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' already covered.';
      end;
   end
   else
   if ((Thing is FPileClass) and (ThingPosition = tpIn)) then
   begin
      { hole will be filled }
      if (GetInsideSizeManifest() < FSize) then
      begin
         Result := True;
      end
      else
      begin
         Result := False;
         Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' already full to overflowing.';
      end;
   end
   else
   begin
      Result := inherited;
   end;
end;

procedure THole.HandleAdd(Thing: TThing; Blame: TAvatar);
var
   Child: TThing;
   E: TThingEnumerator;
   PileSize: TThingSizeManifest;
   OldParent: TAtom;
   OldThing: TThing;
begin
   inherited;
   if (Thing is FPileClass) then
   begin
      { First, see if the pile was put on top of an overflowing hole. If so, then we just leave it there and the hole is covered by the pile. Otherwise, drop it in. }
      if (Thing.Position = tpOn) then
      begin
         if (GetInsideSizeManifest() < FSize) then
            Thing.Position := tpIn;
      end;
      { Now, see if the hole has enough pile _in_ it to fill it. }
      if (Thing.Position = tpIn) then
      begin
         Zero(PileSize);
         for Child in FChildren do
            if ((Child is FPileClass) and (Child.Position = tpIn)) then
               PileSize := PileSize + Child.GetOutsideSizeManifest();
         if (PileSize >= FSize) then
         begin
            // { Check all descendants for TAvatars, to avoid burying them alive }
            // for Child in Descendants do
            //    if Child is TAvatar do
            //       DoNavigation(Avatar.FParent, FParent.GetDefaultAtom(), cdOut, Avatar);
            { Fill hole and bury treasure }
            DoBroadcast([FParent], nil, [C(M(@Blame.GetDefiniteName)), SP, MP(Blame, M('fills'), M('fill')), SP, M(@GetDefiniteName), M(' with '), M(@Thing.GetDefiniteName), M('.')]);
            OldParent := FParent;
            FParent.Remove(Self); { have to do this first so that the Surface switches to using itself as an inside }
            Zero(PileSize);
            E := FChildren.GetEnumerator();
            try
               while (E.MoveNext()) do
               begin
                  OldThing := E.Current;
                  Assert(not (OldThing is TAvatar));
                  { first check a few edge cases }
                  if (OldThing is FPileClass) then
                  begin
                     { set it up to be removed below }
                     OldThing.Position := tpAt;
                  end
                  else
                  if (OldThing.Position = tpIn) then
                  begin
                     { make sure we're not burrying too much (currently this won't trigger, since you can't fill an overfilled hole) }
                     PileSize := PileSize + OldThing.GetOutsideSizeManifest();
                     Assert(PileSize <= FSize);
                     if (PileSize > FSize) then
                        OldThing.Position := tpOn;
                  end;
                  { now move or remove the items in question }
                  if (OldThing.Position in [tpIn, tpOn]) then
                  begin
                     { keep these in play }
                     OldParent.Add(E, OldThing.Position);
                  end
                  else
                  begin
                     { dump these }
                     Remove(E);
                     OldThing.Free(); // strictly speaking should be Destroy
                  end;
               end;
               Assert(FChildren.Length = 0);
            finally
               E.Free();
            end;
            // XXX announce the hole is filled
            QueueForDisposal(Self);
         end;
      end;
   end
   else
   if ((Thing.Position = tpOn) and (Thing.GetOutsideSizeManifest() < FSize)) then
   begin
      Thing.Position := tpIn;
      if (GetInsideSizeManifest() < FSize) then
         DoBroadcast([Self], nil, [C(M(@Thing.GetDefiniteName)), SP, MP(Self, M('falls'), M('fall')), M(' into '), M(@GetDefiniteName), M('.')]);
      { else well the hole is full and this is just piling on top of the hole,
        so though we pretend like it's in the hole, it's not like it fell in }
   end; { else it covers the hole }
end;

function THole.IsOpen(): Boolean;
var
   Child: TThing;
begin
   for Child in FChildren do
   begin
      if (Child.Position = tpOn) then
      begin
         Result := False;
         Exit;
      end;
   end;
   Result := True;
end;

procedure THole.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
begin
   Assert(FParent is TEarthGround);
   case Direction of
     cdUp, cdOut: DoNavigation(Self, FParent.GetDefaultAtom(), cdUp, Perspective);
    else
      Perspective.AvatarMessage('You''re in a hole.');
   end;
end;

function THole.GetBiggestCoverer: TThing;
var
   Child: TThing;
begin
   Assert(not IsOpen());
   Result := nil;
   for Child in FChildren do
      if ((Child.Position = tpOn) and ((not (Assigned(Result))) or (Result.GetOutsideSizeManifest() < Child.GetOutsideSizeManifest()))) then
         Result := Child;
   Assert(Assigned(Result));
end;

function THole.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfCanHaveThingsPushedOn, tfCanHaveThingsPushedIn];
end;


constructor TPile.Create(SingularIngredients: array of AnsiString; PluralIngredients: array of AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
var
   Index: Cardinal;
   Pattern: AnsiString;
begin
   Assert(Length(SingularIngredients) > 0);
   Assert(Length(PluralIngredients) = Length(SingularIngredients));
   Pattern := '((pile/piles (of (' + PluralIngredients[0];
   if (Length(PluralIngredients) > 1) then
      for Index := 1 to High(PluralIngredients) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         Pattern := Pattern + ' ' + PluralIngredients[Index];
   Pattern := Pattern + ')@)?)';
   for Index := Low(SingularIngredients) to High(SingularIngredients) do
   begin
      {$IFDEF DEBUG} Assert(not HasPatternChars(SingularIngredients[Index])); {$ENDIF}
      {$IFDEF DEBUG} Assert(not HasPatternChars(PluralIngredients[Index])); {$ENDIF}
      Pattern := Pattern + ' ' + SingularIngredients[Index] + '/' + PluralIngredients[Index];
   end;
   Pattern := Pattern + ')@';
   inherited Create('pile of ' + PluralIngredients[0], Pattern);
   FIngredient := PluralIngredients[0];
   FDescription := Description;
   FMass := Mass;
   FSize := Size;
end;

constructor TPile.Read(Stream: TReadStream);
begin
   inherited;
   FIngredient := Stream.ReadAnsiString();
   FDescription := Stream.ReadAnsiString();
   FMass := TThingMass(Stream.ReadCardinal());
   FSize := TThingSize(Stream.ReadCardinal());
end;

procedure TPile.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FIngredient);
   Stream.WriteAnsiString(FDescription);
   Stream.WriteCardinal(Cardinal(TThingMass(FMass)));
   Stream.WriteCardinal(Cardinal(TThingSize(FSize)));
end;

function TPile.GetIntrinsicMass(): TThingMass;
begin
   Result := FMass;
end;

function TPile.GetIntrinsicSize(): TThingSize;
begin
   Result := FSize;
end;

function TPile.GetOutsideSizeManifest(): TThingSizeManifest;
begin
   Result := inherited GetOutsideSizeManifest() + GetInsideSizeManifest();
end;

function TPile.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := 'Digging through ' + GetDefiniteName(Perspective) + ' to the bottom, you find ' + FParent.GetIndefiniteName(Perspective) + '.';
end;

function TPile.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;

function TPile.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString;
begin
   if (optThorough in Options) then
      Result := inherited
   else
      Result := '';
end;

function TPile.GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
begin
   if (optThorough in Options) then
      Result := 'A thorough search through ' + GetDefiniteName(Perspective) + ' reveals:'
   else
      Result := 'Scatterered amongst the ' + FIngredient + ' one can see:';
end;

function TPile.GetDescriptionEmpty(Perspective: TAvatar): AnsiString;
begin
   Result := 'A thorough search through ' + GetDefiniteName(Perspective) + ' reveals only a lot of ' + FIngredient + '.';
end;

function TPile.CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('slips', 'slip', IsPlural(Perspective)) + ' through your fingers.';
   Result := False;
end;

function TPile.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Result := Self;
end;

function TPile.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function TPile.IsOpen(): Boolean;
begin
   Result := True;
end;


constructor TEarthPile.Create(Size: TThingSize);
begin
   inherited Create(['earth', 'dirt', 'soil'], ['earth', 'dirt', 'soil'], 'The pile of earth is quite dirty.', kDensityMap[tdLow, Size], Size);
end;


constructor TSign.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Writing: AnsiString; Mass: TThingMass; Size: TThingSize);
begin
   inherited Create(Name, Pattern, Description, Mass, Size);
   FWriting := Writing;
end;

constructor TSign.Read(Stream: TReadStream);
begin
   inherited;
   FWriting := Stream.ReadAnsiString();
end;

procedure TSign.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FWriting);
end;

function TSign.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfExaminingReads];
end;

function TSign.GetDescriptionWriting(Perspective: TAvatar): AnsiString;
begin
   Result := 'On ' + GetDefiniteName(Perspective) + ' is written "' + FWriting + '".';
end;


initialization
   RegisterStorableClass(TNamedThing,             1000);
   RegisterStorableClass(TStaticThing,            1001);
   RegisterStorableClass(TFeature,                1002);
   RegisterStorableClass(TScenery,                1003);
   RegisterStorableClass(TLocationProxy,          1004);
   RegisterStorableClass(TSurface,                1005);
   RegisterStorableClass(TEarthGround,            1006);
   RegisterStorableClass(TContainer,              1008);
   RegisterStorableClass(TOpening,                1009);
   RegisterStorableClass(TSpade,                  1010);
   RegisterStorableClass(TBag,                    1011);
   RegisterStorableClass(THole,                   1012);
   RegisterStorableClass(TPile,                   1013);
   RegisterStorableClass(TEarthPile,              1014);
   RegisterStorableClass(TSign,                   1015);
   RegisterStorableClass(TTree,                   1016);
end.