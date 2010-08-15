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
      constructor Create(Name: AnsiString);
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
      constructor Create(Name: AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
   end;

   TScenery = class(TStaticThing)
    protected
      FUnderDescription: AnsiString;
      FFindDescription: AnsiString;
      FCannotMoveExcuse: AnsiString;
    public
      constructor Create(Name: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      property UnderDescription: AnsiString read FUnderDescription write FUnderDescription;
      property FindDescription: AnsiString read FFindDescription write FFindDescription;
      property CannotMoveExcuse: AnsiString read FCannotMoveExcuse write FCannotMoveExcuse;
   end;

   TLocationProxy = class(TScenery)
    protected
      FDestination: TLocation;
    public
      constructor Create(Name: AnsiString; Description: AnsiString; Destination: TLocation; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Destination: TLocation; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
   end;

   THole = class;

   TSurface = class(TStaticThing)
    protected
      FHole: THole;
    public
      constructor Create(Name: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetLookIn(Perspective: TAvatar): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      function GetDefaultAtom(): TAtom; override;
      function GetFeatures(): TThingFeatures; override;
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; override;
      procedure Removed(Thing: TThing); override;
   end;

   TDistantScenery = class(TNamedThing)
    protected
      FDirection: TCardinalDirection;
      function FarAway(Perspective: TAvatar): AnsiString; virtual;
    public
      constructor Create(Name: AnsiString; Direction: TCardinalDirection);
      constructor Create(Name: AnsiString; Pattern: AnsiString; Direction: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
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
      constructor Create(Name: AnsiString; Description: AnsiString; MaxSize: TThingSize);
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
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      function IsOpen(): Boolean; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      function GetBiggestCoverer: TThing;
      function GetFeatures(): TThingFeatures; override;
   end;

   TPile = class(TNamedThing)
    protected
      FIngredient: AnsiString;
      FDescription: AnsiString;
      FMass: TThingMass;
      FSize: TThingSize;
    public
      constructor Create(Ingredient: AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
      constructor Create(Ingredients: array of AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
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

implementation

uses
   sysutils, broadcast;

constructor TNamedThing.Create(Name: AnsiString);
begin
   {$IFDEF DEBUG} Assert(not HasPatternChars(Name)); {$ENDIF}
   Create(Name, Name);
end;

constructor TNamedThing.Create(Name: AnsiString; Pattern: AnsiString);
var
   TokenisedName: TTokens;
   NameIsSingular: Boolean;
   {$IFOPT C+} NameIsPlural: Boolean; {$ENDIF}
begin
   inherited Create();
   FName := Name;
   CompilePattern(Pattern, FSingularPattern, FPluralPattern);
   TokenisedName := TokeniseCanonically(Name);
   NameIsSingular := FSingularPattern.Matches(TokenisedName, 0) = Length(TokenisedName);
   {$IFOPT C+}
     NameIsPlural := FPluralPattern.Matches(TokenisedName, 0) = Length(TokenisedName);
     if ((not NameIsSingular) and (not NameIsPlural)) then
        raise EAssertionFailed.Create('Default name ("' + Name + '") does not match given pattern ("' + Pattern + '")');
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


constructor TStaticThing.Create(Name: AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
begin
   {$IFDEF DEBUG} Assert(not HasPatternChars(Name)); {$ENDIF}
   Create(Name, Name, Description, Mass, Size);
end;

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


constructor TScenery.Create(Name: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   { needed for default values }
   inherited;
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
end;

procedure TScenery.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FUnderDescription);
   Stream.WriteAnsiString(FFindDescription);
   Stream.WriteAnsiString(FCannotMoveExcuse);
end;

function TScenery.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   if (Length(FCannotMoveExcuse) > 0) then
      Message := FCannotMoveExcuse
   else
   if (FPosition = tpPartOfImplicit) then
      Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' part of ' + FParent.GetDefiniteName(Perspective) + '.'
   else
   if (FPosition = tpOpening) then
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
      inherited;
end;


constructor TLocationProxy.Create(Name: AnsiString; Description: AnsiString; Destination: TLocation; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   inherited Create(Name, Description, tmLudicrous, tsLudicrous);
   FDestination := Destination;
end;

constructor TLocationProxy.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Destination: TLocation; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   inherited Create(Name, Pattern, Description, tmLudicrous, tsLudicrous);
   FDestination := Destination;
end;

constructor TLocationProxy.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@FDestination);
end;

procedure TLocationProxy.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FDestination);
end;

function TLocationProxy.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Result := FDestination.GetInside(PositionOverride);
   if (not Assigned(Result)) then
   begin
      Result := FDestination.GetSurface();
      PositionOverride := tpOn;
   end;
end;


constructor TSurface.Create(Name: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   { needed for default values }
   inherited;
end;

constructor TSurface.Create(Name: AnsiString; Pattern: AnsiString; Description: AnsiString; Mass: TThingMass = tmLudicrous; Size: TThingSize = tsLudicrous);
begin
   { needed for default values }
   inherited;
end;

constructor TSurface.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@FHole);
end;

procedure TSurface.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FHole);
end;

function TSurface.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Assert(Assigned(FParent));
   Result := False;
   Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' part of ' + FParent.GetDefiniteName(Perspective) + '.';
end;

procedure TSurface.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
begin
   Assert(Assigned(FParent));
   FParent.Navigate(Direction, Perspective);
end;

function TSurface.GetDefaultAtom(): TAtom;
begin
   Assert(Assigned(FParent));
   Result := FParent;
end;

function TSurface.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString;
begin
   if ((Direction = cdDown) and (Assigned(FHole)) and (FHole.IsOpen())) then
      Result := FHole.GetDescriptionRemoteDetailed(Perspective, Direction)
   else
      Result := inherited;
end;

function TSurface.GetLookIn(Perspective: TAvatar): AnsiString;
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

function TSurface.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   if (Assigned(FHole) and (FHole.IsOpen())) then
      Result := 'If the hole is any guide, there is a lot of earth under ' + GetDefiniteName(Perspective) + '.'
   else
      Result := 'To look under ' + GetDefiniteName(Perspective) + ', you first need to get below it.';
end;

function TSurface.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfDiggable];
end;

function TSurface.Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean;
const
   Size = tsGigantic;
var
   Pile: TPile;
   Child: PThingItem;
   LastThing: TThing;
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
      Child := FChildren;
      while (Assigned(Child)) do
      begin
         { This is a little hairy, because we're walking the FChildren list just ahead of where we are mutating it }
         LastThing := Child^.Value;
         Child := Child^.Next;
         if (LastThing.Position in tpContained) then
         begin
            Assert(not (LastThing is TEarthPile));
            Pile.Add(LastThing, tpEmbedded);
         end;
      end;
      Add(Pile, tpOn);
      FHole := THole.Create('The hole is quite dirty.', Size, TEarthPile);
      Add(FHole, tpOpening);
      Result := True;
      Message := 'With much effort, you dig a huge hole.';
   end;
end;

function TSurface.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   if (Assigned(FHole) and (FHole.IsOpen())) then { if it's not open then the hole isn't visible, so we pretend it's not there }
      Result := FHole
   else
      Result := Self;
end;

function TSurface.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   if (Assigned(FHole)) then
      Result := FHole.CanInsideHold(Manifest)
   else
      Result := False;
end;

function TSurface.GetDescriptionClosed(Perspective: TAvatar): AnsiString;
begin
   Result := 'To put things in ' + GetDefiniteName(Perspective) + ', you''ll have to dig a hole to put them in.';
end;

procedure TSurface.Removed(Thing: TThing);
begin
   if (Thing = FHole) then
      FHole := nil;
end;


constructor TDistantScenery.Create(Name: AnsiString; Direction: TCardinalDirection);
begin
   inherited Create(Name);
   FDirection := Direction;
end;

constructor TDistantScenery.Create(Name: AnsiString; Pattern: AnsiString; Direction: TCardinalDirection);
begin
   inherited Create(Name, Pattern);
   FDirection := Direction;
end;

constructor TDistantScenery.Read(Stream: TReadStream);
begin
   inherited;
   FDirection := TCardinalDirection(Stream.ReadCardinal());
end;

procedure TDistantScenery.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FDirection));
end;

function TDistantScenery.GetIntrinsicMass(): TThingMass;
begin
   Result := tmLudicrous;
end;

function TDistantScenery.GetIntrinsicSize(): TThingSize;
begin
   Result := tsLudicrous;
end;

function TDistantScenery.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := FarAway(Perspective);
end;

function TDistantScenery.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FarAway(Perspective);
end;

function TDistantScenery.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString;
begin
   if (Mode = psThereIsAThingHere) then
      Result := Capitalise(CardinalDirectionToDirectionString(FDirection)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + '.'
   else
      Result := FarAway(Perspective);
end;

function TDistantScenery.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := FarAway(Perspective);
end;

function TDistantScenery.FarAway(Perspective: TAvatar): AnsiString;
begin
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + CardinalDirectionToDirectionString(FDirection) + '.';
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


constructor TBag.Create(Name: AnsiString; Description: AnsiString; MaxSize: TThingSize);
begin
   {$IFDEF DEBUG} Assert(not HasPatternChars(Name)); {$ENDIF}
   Create(Name, Name, Description, MaxSize);
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
   Child: PThingItem;
begin
   Result := '';
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (Child^.Value.Position = tpOn) then
      begin
         if (Mode = psTheThingIsOnThatThing) then
         begin
            Result := Result + 'There ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' under ' + Child^.Value.GetDefiniteName(Perspective) + { ', ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + } '.';
            Exit;
         end
         else
         begin
            if (Length(Result) > 0) then
               Result := Result + ' ';
            Result := Result + Child^.Value.GetPresenceStatement(Perspective, psThereIsAThingHere);
         end;
      end;
      Child := Child^.Next;
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
   Child, Previous, Next: PThingItem;
   ContentsSize: TThingSizeManifest;
   Count: Cardinal;
   HaveMultipleOverflowing: Boolean;
   Result1, Result2, Result3: AnsiString;
begin
   Result := '';
   if (IsOpen()) then
   begin
      { Reverse the child list so that the next algorithm can skip the earliest-added things (which are usually at the end of the list) }
      Next := FChildren;
      FChildren := nil;
      while (Assigned(Next)) do
      begin
         Previous := FChildren;
         FChildren := Next;
         Next := FChildren^.Next;
         FChildren^.Next := Previous;
      end;
      try
         Count := 0;
         Result1 := '';
         Result2 := '';
         Result3 := '';
         HaveMultipleOverflowing := False;
         Zero(ContentsSize);
         Child := FChildren;
         while (Assigned(Child)) do
         begin
            if (Child^.Value.Position = tpIn) then
            begin
               ContentsSize := ContentsSize + Child^.Value.GetOutsideSizeManifest();
               if (ContentsSize > FSize) then
               begin
                  Count := Count + 1;
                  case Count of
                    1: Result1 := Child^.Value.GetIndefiniteName(Perspective);
                    2: Result2 := Child^.Value.GetIndefiniteName(Perspective);
                    3: Result3 := Child^.Value.GetIndefiniteName(Perspective) + ', ';
                   else
                      Result3 := Child^.Value.GetIndefiniteName(Perspective) + ', ' + Result3;
                  end;
                  HaveMultipleOverflowing := (Count > 1) or Child^.Value.IsPlural(Perspective);
               end;
            end;
            Child := Child^.Next;
         end;
         if (Count > 0) then
         begin
            Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' full to overflowing; at the top of it ' + TernaryConditional('is', 'are', HaveMultipleOverflowing) + ' ';
            case Count of
              1: Result := Result + Result1;
              2: Result := Result + Result2 + ' and ' + Result1;
             else
                Result := Result + Result3 + Result2 + ', and ' + Result1;
            end;
            Result := Result + '.';
         end;
      finally
         { Put the list back in the original order }
         { We don't do this at the same time as the above algorithm so that exceptions don't leave the list in an unstable order }
         { (the reversal itself is exception-safe) }
         Next := FChildren;
         FChildren := nil;
         while (Assigned(Next)) do
         begin
            Previous := FChildren;
            FChildren := Next;
            Next := FChildren^.Next;
            FChildren^.Next := Previous;
         end;
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
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' covered by ' + Cover.GetIndefiniteName(Perspective) + '.';
end;

function THole.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := 'To look under ' + GetDefiniteName(Perspective) + ', you first need to get below it.';
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

function THole.CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := 'Taking a hole is a ludicrous concept.';
end;

function THole.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
   Message := 'You can''t move a hole. That''s silly. I recommend digging a new hole and then filling this one in.';
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
         Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' already covered.';
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
         Message := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' already full to overflowing.';
      end;
   end
   else
   begin
      Result := inherited;
   end;
end;

procedure THole.HandleAdd(Thing: TThing; Blame: TAvatar);
var
   Child: PThingItem;
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
         Child := FChildren;
         while (Assigned(Child)) do
         begin
            if ((Child^.Value is FPileClass) and (Child^.Value.Position = tpIn)) then
               PileSize := PileSize + Child^.Value.GetOutsideSizeManifest();
            Child := Child^.Next;
         end;
         if (PileSize >= FSize) then
         begin
            // { Check all descendants for TAvatars, to avoid burying them }
            // DoNavigation(Self, FParent.GetDefaultAtom(), cdOut, Avatar);
            { Fill hole and bury treasure }
            DoBroadcast([FParent], nil, [C(M(@Blame.GetDefiniteName)), SP, MP(Blame, M('fills'), M('fill')), SP, M(@GetDefiniteName), M(' with '), M(@Thing.GetDefiniteName), M('.')]);
            OldParent := FParent;
            FParent.Remove(Self); { have to do this first so that the Surface switches to using itself as an inside }
            Zero(PileSize);
            while (Assigned(FChildren)) do
            begin
               OldThing := FChildren^.Value;
               Assert(not (OldThing is TAvatar));
               if (OldThing is FPileClass) then
               begin
                  { the earth disappears }
                  OldThing.Position := tpAt
               end
               else
               if (OldThing.Position = tpIn) then
               begin
                  { make sure we're not burrying too much (currently this won't trigger, since you can't fill an overfilled hole) }
                  PileSize := PileSize + OldThing.GetOutsideSizeManifest();
                  if (PileSize > FSize) then
                     OldThing.Position := tpOn;
               end;
               if (OldThing.Position in [tpIn, tpOn]) then
               begin
                  OldParent.Add(OldThing, OldThing.Position);
               end
               else
               begin
                  Remove(OldThing);
                  OldThing.Destroy();
               end;
               Assert((not Assigned(FChildren)) or (FChildren^.Value <> OldThing));
            end;
            // announce the hole is filled
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
   Child: PThingItem;
begin
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if (Child^.Value.Position = tpOn) then
      begin
         Result := False;
         Exit;
      end;
      Child := Child^.Next;
   end;
   Result := True;
end;

procedure THole.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
begin
   Assert(FParent is TSurface);
   case Direction of
     cdUp, cdOut: DoNavigation(Self, FParent.GetDefaultAtom(), cdUp, Perspective);
    else
      Perspective.AvatarMessage('You''re in a hole.');
   end;
end;

function THole.GetBiggestCoverer: TThing;
var
   Child: PThingItem;
begin
   Assert(not IsOpen());
   Result := nil;
   Child := FChildren;
   while (Assigned(Child)) do
   begin
      if ((Child^.Value.Position = tpOn) and ((not (Assigned(Result))) or (Result.GetOutsideSizeManifest() < Child^.Value.GetOutsideSizeManifest()))) then
         Result := Child^.Value;
      Child := Child^.Next;
   end;
   Assert(Assigned(Result));
end;

function THole.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfCanHaveThingsPushedOn, tfCanHaveThingsPushedIn];
end;


constructor TPile.Create(Ingredient: AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
begin
   Create([Ingredient], [Ingredient], Description, Mass, Size);
end;

constructor TPile.Create(Ingredients: array of AnsiString; Description: AnsiString; Mass: TThingMass; Size: TThingSize);
begin
   Create(Ingredients, Ingredients, Description, Mass, Size);
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
      for Index := 1 to Length(PluralIngredients)-1 do
         Pattern := Pattern + ' ' + PluralIngredients[Index];
   Pattern := Pattern + ')@)?)';
   for Index := 0 to Length(SingularIngredients)-1 do
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
   inherited Create(['earth', 'dirt', 'soil'], 'The pile of earth is quite dirty.', kDensityMap[tdLow, Size], Size);
end;


initialization
   RegisterStorableClass(TNamedThing,             1000);
   RegisterStorableClass(TStaticThing,            1001);
   RegisterStorableClass(TScenery,                1002);
   RegisterStorableClass(TLocationProxy,          1003);
   RegisterStorableClass(TSurface,                1004);
   RegisterStorableClass(TDistantScenery,         1005);
   RegisterStorableClass(TContainer,              1006);
   RegisterStorableClass(TSpade,                  1010);
   RegisterStorableClass(TBag,                    1011);
   RegisterStorableClass(THole,                   1012);
   RegisterStorableClass(TPile,                   1013);
   RegisterStorableClass(TEarthPile,              1014);
end.