{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit things;

interface

uses
   storable, physics, messages, thingdim, grammarian, matcher, textstream;

type
   TNamedThing = class (TThing)
    protected
      FName, FCachedLongName: UTF8String;
      FCachedLongNameMatcherFlags: TMatcherFlags;
      FSingularPattern, FPluralPattern: TMatcher;
      FPlural: Boolean;
      function GetMatcherFlags(Perspective: TAvatar): TMatcherFlags; virtual;
      class function CreateFromProperties(Properties: TTextStreamProperties): TNamedThing; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetLongName(Perspective: TAvatar): UTF8String; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; override;
      {$IFDEF DEBUG} function Debug(): UTF8String; override; {$ENDIF}
      property Plural: Boolean read FPlural write FPlural;
   end;

   // things that have a description
   TDescribedThing = class (TNamedThing)
    protected
      FDescription: UTF8String;
      class function CreateFromProperties(Properties: TTextStreamProperties): TDescribedThing; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
   end;

   // Things that have a specific mass and size
   TPhysicalThing = class (TNamedThing)
    protected
      FMass: TThingMass;
      FSize: TThingSize;
      class function CreateFromProperties(Properties: TTextStreamProperties): TPhysicalThing; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Mass: TThingMass; Size: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      property Mass: TThingMass read FMass write FMass;
      property Size: TThingSize read FSize write FSize;
   end;

   // Things that never change: MacGuffins, set pieces, and other non-interactive nouns
   TDescribedPhysicalThing = class(TPhysicalThing) // @RegisterStorableClass
    protected
      FDescription: UTF8String;
      class function CreateFromProperties(Properties: TTextStreamProperties): TDescribedPhysicalThing; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass; ASize: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
   end;

   // Things that are really just aspects of other things
   TFeature = class(TDescribedPhysicalThing) // @RegisterStorableClass
    protected
      class function CreateFromProperties(Properties: TTextStreamProperties): TFeature; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String);
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; override;
   end;

   // Things that never change and don't even ever move and are typically gigantic
   TScenery = class(TDescribedPhysicalThing) // @RegisterStorableClass
    protected
      FUnderDescription: UTF8String;
      FFindDescription: UTF8String;
      FCannotMoveExcuse: UTF8String;
      FOpened: Boolean;
      class function CreateFromProperties(Properties: TTextStreamProperties): TScenery; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function IsOpen(): Boolean; override;
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String; override;
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      property UnderDescription: UTF8String read FUnderDescription write FUnderDescription;
      property FindDescription: UTF8String read FFindDescription write FFindDescription;
      property CannotMoveExcuse: UTF8String read FCannotMoveExcuse write FCannotMoveExcuse;
      property Opened: Boolean read FOpened write FOpened;
   end;

   // Things that don't change and don't move but that provide a portal to other locations
   TLocationProxy = class(TScenery)
    protected
      FDestination: TLocation;
      class function CreateFromProperties(Properties: TTextStreamProperties): TLocationProxy; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; Destination: TLocation; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String; override;
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function IsOpen(): Boolean; override;
   end;

   TOpening = class(TLocationProxy) // @RegisterStorableClass
    protected
      class function CreateFromProperties(Properties: TTextStreamProperties): TOpening; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; Destination: TLocation; ASize: TThingSize);
      function GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom; override;
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanShake(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function GetFeatures(): TThingFeatures; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
   end;

   // The ground, mainly
   TSurface = class(TDescribedPhysicalThing) // @RegisterStorableClass
    protected
      class function CreateFromProperties(Properties: TTextStreamProperties): TSurface; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous);
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var MEssage: TMessage): TNavigationInstruction; override;
      function GetRepresentative(): TAtom; override;
   end;

   THole = class;

   TEarthGround = class(TSurface) // @RegisterStorableClass
    protected
      FHole: THole;
      {$IFOPT C+} procedure AssertChildPositionOk(Thing: TThing; APosition: TThingPosition); override; {$ENDIF}
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
      function GetLookIn(Perspective: TAvatar): UTF8String; override;
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function GetFeatures(): TThingFeatures; override;
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetDescriptionNoInside(Perspective: TAvatar): UTF8String; override;
      procedure Removed(Thing: TThing); override;
   end;

   // Open boxes, crates, etc
   TContainer = class(TDescribedPhysicalThing) // @RegisterStorableClass
     public
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function IsOpen(): Boolean; override;
      function GetFeatures(): TThingFeatures; override;
   end;

   TSpade = class(TDescribedPhysicalThing) // @RegisterStorableClass
    protected
      class function CreateFromProperties(Properties: TTextStreamProperties): TSpade; override;
    public
      constructor Create();
      function GetFeatures(): TThingFeatures; override;
      function CanDig(Target: TThing; Perspective: TAvatar; var Message: TMessage): Boolean; override;
   end;

   TSign = class(TScenery) // @RegisterStorableClass
    protected
      FWriting: UTF8String;
      class function CreateFromProperties(Properties: TTextStreamProperties): TSign; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; Writing: UTF8String; AMass: TThingMass; ASize: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetFeatures(): TThingFeatures; override;
      function GetDescriptionWriting(Perspective: TAvatar): UTF8String; override;
   end;

   TTree = class(TScenery) // @RegisterStorableClass
   end;


   // More complicated things

   TBag = class(TDescribedThing) // @RegisterStorableClass
    protected
      FMaxSize: TThingSize;
      class function CreateFromProperties(Properties: TTextStreamProperties): TBag; override;
    public
      constructor Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; MaxSize: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function IsOpen(): Boolean; override;
      function GetFeatures(): TThingFeatures; override;
   end;

   TPileClass = class of TPile;

   THole = class(TDescribedThing) // @RegisterStorableClass
    protected
      FSize: TThingSize;
      FPileClass: TPileClass;
      class function CreateFromProperties(Properties: TTextStreamProperties): THole; override;
    public
      constructor Create(Description: UTF8String; Size: TThingSize; PileClass: TPileClass);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetTitle(Perspective: TAvatar): UTF8String; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String; override;
      function GetDescriptionState(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionClosed(Perspective: TAvatar): UTF8String; override;
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String; override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function CanMove(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanTake(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanShake(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      function IsOpen(): Boolean; override;
      function GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction; override;
      function GetBiggestCoverer: TThing; { only call if IsOpen() is false meaning there is a coverer in the first place }
      function GetFeatures(): TThingFeatures; override;
   end;

   TPile = class(TDescribedPhysicalThing) // @RegisterStorableClass
    protected
      FIngredient: UTF8String;
      class function CreateFromProperties(Properties: TTextStreamProperties): TPile; override;
    public
      constructor Create(SingularIngredients: array of UTF8String; PluralIngredients: array of UTF8String; Description: UTF8String; AMass: TThingMass; ASize: TThingSize); { ingredient must be canonical plural form }
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function GetLookUnder(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String): UTF8String; override;
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): UTF8String; override;
      function GetDescriptionEmpty(Perspective: TAvatar): UTF8String; override;
      function CanTake(Perspective: TAvatar; var Message: TMessage): Boolean; override;
      function GetInside(var PositionOverride: TThingPosition): TThing; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function IsOpen(): Boolean; override;
   end;

   TEarthPile = class(TPile) // @RegisterStorableClass
    protected
      class function CreateFromProperties(Properties: TTextStreamProperties): TEarthPile; override;
    public
      constructor Create(ASize: TThingSize);
   end;

implementation

uses
   sysutils, broadcast, exceptions;

constructor TNamedThing.Create(Name: UTF8String; Pattern: UTF8String);
var
   TokenisedName: TTokens;
   NameIsSingular: Boolean;
   {$IFOPT C+} NameIsPlural: Boolean; {$ENDIF}
begin
   inherited Create();
   FCachedLongNameMatcherFlags := mfUnset;
   FName := Name;
   {$IFOPT C+} Assert(HasSingularVsPluralAnnotation(Pattern), 'The ' + Name + ' needs explicit singular and plural patterns (no slashes found in "' + Pattern + '").'); {$ENDIF}
   CompilePattern(Pattern, FSingularPattern, FPluralPattern);
   TokenisedName := TokeniseCanonically(Name);
   NameIsSingular := FSingularPattern.Matches(TokenisedName, 0) = Length(TokenisedName);
   {$IFOPT C+}
     NameIsPlural := FPluralPattern.Matches(TokenisedName, 0) = Length(TokenisedName);
     if ((not NameIsSingular) and (not NameIsPlural)) then
        Assert(False, 'Default name ("' + Name + '") does not match given pattern ("' + Pattern + '")');
   {$ENDIF}
   FPlural := not NameIsSingular;
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
   FCachedLongNameMatcherFlags := mfUnset;
   FName := Stream.ReadString();
   FSingularPattern := Stream.ReadObject() as TMatcher;
   FPluralPattern := Stream.ReadObject() as TMatcher;
   FPlural := Stream.ReadBoolean();
end;

procedure TNamedThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FName);
   Stream.WriteObject(FSingularPattern);
   Stream.WriteObject(FPluralPattern);
   Stream.WriteBoolean(FPlural);
end;

class function TNamedThing.CreateFromProperties(Properties: TTextStreamProperties): TNamedThing;
var
   Name: UTF8String;
   Pattern: UTF8String;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          HandleChildProperties(Properties, StreamedChildren)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern]);
   Result := Create(Name, Pattern);
   StreamedChildren.Apply(Result);
end;

function TNamedThing.GetMatcherFlags(Perspective: TAvatar): TMatcherFlags;
begin
   Result := 0;
end;

function TNamedThing.GetName(Perspective: TAvatar): UTF8String;
begin
   Result := FName;
end;

function TNamedThing.GetLongName(Perspective: TAvatar): UTF8String;
var
   CurrentFlags: TMatcherFlags;
begin
   CurrentFlags := GetMatcherFlags(Perspective);
   if (FCachedLongNameMatcherFlags <> CurrentFlags) then
   begin
      if (FPlural) then
      begin
         FCachedLongName := FPluralPattern.GetCanonicalMatch(' ', CurrentFlags);
         Assert(FCachedLongName <> '', 'Couldn''t get a long name out of:'#10 + FPluralPattern.GetPatternDescription());
      end
      else
      begin
         FCachedLongName := FSingularPattern.GetCanonicalMatch(' ', CurrentFlags);
         Assert(FCachedLongName <> '', 'Couldn''t get a long name out of:'#10 + FSingularPattern.GetPatternDescription());
      end;
      FCachedLongNameMatcherFlags := CurrentFlags;
   end
   else
   begin
      Assert(FCachedLongName <> '', 'Flags were already ' + IntToStr(FCachedLongNameMatcherFlags) + ' but didn''t have a cached name.');
   end;
   Result := FCachedLongName;
end;

function TNamedThing.IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean;
var
   PossibleCount: Cardinal;
begin
   Result := False;
   GrammaticalNumber := [];
   Count := FSingularPattern.Matches(Tokens, Start, GetMatcherFlags(Perspective));
   if (Count > 0) then
   begin
      Result := True;
      GrammaticalNumber := [gnSingular];
   end;
   PossibleCount := FPluralPattern.Matches(Tokens, Start, GetMatcherFlags(Perspective));
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
function TNamedThing.Debug(): UTF8String;
begin
   Result := inherited;
   Result := Result + #10 +
             'Singular pattern as dot file: ' + #10 + FSingularPattern.GetPatternDotFileLabels();
end;
{$ENDIF}


constructor TDescribedThing.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String);
begin
   inherited Create(Name, Pattern);
   FDescription := Description;
end;

constructor TDescribedThing.Read(Stream: TReadStream);
begin
   inherited;
   FDescription := Stream.ReadString();
end;

procedure TDescribedThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FDescription);
end;

class function TDescribedThing.CreateFromProperties(Properties: TTextStreamProperties): TDescribedThing;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and    
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription]);
   Result := Create(Name, Pattern, Description);
   StreamedChildren.Apply(Result);
end;

function TDescribedThing.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := FDescription;
end;


constructor TPhysicalThing.Create(Name: UTF8String; Pattern: UTF8String; Mass: TThingMass; Size: TThingSize);
begin
   inherited Create(Name, Pattern);
   FMass := Mass;
   FSize := Size;
end;

constructor TPhysicalThing.Read(Stream: TReadStream);
begin
   inherited;
   FMass := TThingMass(Stream.ReadCardinal());
   FSize := TThingSize(Stream.ReadCardinal());
end;

procedure TPhysicalThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FMass));
   Stream.WriteCardinal(Cardinal(FSize));
end;

class function TPhysicalThing.CreateFromProperties(Properties: TTextStreamProperties): TPhysicalThing;
var
   Name: UTF8String;
   Pattern: UTF8String;
   MassValue: TThingMass;
   SizeValue: TThingSize;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and {BOGUS Hint: Local variable "MassValue" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and {BOGUS Hint: Local variable "SizeValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnMass, pnSize]);
   Result := Create(Name, Pattern, MassValue, SizeValue);
   StreamedChildren.Apply(Result);
end;

function TPhysicalThing.GetIntrinsicMass(): TThingMass;
begin
   Result := FMass;
end;

function TPhysicalThing.GetIntrinsicSize(): TThingSize;
begin
   Result := FSize;
end;


constructor TDescribedPhysicalThing.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass; ASize: TThingSize);
begin
   inherited Create(Name, Pattern, AMass, ASize);
   FDescription := Description;
end;

constructor TDescribedPhysicalThing.Read(Stream: TReadStream);
begin
   inherited;
   FDescription := Stream.ReadString();
end;

procedure TDescribedPhysicalThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FDescription);
end;

class function TDescribedPhysicalThing.CreateFromProperties(Properties: TTextStreamProperties): TDescribedPhysicalThing;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   MassValue: TThingMass;
   SizeValue: TThingSize;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and {BOGUS Hint: Local variable "MassValue" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and {BOGUS Hint: Local variable "SizeValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnMass, pnSize]);
   Result := Create(Name, Pattern, Description, MassValue, SizeValue);
   StreamedChildren.Apply(Result);
end;

function TDescribedPhysicalThing.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := FDescription;
end;


constructor TFeature.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String);
begin
   inherited Create(Name, Pattern, Description, tmLudicrous, tsLudicrous);
end;

function TFeature.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkCannotMoveBecauseLocation, '_ _ _ _.',
                                                           [Capitalise(GetDefiniteName(Perspective)),
                                                            IsAre(IsPlural(Perspective)),
                                                            ThingPositionToString(FPosition),
                                                            FParent.GetDefiniteName(Perspective)]);
end;

class function TFeature.CreateFromProperties(Properties: TTextStreamProperties): TFeature;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription]);
   Result := Create(Name, Pattern, Description);
   StreamedChildren.Apply(Result);
end;


constructor TScenery.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous);
begin
   { needed for default values }
   inherited;
end;

constructor TScenery.Read(Stream: TReadStream);
begin
   inherited;
   FUnderDescription := Stream.ReadString();
   FFindDescription := Stream.ReadString();
   FCannotMoveExcuse := Stream.ReadString();
   FOpened := Stream.ReadBoolean();
end;

procedure TScenery.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FUnderDescription);
   Stream.WriteString(FFindDescription);
   Stream.WriteString(FCannotMoveExcuse);
   Stream.WriteBoolean(FOpened);
end;

class function TScenery.CreateFromProperties(Properties: TTextStreamProperties): TScenery;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description, UnderDescriptionValue, FindDescriptionValue, CannotMoveExcuseValue: UTF8String;
   MassValue: TThingMass = tmLudicrous;
   SizeValue: TThingSize = tsLudicrous;
   OpenedValue: Boolean = False;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.HandleUniqueStringProperty(pnUnderDescription, UnderDescriptionValue) and
          Properties.HandleUniqueStringProperty(pnFindDescription, FindDescriptionValue) and
          Properties.HandleUniqueStringProperty(pnCannotMoveExcuse, CannotMoveExcuseValue) and
          Properties.HandleUniqueBooleanProperty(pnOpened, OpenedValue) and
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription]);
   Result := Create(Name, Pattern, Description, MassValue, SizeValue);
   if (Properties.Seen(pnUnderDescription)) then
      Result.UnderDescription := UnderDescriptionValue;
   if (Properties.Seen(pnFindDescription)) then
      Result.FindDescription := FindDescriptionValue;
   if (Properties.Seen(pnCannotMoveExcuse)) then
      Result.CannotMoveExcuse := CannotMoveExcuseValue;
   if (Properties.Seen(pnOpened)) then
      Result.Opened := OpenedValue;
   StreamedChildren.Apply(Result);
end;

function TScenery.IsOpen(): Boolean;
begin
   Result := FOpened;
end;

function TScenery.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   if (Length(FCannotMoveExcuse) > 0) then
      Message := TMessage.Create(mkCannotMoveBecauseCustom, FCannotMoveExcuse)
   else
   if (FPosition = tpPartOfImplicit) then
      Message := TMessage.Create(mkCannotMoveBecauseLocation, '_ _ part of _.',
                                                              [Capitalise(GetDefiniteName(Perspective)),
                                                               IsAre(IsPlural(Perspective)),
                                                               FParent.GetDefiniteName(Perspective)])
   else
   if (FPosition in tpOpening) then
      Message := TMessage.Create(mkBogus, 'Moving _ doesn''t even make sense.', [GetIndefiniteName(Perspective)])
   else
      Message := TMessage.Create(mkCannotMoveBecauseLocation, '_ cannot be moved.',
                                                              [Capitalise(GetDefiniteName(Perspective))]);
end;

function TScenery.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String;
begin
   if ((Mode = psTheThingIsOnThatThing) and (Length(FFindDescription) > 0)) then
      Result := FFindDescription
   else
      Result := inherited;
end;

function TScenery.GetLookUnder(Perspective: TAvatar): UTF8String;
begin
   if (FPosition in tpScenery) then
   begin
      if (Length(FUnderDescription) > 0) then
         Result := FUnderDescription
      else
         Result := Capitalise(Perspective.GetDefiniteName(Perspective)) + ' cannot see under ' + GetDefiniteName(Perspective) + '.';
   end
   else
      Result := inherited;
end;


constructor TLocationProxy.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; Destination: TLocation; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous);
begin
   Assert(Assigned(Destination));
   inherited Create(Name, Pattern, Description, AMass, ASize);
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

class function TLocationProxy.CreateFromProperties(Properties: TTextStreamProperties): TLocationProxy;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   Destination: TLocation;
   MassValue: TThingMass = tmLudicrous;
   SizeValue: TThingSize = tsLudicrous;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          TLocation.HandleUniqueLocationProperty(Properties, pnDestination, Destination) and {BOGUS Hint: Local variable "Destination" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnDestination]);
   Result := Create(Name, Pattern, Description, Destination, MassValue, SizeValue);
   StreamedChildren.Apply(Result);
end;

function TLocationProxy.GetDescriptionDirectional(Perspective: TAvatar; Mode: TGetPresenceStatementMode; Direction: TCardinalDirection): UTF8String;
begin
   Result := Capitalise(GetIndefiniteName(Perspective)) + ' ' + TernaryConditional('leads', 'lead', IsPlural(Perspective)) + ' ' + CardinalDirectionToString(Direction) + '.';
end;

function TLocationProxy.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   DisambiguationOpening := Self;       
   Result := inherited;
end;

function TLocationProxy.GetInside(var PositionOverride: TThingPosition): TThing;
begin
   Assert(Assigned(FDestination));
   Result := FDestination.GetInside(PositionOverride);
   if (not Assigned(Result)) then
   begin
      Result := FDestination.GetSurface();
      Assert(Assigned(Result));
      PositionOverride := tpOn;
   end;
end;

function TLocationProxy.IsOpen(): Boolean;
begin
   Result := True;
end;


constructor TOpening.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; Destination: TLocation; ASize: TThingSize);
begin
   inherited Create(Name, Pattern, Description, Destination, tmLudicrous, ASize);
end;

class function TOpening.CreateFromProperties(Properties: TTextStreamProperties): TOpening;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   Destination: TLocation;
   SizeValue: TThingSize;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          TLocation.HandleUniqueLocationProperty(Properties, pnDestination, Destination) and {BOGUS Hint: Local variable "Destination" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and {BOGUS Hint: Local variable "SizeValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnSize, pnDestination]);
   Result := Create(Name, Pattern, Description, Destination, SizeValue);
   StreamedChildren.Apply(Result);
end;

function TOpening.GetEntrance(Traveller: TThing; Direction: TCardinalDirection; Perspective: TAvatar; var PositionOverride: TThingPosition; var DisambiguationOpening: TThing; var Message: TMessage; NotificationList: TAtomList): TAtom;
begin
   Assert(Message.IsValid);
   Assert(Assigned(Traveller));
   if (not CanInsideHold(Traveller.GetOutsideSizeManifest())) then
   begin
      Result := nil;
      Message := TMessage.Create(mkTooBig, '_ _ too big to fit in _.',
                                           [Capitalise(Traveller.GetDefiniteName(Perspective)),
                                            IsAre(Traveller.IsPlural(Perspective)),
                                            GetDefiniteName(Perspective)]);
   end
   else
      Result := inherited; // defers to GetInside(); TLocation.GetInside() gets the FDestination surface
end;

function TOpening.GetLookUnder(Perspective: TAvatar): UTF8String;
begin
   Result := GetLookIn(Perspective);
end;

function TOpening.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkBogus, '_ cannot move _. That does not make sense.',
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetIndefiniteName(Perspective)]);
end;

function TOpening.CanTake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkBogus, '_ cannot physically pick up _. That is ludicrous.',
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetIndefiniteName(Perspective)]);
end;

function TOpening.CanShake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkBogus, '_ cannot shake _. That is just silly.',
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetIndefiniteName(Perspective)]);
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


constructor TSurface.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous);
begin
   { needed for default values }
   inherited;
end;

class function TSurface.CreateFromProperties(Properties: TTextStreamProperties): TSurface;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   MassValue: TThingMass = tmLudicrous;
   SizeValue: TThingSize = tsLudicrous;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription]);
   Result := Create(Name, Pattern, Description, MassValue, SizeValue);
   StreamedChildren.Apply(Result);
end;

function TSurface.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Assert(Assigned(FParent));
   Result := False;
   Message := TMessage.Create(mkCannotMoveBecauseLocation, '_ _ part of _.',
                                                           [Capitalise(GetDefiniteName(Perspective)),
                                                            IsAre(IsPlural(Perspective)),
                                                            FParent.GetDefiniteName(Perspective)]);
end;

function TSurface.GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction;
begin
   Assert(Message.IsValid);
   // always defer to parent, so that "get out" doesn't try to get out of the TSurface and into the TLocation!
   Assert(Assigned(FParent));
   Result := FParent.GetNavigationInstructions(Direction, Self, Perspective, Message);
end;

function TSurface.GetRepresentative(): TAtom;
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

function TEarthGround.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
begin
   // we intentionally override seeing the ground if there's a hole, because we want to see what's in the hole
   if ((Direction = cdDown) and (Assigned(FHole)) and (FHole.IsOpen())) then
   begin
      Exclude(Options, lpNamesTarget);
      Result := FHole.GetDescriptionRemoteDetailed(Perspective, Direction, LeadingPhrase, Options);
   end
   else
      Result := inherited;
end;

function TEarthGround.GetLookIn(Perspective: TAvatar): UTF8String;
var
   Contents: UTF8String;
begin
   if (Assigned(FHole) and (FHole.IsOpen())) then
   begin
      Result := FHole.GetDescriptionRemoteDetailed(Perspective, cdIn, 'Looking in ' + FHole.GetDefiniteName(Perspective), [lpNamesTarget]);
      Contents := FHole.GetDescriptionIn(Perspective, []);
      if (Length(Contents) > 0) then
         Result := Result + #10 + Contents;
   end
   else
      Result := 'To look in ' + GetDefiniteName(Perspective) + ', you first need to dig a hole in it.';
end;

function TEarthGround.GetLookUnder(Perspective: TAvatar): UTF8String;
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

function TEarthGround.Dig(Spade: TThing; Perspective: TAvatar; var Message: TMessage): Boolean;
const
   HoleSize = tsGigantic;
var
   Pile: TPile;
   E: TThingList.TEnumerator;
begin
   Assert(Message.IsValid);
   if (Assigned(FHole)) then
   begin
      Result := False;
      if (FHole.IsOpen()) then
      begin
         Message := TMessage.Create(mkDuplicate, 'There is already a hole here.');
      end
      else
      begin
         Message := TMessage.Create(mkTooBig, 'There''s not really much room here to dig a hole. In particular, _ takes up a lot of room.',
                                              [FHole.GetBiggestCoverer().GetDefiniteName(Perspective)]);
      end;
   end
   else
   begin
      Pile := TEarthPile.Create(HoleSize);
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
      FHole := THole.Create('The hole is quite dirty.', HoleSize, TEarthPile);
      Add(FHole, tpSurfaceOpening);
      Add(Pile, tpOn);
      DoBroadcast([Self, FHole, Pile, Perspective], Perspective,
                  [C(M(@Perspective.GetDefiniteName)), SP,
                   MP(Perspective, M('digs'), M('dig')), SP,
                   M(@FHole.GetIndefiniteName), SP,
                   M(ThingPositionToString(FHole.Position)), SP,
                   M(@GetDefiniteName), M('.')]);
      Result := True;
      Message := TMessage.Create(mkSuccess, 'With much effort, _ dig a huge hole.',
                                            [Perspective.GetDefiniteName(Perspective)]);
   end;
end;

function TEarthGround.GetInside(var PositionOverride: TThingPosition): TThing;
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

function TEarthGround.GetDescriptionNoInside(Perspective: TAvatar): UTF8String;
begin
   Assert((not Assigned(FHole)) or (not FHole.IsOpen()));
   Result := 'To put things in ' + GetDefiniteName(Perspective) + ', you''ll have to dig a hole to put them in.';
end;

procedure TEarthGround.Removed(Thing: TThing);
begin
   if (Thing = FHole) then
      FHole := nil;
end;


function TContainer.GetInside(var PositionOverride: TThingPosition): TThing;
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

class function TSpade.CreateFromProperties(Properties: TTextStreamProperties): TSpade;
var
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Result := Create();
   StreamedChildren.Apply(Result);
end;

function TSpade.GetFeatures(): TThingFeatures;
begin
   Result := inherited GetFeatures() + [tfCanDig];
end;

function TSpade.CanDig(Target: TThing; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := True;
end;


constructor TBag.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; MaxSize: TThingSize);
begin
   inherited Create(Name, Pattern, Description);
   FMaxSize := MaxSize;
end;

constructor TBag.Read(Stream: TReadStream);
begin
   inherited;
   FMaxSize := TThingSize(Stream.ReadCardinal());
end;

procedure TBag.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FMaxSize));
end;

class function TBag.CreateFromProperties(Properties: TTextStreamProperties): TBag;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   MaxSizeValue: TThingSize;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnMaxSize, MaxSizeValue) and {BOGUS Hint: Local variable "MaxSizeValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnMaxSize]);
   Result := Create(Name, Pattern, Description, MaxSizeValue);
   StreamedChildren.Apply(Result);
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

function TBag.GetInside(var PositionOverride: TThingPosition): TThing;
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


constructor THole.Create(Description: UTF8String; Size: TThingSize; PileClass: TPileClass);
begin
   inherited Create('hole', 'hole/holes', Description);
   FSize := Size;
   FPileClass := PileClass;
end;

constructor THole.Read(Stream: TReadStream);
var
   PileClass: TClass;
begin
   inherited;
   FSize := TThingSize(Stream.ReadCardinal());
   {$IFOPT C-} {$HINT This could be optimised further in non-debug builds.} {$ENDIF}
   PileClass := Stream.ReadClass();
   Assert((PileClass = TPile) or (PileClass.InheritsFrom(TPile)));
   FPileClass := TPileClass(PileClass);
end;

procedure THole.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteCardinal(Cardinal(FSize));
   Stream.WriteClass(FPileClass);
end;

class function THole.CreateFromProperties(Properties: TTextStreamProperties): THole;
var
   Description: UTF8String;
   SizeValue: TThingSize;
   PileClassValue: TPileClass;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and {BOGUS Hint: Local variable "SizeValue" does not seem to be initialized}
          Properties.specialize HandleUniqueClassProperty<TPileClass>(pnPileClass, PileClassValue, TPile) and {BOGUS Hint: Local variable "PileClassValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnDescription, pnSize, pnPileClass]);
   Result := Create(Description, SizeValue, PileClassValue);
   StreamedChildren.Apply(Result);
end;

function THole.GetTitle(Perspective: TAvatar): UTF8String;
begin
   Result := 'hole in ' + FParent.GetDefiniteName(Perspective);
end;

function THole.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String;
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
            Result := 'There ' + IsAre(IsPlural(Perspective)) + ' ' + GetIndefiniteName(Perspective) + ' under ' + Child.GetLongDefiniteName(Perspective) + { ', ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + } '.';
            Exit;
         end
         else
         if (Mode = psOnThatSpecialThing) then
         begin
            Result := 'under ' + Child.GetLongDefiniteName(Perspective);
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

function THole.GetDescriptionState(Perspective: TAvatar): UTF8String;
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

function THole.GetDescriptionClosed(Perspective: TAvatar): UTF8String;
var
   Cover: TThing;
begin
   Assert(not IsOpen());
   Cover := GetBiggestCoverer();
   Assert(Assigned(Cover));
   Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' covered by ' + Cover.GetIndefiniteName(Perspective) + '.';
end;

function THole.GetLookUnder(Perspective: TAvatar): UTF8String;
begin
   Result := 'To look under ' + GetDefiniteName(Perspective) + ', you first need to get below it.';
end;

function THole.GetLookTowardsDirection(Perspective: TAvatar; Direction: TCardinalDirection): UTF8String;
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

function THole.GetInside(var PositionOverride: TThingPosition): TThing;
begin
   Result := Self;
end;

function THole.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function THole.CanMove(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkBogus, '_ cannot move _. That does not make sense. Maybe try filling this hole in and digging a new one in the new location.', 
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetIndefiniteName(Perspective)]);
end;

function THole.CanTake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkBogus, '_ cannot physically pick up a _. That is ludicrous.',
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetIndefiniteName(Perspective)]);
end;

function THole.CanShake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Result := False;
   Message := TMessage.Create(mkBogus, '_ cannot shake _. That is just silly.',
                                       [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                        GetIndefiniteName(Perspective)]);
end;

function THole.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
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
         Message := TMessage.Create(mkDuplicate, '_ _ already covered.',
                                                 [Capitalise(GetDefiniteName(Perspective)),
                                                  IsAre(IsPlural(Perspective))]);
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
         Message := TMessage.Create(mkDuplicate, '_ _ already full to overflowing.',
                                                 [Capitalise(GetDefiniteName(Perspective)),
                                                  IsAre(IsPlural(Perspective))]);
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
   E: TThingList.TEnumerator;
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
            //       ForceTravel(Child, Avatar.FParent, FParent.GetRepresentative(), cdOut, [], Child);
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

function THole.GetNavigationInstructions(Direction: TCardinalDirection; Child: TThing; Perspective: TAvatar; var Message: TMessage): TNavigationInstruction;
begin
   Assert(Message.IsValid);
   Result.TravelType := ttNone;
   Assert(FParent is TEarthGround);
   Assert(Child.Position = tpIn); // if this fires, then we need to make sure we handle navigation while on a filled pile by deferring to parent
   case Direction of
     cdUp, cdOut:
        begin
           Result.TravelType := ttByDirection;
           Result.RequiredAbilities := [naJump];
           Result.Target := FParent.GetRepresentative();
           Result.Direction := cdUp;
        end;
    else
      Message := TMessage.Create(mkInHole, '_ _ in a hole.',
                                 [Capitalise(Perspective.GetDefiniteName(Perspective)),
                                  IsAre(Perspective.IsPlural(Perspective))]);
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


constructor TPile.Create(SingularIngredients: array of UTF8String; PluralIngredients: array of UTF8String; Description: UTF8String; AMass: TThingMass; ASize: TThingSize);
var
   Index: Cardinal;
   Pattern: UTF8String;
begin
   Assert(Length(SingularIngredients) > 0);
   Assert(Length(PluralIngredients) = Length(SingularIngredients));
   Pattern := '((pile/piles (of (' + PluralIngredients[0];
   if (Length(PluralIngredients) > 1) then
      for Index := 1 to High(PluralIngredients) do // $R-
         Pattern := Pattern + ' ' + PluralIngredients[Index];
   Pattern := Pattern + ')@)?)';
   for Index := Low(SingularIngredients) to High(SingularIngredients) do // High() won't return -ve since Length(SingularIngredients) > 0 // $R-
   begin
      {$IFDEF DEBUG} Assert(not HasPatternChars(SingularIngredients[Index])); {$ENDIF}
      {$IFDEF DEBUG} Assert(not HasPatternChars(PluralIngredients[Index])); {$ENDIF}
      Pattern := Pattern + ' ' + SingularIngredients[Index] + '/' + PluralIngredients[Index];
   end;
   Pattern := Pattern + ')@';
   inherited Create('pile of ' + PluralIngredients[0], Pattern, Description, AMass, ASize);
   FIngredient := PluralIngredients[0];
end;

constructor TPile.Read(Stream: TReadStream);
begin
   inherited;
   FIngredient := Stream.ReadString();
end;

procedure TPile.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FIngredient);
end;

class function TPile.CreateFromProperties(Properties: TTextStreamProperties): TPile;
var
   Description: UTF8String;
   MassValue: TThingMass;
   SizeValue: TThingSize;
   SingularIngredients: array of UTF8String;
   PluralIngredients: array of UTF8String;
   StreamedChildren: TStreamedChildren;

   procedure Add(Singular, Plural: UTF8String);
   begin
      Assert(Length(SingularIngredients) = Length(PluralIngredients));
      SetLength(SingularIngredients, Length(SingularIngredients) + 1);
      SetLength(PluralIngredients, Length(PluralIngredients) + 1);
      SingularIngredients[High(SingularIngredients)] := Singular;
      PluralIngredients[High(PluralIngredients)] := Plural;
      Assert(Length(SingularIngredients) = Length(PluralIngredients));
   end;
   
   function HandleIngredientsProperty(): Boolean;
   var
      Stream: TTextStream;
      Singular: UTF8String;
      Plural: UTF8String;
   begin
      if (Properties.Name = pnIngredients) then
      begin
         Properties.EnsureNotSeen(pnIngredients);
         Stream := Properties.Accept();
         while (True) do
         begin      
            Singular := Stream.GetString();
            Stream.ExpectPunctuation('/');
            Plural := Stream.GetString();
            Add(Singular, Plural);
            if (Stream.PeekPunctuation() = ';') then
               break;
            Stream.ExpectPunctuation(',');
         end;
         Properties.Advance();
         Result := False;
      end
      else
         Result := True;
   end;
      
begin
   while (not Properties.Done) do
   begin
      if (HandleIngredientsProperty() and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and {BOGUS Hint: Local variable "MassValue" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and {BOGUS Hint: Local variable "SizeValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnIngredients, pnDescription, pnMass, pnSize]);
   Result := Create(SingularIngredients, PluralIngredients, Description, MassValue, SizeValue);
   StreamedChildren.Apply(Result);
end;

function TPile.GetOutsideSizeManifest(): TThingSizeManifest;
begin
   Result := inherited GetOutsideSizeManifest() + GetInsideSizeManifest();
end;

function TPile.GetLookUnder(Perspective: TAvatar): UTF8String;
begin
   Result := 'Digging through ' + GetDefiniteName(Perspective) + ' to the bottom, you find ' + FParent.GetIndefiniteName(Perspective) + '.';
end;

function TPile.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: UTF8String): UTF8String;
begin
   if (optThorough in Options) then
      Result := inherited
   else
      Result := '';
end;

function TPile.GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): UTF8String;
begin
   if (optThorough in Options) then
      Result := 'A thorough search through ' + GetDefiniteName(Perspective) + ' reveals:'
   else
      Result := 'Scatterered amongst the ' + FIngredient + ' one can see:';
end;

function TPile.GetDescriptionEmpty(Perspective: TAvatar): UTF8String;
begin
   Result := 'A thorough search through ' + GetDefiniteName(Perspective) + ' reveals only a lot of ' + FIngredient + '.';
end;

function TPile.CanTake(Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   Message := TMessage.Create(mkCannotTakeBecausePile, '_ _ through your fingers.',
                                                       [Capitalise(GetDefiniteName(Perspective)),
                                                        TernaryConditional('slips', 'slip', IsPlural(Perspective))]);
   Result := False;
end;

function TPile.GetInside(var PositionOverride: TThingPosition): TThing;
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


constructor TEarthPile.Create(ASize: TThingSize);
begin
   inherited Create(['earth', 'dirt', 'soil'], ['earth', 'dirt', 'soil'], 'The pile of earth is quite dirty.', kDensityMap[tdLow, ASize], ASize);
end;

class function TEarthPile.CreateFromProperties(Properties: TTextStreamProperties): TEarthPile;
var
   SizeValue: TThingSize;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and {BOGUS Hint: Local variable "SizeValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnSize]);
   Result := Create(SizeValue);
   StreamedChildren.Apply(Result);
end;


constructor TSign.Create(Name: UTF8String; Pattern: UTF8String; Description: UTF8String; Writing: UTF8String; AMass: TThingMass; ASize: TThingSize);
begin
   inherited Create(Name, Pattern, Description, AMass, ASize);
   FWriting := Writing;
end;

constructor TSign.Read(Stream: TReadStream);
begin
   inherited;
   FWriting := Stream.ReadString();
end;

procedure TSign.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FWriting);
end;

class function TSign.CreateFromProperties(Properties: TTextStreamProperties): TSign;
var
   Name: UTF8String;
   Pattern: UTF8String;
   Description: UTF8String;
   Writing: UTF8String;
   MassValue: TThingMass;
   SizeValue: TThingSize;
   StreamedChildren: TStreamedChildren;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnPattern, Pattern) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          Properties.HandleUniqueStringProperty(pnWriting, Writing) and
          Properties.specialize HandleUniqueEnumProperty<TThingMass>(pnMass, MassValue) and {BOGUS Hint: Local variable "MassValue" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingSize>(pnSize, SizeValue) and {BOGUS Hint: Local variable "SizeValue" does not seem to be initialized}
          HandleChildProperties(Properties, StreamedChildren)) then
         Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnPattern, pnDescription, pnWriting, pnMass, pnSize]);
   Result := Create(Name, Pattern, Description, Writing, MassValue, SizeValue);
   StreamedChildren.Apply(Result);
end;

function TSign.GetFeatures(): TThingFeatures;
begin
   Result := inherited;
   Result := Result + [tfExaminingReads];
end;

function TSign.GetDescriptionWriting(Perspective: TAvatar): UTF8String;
begin
   Result := 'On ' + GetDefiniteName(Perspective) + ' is written "' + FWriting + '".';
end;


initialization
{$INCLUDE registrations/things.inc}
end.
