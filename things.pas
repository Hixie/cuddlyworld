{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit things;

interface

uses
   storable, world, grammarian;

type
   TMultinameThing = class(TThing)
    protected
      FNames: array of AnsiString;
      function IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean; override;
    public
      constructor Create(AName: AnsiString);
      constructor Create(ANames: array of AnsiString);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
   end;

   TStaticThing = class(TMultinameThing)
    protected
      FDescription: AnsiString;
      FMass: TThingMass;
      FSize: TThingSize;
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
      constructor Create(ANames: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
   end;

   TScenery = class(TStaticThing)
    protected
      FUnderDescription: AnsiString;
      FFindDescription: AnsiString;
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString);
      constructor Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
      constructor Create(ANames: array of AnsiString; ADescription: AnsiString);
      constructor Create(ANames: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      property UnderDescription: AnsiString read FUnderDescription write FUnderDescription;
      property FindDescription: AnsiString read FFindDescription write FFindDescription;
   end;

   TInternalLocationProxy = class(TScenery)
      FDestination: TLocation;
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; ADestination: TLocation);
      constructor Create(ANames: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; ADestination: TLocation);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanEnter(Perspective: TAvatar; AFrom: TAtom): TAtom; override;
   end;

   TSurface = class(TStaticThing)
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString);
      constructor Create(ANames: array of AnsiString; ADescription: AnsiString);
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      function GetDefaultAtom(): TAtom; override;
   end;

   TDistantScenery = class(TMultinameThing)
    protected
      FDirection: TCardinalDirection;
      function FarAway(Perspective: TAvatar): AnsiString; virtual;
    public
      constructor Create(AName: AnsiString; ADirection: TCardinalDirection);
      constructor Create(ANames: array of AnsiString; ADirection: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
   end;

   TPileState = set of (psTidy);
   TPile = class(TThing)
    protected
      FIngredients: array of AnsiString;
      FDescription: AnsiString;
      FMass: TThingMass;
      FSize: TThingSize;
      FState: TPileState;
      function AreMatchingWords(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar): Boolean; override;
      function IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean; override;
      function IsMatchingIngredientWord(Word: AnsiString; Perspective: TAvatar): Boolean; virtual;
      function AreChildrenImplicitlyReferencable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean; override;
    public
      constructor Create(AIngredients: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize); // first ingredient must be canonical plural form
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionState(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString; override;
      function GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString; override;
      function GetDescriptionEmpty(Perspective: TAvatar): AnsiString; override;
      function CanTake(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
   end;

implementation

uses
   sysutils;

constructor TMultinameThing.Create(AName: AnsiString);
begin
   Create([AName]);
end;

constructor TMultinameThing.Create(ANames: array of AnsiString);
var
   Index: Cardinal;
begin
   inherited Create();
   Assert(Length(ANames) > 0);
   SetLength(FNames, Length(ANames));
   for Index := 0 to Length(ANames)-1 do
      FNames[Index] := ANames[Index];
end;

destructor TMultinameThing.Destroy();
begin
   inherited;
end;

constructor TMultinameThing.Read(Stream: TReadStream);
var
   Index: Cardinal;
begin
   inherited;
   SetLength(FNames, Stream.ReadCardinal());
   for Index := 0 to Length(FNames)-1 do
      FNames[Index] := Stream.ReadAnsiString();
end;

procedure TMultinameThing.Write(Stream: TWriteStream);
var
   Index: Cardinal;
begin
   inherited;
   Stream.WriteCardinal(Length(FNames));
   for Index := 0 to Length(FNames)-1 do
      Stream.WriteAnsiString(FNames[Index]);
end;

function TMultinameThing.IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean;
var
   Index: Cardinal;
begin
   for Index := 0 to Length(FNames)-1 do
   begin
      if (Word = LowerCase(FNames[Index])) then
      begin
         Result := True;
         Exit;
      end;
   end;
   Result := False;
end;


constructor TStaticThing.Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
begin
   Create([AName], ADescription, AMass, ASize);
end;

constructor TStaticThing.Create(ANames: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
begin
   inherited Create(ANames);
   FDescription := ADescription;
   FMass := AMass;
   FSize := ASize;
end;

destructor TStaticThing.Destroy();
begin
   inherited;
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

function TStaticThing.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := FNames[0];
end;

function TStaticThing.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;


constructor TScenery.Create(AName: AnsiString; ADescription: AnsiString);
begin
   inherited Create([AName], ADescription, tmLudicrous, tsLudicrous);
end;

constructor TScenery.Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
begin
   inherited Create([AName], ADescription, AMass, ASize);
end;

constructor TScenery.Create(ANames: array of AnsiString; ADescription: AnsiString);
begin
   inherited Create(ANames, ADescription, tmLudicrous, tsLudicrous);
end;

constructor TScenery.Create(ANames: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
begin
   inherited Create(ANames, ADescription, AMass, ASize);
end;

constructor TScenery.Read(Stream: TReadStream);
begin
   inherited;
   FUnderDescription := Stream.ReadAnsiString();
   FFindDescription := Stream.ReadAnsiString();
end;

procedure TScenery.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FUnderDescription);
   Stream.WriteAnsiString(FFindDescription);
end;

function TScenery.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := False;
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


constructor TInternalLocationProxy.Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; ADestination: TLocation);
begin
   Create([AName], ADescription, AMass, ASize, ADestination);
end;

constructor TInternalLocationProxy.Create(ANames: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; ADestination: TLocation);
begin
   inherited Create(ANames, ADescription, AMass, ASize);
   FDestination := ADestination;
end;

constructor TInternalLocationProxy.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@FDestination);
end;

procedure TInternalLocationProxy.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FDestination);
end;

procedure TInternalLocationProxy.Navigate(Direction: TCardinalDirection; Perspective: TAvatar);
begin
   Assert(Assigned(FDestination));
   FDestination.Navigate(Direction, Perspective);
end;

function TInternalLocationProxy.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Result := FDestination.GetInside(PositionOverride);
   if (not Assigned(Result)) then
   begin
      Result := FDestination.GetSurface();
      PositionOverride := tpOn;
   end;
end;

function TInternalLocationProxy.CanEnter(Perspective: TAvatar; AFrom: TAtom): TAtom;
begin
   Result := FDestination.CanEnter(Perspective, AFrom);
end;


constructor TSurface.Create(AName: AnsiString; ADescription: AnsiString);
begin
   inherited Create([AName], ADescription, tmLudicrous, tsLudicrous);
end;

constructor TSurface.Create(ANames: array of AnsiString; ADescription: AnsiString);
begin
   inherited Create(ANames, ADescription, tmLudicrous, tsLudicrous);
end;

function TSurface.CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Assert(Assigned(FParent));
   Result := False;
   Message := Capitalise(GetDefiniteName(Perspective)) + ' is part of ' + FParent.GetDefiniteName(Perspective) + '.';
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

function TSurface.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := 'To look under ' + GetDefiniteName(Perspective) + ', you first need to get below it.';
end;


constructor TDistantScenery.Create(AName: AnsiString; ADirection: TCardinalDirection);
begin
   Create([AName], ADirection);
end;

constructor TDistantScenery.Create(ANames: array of AnsiString; ADirection: TCardinalDirection);
begin
   inherited Create(ANames);
   FDirection := ADirection;
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

function TDistantScenery.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := FNames[0];
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


constructor TPile.Create(AIngredients: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize);
var
   Index: Cardinal;
begin
   inherited Create();
   Assert(Length(AIngredients) > 0);
   SetLength(FIngredients, Length(AIngredients));
   for Index := 0 to Length(FIngredients)-1 do
      FIngredients[Index] := AIngredients[Index];
   FDescription := ADescription;
   FMass := AMass;
   FSize := ASize;
   FState := [psTidy];
end;

destructor TPile.Destroy();
begin
   inherited;
end;

constructor TPile.Read(Stream: TReadStream);
var
   Index: Cardinal;
begin
   inherited;
   SetLength(FIngredients, Stream.ReadCardinal());
   for Index := 0 to Length(FIngredients)-1 do
      FIngredients[Index] := Stream.ReadAnsiString();
   FDescription := Stream.ReadAnsiString();
   FMass := TThingMass(Stream.ReadCardinal());
   FSize := TThingSize(Stream.ReadCardinal());
   FState := TPileState(Stream.ReadCardinal());
end;

procedure TPile.Write(Stream: TWriteStream);
var
   Index: Cardinal;
begin
   inherited;
   Stream.WriteCardinal(Length(FIngredients));
   for Index := 0 to Length(FIngredients)-1 do
      Stream.WriteAnsiString(FIngredients[Index]);
   Stream.WriteAnsiString(FDescription);
   Stream.WriteCardinal(Cardinal(TThingMass(FMass)));
   Stream.WriteCardinal(Cardinal(TThingSize(FSize)));
   Stream.WriteCardinal(Cardinal(TPileState(FState)));
end;

function TPile.AreChildrenImplicitlyReferencable(Perspective: TAvatar; var PositionFilter: TThingPositionFilter): Boolean;
begin
   if ((psTidy in FState) and (Perspective.Parent <> Self)) then
      PositionFilter := PositionFilter - [tpIn];
   Result := inherited;
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

function TPile.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := 'pile of ' + FIngredients[0];
end;

function TPile.AreMatchingWords(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar): Boolean;
begin
   if ((Count = 3) and
       (Tokens[Start] = 'pile') and
       (Tokens[Start+1] = 'of') and
       (IsMatchingIngredientWord(Tokens[Start+2], Perspective))) then
      Result := true
   else
      Result := inherited;
end;

function TPile.IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean;
begin
   Result := (Word = 'pile') or (IsMatchingIngredientWord(Word, Perspective));
end;

function TPile.IsMatchingIngredientWord(Word: AnsiString; Perspective: TAvatar): Boolean;
var
   Index: Cardinal;
begin
   for Index := 0 to Length(FIngredients)-1 do
   begin
      if (Word = LowerCase(FIngredients[Index])) then
      begin
         Result := True;
         Exit;
      end;
   end;
   Result := False;
end;

function TPile.GetLookUnder(Perspective: TAvatar): AnsiString;
begin
   Result := 'Digging through ' + GetDefiniteName(Perspective) + ' to the bottom, you find ' + FParent.GetDefiniteName(Perspective) + '.';
end;

function TPile.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;

function TPile.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString;
var
   PositionFilter: TThingPositionFilter;
begin
   PositionFilter := [tpIn];
   if ((optThorough in Options) or (AreChildrenImplicitlyReferencable(Perspective, PositionFilter) and (tpIn in PositionFilter))) then
      Result := inherited
   else
      Result := '';
end;

function TPile.GetDescriptionInTitle(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions): AnsiString;
begin
   if (optThorough in Options) then
      Result := 'A thorough search through ' + GetDefiniteName(Perspective) + ' reveals:'
   else
      Result := 'Scatterered amongst the ' + FIngredients[0] + ' one can see:';
end;

function TPile.GetDescriptionEmpty(Perspective: TAvatar): AnsiString;
begin
   Result := 'A thorough search through ' + GetDefiniteName(Perspective) + ' reveals only a lot of ' + FIngredients[0] + '.';
end;

function TPile.GetDescriptionState(Perspective: TAvatar): AnsiString;
begin
   Result := inherited;
   if (not (psTidy in FState)) then
   begin
      if (Length(Result) > 0) then
         Result := Result + ' ';
      Result := Result + Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' spread somewhat all over the place.';
   end;
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
   Result := SizeAdder(GetInsideSizeManifest() + Manifest) < FSize;
end;


initialization
   RegisterStorableClass(TStaticThing,            100000);
   RegisterStorableClass(TScenery,                110000);
   RegisterStorableClass(TInternalLocationProxy,  111000);
   RegisterStorableClass(TSurface,                120000);
   RegisterStorableClass(TDistantScenery,         200000);
   RegisterStorableClass(TPile,                   300000);
end.