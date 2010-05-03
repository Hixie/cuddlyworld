{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit things;

interface

uses
   storable, world, thingdim, grammarian;

type
   TSynonymThing = class(TThing)
    protected
      FSynonyms: array of AnsiString;
      FLongName: AnsiString;
      function AreMatchingWords(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar): Boolean; override;
    public
      constructor Create(AName: AnsiString);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString); { first synonym is the name, and can have spaces; the rest must not have spaces }
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetLongDefiniteName(Perspective: TAvatar): AnsiString; override;
   end;

   TStaticThingFlags = set of (stfPlural);

   TStaticThing = class(TSynonymThing)
    protected
      FDescription: AnsiString;
      FMass: TThingMass;
      FSize: TThingSize;
      FFlags: TStaticThingFlags;
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; AFlags: TStaticThingFlags = []);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; AFlags: TStaticThingFlags = []);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
   end;

   TScenery = class(TStaticThing)
    protected
      FUnderDescription: AnsiString;
      FFindDescription: AnsiString;
      FCannotMoveExcuse: AnsiString;
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
      constructor Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
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
      constructor Create(AName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AFlags: TStaticThingFlags);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AFlags: TStaticThingFlags);
      constructor Create(AName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
   end;

   THole = class;

   TSurface = class(TStaticThing)
    protected
      FHole: THole;
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
      constructor Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection): AnsiString; override;
      function GetLookIn(Perspective: TAvatar): AnsiString; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      procedure Navigate(Direction: TCardinalDirection; Perspective: TAvatar); override;
      function GetDefaultAtom(): TAtom; override;
      function GetProperties(): TThingProperties; override;
      function Dig(Spade: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function GetDescriptionClosed(Perspective: TAvatar): AnsiString; override;
      procedure Removed(Thing: TThing); override;
   end;

   TDistantScenery = class(TSynonymThing)
    protected
      FDirection: TCardinalDirection;
      function FarAway(Perspective: TAvatar): AnsiString; virtual;
    public
      constructor Create(AName: AnsiString; ADirection: TCardinalDirection);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADirection: TCardinalDirection);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetLookUnder(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function CanMove(Perspective: TAvatar; var Message: AnsiString): Boolean; override;
   end;

   TSpade = class(TStaticThing)
    public
      constructor Create();
      function GetProperties(): TThingProperties; override;
      function CanDig(Target: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
   end;

   TBag = class(TSynonymThing)
    protected
      FDescription: AnsiString;
      FMaxSize: TThingSize;
    public
      constructor Create(AName: AnsiString; ADescription: AnsiString; AMaxSize: TThingSize);
      constructor Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMaxSize: TThingSize);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetOutsideSizeManifest(): TThingSizeManifest; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function IsOpen(): Boolean; override;
      function GetProperties(): TThingProperties; override;
   end;

   TPileClass = class of TPile;

   THole = class(TThing)
    protected
      FDescription: AnsiString;
      FSize: TThingSize;
      FPileClass: TPileClass;
    public
      constructor Create(ADescription: AnsiString; ASize: TThingSize; APileClass: TPileClass);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): AnsiString; override;
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
      function GetProperties(): TThingProperties; override;
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
      function IsMatchingIngredientWord(Word: AnsiString; Perspective: TAvatar): Boolean; virtual;
      function IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean; override;
    public
      constructor Create(AIngredients: array of AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize); { first ingredient must be canonical plural form }
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
      function IsOpen(): Boolean; override;
   end;

   TEarthPile = class(TPile)
    public
      constructor Create(ASize: TThingSize);
   end;

implementation

uses
   sysutils, broadcast;

constructor TSynonymThing.Create(AName: AnsiString);
begin
   Create([AName], AName);
end;

constructor TSynonymThing.Create(ASynonyms: array of AnsiString; ALongName: AnsiString);
var
   Index: Cardinal;
begin
   inherited Create();
   Assert(Length(ASynonyms) > 0);
   SetLength(FSynonyms, Length(ASynonyms));
   for Index := 0 to Length(ASynonyms)-1 do
      FSynonyms[Index] := ASynonyms[Index];
   FLongName := ALongName;
end;

constructor TSynonymThing.Read(Stream: TReadStream);
var
   Index: Cardinal;
begin
   inherited;
   SetLength(FSynonyms, Stream.ReadCardinal());
   for Index := 0 to Length(FSynonyms)-1 do
      FSynonyms[Index] := Stream.ReadAnsiString();
   FLongName := Stream.ReadAnsiString();
end;

procedure TSynonymThing.Write(Stream: TWriteStream);
var
   Index: Cardinal;
begin
   inherited;
   Stream.WriteCardinal(Length(FSynonyms));
   for Index := 0 to Length(FSynonyms)-1 do
      Stream.WriteAnsiString(FSynonyms[Index]);
   Stream.WriteAnsiString(FLongName);
end;

function TSynonymThing.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := FSynonyms[0];
end;

function TSynonymThing.GetLongDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := FLongName;
end;

function TSynonymThing.AreMatchingWords(Tokens: TTokens; Start, Count: Cardinal; Perspective: TAvatar): Boolean;
var
   Index: Cardinal;
begin




   "Ode to Death death wish" - "Ode", "Ode to Death", "death", "death wish", "death-wish", "wish"; NOT "Ode to Death wish", "to", "death death".

   Adjective ['Ode', 'Ode to Death', 'Death']
   Noun ['death wish', 'death-wish']
   Synonym ['wish']


   "Grotesque Blue Sword of Blood" - "Grotesque Sword", "Sword of Blood", "Grotesque Sword of Blood", "Grotesque", "Sword", "Blood"; NOT "Grotesque Blood", "of"

   Adjective ['Grotesque']
   Adjective ['Blue']
   Noun ['Sword', 'Sword of Blood']
   Synonym ['Blood']


   "Happy Fun Ball"          - "Ball", "Happy Ball", "Happy Fun Ball", "Fun Ball", "Happy", "Fun"

   Adjective ['Happy']
   Adjective ['Fun']
   Noun ['Ball']


   "the blue wooden archway to the north" - "blue", "navy", "wooden", "archway", "arch", "n", "north", "northern",
                                            "blue wooden archway", "navy wooden archway", "northern blue wooden archway",
                                            "navy wooden", "northern wooden", "n arch", "navy blue arch", "navy-blue arch";
                                            NOT "blue navy arch", "wooden navy", "arch blue", "archway wooden", "arch wooden", "arch n", "archway northern"

   Adjective ['Blue', 'Navy', 'navy blue', 'navy-blue']
   Adjective ['Wooden']
   Adjective ['n', 'north', 'northern']
   Noun ['Archway', 'Arch']
   


   explicit synonyms (must match entire phrase)
   adjectives (must be given once max each, must be given first, any number can be given, some have alternatives that are mutually exclusive)
   nouns (only one can be given, must be at end)
   

xxxxxxxxxxxxxxxxxxxxx

   for Index := 1 to Length(FSynonyms)-1 do
   begin
      Assert(Pos(' ', FSynonyms[Index]) <= 0);
      if (Word = LowerCase(FSynonyms[Index])) then
      begin
         Result := True;
         Exit;
      end;
   end;
   Result := inherited;
end;


constructor TStaticThing.Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; AFlags: TStaticThingFlags = []);
begin
   Create([AName], AName, ADescription, AMass, ASize);
end;

constructor TStaticThing.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMass: TThingMass; ASize: TThingSize; AFlags: TStaticThingFlags = []);
begin
   inherited Create(ASynonyms, ALongName);
   FDescription := ADescription;
   FMass := AMass;
   FSize := ASize;
   FFlags := AFlags;
end;

constructor TStaticThing.Read(Stream: TReadStream);
begin
   inherited;
   FDescription := Stream.ReadAnsiString();
   FMass := TThingMass(Stream.ReadCardinal());
   FSize := TThingSize(Stream.ReadCardinal());
   FFlags := TStaticThingFlags(Stream.ReadCardinal());
end;

procedure TStaticThing.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FDescription);
   Stream.WriteCardinal(Cardinal(FMass));
   Stream.WriteCardinal(Cardinal(FSize));
   Stream.WriteCardinal(Cardinal(FFlags));
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

function TStaticThing.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := stfPlural in FFlags;
end;


constructor TScenery.Create(AName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
begin
   inherited Create(AName, ADescription, tmLudicrous, tsLudicrous, AFlags);
end;

constructor TScenery.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
begin
   inherited Create(ASynonyms, ALongName, ADescription, tmLudicrous, tsLudicrous, AFlags);
end;

constructor TScenery.Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
begin
   { needed for default values }
   inherited;
end;

constructor TScenery.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
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


constructor TLocationProxy.Create(AName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AFlags: TStaticThingFlags);
begin
   Create([AName], AName, ADescription, ADestination, tmLudicrous, tsLudicrous, AFlags);
end;

constructor TLocationProxy.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AFlags: TStaticThingFlags);
begin
   Create(ASynonyms, ALongName, ADescription, ADestination, tmLudicrous, tsLudicrous, AFlags);
end;

constructor TLocationProxy.Create(AName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
begin
   Create([AName], AName, ADescription, ADestination, AMass, ASize, AFlags);
end;

constructor TLocationProxy.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; ADestination: TLocation; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
begin
   inherited Create(ASynonyms, ALongName, ADescription, AMass, ASize, AFlags);
   FDestination := ADestination;
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


constructor TSurface.Create(AName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
begin
   inherited Create(AName, ADescription, tmLudicrous, tsLudicrous, AFlags);
end;

constructor TSurface.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AFlags: TStaticThingFlags);
begin
   inherited Create(ASynonyms, ALongName, ADescription, tmLudicrous, tsLudicrous, AFlags);
end;

constructor TSurface.Create(AName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
begin
   { needed for default values }
   inherited;
end;

constructor TSurface.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMass: TThingMass = tmLudicrous; ASize: TThingSize = tsLudicrous; AFlags: TStaticThingFlags = []);
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

function TSurface.GetProperties(): TThingProperties;
begin
   Result := inherited;
   Result := Result + [tpDiggable];
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
         if (LastThing.Position = tpIn) then
         begin
            Assert(not (LastThing is TEarthPile));
            Pile.Add(LastThing, tpIn);
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
   if (Assigned(FHole) and (FHole.IsOpen())) then // XXX why the IsOpen check?
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


constructor TDistantScenery.Create(AName: AnsiString; ADirection: TCardinalDirection);
begin
   Create([AName], AName, ADirection);
end;

constructor TDistantScenery.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADirection: TCardinalDirection);
begin
   inherited Create(ASynonyms, ALongName);
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


constructor TSpade.Create();
begin
   inherited Create(['spade', 'shovel'], 'the spade', 'The spade is a small handheld tool apparently shaped from a single piece of metal.', tmLight, tsSmall);
end;

function TSpade.GetProperties(): TThingProperties;
begin
   Result := inherited; 
   Result := Result + [tpCanDig];
end;

function TSpade.CanDig(Target: TThing; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   Result := True;
end;


constructor TBag.Create(AName: AnsiString; ADescription: AnsiString; AMaxSize: TThingSize);
begin
   Create([AName], AName, ADescription, AMaxSize);
end;

constructor TBag.Create(ASynonyms: array of AnsiString; ALongName: AnsiString; ADescription: AnsiString; AMaxSize: TThingSize);
begin
   inherited Create(ASynonyms, ALongName);
   FDescription := ADescription;
   FMaxSize := AMaxSize;
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

function TBag.GetProperties(): TThingProperties;
begin
   Result := inherited;
   Result := Result + [tpCanHaveThingsPushedIn];
end;


constructor THole.Create(ADescription: AnsiString; ASize: TThingSize; APileClass: TPileClass);
begin
   inherited Create();
   FDescription := ADescription;
   FSize := ASize;
   FPileClass := APileClass;
end;

constructor THole.Read(Stream: TReadStream);
var
   AClass: TClass;
begin
   inherited;
   FDescription := Stream.ReadAnsiString();
   FSize := TThingSize(Stream.ReadCardinal());
   {$IFOPT C-} {$HINT This could be optimised further in non-debug builds.} {$ENDIF}
   AClass := Stream.ReadClass();
   Assert((AClass = TPile) or (AClass.InheritsFrom(TPile)));
   FPileClass := TPileClass(AClass);
end;

procedure THole.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FDescription);
   Stream.WriteCardinal(Cardinal(FSize));
   Stream.WriteClass(FPileClass);
end;

function THole.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := 'hole';
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
   Plural: Boolean;
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
         Plural := False;
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
                  Plural := (Count > 1) or Child^.Value.IsPlural(Perspective);
               end;
            end;
            Child := Child^.Next;
         end;
         if (Count > 0) then
         begin
            Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' full to overflowing; at the top of it ' + TernaryConditional('is', 'are', Plural) + ' ';
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

function THole.GetProperties(): TThingProperties;
begin
   Result := inherited;
   Result := Result + [tpCanHaveThingsPushedOn, tpCanHaveThingsPushedIn];
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

function TPile.IsChildTraversable(Child: TThing; Perspective: TAvatar; FromOutside: Boolean): Boolean;
begin
   Result := ((not (Child.Position in tpContained)) or (not FromOutside) or (not (psTidy in FState))) and inherited;
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
   if (((Count = 3) and
        (Tokens[Start] = 'pile') and
        (Tokens[Start+1] = 'of') and
        (IsMatchingIngredientWord(Tokens[Start+2], Perspective))) or
       ((Count = 1) and
        ((Tokens[Start] = 'pile') or
         (IsMatchingIngredientWord(Tokens[Start], Perspective)))) then
      Result := True
   else
      Result := False;
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
   Result := 'Digging through ' + GetDefiniteName(Perspective) + ' to the bottom, you find ' + FParent.GetIndefiniteName(Perspective) + '.';
end;

function TPile.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;

function TPile.GetDescriptionIn(Perspective: TAvatar; Options: TGetDescriptionChildrenOptions; Prefix: AnsiString): AnsiString;
begin
   if ((optThorough in Options) or (not (psTidy in FState))) then
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
   Result := (GetInsideSizeManifest() + Manifest) < FSize;
end;

function TPile.IsOpen(): Boolean;
begin
   Result := True;
end;


constructor TEarthPile.Create(ASize: TThingSize);
begin
   inherited Create(['earth', 'dirt', 'soil'], 'The pile of earth is quite dirty.', kDensityMap[tdLow, ASize], ASize);
end;


initialization
   RegisterStorableClass(TStaticThing,            100000);
   RegisterStorableClass(TScenery,                100001);
   RegisterStorableClass(TLocationProxy,          100002);
   RegisterStorableClass(TSurface,                100003);
   RegisterStorableClass(TSpade,                  100004);
   RegisterStorableClass(TBag,                    100005);
   RegisterStorableClass(TDistantScenery,         100006);
   RegisterStorableClass(THole,                   100007);
   RegisterStorableClass(TPile,                   100008);
   RegisterStorableClass(TEarthPile,              100009);
end.