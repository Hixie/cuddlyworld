{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit grammarian;

interface

uses
   sysutils;

type
   EParseError = class(Exception)
   end;

procedure Fail(Message: AnsiString); inline;

type
   TGrammaticalNumber = set of (gnSingular, gnPlural);
   TTokens = array of AnsiString;

const
   gnBoth = [gnSingular, gnPlural];
   gnEither = gnBoth;
   gnAmbiguous = gnBoth;

type
   TCardinalDirection = (cdNorth, cdNorthEast, cdEast, cdSouthEast, cdSouth, cdSouthWest, cdWest, cdNorthWest, cdUp, cdDown,
                         cdOut, cdIn); { physical directions then logical directions }

const
   cdFirstPhysical = cdNorth;
   cdLastPhysical = cdDown;
   cdPhysicalDirections = [cdFirstPhysical .. cdLastPhysical];

type
   { Ambiguous means that the placement is made explicit in the name (e.g. "rim" + "of bag") }
   { Implicit means that the thing isn't mentioned when looking at its parent }
   { Currently, only one tpOpening is allowed per thing. We could relax this by changing GetInside() to GetOpenings() and making
     everything disambiguate wherever we are currently using GetInside() instead of having it assume it's one or nil. }
   { See further notes below for other implications of these values }
   TThingPosition = (tpPartOfImplicit, tpAmbiguousPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit,
                     tpOpening, tpAt, tpOn, tpIn, tpEmbedded, tpCarried);
   TThingPositionFilter = set of TThingPosition;

const
   tpEverything = [tpPartOfImplicit, tpAmbiguousPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpOpening, tpAt, tpOn, tpIn, tpEmbedded, tpCarried];
   tpImplicit = [tpPartOfImplicit, tpAmbiguousPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit]; { parent is assumed to include the description of these children already }
   tpAutoDescribe = [tpOpening, tpAt]; { things that should be included in the main description of an object }
   tpScenery = [tpPartOfImplicit, tpAmbiguousPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpOpening, tpAt]; { parent includes the mass of these children already }
   tpCountsForAll = [tpOnImplicit, tpOn, tpIn, tpCarried]; { things that should be included when listing 'all', as in "take all" }
   tpStacked = [tpPartOfImplicit, tpAmbiguousPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpAt, tpOn]; { affects how things are removed }
   tpSeparate = [tpAroundImplicit, tpAtImplicit, tpAt, tpIn, tpCarried]; { affects how things are pushed around }
   tpContained = [tpIn, tpEmbedded]; { things that shouldn't be aware of goings-on outside, if the parent is closed; count towards InsideSizeManifest }
   tpArguablyInside = [tpIn, tpEmbedded, tpOpening]; { things that the user can refer to as being "in" their parent }
   tpOutside = [tpOn, tpCarried]; { things that count towards OutsideSizeManifest }
   tpSurface = [tpOn]; { things that count towards SurfaceSizeManifest }
   tpDeferNavigationToParent = [tpPartOfImplicit, tpAmbiguousPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpAt, tpOn]; { only defer physical directions }

function Tokenise(const S: AnsiString): TTokens;
function TokeniseCanonically(const S: AnsiString): TTokens;
function TryMatch(var CurrentToken: Cardinal; const Tokens: TTokens; Pattern: array of AnsiString): Boolean;
function TryMatchWithLookahead(var CurrentToken: Cardinal; const Tokens: TTokens; Pattern: array of AnsiString; LookAheadPattern: array of AnsiString): Boolean;
function TryMatchWithNumber(var CurrentToken: Cardinal; const Tokens: TTokens; Pattern: array of AnsiString; out Number: Cardinal): Boolean; { '#' in the pattern is the number -- only matches numbers in the range 2..999,999,999}
function Serialise(const Tokens: TTokens; const Start, Count: Cardinal; const Separator: AnsiString = ' '): AnsiString;
function Canonicalise(const S: AnsiString): AnsiString;
function IndefiniteArticle(Noun: AnsiString): AnsiString; inline;
function Capitalise(Phrase: AnsiString): AnsiString; inline;
function TernaryConditional(FalseResult, TrueResult: AnsiString; Condition: Boolean): AnsiString; inline;
function WithSpaceIfNotEmpty(S: AnsiString): AnsiString; inline;
function WithNewlineIfNotEmpty(S: AnsiString): AnsiString; inline;

{$IFOPT C+}
function GrammaticalNumberToString(GrammaticalNumber: TGrammaticalNumber): AnsiString;
{$ENDIF}

function CardinalDirectionToString(CardinalDirection: TCardinalDirection): AnsiString; { 'north', 'up' }
function CardinalDirectionToDefiniteString(CardinalDirection: TCardinalDirection): AnsiString; { 'the north', 'above' }
function CardinalDirectionToDirectionString(CardinalDirection: TCardinalDirection): AnsiString; { 'to the north', 'above' - non-physical directions not allowed }
function ReverseCardinalDirection(CardinalDirection: TCardinalDirection): TCardinalDirection; { cdSouth, cdDown }

function ThingPositionToString(Position: TThingPosition): AnsiString;
function ThingPositionToDirectionString(Position: TThingPosition): AnsiString;

function NumberToEnglish(Number: Cardinal): AnsiString;

function IsAre(const IsPlural: Boolean): AnsiString; inline;

implementation

procedure Fail(Message: AnsiString);
begin
   raise EParseError.Create(Message);
end;

type
   TStringFilter = function (const S: AnsiString): AnsiString;

function Identity(const S: AnsiString): AnsiString;
begin
   Result := S;
end;        

function InternalTokenise(const S: AnsiString; const Canonicaliser: TStringFilter): TTokens;
var
   Start: Cardinal;
   Index: Cardinal;

   procedure PushToken(t: AnsiString);
   begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := t;
   end;

type
   TTokeniserState = (tsWordStart, tsWordBody, tsQuoted, tsDoubleQuoted);
var
   TokeniserState: TTokeniserState;
begin
   Start := 1;
   Index := Start;
   TokeniserState := tsWordStart;
   while (Index <= Length(S)) do
   begin
      case TokeniserState of
       tsWordStart:
          case S[Index] of
           ' ', #9: begin end;
           ',', ';', ':', '.', '?', '!', '+', '&': begin PushToken(S[Index]); end; { if you change this also change the serialiser }
           '''': begin Start := Index+1; TokeniserState := tsQuoted; end;
           '"': begin Start := Index+1; TokeniserState := tsDoubleQuoted; end;
          else
           Start := Index;
           TokeniserState := tsWordBody;
          end;
       tsWordBody:
          case S[Index] of
           ' ', #9: begin PushToken(Canonicaliser(S[Start..Index-1])); TokeniserState := tsWordStart; end;
           ',', ';', ':', '.', '?', '!', '+', '&': begin PushToken(Canonicaliser(S[Start..Index-1])); PushToken(S[Index]); TokeniserState := tsWordStart; end;
           '"': begin PushToken(Canonicaliser(S[Start..Index-1])); Start := Index+1; TokeniserState := tsDoubleQuoted; end;
          end;
       tsQuoted:
          case S[Index] of
           '''': begin
              if (Start < Index) then
                 PushToken('"' + AnsiString(S[Start..Index-1]) + '"')
              else
                 PushToken('""');
              TokeniserState := tsWordStart;
           end;
          end;
       tsDoubleQuoted:
          case S[Index] of
           '"': begin
              if (Start < Index) then
                 PushToken('"' + AnsiString(S[Start..Index-1]) + '"')
              else
                 PushToken('""');
              TokeniserState := tsWordStart;
           end;
          end;
      else
       raise EAssertionFailed.Create('Tokeniser reached bogus state ' + IntToStr(Ord(TokeniserState)));
      end;
      Inc(Index);
   end;
   case TokeniserState of
    tsWordStart: ;
    tsWordBody: PushToken(Canonicaliser(S[Start..Index-1]));
    tsQuoted: begin
        if (Start < Index) then
           PushToken('"' + AnsiString(S[Start..Index-1]) + '"')
        else
           PushToken('""')
     end;
    tsDoubleQuoted: begin
        if (Start < Index) then
           PushToken('"' + AnsiString(S[Start..Index-1]) + '"')
        else
           PushToken('""')
     end;
   else
    raise EAssertionFailed.Create('Tokeniser reached bogus state ' + IntToStr(Ord(TokeniserState)));
   end;
end;

function Tokenise(const S: AnsiString): TTokens;
begin
   Result := InternalTokenise(S, @Identity);
end;

function TokeniseCanonically(const S: AnsiString): TTokens;
begin
   Result := InternalTokenise(S, @Canonicalise);
end;

function TryMatch(var CurrentToken: Cardinal; const Tokens: TTokens; Pattern: array of AnsiString): Boolean;
var
   Index: Cardinal;
begin
   Result := False;
   if (CurrentToken + Length(Pattern) <= Length(Tokens)) then
   begin
      Index := 0;
      while (Index < Length(Pattern)) do
      begin
         if (Tokens[CurrentToken+Index] <> Pattern[Index]) then
            Exit;
         Inc(Index);
      end;
      Assert(Index = Length(Pattern));
      Inc(CurrentToken, Index);
      Result := True;
   end;
end;

function TryMatchWithLookahead(var CurrentToken: Cardinal; const Tokens: TTokens; Pattern: array of AnsiString; LookAheadPattern: array of AnsiString): Boolean;
var
   Index, LookAheadIndex: Cardinal;
begin
   Result := False;
   if (CurrentToken + Length(Pattern) + Length(LookAheadPattern) <= Length(Tokens)) then
   begin
      Index := 0;
      while (Index < Length(Pattern)) do
      begin
         if (Tokens[CurrentToken+Index] <> Pattern[Index]) then
            Exit;
         Inc(Index);
      end;
      LookAheadIndex := 0;
      while (LookAheadIndex < Length(LookAheadPattern)) do
      begin
         if (Tokens[CurrentToken+Index] <> LookAheadPattern[LookAheadIndex]) then
            Exit;
         Inc(Index);
         Inc(LookAheadIndex);
      end;
      Assert(Index = Length(Pattern) + Length(LookAheadPattern));
      Inc(CurrentToken, Length(Pattern));
      Result := True;
   end;
end;

function TryMatchWithNumber(var CurrentToken: Cardinal; const Tokens: TTokens; Pattern: array of AnsiString; out Number: Cardinal): Boolean;
// would be good to extend this to supprot "a dozen" "one dozen" etc
var
   Index, Subindex: Cardinal;
   FoundNumber, Digits: Boolean;
begin
   Result := False;
   FoundNumber := False;
   if (CurrentToken + Length(Pattern) <= Length(Tokens)) then
   begin
      Index := 0;
      while (Index < Length(Pattern)) do
      begin
         if (Pattern[Index] = '#') then
         begin
            Assert(not FoundNumber);
            FoundNumber := True;
            Assert(Length(Tokens[CurrentToken+Index]) > 0);
            Digits := True;
            Subindex := 1;
            while (Digits and (Subindex <= Length(Tokens[CurrentToken+Index]))) do
            begin
               if (not (Tokens[CurrentToken+Index][Subindex] in ['0'..'9'])) then
                  Digits := False
               else
                  Inc(Subindex);
            end;
            if (Digits) then
            begin
               { only numbers in the range 2..999,999,999 are supported }
               if (Length(Tokens[CurrentToken+Index]) >= 10) then
                  Exit;
               if (Tokens[CurrentToken+Index][1] = '0') then
                  Exit;
               if (Tokens[CurrentToken+Index] = '1') then
                  Exit;
               Val(Tokens[CurrentToken+Index], Number, Subindex);
               Assert(Subindex = 0, 'Digits was true for non-numeric string "' + Tokens[CurrentToken+Index] + '"');
               Assert(Number >= 2);
               Assert(Number <= 999999999);
            end
            else
            if (Tokens[CurrentToken+Index] = 'two') then
               Number := 2
            else
            if (Tokens[CurrentToken+Index] = 'three') then
               Number := 3
            else
            if (Tokens[CurrentToken+Index] = 'four') then
               Number := 4
            else
            if (Tokens[CurrentToken+Index] = 'five') then
               Number := 5
            else
            if (Tokens[CurrentToken+Index] = 'six') then
               Number := 6
            else
            if (Tokens[CurrentToken+Index] = 'seven') then
               Number := 7
            else
            if (Tokens[CurrentToken+Index] = 'eight') then
               Number := 8
            else
            if (Tokens[CurrentToken+Index] = 'nine') then
               Number := 9
            else
            if (Tokens[CurrentToken+Index] = 'ten') then
               Number := 10
            else
            if (Tokens[CurrentToken+Index] = 'eleven') then
               Number := 11
            else
            if (Tokens[CurrentToken+Index] = 'twelve') then
               Number := 12
            else
               Exit;
         end
         else
         if (Tokens[CurrentToken+Index] <> Pattern[Index]) then
            Exit;
         Inc(Index);
      end;
      Inc(CurrentToken, Length(Pattern));
      Result := True;
      Assert(FoundNumber);
   end;
end;

function Serialise(const Tokens: TTokens; const Start, Count: Cardinal; const Separator: AnsiString = ' '): AnsiString;
var
   Index: Cardinal;
begin
   Assert(Start < Length(Tokens));
   Assert(Count > 0);
   Result := Tokens[Start];
   if (Count > 1) then
   begin
      for Index := Start+1 to Start+Count-1 do
      begin
         { if you change this also update the tokeniser }
         if ((Tokens[Index] = ',') or
             (Tokens[Index] = ';') or
             (Tokens[Index] = ':') or
             (Tokens[Index] = '.') or
             (Tokens[Index] = '?') or
             (Tokens[Index] = '!')) then
            Result := Result + Tokens[Index]
         else
            Result := Result + Separator + Tokens[Index];
      end;
   end;
end;

function IndefiniteArticle(Noun: AnsiString): AnsiString;
begin
   Assert(Length(Noun) > 0);
   case Noun[1] of
    'a', 'e', 'i', 'o', 'u': Result := 'an';
    else Result := 'a';
   end;
end;

function Capitalise(Phrase: AnsiString): AnsiString;
begin
   Result := Phrase;
   Result[1] := UpperCase(Result[1])[1];
end;

function Canonicalise(const S: AnsiString): AnsiString;
begin
   Result := LowerCase(S);
end;        

function TernaryConditional(FalseResult, TrueResult: AnsiString; Condition: Boolean): AnsiString;
begin
   if (Condition) then
      Result := TrueResult
   else
      Result := FalseResult;
end;

function WithSpaceIfNotEmpty(S: AnsiString): AnsiString;
begin
   if (S = '') then
      Result := ''
   else
      Result := ' ' + S;
end;

function WithNewlineIfNotEmpty(S: AnsiString): AnsiString;
begin
   if (S = '') then
      Result := ''
   else
      Result := #10 + S;
end;

{$IFOPT C+}
function GrammaticalNumberToString(GrammaticalNumber: TGrammaticalNumber): AnsiString;
begin
   if (GrammaticalNumber = gnBoth) then
      Result := 'ambiguous'
   else
   if (GrammaticalNumber = [gnSingular]) then
      Result := 'singular'
   else
   if (GrammaticalNumber = [gnPlural]) then
      Result := 'plural'
   else
   if (GrammaticalNumber = []) then
      Result := 'neither'
   else
      raise EAssertionFailed.Create('unknown grammatical number');
end;
{$ENDIF}

function CardinalDirectionToString(CardinalDirection: TCardinalDirection): AnsiString;
begin
   case CardinalDirection of
     cdNorth: Result := 'north';
     cdNorthEast: Result := 'northeast';
     cdEast: Result := 'east';
     cdSouthEast: Result := 'southeast';
     cdSouth: Result := 'south';
     cdSouthWest: Result := 'southwest';
     cdWest: Result := 'west';
     cdNorthWest: Result := 'northwest';
     cdUp: Result := 'up';
     cdDown: Result := 'down';
     cdOut: Result := 'out';
     cdIn: Result := 'in';
    else
      raise EAssertionFailed.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
   end;
end;

function CardinalDirectionToDefiniteString(CardinalDirection: TCardinalDirection): AnsiString;
begin
   case CardinalDirection of
     cdNorth: Result := 'the north';
     cdNorthEast: Result := 'the northeast';
     cdEast: Result := 'the east';
     cdSouthEast: Result := 'the southeast';
     cdSouth: Result := 'the south';
     cdSouthWest: Result := 'the southwest';
     cdWest: Result := 'the west';
     cdNorthWest: Result := 'the northwest';
     cdUp: Result := 'above';
     cdDown: Result := 'below';
     cdOut: Result := 'outside';
     cdIn: Result := 'inside';
    else
      raise EAssertionFailed.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
   end;
end;

function CardinalDirectionToDirectionString(CardinalDirection: TCardinalDirection): AnsiString;
begin
   { There is an exit... }
   case CardinalDirection of
     cdNorth: Result := 'to the north';
     cdNorthEast: Result := 'to the northeast';
     cdEast: Result := 'to the east';
     cdSouthEast: Result := 'to the southeast';
     cdSouth: Result := 'to the south';
     cdSouthWest: Result := 'to the southwest';
     cdWest: Result := 'to the west';
     cdNorthWest: Result := 'to the northwest';
     cdUp: Result := 'above';
     cdDown: Result := 'below';
     cdOut: raise EAssertionFailed.Create('Tried to get direction string for cdOut.');
     cdIn: raise EAssertionFailed.Create('Tried to get direction string for cdIn.');
    else
      raise EAssertionFailed.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
   end;
end;

function ReverseCardinalDirection(CardinalDirection: TCardinalDirection): TCardinalDirection;
begin
   case CardinalDirection of
     cdNorth: Result := cdSouth;
     cdNorthEast: Result := cdSouthWest;
     cdEast: Result := cdWest;
     cdSouthEast: Result := cdNorthWest;
     cdSouth: Result := cdNorth;
     cdSouthWest: Result := cdNorthEast;
     cdWest: Result := cdEast;
     cdNorthWest: Result := cdSouthEast;
     cdUp: Result := cdDown;
     cdDown: Result := cdUp;
     cdOut: Result := cdIn;
     cdIn: Result := cdOut;
    else
      raise EAssertionFailed.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
   end;
end;

function ThingPositionToString(Position: TThingPosition): AnsiString;
begin
   case Position of
     tpPartOfImplicit, tpAmbiguousPartOfImplicit: Result := 'part of';
     tpAroundImplicit: Result := 'around';
     tpAtImplicit, tpAt: Result := 'at';
     tpOn: Result := 'on';
     tpOpening, tpIn, tpEmbedded: Result := 'in';
     tpCarried: Result := 'being carried by';
    else
     raise EAssertionFailed.Create('Unknown thing position ' + IntToStr(Ord(Position)));
   end;
end;

function ThingPositionToDirectionString(Position: TThingPosition): AnsiString;
begin
   { as in "moved ... the floor" }
   case Position of
     tpPartOfImplicit, tpAmbiguousPartOfImplicit: Result := 'so that it is part of'; // assert instead?
     tpAroundImplicit: Result := 'to'; // assert instead?
     tpAtImplicit: Result := 'to'; // assert instead?
     tpAt: Result := 'to';
     tpOn: Result := 'onto';
     tpOpening, tpIn, tpEmbedded: Result := 'into';
     tpCarried: Result := 'so that it is carried by'; // assert instead?
    else
     raise EAssertionFailed.Create('Unknown thing position ' + IntToStr(Ord(Position)));
   end;
end;

function NumberToEnglish(Number: Cardinal): AnsiString;
begin
   case Number of
    0: Result := 'zero';
    1: Result := 'one';
    2: Result := 'two';
    3: Result := 'three';
    4: Result := 'four';
    5: Result := 'five';
    6: Result := 'six';
    7: Result := 'seven';
    8: Result := 'eight';
    9: Result := 'nine';
    else Result := IntToStr(Number);
   end;
end;

function IsAre(const IsPlural: Boolean): AnsiString;
begin
   if (IsPlural) then
      Result := 'are'
   else
      Result := 'is';
end;

end.

{

procedure QuickSort(var List: TTokens); forward;
procedure QuickSort(var List: TTokens; L, R: Integer); forward;

procedure QuickSort(var List: TTokens);
begin
   Assert(Low(List) >= Low(Integer));
   Assert(High(List) <= High(Integer));
   if (Length(List) > 1) then
      QuickSort(List, Low(List), High(List));
end;

procedure QuickSort(var List: TTokens; L, R: Integer); // based on QuickSort in rtl/objpas/classes/lists.inc
var
   I, J : Integer;
   P, Q : AnsiString;
begin
   repeat
      I := L;
      J := R;
      P := List[(L + R) div 2];
      repeat
         while (P > List[I]) do
            I := I + 1;
         while (P < List[J]) do
            J := J - 1;
         if (I <= J) then
         begin
            Q := List[I];
            List[I] := List[J];
            List[J] := Q;
            I := I + 1;
            J := J - 1;
         end;
      until I > J;
      if (L < J) then
         QuickSort(List, L, J);
      L := I;
   until I >= R;
end;

}