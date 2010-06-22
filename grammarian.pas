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
   cdLastPhysical = cdDown;

type
   TThingPosition = (tpPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpInImplicit,
                     tpOpening, tpAt, tpOn, tpIn, tpCarried);
   TThingPositionFilter = set of TThingPosition;

const
   tpEverything = [tpPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpInImplicit, tpOpening, tpAt, tpOn, tpIn, tpCarried];
   tpImplicit = [tpPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpInImplicit]; { parent is assumed to include the description of these children already }
   tpAutoDescribe = [tpOpening, tpAt]; { things that should be included in the main description of an object }
   tpExplicit = [tpOnImplicit, tpInImplicit, tpOn, tpIn, tpCarried]; { things that should be included when listing 'all', as in "take all" }
   tpScenery = [tpPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpInImplicit, tpOpening, tpAt]; { parent includes the mass of these children already }
   tpContained = [tpInImplicit, tpIn]; { affects how things are pushed around }
   tpStacked = [tpPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpAt, tpOn]; { affects how things are removed }
   tpSeparate = [tpAroundImplicit, tpAtImplicit, tpInImplicit, tpAt, tpIn, tpCarried]; { affects how things are pushed around }
   tpDeferNavigationToParent = [tpPartOfImplicit, tpAroundImplicit, tpAtImplicit, tpOnImplicit, tpAt, tpOn]; { only defer physical directions }

function Tokenise(const S: AnsiString): TTokens;
function TokeniseCanonically(const S: AnsiString): TTokens;
function TryMatch(var CurrentToken: Cardinal; const Tokens: TTokens; Pattern: array of AnsiString): Boolean; inline;
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
           ',', ';', ':', '.', '?', '!', '+', '&': begin PushToken(S[Index]); end;
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
      end;
      Inc(CurrentToken, Length(Pattern));
      Result := True;
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
      for Index := Start+1 to Start+Count-1 do
         Result := Result + Separator + Tokens[Index];
end;

function IndefiniteArticle(Noun: AnsiString): AnsiString;
begin
   Assert(Length(Noun) > 0);
   case Noun[1] of
    'a', 'e', 'i', 'o', 'u', 'y': Result := 'an';
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
     tpPartOfImplicit: Result := 'part of';
     tpAroundImplicit: Result := 'around';
     tpAtImplicit, tpAt: Result := 'at';
     tpOn: Result := 'on';
     tpOpening, tpInImplicit, tpIn: Result := 'in';
     tpCarried: Result := 'being carried by';
    else
     raise EAssertionFailed.Create('Unknown thing position ' + IntToStr(Ord(Position)));
   end;
end;

function ThingPositionToDirectionString(Position: TThingPosition): AnsiString;
begin
   { as in "moved ... the floor" }
   case Position of
     tpPartOfImplicit: Result := 'so that it is part of'; // assert instead?
     tpAroundImplicit: Result := 'to'; // assert instead?
     tpAtImplicit: Result := 'to'; // assert instead?
     tpAt: Result := 'to';
     tpOn: Result := 'onto';
     tpOpening, tpInImplicit, tpIn: Result := 'into';
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
         while P > List[I] do
            I := I + 1;
         while P < List[J] do
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