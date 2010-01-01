{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit grammarian;

interface

type
   PCardinalDirection = ^TCardinalDirection;
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

function IndefiniteArticle(Noun: AnsiString): AnsiString; inline;
function Capitalise(Phrase: AnsiString): AnsiString; inline;
function MeansEverything(Word: AnsiString): Boolean; inline;

function TernaryConditional(FalseResult, TrueResult: AnsiString; Condition: Boolean): AnsiString; inline;
function WithSpaceIfNotEmpty(S: AnsiString): AnsiString; inline;
function WithNewlineIfNotEmpty(S: AnsiString): AnsiString; inline;

function CardinalDirectionToString(CardinalDirection: TCardinalDirection): AnsiString; { 'north', 'up' }
function CardinalDirectionToDefiniteString(CardinalDirection: TCardinalDirection): AnsiString; { 'the north', 'above' }
function CardinalDirectionToDirectionString(CardinalDirection: TCardinalDirection): AnsiString; { 'to the north', 'above' - non-physical directions not allowed }
function ReverseCardinalDirection(CardinalDirection: TCardinalDirection): TCardinalDirection; { cdSouth, cdDown }

function ThingPositionToString(Position: TThingPosition): AnsiString;
function ThingPositionToDirectionString(Position: TThingPosition): AnsiString;

implementation

uses
   sysutils;

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

function MeansEverything(Word: AnsiString): Boolean;
begin
   Result := (Word = 'all') or (Word = 'everything');
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
      raise Exception.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
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
      raise Exception.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
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
      raise Exception.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
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
      raise Exception.Create('Unknown cardinal direction ' + IntToStr(Ord(CardinalDirection)));
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
     raise Exception.Create('Unknown thing position ' + IntToStr(Ord(Position)));
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
     raise Exception.Create('Unknown thing position ' + IntToStr(Ord(Position)));
   end;
end;

end.