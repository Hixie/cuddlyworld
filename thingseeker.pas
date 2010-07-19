{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit thingseeker;

interface

uses
   world, grammarian;

type
   TAllImpliedScope = set of (aisSurroundings, aisSelf);
   TClauseKind = (ckStart,
                  ckComma, ckCommaAnd, ckAnd,
                  ckThatIs{, ckThatIsNot, ckThatAre, ckThatAreNot},
                  ckBut{,
                  ckFrom, ckIn});
   TClauseKinds = set of TClauseKind;

   TThingCollector = class
    protected
      FTokenCount: Cardinal;
      FDisambiguate: Boolean;
      FThingList: PThingItem;
      {$IFOPT C+} FCurrentlyCollecting, FCollected: Boolean; {$ENDIF}
      FCurrentPreferredGrammaticalNumber: TGrammaticalNumber;
      FCurrentBestLength: Cardinal;
      FCurrentBestGrammaticalNumber: TGrammaticalNumber;
      FCurrentBestThingList: PThingItem;
      procedure ReferencedCallback(Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber);
    public
      constructor Create();
      destructor Destroy(); override;
      function Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TClauseKinds; Verb: AnsiString): Boolean;
      function GetTokenCount(): Cardinal;
      function GetDisambiguate(): Boolean;
      function GetThingList(): PThingItem; { must be called exactly once after Collect() returns true }
   end;

implementation

//{$DEFINE DEBUG_SEEKER}

uses
   sysutils;

constructor TThingCollector.Create();
begin
   inherited;
end;

destructor TThingCollector.Destroy();
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(not FCollected); {$ENDIF}
   Assert(FTokenCount = 0);
   Assert(not Assigned(FThingList));
   Assert(not Assigned(FCurrentBestThingList));
   inherited;
end;

procedure TThingCollector.ReferencedCallback(Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber);
var
   ThingItem: PThingItem;
begin
{$IFDEF DEBUG_SEEKER} Writeln(' - ReferencedCallback(', Thing.GetDefiniteName(nil), ', ', Count, ', ', GrammaticalNumberToString(GrammaticalNumber), ')'); {$ENDIF}
   if (Count < FCurrentBestLength) then
      Exit;
   if (Count > FCurrentBestLength) then
   begin
{$IFDEF DEBUG_SEEKER} Writeln('     new record length!'); {$ENDIF}
      FreeThingList(FCurrentBestThingList);
      FCurrentBestThingList := nil;
      FCurrentBestLength := Count;
      FCurrentBestGrammaticalNumber := [];
   end;
{$IFDEF DEBUG_SEEKER} Writeln('     FCurrentBestGrammaticalNumber=', GrammaticalNumberToString(FCurrentBestGrammaticalNumber)); {$ENDIF}
{$IFDEF DEBUG_SEEKER} Writeln('     GrammaticalNumber=', GrammaticalNumberToString(GrammaticalNumber)); {$ENDIF}
   if (FCurrentBestGrammaticalNumber <> GrammaticalNumber) then
   begin
{$IFDEF DEBUG_SEEKER} Writeln('     examining grammatical number'); {$ENDIF}
      if (FCurrentBestGrammaticalNumber = []) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('     assuming new one is better'); {$ENDIF}
         FCurrentBestGrammaticalNumber := GrammaticalNumber;
         Assert(not Assigned(FCurrentBestThingList));
      end
      else
      if (GrammaticalNumber >< FCurrentPreferredGrammaticalNumber = []) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('     new one is useless, we have better already'); {$ENDIF}
         { Either we're looking for plural and this is singular, or vice versa. Either way, not a match. }
         Exit;
      end
      else
      if (GrammaticalNumber <> gnAmbiguous) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('     new record grammatical number!'); {$ENDIF}
         Assert(FCurrentBestGrammaticalNumber >< gnAmbiguous = FCurrentPreferredGrammaticalNumber);
         Assert(GrammaticalNumber = FCurrentPreferredGrammaticalNumber);
         FreeThingList(FCurrentBestThingList);
         FCurrentBestThingList := nil;
         FCurrentBestLength := Count;
         FCurrentBestGrammaticalNumber := FCurrentPreferredGrammaticalNumber;
      end;
   end;
   New(ThingItem);
   ThingItem^.Value := Thing;
   ThingItem^.Next := FCurrentBestThingList;
   FCurrentBestThingList := ThingItem;
{$IFDEF DEBUG_SEEKER} Writeln('     current grammatical number: ', GrammaticalNumberToString(FCurrentBestGrammaticalNumber)); {$ENDIF}
end;

type
   TThingSelectionMechanism = (tsmPickAll, tsmPickOne, tsmPickNumber, tsmPickSome, tsmPickOnlyNumber);
   TClauseFlags = set of (cfAllowExceptions, cfSingular, cfExpectedSingleThing, cfFakeEverything);

type
   TAbstractJoiningClause = class;
   TAbstractClause = class
     private
      FFlags: TClauseFlags;
      FNumber: Cardinal;
      FSelectionMechanism: TThingSelectionMechanism;
      FThings: PThingItem;
      FInputFragment: AnsiString;
      FNext: TAbstractClause;
      FPrevious: TAbstractClause;
      function GetSelectionMechanism(): TThingSelectionMechanism; virtual;
     public
      constructor Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; Things: PThingItem; InputFragment: AnsiString); virtual;
      constructor CreateAll(Things: PThingItem; Flags: TClauseFlags); virtual;
      destructor Destroy(); override;
      procedure CheckContext(); virtual;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; virtual; abstract;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); virtual; abstract;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; virtual;
      function Select(): Boolean; { returns true if the result should be explicitly disambiguated }
      procedure Process(); virtual; abstract;
      procedure Add(Next: TAbstractClause);
      class function GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber; virtual;
      class function RequiresMatch(): Boolean; virtual;
      class function CanFail(): Boolean; virtual; { return True if the clause could also be a preposition or action join (e.g. "from", "and") }
   end;
   TAbstractClauseClass = class of TAbstractClause;

   TAbstractJoiningClause = class(TAbstractClause)
     protected
      FJoinedTo: TAbstractClause;
     public
      procedure CheckContext(); override;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); override;
      procedure ReportNoAcceptedJoin(); virtual; abstract;
      procedure Process(); override;
   end;
   TAbstractPlainJoiningClause = class(TAbstractJoiningClause)
      procedure ReportNoAcceptedJoin(); override;
      class function RequiresMatch(): Boolean; override;
   end;
   TCommaClause = class(TAbstractPlainJoiningClause)
      class function CanFail(): Boolean; override;
   end;
   TAndClause = class(TAbstractPlainJoiningClause)
      class function CanFail(): Boolean; override;
   end;

   TJoinableClause = class(TAbstractClause)
     protected
      FRegisteredJoins: array of TAbstractJoiningClause;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); override;
      procedure Process(); override;
   end;

   TAbstractFilteringClause = class(TJoinableClause)
     protected
      FVictims: array of TAbstractClause;
     public
      procedure CheckContext(); override;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure ReportNoVictims(); virtual; abstract;
      procedure Process(); override;
      procedure Filter(Victim: TAbstractClause); virtual; abstract;
      function GetFragmentAnnotation(): AnsiString; virtual; abstract;
      class function GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber; override;
   end;

   TUnionFilterClause = class(TAbstractFilteringClause)
      procedure Filter(Victim: TAbstractClause); override;
      procedure ReportNothingLeft(); virtual;
   end;
   TThatIsClause = class(TUnionFilterClause)
     protected
      function GetSelectionMechanism(): TThingSelectionMechanism; override;
     public
      procedure CheckContext(); override;
      procedure ReportNoVictims(); override;
      procedure ReportNotSingular(); virtual;
      procedure ReportExplicitNumber(); virtual;
      function GetFragmentAnnotation(): AnsiString; override;
   end;

   TExclusionFilteringClause = class(TAbstractFilteringClause)
      procedure Filter(Victim: TAbstractClause); override;
      procedure ReportNothingLeft(); virtual;
   end;
   TExceptionFilteringClause = class(TExclusionFilteringClause) { filters that filter the nearest sequence of "all"s }
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
   end;
   TButClause = class(TExceptionFilteringClause)
      procedure ReportNoVictims(); override;
      function GetFragmentAnnotation(): AnsiString; override;
   end;

   TStartClause = class(TJoinableClause)
      procedure CheckContext(); override;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure Bank(var Things: PThingItem);
      class function RequiresMatch(): Boolean; override;
      class function CanFail(): Boolean; override;
   end;

   EMatcherException = class
      FClause: TAbstractClause;
      constructor Create(Clause: TAbstractClause);
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; virtual; abstract;
   end;
   ESelectNotEnough = class(EMatcherException)
      FCount: Cardinal;
      constructor Create(Clause: TAbstractClause; Count: Cardinal);
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; override;
   end;
   ESelectTooMany = class(EMatcherException)
      FWanted, FGot: Cardinal;
      constructor Create(Clause: TAbstractClause; Wanted, Got: Cardinal);
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; override;
   end;

constructor EMatcherException.Create(Clause: TAbstractClause);
begin
   inherited Create();
   FClause := Clause;
end;

constructor ESelectNotEnough.Create(Clause: TAbstractClause; Count: Cardinal);
begin
   inherited Create(Clause);
   FCount := Count;
end;

function ESelectNotEnough.Message(Perspective: TAvatar; Input: AnsiString): AnsiString;
begin
   Assert(Length(Input) > 0);
   Result := 'About "' + Input + '"... I can only find ' + NumberToEnglish(FCount) + ': ' + ThingListToLongDefiniteString(FClause.FThings, Perspective, 'and') + '.';
end;

constructor ESelectTooMany.Create(Clause: TAbstractClause; Wanted, Got: Cardinal);
begin
   inherited Create(Clause);
   FWanted := Wanted;
   FGot := Got;
end;

function ESelectTooMany.Message(Perspective: TAvatar; Input: AnsiString): AnsiString;
begin
   Assert(Length(Input) > 0);
   if (FWanted = 1) then
      Result := 'Which "' + Input + '" do you mean, ' + ThingListToLongDefiniteString(FClause.FThings, Perspective, 'or') + '?'
   else
      Result := 'About "' + Input + '"... I count ' + NumberToEnglish(FGot) + ', not ' + NumberToEnglish(FWanted) + '.';
end;


constructor TAbstractClause.Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; Things: PThingItem; InputFragment: AnsiString);
begin
   inherited Create();
   FNumber := Number;
   FSelectionMechanism := SelectionMechanism;
   FFlags := Flags;
   FThings := Things;
   FInputFragment := InputFragment;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.Create(', Number, ', ', SelectionMechanism, ', ..., ', ThingListToLongDefiniteString(Things, nil, 'and'), ', ', InputFragment, ')'); {$ENDIF}
end;

constructor TAbstractClause.CreateAll(Things: PThingItem; Flags: TClauseFlags);
begin
   inherited Create();
   FNumber := 0;
   FSelectionMechanism := tsmPickAll;
   FFlags := Flags;
   FThings := Things;
   FInputFragment := 'everything';
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.CreateAll(', ThingListToLongDefiniteString(Things, nil, 'and'), ')'); {$ENDIF}
end;

destructor TAbstractClause.Destroy();
begin
   FreeThingList(FThings);
   FNext.Free();
   inherited;
end;

procedure TAbstractClause.CheckContext();
begin
   Assert(Assigned(FPrevious) xor (Self is TStartClause));
end;

function TAbstractClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.AcceptsFilter() called on a ', ClassName, ' for a peer that is a ', Peer.ClassName); {$ENDIF}
   if (Peer is TExceptionFilteringClause) then
   begin
      Result := cfAllowExceptions in FFlags;
      CanContinue := True;
   end
   else
   if (Peer is TThatIsClause) then
   begin
      Result := cfSingular in FFlags;
      CanContinue := False;
   end
   else
   begin
      Result := False;
      CanContinue := False;
   end;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

function TAbstractClause.Select(): Boolean;
var
   Count: Cardinal;
   Handle: PThingItem;

   procedure SelectN(N: Cardinal);
   begin
      Assert(N <= Count);
      Handle := FThings;
      while (N > 1) do
      begin
         Assert(Assigned(Handle));
         Handle := Handle^.Next;
         Dec(N);
      end;
      Assert(Assigned(Handle));
      if (Assigned(Handle^.Next)) then
      begin
         FreeThingList(Handle^.Next);
         Handle^.Next := nil;
      end;
   end;

begin
{$IFDEF DEBUG_SEEKER} Writeln('Select() called for ', ClassName(), ': ', FSelectionMechanism, '; number=', FNumber); {$ENDIF}
   Count := 0;
   Handle := FThings;
   while (Assigned(Handle)) do
   begin
      Handle := Handle^.Next;
      Inc(Count);
   end;
   if (Count < FNumber) then
   begin
      Assert((Count > 0) or (FSelectionMechanism = tsmPickAll), 'Count=' + IntToStr(Count) + '; FSelectionMechanism=' + IntToStr(Cardinal(FSelectionMechanism)));
      if (Count > 0) then
         raise ESelectNotEnough.Create(Self, Count);
   end;
   case GetSelectionMechanism() of
    tsmPickAll: Result := False;
    tsmPickNumber:
       begin
          SelectN(FNumber);
          Result := Count > FNumber;
       end;
    tsmPickSome:
       begin
          if (Count <= 2) then
          begin
             SelectN(Count);
             Result := False;
          end
          else
          begin
             if (Count > 7) then
                SelectN(7)
             else
                SelectN(Count div 2);
             Result := True;
          end;
       end;
    tsmPickOnlyNumber:
       begin
          if (Count <> FNumber) then
          begin
             Assert(Count > FNumber);
             raise ESelectTooMany.Create(Self, FNumber, Count);
          end;
          Result := False;
       end;
    else
       raise EAssertionFailed.Create('unknown thing selection mechanism');
   end;
   if (cfExpectedSingleThing in FFlags) then
      Result := True;
end;

procedure TAbstractClause.Add(Next: TAbstractClause);
begin
   Assert(Assigned(Next));
   Assert(not Assigned(FNext));
   Assert(not Assigned(Next.FPrevious));
   FNext := Next;
   Next.FPrevious := Self;
end;

function TAbstractClause.GetSelectionMechanism(): TThingSelectionMechanism;
begin
   Result := FSelectionMechanism;
end;

class function TAbstractClause.GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber;
begin
   Result := DefaultGrammaticalNumber;
end;

class function TAbstractClause.RequiresMatch(): Boolean;
begin
   Result := False;
end;

class function TAbstractClause.CanFail(): Boolean;
begin
   Result := False;
end;


procedure TAbstractJoiningClause.CheckContext();
var
   EarlierClause: TAbstractClause;
begin
   inherited;
   EarlierClause := FPrevious;
   while (not EarlierClause.AcceptsJoins(Self)) do
   begin
      EarlierClause := EarlierClause.FPrevious;
      if (not Assigned(EarlierClause)) then
         ReportNoAcceptedJoin();
   end;
   FJoinedTo := EarlierClause;
end;

procedure TAbstractJoiningClause.Process();
begin
   Assert(Assigned(FJoinedTo));
   FJoinedTo.RegisterJoin(Self);
end;

function TAbstractJoiningClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Assert(Assigned(FJoinedTo));
   Result := FJoinedTo.AcceptsJoins(Peer);
end;

procedure TAbstractJoiningClause.RegisterJoin(Peer: TAbstractJoiningClause);
begin
   Assert(Assigned(FJoinedTo));
   FJoinedTo.RegisterJoin(Peer);
end;


procedure TAbstractPlainJoiningClause.ReportNoAcceptedJoin();
begin
   Assert(False);
   Fail('I just don''t understand.');
end;

class function TAbstractPlainJoiningClause.RequiresMatch(): Boolean;
begin
   Result := True;
end;


class function TCommaClause.CanFail(): Boolean;
begin
   Result := True;
end;


class function TAndClause.CanFail(): Boolean;
begin
   Result := True;
end;


procedure TJoinableClause.Process();
var
   Index: Cardinal;
   CurrentThing: PThingItem;
begin
   Assert(not (cfFakeEverything in FFlags));
   if (Length(FRegisteredJoins) > 0) then
   begin
      CurrentThing := FThings;
      for Index := Low(FRegisteredJoins) to High(FRegisteredJoins) do
      begin
         Assert(Assigned(CurrentThing));
         while (Assigned(CurrentThing^.Next)) do
            CurrentThing := CurrentThing^.Next;
         Assert(not (cfFakeEverything in FRegisteredJoins[Index].FFlags));
         Assert(Assigned(FRegisteredJoins[Index].FThings));
         CurrentThing^.Next := FRegisteredJoins[Index].FThings;
         CurrentThing := FRegisteredJoins[Index].FThings;
         FRegisteredJoins[Index].FThings := nil;
      end;
   end;
end;

function TJoinableClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := False;
end;

function TJoinableClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   Result := inherited AcceptsFilter(Peer, CanContinue);
   if (Peer is TExceptionFilteringClause) then
      CanContinue := Result;
end;

procedure TJoinableClause.RegisterJoin(Peer: TAbstractJoiningClause);
begin
   SetLength(FRegisteredJoins, Length(FRegisteredJoins)+1);
   FRegisteredJoins[High(FRegisteredJoins)] := Peer;
end;


procedure TAbstractFilteringClause.CheckContext();
var
   CurrentClause: TAbstractClause;
   Continue: Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() called on a ', ClassName); {$ENDIF}
   inherited;
   CurrentClause := FPrevious;
   Continue := True;
   while (Continue) do
   begin
{$IFDEF DEBUG_SEEKER} Writeln('   examining clause that is a ', CurrentClause.ClassName); {$ENDIF}
      // we'll have to add some special magic here to handle "and that is" clauses
      // so that "take arch and that is blue" doesn't work but "take arch that is red and that is blue" does
      // (also once "all that is" is considered a simple prefix clause type, we need to handle that)
      if (CurrentClause.AcceptsFilter(Self, Continue)) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('   they said yes'); {$ENDIF}
         SetLength(FVictims, Length(FVictims)+1);
         FVictims[High(FVictims)] := CurrentClause;
      end
{$IFDEF DEBUG_SEEKER} else Writeln('   they said no') {$ENDIF};
      CurrentClause := CurrentClause.FPrevious;
      Assert((not Continue) or (Assigned(CurrentClause)));
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() done'); {$ENDIF}
   if (not (Length(FVictims) > 0)) then
      ReportNoVictims();
end;

function TAbstractFilteringClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   Result := False;
   CanContinue := not (Peer is ClassType);
end;

procedure TAbstractFilteringClause.Process();
var
   Index: Cardinal;
begin
   inherited;
   for Index := Low(FVictims) to High(FVictims) do
      Filter(FVictims[Index]);
end;

class function TAbstractFilteringClause.GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber;
begin
   Result := gnAmbiguous;
end;


procedure TUnionFilterClause.Filter(Victim: TAbstractClause);
var
   VictimThing, CondemnedThing, FilterThing: PThingItem;
   LastNext: PPThingItem;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TUnionFilterClause.Filter() called on a ', ClassName); {$ENDIF}
   if (cfFakeEverything in Victim.FFlags) then
   begin
      Victim.FThings := FThings;
      Exclude(Victim.FFlags, cfFakeEverything);
      FThings := nil;
      Exit;
   end;
   VictimThing := Victim.FThings;
   Victim.FThings := nil;
   LastNext := @Victim.FThings;
   while (Assigned(VictimThing)) do
   begin
{$IFDEF DEBUG_SEEKER} Writeln('  Filter(): considering ', VictimThing^.Value.GetDefiniteName(nil)); {$ENDIF}
      FilterThing := FThings;
{$IFDEF DEBUG_SEEKER} Writeln('            ...against ', FilterThing^.Value.GetDefiniteName(nil)); {$ENDIF}
      while (Assigned(FilterThing) and (FilterThing^.Value <> VictimThing^.Value)) do
      begin
         FilterThing := FilterThing^.Next;
{$IFDEF DEBUG_SEEKER}
if (Assigned(FilterThing)) then
 Writeln('            ...and against ', FilterThing^.Value.GetDefiniteName(nil))
else
 Writeln('            ...and that''s it.')
{$ENDIF}
      end;
      if (Assigned(FilterThing)) then
      begin
         { put the victim's thing back in its list }
         Assert(FilterThing^.Value = VictimThing^.Value);
{$IFDEF DEBUG_SEEKER} Writeln('  Filter(): ', VictimThing^.Value.GetDefiniteName(nil), ' is IN'); {$ENDIF}
         LastNext^ := VictimThing;
         LastNext := @VictimThing^.Next;
         VictimThing := VictimThing^.Next;
         LastNext^ := nil;
      end
      else
      begin
         { get rid of this thing's entry }
{$IFDEF DEBUG_SEEKER} Writeln('  Filter(): ', VictimThing^.Value.GetDefiniteName(nil), ' is OUT'); {$ENDIF}
         CondemnedThing := VictimThing;
         VictimThing := VictimThing^.Next;
         Dispose(CondemnedThing);
      end;
   end;
   if (not Assigned(Victim.FThings)) then
      ReportNothingLeft();
   Victim.FInputFragment := Victim.FInputFragment + ' ' + GetFragmentAnnotation();
end;

procedure TUnionFilterClause.ReportNothingLeft();
begin
   Fail('It''s not clear to what you are referring.');
end;


procedure TThatIsClause.CheckContext();
begin
   inherited;
   if (not (cfSingular in FFlags)) then
      ReportNotSingular();
   if ((FSelectionMechanism = tsmPickOnlyNumber) and (FNumber <> 1)) then
      ReportExplicitNumber();
end;

procedure TThatIsClause.ReportNoVictims();
begin
   { e.g. "take all that is blue that is red" }
   Fail('You used the term "that is" in a way I don''t understand.');
end;

procedure TThatIsClause.ReportNotSingular();
begin
   { e.g. "take fruit that is apples" }
   { e.g. "take the apple that is the blue one" }
   Fail('You used the term "that is" in a way I don''t understand.');
end;

procedure TThatIsClause.ReportExplicitNumber();
begin
   { e.g. "take fruit that is the three apples" }
   Fail('You used the term "that is" in a way I don''t understand.');
end;

function TThatIsClause.GetFragmentAnnotation(): AnsiString;
begin
   Result := 'that is ' + FInputFragment;
end;

function TThatIsClause.GetSelectionMechanism(): TThingSelectionMechanism;
begin
   Result := tsmPickAll;
end;


procedure TExclusionFilteringClause.Filter(Victim: TAbstractClause);
var
   VictimThing, CondemnedThing, FilterThing: PThingItem;
   LastNext: PPThingItem;
begin
   Assert(not (cfFakeEverything in Victim.FFlags));
   VictimThing := Victim.FThings;
   Victim.FThings := nil;
   LastNext := @Victim.FThings;
   while (Assigned(VictimThing)) do
   begin
      FilterThing := FThings;
      while (Assigned(FilterThing) and (FilterThing^.Value <> VictimThing^.Value)) do
      begin
         FilterThing := FilterThing^.Next;
      end;
      if (not Assigned(FilterThing)) then
      begin
         { put the victim's thing back in its list }
         LastNext^ := VictimThing;
         LastNext := @VictimThing^.Next;
         VictimThing := VictimThing^.Next;
         LastNext^ := nil;
      end
      else
      begin
         { get rid of this thing's entry }
         Assert(FilterThing^.Value = VictimThing^.Value);
         CondemnedThing := VictimThing;
         VictimThing := VictimThing^.Next;
         Dispose(CondemnedThing);
      end;
   end;
   if (not Assigned(Victim.FThings)) then
      ReportNothingLeft();
   Victim.FInputFragment := Victim.FInputFragment + ' ' + GetFragmentAnnotation();
end;

procedure TExclusionFilteringClause.ReportNothingLeft();
begin
   Fail('It''s not clear to what you are referring.');
end;


function TExceptionFilteringClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := Peer is TAbstractPlainJoiningClause;
end;


procedure TButClause.ReportNoVictims();
begin
   { e.g. "take all but knife but fork" }
   Fail('You used the term "but" in a way I don''t understand.');
end;

function TButClause.GetFragmentAnnotation(): AnsiString;
begin
   Result := 'but ' + FInputFragment;
end;


procedure TStartClause.CheckContext();
begin
   inherited;
   Assert(not Assigned(FPrevious));
end;

function TStartClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := Peer is TAbstractPlainJoiningClause;
end;

function TStartClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   Result := inherited;
   CanContinue := False;
end;

procedure TStartClause.Bank(var Things: PThingItem);
begin
   Things := FThings;
   FThings := nil;
end;

class function TStartClause.RequiresMatch(): Boolean;
begin
   Result := True;
end;

class function TStartClause.CanFail(): Boolean;
begin
   Result := True;
end;


function TThingCollector.Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TClauseKinds; Verb: AnsiString): Boolean;

   procedure Deduplicate(var Things: PThingItem);
   var
      CurrentThing, CompareThing, PreviousThing: PThingItem;
   begin
      { if this ends up being a bottleneck, it can probably be sped up using a hash table instead of rewalking the list each time }
      if (Assigned(Things)) then
      begin
         PreviousThing := Things;
         CurrentThing := Things^.Next;
         while (Assigned(CurrentThing)) do
         begin
            CompareThing := Things;
            while (CompareThing <> CurrentThing) do
            begin
               if (CompareThing^.Value = CurrentThing^.Value) then
               begin
                  PreviousThing^.Next := CurrentThing^.Next;
                  Dispose(CurrentThing);
                  CurrentThing := PreviousThing;
                  Break;
               end;
               CompareThing := CompareThing^.Next;
            end;
            PreviousThing := CurrentThing;
            CurrentThing := CurrentThing^.Next;
         end;
      end;
   end;

   procedure Collapse(FirstClause: TAbstractClause; out Things: PThingItem; out Disambiguate: Boolean);
   var
      CurrentClause, LastClause: TAbstractClause;
   begin
      Assert(FirstClause is TStartClause);
      CurrentClause := FirstClause;
      Things := nil;
      Disambiguate := False;
      repeat
         CurrentClause.CheckContext();
         LastClause := CurrentClause;
         CurrentClause := CurrentClause.FNext;
      until not Assigned(CurrentClause);
      CurrentClause := LastClause;
      repeat
         try
            if (CurrentClause.Select()) then
               Disambiguate := True;
            CurrentClause.Process();
         except
            on E: EMatcherException do { raised by Select() }
               Fail(E.Message(Perspective, CurrentClause.FInputFragment));
         end;
         CurrentClause := CurrentClause.FPrevious;
      until not Assigned(CurrentClause);
      (FirstClause as TStartClause).Bank(Things);
      Deduplicate(Things);
   end;

var
   CurrentToken: Cardinal;
   FirstClause, LastClause: TAbstractClause;

   procedure AppendClause(Clause: TAbstractClause);
   begin
{$IFDEF DEBUG_SEEKER} Writeln('AppendClause(', Clause.ClassName, ') called'); {$ENDIF}
      if (not Assigned(FirstClause)) then
         FirstClause := Clause
      else
         LastClause.Add(Clause);
      LastClause := Clause;
   end;

   function CollectArticleAndThings(ClauseClass: TAbstractClauseClass; ClauseLength: Cardinal): Boolean;
   var
      ClauseStart: Cardinal;

      function CollectExplicitThings(ExplicitGrammaticalNumber: TGrammaticalNumber; Number: Cardinal; SingularThingSelectionMechanism, PluralThingSelectionMechanism: TThingSelectionMechanism; AllowExceptions, CanFail: Boolean): Boolean;
      var
         FromOutside: Boolean;
         Start, Index: Cardinal;
         Flags: TClauseFlags;
         Message: AnsiString;
      begin
{$IFDEF DEBUG_SEEKER} Writeln('    CollectExplicitThings():'); {$ENDIF}
{$IFDEF DEBUG_SEEKER} Writeln('       CalFail=', CanFail); {$ENDIF}
         Assert((ExplicitGrammaticalNumber <> []));
         Assert(not Assigned(FCurrentBestThingList));
         Start := CurrentToken;
         if (CurrentToken < Length(Tokens)) then
         begin
{$IFDEF DEBUG_SEEKER} Writeln('      examining some tokens... starting with "', Tokens[CurrentToken] , '"'); {$ENDIF}
            FCurrentPreferredGrammaticalNumber := ExplicitGrammaticalNumber;
            FCurrentBestLength := 0;
            FCurrentBestGrammaticalNumber := [];
            FCurrentBestThingList := nil;
            try
               Perspective.GetSurroundingsRoot(FromOutside).AddExplicitlyReferencedThings(Tokens, CurrentToken, Perspective, FromOutside, @ReferencedCallback);
            except
               FreeThingList(FCurrentBestThingList);
               FCurrentBestThingList := nil;
               raise;
            end;
            if (Assigned(FCurrentBestThingList)) then
            begin
               Inc(CurrentToken, FCurrentBestLength);
               Assert(CurrentToken > Start);
               if (CurrentToken < Length(Tokens)) then
               begin
                  if (gnSingular in FCurrentBestGrammaticalNumber) then
                  begin
                     if (Tokens[CurrentToken] = 'one') then
                     begin
                        Inc(CurrentToken);
                        FCurrentBestGrammaticalNumber := [gnSingular];
                     end
                     else
                     if (Tokens[CurrentToken] = 'ones') then
                     begin
                        Inc(CurrentToken);
                        FCurrentBestGrammaticalNumber := [gnPlural];
                     end;
                  end;
               end;
               if (gnPlural in FCurrentBestGrammaticalNumber) then { take apples }
               begin
                  Assert(Assigned(FCurrentBestThingList));
                  Flags := [];
                  if (AllowExceptions) then
                     Include(Flags, cfAllowExceptions);
                  if (gnSingular in FCurrentBestGrammaticalNumber) then
                     Include(Flags, cfSingular);
                  AppendClause(ClauseClass.Create(Number, PluralThingSelectionMechanism, Flags, FCurrentBestThingList, Serialise(OriginalTokens, Start, CurrentToken - Start)));
                  Result := True;
               end
               else { take apple }
               begin
                  Assert(gnSingular in FCurrentBestGrammaticalNumber);
                  Assert(Assigned(FCurrentBestThingList));
                  AppendClause(ClauseClass.Create(Number, SingularThingSelectionMechanism, [cfSingular], FCurrentBestThingList, Serialise(OriginalTokens, Start, CurrentToken - Start)));
                  Result := True;
               end;
               FCurrentBestThingList := nil;
            end
            else
            begin
               Assert(Start = CurrentToken);
               if (not CanFail) then
               begin
                  if (ClauseClass.RequiresMatch()) then
                     Fail('I can''t see any "' + Serialise(OriginalTokens, CurrentToken, 1) + '" here to ' + Verb + '.')
                  else
                     Fail('I was with you until you said "' + Serialise(OriginalTokens, ClauseStart, CurrentToken - ClauseStart + 1) + '".');
               end
               else
               begin
                  Result := False;
               end;
            end;
         end
         else
         begin
            Message := Capitalise(Serialise(Tokens, ClauseStart, CurrentToken - ClauseStart)) + ' what?';
            for Index := ClauseStart to CurrentToken-1 do
            begin
               { these are, more or less by definition, guaranteed to be keywords we have hard-coded below }
               if (Tokens[Index] = ',') then
               begin
                  Message := 'I don''t understand your use of commas.';
                  Break;
               end;
               // add other punctuation we support later (e.g. "+", "-", whatever)
            end;
            Fail(Message);
         end;
         Assert(CanFail or Result);
{$IFDEF DEBUG_SEEKER} Writeln('    CollectExplicitThings() finished, Result=', Result); {$ENDIF}
      end;

      function CollectImplicitThings(Flags: TClauseFlags): Boolean;
      var
         FromOutside: Boolean;
      begin
         Assert(not Assigned(FCurrentBestThingList));
         Assert(cfAllowExceptions in Flags); // if we don't end up removing this assertion, then we should just automatically add cfAllowExceptions to Flags here; if we do, then we should revisit whether 'but' is handled correctly with the cases were this is not set
         if (aisSurroundings in Scope) then
         begin
            Perspective.GetSurroundingsRoot(FromOutside).AddImplicitlyReferencedThings(Perspective, FromOutside, aisSelf in Scope, tpExplicit, [], FCurrentBestThingList);
            AppendClause(ClauseClass.CreateAll(FCurrentBestThingList, Flags));
            Result := True;
         end
         else
         if (aisSelf in Scope) then
         begin
            Perspective.AddImplicitlyReferencedThings(Perspective, True, True, tpExplicit, [], FCurrentBestThingList);
            AppendClause(ClauseClass.CreateAll(FCurrentBestThingList, Flags));
            Result := True;
         end
         else
         begin
            { we can only reach here if the caller is expecting only a single thing to be returned and will always fail with multiple things }
            Assert(Scope = []);
            Perspective.GetSurroundingsRoot(FromOutside).AddImplicitlyReferencedThings(Perspective, FromOutside, True, tpExplicit, [], FCurrentBestThingList);
            AppendClause(ClauseClass.CreateAll(FCurrentBestThingList, Flags + [cfExpectedSingleThing]));
            Result := True;
         end;
         FCurrentBestThingList := nil;
         Assert(Result);
      end;

      function CollectFakeEverything(Flags: TClauseFlags): Boolean;
      begin
         Assert(cfAllowExceptions in Flags); // if we don't end up removing this assertion, then we should just automatically add cfAllowExceptions to Flags here; if we do, then we should revisit whether 'but' is handled correctly with the cases were this is not set
         if ((not (aisSurroundings in Scope)) and (not (aisSelf in Scope))) then
         begin
            Assert(Scope = []);
            Include(Flags, cfExpectedSingleThing);
         end;
         AppendClause(ClauseClass.CreateAll(nil, Flags + [cfFakeEverything]));
         Result := True;
      end;

   { The following aren't supported yet, but could be if necessary: }
   {   "take all of cake"                   }
   {   "take many of the coins"             }
   {   "take most of the coins"             }
   { The following would require internal changes to support (to track a second number): }
   {   "any of the <number> <things>"       }
   {   "some of the <number> <things>"      }
   {   "one of the <number> <things>"       }
   {   "<number> of the <number> <things>"  }
   {   "many of the <number> <things>"      }
   {   "most of the <number> <things>"      }
   {   "all of the <number> <things>"       }

   var
      Number: Cardinal;
   begin
{$IFDEF DEBUG_SEEKER} Writeln('CollectArticleAndThings() called'); {$ENDIF}
      Assert(not Assigned(FCurrentBestThingList));
      Assert(Assigned(ClauseClass));
      ClauseStart := CurrentToken;
      Inc(CurrentToken, ClauseLength);
      if (TryMatch(CurrentToken, Tokens, ['all', 'of', 'the'])) then
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, True, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, True, False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['all', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickOnlyNumber, tsmPickOnlyNumber, True, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all', 'that', 'is'])) then
      begin
         Result := CollectFakeEverything([cfAllowExceptions, cfSingular]);
         Assert(Result);
         ClauseClass := TThatIsClause;
         Result := CollectExplicitThings([gnSingular], 1, tsmPickAll, tsmPickAll, True, False);
         Assert(Result);
      end
      else
      if (TryMatch(CurrentToken, Tokens, ['all'])) then
      begin
         Result := (((CurrentToken < Length(Tokens)) and (CollectExplicitThings([gnPlural], 1, tsmPickAll, tsmPickAll, True, True))) or
                    (CollectImplicitThings([cfAllowExceptions, cfSingular])));
      end
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, True, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'one']) or TryMatch(CurrentToken, Tokens, ['any', '1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, True, False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['any', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, True, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any'])) then
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickNumber, tsmPickAll, True, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['an'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, False, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['a'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, False, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['everything', 'that', 'is'])) then
      begin
         Result := CollectFakeEverything([cfAllowExceptions, cfSingular]);
         Assert(Result);
         ClauseClass := TThatIsClause;
         Result := CollectExplicitThings([gnSingular], 1, tsmPickAll, tsmPickAll, True, False);
         Assert(Result);
      end
      else
      if (TryMatch(CurrentToken, Tokens, ['everything'])) then
         Result := CollectImplicitThings([cfAllowExceptions, cfSingular])
      else
      if (TryMatch(CurrentToken, Tokens, ['every'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickAll, tsmPickAll, True, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one', 'of', 'the']) or TryMatch(CurrentToken, Tokens, ['1', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, False, False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#', 'of', 'the'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, False, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one']) or TryMatch(CurrentToken, Tokens, ['1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, False, False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, False, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['the', 'one']) or TryMatch(CurrentToken, Tokens, ['the', '1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickOnlyNumber, tsmPickOnlyNumber, False, False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['the', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickOnlyNumber, tsmPickOnlyNumber, False, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['the'])) then
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyNumber, tsmPickAll, False, False)
      else
      if (TryMatch(CurrentToken, Tokens, ['some'])) then
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickNumber, tsmPickSome, False, False)
      else
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyNumber, tsmPickAll, False, ClauseClass.CanFail());
      if (not Result) then
         Dec(CurrentToken, ClauseLength);
      Assert(not Assigned(FCurrentBestThingList));
{$IFDEF DEBUG_SEEKER} Writeln(' = CollectArticleAndThings() returned ', Result); {$ENDIF}
   end;

   function TryClauses(out NextClauseClass: TAbstractClauseClass; out NextClauseLength: Cardinal): Boolean;
   const
      MaxTokensInClause = 3;

      function TryClause(CandidateTokens: array of AnsiString; out NextClauseLength: Cardinal): Boolean;
      begin
         Assert(CandidateTokens[MaxTokensInClause] = '');
         Assert(CandidateTokens[0] <> '');
         NextClauseLength := 0;
         while ((CandidateTokens[NextClauseLength] <> '') and
                (CurrentToken + NextClauseLength < Length(Tokens)) and
                (Tokens[CurrentToken + NextClauseLength] = CandidateTokens[NextClauseLength])) do
         begin
{$IFDEF DEBUG_SEEKER} Writeln('looking at token "', Tokens[CurrentToken + NextClauseLength], '"'); {$ENDIF}
            Inc(NextClauseLength);
         end;
         Result := CandidateTokens[NextClauseLength] = '';
      end;

   type
      TClauseConfigurationIndex = Low(TClauseKind)..Pred(High(TClauseKind));
      TClauseConfiguration = record
         Tokens: array[0..MaxTokensInClause] of AnsiString;
         ClauseClass: TAbstractClauseClass;
         ClauseKind: TClauseKind;
      end;
   const
      EOT = ''; _ = EOT; { to make the constant below prettier }
      { order by length of Tokens, then alphabetically }
      ClauseConfigurations: array[TClauseConfigurationIndex] of TClauseConfiguration = (
         (Tokens: (',', 'and',                     _,EOT); ClauseClass: TAndClause;       ClauseKind: ckCommaAnd),
         (Tokens: ('that', 'is',                   _,EOT); ClauseClass: TThatIsClause;    ClauseKind: ckThatIs),
         (Tokens: (',',                          _,_,EOT); ClauseClass: TCommaClause;     ClauseKind: ckComma),
         (Tokens: ('and',                        _,_,EOT); ClauseClass: TAndClause;       ClauseKind: ckAnd),
         (Tokens: ('but',                        _,_,EOT); ClauseClass: TButClause;       ClauseKind: ckBut)
      );
   var
      Index: TClauseConfigurationIndex;
      {$IFOPT C+} TokenIndex, LastLength: Cardinal; {$ENDIF}
      {$IFOPT C+} FoundEOT: Boolean; {$ENDIF}
   begin
{$IFOPT C+}
LastLength := MaxTokensInClause;
for Index := Low(ClauseConfigurations) to High(ClauseConfigurations) do
begin
   Assert(ClauseConfigurations[Index].Tokens[0] <> EOT);
   Assert(ClauseConfigurations[Index].Tokens[MaxTokensInClause] = EOT);
   FoundEOT := False;
   for TokenIndex := 1 to MaxTokensInClause-1 do
   begin
      if (FoundEOT) then
      begin
         Assert(ClauseConfigurations[Index].Tokens[TokenIndex] = EOT);
      end
      else
      begin
         Assert(TokenIndex <= LastLength);
         if (ClauseConfigurations[Index].Tokens[TokenIndex] = EOT) then
         begin
            FoundEOT := True;
            LastLength := TokenIndex;
         end;
      end;
   end;
end;
{$ENDIF}
      for Index := Low(ClauseConfigurations) to High(ClauseConfigurations) do
      begin
         Assert(ClauseConfigurations[Index].ClauseKind <> ckStart);
         if (not (ClauseConfigurations[Index].ClauseKind in Ends)) then
         begin
{$IFDEF DEBUG_SEEKER} Writeln('trying clause ', ClauseConfigurations[Index].ClauseKind); {$ENDIF}
            if (TryClause(ClauseConfigurations[Index].Tokens, NextClauseLength)) then
            begin
               Result := True;
               NextClauseClass := ClauseConfigurations[Index].ClauseClass;
{$IFDEF DEBUG_SEEKER} Writeln('  found! clause ', ClauseConfigurations[Index].ClauseKind, ' with length ', NextClauseLength, ' and class ', NextClauseClass.ClassName); {$ENDIF}
               Exit;
            end;
         end;
      end;
{$IFDEF DEBUG_SEEKER} Writeln('no clause found'); {$ENDIF}
      Result := False;
   end;

var
   NextClauseClass: TAbstractClauseClass;
   NextClauseLength: Cardinal;
begin
{$IFDEF DEBUG_SEEKER} Writeln('collecting for: "' + Serialise(OriginalTokens, Start, 1) + '" of "' + Serialise(OriginalTokens, 0, Length(OriginalTokens)) + '"'); {$ENDIF}
   {$IFOPT C+}
   Assert(not FCurrentlyCollecting);
   Assert(not FCollected);
   Assert(FTokenCount = 0);
   Assert(not FDisambiguate);
   Assert(not Assigned(FThingList));
   FCurrentlyCollecting := True;
   Result := False;
   try
   {$ENDIF}
      CurrentToken := Start;
      FirstClause := nil;
      try
         if (CollectArticleAndThings(TStartClause, 0)) then
         begin
            Assert(Assigned(FirstClause));
            repeat until ((CurrentToken > Length(Tokens)) or
                          (not TryClauses(NextClauseClass, NextClauseLength)) or
                          (not CollectArticleAndThings(NextClauseClass, NextClauseLength)));
            Assert(FTokenCount = 0);
            Assert(not FDisambiguate);
            Assert(not Assigned(FThingList));
            Assert(Assigned(FirstClause));
            try
               Collapse(FirstClause, FThingList, FDisambiguate);
               FTokenCount := CurrentToken - Start;
            except
               FTokenCount := 0;
               FDisambiguate := False;
               FreeThingList(FThingList);
               FThingList := nil;
               raise;
            end;
            Result := True;
         end
         else
         begin
            Result := False;
            Assert(FTokenCount = 0);
            Assert(not FDisambiguate);
            Assert(not Assigned(FThingList));
         end;
      finally
         FirstClause.Free();
      end;
   {$IFOPT C+}
{$IFDEF DEBUG_SEEKER}
Writeln('collecting successful; result = ', Result);
{$ENDIF}
   finally
{$IFDEF DEBUG_SEEKER}
Writeln('collecting complete.');
Writeln();
{$ENDIF}
      FCurrentlyCollecting := False;
      FCollected := Result;
      if (not FCollected) then
      begin
         Assert(FTokenCount = 0);
         Assert(not FDisambiguate);
         Assert(not Assigned(FThingList));
      end;
   end;
   {$ENDIF}
end;

function TThingCollector.GetTokenCount(): Cardinal;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FTokenCount;
end;

function TThingCollector.GetDisambiguate(): Boolean;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FDisambiguate;
end;

function TThingCollector.GetThingList(): PThingItem;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FThingList;
   FTokenCount := 0;
   FDisambiguate := False;
   FThingList := nil;
   {$IFOPT C+} FCollected := False; {$ENDIF}
end;

end.
