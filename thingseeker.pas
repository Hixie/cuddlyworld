{$MODE OBJFPC} { -*- text -*- }

//{$DEFINE DEBUG_SEEKER}

{$INCLUDE settings.inc}
unit thingseeker;

interface

uses
   world, grammarian;

type
   TAllImpliedScope = set of (aisSurroundings, aisSelf);
   TEndingClauseKind = (eckNormal{, eckIn});
   TEndingClauseKinds = set of TEndingClauseKind;

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
      function Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TEndingClauseKinds; Verb: AnsiString): Boolean;
      function GetTokenCount(): Cardinal;
      function GetDisambiguate(): Boolean;
      function GetThingList(): PThingItem; { must be called exactly once after Collect() returns true }
   end;

implementation

uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils;

type
   TThingSelectionMechanism = (tsmPickAll, tsmPickOne, tsmPickNumber, tsmPickSome, tsmPickOnlyNumber, tsmPickOnlyRelevantNumber);
   TClauseFlags = set of (cfAllowExceptions, cfSingular, cfDisambiguateLoneResult, cfHaveThatIsClause, cfRemoveHiddenThings);

type
   TAbstractJoiningClause = class;
   TAbstractClause = class
     protected
      FFlags: TClauseFlags;
      FNumber: Cardinal;
      FSelectionMechanism: TThingSelectionMechanism;
      FThings: PThingItem;
      FInputFragment: AnsiString;
      FNext: TAbstractClause;
      FPrevious: TAbstractClause;
      procedure Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); virtual;
      function IsPlainClause(): Boolean; virtual;
      function GetPreviousOpenClause(): TAbstractClause; virtual;
     public
      constructor Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; Things: PThingItem; InputFragment: AnsiString); virtual;
      destructor Destroy(); override;
      procedure CheckContext(); virtual;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; virtual; abstract;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); virtual; abstract;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; virtual;
      procedure SelfCensor(Whitelist: PThingItem); virtual;
      function Select(): Boolean; virtual;
      procedure Process(); virtual; abstract;
      procedure Add(Next: TAbstractClause);
      class function GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber; virtual;
      class function CanFail(): Boolean; virtual; { return True if the clause could also be a preposition or action join (e.g. "from", "and") }
      class procedure ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString); virtual;
   end;
   TAbstractClauseClass = class of TAbstractClause;

   TAbstractJoiningClause = class(TAbstractClause)
     protected
      FJoinedTo: TAbstractClause;
      function IsPlainClause(): Boolean; override;
      function GetPreviousOpenClause(): TAbstractClause; override;
     public
      procedure CheckContext(); override;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); override;
      procedure Process(); override;
   end;
   TAndClause = class(TAbstractJoiningClause)
      class function CanFail(): Boolean; override;
   end;
   TAndFromClause = class(TAbstractJoiningClause)
   end;

   TJoinableClause = class(TAbstractClause)
     protected
      FRegisteredJoins: array of TAbstractJoiningClause;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); override;
      procedure Process(); override;
   end;

   TAbstractFilteringClause = class(TJoinableClause)
     protected
      FVictims: array of TAbstractClause;
      function GetPreviousOpenClause(): TAbstractClause; override;
      procedure Filter(Victim: TAbstractClause); virtual; abstract;
      procedure Victimise(Clause: TAbstractClause); virtual;
     public
      procedure CheckContext(); override;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure ReportNoVictims(); virtual; abstract;
      procedure Process(); override;
      function GetFragmentAnnotation(): AnsiString; virtual; abstract;
      class function GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber; override;
   end;

   TInclusionFilterClause = class(TAbstractFilteringClause)
     protected
      procedure Filter(Victim: TAbstractClause); override;
      function IsMatch(Candidate, Condition: TThing): Boolean; virtual; abstract;
     public
      procedure ReportNothingLeft(); virtual;
   end;
   TAbstractThatIsClause = class(TInclusionFilterClause)
     protected
      procedure Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      function IsMatch(Candidate, Condition: TThing): Boolean; override;
      procedure Victimise(Clause: TAbstractClause); override;
     public
      procedure CheckContext(); override;
      procedure ReportNotSingular(); virtual; abstract;
      procedure ReportExplicitNumber(); virtual; abstract;
   end;
   TThatIsClause = class(TAbstractThatIsClause)
      procedure ReportNoVictims(); override;
      procedure ReportNotSingular(); override;
      procedure ReportExplicitNumber(); override;
      function GetFragmentAnnotation(): AnsiString; override;
   end;
   TAndThatIsClause = class(TAbstractThatIsClause)
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure ReportNoVictims(); override;
      procedure ReportNotSingular(); override;
      procedure ReportExplicitNumber(); override;
      function GetFragmentAnnotation(): AnsiString; override;
   end;
   TFromClause = class(TInclusionFilterClause)
     protected
      procedure Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      function IsMatch(Candidate, Condition: TThing): Boolean; override;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure ReportNoVictims(); override;
      function GetFragmentAnnotation(): AnsiString; override;
   end;

   TExclusionFilteringClause = class(TAbstractFilteringClause)
     protected
      procedure Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
     public
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
     protected
      function IsPlainClause(): Boolean; override;
     public
      procedure CheckContext(); override;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure Process(); override;
      procedure Bank(var Things: PThingItem; var Disambiguate: Boolean);
      class function CanFail(): Boolean; override;
      class procedure ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString); override;
   end;

   EMatcherException = class
      FClause: TAbstractClause;
      constructor Create(Clause: TAbstractClause);
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; virtual; abstract;
   end;
   ECountWrong = class(EMatcherException)
      FWanted, FGot: Cardinal;
      constructor Create(Clause: TAbstractClause; Wanted, Got: Cardinal);
   end;
   ESelectNotEnough = class(ECountWrong)
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; override;
   end;
   ESelectTooMany = class(ECountWrong)
      function Message(Perspective: TAvatar; Input: AnsiString): AnsiString; override;
   end;

constructor EMatcherException.Create(Clause: TAbstractClause);
begin
   inherited Create();
   FClause := Clause;
end;

constructor ECountWrong.Create(Clause: TAbstractClause; Wanted, Got: Cardinal);
begin
   inherited Create(Clause);
   FWanted := Wanted;
   FGot := Got;
end;

function ESelectNotEnough.Message(Perspective: TAvatar; Input: AnsiString): AnsiString;
begin
   Assert(Length(Input) > 0);
   Assert(FGot > 0);
   Assert(FWanted > FGot);
   Result := 'About the ' + NumberToEnglish(FWanted) + ' "' + Input + '"... I can only find ' + NumberToEnglish(FGot) + ': ' + ThingListToLongDefiniteString(FClause.FThings, Perspective, 'and') + '.';
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
var
   PreviousOpen: TAbstractClause;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.AcceptsFilter() called on a ', ClassName, ' for a peer that is a ', Peer.ClassName); {$ENDIF}
   if (Peer is TExceptionFilteringClause) then
   begin
      Result := cfAllowExceptions in FFlags;
      PreviousOpen := GetPreviousOpenClause();
      CanContinue := (not Result) or (Assigned(PreviousOpen) and ((Self is TAbstractFilteringClause) = (PreviousOpen is TAbstractFilteringClause)));
   end
   else
   if (Peer is TThatIsClause) then
   begin
      Result := cfSingular in FFlags;
      CanContinue := False;
   end
   else
   if (Peer is TAndThatIsClause) then
   begin
      Result := cfHaveThatIsClause in FFlags;
      CanContinue := not IsPlainClause();
   end
   else
   if (Peer is TFromClause) then
   begin
      Result := IsPlainClause();
      CanContinue := True;
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

var
   SelectionMechanism: TThingSelectionMechanism;
begin
{$IFDEF DEBUG_SEEKER} Writeln('Select() called for ', ClassName(), ': ', FSelectionMechanism, '; number=', FNumber); {$ENDIF}
   Assert((FSelectionMechanism <> tsmPickOnlyRelevantNumber) or (FNumber = 1));
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
         raise ESelectNotEnough.Create(Self, FNumber, Count);
   end;
   SelectionMechanism := FSelectionMechanism;
   Preselect(Count, SelectionMechanism);
   case SelectionMechanism of
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
    tsmPickOnlyNumber, tsmPickOnlyRelevantNumber:
       begin
          Assert((SelectionMechanism <> tsmPickOnlyRelevantNumber) or (FNumber = 1));
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
{$IFDEF DEBUG_SEEKER} Writeln('Select() resulted in the following list: ', ThingListToLongDefiniteString(FThings, nil, 'and')); {$ENDIF}
end;

procedure TAbstractClause.SelfCensor(Whitelist: PThingItem);
var
   CurrentThing, CondemnedThing, FilterThing: PThingItem;
   LastNext: PPThingItem;
begin
   CurrentThing := FThings;
   FThings := nil;
   LastNext := @FThings;
   while (Assigned(CurrentThing)) do
   begin
      FilterThing := Whitelist;
      while (Assigned(FilterThing) and (CurrentThing^.Value <> FilterThing^.Value)) do
         FilterThing := FilterThing^.Next;
      if (Assigned(FilterThing)) then
      begin
         { found it, keep it }
         LastNext^ := CurrentThing;
         LastNext := @CurrentThing^.Next;
         CurrentThing := CurrentThing^.Next;
         LastNext^ := nil;
      end
      else
      begin
         { get rid of this thing's entry }
         CondemnedThing := CurrentThing;
         CurrentThing := CurrentThing^.Next;
         Dispose(CondemnedThing);
      end;
   end;
end;

procedure TAbstractClause.Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
end;

procedure TAbstractClause.Add(Next: TAbstractClause);
begin
   Assert(Assigned(Next));
   Assert(not Assigned(FNext));
   Assert(not Assigned(Next.FPrevious));
   FNext := Next;
   Next.FPrevious := Self;
end;

function TAbstractClause.IsPlainClause(): Boolean;
begin
   Result := False;
end;

function TAbstractClause.GetPreviousOpenClause(): TAbstractClause;
begin
   Result := FPrevious;
end;

class function TAbstractClause.GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber;
begin
   Result := DefaultGrammaticalNumber;
end;

class function TAbstractClause.CanFail(): Boolean;
begin
   Result := False;
end;

class procedure TAbstractClause.ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString);
begin
   Assert(ClauseStart <> CurrentToken);
   Fail('I was with you until you said "' + Serialise(Tokens, ClauseStart, CurrentToken - ClauseStart + 1) + '".');
end;


procedure TAbstractJoiningClause.CheckContext();
var
   EarlierClause: TAbstractClause;
begin
   inherited;
   EarlierClause := FPrevious;
   while (not EarlierClause.AcceptsJoins(Self)) do
   begin
      EarlierClause := EarlierClause.GetPreviousOpenClause();
      Assert(Assigned(EarlierClause));
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

function TAbstractJoiningClause.IsPlainClause(): Boolean;
begin
   Assert(Assigned(FJoinedTo));
   Result := FJoinedTo.IsPlainClause();
end;

function TAbstractJoiningClause.GetPreviousOpenClause(): TAbstractClause;
begin
   Assert(Assigned(FJoinedTo));
   Result := FJoinedTo;
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
   if (Length(FRegisteredJoins) > 0) then
   begin
      CurrentThing := FThings;
      for Index := Low(FRegisteredJoins) to High(FRegisteredJoins) do
      begin
         if (Assigned(FRegisteredJoins[Index].FThings)) then
         begin
            if (Assigned(CurrentThing)) then
            begin
               while (Assigned(CurrentThing^.Next)) do
                  CurrentThing := CurrentThing^.Next;
               CurrentThing^.Next := FRegisteredJoins[Index].FThings;
            end;
            CurrentThing := FRegisteredJoins[Index].FThings;
            if (not Assigned(FThings)) then
               FThings := CurrentThing;
         end;
         FRegisteredJoins[Index].FThings := nil;
      end;
   end;
end;

function TJoinableClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := False;
end;

procedure TJoinableClause.RegisterJoin(Peer: TAbstractJoiningClause);
begin
   SetLength(FRegisteredJoins, Length(FRegisteredJoins)+1);
   FRegisteredJoins[High(FRegisteredJoins)] := Peer;
end;


procedure TAbstractFilteringClause.CheckContext();
var
   CurrentClause, SkipUntilClause: TAbstractClause;
   Continue: Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() called on a ', ClassName); {$ENDIF}
   inherited;
   CurrentClause := FPrevious;
   SkipUntilClause := CurrentClause;
   Continue := True;
   while (Continue) do
   begin
{$IFDEF DEBUG_SEEKER} Writeln('   examining clause that is a ', CurrentClause.ClassName); {$ENDIF}
      // we'll have to add some special magic here to handle "and that is" clauses
      // so that "take arch and that is blue" doesn't work but "take arch that is red and that is blue" does
      // (also once "all that is" is considered a simple prefix clause type, we need to handle that)
      if (CurrentClause.AcceptsFilter(Self, Continue)) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('   they said yes, continue=', Continue); {$ENDIF}
         if (CurrentClause = SkipUntilClause) then
            Victimise(CurrentClause)
{$IFDEF DEBUG_SEEKER} else Writeln('   ...but we''re skipping them') {$ENDIF};
      end
{$IFDEF DEBUG_SEEKER} else Writeln('   they said no, continue=', Continue) {$ENDIF};
      if (SkipUntilClause = CurrentClause) then
         SkipUntilClause := CurrentClause.GetPreviousOpenClause();
      CurrentClause := CurrentClause.FPrevious;
      Assert((not Continue) or (Assigned(CurrentClause)));
{$IFDEF DEBUG_SEEKER} Writeln('   continue=', Continue) {$ENDIF};
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() done'); {$ENDIF}
   if (not (Length(FVictims) > 0)) then
      ReportNoVictims();
end;

procedure TAbstractFilteringClause.Victimise(Clause: TAbstractClause);
begin
   SetLength(FVictims, Length(FVictims)+1);
   FVictims[High(FVictims)] := Clause;
end;

function TAbstractFilteringClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.AcceptsFilter() called on a ', ClassName, ' for a peer that is a ', Peer.ClassName); {$ENDIF}
   if (Peer is ClassType) then
   begin
      Result := False;
      CanContinue := False;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.AcceptsFilter() on a ', ClassName, ' is saying don''t continue.') {$ENDIF};
   end
   else
      Result := inherited;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

function TAbstractFilteringClause.GetPreviousOpenClause(): TAbstractClause;
begin
   Assert(Length(FVictims) > 0);
   Result := FVictims[High(FVictims)];
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


procedure TInclusionFilterClause.Filter(Victim: TAbstractClause);
var
   VictimThing, CondemnedThing, FilterThing: PThingItem;
   LastNext: PPThingItem;
begin
   VictimThing := Victim.FThings;
   Victim.FThings := nil;
   LastNext := @Victim.FThings;
   while (Assigned(VictimThing)) do
   begin
      FilterThing := FThings;
      while (Assigned(FilterThing) and (not IsMatch(VictimThing^.Value, FilterThing^.Value))) do
         FilterThing := FilterThing^.Next;
      if (Assigned(FilterThing)) then
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
         CondemnedThing := VictimThing;
         VictimThing := VictimThing^.Next;
         Dispose(CondemnedThing);
      end;
   end;
   if (not Assigned(Victim.FThings)) then
      ReportNothingLeft();
   if (not (cfRemoveHiddenThings in FFlags)) then
      Exclude(Victim.FFlags, cfRemoveHiddenThings);
   Victim.FInputFragment := Victim.FInputFragment + ' ' + GetFragmentAnnotation();
end;

procedure TInclusionFilterClause.ReportNothingLeft();
begin
   Fail('It''s not clear to what you are referring.');
end;


function TAbstractThatIsClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := Candidate = Condition;
end;

procedure TAbstractThatIsClause.CheckContext();
begin
   inherited;
   if (not (cfSingular in FFlags)) then
      ReportNotSingular();
   if ((FSelectionMechanism = tsmPickOnlyNumber) and (FNumber <> 1)) then
      ReportExplicitNumber();
end;

procedure TAbstractThatIsClause.Victimise(Clause: TAbstractClause);
begin
   inherited;
   Include(Clause.FFlags, cfHaveThatIsClause);
end;

procedure TAbstractThatIsClause.Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
   SelectionMechanism := tsmPickAll;
end;


procedure TThatIsClause.ReportNoVictims();
begin
   { e.g. "take all that is blue that is red" }
   Fail('You used the term "that is" in a way I don''t understand.');
end;

procedure TThatIsClause.ReportNotSingular();
begin
   { e.g. "take a fruit that is apples" }
   { e.g. "take the apple that is the blue ones" }
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


function TAndThatIsClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAndThatIsClause.AcceptsFilter() called on a ', ClassName, ' for a peer that is a ', Peer.ClassName); {$ENDIF}
   if (Peer is TAndThatIsClause) then
   begin
      Result := False;
      CanContinue := True;
   end
   else
      Result := inherited;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

procedure TAndThatIsClause.ReportNoVictims();
begin
   { e.g. "take all and that is blue" }
   Fail('You used the term "and that is" in a way I don''t understand.');
end;

procedure TAndThatIsClause.ReportNotSingular();
begin
   { e.g. "take a fruit that is blue and that is apples" }
   { e.g. "take the apple that is blue and that is the blue ones" }
   Fail('You used the term "and that is" in a way I don''t understand.');
end;

procedure TAndThatIsClause.ReportExplicitNumber();
begin
   { e.g. "take fruit that is blue and that is the three apples" }
   Fail('You used the term "and that is" in a way I don''t understand.');
end;

function TAndThatIsClause.GetFragmentAnnotation(): AnsiString;
begin
   Result := 'and that is ' + FInputFragment;
end;


function TFromClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Assert(Assigned(Candidate));
   Assert(Assigned(Candidate.Parent));
   Assert(Assigned(Condition));
   Result := Candidate.Parent = Condition;
end;

procedure TFromClause.Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
var
   NeedMatching, HaveMatching, VictimIndex: Cardinal;
   VictimThing, FilterThing, BadThings: PThingItem;
   LastNextGood, LastNextBad: PPThingItem;
begin
   case SelectionMechanism of
    tsmPickOne, tsmPickSome: NeedMatching := 1;
    tsmPickNumber, tsmPickOnlyRelevantNumber: NeedMatching := FNumber;
   else
     NeedMatching := 0;
   end;
   Assert(NeedMatching <= Count);
   if ((NeedMatching > 0) and (NeedMatching < Count)) then
   begin
      HaveMatching := 0;
      FilterThing := FThings;
      FThings := nil;
      LastNextGood := @FThings;
      BadThings := nil;
      LastNextBad := @BadThings;
      while (Assigned(FilterThing)) do
      begin
         Assert(Length(FVictims) > 0);
         VictimIndex := Low(FVictims);
         VictimThing := FVictims[VictimIndex].FThings;
         while (Assigned(VictimThing) and (not IsMatch(VictimThing^.Value, FilterThing^.Value))) do
         begin
            VictimThing := VictimThing^.Next;
            if (not Assigned(VictimThing)) then
            begin
               Inc(VictimIndex);
               if (VictimIndex <= High(FVictims)) then
                  VictimThing := FVictims[VictimIndex].FThings;
            end;
         end;
         if (Assigned(VictimThing)) then
         begin
            { this FilterThing is ok, keep it }
            LastNextGood^ := FilterThing;
            LastNextGood := @FilterThing^.Next;
            FilterThing := FilterThing^.Next;
            LastNextGood^ := nil;
            Inc(HaveMatching);
         end
         else
         begin
            { this FilterThing is useless, put it aside for now }
            LastNextBad^ := FilterThing;
            LastNextBad := @FilterThing^.Next;
            FilterThing := FilterThing^.Next;
            LastNextBad^ := nil;
         end;
      end;
      if (not Assigned(FThings)) then
      begin
         FreeThingList(BadThings);
         ReportNothingLeft();
      end;
      Assert(Assigned(FThings)); { because ReportNothingLeft() is supposed to throw an exception }
      if (HaveMatching < NeedMatching) then
      begin
         Assert(NeedMatching > 1); { if it was 1, then we must have 0; if we have 0, FThings would be nil }
         Assert(Assigned(BadThings)); { we must have thrown something away to end up with fewer than we needed }
         LastNextGood^ := BadThings;
      end
      else
      begin
         if (HaveMatching < Count) then
         begin
            Assert(Assigned(BadThings));
            FreeThingList(BadThings);
         end
         else
         begin
            Assert(not Assigned(BadThings));
         end;
         Count := HaveMatching;
      end;
   end;
end;

function TFromClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := Peer is TAndFromClause;
end;

procedure TFromClause.ReportNoVictims();
begin
   { e.g. "take fruit from table from ground" (though we could also just make that work) }
   { e.g. "take fruit from table that is blue from ground" }
   Fail('You used the term "from" in a way I don''t understand.');
end;

function TFromClause.GetFragmentAnnotation(): AnsiString;
begin
   Result := 'from ' + FInputFragment;
end;


procedure TExclusionFilteringClause.Preselect(var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
   if (((SelectionMechanism <> tsmPickNumber) and (SelectionMechanism <> tsmPickOnlyNumber)) or
       ((SelectionMechanism <> tsmPickNumber) and (FNumber = 1))) then
     SelectionMechanism := tsmPickAll;
end;

procedure TExclusionFilteringClause.Filter(Victim: TAbstractClause);
var
   VictimThing, CondemnedThing, FilterThing: PThingItem;
   LastNext: PPThingItem;
begin
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
   Result := Peer is TAndClause;
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
   Result := Peer is TAndClause;
end;

function TStartClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TStartClause.AcceptsFilter() called on a ', ClassName, ' for a peer that is a ', Peer.ClassName); {$ENDIF}
   Result := inherited;
   CanContinue := False;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

procedure TStartClause.Process();
begin
   if (Length(FRegisteredJoins) > 0) then
      Include(FFlags, cfDisambiguateLoneResult);
   inherited;
end;

procedure TStartClause.Bank(var Things: PThingItem; var Disambiguate: Boolean);

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

begin
   Deduplicate(FThings);
   Things := FThings;
   if ((Assigned(FThings)) and (not Assigned(FThings^.Next)) and (cfDisambiguateLoneResult in FFlags)) then
      Disambiguate := True;
   FThings := nil;
end;

function TStartClause.IsPlainClause(): Boolean;
begin
   Result := True;
end;

class function TStartClause.CanFail(): Boolean;
begin
   Result := True;
end;

class procedure TStartClause.ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString);
begin
   Fail('I can''t see any "' + Tokens[CurrentToken] + '" here to ' + Verb + '.')
end;


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

function TThingCollector.Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TEndingClauseKinds; Verb: AnsiString): Boolean;

   procedure Collapse(FirstClause: TAbstractClause; out Things: PThingItem; out Disambiguate: Boolean);
   var
      CurrentClause, LastClause: TAbstractClause;
      FromOutside, GotWhitelist: Boolean;
      Root: TAtom;
      WhiteList: PThingItem;
   begin
{$IFDEF DEBUG_SEEKER} Writeln('Collapsing clauses...'); {$ENDIF}
      Assert(FirstClause is TStartClause);
      CurrentClause := FirstClause;
      Things := nil;
      Disambiguate := False;
{$IFDEF DEBUG_SEEKER} Writeln(' Forwards:'); {$ENDIF}
      repeat
{$IFDEF DEBUG_SEEKER} Writeln('   ', CurrentClause.ClassName); {$ENDIF}
         CurrentClause.CheckContext();
         LastClause := CurrentClause;
         CurrentClause := CurrentClause.FNext;
      until not Assigned(CurrentClause);
      CurrentClause := LastClause;
{$IFDEF DEBUG_SEEKER} Writeln(' In reverse:'); {$ENDIF}
      GotWhitelist := False;
      try
         repeat
   {$IFDEF DEBUG_SEEKER} Writeln('   ', CurrentClause.ClassName); {$ENDIF}
            try
               if ((cfRemoveHiddenThings in CurrentClause.FFlags) and (Scope <> [])) then
               begin
                  if (not GotWhitelist) then
                  begin
                     Whitelist := nil;
                     if (aisSurroundings in Scope) then
                        Root := Perspective.GetSurroundingsRoot(FromOutside)
                     else
                     if (aisSelf in Scope) then
                        Root := Perspective
                     else
                        raise EAssertionFailed.Create('unexpected TAllImpliedScope value');
                     Root.AddImplicitlyReferencedDescendantThings(Perspective, FromOutside, aisSelf in Scope, tpCountsForAll, [], WhiteList);
                     GotWhitelist := True;
                  end;
   {$IFDEF DEBUG_SEEKER} Writeln('      Self Censoring... (Whitelist: ', ThingListToLongDefiniteString(Whitelist, Perspective, 'and'), ')'); {$ENDIF}
                  CurrentClause.SelfCensor(Whitelist);
               end;
               if (CurrentClause.Select()) then
                  Disambiguate := True;
               CurrentClause.Process();
            except
               on E: EMatcherException do { raised by Select() }
                  Fail(E.Message(Perspective, CurrentClause.FInputFragment));
            end;
            CurrentClause := CurrentClause.FPrevious;
         until not Assigned(CurrentClause);
      finally
         if (GotWhitelist) then
            FreeThingList(Whitelist);
      end;
      (FirstClause as TStartClause).Bank(Things, Disambiguate);
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

      function CollectExplicitThings(ExplicitGrammaticalNumber: TGrammaticalNumber; Number: Cardinal; SingularThingSelectionMechanism, PluralThingSelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; CanFail: Boolean): Boolean;
      var
         FromOutside: Boolean;
         Start, Index: Cardinal;
         Message: AnsiString;
      begin
{$IFDEF DEBUG_SEEKER} Writeln('    CollectExplicitThings():'); {$ENDIF}
{$IFDEF DEBUG_SEEKER} Writeln('       CanFail=', CanFail); {$ENDIF}
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
                  if (gnSingular in FCurrentBestGrammaticalNumber) then
                     Include(Flags, cfSingular);
                  AppendClause(ClauseClass.Create(Number, PluralThingSelectionMechanism, Flags, FCurrentBestThingList, Serialise(OriginalTokens, Start, CurrentToken - Start)));
                  Result := True;
               end
               else { take apple }
               begin
                  Assert(gnSingular in FCurrentBestGrammaticalNumber);
                  Assert(Assigned(FCurrentBestThingList));
                  Include(Flags, cfSingular);
                  AppendClause(ClauseClass.Create(Number, SingularThingSelectionMechanism, Flags, FCurrentBestThingList, Serialise(OriginalTokens, Start, CurrentToken - Start)));
                  Result := True;
               end;
               FCurrentBestThingList := nil;
            end
            else
            begin
               Assert(Start = CurrentToken);
               if (not CanFail) then
                  ClauseClass.ReportFailedMatch(OriginalTokens, ClauseStart, CurrentToken, Verb)
               else
                  Result := False;
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
         Perspective.GetSurroundingsRoot(FromOutside).AddImplicitlyReferencedDescendantThings(Perspective, FromOutside, True, tpEverything, [], FCurrentBestThingList);
         AppendClause(ClauseClass.Create(0, tsmPickAll, Flags, FCurrentBestThingList, Serialise(OriginalTokens, ClauseStart, CurrentToken - ClauseStart)));
         FCurrentBestThingList := nil;
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
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['all', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickOnlyNumber, tsmPickOnlyNumber, [cfAllowExceptions, cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all'])) then
      begin
         Result := (((CurrentToken < Length(Tokens)) and
                     (CollectExplicitThings([gnPlural], 1, tsmPickAll, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult], True))) or
                    (CollectImplicitThings([cfAllowExceptions, cfSingular, cfDisambiguateLoneResult, cfRemoveHiddenThings])));
      end
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'one']) or TryMatch(CurrentToken, Tokens, ['any', '1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateLoneResult], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['any', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any'])) then
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['an'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['a'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['everything'])) then
         Result := CollectImplicitThings([cfAllowExceptions, cfSingular, cfDisambiguateLoneResult, cfRemoveHiddenThings])
      else
      if (TryMatch(CurrentToken, Tokens, ['every'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickAll, tsmPickAll, [cfAllowExceptions], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one', 'of', 'the']) or TryMatch(CurrentToken, Tokens, ['1', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#', 'of', 'the'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one']) or TryMatch(CurrentToken, Tokens, ['1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['the', 'one']) or TryMatch(CurrentToken, Tokens, ['the', '1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickOnlyNumber, tsmPickOnlyNumber, [], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['the', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickOnlyNumber, tsmPickOnlyNumber, [], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['the'])) then
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyNumber, tsmPickAll, [], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['some'])) then
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickNumber, tsmPickSome, [cfDisambiguateLoneResult], False)
      else
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyRelevantNumber, tsmPickAll, [], ClauseClass.CanFail());
      if (not Result) then
         Dec(CurrentToken, ClauseLength);
      Assert(not Assigned(FCurrentBestThingList));
{$IFDEF DEBUG_SEEKER} Writeln(' = CollectArticleAndThings() returned ', Result); {$ENDIF}
   end;

   function TryClauses(out NextClauseClass: TAbstractClauseClass; out NextClauseLength: Cardinal): Boolean;
   const
      MaxTokensInClause = 4;

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
      TClauseConfiguration = record
         Tokens: array[0..MaxTokensInClause] of AnsiString;
         ClauseClass: TAbstractClauseClass;
         EndingClauseKind: TEndingClauseKind;
      end;
   const
      EOT = ''; _ = EOT; { to make the constant below prettier }
      { order by length of Tokens, then alphabetically }
      ClauseConfigurations: array[0..9] of TClauseConfiguration = (
         (Tokens: (',', 'and', 'that', 'is',         EOT); ClauseClass: TAndThatIsClause;    EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'from',             _,EOT); ClauseClass: TAndFromClause;      EndingClauseKind: eckNormal),
         (Tokens: ('and', 'that', 'is',            _,EOT); ClauseClass: TAndThatIsClause;    EndingClauseKind: eckNormal),
         (Tokens: (',', 'and',                   _,_,EOT); ClauseClass: TAndClause;          EndingClauseKind: eckNormal),
         (Tokens: ('and', 'from',                _,_,EOT); ClauseClass: TAndFromClause;      EndingClauseKind: eckNormal),
         (Tokens: ('that', 'is',                 _,_,EOT); ClauseClass: TThatIsClause;       EndingClauseKind: eckNormal),
         (Tokens: (',',                        _,_,_,EOT); ClauseClass: TAndClause;          EndingClauseKind: eckNormal),
         (Tokens: ('and',                      _,_,_,EOT); ClauseClass: TAndClause;          EndingClauseKind: eckNormal),
         (Tokens: ('but',                      _,_,_,EOT); ClauseClass: TButClause;          EndingClauseKind: eckNormal),
         (Tokens: ('from',                     _,_,_,EOT); ClauseClass: TFromClause;         EndingClauseKind: eckNormal)
      );
   var
      Index: Cardinal;
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
         if ((not (ClauseConfigurations[Index].EndingClauseKind in Ends)) and
             (TryClause(ClauseConfigurations[Index].Tokens, NextClauseLength))) then
         begin
            Result := True;
            NextClauseClass := ClauseConfigurations[Index].ClauseClass;
            Exit;
         end;
      end;
      Result := False;
   end;

var
   NextClauseClass: TAbstractClauseClass;
   NextClauseLength: Cardinal;
   {$IFDEF DEBUG} OldHeapInfo: THeapInfo; {$ENDIF}
begin
   {$IFDEF DEBUG}
   OldHeapInfo := SetHeapInfo('ThingSeeker: "' + Copy(Serialise(OriginalTokens, 0, Length(OriginalTokens)), 1, HeapInfoSize - 20) + '"');
   try
   {$ENDIF}
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
         {$IFDEF DEBUG_SEEKER} Writeln('collecting starting at ', CurrentToken, ':', Serialise(OriginalTokens, CurrentToken, 1)); {$ENDIF}
         if (CollectArticleAndThings(TStartClause, 0)) then
         begin
            {$IFDEF DEBUG_SEEKER} Writeln('Found a start.'); {$ENDIF}
            Assert(Assigned(FirstClause));
            repeat
               {$IFDEF DEBUG_SEEKER}
               if (CurrentToken < Length(Tokens)) then
                  Writeln('collecting continuing at ', CurrentToken, ':', Serialise(OriginalTokens, CurrentToken, 1))
               else
                  Writeln('collecting continuing at end');
               {$ENDIF}
                   until ((CurrentToken > Length(Tokens)) or
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
   {$IFDEF DEBUG}
   finally
      SetHeapInfo(OldHeapInfo);
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
