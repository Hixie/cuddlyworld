{$MODE OBJFPC} { -*- text -*- }

//{$DEFINE DEBUG_SEEKER}

{$INCLUDE settings.inc}
unit thingseeker;

interface

uses
   world, grammarian;

type
   TAllImpliedScope = set of (aisSurroundings, aisSelf);
   TEndingClauseKind = (eckNormal, eckIn, eckOn);
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
   TClauseFlags = set of (cfAllowExceptions, cfSingular, cfDisambiguateLoneResult, cfHaveThatIsClause, cfRemoveHiddenThings, cfHadArticle);

   TIsMatchFunction = function (Candidate, Condition: TThing): Boolean of object;

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
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); virtual;
      function IsPlainClause(): Boolean; virtual;
      function GetPreviousOpenClause(): TAbstractClause; virtual;
      function GetArticle(): String; virtual;
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
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      function IsPlainClause(): Boolean; override;
      function GetPreviousOpenClause(): TAbstractClause; override;
      function GetFragmentAnnotation(): AnsiString; virtual;
      function GetClausePrefix(): AnsiString; virtual; abstract;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); override;
      procedure Process(); override;
   end;
   TAndClause = class(TAbstractJoiningClause)
     protected
      function GetClausePrefix(): AnsiString; override;
     public
      procedure CheckContext(); override;
      class function CanFail(): Boolean; override;
   end;
   TAbstractJoiningPreconditionFilterClause = class(TAbstractJoiningClause)
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; virtual; abstract;
     public
      procedure CheckContext(); override;
      procedure ReportNoJoinee(); virtual;
   end;
   TAndFromClause = class(TAbstractJoiningPreconditionFilterClause)
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
      function GetClausePrefix(): AnsiString; override;
     public
      class procedure ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString); override;
   end;
   TAndInClause = class(TAbstractJoiningPreconditionFilterClause)
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
      function GetClausePrefix(): AnsiString; override;
     public
      class procedure ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString); override;
   end;
   TAndOnClause = class(TAbstractJoiningPreconditionFilterClause)
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
      function GetClausePrefix(): AnsiString; override;
     public
      class procedure ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString); override;
   end;

   TJoinableClause = class(TAbstractClause)
     protected
      FRegisteredJoins: array of TAbstractJoiningClause;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); override;
   end;

   TAbstractFilteringClause = class(TJoinableClause)
     protected
      FVictims: array of TAbstractClause;
      function GetPreviousOpenClause(): TAbstractClause; override;
      procedure Filter(Victim: TAbstractClause); virtual;
      class function IsMatch(Candidate, Condition: TThing): Boolean; virtual; abstract;
      function KeepMatches(): Boolean; virtual; abstract;
      procedure Victimise(Clause: TAbstractClause); virtual;
      function GetIsMatch(Target: TAbstractClause): TIsMatchFunction; virtual;
      function GetFragmentAnnotation(): AnsiString; virtual;
      function GetClausePrefix(): AnsiString; virtual; abstract;
     public
      procedure CheckContext(); override;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure ReportNoVictims(); virtual;
      procedure ReportNothingLeft(); virtual;
      procedure Process(); override;
      class function GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber; override;
   end;

   TInclusionFilterClause = class(TAbstractFilteringClause)
     protected
      procedure Filter(Victim: TAbstractClause); override;
      function KeepMatches(): Boolean; override;
   end;
   TAbstractThatIsClause = class(TInclusionFilterClause)
     protected
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
      procedure Victimise(Clause: TAbstractClause); override;
     public
      procedure CheckContext(); override;
      procedure ReportNotSingular(); virtual;
      procedure ReportExplicitNumber(); virtual;
   end;
   TThatIsClause = class(TAbstractThatIsClause)
     protected
      function GetClausePrefix(): AnsiString; override;
   end;
   TAndThatIsClause = class(TAbstractThatIsClause)
     protected
      function GetClausePrefix(): AnsiString; override;
     public
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
   end;
   TAbstractPreconditionFilterClause = class(TInclusionFilterClause)
     protected
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      function GetIsMatch(Target: TAbstractClause): TIsMatchFunction; override;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
   end;
   TFromClause = class(TAbstractPreconditionFilterClause)
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
      function GetClausePrefix(): AnsiString; override;
     public
   end;
   TInClause = class(TAbstractPreconditionFilterClause)
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
      function GetClausePrefix(): AnsiString; override;
     public
   end;
   TOnClause = class(TAbstractPreconditionFilterClause)
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
     public
      function GetClausePrefix(): AnsiString; override;
   end;

   TExclusionFilteringClause = class(TAbstractFilteringClause)
     protected
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      function KeepMatches(): Boolean; override;
   end;
   TExceptionFilteringClause = class(TExclusionFilteringClause) { filters that filter the nearest sequence of "all"s }
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
   end;
   TButClause = class(TExceptionFilteringClause)
     protected
      function GetClausePrefix(): AnsiString; override;
     public
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
   Result := 'About the ' + NumberToEnglish(FWanted) + ' ' + Input + '... I can only find ' + NumberToEnglish(FGot) + ': ' + ThingListToLongDefiniteString(FClause.FThings, Perspective, 'and') + '.';
end;

function ESelectTooMany.Message(Perspective: TAvatar; Input: AnsiString): AnsiString;
begin
   Assert(Length(Input) > 0);
   if (FWanted = 1) then
      Result := 'Which ' + Input + ' do you mean, ' + ThingListToLongDefiniteString(FClause.FThings, Perspective, 'or') + '?'
   else
   if (FWanted = 1) then
      Result := 'About the ' + Input + '... I count ' + NumberToEnglish(FGot) + ', not ' + NumberToEnglish(FWanted) + '.'
   else
      Result := 'About the ' + NumberToEnglish(FWanted) + ' ' + Input + '... I count ' + NumberToEnglish(FGot) + ', not ' + NumberToEnglish(FWanted) + '.';
end;


constructor TAbstractClause.Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; Things: PThingItem; InputFragment: AnsiString);
begin
   inherited Create();
   FNumber := Number;
   FSelectionMechanism := SelectionMechanism;
   FFlags := Flags;
   FThings := Things;
   FInputFragment := InputFragment;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.Create(', Number, ', ', SelectionMechanism, ', ..., ', ThingListToLongDefiniteString(Things, nil, 'and'), ', "', InputFragment, '") for ', ClassName); {$ENDIF}
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
   if ((Peer is TFromClause) or (Peer is TInClause) or (Peer is TOnClause)) then
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

function TAbstractClause.GetArticle(): AnsiString;
begin
   if (cfHadArticle in FFlags) then
   begin
      case FSelectionMechanism of
        tsmPickAll: if (cfSingular in FFlags) then Result := '' else Result := 'the';
        tsmPickOne: Result := IndefiniteArticle(FInputFragment);
        tsmPickNumber: if (FNumber = 1) then Result := IndefiniteArticle(FInputFragment) else Result := NumberToEnglish(FNumber);
        tsmPickSome: begin Assert(not (cfSingular in FFlags)); Result := 'some'; end;
        tsmPickOnlyNumber: if (FNumber = 1) then Result := 'the' else Result := 'the ' + NumberToEnglish(FNumber);
        tsmPickOnlyRelevantNumber: begin Assert(cfSingular in FFlags); Result := ''; end;
       else
        raise EAssertionFailed.Create('unknown TSelectionMechanism');
      end;
      if (cfAllowExceptions in FFlags) then
      begin
         if (Result <> '') then
            Result := 'all ' + Result
         else
            Result := 'all';
      end;
   end
   else
      Result := '';
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
   Preselect(Self, Count, SelectionMechanism);
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

procedure TAbstractClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
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


procedure TAbstractJoiningClause.Process();
begin
   Assert(Assigned(FJoinedTo));
   FJoinedTo.RegisterJoin(Self);
end;

procedure TAbstractJoiningClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
   Assert(Assigned(FJoinedTo));
   Assert(Target is TAbstractJoiningClause);
   FJoinedTo.Preselect(Target, Count, SelectionMechanism);
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

function TAbstractJoiningClause.GetFragmentAnnotation(): AnsiString;
var
   Article: AnsiString;
begin
   Article := GetArticle();
   if (Article <> '') then
      Article := Article + ' ';
   Result := GetClausePrefix() + ' ' + Article + FInputFragment;
end;


function TAndClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and';
end;

procedure TAndClause.CheckContext();
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

class function TAndClause.CanFail(): Boolean;
begin
   Result := True;
end;


procedure TAbstractJoiningPreconditionFilterClause.CheckContext();
var
   EarlierClause: TAbstractClause;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractJoiningPreconditionFilterClause.CheckContext() begin'); {$ENDIF}
   inherited;
   EarlierClause := FPrevious;
{$IFDEF DEBUG_SEEKER} Writeln('asking a ' + EarlierClause.ClassName); {$ENDIF}
   while ((Assigned(EarlierClause)) and (not EarlierClause.AcceptsJoins(Self))) do
   begin
{$IFDEF DEBUG_SEEKER} Writeln('got rejected by a ' + EarlierClause.ClassName); {$ENDIF}
      EarlierClause := EarlierClause.GetPreviousOpenClause();
{$IFDEF DEBUG_SEEKER} if (Assigned(EarlierClause)) then Writeln('now asking a ' + EarlierClause.ClassName); {$ENDIF}
   end;
   if (not Assigned(EarlierClause)) then
      ReportNoJoinee();
{$IFDEF DEBUG_SEEKER} Writeln('accepted by a ' + EarlierClause.ClassName); {$ENDIF}
   FJoinedTo := EarlierClause;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractJoiningPreconditionFilterClause.CheckContext() end'); {$ENDIF}
end;

procedure TAbstractJoiningPreconditionFilterClause.ReportNoJoinee();
begin
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;


class function TAndFromClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := TFromClause.IsMatch(Candidate, Condition);
end;

function TAndFromClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and from';
end;

class procedure TAndFromClause.ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString);
begin
   Fail('I don''t see any "' + Tokens[CurrentToken] + '".');
end;


class function TAndInClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := TInClause.IsMatch(Candidate, Condition);
end;

function TAndInClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and in';
end;

class procedure TAndInClause.ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString);
begin
   Fail('I don''t see any "' + Tokens[CurrentToken] + '" for anything to be in.');
end;


class function TAndOnClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := TOnClause.IsMatch(Candidate, Condition);
end;

function TAndOnClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and on';
end;

class procedure TAndOnClause.ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString);
begin
   Fail('I don''t see any "' + Tokens[CurrentToken] + '" for anything to be on.');
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
   inherited;
   CurrentClause := FPrevious;
   SkipUntilClause := CurrentClause;
   Continue := True;
   while (Continue) do
   begin
      if (CurrentClause.AcceptsFilter(Self, Continue)) then
      begin
         if (CurrentClause = SkipUntilClause) then
            Victimise(CurrentClause)
      end;
      if (SkipUntilClause = CurrentClause) then
         SkipUntilClause := CurrentClause.GetPreviousOpenClause();
      CurrentClause := CurrentClause.FPrevious;
      Assert((not Continue) or (Assigned(CurrentClause)));
   end;
   if (not (Length(FVictims) > 0)) then
      ReportNoVictims();
end;

procedure TAbstractFilteringClause.Filter(Victim: TAbstractClause);
var
   VictimThing, CondemnedThing, FilterThing: PThingItem;
   LastNext: PPThingItem;
   NextRegisteredJoin: Cardinal;
   CurrentIsMatch: TIsMatchFunction;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Filter() called on a ', ClassName, ' for a victim that is a ', Victim.ClassName); {$ENDIF}
   VictimThing := Victim.FThings;
   Victim.FThings := nil;
   LastNext := @Victim.FThings;
   while (Assigned(VictimThing)) do
   begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Filter() considering to filter ', VictimThing^.Value.GetDefiniteName(nil)); {$ENDIF}
      FilterThing := FThings;
      NextRegisteredJoin := Low(FRegisteredJoins);
      CurrentIsMatch := @IsMatch;
      while (Assigned(FilterThing) and (not CurrentIsMatch(VictimThing^.Value, FilterThing^.Value))) do
      begin
         FilterThing := FilterThing^.Next;
         if ((not Assigned(FilterThing)) and (NextRegisteredJoin <= High(FRegisteredJoins))) then
         begin
            FilterThing := FRegisteredJoins[NextRegisteredJoin].FThings;
            CurrentIsMatch := GetIsMatch(FRegisteredJoins[NextRegisteredJoin]);
            Inc(NextRegisteredJoin);
         end;
      end;
      if (Assigned(FilterThing) = KeepMatches()) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Filter() keeping ', VictimThing^.Value.GetDefiniteName(nil)); {$ENDIF}
         { put the victim's thing back in its list }
         LastNext^ := VictimThing;
         LastNext := @VictimThing^.Next;
         VictimThing := VictimThing^.Next;
         LastNext^ := nil;
      end
      else
      begin
         { get rid of this thing's entry }
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Filter() dumping ', VictimThing^.Value.GetDefiniteName(nil)); {$ENDIF}
         CondemnedThing := VictimThing;
         VictimThing := VictimThing^.Next;
         Dispose(CondemnedThing);
      end;
   end;
   if (not Assigned(Victim.FThings)) then
      ReportNothingLeft();
   Victim.FInputFragment := Victim.FInputFragment + ' ' + GetFragmentAnnotation();
   if (Length(FRegisteredJoins) > 0) then
      for NextRegisteredJoin := High(FRegisteredJoins) downto Low(FRegisteredJoins) do
         Victim.FInputFragment := Victim.FInputFragment + ' ' + FRegisteredJoins[NextRegisteredJoin].GetFragmentAnnotation();
end;

function TAbstractFilteringClause.GetIsMatch(Target: TAbstractClause): TIsMatchFunction;
begin
   Result := @IsMatch;
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
   for Index := Low(FVictims) to High(FVictims) do
      Filter(FVictims[Index]);
end;

class function TAbstractFilteringClause.GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber;
begin
   Result := gnAmbiguous;
end;

function TAbstractFilteringClause.GetFragmentAnnotation(): AnsiString;
var
   Article: AnsiString;
begin
   Article := GetArticle();
   if (Article <> '') then
      Article := Article + ' ';
   Result := GetClausePrefix() + ' ' + Article + FInputFragment;
end;

procedure TAbstractFilteringClause.ReportNothingLeft();
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.ReportNothingLeft() for a ' + ClassName); {$ENDIF}
   Fail('It''s not clear to what you are referring.');
end;

procedure TAbstractFilteringClause.ReportNoVictims();
begin
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;


procedure TInclusionFilterClause.Filter(Victim: TAbstractClause);
begin
   inherited;
   if (not (cfRemoveHiddenThings in FFlags)) then
      Exclude(Victim.FFlags, cfRemoveHiddenThings);
end;

function TInclusionFilterClause.KeepMatches(): Boolean;
begin
   Result := True;
end;


class function TAbstractThatIsClause.IsMatch(Candidate, Condition: TThing): Boolean;
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

procedure TAbstractThatIsClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
   SelectionMechanism := tsmPickAll;
end;

procedure TAbstractThatIsClause.ReportNotSingular();
begin
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;

procedure TAbstractThatIsClause.ReportExplicitNumber();
begin
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;


function TThatIsClause.GetClausePrefix(): AnsiString;
begin
   Result := 'that is';
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

function TAndThatIsClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and that is';
end;


procedure TAbstractPreconditionFilterClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
var
   NeedMatching, HaveMatching, VictimIndex: Cardinal;
   VictimThing, FilterThing, BadThings: PThingItem;
   LastNextGood, LastNextBad: PPThingItem;
   TargetIsMatch: TIsMatchFunction;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() called on a ', ClassName, ' for a target that is a ', Target.ClassName); {$ENDIF}
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() Count = ', Count, '; SelectionMechanism = ', SelectionMechanism); {$ENDIF}
   Assert((Target is TAbstractJoiningPreconditionFilterClause) or (Target = Self));
   if (Target = Self) then
      TargetIsMatch := @IsMatch
   else
      TargetIsMatch := GetIsMatch(Target);
   case SelectionMechanism of
    tsmPickOne, tsmPickSome: NeedMatching := 1;
    tsmPickNumber, tsmPickOnlyRelevantNumber: NeedMatching := FNumber;
   else
     NeedMatching := 0;
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() NeedMatching = ', NeedMatching); {$ENDIF}
   Assert(NeedMatching <= Count);
   if ((NeedMatching > 0) and (NeedMatching < Count)) then
   begin
      HaveMatching := 0;
      FilterThing := Target.FThings;
      Target.FThings := nil;
      LastNextGood := @Target.FThings;
      BadThings := nil;
      LastNextBad := @BadThings;
      while (Assigned(FilterThing)) do
      begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() checking usefulness of ', FilterThing^.Value.GetDefiniteName(nil)); {$ENDIF}
         Assert(Length(FVictims) > 0);
         VictimIndex := Low(FVictims);
         VictimThing := FVictims[VictimIndex].FThings;
         while (Assigned(VictimThing) and (not TargetIsMatch(VictimThing^.Value, FilterThing^.Value))) do
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
{$IFDEF DEBUG_SEEKER} Writeln('   ...good'); {$ENDIF}
            { this FilterThing is ok, keep it }
            LastNextGood^ := FilterThing;
            LastNextGood := @FilterThing^.Next;
            FilterThing := FilterThing^.Next;
            LastNextGood^ := nil;
            Inc(HaveMatching);
         end
         else
         begin
{$IFDEF DEBUG_SEEKER} Writeln('   ...bad'); {$ENDIF}
            { this FilterThing is useless, put it aside for now }
            LastNextBad^ := FilterThing;
            LastNextBad := @FilterThing^.Next;
            FilterThing := FilterThing^.Next;
            LastNextBad^ := nil;
         end;
      end;
      if (not Assigned(Target.FThings)) then
      begin
         Target.FThings := BadThings;
         BadThings := nil;
      end
      else
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

function TAbstractPreconditionFilterClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := Peer is TAbstractJoiningPreconditionFilterClause;
end;

function TAbstractPreconditionFilterClause.GetIsMatch(Target: TAbstractClause): TIsMatchFunction;
begin
   Assert(Target is TAbstractJoiningPreconditionFilterClause);
   Result := @((Target as TAbstractJoiningPreconditionFilterClause).IsMatch);
end;


class function TFromClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Assert(Assigned(Candidate));
   Assert(Assigned(Candidate.Parent));
   Assert(Assigned(Condition));
   Result := Candidate.Parent = Condition;
end;

function TFromClause.GetClausePrefix(): AnsiString;
begin
   Result := 'from';
end;


class function TInClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Assert(Assigned(Candidate));
   Assert(Assigned(Candidate.Parent));
   Assert(Assigned(Condition));
   Result := (Candidate.Parent = Condition) and (Candidate.Position in tpContained);
end;

function TInClause.GetClausePrefix(): AnsiString;
begin
   Result := 'in';
end;


class function TOnClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Assert(Assigned(Candidate));
   Assert(Assigned(Candidate.Parent));
   Assert(Assigned(Condition));
   Result := (Candidate.Parent = Condition) and (Candidate.Position in tpStacked); { maybe should change that to just [tpOn, tpOnImplicit] }
end;

function TOnClause.GetClausePrefix(): AnsiString;
begin
   Result := 'on';
end;


procedure TExclusionFilteringClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
   if (((SelectionMechanism <> tsmPickNumber) and (SelectionMechanism <> tsmPickOnlyNumber)) or
       ((SelectionMechanism <> tsmPickNumber) and (FNumber = 1))) then
     SelectionMechanism := tsmPickAll;
end;

function TExclusionFilteringClause.KeepMatches(): Boolean;
begin
   Result := False;
end;


class function TExceptionFilteringClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := Candidate = Condition;
end;

function TExceptionFilteringClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := Peer is TAndClause;
end;


function TButClause.GetClausePrefix(): AnsiString;
begin
   Result := 'but';
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
var
   Index: Cardinal;
   CurrentThing: PThingItem;
begin
   if (Length(FRegisteredJoins) > 0) then
   begin
      Include(FFlags, cfDisambiguateLoneResult);
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
         Assert(not (cfHadArticle in Flags));
         Perspective.GetSurroundingsRoot(FromOutside).AddImplicitlyReferencedDescendantThings(Perspective, FromOutside, True, tpEverything, [], FCurrentBestThingList);
         AppendClause(ClauseClass.Create(0, tsmPickAll, Flags, FCurrentBestThingList, OriginalTokens[CurrentToken - 1]));
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
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['all', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickOnlyNumber, tsmPickOnlyNumber, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all'])) then
      begin
         Result := (((CurrentToken < Length(Tokens)) and
                     (CollectExplicitThings([gnPlural], 1, tsmPickAll, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], True))) or
                    (CollectImplicitThings([cfAllowExceptions, cfSingular, cfDisambiguateLoneResult, cfRemoveHiddenThings])));
      end
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'one']) or TryMatch(CurrentToken, Tokens, ['any', '1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['any', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any'])) then
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['an'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['a'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['everything'])) then
         Result := CollectImplicitThings([cfAllowExceptions, cfSingular, cfDisambiguateLoneResult, cfRemoveHiddenThings])
      else
      if (TryMatch(CurrentToken, Tokens, ['every'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickAll, tsmPickAll, [cfAllowExceptions, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one', 'of', 'the']) or TryMatch(CurrentToken, Tokens, ['1', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#', 'of', 'the'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one']) or TryMatch(CurrentToken, Tokens, ['1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateLoneResult, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['the', 'one']) or TryMatch(CurrentToken, Tokens, ['the', '1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickOnlyNumber, tsmPickOnlyNumber, [cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['the', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickOnlyNumber, tsmPickOnlyNumber, [cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['the'])) then
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyNumber, tsmPickAll, [cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['some'])) then
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickNumber, tsmPickSome, [cfDisambiguateLoneResult, cfHadArticle], False)
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
      ClauseConfigurations: array[0..15] of TClauseConfiguration = (
         (Tokens: (',', 'and', 'that', 'is',         EOT); ClauseClass: TAndThatIsClause;    EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'from',             _,EOT); ClauseClass: TAndFromClause;      EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'in',               _,EOT); ClauseClass: TAndInClause;        EndingClauseKind: eckIn),
         (Tokens: (',', 'and', 'on',               _,EOT); ClauseClass: TAndOnClause;        EndingClauseKind: eckOn),
         (Tokens: ('and', 'that', 'is',            _,EOT); ClauseClass: TAndThatIsClause;    EndingClauseKind: eckNormal),
         (Tokens: (',', 'and',                   _,_,EOT); ClauseClass: TAndClause;          EndingClauseKind: eckNormal),
         (Tokens: ('and', 'from',                _,_,EOT); ClauseClass: TAndFromClause;      EndingClauseKind: eckNormal),
         (Tokens: ('and', 'in',                  _,_,EOT); ClauseClass: TAndInClause;        EndingClauseKind: eckIn),
         (Tokens: ('and', 'on',                  _,_,EOT); ClauseClass: TAndOnClause;        EndingClauseKind: eckOn),
         (Tokens: ('that', 'is',                 _,_,EOT); ClauseClass: TThatIsClause;       EndingClauseKind: eckNormal),
         (Tokens: (',',                        _,_,_,EOT); ClauseClass: TAndClause;          EndingClauseKind: eckNormal),
         (Tokens: ('and',                      _,_,_,EOT); ClauseClass: TAndClause;          EndingClauseKind: eckNormal),
         (Tokens: ('but',                      _,_,_,EOT); ClauseClass: TButClause;          EndingClauseKind: eckNormal),
         (Tokens: ('from',                     _,_,_,EOT); ClauseClass: TFromClause;         EndingClauseKind: eckNormal),
         (Tokens: ('in',                       _,_,_,EOT); ClauseClass: TInClause;           EndingClauseKind: eckIn),
         (Tokens: ('on',                       _,_,_,EOT); ClauseClass: TOnClause;           EndingClauseKind: eckOn)
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
