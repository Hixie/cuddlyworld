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
      FThingList: TThingList;
      {$IFOPT C+} FCurrentlyCollecting, FCollected: Boolean; {$ENDIF}
      FCurrentPreferredGrammaticalNumber: TGrammaticalNumber;
      FCurrentBestLength: Cardinal;
      FCurrentBestGrammaticalNumber: TGrammaticalNumber;
      FCurrentBestThingList: TThingList;
      procedure ReferencedCallback(Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber);
    public
      constructor Create();
      destructor Destroy(); override;
      function Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TEndingClauseKinds; Verb: AnsiString): Boolean;
      function GetTokenCount(): Cardinal;
      function GetDisambiguate(): Boolean;
      function GetThingList(): TThingList; { must be called exactly once after Collect() returns true }
   end;

implementation

uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils;

type
   TThingSelectionMechanism = (tsmPickAll, tsmPickNumber, tsmPickSome, tsmPickOnlyNumber, tsmPickOnlyRelevantNumber);
   TClauseFlags = set of (cfAllowExceptions, cfSingular, cfPlural, cfDisambiguateAnyLoneResult, cfDisambiguateSingularLoneResult,
                          cfHaveThatIsClause, cfHaveThatAreClause, cfRemoveHiddenThings, cfHadArticle);

   TIsMatchFunction = function (Candidate, Condition: TThing): Boolean of object;

   TAbstractJoiningClause = class;
   TAbstractClause = class
     protected
      FFlags: TClauseFlags;
      FNumber: Cardinal;
      FSelectionMechanism: TThingSelectionMechanism;
      FThings: TThingList;
      FInputFragment, FFilterFragments: AnsiString;
      FNext: TAbstractClause;
      FPrevious: TAbstractClause;
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); virtual;
      function IsPlainClause(): Boolean; virtual;
      function GetPreviousOpenClause(): TAbstractClause; virtual;
      function GetArticle(): String; virtual;
      procedure AddFilterFragment(Fragment: AnsiString);
     public
      constructor Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; Things: TThingList; InputFragment: AnsiString); virtual;
      destructor Destroy(); override;
      procedure CheckContext(); virtual;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; virtual; abstract;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); virtual; abstract;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; virtual;
      procedure SelfCensor(Whitelist: TThingList); virtual;
      function Select(): Boolean; virtual;
      procedure Process(); virtual; abstract;
      procedure Add(Next: TAbstractClause);
      class function GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber; virtual;
      class function CanFail(): Boolean; virtual; { return True if the clause could also be a preposition or action join (e.g. "from", "and") }
      class procedure ReportFailedMatch(Tokens: TTokens; ClauseStart, CurrentToken: Cardinal; Verb: AnsiString); virtual;
      function GetFragment(): AnsiString;
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
   TAbstractPlainJoiningClause = class(TAbstractJoiningClause)
     public
      procedure CheckContext(); override;
   end;
   TAndClause = class(TAbstractPlainJoiningClause)
     protected
      function GetClausePrefix(): AnsiString; override;
     public
      class function CanFail(): Boolean; override;
   end;
   TPlusClause = class(TAbstractPlainJoiningClause)
     protected
      function GetClausePrefix(): AnsiString; override;
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

   TAbstractThatIsAreClause = class(TInclusionFilterClause)
     protected
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      procedure Victimise(Clause: TAbstractClause); override;
      function WantSingular(): Boolean; virtual; abstract;
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
     public
      procedure CheckContext(); override;
      procedure ReportNotRightGrammaticalNumber(); virtual;
      procedure ReportExplicitNumber(); virtual;
   end;
   TAbstractThatIsClause = class(TAbstractThatIsAreClause)
     protected
      function WantSingular(): Boolean; override;
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
   TAbstractThatAreClause = class(TAbstractThatIsAreClause)
     protected
      function WantSingular(): Boolean; override;
   end;
   TThatAreClause = class(TAbstractThatAreClause)
     protected
      function GetClausePrefix(): AnsiString; override;
   end;
   TAndThatAreClause = class(TAbstractThatAreClause)
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
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
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

   TAbstractThatIsAreNotClause = class(TExclusionFilteringClause)
     protected
      procedure Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism); override;
      procedure Victimise(Clause: TAbstractClause); override;
      function WantSingular(): Boolean; virtual; abstract;
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
     public
      procedure CheckContext(); override;
      procedure ReportNotRightGrammaticalNumber(); virtual;
      procedure ReportExplicitNumber(); virtual;
   end;
   TAbstractThatIsNotClause = class(TAbstractThatIsAreNotClause)
     protected
      function WantSingular(): Boolean; override;
   end;
   TThatIsNotClause = class(TAbstractThatIsNotClause)
     protected
      function GetClausePrefix(): AnsiString; override;
   end;
   TAndThatIsNotClause = class(TAbstractThatIsNotClause)
     protected
      function GetClausePrefix(): AnsiString; override;
     public
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
   end;
   TAbstractThatAreNotClause = class(TAbstractThatIsAreNotClause)
     protected
      function WantSingular(): Boolean; override;
   end;
   TThatAreNotClause = class(TAbstractThatAreNotClause)
     protected
      function GetClausePrefix(): AnsiString; override;
   end;
   TAndThatAreNotClause = class(TAbstractThatAreNotClause)
     protected
      function GetClausePrefix(): AnsiString; override;
     public
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
   end;

   TAbstractButClause = class(TExclusionFilteringClause) { filters that filter the nearest sequence of "all"s }
     protected
      class function IsMatch(Candidate, Condition: TThing): Boolean; override;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
   end;
   TButClause = class(TAbstractButClause)
     protected
      function GetClausePrefix(): AnsiString; override;
   end;

   TStartClause = class(TJoinableClause)
     protected
      function IsPlainClause(): Boolean; override;
     public
      procedure CheckContext(); override;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      function AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean; override;
      procedure Process(); override;
      procedure Bank(Perspective: TAvatar; var Things: TThingList; var Disambiguate: Boolean);
      {$IFDEF DEBUG_SEEKER} function GetCompleteFragment(): AnsiString; {$ENDIF}
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
   Result := 'About the ' + NumberToEnglish(FWanted) + ' ' + Input + '... I can only find ' + NumberToEnglish(FGot) + ': ' + FClause.FThings.GetLongDefiniteString(Perspective, 'and') + '.';
end;

function ESelectTooMany.Message(Perspective: TAvatar; Input: AnsiString): AnsiString;
begin
   Assert(Length(Input) > 0);
   if (FWanted = 1) then
      Result := 'Which ' + Input + ' do you mean, ' + FClause.FThings.GetLongDefiniteString(Perspective, 'or') + '?'
   else
   if (FWanted = 1) then
      Result := 'About the ' + Input + '... I count ' + NumberToEnglish(FGot) + ', not ' + NumberToEnglish(FWanted) + '.'
   else
      Result := 'About the ' + NumberToEnglish(FWanted) + ' ' + Input + '... I count ' + NumberToEnglish(FGot) + ', not ' + NumberToEnglish(FWanted) + '.';
end;


constructor TAbstractClause.Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; Things: TThingList; InputFragment: AnsiString);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.Create() constructing a ', ClassName, ' with Number=', Number, '; SelectionMechanism=', SelectionMechanism, '; Things=', Things.GetDefiniteString(nil, 'and'), '; InputFragment="', InputFragment, '"'); {$ENDIF}
   Assert((cfSingular in Flags) or (cfPlural in Flags));
   Assert(Assigned(Things));
   inherited Create();
   FNumber := Number;
   FSelectionMechanism := SelectionMechanism;
   FFlags := Flags;
   FThings := Things;
   FInputFragment := InputFragment;
end;

destructor TAbstractClause.Destroy();
begin
   if (Assigned(FThings)) then
      FThings.Free(); { TStartClause.Bank() hands it off to someone else }
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
   if (Peer is TAbstractButClause) then
   begin
      Result := cfAllowExceptions in FFlags;
      PreviousOpen := GetPreviousOpenClause();
      CanContinue := (not Result) or (Assigned(PreviousOpen) and ((Self is TAbstractFilteringClause) = (PreviousOpen is TAbstractFilteringClause)));
   end
   else
   if ((Peer is TThatIsClause) or (Peer is TThatIsNotClause)) then
   begin
      Result := (cfSingular in FFlags) and not (cfAllowExceptions in Peer.FFlags);
      CanContinue := False;
   end
   else
   if ((Peer is TAndThatIsClause) or (Peer is TAndThatIsNotClause)) then
   begin
      Result := cfHaveThatIsClause in FFlags;
      CanContinue := (not Result) and (not IsPlainClause());
   end
   else
   if ((Peer is TThatAreClause) or (Peer is TThatAreNotClause)) then
   begin
      Result := cfPlural in FFlags;
      CanContinue := False;
   end
   else
   if ((Peer is TAndThatAreClause) or (Peer is TAndThatAreNotClause)) then
   begin
      Result := cfHaveThatAreClause in FFlags;
      CanContinue := (not Result) and (not IsPlainClause());
   end
   else
   if (Peer is TAbstractPreconditionFilterClause) then
   begin
      Result := IsPlainClause();
      PreviousOpen := GetPreviousOpenClause();
      CanContinue := (not Result) or (Assigned(PreviousOpen) and ((cfAllowExceptions in FFlags) = (cfAllowExceptions in GetPreviousOpenClause().FFlags)));
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
        tsmPickAll:
           if (cfPlural in FFlags) then
           begin
              if (cfAllowExceptions in FFlags) then
                 Result := 'all of the'
              else
                 Result := 'the';
           end
           else
           begin
              Assert(cfSingular in FFlags);
              Assert(cfAllowExceptions in FFlags);
              Result := 'every'
           end;
        tsmPickNumber:
           if (FNumber = 1) then
           begin
              if (cfPlural in FFlags) then
              begin
                 if (cfAllowExceptions in FFlags) then
                    Result := 'any of the'
                 else
                    Result := 'one of the';
              end
              else
              begin
                 Assert(cfSingular in FFlags);
                 if (cfAllowExceptions in FFlags) then
                    Result := 'any'
                 else
                    Result := IndefiniteArticle(GetFragment());
              end;
           end
           else
           begin
              if (cfAllowExceptions in FFlags) then
                 Result := 'any ' + NumberToEnglish(FNumber)
              else
                 Result := NumberToEnglish(FNumber);
           end;
        tsmPickSome:
           begin
              Assert(not (cfSingular in FFlags));
              Assert(cfPlural in FFlags);
              Assert(not (cfAllowExceptions in FFlags));
              Result := 'some';
           end;
        tsmPickOnlyNumber:
           if ((FNumber = 1) and (cfSingular in FFlags)) then
           begin
              if (cfAllowExceptions in FFlags) then
                 Result := 'all of the'
              else
                 Result := 'the';
           end
           else
           begin
              if (cfAllowExceptions in FFlags) then
                 Result := 'all ' + NumberToEnglish(FNumber)
              else
                 Result := 'the ' + NumberToEnglish(FNumber);
           end;
        tsmPickOnlyRelevantNumber:
           begin
              Assert(cfSingular in FFlags);
              Assert(not (cfPlural in FFlags));
              Assert(not (cfAllowExceptions in FFlags));
              Result := '';
           end;
       else
        raise EAssertionFailed.Create('unknown TSelectionMechanism');
      end;
   end
   else
      Result := '';
end;

function TAbstractClause.Select(): Boolean;
var
   Count: Cardinal;

   procedure SelectN(N: Cardinal);
   var
      E: TThingEnumerator;
   begin
      Assert(N <= Count);
      E := FThings.GetEnumerator();
      try
         repeat
            {$IFOPT C+} Assert( {$ENDIF} E.MoveNext() {$IFOPT C+} ) {$ENDIF} ;
            Dec(N);
         until N = 0;
         if (E.MoveNext()) then
            E.RemoveRemainder();
      finally
         E.Free();
      end;
  end;

var
   SelectionMechanism: TThingSelectionMechanism;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.Select() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and')); {$ENDIF}
   Assert((FSelectionMechanism <> tsmPickOnlyRelevantNumber) or (FNumber = 1));
   Count := FThings.Length;
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
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.Select() for a ', ClassName, ' ended with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Result=', Result); {$ENDIF}
end;

procedure TAbstractClause.SelfCensor(Whitelist: TThingList);
var
   E: TThingEnumerator;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.SelfCensor() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Whitelist=', Whitelist.GetDefiniteString(nil, 'and')); {$ENDIF}
   E := FThings.GetEnumerator();
   try
      while (E.MoveNext()) do
      begin
         if (not Whitelist.Contains(E.Current)) then
            E.Remove();
      end;
   finally
      E.Free();
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.SelfCensor() for a ', ClassName, ' ending with FThings=', FThings.GetDefiniteString(nil, 'and')); {$ENDIF}
end;

procedure TAbstractClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractClause.Preselect() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Target=', Target.ClassName, ', Count=', Count, ', SelectionMechanism=', SelectionMechanism); {$ENDIF}
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

procedure TAbstractClause.AddFilterFragment(Fragment: AnsiString);
begin
   if (FFilterFragments <> '') then
      FFilterFragments := Fragment + ' ' + FFilterFragments
   else
      FFilterFragments := Fragment;
end;

function TAbstractClause.GetFragment(): AnsiString;
begin
   Result := FInputFragment;
   if (FFilterFragments <> '') then
      Result := Result + ' ' + FFilterFragments;
end;


procedure TAbstractJoiningClause.Process();
begin
   Assert(Assigned(FJoinedTo));
   FJoinedTo.RegisterJoin(Self);
end;

procedure TAbstractJoiningClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractJoiningClause.Preselect() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Target=', Target.ClassName, ', Count=', Count, ', SelectionMechanism=', SelectionMechanism, ' -- deferring to ', FJoinedTo.ClassName); {$ENDIF}
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
   Result := GetClausePrefix() + ' ' + Article + GetFragment();
end;


procedure TAbstractPlainJoiningClause.CheckContext();
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

function TAndClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and';
end;

class function TAndClause.CanFail(): Boolean;
begin
   Result := True;
end;


function TPlusClause.GetClausePrefix(): AnsiString;
begin
   Result := 'plus';
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
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractJoiningPreconditionFilterClause.ReportNoJoinee() for a ' + ClassName); {$ENDIF}
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
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() for a ', ClassName); {$ENDIF}
   inherited;
   CurrentClause := FPrevious;
   SkipUntilClause := CurrentClause;
   Continue := True;
   while (Continue) do
   begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() looking for a victim; target=', CurrentClause.ClassName); {$ENDIF}
      if (CurrentClause.AcceptsFilter(Self, Continue)) then
      begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() looking for a victim; target=', CurrentClause.ClassName, ' accepted! Continue=', Continue); {$ENDIF}
         if (CurrentClause = SkipUntilClause) then
         begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() looking for a victim; target=', CurrentClause.ClassName, ' accepted and SELECTED!'); {$ENDIF}
            Victimise(CurrentClause);
         end;
      end;
      if (SkipUntilClause = CurrentClause) then
         SkipUntilClause := CurrentClause.GetPreviousOpenClause();
{$IFDEF DEBUG_SEEKER} if (Assigned(SkipUntilClause)) then Writeln('TAbstractFilteringClause.CheckContext() now skipping until ', SkipUntilClause.ClassName); {$ENDIF}
      CurrentClause := CurrentClause.FPrevious;
      Assert((not Continue) or (Assigned(CurrentClause)));
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() loop end'); {$ENDIF}
   if (not (Length(FVictims) > 0)) then
      ReportNoVictims();
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.CheckContext() for a ', ClassName, ' checked out a-ok.'); {$ENDIF}
end;

procedure TAbstractFilteringClause.Filter(Victim: TAbstractClause);
var
   VictimEnumerator: TThingEnumerator;
   FilterEnumerator, OldFilterEnumerator: TThingEnumerator;
   Done, Found: Boolean;
   NextRegisteredJoin: Cardinal;
   CurrentIsMatch: TIsMatchFunction;
   NewFilterFragment: AnsiString;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Filter() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Victim=', Victim.ClassName, '; Victim.FThings=', Victim.FThings.GetDefiniteString(nil, 'and')); {$ENDIF}
   VictimEnumerator := Victim.FThings.GetEnumerator();
   try
      while (VictimEnumerator.MoveNext()) do
      begin
{$IFDEF DEBUG_SEEKER} Writeln('TInclusionFilterClause.Filter() for a ', ClassName, ': examining ', VictimEnumerator.Current.GetDefiniteName(nil)); {$ENDIF}
         FilterEnumerator := FThings.GetEnumerator();
         try
            NextRegisteredJoin := Low(FRegisteredJoins);
            CurrentIsMatch := @IsMatch;
            Done := False;
            Found := False;
            while (not Done) do
            begin
               Done := not FilterEnumerator.MoveNext();
               while ((Done) and (NextRegisteredJoin <= High(FRegisteredJoins))) do
               begin
                  { perform a little dance here to make sure the 'finally' block below can never try to free freed memory }
                  OldFilterEnumerator := FilterEnumerator;
                  FilterEnumerator := nil;
                  OldFilterEnumerator.Free();
                  FilterEnumerator := FRegisteredJoins[NextRegisteredJoin].FThings.GetEnumerator();
                  CurrentIsMatch := GetIsMatch(FRegisteredJoins[NextRegisteredJoin]);
                  Inc(NextRegisteredJoin);
                  Done := not FilterEnumerator.MoveNext();
               end;
               if ((not Done) and (CurrentIsMatch(VictimEnumerator.Current, FilterEnumerator.Current))) then
               begin
                  Found := True;
                  Done := True;
               end;
            end;
            if (Found <> KeepMatches()) then { i.e. if found and not keep matches, or if not found and keep matches }
            begin
{$IFDEF DEBUG_SEEKER} Writeln('TInclusionFilterClause.Filter() for a ', ClassName, ': decided to remove ', VictimEnumerator.Current.GetDefiniteName(nil)); {$ENDIF}
               VictimEnumerator.Remove();
            end
            else
            begin
{$IFDEF DEBUG_SEEKER} Writeln('TInclusionFilterClause.Filter() for a ', ClassName, ': decided to keep ', VictimEnumerator.Current.GetDefiniteName(nil)); {$ENDIF}
            end;
         finally
            FilterEnumerator.Free();
         end;
      end;
   finally
      VictimEnumerator.Free();
   end;
   if (Victim.FThings.Length = 0) then
      ReportNothingLeft();
   NewFilterFragment := GetFragmentAnnotation();
   if (Length(FRegisteredJoins) > 0) then
      for NextRegisteredJoin := High(FRegisteredJoins) downto Low(FRegisteredJoins) do
         NewFilterFragment := NewFilterFragment + ' ' + FRegisteredJoins[NextRegisteredJoin].GetFragmentAnnotation();
   Victim.AddFilterFragment(NewFilterFragment);
end;

function TAbstractFilteringClause.GetIsMatch(Target: TAbstractClause): TIsMatchFunction;
begin
   Result := @IsMatch;
end;

procedure TAbstractFilteringClause.Victimise(Clause: TAbstractClause);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Victimise() called on a ', ClassName, ' for a peer that is a ', Clause.ClassName); {$ENDIF}
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
   Result := FVictims[Low(FVictims)];
end;

procedure TAbstractFilteringClause.Process();
var
   Index: Cardinal;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Process() on a ', ClassName, ' -- ', Length(FVictims), ' victims follow') {$ENDIF};
   for Index := Low(FVictims) to High(FVictims) do
   begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Process() on a ', ClassName, ' -- victim ', Index, ':') {$ENDIF};
      Filter(FVictims[Index]);
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.Process() on a ', ClassName, ' -- end of victim list') {$ENDIF};
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
   Result := GetClausePrefix() + ' ' + Article + GetFragment();
end;

procedure TAbstractFilteringClause.ReportNothingLeft();
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.ReportNothingLeft() for a ' + ClassName); {$ENDIF}
   Fail('It''s not clear to what you are referring.');
end;

procedure TAbstractFilteringClause.ReportNoVictims();
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractFilteringClause.ReportNoVictims() for a ' + ClassName); {$ENDIF}
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;


procedure TInclusionFilterClause.Filter(Victim: TAbstractClause);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TInclusionFilterClause.Filter() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Victim=', Victim.ClassName); {$ENDIF}
   inherited;
   if (not (cfRemoveHiddenThings in FFlags)) then
      Exclude(Victim.FFlags, cfRemoveHiddenThings);
end;

function TInclusionFilterClause.KeepMatches(): Boolean;
begin
   Result := True;
end;


procedure TAbstractThatIsAreClause.CheckContext();
var
   WantedSingular: Boolean;
begin
   inherited;
   WantedSingular := WantSingular();
   if (((WantedSingular) and (not (cfSingular in FFlags))) or
       ((not WantedSingular) and (not (cfPlural in FFlags)))) then
      ReportNotRightGrammaticalNumber();
   if ((FSelectionMechanism = tsmPickOnlyNumber) and (FNumber <> 1)) then
      ReportExplicitNumber();
end;

procedure TAbstractThatIsAreClause.Victimise(Clause: TAbstractClause);
begin
   inherited;
   if (WantSingular()) then
      Include(Clause.FFlags, cfHaveThatIsClause)
   else
      Include(Clause.FFlags, cfHaveThatAreClause);
end;

procedure TAbstractThatIsAreClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractThatIsAreClause.Preselect() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Target=', Target.ClassName, ', Count=', Count, ', SelectionMechanism=', SelectionMechanism, ' -- setting SelectionMechanism to tsmPickAll'); {$ENDIF}
   SelectionMechanism := tsmPickAll;
end;

procedure TAbstractThatIsAreClause.ReportNotRightGrammaticalNumber();
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractThatIsAreClause.ReportNotRightGrammaticalNumber() for a ' + ClassName); {$ENDIF}
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;

procedure TAbstractThatIsAreClause.ReportExplicitNumber();
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractThatIsAreClause.ReportExplicitNumber() for a ' + ClassName); {$ENDIF}
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;

class function TAbstractThatIsAreClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := Candidate = Condition;
end;


function TAbstractThatIsClause.WantSingular(): Boolean;
begin
   Result := True;
end;


function TThatIsClause.GetClausePrefix(): AnsiString;
begin
   Result := 'that is';
end;


function TAndThatIsClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   if ((Peer is TAndThatIsClause) or (Peer is TAndThatIsNotClause)) then
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


function TAbstractThatAreClause.WantSingular(): Boolean;
begin
   Result := False;
end;


function TThatAreClause.GetClausePrefix(): AnsiString;
begin
   Result := 'that are';
end;


function TAndThatAreClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   if ((Peer is TAndThatAreClause) or (Peer is TAndThatAreNotClause)) then
   begin
      Result := False;
      CanContinue := True;
   end
   else
      Result := inherited;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

function TAndThatAreClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and that are';
end;

procedure TAbstractPreconditionFilterClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
var
   NeedMatching, HaveMatching, VictimIndex: Cardinal;
   FilterEnumerator, VictimEnumerator, OldVictimEnumerator: TThingEnumerator;
   Trash: TThingList;
   TargetIsMatch: TIsMatchFunction;
   Done, Found: Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Target=', Target.ClassName, ', Count=', Count, ', SelectionMechanism=', SelectionMechanism); {$ENDIF}
   Assert((Target is TAbstractJoiningPreconditionFilterClause) or (Target = Self));
   if (Target = Self) then
      TargetIsMatch := @IsMatch
   else
      TargetIsMatch := GetIsMatch(Target);
   case SelectionMechanism of
    tsmPickSome: NeedMatching := 1;
    tsmPickNumber, tsmPickOnlyRelevantNumber: NeedMatching := FNumber;
   else
     NeedMatching := 0;
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() NeedMatching = ', NeedMatching); {$ENDIF}
   Assert(NeedMatching <= Count);
   if ((NeedMatching > 0) and (NeedMatching < Count)) then
   begin
      HaveMatching := 0;
      Trash := TThingList.Create();
      try
         FilterEnumerator := Target.FThings.GetEnumerator();
         try
            while (FilterEnumerator.MoveNext()) do
            begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() checking usefulness of ', FilterEnumerator.Current.GetDefiniteName(nil)); {$ENDIF}
               Assert(Length(FVictims) > 0);
               VictimIndex := Low(FVictims);
               VictimEnumerator := FVictims[VictimIndex].FThings.GetEnumerator();
               try
                  Done := False;
                  Found := False;
                  while (not Done) do
                  begin
                     Done := not VictimEnumerator.MoveNext();
                     while ((Done) and (VictimIndex < High(FVictims))) do
                     begin
                        Inc(VictimIndex);
                        OldVictimEnumerator := VictimEnumerator;
                        VictimEnumerator := nil;
                        OldVictimEnumerator.Free();
                        VictimEnumerator := FVictims[VictimIndex].FThings.GetEnumerator();
                        Done := not VictimEnumerator.MoveNext();
                     end;
                     if ((not Done) and (TargetIsMatch(VictimEnumerator.Current, FilterEnumerator.Current))) then
                     begin
                        Done := True;
                        Found := True;
                     end;
                  end;
                  if (Found) then
                  begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() keeping ', FilterEnumerator.Current.GetName(nil)); {$ENDIF}
                     Inc(HaveMatching) { FilterEnumerator.Current is ok, keep it }
                  end
                  else
                  begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() trashing ', FilterEnumerator.Current.GetName(nil)); {$ENDIF}
                     Trash.AdoptItem(FilterEnumerator); { FilterEnumerator.Current is useless, put it aside for now }
                  end;
               finally
                  VictimEnumerator.Free();
               end;
            end;
         finally
            FilterEnumerator.Free();
         end;
         if (Target.FThings.Length = 0) then
         begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() failed to find anything useful; readopting trash'); {$ENDIF}
            { we failed to find anything useful }
            Assert(HaveMatching = 0);
            Target.FThings.Free();
            Target.FThings := Trash;
            Trash := nil;
         end
         else
         if (HaveMatching < NeedMatching) then
         begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() found some stuff but not enough; appending trash'); {$ENDIF}
            { we found some but not enough }
            Assert(NeedMatching > 1); { if it was 1, then we must have 0; if we have 0, FThings.Length would be nil }
            Assert(Trash.Length > 0); { we must have thrown something away to end up with fewer than we needed }
            { reorder the list so that the good ones are first and the bad ones last } // (for no good reason)
            Target.FThings.AdoptList(Trash);
         end
         else
         begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() found a suitable number (', Count, ')'); {$ENDIF}
            {$IFOPT C+}
               if (HaveMatching < Count) then
                  Assert(Trash.Length > 0)
               else
                  Assert(Trash.Length = 0);
            {$ENDIF}
            Count := HaveMatching;
         end;
      finally
         Trash.Free();
      end;
   end;
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.Preselect() for a ', ClassName, ' ending with FThings=', FThings.GetDefiniteString(nil, 'and'), ', Target=', Target.ClassName, ', Count=', Count, ', SelectionMechanism=', SelectionMechanism); {$ENDIF}
end;

function TAbstractPreconditionFilterClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := Peer is TAbstractJoiningPreconditionFilterClause;
end;

function TAbstractPreconditionFilterClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractPreconditionFilterClause.AcceptsFilter() called on a ', ClassName, ' for a peer that is a ', Peer.ClassName); {$ENDIF}
   if ((Peer is TAbstractJoiningPreconditionFilterClause) or
       (Peer is TAbstractPreconditionFilterClause)) then
   begin
      Result := True;
      CanContinue := False;
   end
   else
      Result := inherited;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
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
   Result := (Candidate.Parent = Condition) and (Candidate.Position in tpArguablyInside);
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
{$IFDEF DEBUG_SEEKER} Writeln('TExclusionFilteringClause.Preselect() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Target=', Target.ClassName, ', Count=', Count, ', SelectionMechanism=', SelectionMechanism, ' -- considering changing SelectionMechanism'); {$ENDIF}
   if (((SelectionMechanism <> tsmPickNumber) and (SelectionMechanism <> tsmPickOnlyNumber)) or
       ((SelectionMechanism <> tsmPickNumber) and (FNumber = 1))) then
     SelectionMechanism := tsmPickAll;
end;

function TExclusionFilteringClause.KeepMatches(): Boolean;
begin
   Result := False;
end;


procedure TAbstractThatIsAreNotClause.CheckContext();
var
   WantedSingular: Boolean;
begin
   inherited;
   WantedSingular := WantSingular();
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractThatIsAreNotClause.CheckContext() for a ' + ClassName, '; WantSingular() returned ', WantedSingular); {$ENDIF}
   if ((WantedSingular) and (not (cfSingular in FFlags))) then
      ReportNotRightGrammaticalNumber();
      { "that are not the kitchen table" is fine, it's only the other combinations that are bad:
           "everything that is the kitchen tables", "all things that are the kitchen table", "everything that is not the kitchen tables" }
   if ((FSelectionMechanism = tsmPickOnlyNumber) and (FNumber <> 1)) then
      ReportExplicitNumber();
end;

procedure TAbstractThatIsAreNotClause.Victimise(Clause: TAbstractClause);
begin
   inherited;
   if (WantSingular()) then
      Include(Clause.FFlags, cfHaveThatIsClause)
   else
      Include(Clause.FFlags, cfHaveThatAreClause);
end;

procedure TAbstractThatIsAreNotClause.Preselect(Target: TAbstractClause; var Count: Cardinal; var SelectionMechanism: TThingSelectionMechanism);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractThatIsAreNotClause.Preselect() for a ', ClassName, ' with FThings=', FThings.GetDefiniteString(nil, 'and'), '; Target=', Target.ClassName, ', Count=', Count, ', SelectionMechanism=', SelectionMechanism, ' -- SelectionMechanism := tsmPickAll'); {$ENDIF}
   SelectionMechanism := tsmPickAll;
end;

procedure TAbstractThatIsAreNotClause.ReportNotRightGrammaticalNumber();
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractThatIsAreNotClause.ReportNotRightGrammaticalNumber() for a ' + ClassName); {$ENDIF}
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;

procedure TAbstractThatIsAreNotClause.ReportExplicitNumber();
begin
{$IFDEF DEBUG_SEEKER} Writeln('TAbstractThatIsAreNotClause.ReportExplicitNumber() for a ' + ClassName); {$ENDIF}
   Fail('You used the term "' + GetClausePrefix() + '" in a way I don''t understand.');
end;

class function TAbstractThatIsAreNotClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := Candidate = Condition;
end;


function TAbstractThatIsNotClause.WantSingular(): Boolean;
begin
   Result := True;
end;


function TThatIsNotClause.GetClausePrefix(): AnsiString;
begin
   Result := 'that is not';
end;


function TAndThatIsNotClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   if ((Peer is TAndThatIsClause) or (Peer is TAndThatIsNotClause)) then
   begin
      Result := False;
      CanContinue := True;
   end
   else
      Result := inherited;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

function TAndThatIsNotClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and that is not';
end;


function TAbstractThatAreNotClause.WantSingular(): Boolean;
begin
   Result := False;
end;


function TThatAreNotClause.GetClausePrefix(): AnsiString;
begin
   Result := 'that are not';
end;


function TAndThatAreNotClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   if ((Peer is TAndThatAreClause) or (Peer is TAndThatAreNotClause)) then
   begin
      Result := False;
      CanContinue := True;
   end
   else
      Result := inherited;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

function TAndThatAreNotClause.GetClausePrefix(): AnsiString;
begin
   Result := 'and that are not';
end;


class function TAbstractButClause.IsMatch(Candidate, Condition: TThing): Boolean;
begin
   Result := Candidate = Condition;
end;

function TAbstractButClause.AcceptsJoins(Peer: TAbstractClause): Boolean;
begin
   Result := (Peer is TAndClause) and ((cfAllowExceptions in FFlags) or (not (cfAllowExceptions in Peer.FFlags)));
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
   Result := Peer is TAbstractPlainJoiningClause;
end;

function TStartClause.AcceptsFilter(Peer: TAbstractClause; out CanContinue: Boolean): Boolean;
begin
   Result := inherited;
   CanContinue := False;
{$IFDEF DEBUG_SEEKER} Writeln('Result=', Result, ' CanContinue=', CanContinue); {$ENDIF}
end;

procedure TStartClause.Process();
var
   Index: Cardinal;
begin
   if (Length(FRegisteredJoins) > 0) then
   begin
      Include(FFlags, cfDisambiguateAnyLoneResult);
      for Index := High(FRegisteredJoins) downto Low(FRegisteredJoins) do
         FThings.AdoptList(FRegisteredJoins[Index].FThings);
   end;
end;

procedure TStartClause.Bank(Perspective: TAvatar; var Things: TThingList; var Disambiguate: Boolean);
begin
   FThings.Deduplicate();
   Things := FThings;
   if ((FThings.Length = 1) and
       (((cfDisambiguateAnyLoneResult in FFlags) or
         ((cfDisambiguateSingularLoneResult in FFlags) and (not FThings.First.IsPlural(Perspective)))))) then
      Disambiguate := True;
   FThings := nil;
end;

{$IFDEF DEBUG_SEEKER}
function TStartClause.GetCompleteFragment(): AnsiString;
var
   Article: AnsiString;
   Index: Cardinal;
begin
   Result := GetFragment();
   Article := GetArticle();
   if (Article <> '') then
      Result := Article + ' ' + Result;
   if (Length(FRegisteredJoins) > 0) then
      for Index := High(FRegisteredJoins) downto Low(FRegisteredJoins) do
      begin
         if ((Index > Low(FRegisteredJoins)) or (Index < High(FRegisteredJoins))) then
            Result := Result + ', '
         else
            Result := Result + ' ';
         Result := Result + FRegisteredJoins[Index].GetFragmentAnnotation();
      end;
end;
{$ENDIF}

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
   if (Assigned(FCurrentBestThingList)) then
   begin
      Assert(FCurrentBestThingList.Length = 0);
      FCurrentBestThingList.Free();
   end;
   inherited;
end;

procedure TThingCollector.ReferencedCallback(Thing: TThing; Count: Cardinal; GrammaticalNumber: TGrammaticalNumber);
begin
{$IFDEF DEBUG_SEEKER} Writeln('TThingCollector.ReferencedCallback() called with Thing=', Thing.GetDefiniteName(nil), ', Count=', Count); {$ENDIF}
{$IFDEF DEBUG_SEEKER} if (gnSingular in GrammaticalNumber) then Writeln('TThingCollector.ReferencedCallback() GrammaticalNumber contains gnSingular'); {$ENDIF}
{$IFDEF DEBUG_SEEKER} if (gnPlural in GrammaticalNumber) then Writeln('TThingCollector.ReferencedCallback() GrammaticalNumber contains gnPlural'); {$ENDIF}
{$IFDEF DEBUG_SEEKER} if (gnSingular in FCurrentBestGrammaticalNumber) then Writeln('TThingCollector.ReferencedCallback() FCurrentBestGrammaticalNumber contains gnSingular'); {$ENDIF}
{$IFDEF DEBUG_SEEKER} if (gnPlural in FCurrentBestGrammaticalNumber) then Writeln('TThingCollector.ReferencedCallback() FCurrentBestGrammaticalNumber contains gnPlural'); {$ENDIF}
{$IFDEF DEBUG_SEEKER} if (gnSingular in FCurrentPreferredGrammaticalNumber) then Writeln('TThingCollector.ReferencedCallback() FCurrentPreferredGrammaticalNumber contains gnSingular'); {$ENDIF}
{$IFDEF DEBUG_SEEKER} if (gnPlural in FCurrentPreferredGrammaticalNumber) then Writeln('TThingCollector.ReferencedCallback() FCurrentPreferredGrammaticalNumber contains gnPlural'); {$ENDIF}
   Assert(Assigned(FCurrentBestThingList));
   if (Count < FCurrentBestLength) then
      Exit;
   if (Count > FCurrentBestLength) then
   begin
      FCurrentBestThingList.Empty();
      FCurrentBestLength := Count;
      FCurrentBestGrammaticalNumber := [];
   end;
   if (FCurrentBestGrammaticalNumber <> GrammaticalNumber) then
   begin
      { We need to fine tune the FCurrentBestGrammaticalNumber to match the
        GrammaticalNumber of this thing, to make sure we're not adding
        something that is a subset of the FCurrentBestGrammaticalNumber }

//   preferred    this      best

      if (FCurrentBestGrammaticalNumber = []) then
      begin

//      s          s          -  use it, we have nothing else
//      p          p          -
//      b          b          -
//      b          s          -
//      b          p          -

//      s          p          -  use it, reluctantly
//      p          s          -

//      s          b          -  use it, but claim to be the preferred value
//      p          b          -

         { We don't have anything good so far. }
         Assert(FCurrentBestThingList.Length = 0);
         FCurrentBestGrammaticalNumber := GrammaticalNumber * FCurrentPreferredGrammaticalNumber; { forget we're gnAmbiguous if we want specifically singular or plural }
         if (FCurrentBestGrammaticalNumber = []) then
            FCurrentBestGrammaticalNumber := GrammaticalNumber; { hopefully we'll find a better match later }
      end
      else
      if (GrammaticalNumber * FCurrentPreferredGrammaticalNumber = []) then { intersection of this thing and what we're looking for is empty set }
      begin

//      s          p          s  drop it, intersection of preferred and this is null
//      p          s          p
//      s          p          b  can't happen
//      p          s          b  can't happen

         { Either we're looking for plural and this is singular, or vice versa. Either way, not a match, and since we already have something that _is_ a match, drop it. }
         { (we know we have something that is a match because there's only one value that isn't, and we're it, and the current best value doesn't equal our value) }
         Assert(FCurrentPreferredGrammaticalNumber <> gnEither); { can't be, since otherwise we'd have intersected with it (we can't be []) }
         Assert(FCurrentBestGrammaticalNumber <> gnAmbiguous); { shouldn't ever happen since we get set to the intersection when this would otherwise happen; see above }
         Exit;
      end
      else
      if (FCurrentPreferredGrammaticalNumber * FCurrentBestGrammaticalNumber = []) then
      begin

//      s          s          p  throw away current, start over
//      p          p          s
//      s          b          p
//      p          b          s

         { What we have matches what we want (we know because we didn't match the previous if statement),
           and what we'd previously collected does not (we know because that's what this if statement checks). }
         { Throw away what we have and start over, claiming to be the FCurrentPreferredGrammaticalNumber. }
         FCurrentBestThingList.Empty();
         Assert(FCurrentBestLength = Count);
         FCurrentBestGrammaticalNumber := FCurrentPreferredGrammaticalNumber;
      end
      else
      if ((GrammaticalNumber = FCurrentPreferredGrammaticalNumber) and (FCurrentBestGrammaticalNumber = gnAmbiguous)) then
      begin

//      p          p          b  narrow it down
//      s          s          b

         { What we have is consistent with what we had, but we can narrow down our parameters. }
         { We know that our value is not ambiguous, because if it was, we'd be equal to the
           FCurrentBestGrammaticalNumber and so would skip this whole exercise. }
         { We know that the FCurrentPreferredGrammaticalNumber isn't ambiguous because we aren't. }
         Assert(FCurrentPreferredGrammaticalNumber <> gnAmbiguous);
         Assert(GrammaticalNumber <> gnAmbiguous);
         FCurrentBestGrammaticalNumber := GrammaticalNumber;
      end
      else
      if ((FCurrentBestGrammaticalNumber = FCurrentPreferredGrammaticalNumber) and (GrammaticalNumber = gnAmbiguous)) then
      begin

//      s          b          s -- do nothing
//      p          b          p

         { We're good as is. No need to do anything. }

         Assert(FCurrentPreferredGrammaticalNumber <> gnAmbiguous);
         Assert(FCurrentBestGrammaticalNumber <> gnAmbiguous);
         FCurrentBestGrammaticalNumber := GrammaticalNumber;
      end
      else
      begin
         Assert(FCurrentPreferredGrammaticalNumber = gnEither); { all other cases are already taken care of }
         { "take people" should prefer taking the many persons (person/people), not one of the one societies (people/peoples). }
         { Therefore, we prefer plural. }
         if (GrammaticalNumber = [gnSingular]) then
         begin
            Assert((FCurrentBestGrammaticalNumber = [gnPlural]) or (FCurrentBestGrammaticalNumber = gnBoth));
//      b          s          b
//      b          s          p
            Exit;
         end
         else
         if (FCurrentBestGrammaticalNumber = [gnSingular]) then
         begin
            Assert((GrammaticalNumber = [gnPlural]) or (GrammaticalNumber = gnBoth));
//      b          b          s
//      b          p          s
            FCurrentBestThingList.Empty();
            Assert(FCurrentBestLength = Count);
            FCurrentBestGrammaticalNumber := [gnPlural];
         end
         else
         begin
//      b          p          b
//      b          b          p
            Assert(GrammaticalNumber * FCurrentBestGrammaticalNumber = [gnPlural]);
            Assert((GrammaticalNumber = [gnPlural]) xor (FCurrentBestGrammaticalNumber = [gnPlural]));
            Assert((GrammaticalNumber = gnBoth) xor (FCurrentBestGrammaticalNumber = gnBoth));
            GrammaticalNumber := [gnPlural];
         end;
      end;
   end;
// else:
//      s          s          s  leave it, this is no different
//      p          s          s
//      b          s          s
//      s          p          p
//      p          p          p
//      b          p          p
//      s          b          b    "sheep" matching first "sheep skin" then "sheep"
//      p          b          b
//      b          b          b
   FCurrentBestThingList.AppendItem(Thing);
end;

function TThingCollector.Collect(Perspective: TAvatar; Tokens, OriginalTokens: TTokens; Start: Cardinal; PreferredGrammaticalNumber: TGrammaticalNumber; Scope: TAllImpliedScope; Ends: TEndingClauseKinds; Verb: AnsiString): Boolean;
var
   CurrentToken: Cardinal;

   procedure Collapse(FirstClause: TAbstractClause; out Things: TThingList; out Disambiguate: Boolean);
   var
      CurrentClause, LastClause: TAbstractClause;
      FromOutside, GotWhitelist: Boolean;
      Root: TAtom;
      WhiteList: TThingList;
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
      GotWhitelist := False;
      try
         repeat
            try
               if ((cfRemoveHiddenThings in CurrentClause.FFlags) and (Scope <> [])) then
               begin
                  if (not GotWhitelist) then
                  begin
                     if (aisSurroundings in Scope) then
                        Root := Perspective.GetSurroundingsRoot(FromOutside)
                     else
                     if (aisSelf in Scope) then
                        Root := Perspective
                     else
                        raise EAssertionFailed.Create('unexpected TAllImpliedScope value');
                     Whitelist := TThingList.Create();
                     GotWhitelist := True;
                     Root.FindMatchingThings(Perspective, FromOutside, aisSelf in Scope, tpCountsForAll, [], Whitelist);
                  end;
                  CurrentClause.SelfCensor(Whitelist);
               end;
               if (CurrentClause.Select()) then
                  Disambiguate := True;
               CurrentClause.Process();
            except
               on E: EMatcherException do { raised by Select() }
                  Fail(E.Message(Perspective, CurrentClause.GetFragment()));
            end;
            CurrentClause := CurrentClause.FPrevious;
         until not Assigned(CurrentClause);
      finally
         if (GotWhitelist) then
            Whitelist.Free();
      end;
      (FirstClause as TStartClause).Bank(Perspective, Things, Disambiguate);
   end;

var
   FirstClause, LastClause: TAbstractClause;

   procedure AppendClause(Clause: TAbstractClause);
   begin
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
         Assert((ExplicitGrammaticalNumber <> []));
         Start := CurrentToken;
         if (CurrentToken < Length(Tokens)) then
         begin
            if (not Assigned(FCurrentBestThingList)) then
               FCurrentBestThingList := TThingList.Create();
            Assert(FCurrentBestThingList.Length = 0);
            FCurrentPreferredGrammaticalNumber := ExplicitGrammaticalNumber;
            FCurrentBestLength := 0;
            FCurrentBestGrammaticalNumber := [];
            try
               Perspective.GetSurroundingsRoot(FromOutside).AddExplicitlyReferencedThings(Tokens, CurrentToken, Perspective, FromOutside, @ReferencedCallback);
            except
               FCurrentBestThingList.Empty();
               raise;
            end;
            if (FCurrentBestThingList.Length > 0) then
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
                  Assert(FCurrentBestThingList.Length > 0);
                  Include(Flags, cfPlural);
                  if (gnSingular in FCurrentBestGrammaticalNumber) then
                     Include(Flags, cfSingular)
                  else
                     Include(Flags, cfDisambiguateSingularLoneResult);
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
      end;

      function CollectImplicitThings(Flags: TClauseFlags): Boolean;
      var
         FromOutside: Boolean;
      begin
         Assert(not (cfHadArticle in Flags));
         if (not Assigned(FCurrentBestThingList)) then
            FCurrentBestThingList := TThingList.Create();
         Assert(FCurrentBestThingList.Length = 0);
         Perspective.GetSurroundingsRoot(FromOutside).FindMatchingThings(Perspective, FromOutside, True, tpEverything, tfEverything, FCurrentBestThingList);
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
      Assert(Assigned(ClauseClass));
      ClauseStart := CurrentToken;
      Inc(CurrentToken, ClauseLength);
      if (TryMatch(CurrentToken, Tokens, ['all', 'of', 'the'])) then
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all', 'one'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickOnlyNumber, tsmPickOnlyNumber, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['all', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickOnlyNumber, tsmPickOnlyNumber, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['all'])) then
      begin
         Result := (((CurrentToken < Length(Tokens)) and
                     (CollectExplicitThings([gnPlural], 1, tsmPickAll, tsmPickAll, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], True))) or
                    (CollectImplicitThings([cfAllowExceptions, cfSingular, cfPlural, cfDisambiguateAnyLoneResult, cfRemoveHiddenThings])));
      end
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any', 'one']) or TryMatch(CurrentToken, Tokens, ['any', '1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['any', '#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['any'])) then
         Result := CollectExplicitThings([gnSingular, gnPlural], 1, tsmPickNumber, tsmPickAll, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['an'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['a'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['everything'])) then
         Result := CollectImplicitThings([cfAllowExceptions, cfSingular, cfDisambiguateAnyLoneResult, cfRemoveHiddenThings])
      else
      if (TryMatch(CurrentToken, Tokens, ['every'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickAll, tsmPickAll, [cfAllowExceptions, cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one', 'of', 'the']) or TryMatch(CurrentToken, Tokens, ['1', 'of', 'the'])) then
         Result := CollectExplicitThings([gnPlural], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateAnyLoneResult, cfSingular, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#', 'of', 'the'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatch(CurrentToken, Tokens, ['one']) or TryMatch(CurrentToken, Tokens, ['1'])) then
         Result := CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, [cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
      if (TryMatchWithNumber(CurrentToken, Tokens, ['#'], Number)) then
         Result := CollectExplicitThings([gnPlural], Number, tsmPickNumber, tsmPickNumber, [cfDisambiguateAnyLoneResult, cfHadArticle], False)
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
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickNumber, tsmPickSome, [cfDisambiguateAnyLoneResult, cfHadArticle], False)
      else
         Result := CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyRelevantNumber, tsmPickAll, [], ClauseClass.CanFail());
      if (not Result) then
         Dec(CurrentToken, ClauseLength);
      Assert((not Assigned(FCurrentBestThingList)) or (FCurrentBestThingList.Length = 0));
   end;

   function TryClauses(out NextClauseClass: TAbstractClauseClass; out NextClauseLength: Cardinal): Boolean;
   const
      MaxTokensInClause = 5;

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
      ClauseConfigurations: array[0..34] of TClauseConfiguration = (
         (Tokens: (',', 'and', 'that', 'are', 'not',           EOT); ClauseClass: TAndThatAreNotClause;     EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'that', 'is', 'not',            EOT); ClauseClass: TAndThatIsNotClause;      EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'are', 'not',                 _,EOT); ClauseClass: TAndThatAreNotClause;     EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'is', 'not',                  _,EOT); ClauseClass: TAndThatIsNotClause;      EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'that', 'are',                _,EOT); ClauseClass: TAndThatAreClause;        EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'that', 'is',                 _,EOT); ClauseClass: TAndThatIsClause;         EndingClauseKind: eckNormal),
         (Tokens: ('and', 'that', 'are', 'not',              _,EOT); ClauseClass: TAndThatAreNotClause;     EndingClauseKind: eckNormal),
         (Tokens: ('and', 'that', 'is', 'not',               _,EOT); ClauseClass: TAndThatIsNotClause;      EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'are',                      _,_,EOT); ClauseClass: TAndThatAreClause;        EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'from',                     _,_,EOT); ClauseClass: TAndFromClause;           EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'in',                       _,_,EOT); ClauseClass: TAndInClause;             EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'is',                       _,_,EOT); ClauseClass: TAndThatIsClause;         EndingClauseKind: eckNormal),
         (Tokens: (',', 'and', 'on',                       _,_,EOT); ClauseClass: TAndOnClause;             EndingClauseKind: eckNormal),
         (Tokens: ('and', 'are', 'not',                    _,_,EOT); ClauseClass: TAndThatAreNotClause;     EndingClauseKind: eckNormal),
         (Tokens: ('and', 'is', 'not',                     _,_,EOT); ClauseClass: TAndThatIsNotClause;      EndingClauseKind: eckNormal),
         (Tokens: ('and', 'that', 'are',                   _,_,EOT); ClauseClass: TAndThatAreClause;        EndingClauseKind: eckNormal),
         (Tokens: ('and', 'that', 'is',                    _,_,EOT); ClauseClass: TAndThatIsClause;         EndingClauseKind: eckNormal),
         (Tokens: ('that', 'are', 'not',                   _,_,EOT); ClauseClass: TThatAreNotClause;        EndingClauseKind: eckNormal),
         (Tokens: ('that', 'is', 'not',                    _,_,EOT); ClauseClass: TThatIsNotClause;         EndingClauseKind: eckNormal),
         (Tokens: (',', 'and',                           _,_,_,EOT); ClauseClass: TAndClause;               EndingClauseKind: eckNormal),
         (Tokens: (',', 'plus',                          _,_,_,EOT); ClauseClass: TPlusClause;              EndingClauseKind: eckNormal),
         (Tokens: ('and', 'are',                         _,_,_,EOT); ClauseClass: TAndThatAreClause;        EndingClauseKind: eckNormal),
         (Tokens: ('and', 'from',                        _,_,_,EOT); ClauseClass: TAndFromClause;           EndingClauseKind: eckNormal),
         (Tokens: ('and', 'in',                          _,_,_,EOT); ClauseClass: TAndInClause;             EndingClauseKind: eckNormal),
         (Tokens: ('and', 'is',                          _,_,_,EOT); ClauseClass: TAndThatIsClause;         EndingClauseKind: eckNormal),
         (Tokens: ('and', 'on',                          _,_,_,EOT); ClauseClass: TAndOnClause;             EndingClauseKind: eckNormal),
         (Tokens: ('that', 'are',                        _,_,_,EOT); ClauseClass: TThatAreClause;           EndingClauseKind: eckNormal),
         (Tokens: ('that', 'is',                         _,_,_,EOT); ClauseClass: TThatIsClause;            EndingClauseKind: eckNormal),
         (Tokens: (',',                                _,_,_,_,EOT); ClauseClass: TAndClause;               EndingClauseKind: eckNormal),
         (Tokens: ('and',                              _,_,_,_,EOT); ClauseClass: TAndClause;               EndingClauseKind: eckNormal),
         (Tokens: ('but',                              _,_,_,_,EOT); ClauseClass: TButClause;               EndingClauseKind: eckNormal),
         (Tokens: ('from',                             _,_,_,_,EOT); ClauseClass: TFromClause;              EndingClauseKind: eckNormal),
         (Tokens: ('in',                               _,_,_,_,EOT); ClauseClass: TInClause;                EndingClauseKind: eckIn),
         (Tokens: ('on',                               _,_,_,_,EOT); ClauseClass: TOnClause;                EndingClauseKind: eckOn),
         (Tokens: ('plus',                             _,_,_,_,EOT); ClauseClass: TPlusClause;              EndingClauseKind: eckNormal)
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
               FThingList.Free();
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

function TThingCollector.GetThingList(): TThingList;
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
