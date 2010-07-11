{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit thingseeker;

interface

uses
   world, grammarian;

type
   TAllImpliedScope = set of (aisSurroundings, aisSelf);
   TClauseKind = (ckStart,
                  ckComma, ckCommaAnd, ckAnd{,
                  ckThatIs, ckThatIsNot, ckThatAre, ckThatAreNot,
                  ckBut,
                  ckFrom, ckIn});
   TClauseKinds = set of TClauseKind;

   TThingCollector = class
    protected
      FTokenCount: Cardinal;
      FMultiplesSuggested, FDisambiguate: Boolean;
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
      function GetMultiplesSuggested(): Boolean;
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
   TClauseFlags = set of (cfAllowExceptions, cfMultiplesSuggested, cfNoTokens);

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
      function GetInputFragment(Perspective: TAvatar; SiblingCount: Cardinal): AnsiString;
     public
      constructor Create(Number: Cardinal; SelectionMechanism: TThingSelectionMechanism; Flags: TClauseFlags; Things: PThingItem; InputFragment: AnsiString); virtual;
      constructor CreateAll(Things: PThingItem; AllowExceptions: Boolean); virtual;
      constructor CreateAllNoScope(); virtual;
      constructor CreateNoTokens(); virtual;
      destructor Destroy(); override;
      procedure CheckContext(); virtual;
      procedure ReportNoTokens(); virtual; abstract;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; virtual; abstract;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); virtual; abstract;
      function Select(): Boolean; { returns true if the result should be explicitly disambiguated }
      procedure Process(); virtual; abstract;
      procedure Add(Next: TAbstractClause);
      class function GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber; virtual;
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
   end;
   TCommaClause = class(TAbstractPlainJoiningClause)
      procedure ReportNoTokens(); override;
   end;
   TAndClause = class(TAbstractPlainJoiningClause)
      procedure ReportNoTokens(); override;
   end;

   TJoinArray = array of TAbstractJoiningClause;

   TJoinableClause = class(TAbstractClause)
     protected
      FRegisteredJoins: TJoinArray;
     public
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure RegisterJoin(Peer: TAbstractJoiningClause); override;
      procedure Process(); override;
   end;

   TAbstractFilteringClause = class(TJoinableClause)
      procedure Process(); override;
   end;

   TStartClause = class(TJoinableClause)
      procedure CheckContext(); override;
      function AcceptsJoins(Peer: TAbstractClause): Boolean; override;
      procedure Bank(var Things: PThingItem; var MultiplesSuggested: Boolean);
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
   if (Assigned(FThings) and Assigned(FThings^.Next)) then
      Include(FFlags, cfMultiplesSuggested);
end;

constructor TAbstractClause.CreateAll(Things: PThingItem; AllowExceptions: Boolean);
begin
   inherited Create();
   FNumber := 0;
   FSelectionMechanism := tsmPickAll;
   if (AllowExceptions) then
      FFlags := [cfAllowExceptions, cfMultiplesSuggested]
   else
      FFlags := [cfMultiplesSuggested];
   FThings := Things;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.CreateAll(', ThingListToLongDefiniteString(Things, nil, 'and'), ')'); {$ENDIF}
end;

constructor TAbstractClause.CreateAllNoScope();
begin
   inherited Create();
   FNumber := 0;
   FSelectionMechanism := tsmPickAll;
   FFlags := [cfMultiplesSuggested];
   FThings := nil;
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.CreateAllNoScope()'); {$ENDIF}
end;

constructor TAbstractClause.CreateNoTokens();
begin
   inherited Create();
   FFlags := [cfNoTokens];
{$IFDEF DEBUG_SEEKER} Writeln(' - TAbstractClause.CreateNoTokens()'); {$ENDIF}
end;

destructor TAbstractClause.Destroy();
begin
   FreeThingList(FThings);
   FNext.Free();
   inherited;
end;

function TAbstractClause.GetInputFragment(Perspective: TAvatar; SiblingCount: Cardinal): AnsiString;
begin
   if ((Assigned(FThings)) and (not Assigned(FThings^.Next))) then
      Result := FThings^.Value.GetLongDefiniteName(Perspective)
   else
   if (Length(FInputFragment) > 0) then
      Result := FInputFragment
   else
      Result := 'everything';
   if (SiblingCount > 0) then
   begin
      Assert(Assigned(FNext));
      Result := Result + ' ' + FNext.GetInputFragment(Perspective, SiblingCount-1);
   end;
end;

procedure TAbstractClause.CheckContext();
begin
   Assert(Assigned(FPrevious) xor (Self is TStartClause));
   if (cfNoTokens in FFlags) then
      ReportNoTokens();
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
   Assert((not (cfAllowExceptions in FFlags)) or (FSelectionMechanism = tsmPickAll));
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
   case FSelectionMechanism of
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
end;

procedure TAbstractClause.Add(Next: TAbstractClause);
begin
   Assert(Assigned(Next));
   Assert(not Assigned(FNext));
   Assert(not Assigned(Next.FPrevious));
   FNext := Next;
   Next.FPrevious := Self;
end;

class function TAbstractClause.GetPreferredGrammaticalNumber(DefaultGrammaticalNumber: TGrammaticalNumber): TGrammaticalNumber;
begin
   Result := DefaultGrammaticalNumber;
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

procedure TCommaClause.ReportNoTokens();
begin
   Fail('I don''t understand your use of commas.');
end;

procedure TAndClause.ReportNoTokens();
begin
   Fail('And what?');
end;


procedure TJoinableClause.Process();
var
   Index: Cardinal;
   CurrentThing: PThingItem;
begin
   if (Length(FRegisteredJoins) > 0) then
   begin
      Include(FFlags, cfMultiplesSuggested);
      CurrentThing := FThings;
      for Index := Low(FRegisteredJoins) to High(FRegisteredJoins) do
      begin
         Assert(Assigned(CurrentThing));
         while (Assigned(CurrentThing^.Next)) do
            CurrentThing := CurrentThing^.Next;
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

procedure TJoinableClause.RegisterJoin(Peer: TAbstractJoiningClause);
begin
   SetLength(FRegisteredJoins, Length(FRegisteredJoins)+1);
   FRegisteredJoins[High(FRegisteredJoins)] := Peer;
end;



procedure TAbstractFilteringClause.Process();
begin
   inherited;
   // WIP
   // This has to go through the people we're filtering and filter them
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

procedure TStartClause.Bank(var Things: PThingItem; var MultiplesSuggested: Boolean);
begin
   Things := FThings;
   FThings := nil;
   MultiplesSuggested := cfMultiplesSuggested in FFlags;
   Assert(Assigned(Things) or MultiplesSuggested);
end;


const
   ClauseKindToClass: array[TClauseKind] of TAbstractClauseClass = (
      TStartClause,
      TCommaClause,
      TAndClause,
      TAndClause{,
      TThatIsClause,
      TThatIsNotClause,
      TThatAreClause,
      TThatAreNotClause,
      TButClause,
      TFromClause,
      TInClause}
   );

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

   procedure Collapse(FirstClause: TAbstractClause; out Things: PThingItem; out MultiplesSuggested: Boolean; out Disambiguate: Boolean);
   var
      CurrentClause, LastClause: TAbstractClause;
   begin
      Assert(FirstClause is TStartClause);
      CurrentClause := FirstClause;
      Things := nil;
      MultiplesSuggested := False;
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
      (FirstClause as TStartClause).Bank(Things, MultiplesSuggested);
      Deduplicate(Things);
   end;

var
   CurrentToken: Cardinal;

   function CollectArticleAndThings(ClauseClass: TAbstractClauseClass; ClauseLength: Cardinal; out Clause: TAbstractClause): Boolean;

      function CollectExplicitThings(ExplicitGrammaticalNumber: TGrammaticalNumber; Number: Cardinal; SingularThingSelectionMechanism, PluralThingSelectionMechanism: TThingSelectionMechanism; AllowExceptions, ForceMatch: Boolean): Boolean;
      var
         FromOutside: Boolean;
         Start: Cardinal;
         Flags: TClauseFlags;
      begin
{$IFDEF DEBUG_SEEKER} Writeln('    CollectExplicitThings():'); {$ENDIF}
         Assert((ExplicitGrammaticalNumber <> []));
         Assert(not Assigned(FCurrentBestThingList));
         Start := CurrentToken;
         if (CurrentToken < Length(Tokens)) then
         begin
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
                  Assert(FCurrentPreferredGrammaticalNumber = ExplicitGrammaticalNumber);
                  if ((FCurrentPreferredGrammaticalNumber <= FCurrentBestGrammaticalNumber) and
                      (((ExplicitGrammaticalNumber = [gnSingular]) and (Tokens[CurrentToken] = 'one')) or
                       ((ExplicitGrammaticalNumber = [gnPlural]) and (Tokens[CurrentToken] = 'ones')))) then
                  begin
                     Inc(CurrentToken);
                  end;
               end;
               if (gnPlural in FCurrentBestGrammaticalNumber) then { take apples }
               begin
                  Assert(Assigned(FCurrentBestThingList));
                  if (AllowExceptions) then
                     Flags := [cfAllowExceptions, cfMultiplesSuggested]
                  else
                     Flags := [cfMultiplesSuggested];
                  Clause := ClauseClass.Create(Number, PluralThingSelectionMechanism, Flags, FCurrentBestThingList, Serialise(OriginalTokens, Start, CurrentToken - Start));
               end
               else { take apple }
               begin
                  Assert(gnSingular in FCurrentBestGrammaticalNumber);
                  Assert(Assigned(FCurrentBestThingList));
                  Clause := ClauseClass.Create(Number, SingularThingSelectionMechanism, [], FCurrentBestThingList, Serialise(OriginalTokens, Start, CurrentToken - Start));
               end;
               FCurrentBestThingList := nil;
            end
            else
            begin
               Assert(Start = CurrentToken);
               if (ForceMatch) then
                  Fail('I can''t see any "' + Serialise(OriginalTokens, CurrentToken, 1) + '" here to ' + Verb + '.');
               Clause := nil;
            end;
         end
         else
         begin
            Clause := ClauseClass.CreateNoTokens();
         end;
{$IFDEF DEBUG_SEEKER} Writeln('    CollectExplicitThings() finished.'); {$ENDIF}
         Result := Assigned(Clause);
      end;

      procedure CollectImplicitThings(AllowExceptions: Boolean);
      var
         FromOutside: Boolean;
      begin
{$IFDEF DEBUG_SEEKER} Writeln('    CollectImplicitThings():'); {$ENDIF}
         Assert(not Assigned(FCurrentBestThingList));
         if (aisSurroundings in Scope) then
         begin
            Perspective.GetSurroundingsRoot(FromOutside).AddImplicitlyReferencedThings(Perspective, FromOutside, aisSelf in Scope, tpExplicit, [], FCurrentBestThingList);
            Clause := ClauseClass.CreateAll(FCurrentBestThingList, AllowExceptions);
         end
         else
         if (aisSelf in Scope) then
         begin
            Perspective.AddImplicitlyReferencedThings(Perspective, True, True, tpExplicit, [], FCurrentBestThingList);
            Clause := ClauseClass.CreateAll(FCurrentBestThingList, AllowExceptions);
         end
         else
            Clause := ClauseClass.CreateAllNoScope();
         FCurrentBestThingList := nil;
{$IFDEF DEBUG_SEEKER} Writeln('    CollectImplicitThings() finished'); {$ENDIF}
      end;

{
   object lists:
    - "one <either-s>"               - there must be one or more, pick number "randomly"
    - "one of the <either-p>"        - there must be one or more, pick number "randomly"; had definite article
    - "<number> <either-p>"          - there must be <number> or more, pick <number> of them "randomly"
    - "the one <either-s>"           - there must be one, pick number; had definite article
    - "the <number> <either-p>"      - there must be <number>, pick number; had definite article

    - "any of the <either-p>"        - there must be one or more, pick number "randomly", allow exceptions; had definite article
    - "any of the <number> <plural>" - there must be one or more, pick number "randomly", allow exceptions; had definite article
    - "any <plural>"                 - there must be zero or more, pick them all, allow exceptions
    - "any <singular>"               - there must be one or more, pick number "randomly", allow exceptions
}

   begin
{$IFDEF DEBUG_SEEKER} Writeln('CollectArticleAndThings() called'); {$ENDIF}
      Assert(not Assigned(FCurrentBestThingList));
      Assert(Assigned(ClauseClass));
      Clause := nil;
      Inc(CurrentToken, ClauseLength);
      if (TryMatch(CurrentToken, Tokens, ['all', 'of', 'the'])) then
         CollectExplicitThings([gnPlural, gnSingular], 1, tsmPickOnlyNumber, tsmPickAll, True, True)
      { "take all of the <number> spades" isn't supported }
      { "take all of spade" isn't supported; should it be? }
      else
      if (TryMatch(CurrentToken, Tokens, ['all', 'the'])) then
         CollectExplicitThings([gnPlural], 1, tsmPickOnlyNumber, tsmPickAll, True, True)
      else
      if (TryMatch(CurrentToken, Tokens, ['all'])) then
      begin
         if ((CurrentToken >= Length(Tokens)) or (not CollectExplicitThings([gnPlural], 1, tsmPickAll, tsmPickAll, True, False))) then
            CollectImplicitThings(True);
      end
      else
      if (TryMatch(CurrentToken, Tokens, ['everything'])) then
         CollectImplicitThings(True)
      else
      if (TryMatch(CurrentToken, Tokens, ['a'])) then
         CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, False, True)
      else
      if (TryMatch(CurrentToken, Tokens, ['an'])) then
         CollectExplicitThings([gnSingular], 1, tsmPickNumber, tsmPickNumber, False, True)
      else
      if (TryMatch(CurrentToken, Tokens, ['every'])) then
         CollectExplicitThings([gnSingular], 1, tsmPickAll, tsmPickAll, True, True)
      else
      if (TryMatch(CurrentToken, Tokens, ['the'])) then
         CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyNumber, tsmPickAll, False, True)
      else
      if (TryMatch(CurrentToken, Tokens, ['some'])) then
         CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickNumber, tsmPickSome, False, True)
      else
         CollectExplicitThings(ClauseClass.GetPreferredGrammaticalNumber(PreferredGrammaticalNumber), 1, tsmPickOnlyNumber, tsmPickAll, False, False);
      Result := Assigned(Clause);
      if (not Result) then
         Dec(CurrentToken, ClauseLength);
      Assert(not Assigned(FCurrentBestThingList));
{$IFDEF DEBUG_SEEKER} Writeln(' = CollectArticleAndThings() returned ', Result); {$ENDIF}
   end;

   function TryClause(ClauseKind: TClauseKind; CandidateTokens: array of AnsiString; out NextClauseKind: TClauseKind; out NextClauseLength: Cardinal): Boolean;
   var
      Index: Cardinal;
{$IFDEF DEBUG_SEEKER}
I: Integer;
S: String;
{$ENDIF}
   begin
{$IFDEF DEBUG_SEEKER}
S := '';
for I := 0 to Length(CandidateTokens)-1 do
   S := S + ' "' + CandidateTokens[I] + '"';
Writeln(' * TryClause(', ClauseKind, ',', S, '):');
{$ENDIF}
      Assert(ClauseKind <> ckStart);
      Assert(Length(CandidateTokens) > 0);
      if ((not (ClauseKind in Ends)) and (CurrentToken + Length(CandidateTokens) <= Length(Tokens))) then
      begin
         for Index := 0 to Length(CandidateTokens) - 1 do
         begin
            if (Tokens[CurrentToken + Index] <> CandidateTokens[Index]) then
            begin
               Result := False;
{$IFDEF DEBUG_SEEKER}
Writeln(' * TryClause() finished, result = ', Result);
{$ENDIF}
               Exit;
            end;
         end;
         NextClauseKind := ClauseKind;
         NextClauseLength := Length(CandidateTokens);
         Result := True;
      end
      else
      begin
         Result := False;
      end;
{$IFDEF DEBUG_SEEKER}
Writeln(' * TryClause() finished, result = ', Result);
{$ENDIF}
   end;

var
   NextClauseKind: TClauseKind;
   NextClauseLength: Cardinal;
   FirstClause, CurrentClause, NextClause: TAbstractClause;
begin
{$IFDEF DEBUG_SEEKER} Writeln('collecting for: "' + Serialise(OriginalTokens, Start, 1) + '" of "' + Serialise(OriginalTokens, 0, Length(OriginalTokens)) + '"'); {$ENDIF}
   {$IFOPT C+}
   Assert(not FCurrentlyCollecting);
   Assert(not FCollected);
   Assert(FTokenCount = 0);
   Assert(not FMultiplesSuggested);
   Assert(not FDisambiguate);
   Assert(not Assigned(FThingList));
   FCurrentlyCollecting := True;
   Result := False;
   try
   {$ENDIF}
      CurrentToken := Start;
      if (CollectArticleAndThings(TStartClause, 0, FirstClause)) then
      begin
{$IFDEF DEBUG_SEEKER}
Writeln('FirstClause matched ', CurrentToken - Start, ' tokens.');
{$ENDIF}
         try
            Assert(Assigned(FirstClause));
            CurrentClause := FirstClause;
            while ((CurrentToken < Length(Tokens)) and
                   ({TryClause(ckFrom, ['from'], NextClauseKind, NextClauseLength) or
                    TryClause(ckIn, ['in'], NextClauseKind, NextClauseLength) or }
                    TryClause(ckCommaAnd, [',', 'and'], NextClauseKind, NextClauseLength) or
                    TryClause(ckComma, [','], NextClauseKind, NextClauseLength) or
                    TryClause(ckAnd, ['and'], NextClauseKind, NextClauseLength)) and
                   (CollectArticleAndThings(ClauseKindToClass[NextClauseKind], NextClauseLength, NextClause))) do
            begin
{$IFDEF DEBUG_SEEKER}
Writeln('NextClause increased the total number of matched tokens to ', CurrentToken - Start, ' tokens.');
{$ENDIF}
               Assert(Assigned(NextClause));
               CurrentClause.Add(NextClause);
               CurrentClause := NextClause;
            end;
            Assert(FTokenCount = 0);
            Assert(not FMultiplesSuggested);
            Assert(not FDisambiguate);
            Assert(not Assigned(FThingList));
            Assert(Assigned(FirstClause));
            try
               Collapse(FirstClause, FThingList, FMultiplesSuggested, FDisambiguate);
               FTokenCount := CurrentToken - Start;
{$IFDEF DEBUG_SEEKER}
if (FTokenCount > 0) then
 Writeln('collapsed "' + Serialise(OriginalTokens, Start, FTokenCount) + '" to "' + ThingListToLongDefiniteString(FThingList, Perspective, 'and') + '"; multiples=', FMultiplesSuggested)
else
 Writeln('collapsed nothing to "' + ThingListToLongDefiniteString(FThingList, Perspective, 'and') + '"; multiples=', FMultiplesSuggested);
{$ENDIF}
            except
               FTokenCount := 0;
               FMultiplesSuggested := False;
               FDisambiguate := False;
               FreeThingList(FThingList);
               FThingList := nil;
               raise;
            end;
            Result := True;
         finally
            FirstClause.Free();
         end;
      end
      else
      begin
         Result := False;
         Assert(FTokenCount = 0);
         Assert(not FMultiplesSuggested);
         Assert(not FDisambiguate);
         Assert(not Assigned(FThingList));
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
         Assert(not FMultiplesSuggested);
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

function TThingCollector.GetMultiplesSuggested(): Boolean;
begin
   {$IFOPT C+} Assert(not FCurrentlyCollecting); {$ENDIF}
   {$IFOPT C+} Assert(FCollected); {$ENDIF}
   Result := FMultiplesSuggested;
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
   FMultiplesSuggested := False;
   FDisambiguate := False;
   FThingList := nil;
   {$IFOPT C+} FCollected := False; {$ENDIF}
end;

end.
