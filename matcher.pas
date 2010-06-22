{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit matcher;

interface

uses
   storable, grammarian;

type
   TByteCode = Byte;
   PCompiledPattern = ^TCompiledPattern;
   TCompiledPattern = packed array[TByteCode] of TByteCode;

   TMatcher = class(TStorable)
    protected
      FTokens: TTokens; { must be stored lexically sorted }
      FOriginalTokens: TTokens; { must be stored in the same order as FTokens }
      FPattern: PCompiledPattern;
      FPatternLength: TByteCode;
      function GetTokenID(Token: AnsiString): TByteCode; { argument must be lowercase }
    public
      constructor Create(Tokens, OriginalTokens: TTokens; Pattern: PCompiledPattern; PatternLength: TByteCode); { tokens are case-aware }
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function Matches(Tokens: TTokens; Start: Cardinal): Cardinal; { case-insensitive, but tokens must be lowercase already }
      function GetLongestMatch(Separator: AnsiString): AnsiString;
      {$IFDEF DEBUG} function GetPatternDescription(): AnsiString; {$ENDIF}
      {$IFDEF DEBUG} function GetPatternDotFileRecords(): AnsiString; {$ENDIF}
      {$IFDEF DEBUG} function GetPatternDotFileLabels(): AnsiString; {$ENDIF}
   end;

procedure CompilePattern(S: AnsiString; out Singular: TMatcher; out Plural: TMatcher);

{

   CompilePattern() takes a string that consists of a space-separated list of tokens or nested lists.
   Nested lists are marked by round brackets (...).
   Tokens can have a "+" suffix indicating that the token can be repeated.
   Tokens can have a "?" suffix indicating that the token can be omitted.
   Nested lists can have suffixes to indicate what kind of list it is:
     (a b c)   - sequence list (all tokens must appear in order)
     (a b c)?  - optional sequence list (if any appear, they must all appear, in order)
     (a b c)@  - alternatives (one of the tokens must appear)
     (a b c)*  - zero or more of the alternatives must appear, in any order
     (a b c)#  - one or more of the alternatives must appear, in any order
     (a b c)%  - zero or more of the alternatives must appear, but they must be in the order given
     (a b c)&  - one or more of the alternatives must appear, but they must be in the order given
   Tokens and nested lists can be split with a "/" to indicate alternative singular/plural forms.
   Special characters can be escaped using \.

   Examples:
     'a b c' - only matched by "a b c"

     'the? ((glowing green)# lantern/lanterns)&' - returns a singular matcher that matches:
         "the glowing", "the green", "the lantern",
         "the glowing green", "the glowing lantern", "the green lantern",
         "the glowing green lantern", and all of those again without "the"
     ...and a plural matcher that matches the same but with "lanterns" instead of "lantern".

     '(two beads)/bead' returns a matcher that matches "two beads" and
     a matcher that matches "bead".

}

{$IFDEF DEBUG}
function HasPatternChars(S: AnsiString): Boolean;
{$ENDIF}

implementation

uses
   sysutils;

 { A compiled pattern consists of a sequence of sequences of byte
   pairs. Each sequence of byte pairs is a state for a
   pattern-matching state machine and is identified by the position of
   the first byte in that sequence in the overall sequence. The first
   state in the pattern is the start state for running the
   pattern. Each byte pair represents a state transition for the state
   and consists of a byte representing the token to match to follow
   the transition, and a byte representing the state to switch to if
   that token is matched. The special token IDs $FE and $FF are always
   matched. They mean the same, except that the last byte pair in each
   state is $FF, other "always matched" transitions are $FE. There is
   always at least one of these transitions; the $FF is how the last
   transition of the state is recognised. There are also two special
   state IDs; $FE means the pattern matched, $FF means it failed. The
   least significant bit of the second byte of each transition is set
   if the state machine must avoid transitioning through this state
   twice while matching the pattern.

   For example,
      00 04 FF FF 01 FE FF FF
   ...would match the token sequence 00 01, but nothing else.

   Similarly,
      00 04 FF 04 01 FE FF FF
   ...would match either the token 01 on its own, or the token
   sequence 00 01.

   Transitions are listed in the states of a pattern in _reverse_
   canonical order.

   For example:
     00 07 01 07 02 07 FF FF FE 00 FF FE
   ...would match the following sequences:
     00, 01, 02, 00 01, 00 02, 01 00, 01 02, 02 00, 02 01, 00 01 02,
     00 02 01, 01 00 02, 01 02 00, 02 00 01, 02 01 00
   ...but the canonical longest sequence that represents this pattern
   would be:
     02 01 00

   }

const
   { Magic Tokens }
   mtFollow = $FE;
   mtLastFollow = $FF;
   mtMaxTrueToken = $FD;
   mtFollowMask = $FE; { mtFollow and mtFollowMask = mtLastFollow and mtFollowMask = mtFollowMask }
   mtNone = $FF;
   { Magic Pattern States }
   mpsMatch = $FE;
   mpsFail = $FF;
   mpsMaxTrueTransition = $FD;
   mpsPreventDuplicatesMask = $01;

type
   PState = ^TState;
   PTransition = ^TTransition;

   TState = record
     TokenTransitions: PTransition;
     FollowTransitions: PTransition;
     TransitionCount: Cardinal;
     NextState: PState;
     PreviousState: PState;
     Index: TByteCode;
   end;

   TTransition = record
     Token: TByteCode;
     State: PState;
     BlockDuplicates: Boolean;
     NextTransition: PTransition;
   end;

   TGetStateCallback = function (): PState of object;

   TTokenReporterCallback = procedure (Token: AnsiString) of object;
   TTokenFinderCallback = function (Token: AnsiString): TByteCode of object;

   TPatternNode = class
    protected
     procedure ReportTokens(Callback: TTokenReporterCallback); virtual; abstract;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); virtual; abstract;
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); virtual; abstract;
    public
     constructor Create();
     destructor Destroy(); override;
   end;
   TTokenNode = class(TPatternNode)
    protected
     FToken: AnsiString;
     FTokenID: TByteCode;
     procedure ReportTokens(Callback: TTokenReporterCallback); override;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); override;
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
    public
     constructor Create(Token: AnsiString);
   end;
   TRepeatableTokenNode = class(TTokenNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
    public
   end;
   TChildrenPatternNode = class(TPatternNode)
    protected
     FChildren: array of TPatternNode;
     procedure ReportTokens(Callback: TTokenReporterCallback); override;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); override;
    public
     constructor Create(Children: array of TPatternNode); // fails if the array is length=0
     destructor Destroy(); override;
   end;
   TSequencePatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;
   TOptionalSequencePatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;
   TRepeatableSequencePatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;
   TAlternativesPatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;
   TZeroOrMoreUnorderedPatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;
   TZeroOrMoreOrderedPatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;
   TOneOrMoreUnorderedPatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;
   TOneOrMoreOrderedPatternNode = class(TChildrenPatternNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
   end;

procedure DualQuickSort(var MasterList, SlaveList: TTokens); forward;
procedure DualQuickSort(var MasterList, SlaveList: TTokens; L, R: Integer); forward;

procedure DualQuickSort(var MasterList, SlaveList: TTokens);
begin
   Assert(Low(MasterList) >= Low(Integer));
   Assert(High(MasterList) <= High(Integer));
   Assert(Length(MasterList) = Length(SlaveList));
   if (Length(MasterList) > 1) then
      DualQuickSort(MasterList, SlaveList, Low(MasterList), High(MasterList));
end;

procedure DualQuickSort(var MasterList, SlaveList: TTokens; L, R: Integer);
{ based on QuickSort in rtl/objpas/classes/lists.inc }
var
   I, J : Integer;
   P, Q : AnsiString;
begin
   repeat
      I := L;
      J := R;
      P := MasterList[(L + R) div 2];
      repeat
         while P > MasterList[I] do
            I := I + 1;
         while P < MasterList[J] do
            J := J - 1;
         if (I <= J) then
         begin
            Q := MasterList[I];
            MasterList[I] := MasterList[J];
            MasterList[J] := Q;
            Q := SlaveList[I];
            SlaveList[I] := SlaveList[J];
            SlaveList[J] := Q;
            I := I + 1;
            J := J - 1;
         end;
      until I > J;
      if (L < J) then
         DualQuickSort(MasterList, SlaveList, L, J);
      L := I;
   until I >= R;
end;

procedure DualRemoveDuplicates(var MasterList, SlaveList: TTokens);
var
   Index, Count: Cardinal;
   NewMasterList, NewSlaveList: TTokens;
   Last: AnsiString;
begin
   Assert(Length(MasterList) = Length(SlaveList));
   Assert(Length(MasterList) > 0);
   SetLength(NewMasterList, Length(MasterList));
   NewMasterList[0] := MasterList[0];
   SetLength(NewSlaveList, Length(SlaveList));
   NewSlaveList[0] := SlaveList[0];
   Index := 1;
   Count := 1;
   Last := MasterList[0];
   while (Index < Length(MasterList)) do
   begin
      if (MasterList[Index] <> Last) then
      begin
         Last := MasterList[Index];
         NewMasterList[Count] := Last;
         NewSlaveList[Count] := SlaveList[Index];
         Inc(Count);
      end;
      Inc(Index);
   end;
   SetLength(NewMasterList, Count);
   MasterList := NewMasterList;
   SetLength(NewSlaveList, Count);
   SlaveList := NewSlaveList;
end;

procedure AddTransition(State: PState; Token: TByteCode; TargetState: PState; BlockDuplicates: Boolean);
var
   Transition: PTransition;
begin
   Assert(Assigned(State));
   New(Transition);
   Transition^.Token := Token;
   Transition^.State := TargetState;
   Transition^.BlockDuplicates := BlockDuplicates;
   if (Token = mtFollow) then
   begin
      Transition^.NextTransition := State^.FollowTransitions;
      State^.FollowTransitions := Transition;
   end
   else
   begin
      Transition^.NextTransition := State^.TokenTransitions;
      State^.TokenTransitions := Transition;
   end;
   Inc(State^.TransitionCount);
end;

procedure NewPattern(out Pattern: PCompiledPattern; Length: TByteCode); inline;
begin
   Assert(Length > 0);
   Pattern := nil; { not strictly necessary }
   GetMem(Pattern, Length*SizeOf(TByteCode));
end;

procedure DisposePattern(var Pattern: PCompiledPattern; Length: TByteCode); inline;
begin
   Assert(Assigned(Pattern));
   FreeMem(Pattern, Length*SizeOf(TByteCode));
   Pattern := nil;
end;


constructor TPatternNode.Create();
begin
   inherited;
end;

destructor TPatternNode.Destroy();
begin
   inherited;
end;


constructor TTokenNode.Create(Token: AnsiString);
begin
   inherited Create();
   FToken := Token;
end;

procedure TTokenNode.ReportTokens(Callback: TTokenReporterCallback);
Begin
   Callback(FToken);
end;

procedure TTokenNode.FixTokenIDs(Callback: TTokenFinderCallback);
begin
   FTokenID := Callback(FToken);
end;

procedure TTokenNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
begin
   {
           Token
       A=---------->Z
   }
   AddTransition(StartState, FTokenID, TargetState, BlockDuplicates);
end;


procedure TRepeatableTokenNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   MiddleState1, MiddleState2: PState;
begin
   {
                        Token
       A=---------->O----------->O----------->Z
                     <-----------
   }
   MiddleState1 := GetNewState();
   MiddleState2 := GetNewState();
   AddTransition(StartState, mtFollow, MiddleState1, BlockDuplicates);
   AddTransition(MiddleState1, FTokenID, MiddleState2, False);
   AddTransition(MiddleState2, mtFollow, MiddleState1, False);
   AddTransition(MiddleState2, mtFollow, TargetState, False);
end;

constructor TChildrenPatternNode.Create(Children: array of TPatternNode);
var
   Index: Cardinal;
begin
   Assert(Length(Children) > 0);
   inherited Create();
   SetLength(FChildren, Length(Children));
   for Index := 0 to Length(Children)-1 do
      FChildren[Index] := Children[Index];
end;

destructor TChildrenPatternNode.Destroy(); 
var
   Index: Cardinal;
begin
   for Index := Low(FChildren) to High(FChildren) do
      FChildren[Index].Free();
end;

procedure TChildrenPatternNode.ReportTokens(Callback: TTokenReporterCallback);
var
   Index: Cardinal;
begin
   for Index := Low(FChildren) to High(FChildren) do
      FChildren[Index].ReportTokens(Callback);
end;

procedure TChildrenPatternNode.FixTokenIDs(Callback: TTokenFinderCallback);
var
   Index: Cardinal;
begin
   for Index := Low(FChildren) to High(FChildren) do
      FChildren[Index].FixTokenIDs(Callback);
end;

procedure TSequencePatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   CurrentState, NextState: PState;
   Index: Cardinal;
begin
   {
       A=--[...]--->(O---[...]--->)*Z
   }
   CurrentState := StartState;
   if (Length(FChildren) > 1) then
      for Index := Low(FChildren) to High(FChildren)-1 do
      begin
         NextState := GetNewState();
         FChildren[Index].HookStates(CurrentState, NextState, GetNewState, BlockDuplicates);
         BlockDuplicates := False;
         CurrentState := NextState;
      end;
   FChildren[High(FChildren)].HookStates(CurrentState, TargetState, GetNewState, BlockDuplicates);
end;

procedure TOptionalSequencePatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   FirstState, CurrentState, NextState: PState;
   Index: Cardinal;
begin
   {
       A=---------->O---[...]--->(O---[...]--->)*Z
                     --------------------------->
   }
   if (BlockDuplicates) then
   begin
      FirstState := GetNewState();
      AddTransition(StartState, mtFollow, FirstState, BlockDuplicates);
   end
   else
      FirstState := StartState;
   CurrentState := FirstState;
   if (Length(FChildren) > 1) then
      for Index := Low(FChildren) to High(FChildren)-1 do
      begin
         NextState := GetNewState();
         FChildren[Index].HookStates(CurrentState, NextState, GetNewState, BlockDuplicates);
         BlockDuplicates := False;
         CurrentState := NextState;
      end;
   FChildren[High(FChildren)].HookStates(CurrentState, TargetState, GetNewState, BlockDuplicates);
   AddTransition(FirstState, mtFollow, TargetState, False);
end;

procedure TRepeatableSequencePatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   FirstState, CurrentState, NextState: PState;
   Index: Cardinal;
begin
   {
       A=---------->O---[...]--->(O---[...]--->)*Z
                     <---------------------------
   }
   if (BlockDuplicates) then
   begin
      FirstState := GetNewState();
      AddTransition(StartState, mtFollow, FirstState, BlockDuplicates);
   end
   else
      FirstState := StartState;
   CurrentState := FirstState;
   if (Length(FChildren) > 1) then
      for Index := Low(FChildren) to High(FChildren)-1 do
      begin
         NextState := GetNewState();
         FChildren[Index].HookStates(CurrentState, NextState, GetNewState, BlockDuplicates);
         BlockDuplicates := False;
         CurrentState := NextState;
      end;
   FChildren[High(FChildren)].HookStates(CurrentState, TargetState, GetNewState, BlockDuplicates);
   AddTransition(TargetState, mtFollow, FirstState, False);
end;

procedure TAlternativesPatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   Index: Cardinal;
   MiddleState: PState;
begin
   {
     If BlockDuplicates is false:
       A---[...]--->Z
       (---[...]--->)*
     If BlockDuplicates is true:
       A=---------->M---[...]--->Z
                    (---[...]--->)*
     (because
        TZeroOrMoreUnorderedPatternNode.Create([TAlternativesPatternNode.Create([TTokenNode.Create('A'),
                                                                                 TTokenNode.Create('B')]),
                                                TTokenNode.Create('C')])
      ...should allow AC and BC but not AB, ABC, or BAC)
   }
   if (BlockDuplicates) then
   begin
      MiddleState := GetNewState();
      AddTransition(StartState, mtFollow, MiddleState, BlockDuplicates);
   end
   else
      MiddleState := StartState;
   for Index := Low(FChildren) to High(FChildren) do
      FChildren[Index].HookStates(MiddleState, TargetState, GetNewState, BlockDuplicates);
end;

procedure TZeroOrMoreUnorderedPatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   Index: Cardinal;
   MiddleState: PState;
begin
   {
       A=---------->M............M----------->Z
                     <---[...]-==
                    (<---[...]-==)*
     (because
        TAlternativesPatternNode.Create([TZeroOrMoreUnorderedPatternNode.Create([TTokenNode.Create('A'),
                                                                                 TTokenNode.Create('B')]),
                                         TTokenNode.Create('C')])
      ...should allow AB, BA, and C, but not CA etc)
   }
   MiddleState := GetNewState();
   AddTransition(StartState, mtFollow, MiddleState, BlockDuplicates);
   for Index := Low(FChildren) to High(FChildren) do
      FChildren[Index].HookStates(MiddleState, MiddleState, GetNewState, True);
   AddTransition(MiddleState, mtFollow, TargetState, False);
end;

procedure TZeroOrMoreOrderedPatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   Index: Cardinal;
   CurrentState, NextState: PState;
begin
   {
     If BlockDuplicates is false:
       A---[...]--->(O---[...]--->)*Z
        ----------->  ----------->
     If BlockDuplicates is true:
       A=---------->M---[...]--->(O---[...]--->)*Z
                     ----------->  ----------->
   }
   if (BlockDuplicates) then
   begin
      CurrentState := GetNewState();
      AddTransition(StartState, mtFollow, CurrentState, BlockDuplicates);
   end
   else
      CurrentState := StartState;
   if (Length(FChildren) > 1) then
      for Index := Low(FChildren) to High(FChildren)-1 do
      begin
         NextState := GetNewState();
         FChildren[Index].HookStates(CurrentState, NextState, GetNewState);
         AddTransition(CurrentState, mtFollow, NextState, False);
         CurrentState := NextState;
      end;
   FChildren[High(FChildren)].HookStates(CurrentState, TargetState, GetNewState, BlockDuplicates);
   AddTransition(CurrentState, mtFollow, TargetState, False);
end;

procedure TOneOrMoreUnorderedPatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   Index: Cardinal;
   MiddleState1, MiddleState2: PState;
begin
   {
       A=---------->O(==-[...]--->)*O----------->Z
                      <-----------
   }
   MiddleState1 := GetNewState();
   MiddleState2 := GetNewState();
   AddTransition(StartState, mtFollow, MiddleState1, BlockDuplicates);
   for Index := Low(FChildren) to High(FChildren) do
      FChildren[Index].HookStates(MiddleState1, MiddleState2, GetNewState, True);
   AddTransition(MiddleState2, mtFollow, MiddleState1, False);
   AddTransition(MiddleState2, mtFollow, TargetState, False);
end;

procedure TOneOrMoreOrderedPatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   Index: Cardinal;
   S1, S2, S3, S4: PState;
begin
   {
     One child:
       A=--[...]--->Z

     Two children without BlockDuplicates:
       /------------------------->\
       A---[...]--->S2----------->S3---[...]--->Z
                     \------------------------->/
      (S1)                       (S1)          (S2)

     Three or more children without BlockDuplicates:
      (............................)*
       /------------------------->\/------------------------->\
       A---[...]--->S2----------->S3---[...]--->S4----------->S5---[...]--->Z
                     \------------------------->/\------------------------->/
      (S1)                       (S1)          (S2)          (S3)          (S4)
                                                             (S1)          (S2)
     Two children with BlockDuplicates:
                     /------------------------->\
       A=---------->S1---[...]--->S2----------->S3---[...]--->Z
                                   \------------------------->/
                                               (S1)          (S2)

     Three or more children with BlockDuplicates:
                    (............................)*
                     /------------------------->\/------------------------->\
       A=---------->S1---[...]--->S2----------->S3---[...]--->S4----------->S5---[...]--->Z
                                   \------------------------->/\------------------------->/
                                               (S1)          (S2)          (S3)          (S4)
                                                                           (S1)          (S2)
   }
   if (Length(FChildren) = 1) then
   begin
      FChildren[Low(FChildren)].HookStates(StartState, TargetState, GetNewState, BlockDuplicates);
   end
   else
   begin
      if (BlockDuplicates) then
      begin
         S1 := GetNewState();
         AddTransition(StartState, mtFollow, S1, BlockDuplicates);
      end
      else
      begin
         S1 := StartState;
      end;
      S2 := GetNewState();
      if (Length(FChildren) > 2) then
         for Index := Low(FChildren) to High(FChildren)-2 do
         begin
            S3 := GetNewState();
            S4 := GetNewState();
            FChildren[Index].HookStates(S1, S2, GetNewState);
            AddTransition(S1, mtFollow, S3, False);
            AddTransition(S2, mtFollow, S3, False);
            AddTransition(S2, mtFollow, S4, False);
            S1 := S3;
            S2 := S4;
         end;
      S3 := GetNewState();
      FChildren[High(FChildren)-1].HookStates(S1, S2, GetNewState);
      AddTransition(S1, mtFollow, S3, False);
      AddTransition(S2, mtFollow, S3, False);
      FChildren[High(FChildren)].HookStates(S3, TargetState, GetNewState);
      AddTransition(S2, mtFollow, TargetState, False);
   end;
end;


type
   TPatternCompiler = class
    protected
     FRoot: TPatternNode;
     FTokens, FOriginalTokens: TTokens;
     FFirstState, FLastState: PState;
     function GetNewState(): PState;
     procedure TokenCollector(Token: AnsiString);
     function GetTokenID(Token: AnsiString): TByteCode; { case-sensitive }
    public
     constructor Create(Root: TPatternNode);
     destructor Destroy(); override;
     function GetAtomisedTokens(): TTokens;
     function GetAtomisedOriginalTokens(): TTokens;
     procedure GetCompiledPattern(out Pattern: PCompiledPattern; out PatternLength: TByteCode);
   end;

constructor TPatternCompiler.Create(Root: TPatternNode);
begin
   inherited Create();
   FRoot := Root;
   FRoot.ReportTokens(@TokenCollector);
   Assert(High(FTokens) <= High(TByteCode), 'Too many unique strings in pattern');
   DualQuickSort(FTokens, FOriginalTokens);
   DualRemoveDuplicates(FTokens, FOriginalTokens);
   FRoot.FixTokenIDs(@GetTokenID);
end;

destructor TPatternCompiler.Destroy();
begin
   Assert(Assigned(FRoot));
   Assert(not Assigned(FFirstState));
   Assert(not Assigned(FLastState));
   FRoot.Destroy();
   inherited;
end;

function TPatternCompiler.GetNewState(): PState;
begin
   New(Result);
   Result^.TokenTransitions := nil;
   Result^.FollowTransitions := nil;
   Result^.TransitionCount := 0;
   Result^.Index := 0;
   Result^.NextState := FLastState;
   Result^.PreviousState := FLastState^.PreviousState;
   FLastState^.PreviousState^.NextState := Result;
   FLastState^.PreviousState := Result;
end;

procedure TPatternCompiler.TokenCollector(Token: AnsiString);
begin
   SetLength(FTokens, Length(FTokens)+1);
   FTokens[High(FTokens)] := Canonicalise(Token);
   SetLength(FOriginalTokens, Length(FOriginalTokens)+1);
   FOriginalTokens[High(FOriginalTokens)] := Token;
end;

function TPatternCompiler.GetTokenID(Token: AnsiString): TByteCode;
var
   L, R, M: TByteCode;
begin
   Assert(Low(FTokens) >= Low(TByteCode));
   Assert(High(FTokens) <= High(TByteCode));
   Token := Canonicalise(Token);
   L := Low(FTokens);
   R := High(FTokens);
   repeat
      M := (R-L) div 2 + L;
      if (FTokens[M] < Token) then
         L := M+1
      else
         R := M;
   until (L >= R);
   Assert(FTokens[R] = Token);
   Result := R;
end;

function TPatternCompiler.GetAtomisedTokens(): TTokens;
begin
   Result := FTokens;
end;

function TPatternCompiler.GetAtomisedOriginalTokens(): TTokens;
begin
   Result := FOriginalTokens;
end;

procedure TPatternCompiler.GetCompiledPattern(out Pattern: PCompiledPattern; out PatternLength: TByteCode);

   function GetUltimateTarget(State: PState): PState;
   begin
      Result := State;
      while (Assigned(Result) and (Result <> FLastState) and (Result^.TransitionCount = 1)) do
      begin
         Assert(not Assigned(Result^.TokenTransitions));
         Assert(Assigned(Result^.FollowTransitions));
         Assert(not Assigned(Result^.FollowTransitions^.NextTransition));
         Assert(Assigned(Result^.FollowTransitions^.State));
         Result := Result^.FollowTransitions^.State;
      end;
   end;

   procedure OptimiseTransitions(Transition: PTransition);
   begin
      while (Assigned(Transition)) do
      begin
         Transition^.State := GetUltimateTarget(Transition^.State);
         Transition := Transition^.NextTransition;
      end;

   end;

var
   State: PState;
   Index: Cardinal;
   Transition: PTransition;
begin
   { Build state machine }
   New(FFirstState);
   FFirstState^.TokenTransitions := nil;
   FFirstState^.FollowTransitions := nil;
   FFirstState^.TransitionCount := 0;
   FFirstState^.Index := 0;
   FFirstState^.PreviousState := nil;
   New(FLastState);
   FLastState^.TokenTransitions := nil;
   FLastState^.FollowTransitions := nil;
   FLastState^.TransitionCount := 0;
   FLastState^.Index := 0;
   FLastState^.NextState := nil;
   FFirstState^.NextState := FLastState;
   FLastState^.PreviousState := FFirstState;
   FRoot.HookStates(FFirstState, FLastState, @GetNewState);
   { Fill in transitions and check for invariants }
   State := FFirstState;
   while (Assigned(State)) do
   begin
      Assert((State = FLastState) or (State^.TransitionCount >= 1));
      if (not Assigned(State^.FollowTransitions)) then
         AddTransition(State, mtFollow, nil, False);
      if (State^.TransitionCount = 1) then
      begin
         Assert(not Assigned(State^.TokenTransitions));
         Assert(Assigned(State^.FollowTransitions));
         Assert(not Assigned(State^.FollowTransitions^.NextTransition));
      end;
      State := State^.NextState;
   end;
   { Optimise transitions through surrogates }
   State := FFirstState;
   while (Assigned(State)) do
   begin
      OptimiseTransitions(State^.TokenTransitions);
      OptimiseTransitions(State^.FollowTransitions);
      State := State^.NextState;
   end;
   { Establish state IDs }
   Index := 0;
   State := FFirstState;
   Assert(FLastState^.TransitionCount = 1);
   while (Assigned(State)) do
   begin
      { We always serialise the first state even if it could be
        optimised away, because optimising it away would require
        making sure the next state came first.  What we really should
        do is detect this case, and copy all the states from the next
        state to the first state, then fixing all the transitions to
        that state to point to the first state, then removing the
        transitions from that state so it gets optimised away. (We
        would then rinse-repeat, in case the next state was also
        redundant.) }
      if ((State = FFirstState) or (State^.TransitionCount > 1)) then
      begin
         Assert(State <> FLastState);
         Assert(Index >= Low(TByteCode));
         Assert(Index <= High(TByteCode));
         State^.Index := Index;
         Inc(Index, State^.TransitionCount * 2);
         Assert(Index <= High(TByteCode), 'Pattern too complicated.');
      end;
      State := State^.NextState;
   end;
   { Serialise the compiled pattern }
   PatternLength := Index;
   NewPattern(Pattern, PatternLength);
   Index := 0;
   State := FFirstState;
   while (State <> FLastState) do
   begin
      Assert(Assigned(State));
      if ((State = FFirstState { see note above }) or (State^.TransitionCount > 1)) then
      begin { references to states with just one transition get optimised away above }
         Transition := State^.TokenTransitions;
         while (Assigned(Transition)) do
         begin
            Pattern^[Index] := Transition^.Token;
            Inc(Index);
            Assert(Assigned(Transition^.State));
            if (Transition^.State = FLastState) then
               Pattern^[Index] := mpsMatch
            else
            if (Transition^.BlockDuplicates) then
               Pattern^[Index] := Transition^.State^.Index or mpsPreventDuplicatesMask
            else
               Pattern^[Index] := Transition^.State^.Index;
            Inc(Index);
            Transition := Transition^.NextTransition;
         end;
         Transition := State^.FollowTransitions;
         while (Assigned(Transition)) do
         begin
            Assert(Transition^.Token = mtFollow);
            if (Assigned(Transition^.NextTransition)) then
               Pattern^[Index] := mtFollow
            else
               Pattern^[Index] := mtLastFollow;
            Inc(Index);
            Assert(Index < PatternLength);
            if (Transition^.State = FLastState) then
            begin
               Pattern^[Index] := mpsMatch;
            end
            else
            if (Assigned(Transition^.State)) then
            begin
               if (Transition^.BlockDuplicates) then
                  Pattern^[Index] := Transition^.State^.Index or mpsPreventDuplicatesMask
               else
                  Pattern^[Index] := Transition^.State^.Index;
            end
            else
            begin
               Pattern^[Index] := mpsFail;
            end;
            Inc(Index);
            Transition := Transition^.NextTransition;
         end;
      end;
      State := State^.NextState;
   end;
   Assert(Index = PatternLength);
   { Release memory }
   while (Assigned(FFirstState)) do
   begin
      State := FFirstState;
      FFirstState := State^.NextState;
      while (Assigned(State^.TokenTransitions)) do
      begin
         Transition := State^.TokenTransitions;
         State^.TokenTransitions := State^.TokenTransitions^.NextTransition;
         Dispose(Transition);
         Transition := nil;
      end;
      while (Assigned(State^.FollowTransitions)) do
      begin
         Transition := State^.FollowTransitions;
         State^.FollowTransitions := State^.FollowTransitions^.NextTransition;
         Dispose(Transition);
         Transition := nil;
      end;
      {$IFOPT C+}
      if (not Assigned(FFirstState)) then
         Assert(State = FLastState);
      {$ENDIF}
      Dispose(State);
      State := nil;
   end;
   FLastState := nil;
end;


constructor TMatcher.Create(Tokens, OriginalTokens: TTokens; Pattern: PCompiledPattern; PatternLength: TByteCode);
begin
   inherited Create();
   FTokens := Tokens;
   FOriginalTokens := OriginalTokens;
   FPattern := Pattern;
   FPatternLength := PatternLength;
end;

destructor TMatcher.Destroy();
begin
   Assert(Assigned(FPattern));
   DisposePattern(FPattern, FPatternLength);
   inherited;
end;

constructor TMatcher.Read(Stream: TReadStream);
var
   TokenCount, Index: Cardinal;
begin
   inherited;
   FPatternLength := Stream.ReadByte();
   NewPattern(FPattern, FPatternLength);
   Stream.ReadByteStream(FPattern^, FPatternLength);
   TokenCount := Stream.ReadCardinal();
   Assert(TokenCount <= High(TByteCode));
   SetLength(FTokens, TokenCount);
   SetLength(FOriginalTokens, TokenCount);
   for Index := 0 to TokenCount-1 do
   begin
      FTokens[Index] := Stream.ReadAnsiString();
      FOriginalTokens[Index] := Stream.ReadAnsiString();
   end;
end;

procedure TMatcher.Write(Stream: TWriteStream);
var
   Index: Cardinal;
begin
   inherited;
   Stream.WriteByte(FPatternLength);
   Stream.WriteByteStream(FPattern^, FPatternLength);
   Stream.WriteCardinal(Length(FTokens));
   for Index := 0 to Length(FTokens)-1 do
   begin
      Stream.WriteAnsiString(FTokens[Index]);
      Stream.WriteAnsiString(FOriginalTokens[Index]);
   end;
end;

function TMatcher.GetTokenID(Token: AnsiString): TByteCode;
var
   L, R, M: TByteCode;
begin
   Assert(Length(FOriginalTokens) = Length(FTokens));
   Assert(Low(FTokens) >= Low(TByteCode));
   Assert(High(FTokens) <= High(TByteCode));
   Assert(Token = Canonicalise(Token));
   L := Low(FTokens);
   R := High(FTokens);
   repeat
      M := (R-L) div 2 + L;
      if (FTokens[M] < Token) then
         L := M+1
      else
         R := M;
   until (L >= R);
   if (FTokens[R] = Token) then
      Result := R
   else
      Result := mtNone;
end;

function TMatcher.Matches(Tokens: TTokens; Start: Cardinal): Cardinal;
type
   PSharedCompiledPattern = ^TSharedCompiledPattern;
   TSharedCompiledPattern = record
      Data: PCompiledPattern;
      RefCount: Cardinal;
   end;

   function CreateSharedCompiledPattern(Pattern: PCompiledPattern): PSharedCompiledPattern; inline;
   begin
      Assert(Assigned(Pattern));
      New(Result);
      NewPattern(Result^.Data, FPatternLength);
      Move(Pattern^, Result^.Data^, FPatternLength * SizeOf(TByteCode));
      Result^.RefCount := 1;
   end;

   function CloneSharedCompiledPattern(Source: PSharedCompiledPattern): PSharedCompiledPattern; inline;
   begin
      Inc(Source^.RefCount);
      Result := Source;
   end;

   procedure ObliterateTransition(var Source: PSharedCompiledPattern; Index: TByteCode); inline;
   begin
      Assert(Source^.RefCount >= 1);
      if (Source^.RefCount > 1) then
      begin
         Dec(Source^.RefCount);
         Source := CreateSharedCompiledPattern(Source^.Data);
      end;
      Source^.Data^[Index] := mpsFail;
   end;

   procedure DisposeSharedCompiledPattern(var Source: PSharedCompiledPattern); inline;
   begin
      Assert(Source^.RefCount >= 1);
      if (Source^.RefCount > 1) then
      begin
         Dec(Source^.RefCount);
      end
      else
      begin
         DisposePattern(Source^.Data, FPatternLength);
         Dispose(Source);
         Source := nil;
      end;
   end;

type
   PStateMachine = ^TStateMachine;
   TStateMachine = record
     State: TByteCode;
     Pattern: PSharedCompiledPattern;
     Next: PStateMachine;
     Previous: PStateMachine;
   end;

   function AppendStateMachine(Parent: PStateMachine): PStateMachine; inline;
   begin
      New(Result);
      Result^.State := Parent^.State;
      Result^.Pattern := CloneSharedCompiledPattern(Parent^.Pattern);
      Result^.Next := Parent^.Next;
      if (Assigned(Result^.Next)) then
         Result^.Next^.Previous := Result;
      Result^.Previous := Parent;
      Parent^.Next := Result;
   end;

   procedure KillStateMachine(var Victim: PStateMachine); inline;
   begin
      Assert(Assigned(Victim));
      if (Assigned(Victim^.Next)) then
         Victim^.Next^.Previous := Victim^.Previous;
      if (Assigned(Victim^.Previous)) then
         Victim^.Previous^.Next := Victim^.Next;
      DisposeSharedCompiledPattern(Victim^.Pattern);
      Dispose(Victim);
      Victim := nil;
   end;

var
   StateMachines: PStateMachine;
   Position, MatchLength, CurrentTransition: Cardinal;
   CurrentStateMachine, NextStateMachine, ChildStateMachine: PStateMachine;
   NextState, TokenID: TByteCode;
   Transitioned: Boolean;
begin
   New(StateMachines);
   StateMachines^.State := 0;
   StateMachines^.Pattern := CreateSharedCompiledPattern(FPattern);
   StateMachines^.Next := nil;
   StateMachines^.Previous := nil;
   AppendStateMachine(StateMachines);
   MatchLength := 0;
   Position := Start;
{
Writeln('=MATCHES=');
Writeln('FTokens = ', Serialise(FTokens, 0, Length(FTokens)));
Writeln('FOriginalTokens = ', Serialise(FOriginalTokens, 0, Length(FOriginalTokens)));
Writeln('Tokens = ', Serialise(Tokens, 0, Length(Tokens)));
}
   while (Assigned(StateMachines^.Next)) do
   begin
      { Follow mtFollow links first }
      CurrentStateMachine := StateMachines^.Next;
      while (Assigned(CurrentStateMachine)) do
      begin
         CurrentTransition := CurrentStateMachine^.State;
         repeat
            Assert(CurrentTransition < High(CurrentTransition));
            if (CurrentStateMachine^.Pattern^.Data^[CurrentTransition] and mtFollowMask = mtFollowMask) then
            begin
               NextState := CurrentStateMachine^.Pattern^.Data^[CurrentTransition+1];
               if (NextState <= mpsMaxTrueTransition) then
               begin
                  ChildStateMachine := AppendStateMachine(CurrentStateMachine);
                  ChildStateMachine^.State := NextState and not mpsPreventDuplicatesMask;
                  if (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
                     ObliterateTransition(ChildStateMachine^.Pattern, CurrentTransition+1);
               end
               else
               if (NextState = mpsMatch) then
               begin
                  Assert(Position - Start <= High(TByteCode));
                  Assert(Position - Start >= Low(TByteCode));
                  MatchLength := Position - Start;
               end;
            end;
            Inc(CurrentTransition, 2);
         until (CurrentStateMachine^.Pattern^.Data^[CurrentTransition-2] = mtLastFollow);
         CurrentStateMachine := CurrentStateMachine^.Next;
      end;
      { Now follow the links that match the token, removing any state machines that don't have a match }
//Writeln('checking for ', Tokens[Position]);
      if (Position > High(Tokens)) then
         TokenID := mtNone
      else
         TokenID := GetTokenID(Tokens[Position]);
//Writeln('Result: ', TokenID);
      CurrentStateMachine := StateMachines^.Next;
      while (Assigned(CurrentStateMachine)) do
      begin
         NextStateMachine := CurrentStateMachine^.Next;
         if (TokenID = mtNone) then
         begin
            KillStateMachine(CurrentStateMachine);
         end
         else
         begin
            CurrentTransition := CurrentStateMachine^.State;
            Transitioned := False;
            repeat
               if (CurrentStateMachine^.Pattern^.Data^[CurrentTransition] = TokenID) then
               begin
                  NextState := CurrentStateMachine^.Pattern^.Data^[CurrentTransition+1];
                  if (NextState = mpsMatch) then
                  begin
                     Assert(Position - Start <= High(TByteCode));
                     Assert(Position - Start >= Low(TByteCode));
                     MatchLength := Position - Start + 1;
                  end
                  else
                  begin
                     Assert(NextState <= mpsMaxTrueTransition);
                     CurrentStateMachine^.State := NextState and not mpsPreventDuplicatesMask;
                     if (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
                        ObliterateTransition(CurrentStateMachine^.Pattern, CurrentTransition+1);
                     Transitioned := True;
                  end;
               end;
               if (not Transitioned) then
                  Inc(CurrentTransition, 2);
            until ((Transitioned) or (CurrentStateMachine^.Pattern^.Data^[CurrentTransition-2] = mtLastFollow));
            if (not Transitioned) then
            begin
               KillStateMachine(CurrentStateMachine);
            end;
         end;
         CurrentStateMachine := NextStateMachine;
      end;
      Inc(Position);
   end;
   KillStateMachine(StateMachines);
   Result := MatchLength;
end;

function TMatcher.GetLongestMatch(Separator: AnsiString): AnsiString;
var
   Pattern: PCompiledPattern;

//{$DEFINE DEBUG_LONGEST_MATCH}

   function GetLongestBranch(State: TByteCode; out Match: AnsiString; out MatchLength: Cardinal {$IFDEF DEBUG_LONGEST_MATCH}; Prefix: AnsiString = ''{$ENDIF}): Boolean;
   var
      CurrentMatch, TryMatch: AnsiString;
      TryLength: Cardinal;
      TryResult: Boolean;
      CurrentIndex, NextState: TByteCode;
{$IFDEF DEBUG_LONGEST_MATCH} S: AnsiString; TestState: TByteCode; {$ENDIF}
   begin
{$IFDEF DEBUG_LONGEST_MATCH} Prefix := Prefix + '   '; {$ENDIF}
{$IFDEF DEBUG_LONGEST_MATCH} Writeln(Prefix, 'GetLongestBranch(', State, '): enter'); {$ENDIF}
      Result := False;
      Match := '';
      MatchLength := 0;
      CurrentIndex := State;
      repeat
         NextState := Pattern^[CurrentIndex+1];
{$IFDEF DEBUG_LONGEST_MATCH}
 if (NextState = mpsFail) then
    S := 'FAIL'
 else
 if (NextState = mpsMatch) then
    S := 'MATCH'
 else
    S := IntToStr(NextState and not mpsPreventDuplicatesMask); 
 if (Pattern^[CurrentIndex] <= mtMaxTrueToken) then
    Writeln(Prefix, 'GetLongestBranch(', State, '): looking at transition ', CurrentIndex, ' which goes to ', S, ' if it matches ', FOriginalTokens[Pattern^[CurrentIndex]])
 else
    Writeln(Prefix, 'GetLongestBranch(', State, '): looking at transition ', CurrentIndex, ' which goes to ', S, ' always');
{$ENDIF}
         if (NextState <> mpsFail) then
         begin
            if (Pattern^[CurrentIndex] <= mtMaxTrueToken) then
               CurrentMatch := FOriginalTokens[Pattern^[CurrentIndex]]
            else
               CurrentMatch := '';
            if (NextState = mpsMatch) then
            begin
               TryResult := True;
               TryMatch := CurrentMatch;
               if (CurrentMatch <> '') then
                  TryLength := 1
               else
                  TryLength := 0;
            end
            else
            begin
{$IFDEF DEBUG_LONGEST_MATCH} Writeln(Prefix, 'GetLongestBranch(', State, '): blocking off transition at ', CurrentIndex); {$ENDIF}
               if ((Pattern^[CurrentIndex] <= mtMaxTrueToken) or (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask)) then
                   Pattern^[CurrentIndex+1] := mpsFail;
{$IFDEF DEBUG_LONGEST_MATCH}
 TestState := Pattern^[CurrentIndex+1];
 if (TestState = mpsFail) then
    S := 'FAIL'
 else
 if (TestState = mpsMatch) then
    S := 'MATCH'
 else
    S := IntToStr(TestState and not mpsPreventDuplicatesMask); 
 if (Pattern^[CurrentIndex] <= mtMaxTrueToken) then
    Writeln(Prefix, 'GetLongestBranch(', State, '): transition ', CurrentIndex, ' now goes to ', S, ' if it matches ', FOriginalTokens[Pattern^[CurrentIndex]])
 else
    Writeln(Prefix, 'GetLongestBranch(', State, '): transition ', CurrentIndex, ' now goes to ', S, ' always');
{$ENDIF}
               if (GetLongestBranch(NextState and not mpsPreventDuplicatesMask, TryMatch, TryLength {$IFDEF DEBUG_LONGEST_MATCH}, Prefix {$ENDIF})) then
               begin
                  TryResult := True;
                  if (CurrentMatch <> '') then
                  begin
                     if (TryMatch <> '') then
                        TryMatch := CurrentMatch + Separator + TryMatch
                     else
                        TryMatch := CurrentMatch;
                     Inc(TryLength);
                  end
               end
               else
                  TryResult := False;
               Pattern^[CurrentIndex+1] := NextState;
{$IFDEF DEBUG_LONGEST_MATCH} Writeln(Prefix, 'GetLongestBranch(', State, '): unblocking transition at ', CurrentIndex); {$ENDIF}
            end;
            if ((TryResult) and (TryLength >= MatchLength)) then { use >= because patterns have the tokens in reverse canonical order }
            begin
               Result := True;
               Match := TryMatch;
               MatchLength := TryLength;
{$IFDEF DEBUG_LONGEST_MATCH} Writeln(Prefix, 'GetLongestBranch(', State, '): got a better candidate: match = "', Match, '", match length = ', MatchLength, ', result = ', Result); {$ENDIF}
            end;
         end;
         Inc(CurrentIndex, 2);
      until Pattern^[CurrentIndex-2] = mtLastFollow;
{$IFDEF DEBUG_LONGEST_MATCH} Writeln(Prefix, 'GetLongestBranch(', State, '): exit with match = "', Match, '", match length = ', MatchLength, ', result = ', Result); {$ENDIF}
   end;

var
   Length: Cardinal;
begin
   NewPattern(Pattern, FPatternLength);
   Move(FPattern^, Pattern^, FPatternLength * SizeOf(TByteCode));
   {$IFOPT C+} Assert( {$ENDIF} GetLongestBranch(0, Result, Length) {$IFOPT C+} ) {$ENDIF} ;
   Assert(Length > 0);
   Assert(Result <> '');
   DisposePattern(Pattern, FPatternLength);
end;

{$IFDEF DEBUG}
function TMatcher.GetPatternDescription(): AnsiString;
var
   Index: TByteCode;
begin
   Result := 'PATTERN' + #10 + 'Token count: ' + IntToStr(Length(FOriginalTokens)) + #10 + 'Pattern length: ' + IntToStr(FPatternLength) + '/' + IntToStr(High(TByteCode)) + #10 + 'States:' + #10;
   Index := 0;
   while (Index < FPatternLength) do
   begin
      Result := Result + ' ' + IntToStr(Index) + ':' + #10;
      repeat
         Result := Result + '   ';
         if (FPattern^[Index] and mtFollowMask = mtFollowMask) then
            Result := Result + 'ELSE'
         else
            Result := Result + IntToStr(FPattern^[Index]) + ':"' + FOriginalTokens[FPattern^[Index]] + '"';
         Result := Result + ' -> ';
         case (FPattern^[Index+1]) of
           mpsMatch: Result := Result + 'MATCH';
           mpsFail: Result := Result + 'FAIL';
         else
           Result := Result + IntToStr(FPattern^[Index+1] and not mpsPreventDuplicatesMask);
           if (FPattern^[Index+1] and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
              Result := Result + ' (duplicates blocked)';
         end;
         Result := Result + #10;
         Inc(Index, 2);
      until FPattern^[Index-2] = mtLastFollow;
   end;
end;
{$ENDIF}

{$IFDEF DEBUG}
function TMatcher.GetPatternDotFileRecords(): AnsiString;
var
   Index, State: TByteCode;
   Edges: AnsiString;
begin
   Result := 'digraph pattern { graph [ rankdir = "LR" ];' + #10;
   Edges := '';
   Index := 0;
   while (Index < FPatternLength) do
   begin
      Result := Result + '"state' + IntToStr(Index) + '" [ label = "<state> ' + IntToStr(Index);
      State := Index;
      repeat
         if (FPattern^[Index] and mtFollowMask = mtFollowMask) then
            Result := Result + '|<i' + IntToStr(Index) + '> *'
         else
            Result := Result + '|<i' + IntToStr(Index) + '> ' + FOriginalTokens[FPattern^[Index]];
         Edges := Edges + '"state' + IntToStr(State) + '":i' + IntToStr(Index) + ' -> ';
         case (FPattern^[Index+1]) of
           mpsMatch: Edges := Edges + '"match"';
           mpsFail: Edges := Edges + '"fail"';
         else
           Edges := Edges + '"state' + IntToStr(FPattern^[Index+1] and not mpsPreventDuplicatesMask) + '":state';
           if (FPattern^[Index+1] and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
             Edges := Edges + ' [ color = "darkgreen" ]';
         end;
         Edges := Edges + ';' + #10;
         Inc(Index, 2);
      until FPattern^[Index-2] = mtLastFollow;
      Result := Result + '" shape = "record" ];' + #10;
   end;
   Result := Result + '"match" [ label = "Match" shape = "ellipse" ];' + #10;
   Result := Result + '"fail" [ label = "Fail" shape = "ellipse" ];' + #10;
   Result := Result + Edges + '}';
end;

{ using labels }
function TMatcher.GetPatternDotFileLabels(): AnsiString;
var
   Index, State: TByteCode;
   S: AnsiString;
   NeedLoop: Boolean;
begin
   Result := 'digraph pattern { graph [ rankdir="LR" ];' + #10;
   Index := 0;
   while (Index < FPatternLength) do
   begin
      if (Index = 0) then
         S := 'circle'
      else
         S := 'rect';
      State := Index;
      NeedLoop := False;
      repeat
         Result := Result + '"' + IntToStr(State) + '" -> ';
         S := 'red';
         case (FPattern^[Index+1]) of
           mpsMatch: begin Result := Result + '"match" [ arrowhead="diamond" fontcolor="navy" '; S := 'blue'; end;
           mpsFail: begin Result := Result + '"fail" [ arrowhead="diamond" fontcolor="grey91" '; S := 'grey91'; end;
         else
           if ((FPattern^[Index+1] and not mpsPreventDuplicatesMask) = State) then
           begin
              Result := Result + '"' + IntToStr(State) + 'p" [ ';
              NeedLoop := True;
           end
           else
              Result := Result + '"' + IntToStr(FPattern^[Index+1] and not mpsPreventDuplicatesMask) + '" [ ';
           if (FPattern^[Index+1] and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
           begin
              Result := Result + 'arrowhead="diamond" fontcolor="navy" ';
              S := 'blue';
           end
           else
           begin
              S := 'black';
           end;
         end;
         if (FPattern^[Index] and mtFollowMask = mtFollowMask) then
            Result := Result + 'color="' + S + ':' + S + '" '
         else
            Result := Result + 'color="' + S + '" label="' + FOriginalTokens[FPattern^[Index]] + '" fontsize=10 samehead="' + IntToStr(State) + '" ';
         Result := Result + '];' + #10;
         Inc(Index, 2);
      until FPattern^[Index-2] = mtLastFollow;
      if (NeedLoop) then
      begin
         Result := Result + '"' + IntToStr(State) + 'p" [ label="' + IntToStr(State) + '''" shape="diamond" ];' + #10;
         Result := Result + '"' + IntToStr(State) + 'p" -> "' + IntToStr(State) + '";' + #10;
      end;
   end;
   Result := Result + '"match" [ label="Match" shape="ellipse" ];' + #10;
   Result := Result + '"fail" [ label="Fail" shape="ellipse" color="grey91" fontcolor="grey91" ];' + #10;
   Result := Result + '}';
end;
{$ENDIF}

function CompilePatternVersion(S: AnsiString; Version: Cardinal): TMatcher;

   function Parse(var Index: Cardinal): TPatternNode;
   type
      TParseMode = (pmToken, pmEscape, pmListType);
   var
      Token: AnsiString;
      CurrentVersion: Cardinal;
      Mode: TParseMode;
      List: array of TPatternNode;

      procedure Push(Node: TPatternNode);
      begin
         if (CurrentVersion <= Version) then
         begin
            SetLength(List, Length(List)+1);
            List[High(List)] := Node;
         end
         else
         begin
            Node.Free();
         end;
      end;

   begin
//Writeln('>>ENTER LIST');
      Token := '';
      CurrentVersion := 0;
      Mode := pmToken;
      Assert(Index >= Low(S));
      while (Index <= Length(S)) do
      begin
//Writeln('parsing character: ', S[Index], ' at index ', Index, ' in mode ', Cardinal(Mode));
         case Mode of
          pmToken:
            case S[Index] of
             ' ':
               begin
                  if (Token <> '') then
                  begin
                     Push(TTokenNode.Create(Token));
                     Token := '';
                  end;
                  CurrentVersion := 0;
               end;
             '+':
               begin
                  Assert(Length(Token) > 0);
                  Push(TRepeatableTokenNode.Create(Token));
                  Token := '';
               end;
             '?':
               begin
                  Assert(Length(Token) > 0);
                  Push(TZeroOrMoreOrderedPatternNode.Create([TTokenNode.Create(Token)]));
                  Token := '';
               end;
             '/':
               begin
                  if (Token <> '') then
                  begin
                     Push(TTokenNode.Create(Token));
                     Token := '';
                  end;
                  Assert(Length(List) > 0);
                  if (CurrentVersion < Version) then
                  begin
                     List[High(List)].Free();
                     SetLength(List, Length(List)-1);
                  end;
                  Inc(CurrentVersion);
               end;
             '(':
               begin
                  Assert(Token = '');
//Writeln('OPEN INC');
                  Inc(Index);
                  Push(Parse(Index));
               end;
             ')':
               begin
                  if (Token <> '') then
                  begin
                     Push(TTokenNode.Create(Token));
                     Token := '';
                  end;
//Writeln('LIST END; SWITCHING TO TYPE CHECK');
                  Mode := pmListType;
               end;
             '\': Mode := pmEscape;
             '@', '*', '#', '%', '&': EAssertionFailed.Create('List suffix used inappropriately.');
             else Token := Token + S[Index];
            end;
          pmEscape:
            begin
               Token := Token + S[Index];
               Mode := pmToken;
            end;
          pmListType:
            begin
               Assert(Length(List) > 0);
               case S[Index] of
                '+': Result := TRepeatableSequencePatternNode.Create(List);
                '?': Result := TOptionalSequencePatternNode.Create(List);
                '@': Result := TAlternativesPatternNode.Create(List);
                '*': Result := TZeroOrMoreUnorderedPatternNode.Create(List);
                '#': Result := TOneOrMoreUnorderedPatternNode.Create(List);
                '%': Result := TZeroOrMoreOrderedPatternNode.Create(List);
                '&': Result := TOneOrMoreOrderedPatternNode.Create(List);
                else
                  begin
                     if (Length(List) > 1) then
                        Result := TSequencePatternNode.Create(List)
                     else
                        Result := List[0];
                     Dec(Index);
//Writeln('NO-TYPE DEC, REDOING AT HIGHER LEVEL');
                  end;
               end;
//Writeln('<<EXIT LIST');
               Exit;
            end;
          else
            raise EAssertionFailed.Create('unknown parse mode');
         end;
//Writeln('END INC');
         Inc(Index);
      end;
      if (Mode = pmListType) then
      begin
         Dec(Index);
//Writeln('NO-TYPE DEC EMERGENCY, REDOING AT HIGHER LEVEL');
      end
      else
      if (Token <> '') then
      begin
         Push(TTokenNode.Create(Token));
         Token := '';
      end;
      Assert(Token = '');
      Assert(Length(List) > 0);
      if (Length(List) > 1) then
         Result := TSequencePatternNode.Create(List)
      else
         Result := List[0];
//Writeln('=END=');
   end;

   function Parse(): TPatternNode;
   var
      Index: Cardinal;
   begin
      Index := 1;
      Result := Parse(Index);
      {$IFOPT C+}
      if (Index <= Length(S)) then
         raise EAssertionFailed.Create('Trailing garbage at index ' + IntToStr(Index) + ' of ' + IntToStr(Length(S)) + ' character pattern "' + S + '"');
      Assert(Index = Length(S)+1, 'Parsed to position ' + IntToStr(Index) + ', beyond end of ' + IntToStr(Length(S)) + ' character string "' + S + '"');
      {$ENDIF}
   end;

var
   Compiler: TPatternCompiler;
   PatternTreeRoot: TPatternNode;
   Tokens, OriginalTokens: TTokens;
   CompiledPattern: PCompiledPattern;
   PatternLength: TByteCode;
begin
   PatternTreeRoot := Parse();
   Compiler := TPatternCompiler.Create(PatternTreeRoot);
   Tokens := Compiler.GetAtomisedTokens();
   OriginalTokens := Compiler.GetAtomisedOriginalTokens();
   Compiler.GetCompiledPattern(CompiledPattern, PatternLength);
   Compiler.Free();
   Result := TMatcher.Create(Tokens, OriginalTokens, CompiledPattern, PatternLength);
end;

procedure CompilePattern(S: AnsiString; out Singular: TMatcher; out Plural: TMatcher);
begin
   Singular := CompilePatternVersion(S, 0);
   Plural := CompilePatternVersion(S, 1);
end;

{$IFDEF DEBUG}
function HasPatternChars(S: AnsiString): Boolean;
begin
   Result := (Pos(S, '+') > 0) or
             (Pos(S, '?') > 0) or
             (Pos(S, '/') > 0) or
             (Pos(S, '(') > 0) or
             (Pos(S, ')') > 0) or
             (Pos(S, '\') > 0) or
             (Pos(S, '@') > 0) or
             (Pos(S, '*') > 0) or
             (Pos(S, '#') > 0) or
             (Pos(S, '%') > 0) or
             (Pos(S, '&') > 0);
end;
{$ENDIF}

initialization
   RegisterStorableClass(TMatcher, 20);
end.
