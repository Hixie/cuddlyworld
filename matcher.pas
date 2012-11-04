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
      function GetCanonicalMatch(Separator: AnsiString): AnsiString;
      {$IFDEF DEBUG} function GetPatternDescription(): AnsiString; {$ENDIF}
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
function HasSingularVsPluralAnnotation(S: AnsiString): Boolean;
{$ENDIF}

implementation

uses
   {$IFDEF DEBUG} debug, {$ENDIF}
   sysutils;

 { A compiled pattern consists of a zero byte, a series of byte pairs,
   and a $FF byte. Each sequence of byte pairs is a state for a
   pattern-matching state machine and is identified by the position of
   the first byte in that sequence in the overall sequence. The first
   state in the pattern is the start state for running the
   pattern. Each byte pair represents a state transition for the state
   and consists of a byte representing the token to match to follow
   the transition, and a byte representing the state to switch to if
   that token is matched. The special token ID $FE is always
   matched. There is also a special state ID, $FF, meaning the pattern
   matched. For other state IDs, the least significant bit of the
   second byte of each transition is set if the state machine must
   avoid transitioning through this state twice while matching the
   pattern.

   Notes:

    - The first byte of each state is currently unused (it's only
      helpful in aligning the states to even byte boundaries). It
      provides eight bits of per-state magic for future expansion.

    - Since the whole pattern has to fit in 255 bytes and each
      transition takes at least 2 bytes; the most tokens that could
      possibly be used in a pattern is therefore 127. Token $FF
      indicates the end of the state. Token $FE indicates is always
      matched. This leaves tokens $80 to FD for other magic purposes.

    - States always start on an even numbered byte, since each state
      is an even number of bytes long; so the least-significant bit of
      each transition byte can be used for magic. It's used for
      indicating that the transition can't be repeated.

    - No state can start on the last three bytes, so state IDs $FF,
      $FE, and $FD can be used for magic. Currently only $FF is used
      in the pattern, to indicate a successful match. $FE is used in
      the state machine to indicate blocked paths (one of the magic
      tokens could be used easily too, if necessary). $FD is used for
      debugging purposes.

   For example,
      00 00 04 FF 00 01 FF FF
   ...would match the token sequence 00 01, but nothing else.

   Similarly,
      00 00 06 FE 06 FF 00 01 FF FF
   ...would match either the token 01 on its own, or the token
   sequence 00 01.

   Transitions are listed in the states of a pattern in canonical
   order, and to get the canonical value should be walked depth first
   until you get a match, avoiding following any token-specific
   transition more than once.

   For example:
     00 00 07 01 07 02 07 FF 00 FE 00 FE FF FF
   ...would match the following sequences:
     00, 01, 02, 00 01, 00 02, 01 00, 01 02, 02 00, 02 01, 00 01 02,
     00 02 01, 01 00 02, 01 02 00, 02 00 01, 02 01 00
   ...but the canonical sequence that represents this pattern would
   be:
     00 01 02

   }

const
   { Magic State Flags }
   msfNormal = $00;
   { Magic Tokens }
   mtMaxTrueToken = $7F;
   mtNone = $80;
   mtFollow = $FE;
   mtStateEnd = $FF;
   { Magic Pattern States }
   mpsMatch = $FF;
   mpsBlocked = $FE;
   {$IFOPT C+} mpsNotSerialised = $FD; {$ENDIF}
   mpsMaxTrueTransition = $FC;
   mpsPreventDuplicatesMask = $01;

type
   PState = ^TState;
   PTransition = ^TTransition;

   TState = record
     Transitions: PTransition;
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
         while (P > MasterList[I]) do
            I := I + 1;
         while (P < MasterList[J]) do
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
      until (I > J);
      if (L < J) then
         DualQuickSort(MasterList, SlaveList, L, J);
      L := I;
   until (I >= R);
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
   Assert(Assigned(TargetState));
   New(Transition);
   Transition^.Token := Token;
   Transition^.State := TargetState;
   Transition^.BlockDuplicates := BlockDuplicates;
   Transition^.NextTransition := State^.Transitions;
   State^.Transitions := Transition;
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


//{$DEFINE DEBUG_PATTERN_COMPILER}

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
   Assert(High(FTokens) <= mtMaxTrueToken, 'Too many unique strings in pattern');
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
   Result^.Transitions := nil;
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
   Assert(Low(FTokens) >= 0);
   Assert(High(FTokens) <= mtMaxTrueToken);
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
      while ((Result^.TransitionCount = 1) and (Result^.Transitions^.Token = mtFollow)) do
      begin
         Assert(Assigned(Result));
         Assert(Result <> FLastState);
         Assert(not Assigned(Result^.Transitions^.NextTransition));
         Assert(Assigned(Result^.Transitions^.State));
         Result := Result^.Transitions^.State;
      end;
   end;

   function StateNeedsSerialising(State: PState): Boolean; inline;
   begin
      Assert(Assigned(State));
      Result := (State = FFirstState { see note above }) or (State^.TransitionCount > 1) or (State^.Transitions^.Token <> mtFollow);
   end;

var
   State: PState;
   Index: Cardinal;
   Transition: PTransition;
begin
   { Build state machine }
   New(FFirstState);
   FFirstState^.Transitions := nil;
   FFirstState^.TransitionCount := 0;
   FFirstState^.Index := 0;
   FFirstState^.PreviousState := nil;
   New(FLastState);
   FLastState^.Transitions := nil;
   FLastState^.TransitionCount := 0;
   FLastState^.Index := 0;
   FLastState^.NextState := nil;
   FFirstState^.NextState := FLastState;
   FLastState^.PreviousState := FFirstState;
   FRoot.HookStates(FFirstState, FLastState, @GetNewState);

{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('FFirstState=', HexStr(FFirstState)); {$ENDIF}
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('FLastState=', HexStr(FLastState)); {$ENDIF}

   { Optimise transitions through surrogates }
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('GetCompiledPattern(): optimising...'); {$ENDIF}
   State := FFirstState;
   repeat
      Assert(Assigned(State));
      Assert(State^.TransitionCount >= 1);

{$IFDEF DEBUG_PATTERN_COMPILER}
  Writeln('  Looking at state $', HexStr(State), '...');
  Writeln('  True or false, top dentists agree, it needs serialising: ', StateNeedsSerialising(State));
  Writeln('  It has these transitions:');
  Transition := State^.Transitions;
  while (Assigned(Transition)) do
  begin
     if (Transition^.State = FLastState) then
        Writeln('    * Token: ', Transition^.Token, ' to MATCH')
     else
        Writeln('    * Token: ', Transition^.Token, ' to state $', HexStr(Transition^.State), ' with dentists ', StateNeedsSerialising(Transition^.State));
     Transition := Transition^.NextTransition;
  end;  
  Writeln('  --');
{$ENDIF}

      Transition := State^.Transitions;
      repeat
         Assert(Assigned(Transition));
         Transition^.State := GetUltimateTarget(Transition^.State);
         Assert((Transition^.State = FLastState) or (StateNeedsSerialising(Transition^.State)));
         Transition := Transition^.NextTransition;
      until (not Assigned(Transition));

{$IFDEF DEBUG_PATTERN_COMPILER}
  Writeln('  Looking again at state $', HexStr(State), ' after optimising.');
  Writeln('  True or false, top dentists agree, it needs serialising: ', StateNeedsSerialising(State));
  Writeln('  It now has these transitions:');
  Transition := State^.Transitions;
  while (Assigned(Transition)) do
  begin
     if (Transition^.State = FLastState) then
        Writeln('    * Token: ', Transition^.Token, ' to MATCH')
     else
        Writeln('    * Token: ', Transition^.Token, ' to state $', HexStr(Transition^.State), ' with dentists ', StateNeedsSerialising(Transition^.State));
     Transition := Transition^.NextTransition;
  end;  
  Writeln('  --');
{$ENDIF}

      State := State^.NextState;
   until (State = FLastState);
   { Future optimisation idea: We currently always serialise the first
     state even if it could be optimised away, because optimising it
     away would require making sure the next state came first.  What
     we really should do is detect this case, and copy all the states
     from the next state to the first state, then fixing all the
     transitions to that state to point to the first state, then
     removing the transitions from that state so it gets optimised
     away. (We would then rinse-repeat, in case the next state was
     also redundant.) }

{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('GetCompiledPattern(): Minting state IDs...'); {$ENDIF}
   { Establish state IDs }
   Index := 0;
   State := FFirstState;
   Assert(FLastState^.TransitionCount = 0);
   repeat
      Assert(Assigned(State));
      Assert(State <> FLastState);
      Assert(State^.TransitionCount >= 1);
      Assert(Index >= 0);
      Assert(Index <= mpsMaxTrueTransition);
      Assert(Index mod 2 = 0);

{$IFDEF DEBUG_PATTERN_COMPILER}
  Writeln('  Looking at state $', HexStr(State), '...');
  Writeln('  True or false, top dentists agree, it needs serialising: ', StateNeedsSerialising(State));
  Writeln('  It has these transitions:');
  Transition := State^.Transitions;
  while (Assigned(Transition)) do
  begin
     if (Transition^.State = FLastState) then
        Writeln('    * Token: ', Transition^.Token, ' to MATCH')
     else
        Writeln('    * Token: ', Transition^.Token, ' to state $', HexStr(Transition^.State), ' with dentists ', StateNeedsSerialising(Transition^.State));
     Transition := Transition^.NextTransition;
  end;  
  Writeln('  --');
{$ENDIF}

      if (StateNeedsSerialising(State)) then
      begin
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('  minting state ', Index); {$ENDIF}
         State^.Index := Index;
         Inc(Index, 2 + State^.TransitionCount * 2);
         Assert(Index <= mpsMaxTrueTransition, 'Pattern too complicated.');
      end {$IFOPT C+} else State^.Index := mpsNotSerialised {$ENDIF};
      State := State^.NextState;
   until (State = FLastState);
   FLastState^.Index := mpsMatch;
   { Serialise the compiled pattern }
   PatternLength := Index;
   NewPattern(Pattern, PatternLength);
   Index := 0;
   State := FFirstState;
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('GetCompiledPattern(): Serialising... PatternLength=', PatternLength); {$ENDIF}
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Index=', Index, ' at start'); {$ENDIF}
   repeat
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Index=', Index, ' at top of state loop'); {$ENDIF}
{$IFDEF DEBUG_PATTERN_COMPILER}
  Writeln('  Looking at state $', HexStr(State), ' marked as index ', State^.Index, '.');
  Writeln('  True or false, top dentists agree, it needs serialising: ', StateNeedsSerialising(State));
  Writeln('  It has these transitions:');
  Transition := State^.Transitions;
  while (Assigned(Transition)) do
  begin
     if (Transition^.State = FLastState) then
        Writeln('    * Token: ', Transition^.Token, ' to MATCH')
     else
        Writeln('    * Token: ', Transition^.Token, ' to state $', HexStr(Transition^.State), ' marked as index ', Transition^.State^.Index, ' with dentists ', StateNeedsSerialising(Transition^.State));
     Transition := Transition^.NextTransition;
  end;  
  Writeln('  --');
{$ENDIF}
      Assert(Assigned(State));
      Assert(State <> FLastState);
      Assert(State^.TransitionCount >= 1);
      Assert(Assigned(State^.Transitions));
      if (StateNeedsSerialising(State)) then
      begin
         Pattern^[Index] := msfNormal;
         Inc(Index);
         Transition := State^.Transitions;
         Inc(Index, (State^.TransitionCount) * 2 + 1);
         repeat
            Assert(Assigned(Transition));
            Assert((Transition^.State = FLastState) or (StateNeedsSerialising(Transition^.State)));
            Dec(Index, 3);
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Index=', Index, ' at top of transition loop'); {$ENDIF}
            Assert(Assigned(Transition));
            Pattern^[Index] := Transition^.Token;
            Inc(Index);
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Index=', Index, ' for target state'); {$ENDIF}
            Assert(Assigned(Transition^.State));
            if (Transition^.BlockDuplicates) then
               Pattern^[Index] := Transition^.State^.Index or mpsPreventDuplicatesMask
            else
               Pattern^[Index] := Transition^.State^.Index;
            Transition := Transition^.NextTransition;
         until (not Assigned(Transition));
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Index=', Index, ' after transition loop'); {$ENDIF}
         Inc(Index, (State^.TransitionCount-1) * 2 + 1);
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Index=', Index, ' for state trailer'); {$ENDIF}
         Pattern^[Index] := mtStateEnd;
         Inc(Index);
      end {$IFDEF DEBUG_PATTERN_COMPILER} else Writeln('skipped a state! wee!') {$ENDIF} ;
      State := State^.NextState;
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Index=', Index, ' at end of state'); {$ENDIF}
   until (State = FLastState);
   Assert(Index = PatternLength);
   { Release memory }
   while (Assigned(FFirstState)) do
   begin
      State := FFirstState;
      FFirstState := State^.NextState;
      while (Assigned(State^.Transitions)) do
      begin
         Transition := State^.Transitions;
         State^.Transitions := State^.Transitions^.NextTransition;
         Dispose(Transition);
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
      Source^.Data^[Index] := mpsBlocked;
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
         Assert(CurrentStateMachine^.Pattern^.Data^[CurrentStateMachine^.State] = msfNormal); { we could have other state flags some day }
         CurrentTransition := CurrentStateMachine^.State + 1;
         repeat
            Assert(CurrentTransition < High(CurrentTransition));
            if (CurrentStateMachine^.Pattern^.Data^[CurrentTransition] = mtFollow) then
            begin
               NextState := CurrentStateMachine^.Pattern^.Data^[CurrentTransition+1];
               if (NextState = mpsMatch) then
               begin
                  MatchLength := Position - Start;
               end
               else
               {$IFOPT C+}
               if (NextState <= mpsMaxTrueTransition) then
               {$ENDIF}
               begin
                  ChildStateMachine := AppendStateMachine(CurrentStateMachine);
                  ChildStateMachine^.State := NextState and not mpsPreventDuplicatesMask;
                  if (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
                     ObliterateTransition(ChildStateMachine^.Pattern, CurrentTransition+1);
               end
               {$IFOPT C+}
               else
               begin
                  Assert(NextState = mpsBlocked);
               end;
               {$ENDIF}
            end;
            Inc(CurrentTransition, 2);
         until (CurrentStateMachine^.Pattern^.Data^[CurrentTransition] = mtStateEnd);
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
         Assert(CurrentStateMachine^.Pattern^.Data^[CurrentStateMachine^.State] = msfNormal); { we could have other state flags some day }
         if (TokenID = mtNone) then
         begin
            KillStateMachine(CurrentStateMachine);
         end
         else
         begin
            CurrentTransition := CurrentStateMachine^.State + 1;
            Transitioned := False;
            repeat
               if (CurrentStateMachine^.Pattern^.Data^[CurrentTransition] = TokenID) then
               begin
                  NextState := CurrentStateMachine^.Pattern^.Data^[CurrentTransition+1];
                  if (NextState = mpsMatch) then
                  begin
                     MatchLength := Position - Start + 1;
                  end
                  else
                  {$IFOPT C+}
                  if (NextState <= mpsMaxTrueTransition) then
                  {$ENDIF}
                  begin
                     CurrentStateMachine^.State := NextState and not mpsPreventDuplicatesMask;
                     if (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
                        ObliterateTransition(CurrentStateMachine^.Pattern, CurrentTransition+1);
                     Transitioned := True;
                  end
                  {$IFOPT C+}
                  else
                  begin
                     Assert(NextState = mpsBlocked);
                  end;
                  {$ENDIF}
               end;
               if (not Transitioned) then
                  Inc(CurrentTransition, 2);
            until ((Transitioned) or (CurrentStateMachine^.Pattern^.Data^[CurrentTransition] = mtStateEnd));
            if (not Transitioned) then
               KillStateMachine(CurrentStateMachine);
         end;
         CurrentStateMachine := NextStateMachine;
      end;
      Inc(Position);
   end;
   Assert(not Assigned(StateMachines^.Next));
   KillStateMachine(StateMachines);
   Result := MatchLength;
end;

function TMatcher.GetCanonicalMatch(Separator: AnsiString): AnsiString;
var
   Pattern: PCompiledPattern;
   Match: AnsiString;

//{$DEFINE DEBUG_CANONICAL_MATCH}

   function GetCanonicalBranch(State: TByteCode {$IFDEF DEBUG_CANONICAL_MATCH}; Prefix: AnsiString = '' {$ENDIF}): Boolean;
   var
      CurrentIndex, NextState: TByteCode;
   begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   GetCanonicalBranch(', State, '); starting with "', Match, '"'); {$ENDIF}
      CurrentIndex := State+1;
      repeat
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   trying transition ', CurrentIndex); {$ENDIF}
         NextState := Pattern^[CurrentIndex+1];
         if (NextState <> mpsBlocked) then 
         begin
            if (Pattern^[CurrentIndex] <= mtMaxTrueToken) then
            begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   ...for token ', FOriginalTokens[Pattern^[CurrentIndex]]); {$ENDIF}
               if (Match <> '') then
                  Match := Match + Separator;
               Match := Match + FOriginalTokens[Pattern^[CurrentIndex]];
            end;
            if (NextState = mpsMatch) then
            begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   MATCH'); {$ENDIF}
               Result := True;
               Exit;
            end
            else
            begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   ...going to nest...'); {$ENDIF}
               if ((Pattern^[CurrentIndex] <= mtMaxTrueToken) or (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask)) then
                   Pattern^[CurrentIndex+1] := mpsBlocked;
               if (GetCanonicalBranch(NextState and not mpsPreventDuplicatesMask {$IFDEF DEBUG_CANONICAL_MATCH}, Prefix + '  ' {$ENDIF})) then
               begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   MATCH'); {$ENDIF}
                  Result := True;
                  Exit;
               end;
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   FAIL'); {$ENDIF}
               Pattern^[CurrentIndex+1] := NextState;
            end;
         end;
         Inc(CurrentIndex, 2);
      until (Pattern^[CurrentIndex] = mtStateEnd);
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   UTTER FAIL'); {$ENDIF}
      Result := False;
   end;

begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln('GetCanonicalMatch() starting...'); {$ENDIF}
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln('Pattern: ', GetPatternDescription()); {$ENDIF}
   NewPattern(Pattern, FPatternLength);
   Move(FPattern^, Pattern^, FPatternLength * SizeOf(TByteCode));
   Match := '';
   {$IFOPT C+} Assert( {$ENDIF} GetCanonicalBranch(0) {$IFOPT C+} ) {$ENDIF} ;
   Result := Match;
   Assert(Result <> '');
   DisposePattern(Pattern, FPatternLength);
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln('   => ', Result); {$ENDIF}
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
      Inc(Index);
      repeat
         Result := Result + '   ';
         if (FPattern^[Index] = mtFollow) then
            Result := Result + 'ELSE'
         else
            Result := Result + IntToStr(FPattern^[Index]) + ':"' + FOriginalTokens[FPattern^[Index]] + '"';
         Result := Result + ' -> ';
         if (FPattern^[Index+1] = mpsMatch) then
            Result := Result + 'MATCH'
         else
            Result := Result + IntToStr(FPattern^[Index+1] and not mpsPreventDuplicatesMask);
         if (FPattern^[Index+1] and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
            Result := Result + ' (duplicates blocked)';
         Result := Result + #10;
         Inc(Index, 2);
      until (FPattern^[Index] = mtStateEnd);
      Inc(Index);
   end;
end;
{$ENDIF}

{$IFDEF DEBUG}
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
      Inc(Index);
      NeedLoop := False;
      repeat
         Result := Result + '"' + IntToStr(State) + '" -> ';
         S := 'red';
         if (FPattern^[Index+1] = mpsMatch) then
         begin
            Result := Result + '"match" [ arrowhead="diamond" fontcolor="navy" ';
            S := 'blue';
         end
         else
         begin
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
         if (FPattern^[Index] = mtFollow) then
            Result := Result + 'color="' + S + ':' + S + '" '
         else
            Result := Result + 'color="' + S + '" label="' + FOriginalTokens[FPattern^[Index]] + '" fontsize=10 samehead="' + IntToStr(State) + '" ';
         Result := Result + '];' + #10;
         Inc(Index, 2);
      until (FPattern^[Index] = mtStateEnd);
      Inc(Index);
      if (NeedLoop) then
      begin
         Result := Result + '"' + IntToStr(State) + 'p" [ label="' + IntToStr(State) + '''" shape="diamond" ];' + #10;
         Result := Result + '"' + IntToStr(State) + 'p" -> "' + IntToStr(State) + '";' + #10;
      end;
   end;
   Result := Result + '"match" [ label="Match" shape="ellipse" ];' + #10;
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
               Assert(Length(List) > 0, 'Lists must not be empty.');
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
            Assert(False, 'unknown parse mode');
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
         Assert(False, 'Trailing garbage at index ' + IntToStr(Index) + ' of ' + IntToStr(Length(S)) + ' character pattern "' + S + '"');
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
{$IFDEF DEBUG}
var
   OldHeapInfo: THeapInfo;
{$ENDIF}
begin
{$IFDEF DEBUG} OldHeapInfo := SetHeapInfo('Compiling "' + Copy(S, 1, HeapInfoSize - 12) + '"'); {$ENDIF}
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln(#10#10'Pattern: "', S, '"'); {$ENDIF}
   Singular := CompilePatternVersion(S, 0);
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln(#10#10); {$ENDIF}
   Plural := CompilePatternVersion(S, 1);
{$IFDEF DEBUG} SetHeapInfo(OldHeapInfo); {$ENDIF}
end;

{$IFDEF DEBUG}
function HasPatternChars(S: AnsiString): Boolean;
begin
   Result := (Pos('+', S) > 0) or
             (Pos('?', S) > 0) or
             (Pos('/', S) > 0) or
             (Pos('(', S) > 0) or
             (Pos(')', S) > 0) or
             (Pos('\', S) > 0) or
             (Pos('@', S) > 0) or
             (Pos('*', S) > 0) or
             (Pos('#', S) > 0) or
             (Pos('%', S) > 0) or
             (Pos('&', S) > 0);
end;

function HasSingularVsPluralAnnotation(S: AnsiString): Boolean;
begin
   Result := (Pos('/', S) > 0);
end;
{$ENDIF}

initialization
   RegisterStorableClass(TMatcher, 20);
end.
