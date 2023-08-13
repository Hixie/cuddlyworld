{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit matcher;

interface

uses
   storable, grammarian;

type
   TByteCode = Byte;
   TMatcherFlags = type Integer;
   TMatcherFlag = type TByteCode;
   PCompiledPattern = ^TCompiledPattern;
   TCompiledPattern = packed array[TByteCode] of TByteCode;

const
   mfUnset = -1; // not a valid flag value

type
   TMatcher = class(TStorable) // @RegisterStorableClass
    protected
      FTokens: TTokens; { must be stored lexically sorted }
      FOriginalTokens: TTokens; { must be stored in the same order as FTokens }
      FPattern: PCompiledPattern;
      FPatternLength: TByteCode;
      function GetTokenID(Token: UTF8String): TByteCode; { argument must be lowercase }
    public
      constructor Create(Tokens, OriginalTokens: TTokens; Pattern: PCompiledPattern; PatternLength: TByteCode); { tokens are case-aware }
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function Matches(Tokens: TTokens; Start: Cardinal; Flags: TMatcherFlags = 0): Cardinal; { case-insensitive, but tokens must be lowercase already }
      function GetCanonicalMatch(Separator: UTF8String; Flags: TMatcherFlags = 0): UTF8String;
      {$IFDEF DEBUG} function GetPatternDescription(): UTF8String; {$ENDIF}
      {$IFDEF DEBUG} function GetPatternDotFileLabels(): UTF8String; {$ENDIF}
   end;

procedure CompilePattern(S: UTF8String; out Singular: TMatcher; out Plural: TMatcher);

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
   Tokens and nested lists can be suffixed (after the suffixes mentioned above) with ":" and an
   integer in the range 0..31 to indicate a flag that must be matched for that token or list to be
   considered. Flag indices are zero-based. (TMatcherFlags' least-significant-bit corresponds to
   flag zero, the second bit corresponds to flag 1, and so forth.)
   Special characters can be escaped using \.

   Examples:
     'a b c' - only matched by "a b c"

     'the? ((glowing green)# lantern/lanterns)&' - returns a singular matcher that matches:
         "the glowing", "the green", "the lantern",
         "the glowing green", "the glowing lantern", "the green lantern",
         "the glowing green lantern", and all of those again without "the"
     ...and a plural matcher that matches the same but with "lanterns" instead of "lantern".

     '(two beads)/bead' - returns a matcher that matches "two beads" and
     a matcher that matches "bead".

     'the? burning:0 bush' - returns a matcher that matches either:
       'the burning bush' and 'burning bush' when flag 0 is set
       just 'the bush' and 'bush' when flag 0 is not set

}

{$IFDEF DEBUG}
function HasPatternChars(S: UTF8String): Boolean;
function HasSingularVsPluralAnnotation(S: UTF8String): Boolean;
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

    - Tokens $81 to $A0 are used for transitions that require certain
      flags to be set, tokens $A1 to $C0 are used for transitions that
      require certain flags not to be set, and tokens $C1 to $FD
      aren't used.

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
   kFlagCount = BitSizeOf(TMatcherFlags);
   { Magic State Flags }
   msfNormal = $00;
   { Magic Tokens }
   mtMaxTrueToken = $7F;
   mtNone = $80;
   mtFlagMin = $81;
   mtFlagMax = mtFlagMin + kFlagCount - 1;
   mtNegativeFlagMin = mtFlagMax + 1;
   mtNegativeFlagMax = mtNegativeFlagMin + kFlagCount - 1;
   mtFollow = $FE; {$IF mtFollow <= mtNegativeFlagMin} {$ERROR TMatcherFlags is too wide} {$ENDIF}
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
     IncomingTransitionCount, OutgoingTransitionCount: Cardinal;
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

   TTokenReporterCallback = procedure (Token: UTF8String) of object;
   TTokenFinderCallback = function (Token: UTF8String): TByteCode of object;

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
     FToken: UTF8String;
     FTokenID: TByteCode;
     procedure ReportTokens(Callback: TTokenReporterCallback); override;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); override;
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
    public
     constructor Create(Token: UTF8String);
   end;
   TRepeatableTokenNode = class(TTokenNode)
    protected
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
    public
   end;
   TFlagNode = class(TPatternNode)
    protected
     FFlag: TMatcherFlag;
     FSecondaryNode: TPatternNode;
     procedure HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False); override;
     procedure ReportTokens(Callback: TTokenReporterCallback); override;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); override;
    public
     constructor Create(Flag: TMatcherFlag; SecondaryNode: TPatternNode);
     destructor Destroy(); override;
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

procedure DualQuickSort(var PrimaryList, SecondaryList: TTokens); forward;
procedure DualQuickSort(var PrimaryList, SecondaryList: TTokens; L, R: Integer); forward;

procedure DualQuickSort(var PrimaryList, SecondaryList: TTokens);
begin
   Assert(Low(PrimaryList) >= Low(Integer));
   Assert(High(PrimaryList) <= High(Integer));
   Assert(Length(PrimaryList) = Length(SecondaryList));
   if (Length(PrimaryList) > 1) then
      DualQuickSort(PrimaryList, SecondaryList, Low(PrimaryList), High(PrimaryList)); // $R-
end;

procedure DualQuickSort(var PrimaryList, SecondaryList: TTokens; L, R: Integer);
{ based on QuickSort in rtl/objpas/classes/lists.inc }
var
   I, J : Integer;
   P, Q : UTF8String;
begin
   repeat
      I := L;
      J := R;
      P := PrimaryList[(L + R) div 2];
      repeat
         while (P > PrimaryList[I]) do
            I := I + 1; // $R-
         while (P < PrimaryList[J]) do
            J := J - 1; // $R-
         if (I <= J) then
         begin
            Q := PrimaryList[I];
            PrimaryList[I] := PrimaryList[J];
            PrimaryList[J] := Q;
            Q := SecondaryList[I];
            SecondaryList[I] := SecondaryList[J];
            SecondaryList[J] := Q;
            I := I + 1; // $R-
            J := J - 1; // $R-
         end;
      until (I > J);
      if (L < J) then
         DualQuickSort(PrimaryList, SecondaryList, L, J);
      L := I;
   until (I >= R);
end;

procedure DualRemoveDuplicates(var PrimaryList, SecondaryList: TTokens);
var
   Index, Count: Cardinal;
   NewPrimaryList, NewSecondaryList: TTokens;
   Last: UTF8String;
begin
   Assert(Length(PrimaryList) = Length(SecondaryList));
   Assert(Length(PrimaryList) > 0);
   SetLength(NewPrimaryList, Length(PrimaryList)); // $DFA- for NewPrimaryList
   NewPrimaryList[0] := PrimaryList[0];
   SetLength(NewSecondaryList, Length(SecondaryList)); // $DFA- for NewSecondaryList
   NewSecondaryList[0] := SecondaryList[0];
   Index := 1;
   Count := 1;
   Last := PrimaryList[0];
   while (Index < Length(PrimaryList)) do
   begin
      if (PrimaryList[Index] <> Last) then
      begin
         Last := PrimaryList[Index];
         NewPrimaryList[Count] := Last;
         NewSecondaryList[Count] := SecondaryList[Index];
         Inc(Count);
      end;
      Inc(Index);
   end;
   SetLength(NewPrimaryList, Count);
   PrimaryList := NewPrimaryList;
   SetLength(NewSecondaryList, Count);
   SecondaryList := NewSecondaryList;
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
   Inc(State^.OutgoingTransitionCount);
   Inc(TargetState^.IncomingTransitionCount);
end;

procedure NewPattern(out Pattern: PCompiledPattern; Length: TByteCode); inline;
begin
   Assert(Length > 0);
   Assert(Length < High(PtrUInt) div SizeOf(TByteCode));
   Pattern := nil; { not strictly necessary }
   GetMem(Pattern, Length*SizeOf(TByteCode)); // $R-
end;

procedure DisposePattern(var Pattern: PCompiledPattern; Length: TByteCode); inline;
begin
   Assert(Assigned(Pattern));
   FreeMem(Pattern, Length*SizeOf(TByteCode)); // $R-
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


constructor TTokenNode.Create(Token: UTF8String);
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


constructor TFlagNode.Create(Flag: TMatcherFlag; SecondaryNode: TPatternNode);
begin
   inherited Create();
   Assert(Flag < kFlagCount);
   FFlag := Flag;
   Assert(Assigned(SecondaryNode));
   FSecondaryNode := SecondaryNode;
end;

destructor TFlagNode.Destroy();
begin
   FSecondaryNode.Free();
end;

procedure TFlagNode.ReportTokens(Callback: TTokenReporterCallback);
begin
   FSecondaryNode.ReportTokens(Callback);
end;

procedure TFlagNode.FixTokenIDs(Callback: TTokenFinderCallback);
begin
   FSecondaryNode.FixTokenIDs(Callback);
end;

procedure TFlagNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   MiddleState: PState;
begin
   {
            Flag
       A=---------->O---[...]--->Z
        ------------------------>
            Negative Flag
   }
   MiddleState := GetNewState();
   AddTransition(StartState, mtFlagMin + FFlag, MiddleState, BlockDuplicates); // $R-
   FSecondaryNode.HookStates(MiddleState, TargetState, GetNewState, BlockDuplicates); // XXX should BlockDuplicates be specified here?
   if (StartState <> TargetState) then
      AddTransition(StartState, mtNegativeFlagMin + FFlag, TargetState, BlockDuplicates); // $R-
end;


constructor TChildrenPatternNode.Create(Children: array of TPatternNode);
var
   Index: Cardinal;
begin
   Assert(Length(Children) > 0);
   inherited Create();
   SetLength(FChildren, Length(Children));
   for Index := 0 to Length(Children)-1 do // $R-
      FChildren[Index] := Children[Index];
end;

destructor TChildrenPatternNode.Destroy();
var
   Index: Cardinal;
begin
   Assert(Length(FChildren) > 0);
   for Index := Low(FChildren) to High(FChildren) do // $R-
      FChildren[Index].Free();
end;

procedure TChildrenPatternNode.ReportTokens(Callback: TTokenReporterCallback);
var
   Index: Cardinal;
begin
   Assert(Length(FChildren) > 0);
   for Index := Low(FChildren) to High(FChildren) do // $R-
      FChildren[Index].ReportTokens(Callback);
end;

procedure TChildrenPatternNode.FixTokenIDs(Callback: TTokenFinderCallback);
var
   Index: Cardinal;
begin
   Assert(Length(FChildren) > 0);
   for Index := Low(FChildren) to High(FChildren) do // $R-
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
      for Index := Low(FChildren) to High(FChildren)-1 do // $R-
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
      for Index := Low(FChildren) to High(FChildren)-1 do // $R-
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
      for Index := Low(FChildren) to High(FChildren)-1 do // $R-
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
   Assert(Length(FChildren) > 0);
   for Index := Low(FChildren) to High(FChildren) do // $R-
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
   Assert(Length(FChildren) > 0);
   for Index := Low(FChildren) to High(FChildren) do // $R-
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
      for Index := Low(FChildren) to High(FChildren)-1 do // $R-
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
   Assert(Length(FChildren) > 0);
   for Index := Low(FChildren) to High(FChildren) do // $R-
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
         for Index := Low(FChildren) to High(FChildren)-2 do // $R-
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
     procedure TokenCollector(Token: UTF8String);
     function GetTokenID(Token: UTF8String): TByteCode; { case-sensitive }
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
   Result^.IncomingTransitionCount := 0;
   Result^.OutgoingTransitionCount := 0;
   Result^.Index := 0;
   Result^.NextState := FLastState;
   Result^.PreviousState := FLastState^.PreviousState;
   FLastState^.PreviousState^.NextState := Result;
   FLastState^.PreviousState := Result;
end;

procedure TPatternCompiler.TokenCollector(Token: UTF8String);
begin
   SetLength(FTokens, Length(FTokens)+1);
   FTokens[High(FTokens)] := Canonicalise(Token);
   SetLength(FOriginalTokens, Length(FOriginalTokens)+1);
   FOriginalTokens[High(FOriginalTokens)] := Token;
end;

function TPatternCompiler.GetTokenID(Token: UTF8String): TByteCode;
var
   L, R, M: TByteCode;
begin
   Assert(Low(FTokens) >= 0);
   Assert(High(FTokens) <= mtMaxTrueToken);
   Assert(Length(FTokens) > 0);
   Token := Canonicalise(Token);
   L := Low(FTokens);
   R := High(FTokens); // $R-
   repeat
      M := (R-L) div 2 + L; // $R-
      if (FTokens[M] < Token) then
         L := M+1 // $R-
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

   {$DEFINE OPT_SURROGATES}
   {$DEFINE OPT_REDUNDANT_TRANSITIONS}
   //{$DEFINE OPT_FIRST_STATE}

   function GetUltimateTarget(State: PState): PState;
   begin
      Result := State;
      while ((Result^.OutgoingTransitionCount = 1) and (Result^.Transitions^.Token = mtFollow)) do
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
      Result := (State = FFirstState) or // we don't know how to optimise the first state away currently, see OPT_FIRST_STATE
                ((State^.IncomingTransitionCount > 0) and
                 ({$IFDEF OPT_SURROGATES} (State^.OutgoingTransitionCount > 1) or (State^.Transitions^.Token <> mtFollow) {$ELSE} True {$ENDIF}));
   end;

   function TransitionIsSubSetOfAnother(Transition: PTransition; State: PState): Boolean; inline;
   var
      CandidateTransition: PTransition;
   begin
      Assert(Assigned(State));
      Assert(Assigned(Transition));
      CandidateTransition := State^.Transitions;
      repeat
         if (CandidateTransition <> Transition) then
         begin
            if (((Transition^.Token > mtMaxTrueToken) and (CandidateTransition^.Token = mtFollow)) and
                (CandidateTransition^.State = Transition^.State) and
                ((CandidateTransition^.BlockDuplicates = Transition^.BlockDuplicates) or
                 (CandidateTransition^.BlockDuplicates = False))) then
            begin
               Result := True;
               exit;
            end;
         end;
         CandidateTransition := CandidateTransition^.NextTransition;
      until not Assigned(CandidateTransition);
      Result := False;
   end;

var
   State {$IFDEF OPT_SURROGATES}, NewTargetState {$ENDIF}: PState;
   Index: Cardinal;
   Transition {$IFDEF OPT_REDUNDANT_TRANSITIONS}, PrevTransition {$ENDIF}: PTransition;
   DidSomething: Boolean;
begin
   { Build state machine }
   New(FFirstState);
   FFirstState^.Transitions := nil;
   FFirstState^.IncomingTransitionCount := 1; // so that it doesn't get optimised away
   FFirstState^.OutgoingTransitionCount := 0;
   FFirstState^.Index := 0;
   FFirstState^.PreviousState := nil;
   New(FLastState);
   FLastState^.Transitions := nil;
   FLastState^.IncomingTransitionCount := 0;
   FLastState^.OutgoingTransitionCount := 0;
   FLastState^.Index := 0;
   FLastState^.NextState := nil;
   FFirstState^.NextState := FLastState;
   FLastState^.PreviousState := FFirstState;
   FRoot.HookStates(FFirstState, FLastState, @GetNewState);

   repeat
      DidSomething := False;

      {$IFDEF OPT_SURROGATES}
      { Optimise transitions through surrogates }
      State := FFirstState;
      repeat
         Assert(Assigned(State));
         if (StateNeedsSerialising(State)) then
         begin
            Assert(State^.OutgoingTransitionCount >= 1);
            Transition := State^.Transitions;
            repeat
               Assert(Assigned(Transition));
               NewTargetState := GetUltimateTarget(Transition^.State);
               if (NewTargetState <> Transition^.State) then
               begin
                  Dec(Transition^.State^.IncomingTransitionCount);
                  Transition^.State := NewTargetState;
                  Inc(Transition^.State^.IncomingTransitionCount);
                  DidSomething := True;
               end;
               Assert((Transition^.State = FLastState) or StateNeedsSerialising(Transition^.State));
               Transition := Transition^.NextTransition;
            until (not Assigned(Transition));
         end;
         State := State^.NextState;
      until (State = FLastState);
      {$ENDIF}

      {$IFDEF OPT_REDUNDANT_TRANSITIONS}
      { Optimise redundant state transitions }
      State := FFirstState;
      repeat
         Assert(Assigned(State));
         if (StateNeedsSerialising(State)) then
         begin
            Assert(State^.OutgoingTransitionCount >= 1);
            Transition := State^.Transitions;
            PrevTransition := nil;
            repeat
               Assert(Assigned(Transition));
               if (TransitionIsSubSetOfAnother(Transition, State)) then
               begin
                  if (Assigned(PrevTransition)) then
                  begin
                     PrevTransition^.NextTransition := Transition^.NextTransition;
                     Dec(Transition^.State^.IncomingTransitionCount);
                     Dispose(Transition);
                     Dec(State^.OutgoingTransitionCount);
                     Transition := PrevTransition^.NextTransition;
                  end
                  else
                  begin
                     Assert(Assigned(Transition^.NextTransition));
                     Dec(Transition^.State^.IncomingTransitionCount);
                     State^.Transitions := Transition^.NextTransition;
                     Dispose(Transition);
                     Dec(State^.OutgoingTransitionCount);
                     Transition := State^.Transitions;
                  end;
                  DidSomething := True;
               end
               else
               begin
                  PrevTransition := Transition;
                  Transition := Transition^.NextTransition;
               end;
            until (not Assigned(Transition));
         end;
         State := State^.NextState;
      until (State = FLastState);
      {$ENDIF}

      {$IFDEF OPT_FIRST_STATE}
      { Future optimisation idea: We currently always serialise the first
        state even if it could be optimised away, because optimising it
        away would require making sure the next state came first.  What
        we really should do is detect this case, and copy all the states
        from the next state to the first state, then fixing all the
        transitions to that state to point to the first state, then
        removing the transitions from that state so it gets optimised
        away. (We would then rinse-repeat, in case the next state was
        also redundant.) }
      {$ENDIF}

   until not DidSomething;

   { Establish state IDs }
   Index := 0;
   State := FFirstState;
   Assert(FLastState^.OutgoingTransitionCount = 0);
   repeat
      Assert(Assigned(State));
      Assert(State <> FLastState);
      Assert(State^.OutgoingTransitionCount >= 1);
      Assert(Index >= 0);
      Assert(Index <= mpsMaxTrueTransition);
      Assert(Index mod 2 = 0);
      if (StateNeedsSerialising(State)) then
      begin
         Assert(Index <= High(TByteCode));
         State^.Index := Index; // $R-
         Inc(Index, 2 + State^.OutgoingTransitionCount * 2);
         Assert(Index <= mpsMaxTrueTransition, 'Pattern too complicated.');
      end {$IFOPT C+} else State^.Index := mpsNotSerialised {$ENDIF};
      State := State^.NextState;
   until (State = FLastState);
   FLastState^.Index := mpsMatch;

   { Serialise the compiled pattern }
   Assert(Index <= High(PatternLength));
   PatternLength := Index; // $R-
   NewPattern(Pattern, PatternLength);
   Index := 0;
   State := FFirstState;
   repeat
      Assert(Assigned(State));
      Assert(State <> FLastState);
      Assert(State^.OutgoingTransitionCount >= 1);
      Assert(Assigned(State^.Transitions));
      if (StateNeedsSerialising(State)) then
      begin
         Pattern^[Index] := msfNormal;
         Inc(Index);
         Transition := State^.Transitions;
         Inc(Index, (State^.OutgoingTransitionCount) * 2 + 1);
         repeat
            Assert(Assigned(Transition));
            Assert((Transition^.State = FLastState) or (StateNeedsSerialising(Transition^.State)));
            Dec(Index, 3);
            Assert(Assigned(Transition));
            Pattern^[Index] := Transition^.Token;
            Inc(Index);
            Assert(Assigned(Transition^.State));
            Assert(Index <= High(Pattern^));
            if (Transition^.BlockDuplicates) then
               Pattern^[Index] := Transition^.State^.Index or mpsPreventDuplicatesMask // $R-
            else
               Pattern^[Index] := Transition^.State^.Index; // $R-
            Transition := Transition^.NextTransition;
         until (not Assigned(Transition));
         Inc(Index, (State^.OutgoingTransitionCount-1) * 2 + 1);
         Pattern^[Index] := mtStateEnd;
         Inc(Index);
      end;
      State := State^.NextState;
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
   Assert(TokenCount > 0);
   for Index := 0 to TokenCount-1 do // $R-
   begin
      FTokens[Index] := Stream.ReadString();
      FOriginalTokens[Index] := Stream.ReadString();
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
   Assert(Length(FTokens) > 0);
   for Index := 0 to Length(FTokens)-1 do // $R-
   begin
      Stream.WriteString(FTokens[Index]);
      Stream.WriteString(FOriginalTokens[Index]);
   end;
end;

function TMatcher.GetTokenID(Token: UTF8String): TByteCode;
var
   L, R, M: TByteCode;
begin
   Assert(Length(FOriginalTokens) = Length(FTokens));
   Assert(Low(FTokens) >= Low(TByteCode));
   Assert(High(FTokens) <= High(TByteCode));
   Assert(Token = Canonicalise(Token));
   L := Low(FTokens);
   R := High(FTokens); // $R-
   repeat
      M := (R-L) div 2 + L; // $R-
      if (FTokens[M] < Token) then
         L := M+1 // $R-
      else
         R := M;
   until (L >= R);
   if (FTokens[R] = Token) then
      Result := R
   else
      Result := mtNone;
end;

// {$DEFINE VERBOSE_MATCHES} // this is highly verbose, you'll want to limit how much work the app does
function TMatcher.Matches(Tokens: TTokens; Start: Cardinal; Flags: TMatcherFlags = 0): Cardinal;

   function IsAcceptableFlagTransition(TokenID: TByteCode): Boolean;
   var
      Flag: TMatcherFlag;
   begin
      if (TokenID < mtFlagMin) then
      begin
         Result := False;
      end
      else
      if (TokenID <= mtFlagMax) then
      begin
         Flag := TokenID - mtFlagMin; // $R-
         Result := (Flags and (1 shl Flag)) <> 0;
      end
      else
      if (TokenID <= mtNegativeFlagMax) then
      begin
         Flag := TokenID - mtNegativeFlagMin; // $R-
         Result := (Flags and (1 shl Flag)) = 0;
      end
      else
      begin
         Assert(TokenID = mtFollow);
         Result := False;
      end;
   end;

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
   NextState, TokenID, CandidateTokenID: TByteCode;
   Transitioned, NeedObliteration: Boolean;
begin
   {$IFDEF VERBOSE_MATCHES}
      Writeln('Matches() starting...');
      Writeln(' Tokens = ', Serialise(Tokens, 0, Length(Tokens))); // $R-
      Writeln(' Start = ', Start);
      Writeln(' Flags = ', Flags);
      Writeln(' FTokens = ', Serialise(FTokens, 0, Length(FTokens))); // $R-
      Writeln(' FOriginalTokens = ', Serialise(FOriginalTokens, 0, Length(FOriginalTokens))); // $R-
      Writeln(' FPattern:');
      Writeln('-------');
      Writeln(GetPatternDescription());
      Writeln('-------');
   {$ENDIF}
   New(StateMachines);
   StateMachines^.State := 0;
   StateMachines^.Pattern := CreateSharedCompiledPattern(FPattern);
   StateMachines^.Next := nil;
   StateMachines^.Previous := nil;
   AppendStateMachine(StateMachines);
   MatchLength := 0;
   Position := Start;
   while (Assigned(StateMachines^.Next)) do
   begin
      { Follow mtFollow links and flag-based links first }
      { We must follow these first because we have to keep following them until we get to the
        complete set of states where the next viable transition is an actual token. }
      { This is why we do these before the real tokens. }
      CurrentStateMachine := StateMachines^.Next;
      while (Assigned(CurrentStateMachine)) do
      begin
         {$IFDEF VERBOSE_MATCHES} Writeln(' + Following automatic links in state machine ', IntToHex(PtrUInt(CurrentStateMachine), 8)); {$ENDIF}
         Assert(CurrentStateMachine^.Pattern^.Data^[CurrentStateMachine^.State] = msfNormal); { we could have other state flags some day }
         Assert(CurrentStateMachine^.State + 1 <= High(CurrentTransition));
         CurrentTransition := CurrentStateMachine^.State + 1; // $R-
         repeat
            Assert(CurrentTransition < High(CurrentTransition));
            CandidateTokenID := CurrentStateMachine^.Pattern^.Data^[CurrentTransition];
            if ((CandidateTokenID = mtFollow) or (IsAcceptableFlagTransition(CandidateTokenID))) then
            begin
               NextState := CurrentStateMachine^.Pattern^.Data^[CurrentTransition+1];
               if (NextState = mpsMatch) then
               begin
                  Assert(Position >= Start);
                  MatchLength := Position - Start; // $R-
               end
               else
               {$IFOPT C+}
               if (NextState <= mpsMaxTrueTransition) then
               {$ENDIF}
               begin
                  ChildStateMachine := AppendStateMachine(CurrentStateMachine);
                  ChildStateMachine^.State := TByteCode(NextState and not mpsPreventDuplicatesMask);
                  if (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
                  begin
                     Assert(CurrentTransition < High(TByteCode));
                     ObliterateTransition(ChildStateMachine^.Pattern, CurrentTransition+1); // $R-
                  end;
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
      if (Position > High(Tokens)) then
      begin
         {$IFDEF VERBOSE_MATCHES} Writeln('- Reached end of token stream.'); {$ENDIF}
         TokenID := mtNone
      end
      else
      begin
         TokenID := GetTokenID(Tokens[Position]);
         {$IFDEF VERBOSE_MATCHES} Writeln('- Next token is "', Tokens[Position], '" with ID ', IntToHex(TokenID, 2)); {$ENDIF}
      end;
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
            Assert(CurrentStateMachine^.State < High(TByteCode));
            CurrentTransition := CurrentStateMachine^.State + 1; // $R-
            Transitioned := False;
            repeat
               {$IFDEF VERBOSE_MATCHES} Writeln(' + Following automatic links in state machine ', IntToHex(PtrUInt(CurrentStateMachine), 8)); {$ENDIF}
               CandidateTokenID := CurrentStateMachine^.Pattern^.Data^[CurrentTransition];
               {$IFDEF VERBOSE_MATCHES} Writeln('    considering pattern entry with token ID: ', CandidateTokenID); {$ENDIF}
               if (CandidateTokenID = TokenID) then
               begin
                  {$IFDEF VERBOSE_MATCHES} Writeln('      matched!'); {$ENDIF}
                  NextState := CurrentStateMachine^.Pattern^.Data^[CurrentTransition+1];
                  if (NextState = mpsMatch) then
                  begin
                     Assert(Position >= Start);
                     MatchLength := Position - Start + 1; // $R-
                  end
                  else
                  {$IFOPT C+}
                  if (NextState <= mpsMaxTrueTransition) then
                  {$ENDIF}
                  begin
                     NeedObliteration := NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask;
                     ChildStateMachine := CurrentStateMachine;
                     if (Transitioned or NeedObliteration) then
                     begin
                        ChildStateMachine := AppendStateMachine(ChildStateMachine);
                     end;
                     ChildStateMachine^.State := TByteCode(NextState and not mpsPreventDuplicatesMask);
                     if (NeedObliteration) then
                     begin
                        Assert(CurrentTransition < High(TByteCode));
                        ObliterateTransition(ChildStateMachine^.Pattern, CurrentTransition+1); // $R-
                     end;
                     Transitioned := True;
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

function TMatcher.GetCanonicalMatch(Separator: UTF8String; Flags: TMatcherFlags = 0): UTF8String;
var
   Pattern: PCompiledPattern;
   Match: UTF8String;

//{$DEFINE DEBUG_CANONICAL_MATCH}

   function IsAcceptableBranch(TokenID: TByteCode {$IFDEF DEBUG_CANONICAL_MATCH}; Prefix: UTF8String {$ENDIF}): Boolean;
   var
      Flag: TMatcherFlag;
   begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   IsAcceptableBranch(', TokenID, '); with flags ', Flags); {$ENDIF}
      if ((TokenID <= mtMaxTrueToken) or (TokenID = mtFollow)) then
      begin
         Result := True;
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '    = is true token'); {$ENDIF}
         exit;
      end;
      if (TokenID in [mtFlagMin .. mtFlagMax]) then
      begin
         Flag := TokenID - mtFlagMin; // $R-
         Result := (Flags and (1 shl Flag)) <> 0;
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '    = is flag ', Flag, ' which is: ', Result); {$ENDIF}
         exit;
      end;
      if (TokenID in [mtNegativeFlagMin .. mtNegativeFlagMax]) then
      begin
         Flag := TokenID - mtNegativeFlagMin; // $R-
         Result := (Flags and (1 shl Flag)) = 0;
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '    = is negative flag ', Flag, ' which is: ', Result); {$ENDIF}
         exit;
      end;
      Result := false;
      Assert(False);
   end;

   function GetCanonicalBranch(State: TByteCode {$IFDEF DEBUG_CANONICAL_MATCH}; Prefix: UTF8String = '' {$ENDIF}): Boolean;
   var
      CurrentIndex, NextState: TByteCode;
   begin
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   GetCanonicalBranch(', State, '); starting with "', Match, '"'); {$ENDIF}
      Assert(State < High(TByteCode));
      CurrentIndex := State+1; // $R-
      repeat
{$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   trying transition ', CurrentIndex); {$ENDIF}
         if (IsAcceptableBranch(Pattern^[CurrentIndex] {$IFDEF DEBUG_CANONICAL_MATCH}, Prefix + ' | ' {$ENDIF})) then
         begin
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
                  if (GetCanonicalBranch(TByteCode(NextState and not mpsPreventDuplicatesMask) {$IFDEF DEBUG_CANONICAL_MATCH}, Prefix + '  ' {$ENDIF})) then
                  begin
   {$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   MATCH'); {$ENDIF}
                     Result := True;
                     Exit;
                  end;
   {$IFDEF DEBUG_CANONICAL_MATCH} Writeln(Prefix + '   FAIL'); {$ENDIF}
                  Pattern^[CurrentIndex+1] := NextState;
               end;
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
function TMatcher.GetPatternDescription(): UTF8String;
var
   Index: TByteCode;
begin
   Result := 'PATTERN' + #10 + 'Token count: ' + IntToStr(Length(FOriginalTokens)) + #10 + 'Pattern length: ' + IntToStr(FPatternLength) + '/' + IntToStr(High(TByteCode)) + #10 + 'States:' + #10;
   Index := 0;
   while (Index < FPatternLength) do
   begin
      Result := Result + ' ' + IntToHex(Index, 2) + ':' + #10;
      Inc(Index);
      repeat
         Result := Result + '   ';
         if (FPattern^[Index] = mtFollow) then
            Result := Result + 'ELSE'
         else
         if (FPattern^[Index] in [mtFlagMin .. mtFlagMax]) then
            Result := Result + 'FLAG ' + IntToStr(1 shl (FPattern^[Index] - mtFlagMin))
         else
         if (FPattern^[Index] in [mtNegativeFlagMin .. mtNegativeFlagMax]) then
            Result := Result + 'NEGATIVE FLAG ' + IntToStr(1 shl (FPattern^[Index] - mtNegativeFlagMin))
         else
            Result := Result + IntToHex(FPattern^[Index], 2) + ':"' + FOriginalTokens[FPattern^[Index]] + '"';
         Result := Result + ' -> ';
         if (FPattern^[Index+1] = mpsMatch) then
            Result := Result + 'MATCH'
         else
            Result := Result + IntToHex(FPattern^[Index+1] and not mpsPreventDuplicatesMask, 2);
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
function TMatcher.GetPatternDotFileLabels(): UTF8String;
var
   Index, State: TByteCode;
   S: UTF8String;
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
         if (FPattern^[Index] > mtMaxTrueToken) then
            case (FPattern^[Index]) of
               mtFlagMin..mtFlagMax: Result := Result + 'color="' + S + '" label="<flag ' + IntToStr(FPattern^[Index] - mtFlagMin) + '>" fontsize10 samehead="' + IntToStr(State) + '" ';
               mtNegativeFlagMin..mtNegativeFlagMax: Result := Result + 'color="' + S + '" label="<negative flag ' + IntToStr(FPattern^[Index] - mtNegativeFlagMin) + '>" fontsize10 samehead="' + IntToStr(State) + '" ';
               else Result := Result + 'color="' + S + '" label="<error ' + IntToStr(FPattern^[Index]) + '>" fontsize10 samehead="' + IntToStr(State) + '" '
            end
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

function CompilePatternVersion(S: UTF8String; Version: Cardinal): TMatcher;
const
   kVersionCount = 2;

   function Parse(var Index: Cardinal): TPatternNode;
   type
      TParseMode = (pmToken, pmFlagIndex, pmEscape, pmListType);
   var
      Token: UTF8String;
      CurrentVersion: Cardinal;
      Mode: TParseMode;
      List: array of TPatternNode;
      FlagIndex: TByteCode;

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
      SetLength(List, 0);
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
                  Assert(Length(Token) > 0, 'The "+" postfix operator must immediately follow a token.');
                  Push(TRepeatableTokenNode.Create(Token));
                  Token := '';
               end;
             '?':
               begin
                  Assert((Length(Token) > 0) or (Length(List) > 0), 'The "?" postfix operator must immediately follow a token.');
                  if (Length(Token) > 0) then
                  begin
                     Push(TZeroOrMoreOrderedPatternNode.Create([TTokenNode.Create(Token)]));
                     Token := '';
                  end
                  else
                  begin
                     Assert(not (List[High(List)] is TZeroOrMoreOrderedPatternNode), 'The "?" postfix operator cannot follow a "?" or "*" operator.');
                     List[High(List)] := TZeroOrMoreOrderedPatternNode.Create([List[High(List)]]);
                  end;
               end;
             '/':
               begin
                  if (Token <> '') then
                  begin
                     Push(TTokenNode.Create(Token));
                     Token := '';
                  end;
                  Assert(Length(List) > 0, 'The "/" operator must not be used at the start of the list.');
                  if (CurrentVersion < Version) then
                  begin
                     List[High(List)].Free();
                     SetLength(List, Length(List)-1);
                  end;
                  Inc(CurrentVersion);
                  Assert(CurrentVersion < kVersionCount, 'The "/" operator is meaningless when used more than ' + IntToStr(kVersionCount - 1) + ' time(s) per token.');
               end;
             ':':
               begin
                  if (Token <> '') then
                  begin
                     Push(TTokenNode.Create(Token));
                     Token := '';
                  end;
                  Assert(Length(List) > 0);
                  FlagIndex := 0;
                  Mode := pmFlagIndex;
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
             '\': Mode := pmEscape; // '
             '@', '*', '#', '%', '&': EAssertionFailed.Create('List suffix used inappropriately.');
             else Token := Token + S[Index];
            end;
          pmFlagIndex:
            begin
               Assert(Length(List) > 0);
               case S[Index] of
                  '0'..'9':
                     begin
                        Assert(Cardinal(FlagIndex * 10 + Ord(S[Index]) - Ord('0')) < High(TByteCode));
                        FlagIndex := FlagIndex * 10 + Ord(S[Index]) - Ord('0'); // $R-
                        Assert(FlagIndex < kFlagCount);
                     end;
               else
                 begin
                    List[High(List)] := TFlagNode.Create(FlagIndex, List[High(List)]);
                    Mode := pmToken;
                    Dec(Index);
                 end;
               end;
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
               Exit; // '(' above calls Parse() re-entrantly, this brings us back to the caller level
            end;
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
   Assert(Version < kVersionCount);
   PatternTreeRoot := Parse();
   Compiler := TPatternCompiler.Create(PatternTreeRoot);
   Tokens := Compiler.GetAtomisedTokens();
   OriginalTokens := Compiler.GetAtomisedOriginalTokens();
   Compiler.GetCompiledPattern(CompiledPattern, PatternLength);
   Compiler.Free();
   Result := TMatcher.Create(Tokens, OriginalTokens, CompiledPattern, PatternLength);
end;

procedure CompilePattern(S: UTF8String; out Singular: TMatcher; out Plural: TMatcher);
{$IFDEF DEBUG}
var
   OldHeapInfo: THeapInfo;
{$ENDIF}
begin
{$IFDEF DEBUG} OldHeapInfo := SetHeapInfo('Compiling "' + Copy(S, 1, HeapInfoSize - 12) + '"'); {$ENDIF}
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Compiling pattern: "', S, '"'); {$ENDIF}
   Singular := CompilePatternVersion(S, 0);
   Plural := CompilePatternVersion(S, 1);
{$IFDEF DEBUG_PATTERN_COMPILER} Writeln('Result (singular):'#10, Singular.GetPatternDescription()); {$ENDIF}
{$IFDEF DEBUG} SetHeapInfo(OldHeapInfo); {$ENDIF}
end;

{$IFDEF DEBUG}
function HasPatternChars(S: UTF8String): Boolean;
begin
   Result := (Pos('+', S) > 0) or
             (Pos('?', S) > 0) or
             (Pos('/', S) > 0) or
             (Pos(':', S) > 0) or
             (Pos('(', S) > 0) or
             (Pos(')', S) > 0) or
             (Pos('\', S) > 0) or // '
             (Pos('@', S) > 0) or
             (Pos('*', S) > 0) or
             (Pos('#', S) > 0) or
             (Pos('%', S) > 0) or
             (Pos('&', S) > 0);
end;

function HasSingularVsPluralAnnotation(S: UTF8String): Boolean;
begin
   Result := (Pos('/', S) > 0); // XXX should be better than that, I mean, consider '\/'...
end;
{$ENDIF}

initialization
{$INCLUDE registrations/matcher.inc}
end.
