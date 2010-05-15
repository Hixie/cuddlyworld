{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit matcher;

interface

uses
   storable;

type
   TByteCode = Byte;
   PCompiledPattern = ^TCompiledPattern;
   TCompiledPattern = packed array[TByteCode] of TByteCode;
   TTokens = array of AnsiString;

   TMatcher = class(TStorable)
    protected
      FTokens: TTokens; { must be stored sorted }
      FPattern: PCompiledPattern;
      FPatternLength: TByteCode;
      function GetTokenID(Token: AnsiString): TByteCode;
    public
      constructor Create(Tokens: TTokens; Pattern: PCompiledPattern; PatternLength: TByteCode);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function Matches(Tokens: TTokens; Start: Cardinal): Cardinal;
      {$IFDEF DEBUG} function GetPatternDescription(): AnsiString; {$ENDIF}
   end;

function Pattern(S: AnsiString): TMatcher;

{

   Pattern() takes a string that consists of a space-separated list of tokens or nested lists.
   Nested lists are marked by round brackets (...).
   Tokens can have a "+" suffix indicating that the token can be repeated.
   Tokens can have a "?" suffix indicating that the token can be omitted.
   Nested lists can have suffixes to indicate what kind of list it is:
     (a b c)   - sequence list (all tokens must appear in order)
     (a b c)@  - alternatives (one of the tokens must appear)
     (a b c)*  - zero or more of the alternatives must appear, in any order
     (a b c)#  - one or more of the alternatives must appear, in any order
     (a b c)%  - zero or more of the alternatives must appear, but they must be in the order given
     (a b c)&  - one or more of the alternatives must appear, but they must be in the order given
   Otherwise special characters can be escaped using \.

   Examples:
     'a b c' - only matched by "a b c"
     'the? ((glowing green)# lantern)&' - matches:
         "the glowing", "the green", "the lantern",
         "the glowing green", "the glowing lantern", "the green lantern",
         "the glowing green lantern", and all of those again without "the"

}

implementation

uses
   sysutils;

const
   { Magic Tokens }
   mtFollow = $FE;
   mtLastFollow = $FF;
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

procedure QuickSort(var List: TTokens); forward;
procedure QuickSort(var List: TTokens; L, R: Integer); forward;

procedure QuickSort(var List: TTokens);
begin
   Assert(Low(List) >= Low(Integer));
   Assert(High(List) <= High(Integer));
   if (Length(List) > 1) then
      QuickSort(List, Low(List), High(List));
end;

procedure QuickSort(var List: TTokens; L, R: Integer);
{ based on QuickSort in rtl/objpas/classes/lists.inc }
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

procedure RemoveDuplicates(var List: TTokens);
var
   Index, Count: Cardinal;
   NewList: TTokens;
   Last: AnsiString;
begin
   Index := 1;
   Count := 1;
   SetLength(NewList, Length(List));
   Last := List[0];
   NewList[0] := List[0];
   while (Index < Length(List)) do
   begin
      if (List[Index] <> Last) then
      begin
         Last := List[Index];
         NewList[Count] := Last;
         Inc(Count);
      end;
      Inc(Index);
   end;
   SetLength(NewList, Count);
   List := NewList;
end;

procedure AddTransition(State: PState; Token: TByteCode; TargetState: PState; BlockDuplicates: Boolean);
var
   Transition: PTransition;
begin
//Writeln('AddTransition(', IntToHex(Cardinal(State), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
   Assert(Assigned(State));
   New(Transition);
   Transition^.Token := Token;
   Transition^.State := TargetState;
   Transition^.BlockDuplicates := BlockDuplicates;
   if (Token = mtFollow) then
   begin
//Writeln('Adding FollowTransition');
      Transition^.NextTransition := State^.FollowTransitions;
      State^.FollowTransitions := Transition;
   end
   else
   begin
//Writeln('Adding TokenTransition');
      Transition^.NextTransition := State^.TokenTransitions;
      State^.TokenTransitions := Transition;
   end;
   Inc(State^.TransitionCount);
//Writeln('Current transition count:', State^.TransitionCount);
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
end;

destructor TPatternNode.Destroy();
begin
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
//Writeln('TTokenNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
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
//Writeln('TRepeatableTokenNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
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
//Writeln('TSequencePatternNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
   {
       A=--[...]--->(O---[...]--->)*Z
   }
   CurrentState := StartState;
//Writeln('  about to loop');
   if (Length(FChildren) > 1) then
      for Index := Low(FChildren) to High(FChildren)-1 do
      begin
//Writeln('    loop');
         NextState := GetNewState();
         FChildren[Index].HookStates(CurrentState, NextState, GetNewState, BlockDuplicates);
         BlockDuplicates := False;
         CurrentState := NextState;
      end;
//Writeln('  looped');
   FChildren[High(FChildren)].HookStates(CurrentState, TargetState, GetNewState, BlockDuplicates);
end;

procedure TAlternativesPatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   Index: Cardinal;
   MiddleState: PState;
begin
//Writeln('TAlternativesPatternNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
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
      ...should allow AB and BC but not ABC or BAC)
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
//Writeln('TZeroOrMoreUnorderedPatternNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
   {
       A=---------->M----------->Z
        <---[...]-==
       (<---[...]-==)*
   }
   MiddleState := GetNewState();
   AddTransition(StartState, mtFollow, MiddleState, BlockDuplicates);
   for Index := Low(FChildren) to High(FChildren) do
      FChildren[Index].HookStates(MiddleState, StartState, GetNewState, True);
   AddTransition(MiddleState, mtFollow, TargetState, False);
end;

procedure TZeroOrMoreOrderedPatternNode.HookStates(StartState: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
var
   Index: Cardinal;
   CurrentState, NextState: PState;
begin
//Writeln('TZeroOrMoreOrderedPatternNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
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
//Writeln('TOneOrMoreUnorderedPatternNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
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
//Writeln('TOneOrMoreOrderedPatternNode.HookStates(', IntToHex(Cardinal(StartState), 16), ', ', IntToHex(Cardinal(TargetState), 16), ')');
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
     FTokens: TTokens;
     FFirstState, FLastState: PState;
     function GetNewState(): PState;
     procedure TokenCollector(Token: AnsiString);
     function GetTokenID(Token: AnsiString): TByteCode;
    public
     constructor Create(Root: TPatternNode);
     destructor Destroy(); override;
     function GetAtomisedTokens(): TTokens;
     procedure GetCompiledPattern(out Pattern: PCompiledPattern; out PatternLength: TByteCode);
   end;

constructor TPatternCompiler.Create(Root: TPatternNode);
begin
   inherited Create();
   FRoot := Root;
   FRoot.ReportTokens(@TokenCollector);
   Assert(High(FTokens) <= High(TByteCode), 'Too many unique strings in pattern');
   QuickSort(FTokens);
   RemoveDuplicates(FTokens);
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
//Writeln('Created PState ', IntToHex(Cardinal(Result), 16));
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
   FTokens[High(FTokens)] := Token;
end;

function TPatternCompiler.GetTokenID(Token: AnsiString): TByteCode;
var
   L, R, M: TByteCode;
begin
   Assert(Low(FTokens) >= Low(TByteCode));
   Assert(High(FTokens) <= High(TByteCode));
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
//Writeln('Created FFirstState ', IntToHex(Cardinal(FFirstState), 16));
   FFirstState^.TokenTransitions := nil;
   FFirstState^.FollowTransitions := nil;
   FFirstState^.TransitionCount := 0;
   FFirstState^.Index := 0;
   FFirstState^.PreviousState := nil;
   New(FLastState);
//Writeln('Created FLastState ', IntToHex(Cardinal(FLastState), 16));
   FLastState^.TokenTransitions := nil;
   FLastState^.FollowTransitions := nil;
   FLastState^.TransitionCount := 0;
   FLastState^.Index := 0;
   FLastState^.NextState := nil;
   FFirstState^.NextState := FLastState;
   FLastState^.PreviousState := FFirstState;
   FRoot.HookStates(FFirstState, FLastState, @GetNewState);
   { Fill in transitions and check for invariants }
//Writeln();
//Writeln('Checking...');
   State := FFirstState;
   while (Assigned(State)) do
   begin
//Writeln('Checking state ', IntToHex(Cardinal(State), 16), '... transition count = ', State^.TransitionCount);
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
//Writeln('Establishing state IDs...');
   while (Assigned(State)) do
   begin
//Writeln(' + State ', IntToHex(Cardinal(State), 16), ' has ', State^.TransitionCount, ' transitions');
      if ((State <> FLastState) and ((State = FFirstState) or (State^.TransitionCount > 1))) then
      begin
//Writeln('   and isn''t FLastState');
         Assert(Index >= Low(TByteCode));
         Assert(Index <= High(TByteCode));
//Writeln('   assigning index ', Index);
         State^.Index := Index;
         Inc(Index, State^.TransitionCount * 2);
         Assert(Index <= High(TByteCode), 'Pattern too complicated.');
      end;
      State := State^.NextState;
   end;
//Writeln('Total length = ', Index);
   { Serialise the compiled pattern }
   PatternLength := Index;
   NewPattern(Pattern, PatternLength);
   Index := 0;
   State := FFirstState;
//Writeln();
//Writeln('Serialising...');
   while (State <> FLastState) do
   begin
//Writeln('Serialising state ', IntToHex(Cardinal(State), 16), '; index=', State^.Index);
      Assert(Assigned(State));
      Transition := State^.TokenTransitions;
      while (Assigned(Transition)) do
      begin
//Writeln('Token transition for token ', Transition^.Token, ' to state ', IntToHex(Cardinal(Transition^.State), 16), '; index=', Transition^.State^.Index);
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
//Writeln('Follow transition');
         Assert(Transition^.Token = mtFollow);
         if (Assigned(Transition^.NextTransition)) then
            Pattern^[Index] := mtFollow
         else
            Pattern^[Index] := mtLastFollow;
         Inc(Index);
         if (Transition^.State = FLastState) then
         begin
//Writeln('FLastState transition');
            Pattern^[Index] := mpsMatch;
         end
         else
         if (Assigned(Transition^.State)) then
         begin
//Writeln('...to state ', IntToHex(Cardinal(Transition^.State), 16));
            if (Transition^.BlockDuplicates) then
               Pattern^[Index] := Transition^.State^.Index or mpsPreventDuplicatesMask
            else
               Pattern^[Index] := Transition^.State^.Index;
         end
         else
         begin
//Writeln('...to nowhere (fail)');
            Pattern^[Index] := mpsFail;
         end;
         Inc(Index);
         Transition := Transition^.NextTransition;
      end;
      State := State^.NextState;
   end;
   Assert(Index = PatternLength);
//Writeln('done serialising');
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


constructor TMatcher.Create(Tokens: TTokens; Pattern: PCompiledPattern; PatternLength: TByteCode);
begin
   inherited Create();
   FTokens := Tokens;
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
   for Index := 0 to TokenCount-1 do
      FTokens[Index] := Stream.ReadAnsiString();
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
      Stream.WriteAnsiString(FTokens[Index]);
end;

function TMatcher.GetTokenID(Token: AnsiString): TByteCode;
var
   L, R, M: TByteCode;
begin
   Assert(Low(FTokens) >= Low(TByteCode));
   Assert(High(FTokens) <= High(TByteCode));
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

   PStateMachine = ^TStateMachine;
   TStateMachine = record
     State: TByteCode;
     Pattern: PSharedCompiledPattern;
     Next: PStateMachine;
     Previous: PStateMachine;
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

var
   StateMachines: PStateMachine;

   function AppendStateMachine(Parent: PStateMachine): PStateMachine; inline;
   begin
      New(Result);
      Result^.State := Parent^.State;
//Writeln('+ state machine ', IntToHex(Cardinal(Result), 16), ' in state ', Result^.State);
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
//Writeln('- state machine ', IntToHex(Cardinal(Victim), 16), ' in state ', Victim^.State);
      if (Assigned(Victim^.Next)) then
         Victim^.Next^.Previous := Victim^.Previous;
      if (Assigned(Victim^.Previous)) then
         Victim^.Previous^.Next := Victim^.Next;
      DisposeSharedCompiledPattern(Victim^.Pattern);
      Dispose(Victim);
      Victim := nil;
   end;

var
   Position, MatchLength, CurrentTransition: Cardinal;
   CurrentStateMachine, NextStateMachine, ChildStateMachine: PStateMachine;
   NextState, TokenID: TByteCode;
   Transitioned: Boolean;
begin
//Writeln();
//Writeln('Starting execution engine...');
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
//Writeln('  position = ', Position, '; match length = ', MatchLength);
//Writeln('  automatic transitions...');
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
//Writeln('= state machine ', IntToHex(Cardinal(ChildStateMachine), 16), ' moved to state ', ChildStateMachine^.State);
                  if (NextState and mpsPreventDuplicatesMask = mpsPreventDuplicatesMask) then
                     ObliterateTransition(ChildStateMachine^.Pattern, CurrentTransition+1);
               end
               else
               if (NextState = mpsMatch) then
               begin
                  Assert(Position - Start <= High(TByteCode));
                  Assert(Position - Start >= Low(TByteCode));
                  MatchLength := Position - Start;
//Writeln('= state machine ', IntToHex(Cardinal(CurrentStateMachine), 16), ' matched in state ', CurrentStateMachine^.State, ' via automatic transition');
//Writeln('  increasing match length to = ', MatchLength);
               end;
            end;
            Inc(CurrentTransition, 2);
         until (CurrentStateMachine^.Pattern^.Data^[CurrentTransition-2] = mtLastFollow);
         CurrentStateMachine := CurrentStateMachine^.Next;
      end;
      { Now follow the links that match the token, removing any state machines that don't have a match }
//Writeln('  match transitions...');
      if (Position > High(Tokens)) then
         TokenID := mtNone
      else
         TokenID := GetTokenID(Tokens[Position]);
      CurrentStateMachine := StateMachines^.Next;
      while (Assigned(CurrentStateMachine)) do
      begin
         NextStateMachine := CurrentStateMachine^.Next;
         if (TokenID = mtNone) then
         begin
//Writeln('= state machine ', IntToHex(Cardinal(CurrentStateMachine), 16), ' died in state ', CurrentStateMachine^.State, ' due to running out of tokens');
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
//Writeln('= state machine ', IntToHex(Cardinal(CurrentStateMachine), 16), ' matched in state ', CurrentStateMachine^.State, ' via token match (', FTokens[TokenID], ')');
//Writeln('  increasing match length to = ', MatchLength);
                  end
                  else
                  begin
                     Assert(NextState <= mpsMaxTrueTransition);
                     CurrentStateMachine^.State := NextState and not mpsPreventDuplicatesMask;
//Writeln('= state machine ', IntToHex(Cardinal(CurrentStateMachine), 16), ' moved to state ', CurrentStateMachine^.State, ' via token match (', FTokens[TokenID], ')');
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
//Writeln('= state machine ', IntToHex(Cardinal(CurrentStateMachine), 16), ' died in state ', CurrentStateMachine^.State, ' due to not transitioning');
               KillStateMachine(CurrentStateMachine);
            end;
         end;
         CurrentStateMachine := NextStateMachine;
      end;
      Inc(Position);
   end;
   KillStateMachine(StateMachines);
//Writeln('Best match is length ', MatchLength);
   Result := MatchLength;
end;

{$IFDEF DEBUG}
function TMatcher.GetPatternDescription(): AnsiString;
var
   Index: TByteCode;
begin
   Result := 'PATTERN' + #10 + 'Token count: ' + IntToStr(Length(FTokens)) + #10 + 'States:' + #10;
   Index := 0;
   while (Index < FPatternLength) do
   begin
      Result := Result + ' ' + IntToStr(Index) + ':' + #10;
      repeat
         Result := Result + '   ';
         if (FPattern^[Index] and mtFollowMask = mtFollowMask) then
            Result := Result + 'ELSE'
         else
            Result := Result + IntToStr(FPattern^[Index]) + ':"' + FTokens[FPattern^[Index]] + '"';
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

function Pattern(S: AnsiString): TMatcher;

   function Parse(var Index: Cardinal): TPatternNode;
   type
      TParseMode = (pmToken, pmEscape, pmListType);
   var
      Token: AnsiString;
      Mode: TParseMode;
      List: array of TPatternNode;

      procedure Push(Node: TPatternNode);
      begin
         SetLength(List, Length(List)+1);
         List[High(List)] := Node;
      end;

   begin
//Writeln('NESTING PARSER starting at index=', Index);
      Token := '';
      Mode := pmToken;
      Assert(Index >= Low(S));
      while (Index <= Length(S)) do
      begin
//Writeln('Index=', Index:3, '; S=', S);
//Writeln('S[Index]=', S[Index]);
         case Mode of
          pmToken:
            case S[Index] of
             ' ':
               if (Token <> '') then
               begin
                  Push(TTokenNode.Create(Token));
                  Token := '';
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
             '(':
               begin
                  Assert(Token = '');
                  Inc(Index);
                  Push(Parse(Index));
               end;
             ')':
               begin
                  if (Token <> '') then
                  begin
                     Push(TTokenNode.Create(Token));
                     {$IFDEF DEBUG}
                       Token := 'ERROR';
                     {$ENDIF}
                  end;
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
//Writeln('pmListType suffix = ', S[Index]);
               Assert(Length(List) > 0);
               case S[Index] of
                '@': Result := TAlternativesPatternNode.Create(List);
                '*': Result := TZeroOrMoreUnorderedPatternNode.Create(List);
                '#': Result := TOneOrMoreUnorderedPatternNode.Create(List);
                '%': Result := TZeroOrMoreOrderedPatternNode.Create(List);
                '&': Result := TOneOrMoreOrderedPatternNode.Create(List);
                '+', '?': EAssertionFailed.Create('Token suffix used inappropriately.');
                else
                  begin
                     if (Length(List) > 1) then
                        Result := TSequencePatternNode.Create(List)
                     else
                        Result := List[0];
                     Dec(Index);
                  end;
               end;
//Writeln('RESUMING PARENT PARSER');
               Exit;
            end;
          else
            raise EAssertionFailed.Create('unknown parse mode');
         end;
         Inc(Index);
      end;
      Assert(Length(List) > 0);
      if (Length(List) > 1) then
         Result := TSequencePatternNode.Create(List)
      else
         Result := List[0];
//Writeln('EXITING PARSER');
   end;

   function Parse(): TPatternNode;
   var
      Index: Cardinal;
   begin
      Index := 1;
      Result := Parse(Index);
      Assert(Index = Length(S)+1);
   end;

var
   Compiler: TPatternCompiler;
   PatternTreeRoot: TPatternNode;
   Tokens: TTokens;
   CompiledPattern: PCompiledPattern;
   PatternLength: TByteCode;
begin
   PatternTreeRoot := Parse();
   Compiler := TPatternCompiler.Create(PatternTreeRoot);
   Tokens := Compiler.GetAtomisedTokens();
   Compiler.GetCompiledPattern(CompiledPattern, PatternLength);
   Compiler.Free();
   Result := TMatcher.Create(Tokens, CompiledPattern, PatternLength);
end;

end.
