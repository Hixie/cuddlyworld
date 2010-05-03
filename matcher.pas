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

type
   PState = ^TState;
   TState = record
     Transitions: PTransition;
     TransitionCount: Cardinal;
     NextState: PState;
     Surrogate: PState;
     Index: TByteCode;
   end;
   PTransition = ^TTransition;
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
    public
     constructor Create();
     destructor Destroy(); virtual;
     function GetCompiledString(): PCompiledPattern;
     procedure ReportTokens(Callback: TTokenReporterCallback); virtual; abstract;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); virtual; abstract;
   end;
   TTokenNode = class(TPatternNode)
    protected
     FToken: AnsiString;
     FTokenID: TByteCode;
    public
     constructor Create(Token: AnsiString);
     procedure ReportTokens(Callback: TTokenReporterCallback); override;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); override;
     procedure HookStates(StartStart: PState; TargetState: PState; GetNewState: TGetStateCallback; BlockDuplicates: Boolean = False);
     {
       add a transition to StartState for FToken to TargetState
     }
   end;
   TChildrenPatternNode = class(TPatternNode)
    protected
    public
     FChildren: array of TPatternNode;
     constructor Create(Children: array of TPatternNode); // fails if the array is length=0
     procedure ReportTokens(Callback: TTokenReporterCallback); override;
     procedure FixTokenIDs(Callback: TTokenFinderCallback); override;
   end;
   TSequencePatternNode = class(TChildrenPatternNode)
    protected
    public
     procedure HookStates(StartStart: PState; TargetState: PState);
     {
       let CurrentState = StartState
       for each pattern node in FChildren:
         if this is the last one, let NextState = TargetState
         otherwise, let NextState = a new State
         pass CurrentState and the NextState to the HookStates() of that node
         let CurrentState = NextState
     }
   end;
   TAlternativesPatternNode = class(TChildrenPatternNode)
    protected
    public
     procedure HookStates(StartStart: PState; TargetState: PState);
     {
       for each pattern node in FChildren:
         pass CurrentState and TargetState to the HookStates() of that node
     }
   end;
   TZeroOrMoreUnorderedPatternNode = class(TChildrenPatternNode)
    protected
    public
     procedure HookStates(StartStart: PState; TargetState: PState);
     {
       let MiddleNode be a new state
       Connect StartState to MiddleNode 0xFF.
       for each pattern node in FChildren:
         pass MiddleNode and MiddleNode to the HookStates() of that node, with 0x01 flag for transition
       add a 0xFF link from MiddleNode to TargetState
     }
   end;
   TZeroOrMoreOrderedPatternNode = class(TChildrenPatternNode)
    protected
    public
     procedure HookStates(StartStart: PState; TargetState: PState);
     {
       for each pattern node in FChildren:
         if this is the last one, let NextState = TargetState
         otherwise, let NextState = a new State
         pass CurrentState and the NextState to the HookStates() of that node
         add a 0xFF transition from CurrentState to NextState
         let CurrentState = NextState
     }
   end;
   TOneOrMoreUnorderedPatternNode = class(TChildrenPatternNode)
    protected
    public
     procedure HookStates(StartStart: PState; TargetState: PState);
     {
       let MiddleNode1 be a new state
       let MiddleNode2 be a new state
       Connect StartState to MiddleNode1 0xFF.
       for each pattern node in FChildren:
         pass MiddleNode1 and MiddleNode2 to the HookStates() of that node, with 0x01 flag for transition
       add a 0xFF link from MiddleNode2 to TargetState
       add a 0xFF link from MiddleNode2 to MiddleNode1
     }
   end;
   TOneOrMoreOrderedPatternNode = class(TChildrenPatternNode)
    protected
    public
     procedure HookStates(StartStart: PState; TargetState: PState);
     {
       if count == 1
         call HookStates() for child
       otherwise:

       let N1 = StartState
       let N2 = a new state
       call HookState for the first child with N1 and N2
       
       for each pattern node in FChildren after the first:
         let N3 = a new state
         if this is the last one:
           let N4 = TargetState
         otherwise:
           let N4 = a new state
         HookState(N3, N4) for the child
         add the following 0xFF links:
           N1-N3
           N2-N3
           N2-N4
         let N1=N3
         let N2=N4
     }
   end;

   TPattern = class
    protected
     FRoot: TPatternNode;
     FTokens: TTokens;
     FFirstState, FLastState: PState;
     function GetNewState(): PState;
    public
     constructor Create(Root: TPatternNode);
     destructor Destroy();
     function GetAtomisedTokens(): TTokens;
     {
       constructor atomises:
       construct list of all the strings, sorted
       then walk the tree again noting the resulting IDs
     }
     function Compile(): PCompiledPattern;
     {
       Create a state and end state.
       HookState the root node to them.
       Go through each one to make sure they each have at least one 0xFF transition that it's the last one (if not, add one 0xFF to nil).
       Change the others to 0xFE
       If any states have only two transitions, 0xFF to another state, then set the state's surrogate flag to that state.
       Let Index = 0
       Number the states one by one:
        if it has a surrogate state, skip the state
        if it has just one transition and it's to nil, skip the state
        set its index to Index
        increase Index by the number of transitions * 2
       Starting again with the first state, going through each state:
        if it has a surrogate state, skip the state
        if it has just one transition and it's to nil, skip the state
        serialise the transitions, as follows:
          output token id
          let s = the target state
          while s is a surrogate
            let s = the target state's surrogate
          if s has only one transition and it's to nil then 
            if it's the end state, output 0xFE else output 0xFF
          else
            output state's index, |0x1 if necessary

     2n-1   2n     (..........................)
     [token][state][token][state][token][state][0xFF][state]

     magic tokens:
      0xFE - always follow
      0xFF - always follow and last one

     magic states:
      0xFE - match
      0xFF - fail
     &0x01 - don't go through fork twice

     }
   end;

type
   PStateMachine = ^TStateMachine;
   TStateMachine = record
     State: TByteCode;
     Pattern: PCompiledPattern;
     Next: PStateMachine;
   end;

   TMatcher = class(TStorable)
    protected
      FTokens: TTokens; { must be stored sorted }
      FPattern: PCompiledPattern;
      FMaxLength: Cardinal; { length of longest match }
    public
      constructor Create(PatternTreeRoot: TPatternNode);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function Matches(Tokens: TTokens; Start: Cardinal): Cardinal; // returns number of matched tokens
      {
        this has to run the special state machine described by FPattern
        create a state machine
        copy the pattern into it
        min match = 0
        while machines still running do:
          for each state machine:
            if the state it is in has any 0xFE or 0xFF transition that are not themselves 0xFF or 0xFE:
              for each such state:
                clone the machine and advance into the state for tht transition, insert it after the current one
          advance to the next token
          if there are no more tokens, then:
            for each machine:
              if there's a transition 0xFF to 0xFE or 0xFE to 0xFE:
                min match := current position
              kill the machine, removing it from the list
          do a binary search of the tokens list to get the token id for the next token
          for each machine:
            if the token has a transition:
              if the transition is to 0xFE or 0xFF:
                if it's to 0xFE:
                  min match := current position
                kill the machine, remove it from the list
              else
                advance the state, setting the transition to 0xFF if it was &0x1=0x1
            else
              if it has a 0xFF or 0xFE transition to 0xFE or 0xFF:
                if it's to 0xFE:
                  min match := current position
                kill the machine, remove it from the list
        return min match
      }
   end;

type
   TTokenIDList = array of TByteCode;

   function Atomize(const Tokens: TTokens; Start: Cardinal): TTokenIDList;

implementation

function Compile(PatternTreeRoot: TPatternNode; out MaxLength: TByteCode): PCompiledPattern;
begin
end;

function Atomize(const Tokens: TTokens; Start: Cardinal): TTokenIDList;
begin
end;

constructor TMatcher.Create();
begin
   inherited;
end;

destructor TMatcher.Destroy();
begin
   inherited;
end;

constructor TMatcher.Read(Stream: TReadStream);
begin
   inherited;
end;

procedure TMatcher.Write(Stream: TWriteStream);
begin
   inherited;
end;

function TMatcher.Matches(Tokens: TTokens; Start: Cardinal): Cardinal; // returns number of matched tokens
begin
   // XXXX
end;

end.
