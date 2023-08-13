{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit messages;

interface

type
   TMessageKind = (mkSuccess, mkNoOp, mkEffect,
                   mkBogus, mkTooHeavy, mkTooBig, mkNotReachable, mkDuplicate, mkInHole, mkImmovable, mkNoOpening, mkClosed, mkInappropriateTool,
                   mkCannotTakeBecausePile, mkCannotMoveBecauseLocation, mkCannotMoveBecauseCustom, mkCannotLeaveLocation,
                   mkCannotFly, mkCannotWalk,
                   mkNoDoor, mkRedundant, mkBlocked, mkCannotPutOnBecauseInstalled, mkThingsFall, mkAvoidingBurialAlive);

const
   mkSuccesses = [mkSuccess, mkNoOp, mkThingsFall];

type
   TMessage = record
    private
      FKind: TMessageKind;
      FText: UTF8String;
      function GetIsValid(): Boolean;
    public
      class function Create(): TMessage; static;
      constructor Create(const NewKind: TMessageKind; const NewText: UTF8String);
      constructor Create(const NewKind: TMessageKind; const Fmt: UTF8String; const Args: array of UTF8String); // [1]
      procedure PrefaceFailureTopic(const NewText: UTF8String);
      procedure PrefaceFailureTopic(const Fmt: UTF8String; const Args: array of UTF8String); // [1]
      property AsKind: TMessageKind read FKind;
      property AsText: UTF8String read FText;
      property IsValid: Boolean read GetIsValid;
   end;

   // [1] These APIs assume that Fmt has exactly as many underscores as Args.
   //     It will crash if this is not the case.
   //     Therefore, only run it with a trusted Fmt input hard coded to have the right number of underscores.

implementation

// only call this with trusted input as Fmt
function QuickFormat(const Fmt: UTF8String; const Args: array of UTF8String): UTF8String;
var
   FmtIndex, ArgIndex, FmtStart, ResultIndex, NewLength, LengthFmt, LengthArg: Cardinal;
begin
   Assert(Length(Args) > 0); // otherwise there's no point doing this
   LengthFmt := Length(Fmt); // $R-
   Assert(Length(Fmt) >= Length(Args));
   NewLength := LengthFmt - Length(Args); // $R-
   for ArgIndex := Low(Args) to High(Args) do // $R-
      Inc(NewLength, Length(Args[ArgIndex]));
   SetLength(Result, NewLength); {BOGUS Hint: Function result variable of a managed type does not seem to be initialized}
   FmtStart := 1;
   FmtIndex := 1;
   ArgIndex := Low(Args);
   ResultIndex := 1;
   while (FmtIndex <= LengthFmt) do
   begin
      if (Fmt[FmtIndex] = '_') then
      begin
         if (FmtStart < FmtIndex) then
         begin
            Move(Fmt[FmtStart], Result[ResultIndex], FmtIndex-FmtStart);
            Inc(ResultIndex, FmtIndex-FmtStart);
         end;
         FmtStart := FmtIndex + 1; // $R-
         Assert(ArgIndex <= High(Args));
         LengthArg := Length(Args[ArgIndex]); // $R-
         if (LengthArg > 0) then
         begin
            Move(Args[ArgIndex][1], Result[ResultIndex], LengthArg);
            Inc(ResultIndex, LengthArg);
         end;
         Inc(ArgIndex);
         if (ArgIndex > High(Args)) then
            Break;
      end;
      Inc(FmtIndex);
   end;
   Assert(ArgIndex = High(Args)+1);
   if (FmtStart <= LengthFmt) then
      Move(Fmt[FmtStart], Result[ResultIndex], LengthFmt-FmtStart+1);
   Assert(ResultIndex + LengthFmt - FmtStart = Length(Result));
end;

class function TMessage.Create(): TMessage;
begin
   Result.FKind := mkSuccess;
   Result.FText := '';
end;

constructor TMessage.Create(const NewKind: TMessageKind; const NewText: UTF8String);
begin
   Assert(NewText <> '');
   FKind := NewKind;
   FText := NewText;
end;

constructor TMessage.Create(const NewKind: TMessageKind; const Fmt: UTF8String; const Args: array of UTF8String);
begin
   Assert(Fmt <> '');
   FKind := NewKind;
   FText := QuickFormat(Fmt, Args);
   Assert(FText <> '');
end;

procedure TMessage.PrefaceFailureTopic(const NewText: UTF8String);
begin
   Assert(not (FKind in mkSuccesses));
   Assert(FText <> '');
   Assert(NewText <> '');
   FText := NewText + ' ' + FText;
end;

procedure TMessage.PrefaceFailureTopic(const Fmt: UTF8String; const Args: array of UTF8String);
begin
   Assert(not (FKind in mkSuccesses));
   Assert(FText <> '');
   Assert(Fmt <> '');
   FText := QuickFormat(Fmt, Args) + ' ' + FText;
end;

function TMessage.GetIsValid(): Boolean;
begin
   Result := (FKind >= Low(TMessageKind)) and (FKind <= High(TMessageKind));
end;

initialization
//{$INCLUDE registrations/messages.inc}
end.
