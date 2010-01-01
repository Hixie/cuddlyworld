{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit broadcast;

interface

uses
   world, grammarian;

type
   TMessageCallback = function (): AnsiString;
   TMessageCallbackPerspective = function (Perspective: TAvatar): AnsiString;
   TMessageCallbackMethod = function (): AnsiString of object;
   TMessageCallbackPerspectiveMethod = function (Perspective: TAvatar): AnsiString of object;

   TMessageKind = (mkSpace, mkEmpty, mkCapitalise, mkAnsiString, mkCallback, mkCallbackPerspective, mkCallbackMethod, mkCallbackPerspectiveMethod, mkPluralCheck, mkPerspectivePluralCheck);
   PMessagePart = ^TMessagePart;
   TMessagePart = record
      case Kind: TMessageKind of
       mkEmpty: ();
       mkSpace: ();
       mkCapitalise: (DataCapitalisePart: PMessagePart);
       mkAnsiString: (DataString: Pointer);
       mkCallback: (DataCallback: TMessageCallback);
       mkCallbackPerspective: (DataCallbackPerspective: TMessageCallbackPerspective);
       mkCallbackMethod: (DataCallbackMethod: TMessageCallbackMethod);
       mkCallbackPerspectiveMethod: (DataCallbackPerspectiveMethod: TMessageCallbackPerspectiveMethod);
       mkPluralCheck: (DataPluralCheckThing: TThing; DataPluralSingularPart: PMessagePart; DataPluralPluralPart: PMessagePart);
       mkPerspectivePluralCheck: (DataPerspectivePluralSingularPart: PMessagePart; DataPerspectivePluralPluralPart: PMessagePart);
   end;

function SP(): PMessagePart; inline;
function C(const M: PMessagePart): PMessagePart; inline;
function M(const V: AnsiString): PMessagePart; inline;
function M(const V: TMessageCallback): PMessagePart; inline;
function M(const V: TMessageCallbackPerspective): PMessagePart; inline;
function M(const V: TMessageCallbackMethod): PMessagePart; inline;
function M(const V: TMessageCallbackPerspectiveMethod): PMessagePart; inline;
function MP(const T: TThing; const M1: PMessagePart; const M2: PMessagePart): PMessagePart; inline;
function MPP(const M1: PMessagePart; const M2: PMessagePart): PMessagePart; inline;

procedure ClearMessagePart(MessagePart: PMessagePart);

procedure DoBroadcast(NotificationTargets: array of TAtom; Perspective: TAvatar; MessageParts: array of PMessagePart);
procedure DoBroadcast(Perspective: TAvatar; MessageParts: array of PMessagePart);

implementation

uses
   sysutils;

function SP(): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkSpace;
end;

function C(const M: PMessagePart): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkCapitalise;
   Result^.DataCapitalisePart := M;
end;

function M(const V: AnsiString): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkAnsiString;
   Result^.DataString := nil;
   AnsiString(Result^.DataString) := V;
end;

function M(const V: TMessageCallback): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkCallback;
   Result^.DataCallback := V;
end;

function M(const V: TMessageCallbackPerspective): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkCallbackPerspective;
   Result^.DataCallbackPerspective := V;
end;

function M(const V: TMessageCallbackMethod): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkCallbackMethod;
   Result^.DataCallbackMethod := V;
end;

function M(const V: TMessageCallbackPerspectiveMethod): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkCallbackPerspectiveMethod;
   Result^.DataCallbackPerspectiveMethod := V;
end;

function MP(const T: TThing; const M1: PMessagePart; const M2: PMessagePart): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkPluralCheck;
   Result^.DataPluralCheckThing := T;
   Result^.DataPluralSingularPart := M1;
   Result^.DataPluralPluralPart := M2;
end;

function MPP(const M1: PMessagePart; const M2: PMessagePart): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkPerspectivePluralCheck;
   Result^.DataPerspectivePluralSingularPart := M1;
   Result^.DataPerspectivePluralPluralPart := M2;
end;


procedure ClearMessagePart(MessagePart: PMessagePart);
begin
   case MessagePart^.Kind of
    mkCapitalise: ClearMessagePart(MessagePart^.DataCapitalisePart);
    mkAnsiString: AnsiString(MessagePart^.DataString) := '';
    mkPluralCheck: begin ClearMessagePart(MessagePart^.DataPluralSingularPart); ClearMessagePart(MessagePart^.DataPluralPluralPart); end;
    mkPerspectivePluralCheck: begin ClearMessagePart(MessagePart^.DataPerspectivePluralSingularPart); ClearMessagePart(MessagePart^.DataPerspectivePluralPluralPart); end;
   end;
   Dispose(MessagePart);
end;

procedure DoBroadcast(Perspective: TAvatar; MessageParts: array of PMessagePart);
begin
   DoBroadcast([Perspective], Perspective, MessageParts);
end;

procedure DoBroadcast(NotificationTargets: array of TAtom; Perspective: TAvatar; MessageParts: array of PMessagePart);

   function Assemble(MessageParts: array of PMessagePart; Perspective: TAvatar): AnsiString;

      function GetPart(Part: PMessagePart): AnsiString; inline;
      begin
         case Part^.Kind of
           mkSpace: Result := ' ';
           mkCapitalise: Result := Result + Capitalise(GetPart(Part^.DataCapitalisePart));
           mkAnsiString: Result := AnsiString(Part^.DataString);
           mkCallback: Result := Part^.DataCallback();
           mkCallbackPerspective: Result := Part^.DataCallbackPerspective(Perspective);
           mkCallbackMethod: Result := Part^.DataCallbackMethod();
           mkCallbackPerspectiveMethod: Result := Part^.DataCallbackPerspectiveMethod(Perspective);
           mkPluralCheck: if (Part^.DataPluralCheckThing.IsPlural(Perspective)) then Result := GetPart(Part^.DataPluralPluralPart) else Result := GetPart(Part^.DataPluralSingularPart);
           mkPerspectivePluralCheck: if (Perspective.IsPlural(Perspective)) then Result := GetPart(Part^.DataPerspectivePluralPluralPart) else Result := GetPart(Part^.DataPerspectivePluralSingularPart);
          else
            raise EAssertionFailed.Create('Failed to assemble broadcast message - unexpected type ' + IntToStr(Cardinal(Part^.Kind)));
         end;
      end;

   var
      Index: Cardinal;
   begin
      Result := '';
      for Index := Low(MessageParts) to High(MessageParts) do
         Result := Result + GetPart(MessageParts[Index]);
   end;

var
   Avatars, NextAvatarItem, LastAvatarItem: PAvatarItem;
   Index: Cardinal;
   FromOutside: Boolean;
begin
   Avatars := nil;
   for Index := Low(NotificationTargets) to High(NotificationTargets) do
   begin
      NextAvatarItem := nil;
      NotificationTargets[Index].GetSurroundingsRoot(FromOutside).GetAvatars(NextAvatarItem, FromOutside);
      Avatars := MergeAvatarLists(Avatars, NextAvatarItem);
   end;
   while (Assigned(Avatars)) do
   begin
      if (Avatars^.Value <> Perspective) then
         Avatars^.Value.AvatarBroadcast(Assemble(MessageParts, Avatars^.Value));
      LastAvatarItem := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatarItem);
   end;
   for Index := Low(MessageParts) to High(MessageParts) do
      ClearMessagePart(MessageParts[Index]);
end;

end.