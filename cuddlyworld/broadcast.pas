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
       mkPluralCheck: (DataPluralCheckTarget: TAtom; DataPluralSingularPart: PMessagePart; DataPluralPluralPart: PMessagePart);
       mkPerspectivePluralCheck: (DataPerspectivePluralSingularPart: PMessagePart; DataPerspectivePluralPluralPart: PMessagePart);
   end;

function SP(): PMessagePart; inline; { SPace }
function C(const M: PMessagePart): PMessagePart; inline; { Capitalise }
function M(const V: AnsiString): PMessagePart; inline; { Message part }
function M(const V: TMessageCallback): PMessagePart; inline; { Message part }
function M(const V: TMessageCallbackPerspective): PMessagePart; inline; { Message part }
function M(const V: TMessageCallbackMethod): PMessagePart; inline; { Message part }
function M(const V: TMessageCallbackPerspectiveMethod): PMessagePart; inline; { Message part }
function MP(const T: TAtom; const M1: PMessagePart; const M2: PMessagePart): PMessagePart; inline; { Message part - thing is Plural check }
function MPP(const M1: PMessagePart; const M2: PMessagePart): PMessagePart; inline; { Message part - Perspective is Plural check }

procedure ClearMessagePart(MessagePart: PMessagePart);

procedure DoBroadcast(NotificationTargets: array of TAtom; Perspective: TAvatar; MessageParts: array of PMessagePart);
procedure DoBroadcast(Perspective: TAvatar; MessageParts: array of PMessagePart);

implementation

uses
   sysutils, lists;

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

function MP(const T: TAtom; const M1: PMessagePart; const M2: PMessagePart): PMessagePart; inline;
begin
   New(Result);
   Result^.Kind := mkPluralCheck;
   Result^.DataPluralCheckTarget := T;
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
           mkPluralCheck: if (Part^.DataPluralCheckTarget.IsPlural(Perspective)) then Result := GetPart(Part^.DataPluralPluralPart) else Result := GetPart(Part^.DataPluralSingularPart);
           mkPerspectivePluralCheck: if (Perspective.IsPlural(Perspective)) then Result := GetPart(Part^.DataPerspectivePluralPluralPart) else Result := GetPart(Part^.DataPerspectivePluralSingularPart);
          else
            Assert(False, 'Failed to assemble broadcast message - unexpected type ' + IntToStr(Cardinal(Part^.Kind)));
         end;
      end;

   var
      Index: Cardinal;
   begin
      Result := '';
      Assert(Length(MessageParts) > 0);
      for Index := Low(MessageParts) to High(MessageParts) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         Result := Result + GetPart(MessageParts[Index]);
   end;

var
   CurrentAvatar: TAvatar;
   Avatars: TAvatarList;
   Index: Cardinal;
   FromOutside: Boolean;
begin
   Assert(Length(NotificationTargets) > 0, 'Don''t call DoBroadcast with nobody to broadcast to!');
   Assert(Length(MessageParts) > 0, 'Don''t call DoBroadcast with nothing to broadcast!');
   try
      Avatars := TAvatarList.Create([slDropDuplicates]);
      try
         Assert(Length(NotificationTargets) > 0);
         for Index := Low(NotificationTargets) to High(NotificationTargets) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
            NotificationTargets[Index].GetSurroundingsRoot(FromOutside).GetAvatars(Avatars, FromOutside);
         for CurrentAvatar in Avatars do
            if (CurrentAvatar <> Perspective) then
               CurrentAvatar.AvatarBroadcast(Assemble(MessageParts, CurrentAvatar));
      finally
         Avatars.Free();
      end;
   finally
      Assert(Length(MessageParts) > 0);
      for Index := Low(MessageParts) to High(MessageParts) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         ClearMessagePart(MessageParts[Index]);
   end;
end;

end.