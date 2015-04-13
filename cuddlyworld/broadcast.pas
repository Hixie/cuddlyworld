{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit broadcast;

interface

uses
   physics, grammarian;

type
   TBroadcastCallback = function (): UTF8String;
   TBroadcastCallbackPerspective = function (Perspective: TAvatar): UTF8String;
   TBroadcastCallbackMethod = function (): UTF8String of object;
   TBroadcastCallbackPerspectiveMethod = function (Perspective: TAvatar): UTF8String of object;
   TBroadcastCallbackPerspectiveConjunctionMethod = function (Perspective: TAvatar; const Conjunction: UTF8String): UTF8String of object;
   TBroadcastCallbackPluralCheckMethod = function (Perspective: TAvatar): Boolean of object;

   TBroadcastPartKind = (bpkSpace, bpkEmpty, bpkCapitalise, bpkUTF8String, bpkCallback, bpkCallbackPerspective, bpkCallbackMethod, bpkCallbackPerspectiveMethod, bpkCallbackPerspectiveConjunctionMethod, bpkPluralCheck, bpkPerspectivePluralCheck, bpkCallbackPluralCheckMethod);
   PBroadcastPart = ^TBroadcastPart;
   TBroadcastPart = record
      case Kind: TBroadcastPartKind of
       bpkEmpty: ();
       bpkSpace: ();
       bpkCapitalise: (DataCapitalisePart: PBroadcastPart);
       bpkUTF8String: (DataString: Pointer);
       bpkCallback: (DataCallback: TBroadcastCallback);
       bpkCallbackPerspective: (DataCallbackPerspective: TBroadcastCallbackPerspective);
       bpkCallbackMethod: (DataCallbackMethod: TBroadcastCallbackMethod);
       bpkCallbackPerspectiveMethod: (DataCallbackPerspectiveMethod: TBroadcastCallbackPerspectiveMethod);
       bpkCallbackPerspectiveConjunctionMethod: (DataCallbackPerspectiveConjunctionMethod: TBroadcastCallbackPerspectiveConjunctionMethod; DataCallbackPerspectiveConjunctionMethodConjuction: PBroadcastPart);
       bpkPluralCheck: (DataPluralCheckTarget: TAtom; DataPluralSingularPart: PBroadcastPart; DataPluralPluralPart: PBroadcastPart);
       bpkPerspectivePluralCheck: (DataPerspectivePluralSingularPart: PBroadcastPart; DataPerspectivePluralPluralPart: PBroadcastPart);
       bpkCallbackPluralCheckMethod: (DataCallbackPluralCheckMethod: TBroadcastCallbackPluralCheckMethod; DataCallbackPluralCheckMethodSingularPart: PBroadcastPart; DataCallbackPluralCheckMethodPluralPart: PBroadcastPart);
   end;

function SP(): PBroadcastPart; inline; { SPace }
function C(const M: PBroadcastPart): PBroadcastPart; inline; { Capitalise }
function M(const V: UTF8String): PBroadcastPart; inline; { Broadcast part }
function M(const V: TBroadcastCallback): PBroadcastPart; inline; { Broadcast part }
function M(const V: TBroadcastCallbackPerspective): PBroadcastPart; inline; { Broadcast part }
function M(const V: TBroadcastCallbackMethod): PBroadcastPart; inline; { Broadcast part }
function M(const V: TBroadcastCallbackPerspectiveMethod): PBroadcastPart; inline; { Broadcast part }
function M(const V: TBroadcastCallbackPerspectiveConjunctionMethod; const C: PBroadcastPart): PBroadcastPart; inline; { Broadcast part with conjunction }
function MP(const T: TAtom; const M1: PBroadcastPart; const M2: PBroadcastPart): PBroadcastPart; inline; { Broadcast part - thing is Plural check }
function MP(const V: TBroadcastCallbackPluralCheckMethod; const M1: PBroadcastPart; const M2: PBroadcastPart): PBroadcastPart; inline; { Broadcast part - method for plural check }
function MPP(const M1: PBroadcastPart; const M2: PBroadcastPart): PBroadcastPart; inline; { Broadcast part - Perspective is Plural check }

procedure ClearBroadcastPart(BroadcastPart: PBroadcastPart);

// skips ExcludedPlayer
procedure DoBroadcast(NotificationTargets: array of TAtom; ExcludedPlayer: TAvatar; BroadcastParts: array of PBroadcastPart);

// uses nil as the ExcludedPlayer
procedure DoBroadcastAll(NotificationTargets: array of TAtom; BroadcastParts: array of PBroadcastPart);

// XXX NPCs don't get broadcasts either currently
// XXX should use TMessageKinds for broadcasts and then send them to NPCs

// XXX should probably use the '_ _ and _.' style from messages.pas

implementation

uses
   sysutils, lists, player;

function SP(): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkSpace;
end;

function C(const M: PBroadcastPart): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkCapitalise;
   Result^.DataCapitalisePart := M;
end;

function M(const V: UTF8String): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkUTF8String;
   Result^.DataString := nil;
   UTF8String(Result^.DataString) := V;
end;

function M(const V: TBroadcastCallback): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkCallback;
   Result^.DataCallback := V;
end;

function M(const V: TBroadcastCallbackPerspective): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkCallbackPerspective;
   Result^.DataCallbackPerspective := V;
end;

function M(const V: TBroadcastCallbackMethod): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkCallbackMethod;
   Result^.DataCallbackMethod := V;
end;

function M(const V: TBroadcastCallbackPerspectiveMethod): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkCallbackPerspectiveMethod;
   Result^.DataCallbackPerspectiveMethod := V;
end;

function M(const V: TBroadcastCallbackPerspectiveConjunctionMethod; const C: PBroadcastPart): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkCallbackPerspectiveConjunctionMethod;
   Result^.DataCallbackPerspectiveConjunctionMethod := V;
   Result^.DataCallbackPerspectiveConjunctionMethodConjuction := C;
end;

function MP(const T: TAtom; const M1: PBroadcastPart; const M2: PBroadcastPart): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkPluralCheck;
   Result^.DataPluralCheckTarget := T;
   Result^.DataPluralSingularPart := M1;
   Result^.DataPluralPluralPart := M2;
end;

function MP(const V: TBroadcastCallbackPluralCheckMethod; const M1: PBroadcastPart; const M2: PBroadcastPart): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkCallbackPluralCheckMethod;
   Result^.DataCallbackPluralCheckMethod := V;
   Result^.DataCallbackPluralCheckMethodSingularPart := M1;
   Result^.DataCallbackPluralCheckMethodPluralPart := M2;
end;

function MPP(const M1: PBroadcastPart; const M2: PBroadcastPart): PBroadcastPart; inline;
begin
   New(Result);
   Result^.Kind := bpkPerspectivePluralCheck;
   Result^.DataPerspectivePluralSingularPart := M1;
   Result^.DataPerspectivePluralPluralPart := M2;
end;

procedure ClearBroadcastPart(BroadcastPart: PBroadcastPart);
begin
   case BroadcastPart^.Kind of
    bpkCapitalise: ClearBroadcastPart(BroadcastPart^.DataCapitalisePart);
    bpkUTF8String: UTF8String(BroadcastPart^.DataString) := '';
    bpkCallbackPerspectiveConjunctionMethod: ClearBroadcastPart(BroadcastPart^.DataCallbackPerspectiveConjunctionMethodConjuction);
    bpkPluralCheck: begin ClearBroadcastPart(BroadcastPart^.DataPluralSingularPart); ClearBroadcastPart(BroadcastPart^.DataPluralPluralPart); end;
    bpkPerspectivePluralCheck: begin ClearBroadcastPart(BroadcastPart^.DataPerspectivePluralSingularPart); ClearBroadcastPart(BroadcastPart^.DataPerspectivePluralPluralPart); end;
    bpkCallbackPluralCheckMethod: begin ClearBroadcastPart(BroadcastPart^.DataCallbackPluralCheckMethodSingularPart); ClearBroadcastPart(BroadcastPart^.DataCallbackPluralCheckMethodPluralPart); end;
   end;
   Dispose(BroadcastPart);
end;

procedure DoBroadcastAll(NotificationTargets: array of TAtom; BroadcastParts: array of PBroadcastPart);
begin
   DoBroadcast(NotificationTargets, nil, BroadcastParts);
end;

procedure DoBroadcast(NotificationTargets: array of TAtom; ExcludedPlayer: TAvatar; BroadcastParts: array of PBroadcastPart);

   function Assemble(BroadcastParts: array of PBroadcastPart; Perspective: TAvatar): UTF8String;

      function GetPart(Part: PBroadcastPart): UTF8String; inline;
      begin
         case Part^.Kind of
           bpkSpace: Result := ' ';
           bpkCapitalise: Result := Capitalise(GetPart(Part^.DataCapitalisePart));
           bpkUTF8String: Result := UTF8String(Part^.DataString);
           bpkCallback: Result := Part^.DataCallback();
           bpkCallbackPerspective: Result := Part^.DataCallbackPerspective(Perspective);
           bpkCallbackMethod: Result := Part^.DataCallbackMethod();
           bpkCallbackPerspectiveMethod: Result := Part^.DataCallbackPerspectiveMethod(Perspective);
           bpkCallbackPerspectiveConjunctionMethod: Result := Part^.DataCallbackPerspectiveConjunctionMethod(Perspective, GetPart(Part^.DataCallbackPerspectiveConjunctionMethodConjuction));
           bpkPluralCheck: if (Part^.DataPluralCheckTarget.IsPlural(Perspective)) then Result := GetPart(Part^.DataPluralPluralPart) else Result := GetPart(Part^.DataPluralSingularPart);
           bpkPerspectivePluralCheck: if (Perspective.IsPlural(Perspective)) then Result := GetPart(Part^.DataPerspectivePluralPluralPart) else Result := GetPart(Part^.DataPerspectivePluralSingularPart);
           bpkCallbackPluralCheckMethod: if (Part^.DataCallbackPluralCheckMethod(Perspective)) then Result := GetPart(Part^.DataCallbackPluralCheckMethodPluralPart) else Result := GetPart(Part^.DataCallbackPluralCheckMethodSingularPart);
          else
            Assert(False, 'Failed to assemble broadcast message - unexpected type ' + IntToStr(Cardinal(Part^.Kind)));
            Result := '<error>';
         end;
      end;

   var
      Index: Cardinal;
   begin
      Result := '';
      Assert(Length(BroadcastParts) > 0);
      for Index := Low(BroadcastParts) to High(BroadcastParts) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         Result := Result + GetPart(BroadcastParts[Index]);
   end;

var
   CurrentPlayer: TThing;
   Players: TThingList;
   Index: Cardinal;
   FromOutside: Boolean;
begin
   Assert(Length(NotificationTargets) > 0, 'Don''t call DoBroadcast with nobody to broadcast to!');
   Assert(Length(BroadcastParts) > 0, 'Don''t call DoBroadcast with nothing to broadcast!');
   try
      Players := TThingList.Create([slDropDuplicates]);
      try
         Assert(Length(NotificationTargets) > 0);
         for Index := Low(NotificationTargets) to High(NotificationTargets) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
            NotificationTargets[Index].GetSurroundingsRoot(FromOutside).GetNearbyThingsByClass(Players, FromOutside, TPlayer);
         for CurrentPlayer in Players do
            if (CurrentPlayer <> ExcludedPlayer) then
               (CurrentPlayer as TPlayer).SendRawMessage(Assemble(BroadcastParts, CurrentPlayer as TPlayer));
      finally
         Players.Free();
      end;
   finally
      Assert(Length(BroadcastParts) > 0);
      for Index := Low(BroadcastParts) to High(BroadcastParts) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         ClearBroadcastPart(BroadcastParts[Index]);
   end;
end;

end.
