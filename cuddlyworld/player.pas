{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit player;

interface

uses
   storable, world, grammarian;

type

   TAllFilter = set of (afSurroundings, afSelf);

   TTalkVolume = (tvWhispering, tvSpeaking, tvShouting);

   TMessageEvent = procedure (Message: AnsiString) of object;
   TForceDisconnectEvent = procedure () of object;

   TPlayer = class(TAvatar)
    protected
      FName, FPassword: AnsiString;
      FOnAvatarMessage: TMessageEvent; { transient }
      FOnForceDisconnect: TForceDisconnectEvent; { transient }
      FContext: AnsiString; { transient }
      procedure Take(Subject: PThingItem);
      procedure Put(Subject: PThingItem; Target: TAtom; ThingPosition: TThingPosition; PutCarefully: Boolean);
      procedure Move(Subject: PThingItem; Target: TAtom; ThingPosition: TThingPosition);
      procedure Shake(Subject: PThingItem);
      function CanCarry(Thing: TThing; var Message: AnsiString): Boolean;
      function CanPush(Thing: TThing; var Message: AnsiString): Boolean;
      procedure Talk(Target: TThing; Message: AnsiString; Volume: TTalkVolume);
      procedure Dance();
      function GetReferencedThings(Tokens: TTokens; Start, Count: Cardinal; AllFilter: TAllFilter): PThingItem;
      function IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean; override;
      procedure SetContext(Context: AnsiString);
      procedure ResetContext();
    public
      constructor Create(AName: AnsiString; APassword: AnsiString);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure Perform(Command: AnsiString);
      procedure Look(); override;
      procedure Inventory();
      procedure Help();
      procedure AvatarMessage(Message: AnsiString); override;
      procedure AutoDisambiguated(Message: AnsiString);
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      procedure AnnounceAppearance(); override;
      procedure AnnounceDisappearance(); override;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceDeparture(Destination: TAtom); override;
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceArrival(Source: TAtom); override;
      function HasConnectedPlayer(): Boolean; override;
      function IsReadyForRemoval(): Boolean; override;
      procedure RemoveFromWorld(); override;
      function GetUsername(): AnsiString; override;
      function GetPassword(): AnsiString;
      procedure Adopt(AOnAvatarMessage: TMessageEvent; AOnForceDisconnect: TForceDisconnectEvent);
      procedure Abandon();
   end;

implementation

uses
   sysutils, exceptions;

type
   TActionVerb = (avNone,
                  avLook, avLookDirectional, avLookAt, avExamine, avLookUnder, avLookIn, avInventory, avFind,
                  avGo, avEnter, avClimbOn,
                  avTake, avPut, avMove, avPush, avShake,
                  avTalk, avDance,
                  avHelp);

   PTalkMessage = ^TTalkMessage;
   TTalkMessage = record
      Message: AnsiString;
   end;

   TAction = record
     case Verb: TActionVerb of
      avNone: ();
      avLook: ();
      avLookDirectional: (LookDirection: TCardinalDirection);
      avLookAt: (LookAt: TThing);
      avExamine: (ExamineSubject: TThing);
      avLookUnder: (LookUnder: TThing);
      avLookIn: (LookIn: TThing);
      avInventory: ();
      avFind: (FindSubject: TThing);
      avGo: (GoDirection: TCardinalDirection);
      avEnter: (EnterSubject: TThing);
      avClimbOn: (ClimbOnSubject: TThing);
      avTake: (TakeSubject: PThingItem);
      avPut: (PutSubject: PThingItem; PutTarget: TAtom; PutPosition: TThingPosition; PutCarefully: Boolean);
      avMove: (MoveSubject: PThingItem; MoveTarget: TAtom; MovePosition: TThingPosition);
      avPush: (PushSubject: PThingItem; PushDirection: TCardinalDirection);
      avShake: (ShakeSubject: PThingItem);
      avTalk: (TalkTarget: TThing; TalkMessage: PTalkMessage; TalkVolume: TTalkVolume);
      avDance: ();
      avHelp: ();
   end;

var
   FailedCommandLog: Text;

type
   EParseError = class
    protected
      FMessage: String;
    public
      constructor Create(AMessage: String);
      property Message: String read FMessage;
   end;

constructor EParseError.Create(AMessage: String);
begin
   inherited Create();
   FMessage := AMessage;
end;


constructor TPlayer.Create(AName: AnsiString; APassword: AnsiString);
begin
   inherited Create();
   FName := AName;
   FPassword := APassword;
end;

destructor TPlayer.Destroy();
begin
   if (Assigned(FOnForceDisconnect)) then
      FOnForceDisconnect();
   inherited;
end;

constructor TPlayer.Read(Stream: TReadStream);
begin
   inherited;
   FName := Stream.ReadAnsiString();
   FPassword := Stream.ReadAnsiString();
end;

procedure TPlayer.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FName);
   Stream.WriteAnsiString(FPassword);
end;

procedure TPlayer.Perform(Command: AnsiString);
var
   Tokens: TTokens;
   CurrentToken: Cardinal;

   procedure Fail(Message: AnsiString);
   begin
      raise EParseError.Create(Message);
   end;

{$INCLUDE parser.inc}

   procedure ExecuteAction(var Action: TAction);
   begin
      case Action.Verb of
       avLook: Look();
       avLookDirectional: AvatarMessage(FParent.GetDefaultAtom().GetLookDirection(Self, Action.LookDirection));
       avLookAt: AvatarMessage(Action.LookAt.GetLookAt(Self));
       avExamine: AvatarMessage(Action.LookAt.GetExamine(Self));
       avLookUnder: AvatarMessage(Action.LookUnder.GetLookUnder(Self));
       avLookIn: AvatarMessage(Action.LookIn.GetLookIn(Self));
       avInventory: Inventory();
       avFind: AvatarMessage(Action.FindSubject.GetPresenceStatement(Self, psTheThingIsOnThatThing));
       avGo: FParent.Navigate(Action.GoDirection, Self);
       avEnter: DoNavigation(FParent, Action.EnterSubject, tpIn, Self);
       avClimbOn: DoNavigation(FParent, Action.ClimbOnSubject, tpOn, Self);
       avTake: Take(Action.TakeSubject);
       avPut: Put(Action.PutSubject, Action.PutTarget, Action.PutPosition, Action.PutCarefully);
       avMove: Move(Action.MoveSubject, Action.MoveTarget, Action.MovePosition);
       avShake: Shake(Action.ShakeSubject);
       avTalk: Talk(Action.TalkTarget, Action.TalkMessage^.Message, Action.TalkVolume);
       avDance: Dance();
       avHelp: Help();
      else
       raise Exception.Create('Unknown verb in ExecuteAction(): ' + IntToStr(Ord(Action.Verb)));
      end;
   end;

var
   Action: TAction;
   More: Boolean;
   Atom: TAtom;
   Location: AnsiString;
begin
   try
      Tokens := Tokenise(Command);
      if (Length(Tokens) = 0) then
         Exit;
      CurrentToken := 0;
      repeat
         Action.Verb := avNone;
         try
            More := ParseAction(Action);
            ExecuteAction(Action);
         finally
            case Action.Verb of
             avTake: FreeThingList(Action.TakeSubject);
             avPut: FreeThingList(Action.PutSubject);
             avMove: FreeThingList(Action.MoveSubject);
             avPush: FreeThingList(Action.PushSubject);
             avShake: FreeThingList(Action.ShakeSubject);
             avTalk: Dispose(Action.TalkMessage);
            end;
         end;
         AvatarMessage('');
      until not More;
   except
      on E: EParseError do
      begin
         Location := '"' + FName + '"';
         Atom := FParent;
         try
            while (Assigned(Atom)) do
            begin
               Location := '"' + Atom.GetName(Self) + '"->' + Location;
               if (Atom is TThing) then
                  Atom := (Atom as TThing).Parent
               else
                  Atom := nil;
            end;
         except
           on EExternal do raise;
           on E: Exception do Location := '(while finding location: ' + E.Message + ')';
         end;
         Writeln(FailedCommandLog, '"', Command, '" for ' + Location + ': ', E.Message);
         AvatarMessage(E.Message);
         AvatarMessage('');
      end;
   end;
end;

procedure TPlayer.Look();
begin
   AvatarMessage(FParent.GetDefaultAtom().GetLook(Self));
end;

procedure TPlayer.Inventory();
var
   Contents: AnsiString;
begin
   Contents := GetInventory(Self);
   if (Length(Contents) = 0) then
      Contents := 'You are not carrying anything.';
   AvatarMessage(Contents);
end;

procedure TPlayer.Help();
begin
   AvatarMessage('Welcome to CuddlyWorld!'+ #10 +
                 'This is a pretty conventional MUD. You can move around using cardinal directions, e.g. "north", "east", "south", "west". You can shorten these to "n", "e", "s", "w". To look around, you can say "look", which can be shortened to "l". ' + 'To see what you''re holding, ask for your "inventory", which can be shortened to "i".' + #10 +
                 'More elaborate constructions are also possible. You can "take something", or "put something in something else", for instance.' + #10 +
                 'You can talk to other people by using "say", e.g. "say ''how are you?'' to Fred".' + #10 +
                 'If you find a bug, you can report it by saying "bug ''something''", for example, "bug ''the description of the camp says i can go north, but when i got north it says i cannot''". ' + 'Please be descriptive and include as much information as possible about how to reproduce the bug. Thanks!' + #10 +
                 'Have fun!');
end;

procedure TPlayer.SetContext(Context: AnsiString);
begin
   FContext := Context;
end;

procedure TPlayer.ResetContext();
begin
   FContext := '';
end;

procedure TPlayer.AvatarMessage(Message: AnsiString);
begin
   if (Assigned(FOnAvatarMessage)) then
   begin
      if (FContext <> '') then
      begin
         if (Pos(#10, Message) > 0) then
            Message := FContext + ':' + #10 + Message
         else
            Message := FContext + ': ' + Message;
      end;
      FOnAvatarMessage(Message);
   end;
end;

procedure TPlayer.AutoDisambiguated(Message: AnsiString);
begin
   AvatarMessage('(' + Message + ')');
end;

function TPlayer.GetIntrinsicMass(): TThingMass;
begin
   Result := tmPonderous;
end;

function TPlayer.GetIntrinsicSize(): TThingSize;
begin
   Result := tsMassive;
end;

function TPlayer.GetName(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := Capitalise(FName);
end;

function TPlayer.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := Capitalise(FName);
end;

function TPlayer.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := Capitalise(FName);
end;

function TPlayer.IsMatchingWord(Word: AnsiString; Perspective: TAvatar): Boolean;
begin
   if (Perspective = Self) then
   begin
      Result := Word = 'me';
   end
   else
   begin
      Result := (Word = 'them') or (Word = 'player') or (Word = 'other') or (Word = LowerCase(FName));
   end;
end;

function TPlayer.IsPlural(Perspective: TAvatar): Boolean;
begin
   if (Perspective = Self) then
      Result := True
   else
      Result := False;
end;

function TPlayer.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString;
begin
   if (Mode = psThereIsAThingHere) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' here.'
   else
   if ((Mode = psOnThatThingIsAThing) or (Mode = psTheThingIsOnThatThing)) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + TernaryConditional('is', 'are', IsPlural(Perspective)) + ' ' +
                           ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Perspective) + '.'
   else
      raise EAssertionFailed.Create('unknown mode');
end;

function TPlayer.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'You look quite fine, ' + FName + ', yes, indeed. Quite fine.'
   else
   begin
      if (HasConnectedPlayer) then
         Result := 'That other player is inconsequential, compared to your good self.'
      else
         Result := 'Their eyes look into the distance, as if they aren''t really here.';
   end;
end;

procedure TPlayer.AnnounceAppearance();
var
   Avatars, LastAvatar: PAvatarItem;
begin
   Assert(Assigned(FParent));
   Avatars := FParent.GetAvatars(Self);
   while (Assigned(Avatars)) do
   begin
      Avatars^.Value.AvatarMessage(Capitalise(GetIndefiniteName(Avatars^.Value)) + ' appears.');
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
end;

procedure TPlayer.AnnounceDisappearance();
var
   Avatars, LastAvatar: PAvatarItem;
begin
   Assert(Assigned(FParent));
   Avatars := FParent.GetAvatars(Self);
   while (Assigned(Avatars)) do
   begin
      Avatars^.Value.AvatarMessage(Capitalise(GetIndefiniteName(Avatars^.Value)) + ' disappears.');
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection);
var
   Avatars, LastAvatar: PAvatarItem;
begin
   Assert(Assigned(FParent));
   Avatars := FParent.GetAvatars(Self);
   while (Assigned(Avatars)) do
   begin
      if (Destination is TLocation) then
         Avatars^.Value.AvatarMessage(Capitalise(GetDefiniteName(Avatars^.Value)) + ' goes ' + CardinalDirectionToString(Direction) + '.')
      else
         Avatars^.Value.AvatarMessage(Capitalise(GetDefiniteName(Avatars^.Value)) + ' enters ' + Destination.GetDefiniteName(Avatars^.Value) + '.');
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom);
var
   Avatars, LastAvatar: PAvatarItem;
begin
   Assert(Assigned(FParent));
   Avatars := FParent.GetAvatars(Self);
   while (Assigned(Avatars)) do
   begin
      Avatars^.Value.AvatarMessage(Capitalise(GetDefiniteName(Avatars^.Value)) + ' enters ' + Destination.GetDefiniteName(Avatars^.Value) + '.');
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
end;

procedure TPlayer.AnnounceArrival(Source: TAtom; Direction: TCardinalDirection);
var
   Avatars, LastAvatar: PAvatarItem;
begin
   Assert(Assigned(FParent));
   Avatars := FParent.GetAvatars(Self);
   while (Assigned(Avatars)) do
   begin
      // this relies on the rooms being symmetric
      Avatars^.Value.AvatarMessage(Capitalise(GetDefiniteName(Avatars^.Value)) + ' arrives from ' + Source.GetDefiniteName(Avatars^.Value) + ' ' + CardinalDirectionToDirectionString(Direction) + '.');
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
end;

procedure TPlayer.AnnounceArrival(Source: TAtom);
var
   Avatars, LastAvatar: PAvatarItem;
begin
   Assert(Assigned(FParent));
   Avatars := FParent.GetAvatars(Self);
   while (Assigned(Avatars)) do
   begin
      // could be more intelligent by querying the current location
      // e.g. "enters from" when the current location has an exit and "arrives from" when it doesn't
      Avatars^.Value.AvatarMessage(Capitalise(GetDefiniteName(Avatars^.Value)) + ' arrives from ' + Source.GetDefiniteName(Avatars^.Value) + '.');
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
end;

procedure TPlayer.Take(Subject: PThingItem);
var
   Multiple: Boolean;
   Success: Boolean;
   Message: AnsiString;
   Ancestor: TAtom;
begin
   Assert(Assigned(Subject));
   Multiple := Assigned(Subject^.Next);
   try
      while (Assigned(Subject)) do
      begin
         if (Multiple) then
            SetContext(Capitalise(Subject^.Value.GetName(Self)));
         Message := '';
         if (Subject^.Value.Parent = Self) then
         begin
            AvatarMessage('You already have that.');
         end
         else
         if (Subject^.Value = Self) then
         begin
            AvatarMessage('You try to pick yourself up but end up on ' + Parent.GetSurface().GetDefiniteName(Self) + '.');
         end
         else
         begin
            Ancestor := Self;
            repeat
               Assert(Ancestor is TThing);
               Assert(Assigned((Ancestor as TThing).Parent));
               Ancestor := (Ancestor as TThing).Parent;
            until ((not (Ancestor is TThing)) or (Ancestor = Subject^.Value));
            if (Ancestor = Subject^.Value) then
            begin
               { we're (possibly indirectly) standing on it }
               AvatarMessage('Given your current position, that would be quite difficult.');
            end
            else
            begin
               Message := 'Taken.';
               Success := Subject^.Value.CanTake(Self, Message) and CanCarry(Subject^.Value, Message);
               AvatarMessage(Message);
               if (Success) then
                  Self.Add(Subject^.Value, tpCarried, True, Self);
            end;
         end;
         Subject := Subject^.Next;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

function TPlayer.CanCarry(Thing: TThing; var Message: AnsiString): Boolean;
begin
   if (MassAdder(Thing.GetMassManifest()) > tmHeavy) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too heavy.';
   end
   else
   if (SizeAdder(Thing.GetOutsideSizeManifest()) > tsBig) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too big.';
   end
   else
      Result := True;
end;

procedure TPlayer.Put(Subject: PThingItem; Target: TAtom; ThingPosition: TThingPosition; PutCarefully: Boolean);
var
   Multiple, Success: Boolean;
   Message: AnsiString;
   SingleThingItem: PThingItem;
   Ancestor: TAtom;
begin
   Assert(Assigned(Subject));
   Assert(Assigned(Target));
   Assert((ThingPosition = tpIn) or (ThingPosition = tpOn));
   Multiple := Assigned(Subject^.Next);
   try
      while (Assigned(Subject)) do
      begin
         if (Multiple) then
            SetContext(Capitalise(Subject^.Value.GetName(Self)));
         if (Target = Subject^.Value) then
         begin
            AvatarMessage('You can''t move something ' + ThingPositionToDirectionString(ThingPosition) + ' itself, however hard you try.');
         end
         else
         begin
            Ancestor := Target;
            repeat
               Assert(Ancestor is TThing);
               Assert(Assigned((Ancestor as TThing).Parent));
               Ancestor := (Ancestor as TThing).Parent;
            until ((not (Ancestor is TThing)) or (Ancestor = Subject^.Value));
            if (Ancestor = Subject^.Value) then
            begin
               { the target is on the thing }
               Assert(Target is TThing);
               Assert(Assigned((Target as TThing).Parent));
               Message := (Target as TThing).GetDefiniteName(Self) + ' ' + TernaryConditional('is', 'are', (Target as TThing).IsPlural(Self)) + ' ' + ThingPositionToString((Target as TThing).Position) + ' ';
               Ancestor := (Target as TThing).Parent;
               while (Ancestor <> Subject^.Value) do
               begin
                  Assert(Ancestor is TThing);
                  Assert(Assigned((Ancestor as TThing).Parent));
                  Message := Message + Ancestor.GetDefiniteName(Self) + ', which ' + TernaryConditional('is', 'are', (Ancestor as TThing).IsPlural(Self)) + ' ' + ThingPositionToString((Ancestor as TThing).Position) + ' ';
                  Ancestor := (Ancestor as TThing).Parent;
               end;
               Assert(Ancestor = Subject^.Value);
               Message := Message + Ancestor.GetDefiniteName(Self);
               AvatarMessage('That would be difficult, since ' + Message + '.');
            end
            else
            begin
               if (Subject^.Value.Parent <> Self) then
               begin
                  AutoDisambiguated('first taking ' + Subject^.Value.GetDefiniteName(Self));
                  New(SingleThingItem);
                  try
                     SingleThingItem^.Next := nil;
                     SingleThingItem^.Value := Subject^.Value;
                     Take(SingleThingItem);
                  finally
                     Dispose(SingleThingItem);
                  end;
               end;
               if (Subject^.Value.Parent = Self) then
               begin
                  if (PutCarefully) then
                     Message := 'Placed'
                  else
                     Message := 'Dropped';
                  if (Target <> FParent.GetSurface()) then
                     Message := Message + ' ' + ThingPositionToString(ThingPosition) + ' ' + Target.GetDefiniteName(Self);
                  Message := Message + '.';
                  Success := Target.CanPut(Subject^.Value, ThingPosition, Self, Message);
                  AvatarMessage(Message);
                  if (Success) then
                     Target.Add(Subject^.Value, ThingPosition, PutCarefully, Self);
               end;
            end;
         end;
         Subject := Subject^.Next;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.Move(Subject: PThingItem; Target: TAtom; ThingPosition: TThingPosition);
// this is somewhat convoluted -- feel free to refactor it...
var
   Multiple, NavigateToTarget, Success: Boolean;
   SingleThingItem: PThingItem;
   Ancestor, SurrogateTarget: TAtom;
   LocationSurface: TThing;
   {$IFDEF DEBUG} PreviousParent: TAtom; {$ENDIF}
   Message: AnsiString;
begin
   Assert(Assigned(Subject));
   Assert((ThingPosition = tpIn) or (ThingPosition = tpOn));
   Multiple := Assigned(Subject^.Next);
   NavigateToTarget := False;
   try
      while (Assigned(Subject)) do
      begin
         if (Multiple) then
            SetContext(Capitalise(Subject^.Value.GetName(Self)));
         if (Subject^.Value.Parent = Self) then
         begin
            New(SingleThingItem);
            try
               SingleThingItem^.Next := nil;
               SingleThingItem^.Value := Subject^.Value;
               if (Assigned(Target)) then
                  Put(SingleThingItem, Target, ThingPosition, True)
               else
                  Shake(SingleThingItem);
            finally
               Dispose(SingleThingItem);
            end;
         end
         else
         if (Subject^.Value = Self) then
         begin
            {$IFDEF DEBUG} PreviousParent := FParent; {$ENDIF}
            if (Assigned(Target)) then
               NavigateToTarget := True
            else
               Dance();
            {$IFDEF DEBUG} Assert(FParent = PreviousParent); {$ENDIF}
         end
         else
         begin
            Ancestor := Self.FParent;
            while ((Ancestor is TThing) and (Ancestor <> Subject^.Value)) do
               Ancestor := (Ancestor as TThing).Parent;
            if (Ancestor = Subject^.Value) then
            begin
               { we're (possibly indirectly) standing on it }
               AvatarMessage('Given your current position, that would be quite difficult.');
            end
            else
            begin
               { if it is directly in the room, or if it is on the
                 object that the room thinks is the surface, or if it
                 is in something: then shake it; otherwise, move it
                 off whatever it is on. }
               if (not Assigned(Target)) then
               begin
                  if (Subject^.Value.Position = tpOn) then
                  begin
                     Ancestor := Subject^.Value.Parent;
                     while (Ancestor is TThing) do
                        Ancestor := (Ancestor as TThing).Parent;
                     LocationSurface := Ancestor.GetSurface() as TThing;
                     if ((Subject^.Value.Parent = Ancestor) or
                         (Subject^.Value.Parent = LocationSurface)) then
                     begin
                        SurrogateTarget := Subject^.Value.Parent;
                     end
                     else
                     begin
                        Assert(Subject^.Value.Parent is TThing);
                        SurrogateTarget := (Subject^.Value.Parent as TThing).Parent;
                     end;
                  end
                  else
                     SurrogateTarget := Subject^.Value.Parent
               end
               else
                  SurrogateTarget := Target;
               if ((SurrogateTarget = Subject^.Value.Parent) and (ThingPosition = Subject^.Value.Position)) then
               begin
                  if (not Assigned(Target)) then
                  begin
                     New(SingleThingItem);
                     try
                        SingleThingItem^.Next := nil;
                        SingleThingItem^.Value := Subject^.Value;
                        Shake(SingleThingItem);
                     finally
                        Dispose(SingleThingItem);
                     end;
                  end
                  else
                  begin
                     AvatarMessage(Capitalise(Subject^.Value.GetDefiniteName(Self) + ' ' + TernaryConditional('is', 'are', Subject^.Value.IsPlural(Self)) + ' already ' + ThingPositionToString(Subject^.Value.Position) + ' ' + Target.GetDefiniteName(Self) + '.'));
                  end;
               end
               else
               if (((ThingPosition = tpOn) and ((not (Subject^.Value.Parent is TThing)) or
                                                ((Subject^.Value.Parent as TThing).Parent <> SurrogateTarget))) or
                   ((ThingPosition = tpIn) and ((not (SurrogateTarget is TThing)) or
                                                (Subject^.Value.Parent <> (SurrogateTarget as TThing).Parent)))) then
               begin
                  // need to lift it there. if it wasn't explicit, assume they wanted to shae. otherwise, just take it and drop it there.
                  // maybe we can make this check the CanCarry() and CanTake() aspects first.
                  // maybe we shouldn't even check the CanTake() aspects; you can shovel dirt or leaves from a pile into a bag you're holding even if you can't hold the pile.
                  // (if you don't just call put, then make sure to check that we're not allowing a parenthood loop)
                  New(SingleThingItem);
                  try
                     SingleThingItem^.Next := nil;
                     SingleThingItem^.Value := Subject^.Value;
                     if (Assigned(Target)) then
                        Put(SingleThingItem, SurrogateTarget, ThingPosition, True)
                     else
                        Shake(SingleThingItem);
                  finally
                     Dispose(SingleThingItem);
                  end;
               end
               else
               if (SurrogateTarget = Subject^.Value) then
               begin
                  AvatarMessage('You can''t move something ' + ThingPositionToDirectionString(ThingPosition) + ' itself, however hard you try.');
               end
               else
               begin
                  { if we get here then we have a target that makes sense }
                  Message := 'Moved ' + ThingPositionToDirectionString(ThingPosition) + ' ' + SurrogateTarget.GetDefiniteName(Self) + '.';
                  Success := Subject^.Value.CanMove(Self, Message) and CanPush(Subject^.Value, Message) and SurrogateTarget.CanPut(Subject^.Value, ThingPosition, Self, Message);
                  AvatarMessage(Message);
                  if (Success) then
                     SurrogateTarget.Add(Subject^.Value, ThingPosition, True, Self);
               end;
            end;
         end;
         Subject := Subject^.Next;
      end;
      if (NavigateToTarget) then
      begin
         if (Multiple) then
            SetContext(Capitalise(GetName(Self)));
         DoNavigation(FParent, Target, ThingPosition, Self);
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

function TPlayer.CanPush(Thing: TThing; var Message: AnsiString): Boolean;
begin
   if (MassAdder(Thing.GetMassManifest()) > tmPonderous) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too heavy.';
   end
   else
   if (SizeAdder(Thing.GetOutsideSizeManifest()) > tsMassive) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too big.';
   end
   else
      Result := True;
end;

procedure TPlayer.Shake(Subject: PThingItem);
begin
   // go through each object, calling its "shake" method?
   // maybe only if not ludicrous in size or mass or something?
   NotImplemented();
end;

procedure TPlayer.Talk(Target: TThing; Message: AnsiString; Volume: TTalkVolume);
var
   Avatars, LastAvatar: PAvatarItem;
   SingularVerb, PluralVerb: AnsiString;
begin
   Assert(Assigned(FParent));
   case Volume of
    tvWhispering: begin SingularVerb := 'whispers'; PluralVerb := 'whisper'; end;
    tvSpeaking: begin SingularVerb := 'says'; PluralVerb := 'say'; end;
    tvShouting: begin SingularVerb := 'shouts'; PluralVerb := 'shout'; end;
   else
    raise Exception.Create('Unknown volume: ' + IntToStr(Ord(Volume)));
   end;
   Avatars := FParent.GetAvatars(Self);
   while (Assigned(Avatars)) do
   begin
      if (Assigned(Target)) then
      begin
         if ((Volume <> tvWhispering) or (Avatars^.Value = Target)) then
            Avatars^.Value.AvatarMessage(Capitalise(GetDefiniteName(Avatars^.Value)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Avatars^.Value)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Avatars^.Value) + '.');
      end
      else
      begin
         Avatars^.Value.AvatarMessage(Capitalise(GetDefiniteName(Avatars^.Value)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Avatars^.Value)) + ' ' + Message + '.');
      end;
      LastAvatar := Avatars;
      Avatars := Avatars^.Next;
      Dispose(LastAvatar);
   end;
   if (Assigned(Target)) then
      AvatarMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Self) + '.')
   else
      AvatarMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + '.');
end;

procedure TPlayer.Dance();
begin
   // need a general emoting mechanic
   NotImplemented();
end;

function TPlayer.GetReferencedThings(Tokens: TTokens; Start, Count: Cardinal; AllFilter: TAllFilter): PThingItem;
var
   ListEnd: PPThingItem;
begin
   Assert(Assigned(FParent));
   Result := nil;
   ListEnd := @Result;
   if ((AllFilter <> []) and (Count = 1) and (MeansEverything(Tokens[Start]))) then
   begin
      if (afSurroundings in AllFilter) then
         FParent.GetDefaultAtom().AddImplicitlyReferencedThings(Self, afSelf in AllFilter, tpExplicit, ListEnd)
      else
         Self.AddImplicitlyReferencedThings(Self, True, tpEverything, ListEnd);
   end
   else
      FParent.GetDefaultAtom().AddExplicitlyReferencedThings(Tokens, Start, Count, Self, ListEnd);
end;

function TPlayer.HasConnectedPlayer(): Boolean;
begin
   Assert(Assigned(FOnAvatarMessage) = Assigned(FOnForceDisconnect));
   Result := Assigned(FOnAvatarMessage);
end;

function TPlayer.IsReadyForRemoval(): Boolean;
begin
   Result := False;
end;

procedure TPlayer.RemoveFromWorld();
var
   Child, Item, LastItem: PThingItem;
begin
   if (Assigned(FChildren)) then
   begin
      Child := FChildren;
      LastItem := nil;
      while (Assigned(Child)) do
      begin
         New(Item);
         Item^.Next := LastItem;
         Item^.Value := Child^.Value;
         LastItem := Item;
         Child := Child^.Next;
      end;
      Put(Item, FParent, tpOn, False);
      FreeThingList(Item);
   end;
   AnnounceDisappearance();
   inherited;
end;

function TPlayer.GetUsername(): AnsiString;
begin
   Result := FName;
end;

function TPlayer.GetPassword(): AnsiString;
begin
   Result := FPassword;
end;

procedure TPlayer.Adopt(AOnAvatarMessage: TMessageEvent; AOnForceDisconnect: TForceDisconnectEvent);
begin
   Assert(Assigned(AOnAvatarMessage));
   Assert(Assigned(AOnForceDisconnect));
   if (HasConnectedPlayer()) then
   begin
      Assert(Assigned(FOnForceDisconnect));
      FOnForceDisconnect();
   end;
   FOnAvatarMessage := AOnAvatarMessage;
   FOnForceDisconnect := AOnForceDisconnect;
end;

procedure TPlayer.Abandon();
begin
   FOnAvatarMessage := nil;
   FOnForceDisconnect := nil;
end;

initialization
   Assign(FailedCommandLog, 'failed-commands.log');
   {$I-} Append(FailedCommandLog); {$I+}
   if (IOResult = 2) then
     Rewrite(FailedCommandLog);
   RegisterStorableClass(TPlayer, 2);
finalization
   Close(FailedCommandLog);
end.