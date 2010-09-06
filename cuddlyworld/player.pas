{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit player;

interface

uses
   storable, world, grammarian, thingdim, thingseeker;

const
   MaxCarryMass = tmPonderous; { not inclusive }
   MaxCarrySize = tsGigantic; { not inclusive }
   MaxPushMass = tmLudicrous; { not inclusive }
   MaxPushSize = tsLudicrous; { not inclusive }
   MaxCarryCount = 10; { not inclusive }

type
   TGender = (gMale, gFemale, gThirdGender, gRobot, gOrb, gHive);

   TTalkVolume = (tvWhispering, tvSpeaking, tvShouting);

   TMessageEvent = procedure (Message: AnsiString) of object;
   TForceDisconnectEvent = procedure () of object;

   TPlayer = class(TAvatar)
    protected
      FName, FPassword: AnsiString;
      FGender: TGender;
      FOnAvatarMessage: TMessageEvent; { transient }
      FOnForceDisconnect: TForceDisconnectEvent; { transient }
      FContext: AnsiString; { transient }
      procedure DoTake(Subject: TThingList);
      procedure DoPut(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition; PutCarefully: Boolean);
      procedure DoMove(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition);
      procedure DoPush(Subject: TThingList; Direction: TCardinalDirection);
      procedure DoRemove(Subject: TThingList; RequiredPosition: TThingPosition; RequiredParent: TThing);
      procedure DoPress(Subject: TThingList);
      procedure DoShake(Subject: TThingList);
      procedure DoDig(Target: TThing; Spade: TThing);
      procedure DoDig(Direction: TCardinalDirection; Spade: TThing);
      procedure DoTalk(Target: TThing; Message: AnsiString; Volume: TTalkVolume);
      procedure DoDance();
      {$IFDEF DEBUG}
      procedure DoDebugStatus();
      procedure DoDebugThings(Things: TThingList);
      procedure DoDebugThing(Thing: TThing);
      {$ENDIF}
      procedure DoHelp();
      procedure DoQuit();
      function CanCarry(Thing: TThing; var Message: AnsiString): Boolean;
      function CanPush(Thing: TThing; var Message: AnsiString): Boolean;
      function Referenceable(Subject: TAtom): Boolean;
      function GetImpliedThing(Scope: TAllImpliedScope; FeatureFilter: TThingFeatures): TThing;
      procedure SetContext(Context: AnsiString);
      procedure ResetContext();
    public
      constructor Create(AName: AnsiString; APassword: AnsiString; AGender: TGender);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure Perform(Command: AnsiString); virtual;
      procedure DoLook(); override;
      procedure DoInventory();
      procedure AvatarMessage(Message: AnsiString); override;
      procedure AvatarBroadcast(Message: AnsiString); override;
      procedure AutoDisambiguated(Message: AnsiString);
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetLongName(Perspective: TAvatar): AnsiString; override;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetLongDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetSubjectPronoun(Perspective: TAvatar): AnsiString; override; // I
      function GetObjectPronoun(Perspective: TAvatar): AnsiString; override; // me
      function GetReflexivePronoun(Perspective: TAvatar): AnsiString; override; // myself
      function GetPossessivePronoun(Perspective: TAvatar): AnsiString; override; // mine
      function GetPossessiveAdjective(Perspective: TAvatar): AnsiString; override; // my
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      procedure AnnounceAppearance(); override;
      procedure AnnounceDisappearance(); override;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceDeparture(Destination: TAtom); override;
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceArrival(Source: TAtom); override;
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      function HasConnectedPlayer(): Boolean; override;
      function IsReadyForRemoval(): Boolean; override;
      procedure RemoveFromWorld(); override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; override;
      function GetUsername(): AnsiString; override;
      function GetPassword(): AnsiString;
      procedure Adopt(AOnAvatarMessage: TMessageEvent; AOnForceDisconnect: TForceDisconnectEvent);
      procedure Abandon();
      property Gender: TGender read FGender write FGender;
   end;

{$IFDEF DEBUG}
type
   TStatusReportProc = procedure (Perspective: TAvatar) of object;
var
   StatusReport: TStatusReportProc = nil;
{$ENDIF}

implementation

uses
   sysutils, exceptions, broadcast, things;

type
   TActionVerb = (avNone,
                  avLook, avLookDirectional, avLookAt, avExamine, avRead, avLookUnder, avLookIn, avInventory, avFind,
                  avGo, avEnter, avClimbOn,
                  avTake, avPut, avMove, avPush, avRemove, avPress, avShake, avDig, avDigDirection,
                  avTalk, avDance,
                  {$IFDEF DEBUG} avDebugStatus, avDebugThings, avDebugThing, {$ENDIF}
                  avHelp, avQuit);

   PTalkMessage = ^TTalkMessage;
   TTalkMessage = record
      Message: AnsiString;
   end;

   TAction = record
     case Verb: TActionVerb of
      avNone: ();
      avLook: ();
      avLookDirectional: (LookDirection: TCardinalDirection);
      avLookAt: (LookAtSubject: TThing);
      avExamine: (ExamineSubject: TThing);
      avRead: (ReadSubject: TThing);
      avLookUnder: (LookUnder: TThing);
      avLookIn: (LookIn: TThing);
      avInventory: ();
      avFind: (FindSubject: TThing);
      avGo: (GoDirection: TCardinalDirection);
      avEnter: (EnterSubject: TThing);
      avClimbOn: (ClimbOnSubject: TThing);
      avTake: (TakeSubject: TThingList);
      avPut: (PutSubject: TThingList; PutTarget: TAtom; PutPosition: TThingPosition; PutCarefully: Boolean);
      avMove: (MoveSubject: TThingList; MoveTarget: TAtom; MovePosition: TThingPosition);
      avPush: (PushSubject: TThingList; PushDirection: TCardinalDirection);
      avRemove: (RemoveSubject: TThingList; RemoveFromPosition: TThingPosition; RemoveFromObject: TThing);
      avPress: (PressSubject: TThingList);
      avShake: (ShakeSubject: TThingList);
      avDig {and avDigDirection}: (DigSpade: TThing; case TActionVerb of avDig: (DigTarget: TThing); avDigDirection: (DigDirection: TCardinalDirection));
      avTalk: (TalkTarget: TThing; TalkMessage: PTalkMessage; TalkVolume: TTalkVolume);
      avDance: ();
      {$IFDEF DEBUG}
      avDebugStatus: ();
      avDebugThings: (DebugThings: TThingList);
      avDebugThing: (DebugThing: TThing);
      {$ENDIF}
      avHelp: ();
      avQuit: ();
   end;

var
   FailedCommandLog: Text;
   GlobalThingCollector: TThingCollector;

constructor TPlayer.Create(AName: AnsiString; APassword: AnsiString; AGender: TGender);
var
   Bag: TBag;
begin
   inherited Create();
   FName := AName;
   FPassword := APassword;
   FGender := AGender;
   Bag := TBag.Create('bag of holding', '(embroidered (bag/bags (of holding)?) (labeled ' + Capitalise(AName) + '))&', 'The bag has the name "' + Capitalise(AName) + '" embroidered around its rim.', tsLudicrous);
   Bag.Add(TScenery.Create('rim', '(rim/rims (bag? rim/rims))@', 'Around the bag''s rim is embroidered the name "' + Capitalise(AName) + '".'), tpAmbiguousPartOfImplicit); { the weird pattern is to avoid putting "bag" in the canonical description }
   Add(Bag, tpCarried);
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
   FGender := TGender(Stream.ReadCardinal());
end;

procedure TPlayer.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FName);
   Stream.WriteAnsiString(FPassword);
   Stream.WriteCardinal(Cardinal(FGender));
end;

procedure TPlayer.Perform(Command: AnsiString);
var
   Tokens, OriginalTokens: TTokens;
   CurrentToken: Cardinal;

{$INCLUDE parser.inc}

   procedure ExecuteAction(var Action: TAction);
   begin
      case Action.Verb of
       avLook: DoLook();
       avLookDirectional: AvatarMessage(FParent.GetDefaultAtom().GetLookDirection(Self, Action.LookDirection));
       avLookAt: AvatarMessage(Action.LookAtSubject.GetLookAt(Self));
       avExamine: AvatarMessage(Action.ExamineSubject.GetExamine(Self));
       avRead: AvatarMessage(Action.ReadSubject.GetDescriptionWriting(Self));
       avLookUnder: AvatarMessage(Action.LookUnder.GetLookUnder(Self));
       avLookIn: AvatarMessage(Action.LookIn.GetLookIn(Self));
       avInventory: DoInventory();
       avFind: AvatarMessage(Action.FindSubject.GetPresenceStatement(Self, psTheThingIsOnThatThing));
       avGo: FParent.Navigate(Action.GoDirection, Self);
       avEnter: DoNavigation(FParent, Action.EnterSubject, tpIn, Self);
       avClimbOn: DoNavigation(FParent, Action.ClimbOnSubject, tpOn, Self);
       avTake: DoTake(Action.TakeSubject);
       avPut: DoPut(Action.PutSubject, Action.PutTarget, Action.PutPosition, Action.PutCarefully);
       avMove: DoMove(Action.MoveSubject, Action.MoveTarget, Action.MovePosition);
       avPush: DoPush(Action.PushSubject, Action.PushDirection);
       avRemove: DoRemove(Action.RemoveSubject, Action.RemoveFromPosition, Action.RemoveFromObject);
       avPress: DoPress(Action.PressSubject);
       avShake: DoShake(Action.ShakeSubject);
       avDig: DoDig(Action.DigTarget, Action.DigSpade);
       avDigDirection: DoDig(Action.DigDirection, Action.DigSpade);
       avTalk: DoTalk(Action.TalkTarget, Action.TalkMessage^.Message, Action.TalkVolume);
       avDance: DoDance();
       {$IFDEF DEBUG}
       avDebugStatus: DoDebugStatus();
       avDebugThings: DoDebugThings(Action.DebugThings);
       avDebugThing: DoDebugThing(Action.DebugThing);
       {$ENDIF}
       avHelp: DoHelp();
       avQuit: DoQuit();
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
      OriginalTokens := Tokenise(Command);
      Tokens := TokeniseCanonically(Command);
      if (Length(Tokens) = 0) then
         Exit;
      CurrentToken := 0;
      repeat
         { This is suboptimal, but it's not clear how else to do it.
           The parsing is dependent on the world's state. To support
           things like "drop all then take all", we have to execute
           the actions as we parse them. However, this means that "a
           then b then" will do a, then fail to do b and complain
           about the trailing then, which isn't ideal.
         }
         Action.Verb := avNone;
         try
            More := ParseAction(Action);
            ExecuteAction(Action);
         finally
            case Action.Verb of
             avTake: Action.TakeSubject.Free();
             avPut: Action.PutSubject.Free();
             avMove: Action.MoveSubject.Free();
             avPush: Action.PushSubject.Free();
             avRemove: Action.RemoveSubject.Free();
             avPress: Action.PressSubject.Free();
             avShake: Action.ShakeSubject.Free();
             avTalk: Dispose(Action.TalkMessage);
             {$IFDEF DEBUG}
             avDebugThings: Action.DebugThings.Free();
             {$ENDIF}
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

procedure TPlayer.DoLook();
begin
   AvatarMessage(FParent.GetDefaultAtom().GetLook(Self));
end;

procedure TPlayer.DoInventory();
var
   Contents: AnsiString;
begin
   Contents := GetInventory(Self);
   if (Length(Contents) = 0) then
      Contents := 'You are not carrying anything.';
   AvatarMessage(Contents);
end;

{$IFDEF DEBUG}
procedure TPlayer.DoDebugStatus();
begin
   AvatarMessage('Debug build.');
   if (Assigned(StatusReport)) then
      StatusReport(Self);
end;

procedure TPlayer.DoDebugThings(Things: TThingList);
var
   Thing: TThing;
   Collect, FromOutside: Boolean;
begin
   Collect := not Assigned(Things);
   try
      if (Collect) then
      begin
         Things := TThingList.Create();
         GetSurroundingsRoot(FromOutside).FindMatchingThings(Self, FromOutside, True, tpEverything, [], Things);
         { there's always at least one thing: us }
      end;
      Assert(Things.Length > 0);
      for Thing in Things do
         AvatarMessage(Thing.GetName(Self) + ': ' + Thing.GetLongDefiniteName(Self));
   finally
      if (Collect) then
         Things.Free();
   end;
end;

procedure TPlayer.DoDebugThing(Thing: TThing);
begin
   AvatarMessage(Thing.Debug());
end;
{$ENDIF}

procedure TPlayer.DoHelp();
begin
   AvatarMessage('Welcome to CuddlyWorld!'+ #10 +
                 'This is a pretty conventional MUD. You can move around using cardinal directions, e.g. "north", "east", "south", "west". You can shorten these to "n", "e", "s", "w". To look around, you can say "look", which can be shortened to "l". ' + 'To see what you''re holding, ask for your "inventory", which can be shortened to "i".' + #10 +
                 'More elaborate constructions are also possible. You can "take something", or "put something in something else", for instance.' + #10 +
                 'You can talk to other people by using "say", e.g. "say ''how are you?'' to Fred".' + #10 +
                 'If you find a bug, you can report it by saying "bug ''something''", for example, "bug ''the description of the camp says i can go north, but when i got north it says i cannot''". ' + 'Please be descriptive and include as much information as possible about how to reproduce the bug. Thanks!' + #10 +
                 'Have fun!');
end;

procedure TPlayer.DoQuit();
begin
   { From 2001... }
   AvatarMessage('Look ' + Capitalise(FName) + ', I can see you''re really upset about this. I honestly think you ought to sit down calmly, take a stress pill, and think things over.');
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

procedure TPlayer.AvatarBroadcast(Message: AnsiString);
begin
   if (Assigned(FOnAvatarMessage)) then
      FOnAvatarMessage(Message);
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

function TPlayer.GetLongName(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := 'other player named ' + GetName(Perspective); // "robot"
end;

function TPlayer.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FGender of
     gMale, gFemale, gThirdGender, gRobot, gOrb: Result := Capitalise(FName);
     gHive: Result := IndefiniteArticle(FName) + ' ' + Capitalise(FName);
    else
      raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
   end;
end;

function TPlayer.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FGender of
     gMale, gFemale, gThirdGender, gRobot, gOrb: Result := Capitalise(FName);
     gHive: Result := 'The ' + Capitalise(FName);
    else
      raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
   end;
end;

function TPlayer.GetLongDefiniteName(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := 'the other player named ' + GetDefiniteName(Perspective); // "robot"
end;

function TPlayer.GetSubjectPronoun(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FGender of
     gMale: Result := 'he';
     gFemale: Result := 'she';
     gThirdGender: Result := 's/he';
     gRobot, gOrb: Result := 'it';
     gHive: Result := 'they';
    else
      raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
   end;
end;

function TPlayer.GetObjectPronoun(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FGender of
     gMale: Result := 'him';
     gFemale: Result := 'her';
     gThirdGender: Result := 'him/her';
     gRobot, gOrb: Result := 'it';
     gHive: Result := 'them';
    else
      raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
   end;
end;

function TPlayer.GetReflexivePronoun(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'yourself'
   else
   case FGender of
     gMale: Result := 'himself';
     gFemale: Result := 'herself';
     gThirdGender: Result := 'him/herself';
     gRobot, gOrb: Result := 'itself';
     gHive: Result := 'themselves';
    else
      raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
   end;
end;

function TPlayer.GetPossessivePronoun(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'yours'
   else
   case FGender of
     gMale: Result := 'his';
     gFemale: Result := 'hers';
     gThirdGender: Result := 'his/hers';
     gRobot, gOrb: Result := 'its';
     gHive: Result := 'theirs';
    else
      raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
   end;
end;

function TPlayer.GetPossessiveAdjective(Perspective: TAvatar): AnsiString;
begin
   if (Perspective = Self) then
      Result := 'your'
   else
   case FGender of
     gMale: Result := 'his';
     gFemale: Result := 'her';
     gThirdGender: Result := 'his/her';
     gRobot, gOrb: Result := 'its';
     gHive: Result := 'their';
    else
      raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
   end;
end;

function TPlayer.IsPlural(Perspective: TAvatar): Boolean;
begin
   if (Perspective = Self) then
      Result := True
   else
      Result := FGender in [gHive];
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
   begin
      case FGender of
        gMale: Result := 'You are quite the man, ' + FName + '.';
        gFemale: Result := 'You look quite the woman, ' + FName + '.';
        gThirdGender: Result := 'You look quite fine, ' + FName + ', yes, indeed. Quite fine.';
        gRobot: Result := 'You are operating within standard parameters, ' + FName + '.';
        gOrb: Result := 'You are a beautiful orb of light, ' + FName + ', floating in the air.';
        gHive: Result := 'You are quite the hive, ' + FName + '.';
       else
         raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
      end;
   end
   else
   begin
      case FGender of
        gMale: Result := Capitalise(GetDefiniteName(Perspective)) + ' is a man of no consequence.';
        gFemale: Result := Capitalise(GetDefiniteName(Perspective)) + ' is a woman of no consequence.';
        gThirdGender: Result := Capitalise(GetDefiniteName(Perspective)) + ' is a person of no consequence.';
        gRobot: Result := Capitalise(GetDefiniteName(Perspective)) + ' is a robot of no consequence.';
        gOrb: Result := Capitalise(GetDefiniteName(Perspective)) + ' is a floating orb of light of no consequence.';
        gHive: Result := Capitalise(GetDefiniteName(Perspective)) + ' is a hive mind of no consequence.';
       else
         raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
      end;
      if (not HasConnectedPlayer) then
      begin
         case FGender of
           gMale, gFemale, gThirdGender, gHive: Result := Result + ' ' + Capitalise(GetPossessiveAdjective(Perspective)) + ' eyes look into the distance, as if ' + GetSubjectPronoun(Perspective) + ' ' + TernaryConditional('isn''t', 'aren''t', IsPlural(Perspective)) + ' really here.';
           gRobot: Result := Result + ' It appears to be currently powered down, though you see no visible means of turning it on.';
           gOrb: Result := Result + ' The light is currently quite dim.';
          else
            raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
         end;
      end;
   end;
end;

procedure TPlayer.AnnounceAppearance();
begin
   DoBroadcast(Self, [C(M(@GetIndefiniteName)), SP, MP(Self, M('appears.'), M('appear.'))]);
end;

procedure TPlayer.AnnounceDisappearance();
begin
   DoBroadcast(Self, [C(M(@GetIndefiniteName)), SP, MP(Self, M('disappears.'), M('disappear.'))]);
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection);
begin
   if (Destination is TLocation) then
      DoBroadcast(Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('goes'), M('go')), SP, M(CardinalDirectionToString(Direction) + '.')])
   else
      DoBroadcast(Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('enters'), M('enter')), SP, M(@Destination.GetDefiniteName), M('.')]);
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom);
begin
   DoBroadcast(Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('enters'), M('enter')), SP, M(@Destination.GetDefiniteName), M('.')]);
end;

procedure TPlayer.AnnounceArrival(Source: TAtom; Direction: TCardinalDirection);
begin
   // this relies on the rooms being symmetric
   DoBroadcast(Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('arrives'), M('arrive')), M(' from '), M(@Source.GetDefiniteName), SP, M(CardinalDirectionToDirectionString(Direction)), M('.')]);
end;

procedure TPlayer.AnnounceArrival(Source: TAtom);
begin
   // could be more intelligent by querying the current location
   // e.g. "enters from" when the current location has an exit and "arrives from" when it doesn't
   DoBroadcast(Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('arrives'), M('arrive')), M(' from '), M(@Source.GetDefiniteName), M('.')]);
end;

procedure TPlayer.DoTake(Subject: TThingList);
var
   Multiple: Boolean;
   Success: Boolean;
   Message: AnsiString;
   Ancestor: TAtom;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Message := '';
         if (not Referenceable(CurrentSubject)) then
         begin
            AvatarMessage('You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.');
         end
         else
         if (CurrentSubject.Parent = Self) then
         begin
            AvatarMessage('You already have that.');
         end
         else
         if (CurrentSubject = Self) then
         begin
            AvatarMessage('You try to pick yourself up but end up on ' + FParent.GetSurface().GetDefiniteName(Self) + '.');
         end
         else
         begin
            Ancestor := Self;
            repeat
               Assert(Ancestor is TThing);
               Assert(Assigned((Ancestor as TThing).Parent));
               Ancestor := (Ancestor as TThing).Parent;
            until ((not (Ancestor is TThing)) or (Ancestor = CurrentSubject));
            if (Ancestor = CurrentSubject) then
            begin
               { we're (possibly indirectly) standing on it }
               AvatarMessage('Given your current position, that would be quite difficult.');
            end
            else
            begin
               Message := 'Taken.';
               Success := CurrentSubject.CanTake(Self, Message) and CanCarry(CurrentSubject, Message);
               AvatarMessage(Message);
               if (Success) then
                  Self.Put(CurrentSubject, tpCarried, True, Self);
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoPut(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition; PutCarefully: Boolean);
var
   Multiple, Success: Boolean;
   Message: AnsiString;
   SingleThingList: TThingList;
   Ancestor: TAtom;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Assert(Assigned(Target));
   Assert((ThingPosition = tpIn) or (ThingPosition = tpOn));
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         if (not Referenceable(CurrentSubject)) then
         begin
            AvatarMessage('You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.');
         end
         else
         if (not Referenceable(Target)) then
         begin
            AvatarMessage('You can''t see ' + Target.GetDefiniteName(Self) + ' anymore.');
         end
         else
         if (Target = CurrentSubject) then
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
            until ((not (Ancestor is TThing)) or (Ancestor = CurrentSubject));
            if (Ancestor = CurrentSubject) then
            begin
               { the target is on the thing }
               Assert(Target is TThing);
               Assert(Assigned((Target as TThing).Parent));
               Message := (Target as TThing).GetDefiniteName(Self) + ' ' + TernaryConditional('is', 'are', (Target as TThing).IsPlural(Self)) + ' ' + ThingPositionToString((Target as TThing).Position) + ' ';
               Ancestor := (Target as TThing).Parent;
               while (Ancestor <> CurrentSubject) do
               begin
                  Assert(Ancestor is TThing);
                  Assert(Assigned((Ancestor as TThing).Parent));
                  Message := Message + Ancestor.GetDefiniteName(Self) + ', which ' + TernaryConditional('is', 'are', (Ancestor as TThing).IsPlural(Self)) + ' ' + ThingPositionToString((Ancestor as TThing).Position) + ' ';
                  Ancestor := (Ancestor as TThing).Parent;
               end;
               Assert(Ancestor = CurrentSubject);
               Message := Message + Ancestor.GetDefiniteName(Self);
               AvatarMessage('That would be difficult, since ' + Message + '.');
            end
            else
            begin
               if (CurrentSubject.Parent <> Self) then
               begin
                  AutoDisambiguated('first taking ' + CurrentSubject.GetDefiniteName(Self));
                  SingleThingList := TThingList.Create();
                  try
                     SingleThingList.AppendItem(CurrentSubject);
                     DoTake(SingleThingList);
                  finally
                     SingleThingList.Free();
                  end;
                  if (not Referenceable(CurrentSubject)) then
                  begin
                     AvatarMessage('You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.');
                     Success := False;
                  end
                  else
                  if (not Referenceable(Target)) then
                  begin
                     AvatarMessage('You can''t see ' + Target.GetDefiniteName(Self) + ' anymore.');
                     Success := False;
                  end
                  else
                  begin
                     Success := CurrentSubject.Parent = Self;
                  end;
               end
               else
               begin
                  Success := True;
               end;
               if (Success) then
               begin
                  if (PutCarefully) then
                     Message := 'Placed'
                  else
                     Message := 'Dropped';
                  if (Target <> FParent.GetSurface()) then
                     Message := Message + ' ' + ThingPositionToString(ThingPosition) + ' ' + Target.GetDefiniteName(Self);
                  Message := Message + '.';
                  Success := Target.CanPut(CurrentSubject, ThingPosition, Self, Message);
                  AvatarMessage(Message);
                  if (Success) then
                     Target.Put(CurrentSubject, ThingPosition, PutCarefully, Self);
               end;
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoMove(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition);
// this is somewhat convoluted -- feel free to refactor it...

   function CanBePushedTo(Thing: TThing; Surface: TAtom): Boolean; { Thing is being pushed onto Surface }
   var
      Ancestor: TAtom;
   begin
      if (Thing.Position <> tpOn) then
      begin
         { There is no way you can push something onto something else if it's not already on something }
         Result := False;
         Exit;
      end;
      { can slide onto something we're holding }
      if ((Surface is TThing) and ((Surface as TThing).Parent = Self) and ((Surface as TThing).Position = tpCarried)) then
      begin
         Result := True;
         Exit;
      end;
      { next see if we're trying to move the thing off something else onto it (i.e. pushing onto an ancestor) }
      { note we have to stop if we get to something that is not on something, e.g. you can't push from on a plate that is in a bag onto the table the bag is on }
      { nor can you push it from on the plate in the bag onto the bag itself }
      Ancestor := Thing;
      repeat
         Ancestor := (Ancestor as TThing).Parent;
         if ((Ancestor = Surface) or
             ((Surface is TThing) and ((Surface as TThing).Parent = Ancestor) and (tfCanHaveThingsPushedOn in (Surface as TThing).GetFeatures()))) then
         begin
            Result := True;
            Exit;
         end;
      until ((not (Ancestor is TThing)) or
             (((Ancestor as TThing).Position in tpSeparate) and { tpIn is ok in the case where things can be pushed in/out from/to parent }
              (not (((Ancestor as TThing).Position in tpContained) and (tfCanHaveThingsPushedIn in (Ancestor as TThing).GetFeatures())))));
      Result := False;
   end;

   function CanBePushedInto(Thing: TThing; Surface: TAtom): Boolean; { Thing is being pushed into something whose inside is Surface }
   var
      Ancestor: TAtom;
   begin
      if (Thing.Position <> tpOn) then
      begin
         { There is no way you can push something into something else if it's not on something }
         Result := False;
         Exit;
      end;
      { can slide into something we're holding }
      if ((Surface is TThing) and ((Surface as TThing).Parent = Self) and ((Surface as TThing).Position = tpCarried)) then
      begin
         Result := True;
         Exit;
      end;
      { next see if we're trying to move the thing off something else into it (i.e. pushing into an ancestor) }
      { note we have to stop if we get to something that is not on something, e.g. you can't push from on a plate that is in a bag into the chest that the bag is in }
      { (but you _can_ push it into the bag) }
      Ancestor := Thing;
      repeat
         Ancestor := (Ancestor as TThing).Parent;
         if ((Ancestor = Surface) or
             ((Surface is TThing) and ((Surface as TThing).Parent = Ancestor) and (tfCanHaveThingsPushedIn in (Surface as TThing).GetFeatures()))) then
         begin
            Result := True;
            Exit;
         end;
      until ((not (Ancestor is TThing)) or
             (((Ancestor as TThing).Position in tpSeparate) and { tpIn is ok in the case where things can be pushed in/out from/to parent }
              (not (((Ancestor as TThing).Position in tpContained) and (tfCanHaveThingsPushedIn in (Thing as TThing).GetFeatures())))));
      Result := False;
   end;

var
   Multiple, NavigateToTarget, Success: Boolean;
   SingleThingList: TThingList;
   Ancestor, SurrogateTarget: TAtom;
   LocationSurface, CurrentSubject: TThing;
   {$IFOPT C+} PreviousParent: TAtom; {$ENDIF}
   Message: AnsiString;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Assert((ThingPosition = tpIn) or (ThingPosition = tpOn));
   Multiple := Subject.Length > 1;
   NavigateToTarget := False;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         if (not Referenceable(CurrentSubject)) then
         begin
            AvatarMessage('You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.');
         end
         else
         if (Assigned(Target) and (not Referenceable(Target))) then
         begin
            AvatarMessage('You can''t see ' + Target.GetDefiniteName(Self) + ' anymore.');
         end
         else
         if (CurrentSubject.Parent = Self) then
         begin
            SingleThingList := TThingList.Create();
            try
               SingleThingList.AppendItem(CurrentSubject);
               if (Assigned(Target)) then
                  DoPut(SingleThingList, Target, ThingPosition, True)
               else
                  DoShake(SingleThingList);
            finally
               SingleThingList.Free();
            end;
         end
         else
         if (CurrentSubject = Self) then
         begin
            {$IFOPT C+} PreviousParent := FParent; {$ENDIF}
            if (Assigned(Target)) then
               NavigateToTarget := True
            else
               DoDance();
            {$IFOPT C+} Assert(FParent = PreviousParent); {$ENDIF}
         end
         else
         begin
            Ancestor := FParent;
            while ((Ancestor is TThing) and (Ancestor <> CurrentSubject)) do
               Ancestor := (Ancestor as TThing).Parent;
            if (Ancestor = CurrentSubject) then
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
                  if (CurrentSubject.Position = tpOn) then
                  begin
                     Ancestor := CurrentSubject.Parent;
                     while (Ancestor is TThing) do
                        Ancestor := (Ancestor as TThing).Parent;
                     LocationSurface := Ancestor.GetSurface() as TThing;
                     if ((CurrentSubject.Parent = Ancestor) or
                         (CurrentSubject.Parent = LocationSurface)) then
                     begin
                        SurrogateTarget := CurrentSubject.Parent;
                     end
                     else
                     begin
                        Assert(CurrentSubject.Parent is TThing);
                        SurrogateTarget := (CurrentSubject.Parent as TThing).Parent;
                     end;
                  end
                  else
                     SurrogateTarget := CurrentSubject.Parent
               end
               else
                  SurrogateTarget := Target;
               if ((SurrogateTarget = CurrentSubject.Parent) and (ThingPosition = CurrentSubject.Position)) then
               begin
                  if (not Assigned(Target)) then
                  begin
                     SingleThingList := TThingList.Create();
                     try
                        SingleThingList.AppendItem(CurrentSubject);
                        DoShake(SingleThingList);
                     finally
                        SingleThingList.Free();
                     end;
                  end
                  else
                  begin
                     AvatarMessage(Capitalise(CurrentSubject.GetDefiniteName(Self) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' already ' + ThingPositionToString(CurrentSubject.Position) + ' ' + Target.GetDefiniteName(Self) + '.'));
                  end;
               end
               else
               if (((ThingPosition = tpOn) and (not CanBePushedTo(CurrentSubject, SurrogateTarget))) or
                   ((ThingPosition = tpIn) and (not CanBePushedInto(CurrentSubject, SurrogateTarget)))) then
               begin
                  { can't be pushed }
                  { if the target was explicit, then try putting it there, otherwise, just shake it }
                  SingleThingList := TThingList.Create();
                  try
                     SingleThingList.AppendItem(CurrentSubject);
                     if (Assigned(Target)) then
                        DoPut(SingleThingList, SurrogateTarget, ThingPosition, True)
                     else
                        DoShake(SingleThingList);
                  finally
                     SingleThingList.Free();
                  end;
               end
               else
               if (SurrogateTarget = CurrentSubject) then
               begin
                  AvatarMessage('You can''t move something ' + ThingPositionToDirectionString(ThingPosition) + ' itself, however hard you try.');
               end
               else
               begin
                  { if we get here then we have a target that makes sense }
                  Message := 'Moved ' + ThingPositionToDirectionString(ThingPosition) + ' ' + SurrogateTarget.GetDefiniteName(Self) + '.';
                  Success := CurrentSubject.CanMove(Self, Message) and CanPush(CurrentSubject, Message) and SurrogateTarget.CanPut(CurrentSubject, ThingPosition, Self, Message);
                  AvatarMessage(Message);
                  if (Success) then
                     SurrogateTarget.Put(CurrentSubject, ThingPosition, True, Self);
               end;
            end;
         end;
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

procedure TPlayer.DoPush(Subject: TThingList; Direction: TCardinalDirection);
var
   Multiple, Success: Boolean;
   CurrentSubject: TThing;
   Destination, CurrentNotifiee: TAtom;
   Location: TLocation;
   ThingPosition: TThingPosition;
   Message: AnsiString;
   NotificationList: TAtomList;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         if (not Referenceable(CurrentSubject)) then
         begin
            AvatarMessage('You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.');
         end
         else
         begin
            Assert(Assigned(CurrentSubject.Parent));
            Destination := CurrentSubject.Parent.GetDefaultAtom();
            Message := Capitalise(CurrentSubject.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' immovable.';
            if (CurrentSubject = Self) then
            begin
               AvatarMessage('You try to push yourself but find that a closed system cannot apply an external force on itself.');
            end
            else
            if (Destination is TLocation) then
            begin
               Location := Destination as TLocation;
               Destination := Location.GetAtomForDirection(Direction);
               if (not Assigned(Destination)) then
               begin
                  Location.FailNavigation(Direction, Self);
               end
               else
               begin
                  ThingPosition := tpOn;
                  Message := 'Pushed.';
                  NotificationList := TAtomList.Create();
                  try
                     Destination := Destination.GetEntrance(CurrentSubject, CurrentSubject.Parent, Self, ThingPosition, Message, NotificationList);
                     if (Assigned(Destination)) then
                     begin
                        Success := CurrentSubject.CanMove(Self, Message) and
                                   CanPush(CurrentSubject, Message) and
                                   Destination.CanPut(CurrentSubject, ThingPosition, Self, Message);
                        AvatarMessage(Message);
                        if (Success) then
                        begin
                           for CurrentNotifiee in NotificationList do
                              CurrentNotifiee.HandlePassedThrough(CurrentSubject, CurrentSubject.Parent, Destination, ThingPosition, Self);
                           Destination.Put(CurrentSubject, ThingPosition, False, Self);
                        end;
                     end
                     else
                     begin
                        AvatarMessage(Message);
                     end;
                  finally
                     NotificationList.Free();
                  end;
               end;
            end
            else
            if (CurrentSubject.Position = tpOn) then
            begin
               AvatarMessage('You would have to push ' + CurrentSubject.GetDefiniteName(Self) + ' off ' + CurrentSubject.Parent.GetDefiniteName(Self) + ' first.');
            end
            else
            if (CurrentSubject.Position = tpIn) then
            begin
               AvatarMessage('You would have to move ' + CurrentSubject.GetDefiniteName(Self) + ' out of ' + CurrentSubject.Parent.GetDefiniteName(Self) + ' first.');
            end
            else
            begin
               AvatarMessage(Capitalise(CurrentSubject.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' ' + ThingPositionToString(CurrentSubject.Position) + ' ' + CurrentSubject.Parent.GetDefiniteName(Self) + ' and cannot be moved.');
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoRemove(Subject: TThingList; RequiredPosition: TThingPosition; RequiredParent: TThing);
var
   Multiple, Denied, Success: Boolean;
   Destination, Ancestor: TAtom;
   DestinationPosition: TThingPosition;
   SingleThingList: TThingList;
   Message: AnsiString;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Assert((RequiredPosition = tpOn) or (RequiredPosition = tpIn));
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Assert(Assigned(CurrentSubject.Parent));
         Denied := True;
         Message := Capitalise(CurrentSubject.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' immovable.';
         if (not Referenceable(CurrentSubject)) then
         begin
            Message := 'You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.';
         end
         else
         if (Assigned(RequiredParent) and not Referenceable(RequiredParent)) then
         begin
            Message := 'You can''t see ' + RequiredParent.GetDefiniteName(Self) + ' anymore.';
         end
         else
         if (CurrentSubject = Self) then
         begin
            Message := 'You can''t just remove yourself from somewhere... where do you want to go instead?';
         end
         else
         begin
            if (Assigned(RequiredParent)) then
            begin
               if ((CurrentSubject.Parent <> RequiredParent) or (CurrentSubject.Position <> RequiredPosition)) then
                  Message := Capitalise(CurrentSubject.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' not ' + ThingPositionToString(RequiredPosition) + ' ' + RequiredParent.GetDefiniteName(Self) + ', ' + CurrentSubject.GetSubjectPronoun(Self) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' ' + ThingPositionToString(CurrentSubject.Position) + ' ' + CurrentSubject.Parent.GetDefiniteName(Self) + '.'
               else
                  Denied := False;
            end
            else
            begin
               if (CurrentSubject.Position <> RequiredPosition) then
               begin
                  Message := Capitalise(CurrentSubject.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' not ' + ThingPositionToString(RequiredPosition) + ' anything, ' + CurrentSubject.GetSubjectPronoun(Self) + ' ' + TernaryConditional('is', 'are', CurrentSubject.IsPlural(Self)) + ' ' + ThingPositionToString(CurrentSubject.Position) + ' ' + CurrentSubject.Parent.GetDefiniteName(Self) + '.';
               end
               else
               begin
                  if (RequiredPosition = tpIn) then
                  begin
                     AutoDisambiguated('out of ' + CurrentSubject.Parent.GetDefiniteName(Self));
                  end
                  else
                  begin
                     Assert(RequiredPosition = tpOn);
                     AutoDisambiguated('off ' + CurrentSubject.Parent.GetDefiniteName(Self));
                  end;
                  Denied := False;
               end;
            end;
            Denied := (not CurrentSubject.CanMove(Self, Message)) or (not CanPush(CurrentSubject, Message)) or Denied;
         end;
         if (Denied) then
         begin
            AvatarMessage(Message);
         end
         else
         begin
            {$IFOPT C+}
            if (CurrentSubject.Parent is TThing) then
               Assert(Assigned((CurrentSubject.Parent as TThing).Parent));
            {$ENDIF}
            if ((not (CurrentSubject.Parent is TThing)) or { it's in the room - only way to remove it is to pick it up }
                ((CurrentSubject.Parent as TThing).Parent.GetSurface() = CurrentSubject.Parent)) then { parent is a surface, so picking the thing up is the way to remove it }
            begin
               AutoDisambiguated('by taking ' + CurrentSubject.GetDefiniteName(Self));
               SingleThingList := TThingList.Create();
               try
                  SingleThingList.AppendItem(CurrentSubject);
                  DoTake(SingleThingList);
               finally
                  SingleThingList.Free();
               end;
            end
            else
            begin
               Ancestor := FParent;
               while ((Ancestor is TThing) and (Ancestor <> CurrentSubject)) do
                  Ancestor := (Ancestor as TThing).Parent;
               if (Ancestor = CurrentSubject) then
               begin
                  { we're (possibly indirectly) standing on it }
                  AvatarMessage('Given your current position, that would be quite difficult.');
               end
               else
               begin
                  DestinationPosition := (CurrentSubject.Parent as TThing).Position;
                  if (DestinationPosition = tpIn) then
                  begin
                     Destination := (CurrentSubject.Parent as TThing).Parent;
                  end
                  else
                  begin
                     Destination := (CurrentSubject.Parent as TThing).Parent.GetSurface();
                     DestinationPosition := tpOn;
                  end;
                  Message := 'Moved ' + ThingPositionToDirectionString(DestinationPosition) + ' ' + Destination.GetDefiniteName(Self) + '.';
                  // XXX should check that it's possible to slide the thing to the given position
                  Success := Destination.CanPut(CurrentSubject, DestinationPosition, Self, Message);
                  AvatarMessage(Message);
                  if (Success) then
                     Destination.Put(CurrentSubject, DestinationPosition, True, Self);
               end;
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoPress(Subject: TThingList);
var
   Multiple: Boolean;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         if (not Referenceable(CurrentSubject)) then
         begin
            AvatarMessage('You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.');
         end
         else
         if (CurrentSubject = Self) then
         begin
            DoBroadcast(Self, [C(M(@GetDefiniteName)), MP(Self, M(' presses '), M(' press ')), M(@GetReflexivePronoun), M('.')]);
            Press(Self);
         end
         else
         begin
            // CanReach()? (what about "press mountain", etc?) (for now it's ok since it just says "it does nothing")
            DoBroadcast([CurrentSubject, Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' presses '), M(' press ')), M(@CurrentSubject.GetDefiniteName), M('.')]);
            CurrentSubject.Press(Self);
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoShake(Subject: TThingList);
var
   Multiple: Boolean;
   Message: AnsiString;
   CurrentSubject: TThing;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         if (not Referenceable(CurrentSubject)) then
         begin
            AvatarMessage('You can''t see ' + CurrentSubject.GetDefiniteName(Self) + ' anymore.');
         end
         else
         if (CurrentSubject = Self) then
         begin
            DoBroadcast(Self, [C(M(@GetDefiniteName)), MP(Self, M(' shakes '), M(' shake ')), M(@GetReflexivePronoun), M('.')]);
            AvatarMessage('You shake yourself.');
         end
         else
         begin
            Message := 'You can''t shake ' + CurrentSubject.GetDefiniteName(Self) + '.';
            if (CanCarry(CurrentSubject, Message)) then
            begin
               { have to do broadcast as well as avatar message because broadcast won't get the context }
               DoBroadcast([CurrentSubject, Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' shakes '), M(' shake ')), M(@CurrentSubject.GetDefiniteName), M('.')]);
               AvatarMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional('shakes', 'shake', IsPlural(Self)) + ' ' + CurrentSubject.GetDefiniteName(Self) + '.');
               CurrentSubject.Shake(Self);
            end
            else
               AvatarMessage(Message);
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoDig(Target: TThing; Spade: TThing);
var
   SingleThingList: TThingList;
   Success: Boolean;
   Message: AnsiString;
begin
   Assert(Assigned(Spade));
   Assert(Assigned(Spade.Parent));
   Assert(Assigned(Target));
   if (Spade.Parent <> Self) then
   begin
      AutoDisambiguated('first taking ' + Spade.GetDefiniteName(Self));
      SingleThingList := TThingList.Create();
      try
         SingleThingList.AppendItem(Spade);
         DoTake(SingleThingList);
      finally
         SingleThingList.Free();
      end;
   end;
   if (Spade.Parent = Self) then
   begin
      Message := 'Dug.';
      Success := Spade.CanDig(Target, Self, Message) and Target.Dig(Spade, Self, Message);
      if (Success) then
         Spade.Dug(Target, Self, Message);
      AvatarMessage(Message);
   end;
end;

procedure TPlayer.DoDig(Direction: TCardinalDirection; Spade: TThing);
var
   DefaultParent, Destination: TAtom;
   CurrentLocation: TLocation;
begin
   Assert(Assigned(Spade));
   Assert(Assigned(FParent));
   DefaultParent := FParent.GetDefaultAtom();
   if (DefaultParent is TLocation) then
   begin
      CurrentLocation := DefaultParent as TLocation;
      Destination := CurrentLocation.GetAtomForDirection(Direction);
      if (not Assigned(Destination)) then
      begin
         // could give a better message?
         CurrentLocation.FailNavigation(Direction, Self);
      end
      else
      if (Destination is TThing) then
      begin
         DoDig(Destination as TThing, Spade);
      end
      else
      begin
         AvatarMessage('You cannot dig ' + Destination.GetDefiniteName(Self) + '.');
      end;
   end
   else
   begin
      if ((Direction = cdDown) and (FParent is TThing)) then
         DoDig(FParent as TThing, Spade)
      else
         AvatarMessage('You cannot dig ' + CardinalDirectionToString(Direction) + ' while you are ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Self) + '.');
   end;
end;

procedure TPlayer.DoTalk(Target: TThing; Message: AnsiString; Volume: TTalkVolume);
var
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
   if (Assigned(Target)) then
   begin
      if (Volume <> tvWhispering) then
         DoBroadcast(Self, [C(M(@GetDefiniteName)), SP, MP(Self, M(SingularVerb), M(PluralVerb)), SP, M(Message), M(' to '), M(@Target.GetDefiniteName), M('.')])
      else
      if (Target is TAvatar) then
         (Target as TAvatar).AvatarMessage(Capitalise(GetDefiniteName(Target as TAvatar)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Target as TAvatar)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Target as TAvatar) + '.');
      AvatarMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Self) + '.')
   end
   else
   begin
      DoBroadcast(Self, [C(M(@GetDefiniteName)), SP, MP(Self, M(SingularVerb), M(PluralVerb)), SP, M(Message), M('.')]);
      AvatarMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + '.');
   end;
end;

procedure TPlayer.DoDance();
begin
   // need a general emoting mechanic
   NotImplemented();
end;

procedure TPlayer.HandleAdd(Thing: TThing; Blame: TAvatar);
var
   Masses, CandidateMass, ThisMass: TThingMassManifest;
   Sizes, CandidateSize, ThisSize: TThingSizeManifest;
   Count: Cardinal;
   Child: TThing;
   Candidate: TThing;
begin
   Zero(Masses);
   Zero(Sizes);
   Count := 0;
   for Child in FChildren do
   begin
      if (Child.Position in [tpCarried]) then
      begin
         Masses := Masses + Child.GetMassManifest();
         Sizes := Sizes + Child.GetOutsideSizeManifest();
         Count := Count + 1;
      end;
   end;
   while ((Masses > MaxCarryMass) or (Sizes > MaxCarrySize) or (Count > MaxCarryCount)) do
   begin
      Assert(Assigned(FChildren));
      Zero(CandidateMass);
      Zero(CandidateSize);
      for Child in FChildren do
      begin
         ThisMass := Child.GetMassManifest();
         ThisSize := Child.GetOutsideSizeManifest();
         if ((ThisMass > CandidateMass) or (ThisSize > CandidateSize)) then
         begin
            Candidate := Child;
            CandidateMass := ThisMass;
            CandidateSize := ThisSize;
         end;
      end;
      Assert(Assigned(Candidate));
      Masses := Masses - CandidateMass;
      Sizes := Sizes - CandidateSize;
      Count := Count - 1;
      DoBroadcast([Self], nil, [C(M(@GetDefiniteName)), SP, MP(Self, M('fumbles'), M('fumble')), SP, M(@Candidate.GetDefiniteName), M('.')]);
      FParent.Put(Candidate, tpOn, False, Self);
   end;
end;

function TPlayer.CanCarry(Thing: TThing; var Message: AnsiString): Boolean;
begin
   if (Thing.GetMassManifest() > MaxCarryMass) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too heavy.';
   end
   else
   if (Thing.GetOutsideSizeManifest() > MaxCarrySize) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too big.';
   end
   else
      Result := True;
end;

function TPlayer.CanPush(Thing: TThing; var Message: AnsiString): Boolean;
begin
   if (Thing.GetMassManifest() >= MaxPushMass) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too heavy.';
   end
   else
   if (Thing.GetOutsideSizeManifest() >= MaxPushSize) then
   begin
      Result := False;
      Message := Capitalise(Thing.GetDefiniteName(Self)) + ' ' + TernaryConditional('is', 'are', Thing.IsPlural(Self)) + ' far too big.';
   end
   else
      Result := True;
end;

function TPlayer.Referenceable(Subject: TAtom): Boolean;
var
   Root: TAtom;
   FromOutside: Boolean;
begin
   Assert(Assigned(Subject));
   Assert(Assigned(FParent));
   Root := GetSurroundingsRoot(FromOutside);
   Assert(Assigned(Root));
   if (Subject is TLocation) then
      Result := Root = Subject
   else
   if (Subject is TThing) then
      Result := Root.StillReferenceable(Subject as TThing, Self, FromOutside)
   else
      raise Exception.Create('TPlayer.Referencable() does not know how to handle objects of class ' + Subject.ClassName());
end;

function TPlayer.GetImpliedThing(Scope: TAllImpliedScope; FeatureFilter: TThingFeatures): TThing;
var
   List: TThingList;
   FromOutside: Boolean;
begin
   Assert(Assigned(FParent));
   Assert((aisSelf in Scope) or (aisSurroundings in Scope));
   List := TThingList.Create();
   try
      if (aisSurroundings in Scope) then
         GetSurroundingsRoot(FromOutside).FindMatchingThings(Self, FromOutside, aisSelf in Scope, tpEverything, FeatureFilter, List)
      else
         FindMatchingThings(Self, True, aisSelf in Scope, tpEverything, FeatureFilter, List);
      if (List.Length > 0) then
      begin
         // should implement some kind of prioritisation scheme
         Result := List.First;
      end
      else
      begin
         Result := nil;
      end;
   finally
      List.Free();
   end;
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
   ChildrenCopy: TThingList;
begin
   if (FChildren.Length > 0) then
   begin
      ChildrenCopy := TThingList.Clone(FChildren);
      try
         DoPut(ChildrenCopy, FParent, tpOn, False);
      finally
         ChildrenCopy.Free();
      end;
   end;
   AnnounceDisappearance();
   inherited;
end;

function TPlayer.IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean;
var
   Word: AnsiString;
   Aborted: Boolean;
begin
   GrammaticalNumber := [];
   Word := Tokens[Start];
   Count := 1;
   if (Word = 'other') then
   begin
      Aborted := False;
      if (Perspective = Self) then
      begin
         Aborted := True;
      end
      else
      if (Start + Count >= Length(Tokens)) then
      begin
         GrammaticalNumber := [gnSingular];
         Aborted := True;
      end
      else
      begin
         Inc(Count);
         Word := Tokens[Start+Count-1];
      end;
   end
   else
   begin
      Aborted := True;
      if (Word = 'us') then
         GrammaticalNumber := [gnPlural]
      else
      if (Perspective = Self) then
      begin
         if (Word = 'me') then
            GrammaticalNumber := [gnSingular];
      end
      else
      if (Word = 'them') then
         GrammaticalNumber := [gnPlural]
      else
      if (Word = 'others') then
         GrammaticalNumber := [gnPlural]
      else
         Aborted := False;
   end;
   if (not Aborted) then
   begin
      if (Word = 'player') then
         GrammaticalNumber := [gnSingular]
      else
      if (Word = 'players') then
         GrammaticalNumber := [gnPlural]
      else
      case FGender of
         gMale:
            begin
               if ((Word = 'boy') or (Word = 'man') or (Word = 'person') or (Word = 'human') or (Word = 'male')) then
                  GrammaticalNumber := [gnSingular]
               else
               if ((Word = 'boys') or (Word = 'men') or (Word = 'persons') or (Word = 'people') or (Word = 'humans') or (Word = 'males')) then
                  GrammaticalNumber := [gnPlural];
            end;
         gFemale:
            begin
               if ((Word = 'girl') or (Word = 'woman') or (Word = 'person') or (Word = 'human') or (Word = 'female')) then
                  GrammaticalNumber := [gnSingular]
               else
               if ((Word = 'girls') or (Word = 'women') or (Word = 'persons') or (Word = 'people') or (Word = 'humans') or (Word = 'females')) then
                  GrammaticalNumber := [gnPlural];
            end;
         gThirdGender:
            begin
               if ((Word = 'person') or (Word = 'human')) then
                  GrammaticalNumber := [gnSingular]
               else
               if ((Word = 'persons') or (Word = 'people') or (Word = 'humans')) then
                  GrammaticalNumber := [gnPlural];
            end;
         gRobot:
            begin
               if ((Word = 'person') or (Word = 'robot') or (Word = 'bot')) then
                  GrammaticalNumber := [gnSingular]
               else
               if ((Word = 'persons') or (Word = 'people') or (Word = 'robots') or (Word = 'bots')) then
                  GrammaticalNumber := [gnPlural];
            end;
         gOrb:
            begin
               if ((Word = 'person') or (Word = 'orb')) then
                  GrammaticalNumber := [gnSingular]
               else
               if ((Word = 'persons') or (Word = 'people') or (Word = 'orbs')) then
                  GrammaticalNumber := [gnPlural];
            end;
         gHive:
            begin
               if (Word = 'hive') then
               begin
                  if (Start + Count >= Length(Tokens)) then
                  begin
                     GrammaticalNumber := [gnSingular];
                  end
                  else
                  begin
                     Word := Tokens[Start+Count-1];
                     if (Word = 'mind') then
                     begin
                        Inc(Count);
                        GrammaticalNumber := [gnSingular]
                     end
                     else
                     if (Word = 'minds') then
                     begin
                        Inc(Count);
                        GrammaticalNumber := [gnPlural];
                     end
                     else
                        GrammaticalNumber := [gnSingular];
                  end;
               end
               else
               if (Word = 'hives') then
                  GrammaticalNumber := [gnPlural]
               else
               if (Word = 'hive-mind') then
                  GrammaticalNumber := [gnSingular]
               else
               if (Word = 'hive-minds') then
                  GrammaticalNumber := [gnPlural];
            end;
        else
         raise EAssertionFailed.Create('Unknown gender ' + IntToStr(Cardinal(FGender)));
      end;
   end;
   Result := GrammaticalNumber <> [];
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
   RegisterStorableClass(TPlayer, 100);
   GlobalThingCollector := TThingCollector.Create();
finalization
   GlobalThingCollector.Free();
   Close(FailedCommandLog);
end.
