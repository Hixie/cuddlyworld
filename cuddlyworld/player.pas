{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit player;

interface

uses
   storable, physics, grammarian, messages, thingdim, thingseeker, lists;

const
   MaxCarryMass = tmPonderous; { not inclusive }
   MaxCarrySize = tsGigantic; { not inclusive }
   MaxPushMass = tmLudicrous; { not inclusive }
   MaxPushSize = tsLudicrous; { not inclusive }
   MaxShakeMass = tmLudicrous; { not inclusive }
   MaxShakeSize = tsGigantic; { not inclusive }
   MaxCarryCount = 10; { not inclusive }

type
   TGender = (gMale, gFemale, gThirdGender, gRobot, gOrb, gHive);

   TTalkVolume = (tvWhispering, tvSpeaking, tvShouting);

   TMessageEvent = procedure (Message: UTF8String) of object;
   TForceDisconnectEvent = procedure () of object;

   TPlayer = class(TAvatar) // @RegisterStorableClass
    protected
      FName, FPassword: UTF8String;
      FGender: TGender;
      FReportFailedCommands: Boolean;
      FOnMessage: TMessageEvent; { transient }
      FOnForceDisconnect: TForceDisconnectEvent; { transient }
      FContext: UTF8String; { transient }
      procedure DoFind(Subject: TThing);
      procedure DoLookUnder(Subject: TThing);
      procedure DoTake(Subject: TThingList);
      procedure DoPut(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle);
      procedure DoMove(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition);
      procedure DoPush(Subject: TThingList; Direction: TCardinalDirection);
      procedure DoRemove(Subject: TThingList; RequiredPosition: TThingPosition; RequiredParent: TThing);
      procedure DoPress(Subject: TThingList);
      procedure DoShake(Subject: TThingList);
      procedure DoDig(Target: TThing; Spade: TThing);
      procedure DoDig(Direction: TCardinalDirection; Spade: TThing);
      procedure DoOpen(Subject: TThing);
      procedure DoClose(Subject: TThing);
      procedure DoTalk(Target: TThing; Message: UTF8String; Volume: TTalkVolume);
      procedure DoDance();
      {$IFDEF DEBUG}
      procedure DoDebugStatus();
      procedure DoDebugLocation();
      procedure DoDebugThings(Things: TThingList);
      procedure DoDebugThing(Thing: TThing);
      procedure DoDebugTeleport(Target: TAtom);
      {$ENDIF}
      procedure DoHelp();
      procedure DoQuit();
      function CanCarryThing(Thing: TThing; var Message: TMessage): Boolean;
      function CanPushThing(Thing: TThing; var Message: TMessage): Boolean;
      function CanShakeThing(Thing: TThing; var Message: TMessage): Boolean;
      function Reachable(Subject: TAtom; out Message: TMessage): Boolean;
      function GetImpliedThing(Scope: TAllImpliedScope; FeatureFilter: TThingFeatures): TThing;
      procedure SetContext(Context: UTF8String);
      procedure ResetContext();
    public
      constructor Create(AName: UTF8String; APassword: UTF8String; AGender: TGender);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure Perform(Command: UTF8String); virtual;
      procedure DoLook(); override;
      procedure DoInventory();
      procedure AvatarMessage(Message: TMessage); override;
      procedure SendRawMessage(Message: UTF8String);
      procedure SendMessage(Message: UTF8String);
      procedure AutoDisambiguated(Message: UTF8String); override;
      function GetIntrinsicMass(): TThingMass; override;
      function GetIntrinsicSize(): TThingSize; override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetLongName(Perspective: TAvatar): UTF8String; override;
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetLongDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetSubjectPronoun(Perspective: TAvatar): UTF8String; override; // I
      function GetObjectPronoun(Perspective: TAvatar): UTF8String; override; // me
      function GetReflexivePronoun(Perspective: TAvatar): UTF8String; override; // myself
      function GetPossessivePronoun(Perspective: TAvatar): UTF8String; override; // mine
      function GetPossessiveAdjective(Perspective: TAvatar): UTF8String; override; // my
      function IsPlural(Perspective: TAvatar): Boolean; override;
      function GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String; override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
      procedure AnnounceAppearance(); override;
      procedure AnnounceDisappearance(); override;
      procedure AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection); override;
      procedure AnnounceDeparture(Destination: TAtom); override; {BOGUS Hint: Value parameter "Destination" is assigned but never used}
      procedure AnnounceArrival(Source: TAtom; Direction: TCardinalDirection); override; {BOGUS Hint: Value parameter "Source" is assigned but never used}
      procedure AnnounceArrival(Source: TAtom); override; {BOGUS Hint: Value parameter "Source" is assigned but never used}
      procedure HandleAdd(Thing: TThing; Blame: TAvatar); override;
      function HasConnectedPlayer(): Boolean; override;
      function IsReadyForRemoval(): Boolean; override;
      procedure RemoveFromWorld(); override;
      function IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean; override;
      function GetUsername(): UTF8String;
      function GetPassword(): UTF8String;
      procedure Adopt(AOnMessage: TMessageEvent; AOnForceDisconnect: TForceDisconnectEvent);
      procedure Abandon();
      property Gender: TGender read FGender write FGender;
      property ReportFailedCommands: Boolean read FReportFailedCommands write FReportFailedCommands;
   end;

   TPlayerList = specialize TStorableList<TPlayer>; // @RegisterStorableClass

{$IFDEF DEBUG}
type
   TStatusReportProc = procedure (Perspective: TPlayer) of object;
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
                  avTake, avPut, avMove, avPush, avRemove, avPress, avShake, avDig, avDigDirection, avOpen, avClose,
                  avTalk, avDance,
                  {$IFDEF DEBUG} avDebugStatus, avDebugLocation, avDebugThings, avDebugThing, avDebugTeleport, {$ENDIF}
                  avHelp, avQuit);

   PTalkMessage = ^TTalkMessage;
   TTalkMessage = record
      Message: UTF8String;
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
      avPut: (PutSubject: TThingList; PutTarget: TAtom; PutPosition: TThingPosition; PutCare: TPlacementStyle);
      avMove: (MoveSubject: TThingList; MoveTarget: TAtom; MovePosition: TThingPosition);
      avPush: (PushSubject: TThingList; PushDirection: TCardinalDirection);
      avRemove: (RemoveSubject: TThingList; RemoveFromPosition: TThingPosition; RemoveFromObject: TThing);
      avPress: (PressSubject: TThingList);
      avShake: (ShakeSubject: TThingList);
      avDig {and avDigDirection}: (DigSpade: TThing; case TActionVerb of avDig: (DigTarget: TThing); avDigDirection: (DigDirection: TCardinalDirection));
      avOpen: (OpenTarget: TThing);
      avClose: (CloseTarget: TThing);
      avTalk: (TalkTarget: TThing; TalkMessage: PTalkMessage; TalkVolume: TTalkVolume);
      avDance: ();
      {$IFDEF DEBUG}
      avDebugStatus: ();
      avDebugLocation: ();
      avDebugThings: (DebugThings: TThingList);
      avDebugThing: (DebugThing: TThing);
      avDebugTeleport: (DebugTarget: TAtom);
      {$ENDIF}
      avHelp: ();
      avQuit: ();
   end;

var
   FailedCommandLog: Text;
   GlobalThingCollector: TThingCollector; // used in parser.inc

constructor TPlayer.Create(AName: UTF8String; APassword: UTF8String; AGender: TGender);
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
   FName := Stream.ReadString();
   FPassword := Stream.ReadString();
   FGender := TGender(Stream.ReadCardinal());
end;

procedure TPlayer.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FName);
   Stream.WriteString(FPassword);
   Stream.WriteCardinal(Cardinal(FGender));
end;

procedure TPlayer.Perform(Command: UTF8String);
var
   Tokens, OriginalTokens: TTokens;
   CurrentToken: Cardinal;

{$INCLUDE parser.inc}

   procedure ExecuteAction(var Action: TAction);
   begin
      case Action.Verb of
       avLook: DoLook();
       avLookDirectional: SendMessage(FParent.GetRepresentative().GetLookTowardsDirection(Self, Action.LookDirection));
       avLookAt: SendMessage(Action.LookAtSubject.GetLookAt(Self));
       avExamine: SendMessage(Action.ExamineSubject.GetExamine(Self));
       avRead: SendMessage(Action.ReadSubject.GetDescriptionWriting(Self));
       avLookUnder: DoLookUnder(Action.LookUnder);
       avLookIn: SendMessage(Action.LookIn.GetLookIn(Self));
       avInventory: DoInventory();
       avFind: DoFind(Action.FindSubject);
       avGo: FParent.Navigate(Action.GoDirection, Self);
       avEnter: DoNavigation(FParent, Action.EnterSubject, tpIn, Self);
       avClimbOn: DoNavigation(FParent, Action.ClimbOnSubject, tpOn, Self);
       avTake: DoTake(Action.TakeSubject);
       avPut: DoPut(Action.PutSubject, Action.PutTarget, Action.PutPosition, Action.PutCare);
       avMove: DoMove(Action.MoveSubject, Action.MoveTarget, Action.MovePosition);
       avPush: DoPush(Action.PushSubject, Action.PushDirection);
       avRemove: DoRemove(Action.RemoveSubject, Action.RemoveFromPosition, Action.RemoveFromObject);
       avPress: DoPress(Action.PressSubject);
       avShake: DoShake(Action.ShakeSubject);
       avDig: DoDig(Action.DigTarget, Action.DigSpade);
       avDigDirection: DoDig(Action.DigDirection, Action.DigSpade);
       avOpen: DoOpen(Action.OpenTarget);
       avClose: DoClose(Action.CloseTarget);
       avTalk: DoTalk(Action.TalkTarget, Action.TalkMessage^.Message, Action.TalkVolume);
       avDance: DoDance();
       {$IFDEF DEBUG}
       avDebugStatus: DoDebugStatus();
       avDebugLocation: DoDebugLocation();
       avDebugThings: DoDebugThings(Action.DebugThings);
       avDebugThing: DoDebugThing(Action.DebugThing);
       avDebugTeleport: DoDebugTeleport(Action.DebugTarget);
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
   Location: UTF8String;
begin
   try
      OriginalTokens := Tokenise(Command);
      Tokens := TokeniseCanonically(Command);
      Assert(Length(OriginalTokens) = Length(Tokens));
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
         Assert(FContext = '');
         SendRawMessage('');
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
         if (FReportFailedCommands) then
            Writeln(FailedCommandLog, '"', Command, '" for ' + Location + ': ', E.Message);
         SendRawMessage(E.Message);
         SendRawMessage('');
         Assert(FContext = '');
      end;
   end;
end;

procedure TPlayer.DoLook();
begin
   SendMessage(FParent.GetRepresentative().GetLook(Self));
end;

procedure TPlayer.DoInventory();
var
   Contents: UTF8String;
begin
   Contents := GetInventory(Self);
   if (Length(Contents) = 0) then
      Contents := 'You are not carrying anything.';
   SendMessage(Contents);
end;

{$IFDEF DEBUG}
procedure TPlayer.DoDebugStatus();
begin
   SendMessage('Debug build.');
   if (Assigned(StatusReport)) then
      StatusReport(Self);
end;

procedure TPlayer.DoDebugLocation();

   function PresenceModeToString(const PresenceMode: TGetPresenceStatementMode): UTF8String;
   begin
      case (PresenceMode) of
         psThereIsAThingHere { look }: Result := 'psThereIsAThingHere';
         psThereIsAThingThere { look north }: Result := 'psThereIsAThingThere';
         psOnThatThingIsAThing { nested look }: Result := 'psOnThatThingIsAThing';
         psTheThingIsOnThatThing { find }: Result := 'psTheThingIsOnThatThing';
         psOnThatSpecialThing { find (something far away) -- only if parent is TThing, not TLocation }: Result := 'psOnThatSpecialThing';
       else
         Assert(False);
         Result := '<error>';
      end;
   end;

var
   Location: TAtom;
   Direction: TCardinalDirection;
   PresenceMode: TGetPresenceStatementMode;
begin
   Location := Self;
   while (Assigned(Location) and (Location is TThing)) do
      Location := (Location as TThing).Parent;
   if (not Assigned(Location)) then
   begin
      SendMessage('Player is in an orphan TThing tree!');
      exit;
   end;
   if (not (Location is TLocation)) then
   begin
      SendMessage('Player is in a TThing tree that is not rooted by a TLocation!');
      exit;
   end;
   SendMessage('GetLook:' + WithNewlineIfMultiline(Location.GetLook(Self)));
   SendMessage('GetLookAt:' + WithNewlineIfMultiline(Location.GetLookAt(Self)));
   SendMessage('GetLookTowardsDirection:');
   for Direction in TCardinalDirection do
     SendMessage('  ' + CardinalDirectionToString(Direction) + ':' + WithNewlineIfMultiline(Location.GetLookTowardsDirection(Self, Direction)));
   SendMessage('GetBasicDescription:');
   for PresenceMode in [psThereIsAThingThere, psThereIsAThingHere] do
     SendMessage('  ' + PresenceModeToString(PresenceMode) + ':' + WithNewlineIfMultiline(Location.GetBasicDescription(Self, PresenceMode)));
   SendMessage('GetHorizonDescription:' + WithNewlineIfMultiline(Location.GetHorizonDescription(Self, nil)));
   SendMessage('GetDescriptionForHorizon:' + WithNewlineIfMultiline(Location.GetDescriptionForHorizon(Self, nil)));
   SendMessage('GetDescriptionSelf:' + WithNewlineIfMultiline(Location.GetDescriptionSelf(Self)));
   SendMessage('GetDescriptionState:' + WithNewlineIfMultiline(Location.GetDescriptionState(Self)));
   SendMessage('GetDescriptionHere:');
   for PresenceMode in [psThereIsAThingThere, psThereIsAThingHere] do
     SendMessage('  ' + PresenceModeToString(PresenceMode) + ':' + WithNewlineIfMultiline( Location.GetDescriptionHere(Self, PresenceMode)));
   SendMessage('GetDescriptionOn:');
   SendMessage('  []:' + WithNewlineIfMultiline(Location.GetDescriptionOn(Self, [])));
   SendMessage('  [optDeepOn]:' + WithNewlineIfMultiline(Location.GetDescriptionOn(Self, [optDeepOn])));
   SendMessage('  [optPrecise]:' + WithNewlineIfMultiline(Location.GetDescriptionOn(Self, [optPrecise])));
   SendMessage('  [optDeepOn, optPrecise]:' + WithNewlineIfMultiline(Location.GetDescriptionOn(Self, [optDeepOn, optPrecise])));
   SendMessage('GetDescriptionChildren (all options enabled):' + WithNewlineIfMultiline(Location.GetDescriptionChildren(Self, [optDeepChildren, optFar, optThorough, optOmitPerspective])));
   // these are commented out because some locations only have defined remote descriptions for certain directions
   // SendMessage('GetDescriptionRemoteBrief:');
   // for PresenceMode in [psThereIsAThingThere, psThereIsAThingHere] do
   //   SendMessage('  ' + PresenceModeToString(PresenceMode) + ':' + WithNewlineIfMultiline(Location.GetDescriptionRemoteBrief(Self, PresenceMode, cdIn)));
   // SendMessage('GetDescriptionRemoteDetailed:' + WithNewlineIfMultiline(Location.GetDescriptionRemoteDetailed(Self, cdIn)));
end;

procedure TPlayer.DoDebugThings(Things: TThingList);
var
   Thing: TThing;
   Collect, FromOutside: Boolean;
   Root: TAtom;
   FindMatchingThingsOptions: TFindMatchingThingsOptions;
begin
   Collect := not Assigned(Things);
   try
      if (Collect) then
      begin
         Root := GetSurroundingsRoot(FromOutside);
         Things := TThingList.Create();
         FindMatchingThingsOptions := [foIncludePerspectiveChildren, foIncludeNonImplicits];
         if (FromOutside) then
            Include(FindMatchingThingsOptions, foFromOutside);
         Root.FindMatchingThings(Self, FindMatchingThingsOptions, tpEverything, [], Things);
         { there's always at least one thing: us }
      end;
      Assert(Things.Length > 0);
      for Thing in Things do
         SendMessage(Thing.GetName(Self) + ': ' + Thing.GetLongDefiniteName(Self));
   finally
      if (Collect) then
         Things.Free();
   end;
end;

procedure TPlayer.DoDebugThing(Thing: TThing);
begin
   SendMessage(Thing.Debug());
end;

procedure TPlayer.DoDebugTeleport(Target: TAtom);
var
   Ancestor: TAtom;
begin
   Ancestor := Target;
   while ((Ancestor <> Self) and (Ancestor is TThing)) do
      Ancestor := (Ancestor as TThing).Parent;
   if (Ancestor = Self) then
   begin
      SendMessage('Can''t teleport an actor onto the actor itself or any of its descendants.');
   end
   else
   begin
      AnnounceDisappearance();
      Target.Add(Self, tpOn);
      AnnounceAppearance();
      DoLook();
   end;
end;
{$ENDIF}

procedure TPlayer.DoHelp();
begin
   SendMessage('Welcome to CuddlyWorld!'+ #10 +
                 'This is a pretty conventional MUD. You can move around using cardinal directions, e.g. "north", "east", "south", "west". You can shorten these to "n", "e", "s", "w". To look around, you can say "look", which can be shortened to "l". ' + 'To see what you''re holding, ask for your "inventory", which can be shortened to "i".' + #10 +
                 'More elaborate constructions are also possible. You can "take something", or "put something in something else", for instance.' + #10 +
                 'You can talk to other people by using "say", e.g. "say ''how are you?'' to Fred".' + #10 +
                 'If you find a bug, you can report it by saying "bug ''something''", for example, "bug ''the description of the camp says i can go north, but when i got north it says i cannot''". ' + 'Please be descriptive and include as much information as possible about how to reproduce the bug. Thanks!' + #10 +
                 'Have fun!');
end;

procedure TPlayer.DoQuit();
begin
   SendMessage(':-(');
end;

procedure TPlayer.SetContext(Context: UTF8String);
begin
   FContext := Context;
end;

procedure TPlayer.ResetContext();
begin
   FContext := '';
end;

procedure TPlayer.AvatarMessage(Message: TMessage);
begin
   SendMessage(Message.AsText);
end;

procedure TPlayer.SendRawMessage(Message: UTF8String);
begin
   if (Assigned(FOnMessage)) then
      FOnMessage(Message);
end;

procedure TPlayer.SendMessage(Message: UTF8String);
begin
   if (Assigned(FOnMessage)) then
   begin
      if (FContext <> '') then
      begin
         if (Pos(#10, Message) > 0) then
            Message := FContext + ':' + #10 + Message
         else
            Message := FContext + ': ' + Message;
      end;
      FOnMessage(Message);
   end;
end;

procedure TPlayer.AutoDisambiguated(Message: UTF8String);
begin
   SendMessage('(' + Message + ')');
end;

function TPlayer.GetIntrinsicMass(): TThingMass;
begin
   Result := tmPonderous;
end;

function TPlayer.GetIntrinsicSize(): TThingSize;
begin
   Result := tsMassive;
end;

function TPlayer.GetName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := Capitalise(FName);
end;

function TPlayer.GetLongName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := 'other player named ' + GetName(Perspective);
end;

function TPlayer.GetIndefiniteName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FGender of
     gMale, gFemale, gThirdGender, gRobot, gOrb: Result := Capitalise(FName);
     gHive: Result := IndefiniteArticle(FName) + ' ' + Capitalise(FName);
    else
      Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      Result := '<error>';
   end;
end;

function TPlayer.GetDefiniteName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
   case FGender of
     gMale, gFemale, gThirdGender, gRobot, gOrb: Result := Capitalise(FName);
     gHive: Result := 'The ' + Capitalise(FName);
    else
      Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      Result := '<error>';
   end;
end;

function TPlayer.GetLongDefiniteName(Perspective: TAvatar): UTF8String;
begin
   if (Perspective = Self) then
      Result := 'you'
   else
      Result := 'the other player named ' + GetDefiniteName(Perspective);
end;

function TPlayer.GetSubjectPronoun(Perspective: TAvatar): UTF8String;
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
      Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      Result := '<error>';
   end;
end;

function TPlayer.GetObjectPronoun(Perspective: TAvatar): UTF8String;
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
      Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      Result := '<error>';
   end;
end;

function TPlayer.GetReflexivePronoun(Perspective: TAvatar): UTF8String;
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
      Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      Result := '<error>';
   end;
end;

function TPlayer.GetPossessivePronoun(Perspective: TAvatar): UTF8String;
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
      Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      Result := '<error>';
   end;
end;

function TPlayer.GetPossessiveAdjective(Perspective: TAvatar): UTF8String;
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
      Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      Result := '<error>';
   end;
end;

function TPlayer.IsPlural(Perspective: TAvatar): Boolean;
begin
   if (Perspective = Self) then
      Result := True
   else
      Result := FGender in [gHive];
end;

function TPlayer.GetPresenceStatement(Perspective: TAvatar; Mode: TGetPresenceStatementMode): UTF8String;
begin
   if (Mode = psThereIsAThingHere) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' here.'
   else
   if (Mode = psThereIsAThingThere) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' there.'
   else
   if ((Mode = psOnThatThingIsAThing) or (Mode = psTheThingIsOnThatThing)) then
      Result := Capitalise(GetDefiniteName(Perspective)) + ' ' + IsAre(IsPlural(Perspective)) + ' ' +
                           ThingPositionToString(FPosition) + ' ' + FParent.GetLongDefiniteName(Perspective) + '.'
   else
      Result := inherited;
end;

function TPlayer.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
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
         Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
         Result := '<error>';
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
         Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
         Result := '<error>';
      end;
      if (not HasConnectedPlayer) then
      begin
         case FGender of
           gMale, gFemale, gThirdGender, gHive: Result := Result + ' ' + Capitalise(GetPossessiveAdjective(Perspective)) + ' eyes look into the distance, as if ' + GetSubjectPronoun(Perspective) + ' ' + TernaryConditional('isn''t', 'aren''t', IsPlural(Perspective)) + ' really here.';
           gRobot: Result := Result + ' It appears to be currently powered down, though you see no visible means of turning it on.';
           gOrb: Result := Result + ' The light is currently quite dim.';
          else
            Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
         end;
      end;
   end;
end;

procedure TPlayer.AnnounceAppearance();
begin
   DoBroadcast([Self], Self, [C(M(@GetIndefiniteName)), SP, MP(Self, M('appears.'), M('appear.'))]);
end;

procedure TPlayer.AnnounceDisappearance();
begin
   DoBroadcast([Self], Self, [C(M(@GetIndefiniteName)), SP, MP(Self, M('disappears.'), M('disappear.'))]);
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom; Direction: TCardinalDirection);
begin
   if (Destination is TLocation) then
      DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('goes'), M('go')), SP, M(CardinalDirectionToString(Direction) + '.')])
   else
      DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('enters'), M('enter')), SP, M(@Destination.GetDefiniteName), M('.')]);
end;

procedure TPlayer.AnnounceDeparture(Destination: TAtom);
begin
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('enters'), M('enter')), SP, M(@Destination.GetDefiniteName), M('.')]);
end;

procedure TPlayer.AnnounceArrival(Source: TAtom; Direction: TCardinalDirection);
begin
   // XXX this relies on the rooms being symmetric
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('arrives'), M('arrive')), M(' from '), M(@Source.GetDefiniteName), SP, M(CardinalDirectionToDefiniteString(Direction)), M('.')]);
end;

procedure TPlayer.AnnounceArrival(Source: TAtom);
begin
   // XXX could be more intelligent by querying the current location
   // e.g. "enters from" when the current location has an exit and "arrives from" when it doesn't
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('arrives'), M('arrive')), M(' from '), M(@Source.GetDefiniteName), M('.')]);
end;

procedure TPlayer.DoFind(Subject: TThing);
var
   SubjectiveInformation: TSubjectiveInformation;
   Message, ExtraMessage: UTF8String;
   UseCommas: Boolean;
   Count, Index: Cardinal;
   Direction: TCardinalDirection;
begin
   SubjectiveInformation := Locate(Subject as TThing);
   if (SubjectiveInformation.Directions <> []) then
   begin
      Message := Capitalise(Subject.GetDefiniteName(Self)) + ' ' + IsAre(Subject.IsPlural(Self)) + ' ';
      Count := 0;
      for Direction := Low(SubjectiveInformation.Directions) to High(SubjectiveInformation.Directions) do
         if (Direction in SubjectiveInformation.Directions) then
            Inc(Count);
      Assert(Count > 0);
      UseCommas := Count > 2;
      Index := 0;
      for Direction := Low(SubjectiveInformation.Directions) to High(SubjectiveInformation.Directions) do
      begin
         if (Direction in SubjectiveInformation.Directions) then
         begin
            Inc(Index);
            if (Index > 1) then
            begin
               if (UseCommas) then
                  Message := Message + ',';
               if (Count = Index) then
                  Message := Message + ' and';
               Message := Message + ' ';
            end;
            Message := Message + CardinalDirectionToDirectionString(Direction);
         end;
      end;
      ExtraMessage := Subject.GetPresenceStatement(Self, psOnThatSpecialThing);
      if (ExtraMessage <> '') then
         Message := Message + ', ' + ExtraMessage;
      Message := Message + '.';
      SendMessage(Message);
   end
   else
      SendMessage(Subject.GetPresenceStatement(Self, psTheThingIsOnThatThing));
end;

procedure TPlayer.DoLookUnder(Subject: TThing);
var
   SubjectiveInformation: TSubjectiveInformation;
begin
   SubjectiveInformation := Locate(Subject);
   if (SubjectiveInformation.Directions = [cdUp]) then
   begin
      SendMessage('You are under ' + Subject.GetDefiniteName(Self) + '.');
   end
   else
      SendMessage(Subject.GetLookUnder(Self));
end;

procedure TPlayer.DoTake(Subject: TThingList);
var
   Multiple: Boolean;
   Success: Boolean;
   Message: TMessage;
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
         Message := TMessage.Create();
         if (not Reachable(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (CurrentSubject.Parent = Self) then
         begin
            SendMessage('You already have that.');
         end
         else
         if (CurrentSubject = Self) then
         begin
            SendMessage('You try to pick yourself up but end up on ' + FParent.GetSurface().GetDefiniteName(Self) + '.');
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
               SendMessage('Given your current position, that would be quite difficult.');
            end
            else
            begin
               Message := TMessage.Create(mkSuccess, 'Taken.');
               Success := CanCarryThing(CurrentSubject, Message);
               Assert((Message.AsKind = mkSuccess) = Success);
               SendMessage(Message.AsText);
               if (Success) then
                  Self.Put(CurrentSubject, tpCarried, psCarefully, Self);
            end;
         end;
      end;
   finally
      if (Multiple) then
         ResetContext();
   end;
end;

procedure TPlayer.DoPut(Subject: TThingList; Target: TAtom; ThingPosition: TThingPosition; Care: TPlacementStyle);
var
   Multiple, Success: Boolean;
   MessageText: UTF8String;
   Message: TMessage;
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
         Message := TMessage.Create();
         if (not Reachable(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (not Reachable(Target, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (Target = CurrentSubject) then
         begin
            SendMessage('You can''t move something ' + ThingPositionToDirectionString(ThingPosition) + ' itself, however hard you try.');
         end
         else
         begin
            Assert(Message.AsKind = mkSuccess);
            Assert(Message.AsText = '');
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
               MessageText := (Target as TThing).GetDefiniteName(Self) + ' ' + IsAre(Target.IsPlural(Self)) + ' ' + ThingPositionToString((Target as TThing).Position) + ' ';
               Ancestor := (Target as TThing).Parent;
               while (Ancestor <> CurrentSubject) do
               begin
                  Assert(Ancestor is TThing);
                  Assert(Assigned((Ancestor as TThing).Parent));
                  MessageText := MessageText + Ancestor.GetDefiniteName(Self) + ', which ' + IsAre(Ancestor.IsPlural(Self)) + ' ' + ThingPositionToString((Ancestor as TThing).Position) + ' ';
                  Ancestor := (Ancestor as TThing).Parent;
               end;
               Assert(Ancestor = CurrentSubject);
               SendMessage('That would be difficult, since ' + MessageText + Ancestor.GetDefiniteName(Self) + '.');
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
                  if (not Reachable(CurrentSubject, Message)) then
                  begin
                     Assert(Message.AsKind <> mkSuccess);
                     SendMessage(Message.AsText);
                     Success := False;
                  end
                  else
                  if (not Reachable(Target, Message)) then
                  begin
                     Assert(Message.AsKind <> mkSuccess);
                     SendMessage(Message.AsText);
                     Success := False;
                  end
                  else
                  begin
                     Assert(Message.AsKind = mkSuccess);
                     Success := CurrentSubject.Parent = Self;
                  end;
               end
               else
               begin
                  Success := True;
               end;
               if (Success) then
               begin
                  case (Care) of
                    psCarefully: MessageText := 'Placed';
                    psRoughly: MessageText := 'Dropped';
                   else
                      Assert(False);
                  end;
                  if (Target <> FParent.GetSurface()) then
                     MessageText := MessageText + ' ' + ThingPositionToString(ThingPosition) + ' ' + Target.GetDefiniteName(Self);
                  MessageText := MessageText + '.';
                  Message := TMessage.Create(mkSuccess, MessageText);
                  Success := Target.CanPut(CurrentSubject, ThingPosition, Care, Self, Message);
                  Assert((Message.AsKind = mkSuccess) = Success);
                  SendMessage(Message.AsText);
                  if (Success) then
                     Target.Put(CurrentSubject, ThingPosition, Care, Self);
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
   Ancestor, SurrogateTarget, LocationSurface: TAtom;
   CurrentSubject: TThing;
   {$IFOPT C+} PreviousParent: TAtom; {$ENDIF}
   Message: TMessage;
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
         Message := TMessage.Create();
         if (CurrentSubject.Parent = Self) then
         begin
            SingleThingList := TThingList.Create();
            try
               SingleThingList.AppendItem(CurrentSubject);
               if (Assigned(Target)) then
                  DoPut(SingleThingList, Target, ThingPosition, psCarefully)
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
         if (not Reachable(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (Assigned(Target) and (not Reachable(Target, Message))) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         begin
            Ancestor := FParent;
            while ((Ancestor is TThing) and (Ancestor <> CurrentSubject)) do
               Ancestor := (Ancestor as TThing).Parent;
            if (Ancestor = CurrentSubject) then
            begin
               { we're (possibly indirectly) standing on it }
               SendMessage('Given your current position, that would be quite difficult.');
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
                     Assert(Assigned(Ancestor));
                     LocationSurface := Ancestor.GetSurface();
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
                     SendMessage(Capitalise(CurrentSubject.GetDefiniteName(Self) + ' ' + IsAre(CurrentSubject.IsPlural(Self)) + ' already ' + ThingPositionToString(CurrentSubject.Position) + ' ' + Target.GetDefiniteName(Self) + '.'));
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
                        DoPut(SingleThingList, SurrogateTarget, ThingPosition, psCarefully)
                     else
                        DoShake(SingleThingList);
                  finally
                     SingleThingList.Free();
                  end;
               end
               else
               if (SurrogateTarget = CurrentSubject) then
               begin
                  SendMessage('You can''t move something ' + ThingPositionToDirectionString(ThingPosition) + ' itself, however hard you try.');
               end
               else
               begin
                  { if we get here then we have a target that makes sense }
                  Message := TMessage.Create(mkSuccess, 'Moved _ _.',
                                                        [ThingPositionToDirectionString(ThingPosition),
                                                         SurrogateTarget.GetDefiniteName(Self)]);
                  Success := CanPushThing(CurrentSubject, Message) and SurrogateTarget.CanPut(CurrentSubject, ThingPosition, psCarefully, Self, Message);
                  Assert((Message.AsKind = mkSuccess) = Success);
                  SendMessage(Message.AsText);
                  if (Success) then
                     SurrogateTarget.Put(CurrentSubject, ThingPosition, psCarefully, Self);
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
   CurrentSubject, DisambiguationOpening: TThing;
   Destination, CurrentNotifiee: TAtom;
   Location: TLocation;
   ThingPosition: TThingPosition;
   Message: TMessage;
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
         Message := TMessage.Create();
         if (not Reachable(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         begin
            Assert(Assigned(CurrentSubject.Parent));
            Destination := CurrentSubject.Parent.GetRepresentative();
            Message := TMessage.Create(mkImmovable, '_ _ immovable.',
                                                    [Capitalise(CurrentSubject.GetDefiniteName(Self)),
                                                     IsAre(CurrentSubject.IsPlural(Self))]);
            if (CurrentSubject = Self) then
            begin
               SendMessage('You try to push yourself but find that a closed system cannot have any unbalanced internal forces.');
            end
            else
            if (Destination is TLocation) then
            begin
               Location := Destination as TLocation;
               Destination := Location.GetAtomForDirectionalNavigation(Direction);
               if (not Assigned(Destination)) then
               begin
                  Location.FailNavigation(Direction, Self);
               end
               else
               begin
                  ThingPosition := tpOn;
                  DisambiguationOpening := nil;
                  Message := TMessage.Create(mkSuccess, 'Pushed.');
                  NotificationList := TAtomList.Create();
                  try
                     Destination := Destination.GetEntrance(CurrentSubject, Direction, Self, ThingPosition, DisambiguationOpening, Message, NotificationList);
                     if (Assigned(Destination)) then
                     begin
                        Assert(Message.AsKind = mkSuccess);
                        if (Assigned(DisambiguationOpening) and (DisambiguationOpening <> Destination)) then
                           AutoDisambiguated('through ' + DisambiguationOpening.GetDefiniteName(Self));
                        Success := CanPushThing(CurrentSubject, Message) and
                                   Destination.CanPut(CurrentSubject, ThingPosition, psRoughly, Self, Message);
                        Assert((Message.AsKind = mkSuccess) = Success);
                        SendMessage(Message.AsText);
                        if (Success) then
                        begin
                           for CurrentNotifiee in NotificationList do
                              CurrentNotifiee.HandlePassedThrough(CurrentSubject, CurrentSubject.Parent, Destination, ThingPosition, Self);
                           Destination.Put(CurrentSubject, ThingPosition, psRoughly, Self);
                        end;
                     end
                     else
                     begin
                        SendMessage(Message.AsText);
                     end;
                  finally
                     NotificationList.Free();
                  end;
               end;
            end
            else
            if (CurrentSubject.Position = tpOn) then
            begin
               SendMessage('You would have to push ' + CurrentSubject.GetDefiniteName(Self) + ' off ' + CurrentSubject.Parent.GetDefiniteName(Self) + ' first.');
            end
            else
            if (CurrentSubject.Position = tpIn) then
            begin
               SendMessage('You would have to move ' + CurrentSubject.GetDefiniteName(Self) + ' out of ' + CurrentSubject.Parent.GetDefiniteName(Self) + ' first.');
            end
            else
            begin
               SendMessage(Capitalise(CurrentSubject.GetDefiniteName(Self)) + ' ' + IsAre(CurrentSubject.IsPlural(Self)) + ' ' + ThingPositionToString(CurrentSubject.Position) + ' ' + CurrentSubject.Parent.GetDefiniteName(Self) + '. It is not clear how to move ' + CurrentSubject.GetObjectPronoun(Self) + '.');
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
   Message: TMessage;
   CurrentSubject: TThing;
begin
   // this is for "move flowers off table" and the like
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
         Message := TMessage.Create(mkImmovable, '_ _ immovable.',
                                                 [Capitalise(CurrentSubject.GetDefiniteName(Self)),
                                                  IsAre(CurrentSubject.IsPlural(Self))]);
         if (not Reachable(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            // Message set
         end
         else
         if (Assigned(RequiredParent) and (not Reachable(RequiredParent, Message))) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            // Message set
         end
         else
         if (CurrentSubject = Self) then
         begin
            Message := TMessage.Create(mkBogus, 'You can''t just remove yourself from somewhere... where do you want to go instead?');
         end
         else
         begin
            Assert(Message.AsKind = mkSuccess);
            Assert(Denied);
            // let's make these consistent now...
            if (Assigned(RequiredParent)) then
            begin
               if ((CurrentSubject.Parent <> RequiredParent) or (CurrentSubject.Position <> RequiredPosition)) then
                  // "The subject is not on the required parent, it is on the subject's parent."
                  Message := TMessage.Create(mkBogus, '_ _ not _ _, _ _ _ _.',
                                                      [Capitalise(CurrentSubject.GetDefiniteName(Self)),
                                                       IsAre(CurrentSubject.IsPlural(Self)),
                                                       ThingPositionToString(RequiredPosition), 
                                                       RequiredParent.GetDefiniteName(Self),
                                                       CurrentSubject.GetSubjectPronoun(Self),
                                                       IsAre(CurrentSubject.IsPlural(Self)),
                                                       ThingPositionToString(CurrentSubject.Position),
                                                       CurrentSubject.Parent.GetDefiniteName(Self)])
               else
                  Denied := False;
            end
            else
            begin
               if (CurrentSubject.Position <> RequiredPosition) then
               begin
                  // "The subject is not on anything, the subject is in the pot."
                  Message := TMessage.Create(mkBogus, '_ _ not _ anything, _ _ _ _.',
                                                      [Capitalise(CurrentSubject.GetDefiniteName(Self)),
                                                       IsAre(CurrentSubject.IsPlural(Self)),
                                                       ThingPositionToString(RequiredPosition),
                                                       CurrentSubject.GetSubjectPronoun(Self),
                                                       IsAre(CurrentSubject.IsPlural(Self)),
                                                       ThingPositionToString(CurrentSubject.Position),
                                                       CurrentSubject.Parent.GetDefiniteName(Self)]);
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
                  Assert(Message.AsKind = mkSuccess);
               end;
            end;
            Assert(((Message.AsKind <> mkSuccess) and (Denied)) or (Message.AsKind = mkSuccess));
            Denied := (not CanPushThing(CurrentSubject, Message)) or Denied;
            Assert(((Message.AsKind <> mkSuccess) and (Denied)) or (Message.AsKind = mkSuccess));
         end;
         if (Denied) then
         begin
            SendMessage(Message.AsText);
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
                  SendMessage('Given your current position, that would be quite difficult.');
               end
               else
               begin
                  Assert(CurrentSubject.Parent is TThing); { since we checked for this above }
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
                  Message := TMessage.Create(mkSuccess, 'Moved _ _.',
                                                        [ThingPositionToDirectionString(DestinationPosition),
                                                         Destination.GetDefiniteName(Self)]);
                  // XXX should check that it's possible to slide the thing to the given position
                  Success := Destination.CanPut(CurrentSubject, DestinationPosition, psCarefully, Self, Message);
                  Assert((Message.AsKind = mkSuccess) = Success);
                  SendMessage(Message.AsText);
                  if (Success) then
                     Destination.Put(CurrentSubject, DestinationPosition, psCarefully, Self);
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
   Message: TMessage;
begin
   Assert(Assigned(Subject));
   Assert(Subject.Length > 0);
   Multiple := Subject.Length > 1;
   try
      for CurrentSubject in Subject do
      begin
         if (Multiple) then
            SetContext(Capitalise(CurrentSubject.GetName(Self)));
         Message := TMessage.Create();
         if (not Reachable(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (CurrentSubject = Self) then
         begin
            DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' presses '), M(' press ')), M(@GetReflexivePronoun), M('.')]);
            Press(Self);
         end
         else
         begin
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
   Message: TMessage;
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
         Message := TMessage.Create();
         if (not Reachable(CurrentSubject, Message)) then
         begin
            Assert(Message.AsKind <> mkSuccess);
            SendMessage(Message.AsText);
         end
         else
         if (CurrentSubject = Self) then
         begin
            DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' shakes '), M(' shake ')), M(@GetReflexivePronoun), M('.')]);
            SendMessage('You shake yourself.');
         end
         else
         begin
            Message := TMessage.Create(mkImmovable, 'You can''t shake _.', [CurrentSubject.GetDefiniteName(Self)]);
            if (CanShakeThing(CurrentSubject, Message)) then
            begin
               { have to do broadcast as well as avatar message because broadcast won't get the context }
               DoBroadcast([CurrentSubject, Self], Self, [C(M(@GetDefiniteName)), MP(Self, M(' shakes '), M(' shake ')), M(@CurrentSubject.GetDefiniteName), M('.')]);
               SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional('shakes', 'shake', IsPlural(Self)) + ' ' + CurrentSubject.GetDefiniteName(Self) + '.');
               CurrentSubject.Shake(Self);
            end
            else
            begin
               Assert(Message.AsKind <> mkSuccess);
               SendMessage(Message.AsText);
            end;
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
   Message: TMessage;
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
      Message := TMessage.Create(mkSuccess, 'Dug.');
      Success := Spade.CanDig(Target, Self, Message) and Target.Dig(Spade, Self, Message);
      Assert((Message.AsKind = mkSuccess) = Success);
      if (Success) then
         Spade.Dug(Target, Self, Message);
      SendMessage(Message.AsText);
   end;
end;

procedure TPlayer.DoDig(Direction: TCardinalDirection; Spade: TThing);
var
   DefaultParent, Destination: TAtom;
   CurrentLocation: TLocation;
begin
   Assert(Assigned(Spade));
   Assert(Assigned(FParent));
   DefaultParent := FParent.GetRepresentative();
   if (DefaultParent is TLocation) then
   begin
      CurrentLocation := DefaultParent as TLocation;
      Destination := CurrentLocation.GetAtomForDirectionalNavigation(Direction);
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
         SendMessage('You cannot dig ' + Destination.GetDefiniteName(Self) + '.'); // XXX "from here", maybe?
      end;
   end
   else
   begin
      if ((Direction = cdDown) and (FParent is TThing)) then
         DoDig(FParent as TThing, Spade)
      else
         SendMessage('You cannot dig ' + CardinalDirectionToString(Direction) + ' while you are ' + ThingPositionToString(FPosition) + ' ' + FParent.GetDefiniteName(Self) + '.');
   end;
end;

procedure TPlayer.DoOpen(Subject: TThing);
var
   Message: TMessage;
begin
   Message := TMessage.Create(mkSuccess, 'Opened.');
   Subject.Open(Self, Message);
   SendMessage(Message.AsText);
end;

procedure TPlayer.DoClose(Subject: TThing);
var
   Message: TMessage;
begin
   Message := TMessage.Create(mkSuccess, 'Closed.');
   Subject.Close(Self, Message);
   SendMessage(Message.AsText);
end;

procedure TPlayer.DoTalk(Target: TThing; Message: UTF8String; Volume: TTalkVolume);
var
   SingularVerb, PluralVerb: UTF8String;
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
      // XXX notify NPCs
      if (Volume <> tvWhispering) then
         DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M(SingularVerb), M(PluralVerb)), SP, M(Message), M(' to '), M(@Target.GetDefiniteName), M('.')])
      else
      if (Target is TPlayer) then
         (Target as TPlayer).SendMessage(Capitalise(GetDefiniteName(Target as TAvatar)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Target as TAvatar)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Target as TAvatar) + '.');
      SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + ' to ' + Target.GetDefiniteName(Self) + '.')
   end
   else
   begin
      DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M(SingularVerb), M(PluralVerb)), SP, M(Message), M('.')]);
      SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional(SingularVerb, PluralVerb, IsPlural(Self)) + ' ' + Message + '.');
   end;
end;

procedure TPlayer.DoDance();
begin
//   XXX; // need a general emoting mechanic
   DoBroadcast([Self], Self, [C(M(@GetDefiniteName)), SP, MP(Self, M('dances'), M('dance')), M('.')]);
   SendMessage(Capitalise(GetDefiniteName(Self)) + ' ' + TernaryConditional('dances', 'dance', IsPlural(Self)) + '.');
end;

procedure TPlayer.HandleAdd(Thing: TThing; Blame: TAvatar);
var
   Masses, CandidateMass, ThisMass: TThingMassManifest;
   Sizes, CandidateSize, ThisSize: TThingSizeManifest;
   Count: Cardinal;
   Child: TThing;
   Candidate: TThing;
   {$IFOPT C+} Message: TMessage; {$ENDIF}
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
         Assert(Count < High(Count)); // this would be a ludicrous number of objects
         Count := Count + 1; // $R-
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
      Assert(MaxCarryCount >= 0);
      Count := Count - 1; // can't go negative since Count > MaxCarryCount and MaxCarryCount >= 0 // $R-
      DoBroadcast([Self], nil, [C(M(@GetDefiniteName)), SP, MP(Self, M('fumbles'), M('fumble')), SP, M(@Candidate.GetDefiniteName), M('.')]);
      {$IFOPT C+}
        Message.Create();
        Assert(FParent.CanPut(Candidate, tpOn, psRoughly, Self, Message));
        Assert(Message.AsKind = mkSuccess);
      {$ENDIF};
      FParent.Put(Candidate, tpOn, psRoughly, Self);
   end;
end;

function TPlayer.CanCarryThing(Thing: TThing; var Message: TMessage): Boolean;
begin
   if (not Thing.CanTake(Self, Message)) then
   begin
      Result := False;
   end
   else
   if (Thing.GetMassManifest() > MaxCarryMass) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooHeavy, '_ _ far too heavy.',
                                             [Capitalise(Thing.GetDefiniteName(Self)),
                                              IsAre(Thing.IsPlural(Self))]);
   end
   else
   if (Thing.GetOutsideSizeManifest() > MaxCarrySize) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooBig, '_ _ far too big.',
                                           [Capitalise(Thing.GetDefiniteName(Self)),
                                            IsAre(Thing.IsPlural(Self))]);
   end
   else
   begin
      Result := True;
   end;
end;

function TPlayer.CanPushThing(Thing: TThing; var Message: TMessage): Boolean;
begin
   if (not Thing.CanMove(Self, Message)) then
   begin
      Result := False;
   end
   else
   if (Thing.GetMassManifest() >= MaxPushMass) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooHeavy, '_ _ far too heavy.',
                                             [Capitalise(Thing.GetDefiniteName(Self)),
                                              IsAre(Thing.IsPlural(Self))]);
   end
   else
   if (Thing.GetOutsideSizeManifest() >= MaxPushSize) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooBig, '_ _ far too big.',
                                           [Capitalise(Thing.GetDefiniteName(Self)),
                                            IsAre(Thing.IsPlural(Self))]);
   end
   else
      Result := True;
end;

function TPlayer.CanShakeThing(Thing: TThing; var Message: TMessage): Boolean;
begin
   if (not Thing.CanShake(Self, Message)) then
   begin
      Result := False;
   end
   else
   if (Thing.GetMassManifest() >= MaxShakeMass) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooHeavy, '_ _ far too heavy.',
                                             [Capitalise(Thing.GetDefiniteName(Self)),
                                              IsAre(Thing.IsPlural(Self))]);
   end
   else
   if (Thing.GetOutsideSizeManifest() >= MaxShakeSize) then
   begin
      Result := False;
      Message := TMessage.Create(mkTooBig, '_ _ far too big',
                                           [Capitalise(Thing.GetDefiniteName(Self)),
                                            IsAre(Thing.IsPlural(Self))]);
   end
   else
      Result := True;
end;

function TPlayer.Reachable(Subject: TAtom; out Message: TMessage): Boolean;

   function GetRootFor(Atom: TAtom): TAtom;
   begin
      while (Atom is TThing) do
         Atom := (Atom as TThing).Parent;
      Result := Atom;
   end;

var
   SelfRoot: TAtom;
   FromOutside: Boolean;
   SubjectiveInformation: TSubjectiveInformation;
   Direction: TCardinalDirection;
   DirectionMessage: UTF8String;
begin
   Assert(Assigned(Subject));
   Assert(Assigned(FParent));
   SelfRoot := GetSurroundingsRoot(FromOutside);
   Assert(Assigned(SelfRoot));
   if (Subject is TLocation) then
   begin
      // XXX can this branch ever be taken?
      Assert(False, 'There''s a comment here you need to remove... (its answer is apparently yes!)');
      Result := SelfRoot = Subject;
      if (Result) then
      begin
         Message := TMessage.Create();
      end
      else
      begin
         // "at" might not work, e.g. "You aren't in Kansas anymore." needs "in" not "at"; but "The Beach" or
         // "Whole Foods" wants "at"... XXX
         Message := TMessage.Create(mkNotReachable, 'You aren''t at _ anymore.', [Subject.GetDefiniteName(Self)]);
      end;
   end
   else
   if (Subject is TThing) then
   begin
      // XXX could use Locate() but that would duplicate the call to GetSurroundingsRoot()
      Result := SelfRoot.FindThing(Subject as TThing, Self, FromOutside, SubjectiveInformation);
      if (not Result) then
      begin
         Message := TMessage.Create(mkNotReachable, 'You can''t see _ anymore.', [Subject.GetDefiniteName(Self)]);
      end
      else
      if (not (rpReachable in SubjectiveInformation.Reachable)) then
      begin
         Assert(GetRootFor(Self) <> GetRootFor(Subject));
         Result := False;
         DirectionMessage := '';
         for Direction := Low(SubjectiveInformation.Directions) to High(SubjectiveInformation.Directions) do
         begin
            if (Direction in SubjectiveInformation.Directions) then
            begin
               if (DirectionMessage <> '') then
               begin
                  // it's in more than one direction, give up trying to explain where it is
                  DirectionMessage := '';
                  Break;
               end;
               DirectionMessage := ' (' + CardinalDirectionToDirectionString(Direction) + ')';
            end;
         end;
         Message := TMessage.Create(mkNotReachable, '_ _ too far away_.',
                                                    [Capitalise(Subject.GetDefiniteName(Self)),
                                                     IsAre(Subject.IsPlural(Self)),
                                                     DirectionMessage]);
         // SelfRoot := GetRootFor(Self);
         // SubjectRoot := GetRootFor(Subject);
         // '... You are in _ but _ is in _.',
         // [..., SelfRoot.GetDefiniteName(Self), Subject.GetDefiniteName(Self), SubjectRoot.GetDefiniteName(Self)]
      end
      else
      begin
         Message := TMessage.Create();
      end;
   end
   else
      raise Exception.Create('TPlayer.Reachable() does not know how to handle objects of class ' + Subject.ClassName());
end;

function TPlayer.GetImpliedThing(Scope: TAllImpliedScope; FeatureFilter: TThingFeatures): TThing;
var
   Root: TAtom;
   List: TThingList;
   FromOutside: Boolean;
   FindMatchingThingsOptions: TFindMatchingThingsOptions;
begin
   Assert(Assigned(FParent));
   Assert((aisSelf in Scope) or (aisSurroundings in Scope));
   List := TThingList.Create();
   try
      FindMatchingThingsOptions := [];
      if (aisSelf in Scope) then
         Include(FindMatchingThingsOptions, foIncludePerspectiveChildren);
      if (aisSurroundings in Scope) then
      begin
         Root := GetSurroundingsRoot(FromOutside);
         if (FromOutside) then
            Include(FindMatchingThingsOptions, foFromOutside);
      end
      else
      begin
         Root := Self;
         Include(FindMatchingThingsOptions, foFromOutside);
      end;
      Root.FindMatchingThings(Self, FindMatchingThingsOptions, tpEverything, FeatureFilter, List);
      if (List.Length > 0) then
      begin
         // XXX should implement some kind of prioritisation scheme
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
   Assert(Assigned(FOnMessage) = Assigned(FOnForceDisconnect));
   Result := Assigned(FOnMessage);
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
         DoPut(ChildrenCopy, FParent, tpOn, psRoughly);
      finally
         ChildrenCopy.Free();
      end;
   end;
   AnnounceDisappearance();
   inherited;
end;

function TPlayer.IsExplicitlyReferencedThing(Tokens: TTokens; Start: Cardinal; Perspective: TAvatar; out Count: Cardinal; out GrammaticalNumber: TGrammaticalNumber): Boolean;
var
   Word: UTF8String;

   function Consume(const Candidate: UTF8String; out Aborted: Boolean): Boolean;
   begin
      Result := False;
      Aborted := False;
      if (Word = Candidate) then
      begin
         Result := True;
         Inc(Count);
         if (Start + Count >= Length(Tokens)) then
         begin
            Aborted := True;
         end
         else
         begin
            Word := Tokens[Start+Count];
         end;
      end;
   end;

   function ConsumeAndEnd(const Candidate: UTF8String): Boolean;
   begin
      Consume(Candidate, Result);
   end;

   function ConsumeAndEnd(const Candidate: UTF8String; const WouldBeGrammaticalNumber: TGrammaticalNumber): Boolean;
   begin
      if (Consume(Candidate, Result)) then
         GrammaticalNumber := WouldBeGrammaticalNumber;
   end;

   function ConsumeTerminal(const Candidate: UTF8String; const WouldBeGrammaticalNumber: TGrammaticalNumber): Boolean;
   begin
      if (Word = Candidate) then
      begin
         Inc(Count);
         GrammaticalNumber := WouldBeGrammaticalNumber;
         Result := True;
      end
      else
         Result := False;
   end;

   function ConsumeNonTerminal(const Candidate: UTF8String; out Aborted: Boolean): Boolean;
   begin
      Result := False;
      if (Start + Count + 1 >= Length(Tokens)) then
      begin
         Aborted := True;
      end
      else
      begin
         Aborted := False;
         if (Word = Candidate) then
         begin
            Result := True;
            Inc(Count);
            Word := Tokens[Start+Count];
         end;
      end;
   end;

   procedure InternalParse();
   var
      ReachedEnd: Boolean;
   begin
      Count := 0;
      Word := Tokens[Start];
      if (ConsumeTerminal('us', [gnPlural])) then
         Exit
      else
      if (Perspective = Self) then
      begin
         if (ConsumeTerminal('me', [gnSingular])) then
            Exit;
      end
      else
      begin
         Assert(Perspective <> Self);
         if (ConsumeAndEnd('other')) then
         begin
            GrammaticalNumber := [gnSingular];
            Exit;
         end
         else
         if (ConsumeTerminal('them', [gnPlural]) or ConsumeTerminal('others', [gnPlural])) then
            Exit;
      end;
      Assert(GrammaticalNumber = []);
      if (ConsumeAndEnd(Canonicalise(FName), [gnSingular])) then
         Exit;
      case FGender of
         gMale:
            begin
               if (ConsumeAndEnd('boy', [gnSingular]) or 
                   ConsumeAndEnd('man', [gnSingular]) or
                   ConsumeAndEnd('person', [gnSingular]) or
                   ConsumeAndEnd('human', [gnSingular]) or
                   ConsumeAndEnd('male', [gnSingular]) or
                   ConsumeAndEnd('boys', [gnPlural]) or
                   ConsumeAndEnd('men', [gnPlural]) or
                   ConsumeAndEnd('persons', [gnPlural]) or
                   ConsumeAndEnd('people', [gnPlural]) or
                   ConsumeAndEnd('humans', [gnPlural]) or
                   ConsumeAndEnd('males', [gnPlural])) then
                  Exit;
            end;
         gFemale:
            begin
               if (ConsumeAndEnd('girl', [gnSingular]) or
                   ConsumeAndEnd('woman', [gnSingular]) or
                   ConsumeAndEnd('person', [gnSingular]) or
                   ConsumeAndEnd('human', [gnSingular]) or
                   ConsumeAndEnd('female', [gnSingular]) or
                   ConsumeAndEnd('girls', [gnPlural]) or
                   ConsumeAndEnd('women', [gnPlural]) or
                   ConsumeAndEnd('persons', [gnPlural]) or
                   ConsumeAndEnd('people', [gnPlural]) or
                   ConsumeAndEnd('humans', [gnPlural]) or
                   ConsumeAndEnd('females', [gnPlural])) then
                  Exit;
            end;
         gThirdGender:
            begin
               if (ConsumeAndEnd('person', [gnSingular]) or
                   ConsumeAndEnd('human', [gnSingular]) or
                   ConsumeAndEnd('persons', [gnPlural]) or
                   ConsumeAndEnd('people', [gnPlural]) or
                   ConsumeAndEnd('humans', [gnPlural])) then
                  Exit;
            end;
         gRobot:
            begin
               if (ConsumeAndEnd('person', [gnSingular]) or
                   ConsumeAndEnd('robot', [gnSingular]) or
                   ConsumeAndEnd('bot', [gnSingular]) or
                   ConsumeAndEnd('persons', [gnPlural]) or
                   ConsumeAndEnd('people', [gnPlural]) or
                   ConsumeAndEnd('robots', [gnPlural]) or
                   ConsumeAndEnd('bots', [gnPlural])) then
                  Exit;
            end;
         gOrb:
            begin
               if (ConsumeAndEnd('orb', [gnSingular]) or
                   ConsumeAndEnd('orbs', [gnPlural])) then
                  Exit;
            end;
         gHive:
            begin
               if (Consume('hive', ReachedEnd)) then
               begin
                  if (ReachedEnd) then
                  begin
                     GrammaticalNumber := [gnSingular];
                     Exit;
                  end
                  else
                  begin
                     if (ConsumeAndEnd('minds', [gnPlural])) then
                     begin
                        Exit;
                     end
                     else
                     begin
                        GrammaticalNumber := [gnSingular];
                        if (ConsumeAndEnd('mind')) then
                           Exit;
                     end;
                  end;
               end
               else
               if (ConsumeAndEnd('hives', [gnPlural]) or
                   ConsumeAndEnd('hive-mind', [gnSingular]) or
                   ConsumeAndEnd('hive-minds', [gnPlural])) then
                  Exit;
            end;
        else
         Assert(False, 'Unknown gender ' + IntToStr(Cardinal(FGender)));
      end;
      if (GrammaticalNumber <> [gnPlural]) then
      begin
         if (ConsumeAndEnd('player', [gnSingular]) or
             ConsumeAndEnd('players', [gnPlural])) then
            Exit;
      end;
      if (ConsumeNonTerminal('named', ReachedEnd)) then
      begin
         if (ConsumeAndEnd(Canonicalise(FName), [gnSingular])) then
            Exit;
      end;
   end;

begin
   GrammaticalNumber := [];
   InternalParse();
   Result := GrammaticalNumber <> [];
end;

function TPlayer.GetUsername(): UTF8String;
begin
   Result := FName;
end;

function TPlayer.GetPassword(): UTF8String;
begin
   Result := FPassword;
end;

procedure TPlayer.Adopt(AOnMessage: TMessageEvent; AOnForceDisconnect: TForceDisconnectEvent);
begin
   Assert(Assigned(AOnMessage));
   Assert(Assigned(AOnForceDisconnect));
   if (HasConnectedPlayer()) then
   begin
      Assert(Assigned(FOnForceDisconnect));
      FOnForceDisconnect();
   end;
   FOnMessage := AOnMessage;
   FOnForceDisconnect := AOnForceDisconnect;
end;

procedure TPlayer.Abandon();
begin
   FOnMessage := nil;
   FOnForceDisconnect := nil;
end;

initialization
   Assign(FailedCommandLog, 'failed-commands.log');
   {$I-} Append(FailedCommandLog); {$I+}
   if (IOResult = 2) then
     Rewrite(FailedCommandLog);
   GlobalThingCollector := TThingCollector.Create();
{$INCLUDE registrations/player.inc}
finalization
   GlobalThingCollector.Free();
   Close(FailedCommandLog);
end.
