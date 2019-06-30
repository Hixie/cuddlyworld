{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit world;

interface

uses
   storable, lists, physics, player, grammarian;

type
   TWorld = class(TStorable) // @RegisterStorableClass
    protected
      FLocations: TLocationList;
      FGlobalThings: TThingList;
      FPlayers: TPlayerList;
      FDirty: Boolean;
    public
      constructor Create();
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure AddLocation(Location: TLocation); { World will free these }
      procedure AddGlobalThing(GlobalThing: TThing); { World will free these }
      procedure AddPlayer(Player: TPlayer); virtual; { these must added to the world before this method is called (derived classes can override this method to do that) }
      function GetPlayer(Name: UTF8String): TPlayer;
      function GetPlayerCount(): Cardinal;
      procedure Perform(Command: UTF8String; Player: TPlayer); virtual; // overriden by tests
      procedure ExecuteAction(const Action: TAction; Player: TPlayer);
      procedure CheckForDisconnectedPlayers();
      procedure SetDirty();
      procedure Saved();
      property Dirty: Boolean read FDirty;
   end;

implementation

uses
   sysutils, thingseeker
   {$IFDEF DEBUG}, broadcast, textstream, typedump, arrayutils {$ENDIF}; // typedump and arrayutils are used in parser.inc

var
   FailedCommandLog: Text;
   GlobalThingCollector: TThingCollector; // from thingseeker; used in parser.inc

constructor TWorld.Create();
begin
   inherited;
   FLocations := TLocationList.Create([slOwner]);
   FGlobalThings := TThingList.Create([slOwner]);
   FPlayers := TPlayerList.Create();
end;

destructor TWorld.Destroy();
begin
   FLocations.Free();
   FGlobalThings.Free();
   FPlayers.Free();
   inherited;
end;

constructor TWorld.Read(Stream: TReadStream);
begin
   inherited;
   FLocations := Stream.ReadObject() as TLocationList;
   FGlobalThings := Stream.ReadObject() as TThingList;
   FPlayers := Stream.ReadObject() as TPlayerList;
end;

procedure TWorld.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteObject(FLocations);
   Stream.WriteObject(FGlobalThings);
   Stream.WriteObject(FPlayers);
end;

procedure TWorld.AddLocation(Location: TLocation);
begin
   FLocations.AppendItem(Location);
end;

procedure TWorld.AddGlobalThing(GlobalThing: TThing);
begin
   FGlobalThings.AppendItem(GlobalThing);
end;

procedure TWorld.AddPlayer(Player: TPlayer);
begin
   Assert(Assigned(Player.Parent));
   FPlayers.AppendItem(Player);
end;

function TWorld.GetPlayer(Name: UTF8String): TPlayer;
var
   Item: TPlayer;
begin
   Name := LowerCase(Name);
   for Item in FPlayers do
   begin
      Assert(Assigned(Item.Parent));
      if (LowerCase(Item.GetUsername()) = Name) then
      begin
         Result := Item;
         Exit;
      end;
   end;
   Result := nil;
end;

function TWorld.GetPlayerCount(): Cardinal;
begin
   Result := FPlayers.Length;
end;

procedure TWorld.CheckForDisconnectedPlayers();
var
   E: TPlayerList.TEnumerator;
   Item: TPlayer;
begin
   E := FPlayers.GetEnumerator();
   try
      while (E.MoveNext()) do
      begin
         Item := E.Current;
         if ((not Item.HasConnectedPlayer()) and (Item.IsReadyForRemoval())) then
         begin
            E.Remove();
            Item.RemoveFromWorld();
            Item.Free();
         end;
      end;
   finally
      E.Free();
   end;
end;

procedure TWorld.SetDirty();
begin
   FDirty := True;
end;

procedure TWorld.Saved();
begin
   FDirty := False;
end;

procedure TWorld.Perform(Command: UTF8String; Player: TPlayer);
var
   Tokens, OriginalTokens: TTokens;
   CurrentToken: Cardinal;

   {$INCLUDE parser.inc}

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
            ExecuteAction(Action, Player);
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
             avDebugMake: Dispose(Action.DebugMakeData);
             {$ENDIF}
            end;
         end;
         Player.SendRawMessage('');
      until not More;
   except
      on E: EParseError do
      begin
         Location := '"' + Player.Name + '"';
         Atom := Player.Parent;
         try
            while (Assigned(Atom)) do
            begin
               Location := '"' + Atom.GetName(Player) + '"->' + Location;
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
         Player.SendRawMessage(E.Message);
         Player.SendRawMessage('');
      end;
   end;
end;

{$IFDEF DEBUG}
function GetRegisteredClassUTF8(AClassName: UTF8String): TClass;
begin
   Result := GetRegisteredClass(AClassName);
end;
{$ENDIF}

procedure TWorld.ExecuteAction(const Action: TAction; Player: TPlayer);

   {$IFDEF DEBUG}
   procedure DoDebugLocations();
   var
      Location: TLocation;
   begin
      Player.SendMessage('Locations:');
      for Location in FLocations do
         Player.SendMessage(' - ' + Location.GetName(Player));
   end;

   procedure DoDebugMake(Data: UTF8String);
   var
      Creation: TAtom;
      Stream: TTextStream;
   begin
      Assert(Length(Data) >= 2);
      Assert(Data[1] = '"');
      Assert(Data[Length(Data)] = '"');
      Stream := TTextStreamFromString.Create(Copy(Data, 2, Length(Data) - 2), @GetRegisteredClassUTF8, @MakeAtomFromStream);
      try
         try
            Creation := Stream.specialize GetObject<TAtom>();
         except
            on Error: ETextStreamException do
               Fail('The incantation fizzles as you hear a voice whisper "' + Error.Message + '".');
         end;
      finally
         Stream.Free();
      end;
      Assert(Assigned(Creation));
      if (Creation is TLocation) then
      begin
         AddLocation(TLocation(Creation));
         Player.SendMessage('Hocus Pocus! ' + Capitalise(Creation.GetDefiniteName(Player)) + ' now exists.');
      end
      else
      if (Creation is TThing) then
      begin
         Player.Add(TThing(Creation), tpCarried);
         DoBroadcast([Player, Creation, Player], Player,
                     [C(M(@Player.GetDefiniteName)), SP,
                      MP(Player, M('manifests'), M('manifest')), SP,
                      M(@Creation.GetIndefiniteName), M('.')]);
         Player.SendMessage('Poof! ' + TThing(Creation).GetPresenceStatement(Player, psThereIsAThingHere));
      end
      else
      begin
         Creation.Free();
         Fail('');
      end;
   end;
   {$ENDIF}

   procedure DoHelp();
   begin
      Player.SendMessage(
         'Welcome to CuddlyWorld!'+ #10 +
         'This is a pretty conventional MUD. You can move around using cardinal directions, e.g. "north", "east", "south", "west". You can shorten these to "n", "e", "s", "w". To look around, you can say "look", which can be shortened to "l". ' + 'To see what you''re holding, ask for your "inventory", which can be shortened to "i".' + #10 +
         'More elaborate constructions are also possible. You can "take something", or "put something in something else", for instance.' + #10 +
         'You can talk to other people by using "say", e.g. "say ''how are you?'' to Fred".' + #10 +
         'If you find a bug, you can report it by saying "bug ''something''", for example, "bug ''the description of the camp says i can go north, but when i got north it says i cannot''". ' + 'Please be descriptive and include as much information as possible about how to reproduce the bug. Thanks!' + #10 +
         'Have fun!'
      );
   end;

   procedure DoQuit();
   begin
      Player.SendMessage(':-(');
   end;

begin
   case Action.Verb of
    avLook: Player.DoLook();
    avLookDirectional: Player.SendMessage(Player.Parent.GetRepresentative().GetLookTowardsDirection(Player, Action.LookDirection));
    avLookAt: Player.SendMessage(Action.LookAtSubject.GetLookAt(Player));
    avExamine: Player.SendMessage(Action.ExamineSubject.GetExamine(Player));
    avRead: Player.SendMessage(Action.ReadSubject.GetDescriptionWriting(Player));
    avLookUnder: Player.DoLookUnder(Action.LookUnder);
    avLookIn: Player.SendMessage(Action.LookIn.GetLookIn(Player));
    avInventory: Player.DoInventory();
    avFind: Player.DoFind(Action.FindSubject);
    avGo: Player.DoNavigation(Action.GoDirection);
    avEnter: Player.DoNavigation(Action.EnterSubject, tpIn, Action.EnterRequiredAbilities);
    avClimbOn: Player.DoNavigation(Action.ClimbOnSubject, tpOn, Action.ClimbOnRequiredAbilities);
    avTake: Player.DoTake(Action.TakeSubject);
    avPut: Player.DoPut(Action.PutSubject, Action.PutTarget, Action.PutPosition, Action.PutCare);
    avMove: Player.DoMove(Action.MoveSubject, Action.MoveTarget, Action.MoveAmbiguous, Action.MovePosition);
    avPush: Player.DoPush(Action.PushSubject, Action.PushDirection);
    avRemove: Player.DoRemove(Action.RemoveSubject, Action.RemoveFromPosition, Action.RemoveFromObject);
    avPress: Player.DoPress(Action.PressSubject);
    avShake: Player.DoShake(Action.ShakeSubject);
    avDig: Player.DoDig(Action.DigTarget, Action.DigSpade);
    avDigDirection: Player.DoDig(Action.DigDirection, Action.DigSpade);
    avOpen: Player.DoOpen(Action.OpenTarget);
    avClose: Player.DoClose(Action.CloseTarget);
    avTalk: Player.DoTalk(Action.TalkTarget, Action.TalkMessage^, Action.TalkVolume);
    avDance: Player.DoDance();
    {$IFDEF DEBUG}
    avDebugStatus: Player.DoDebugStatus();
    avDebugLocations: DoDebugLocations();
    avDebugLocation: Player.DoDebugLocation();
    avDebugThings: Player.DoDebugThings(Action.DebugThings);
    avDebugThing: Player.DoDebugThing(Action.DebugThing);
    avDebugTeleport: Player.DoDebugTeleport(Action.DebugTarget);
    avDebugMake: DoDebugMake(Action.DebugMakeData^);
    avDebugConnect: Player.DoDebugConnect(Action.DebugConnectDirection, Action.DebugConnectTarget, Action.DebugConnectOptions);
    avDebugListClasses: Player.DoDebugListClasses(Action.DebugSuperclass);
    {$ENDIF}
    avHelp: DoHelp();
    avQuit: DoQuit();
   else
    raise Exception.Create('Unknown verb in ExecuteAction(): ' + IntToStr(Ord(Action.Verb)));
   end;
end;
   
initialization
{$INCLUDE registrations/world.inc}
   Assign(FailedCommandLog, 'failed-commands.log');
   {$I-} Append(FailedCommandLog); {$I+}
   if (IOResult = 2) then
     Rewrite(FailedCommandLog);
   GlobalThingCollector := TThingCollector.Create();
finalization
   GlobalThingCollector.Free();
   Close(FailedCommandLog);
end.
