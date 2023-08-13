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
   sysutils, thingseeker, properties, typinfo, thingdim // typinfo and thingdim are used in parser.inc
   {$IFDEF DEBUG}, broadcast, textstream, typedump, arrayutils {$ENDIF}; // typedump and arrayutils are used in parser.inc

var
   FailedCommandLog: Text;
   GlobalThingCollector: TThingCollector; // from thingseeker; used in parser.inc

type
   TDirectPlayerPropertyDescriber = class(TPropertyDescriber)
    private
      FPlayer: TPlayer;
    public
      constructor Create(Player: TPlayer);
      procedure AddProperty(Name: UTF8String; PropertyType: UTF8String); override;
   end;

constructor TDirectPlayerPropertyDescriber.Create(Player: TPlayer);
begin
   FPlayer := Player;
end;      

procedure TDirectPlayerPropertyDescriber.AddProperty(Name: UTF8String; PropertyType: UTF8String);
begin
   FPlayer.SendMessage(' - ' + Name + ': ' + PropertyType);
end;

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
            else
               ; // not all verbs have allocated memory
            end;
         end;
         if (More) then
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
      end;
   end;
end;

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

   procedure DoDebugConnect(Direction: TCardinalDirection; Location: TLocation; Target: TAtom; Options: TLandmarkOptions; Bidirectional: Boolean);
   begin
      Assert(Assigned(Target));
      Assert(Assigned(Location));
      if (loPermissibleNavigationTarget in Options) then
      begin
         if (Location.HasLandmark(Direction)) then
         begin
            Fail('Cannot connect the ' + CardinalDirectionToString(Direction) + ' exit of ' + Location.GetDefiniteName(Player) + ', ' + Location.GetAtomForDirectionalNavigation(Direction).GetLongDefiniteName(Player) + ' is already in that direction.');
         end;    
      end;       
      Location.AddLandmark(Direction, Target, Options);
      if (loPermissibleNavigationTarget in Options) then
      begin
         DoBroadcast([Location], Player,
                     [C(M(@Player.GetDefiniteName)), SP, MP(Player, M('opens'), M('open')), SP,
                      M('a passage'), SP, M(CardinalDirectionToDirectionString(Direction)), SP,
                      M('leading towards'), SP, M(@Target.GetIndefiniteName), M('.')]);
         Player.SendMessage('Abracadabra! Going ' + CardinalDirectionToString(Direction) + ' from ' + Location.GetLongDefiniteName(Player) + ' now leads to ' + Target.GetLongDefiniteName(Player) + '.');
      end
      else
      begin
         DoBroadcast([Location], Player,
                     [C(M(CardinalDirectionToString(Direction))), SP, M('there'), MP(Target, M('is'), M('are')), SP,
                      M('now'), SP, M(@Target.GetIndefiniteName), M('.')]);
         Player.SendMessage('Abracadabra! ' + Capitalise(CardinalDirectionToString(Direction)) + ' of ' + Location.GetLongDefiniteName(Player) + ' there ' + IsAre(Target.IsPlural(Player)) + ' now ' + Target.GetIndefiniteName(Player) + '.');
      end;
      if (Bidirectional) then
      begin
         if (not (Target is TLocation)) then
            Fail('Cannot connect a directional exit from ' + Target.GetIndefiniteName(Player) + ', since ' + Target.GetSubjectPronoun(Player) + ' is not a location.');
         DoDebugConnect(cdReverse[Direction], Target as TLocation, Location, Options, False);
      end;
   end;
   
   procedure DoDebugMake(Data: UTF8String);
   var
      Creation: TAtom;
      Stream: TTextStream;
      Message: UTF8String;
      LocationA: TLocation;
      LocationB: TAtom;
      Direction: TCardinalDirection;
      Options: TLandmarkOptions;
      Bidirectional: Boolean;
   begin
      Assert(Length(Data) >= 2);
      Assert(Data[1] = '"');
      Assert(Data[Length(Data)] = '"');
      Stream := TTextStreamFromString.Create(Copy(Data, 2, Length(Data) - 2), @GetRegisteredAtomClass, @MakeAtomFromStream);
      try
         repeat
            if (Stream.PeekIdentifier() = 'new') then
            begin
               try
                  Creation := Stream.specialize GetObject<TAtom>();
               except
                  on Error: ETextStreamException do
                     Fail('The incantation fizzles as you hear a voice whisper "' + Error.Message + '".');
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
                  Assert(False); // Should not be possible
                  Message := 'You create ' + Creation.GetIndefiniteName(Player) + ', but then, for lack of anything better to do, ' + Creation.GetLongDefiniteName(Player) + ' vanishes.';
                  Creation.Free(); // XXX this will leave an invalid pointer in the named object table if the creation was named
                  Fail(Message);
               end;
               if (Stream.PeekToken() <> tkEndOfFile) then
                  Stream.ExpectPunctuation(';');
            end
            else
            if (Stream.GotIdentifier('connect')) then
            begin
               LocationA := Stream.specialize GetObject<TLocation>();
               Stream.ExpectPunctuation(',');
               Direction := Stream.specialize GetEnum<TCardinalDirection>();
               Stream.ExpectPunctuation(',');
               LocationB := Stream.specialize GetObject<TAtom>();
               Stream.ExpectPunctuation(',');
               Options := Stream.specialize GetSet<TLandmarkOptions>();
               if (Stream.PeekPunctuation() = ',') then
               begin
                  Stream.ExpectPunctuation(',');
                  Stream.ExpectIdentifier('bidirectional');
                  Bidirectional := True;
               end
               else
               begin
                  Bidirectional := False;
               end;
               Stream.ExpectPunctuation(';');
               DoDebugConnect(Direction, LocationA, LocationB, Options, Bidirectional);
            end
            else
            begin
               Stream.FailExpected('"new" or "connect"');
            end;
         until Stream.PeekToken() = tkEndOfFile;
      finally
         Stream.Free();
      end;
   end;

   procedure DoDebugDescribeClass(AClass: TAtomClass);
   var
      Describer: TPropertyDescriber;
   begin
      Player.SendMessage('Properties available on ' + AClass.ClassName + ':');
      Describer := TDirectPlayerPropertyDescriber.Create(Player);
      AClass.DescribeProperties(Describer);
      Describer.Free();
   end;

   procedure DoDebugDescribeEnum(AEnumTypeInfo: PTypeInfo);

      function AlignToPtr(P: Pointer): Pointer; inline;
      begin
         {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
           Result := Align(P,SizeOf(P));
         {$ELSE FPC_REQUIRES_PROPER_ALIGNMENT}
           Result := P;
         {$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
      end;

   var
      TypeData: PTypeData;
      Index: Integer;
      Current: PShortString;
   begin
      Assert(AEnumTypeInfo^.Kind = tkEnumeration);
      Player.SendMessage('Enum values available on ' + AEnumTypeInfo^.Name + ':');
      TypeData := GetTypeData(AEnumTypeInfo);
      Index := TypeData^.MinValue;
      Player.SendMessage(' - ' + TypeData^.NameList);
      Current := @TypeData^.NameList;
      while (Index < TypeData^.MaxValue) do
      begin
         Current := PShortString(AlignToPtr(Pointer(Current)+Length(Current^)+1));
         Player.SendMessage(' - ' + Current^);
         Inc(Index);
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
         'You can set your pronouns using "i use ... pronouns"; supported pronouns are he/him, she/her, it/its, ze/zer, they/them, and plural they/them.' + #10 +
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
    avLookDirectional: Player.SendMessage(Player.GetLookTowardsDirection(Player, Action.LookDirection));
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
    avUseTransportation: Player.DoNavigation(Action.UseTransportationInstruction);
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
    avPronouns: Player.DoSetPronouns(Action.Pronouns);
    {$IFDEF DEBUG}
    avDebugStatus: Player.DoDebugStatus();
    avDebugLocations: DoDebugLocations();
    avDebugLocation: Player.DoDebugLocation();
    avDebugThings: Player.DoDebugThings(Action.DebugThings);
    avDebugThing: Player.DoDebugThing(Action.DebugThing);
    avDebugTeleport: Player.DoDebugTeleport(Action.DebugTarget);
    avDebugMake: DoDebugMake(Action.DebugMakeData^);
    avDebugConnect: DoDebugConnect(Action.DebugConnectDirection, Action.DebugConnectSource, Action.DebugConnectTarget, Action.DebugConnectOptions, Action.DebugConnectBidirectional);
    avDebugListClasses: Player.DoDebugListClasses(Action.DebugSuperclass);
    avDebugDescribeClass: DoDebugDescribeClass(Action.DebugDescribeClass);
    avDebugDescribeEnum: DoDebugDescribeEnum(Action.DebugDescribeEnumTypeInfo);
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
