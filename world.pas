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
      procedure CheckForDisconnectedPlayers();
      procedure SetDirty();
      procedure Saved();
      property Dirty: Boolean read FDirty;
   end;

implementation

uses
   sysutils, thingseeker;

var
   FailedCommandLog: Text;
   GlobalThingCollector: TThingCollector; // used in parser.inc

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
            Player.ExecuteAction(Action);
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
