{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit world;

interface

uses
   storable, lists, physics, player;

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
      function GetPlayer(Name: AnsiString): TPlayer;
      function GetPlayerCount(): Cardinal;
      procedure CheckForDisconnectedPlayers();
      procedure SetDirty();
      procedure Saved();
      property Dirty: Boolean read FDirty;
   end;

implementation

uses
   sysutils;

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

function TWorld.GetPlayer(Name: AnsiString): TPlayer;
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

initialization
{$INCLUDE registrations/world.inc}
end.
