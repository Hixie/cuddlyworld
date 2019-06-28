{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit locations;

interface

uses
   storable, physics, grammarian, thingdim, messages, textstream;

type
   TNamedLocation = class(TLocation)
    protected
      FName: UTF8String;
      FDefiniteName: UTF8String;
      FIndefiniteName: UTF8String;
      FDescription: UTF8String;
      class function CreateFromProperties(Properties: TTextStreamProperties): TNamedLocation; override;
    public
      constructor Create(Name, DefiniteName, IndefiniteName, Description: UTF8String);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
   end;

   TSlavedLocation = class(TLocation)
    protected
      FMaster: TThing;
      class function CreateFromProperties(Properties: TTextStreamProperties): TSlavedLocation; override;
    public
      constructor Create(Master: TThing; Position: TThingPosition);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
   end;

{$DEFINE PART:=Interface}
{$DEFINE SUPERCLASS:=TNamedLocation}
{$INCLUDE surfacelocations.inc} // defines TSurfaceNamedLocation
{$DEFINE SUPERCLASS:=TSlavedLocation}
{$INCLUDE surfacelocations.inc} // defines TSurfaceSlavedLocation
{$UNDEF SUPERCLASS}
{$UNDEF PART}

   TGroundLocation = class(TSurfaceNamedLocation) // @RegisterStorableClass
   end;

   TAirLocation = class(TNamedLocation) // @RegisterStorableClass
    // make sure this has a downwards reachable landmark if it's possible to put things in it, since they'll drop to it
    protected
      function GetBelow(): TAtom; virtual;
    public
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean; override;
      procedure Put(Thing: TThing; Position: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar); override;
   end;

   TBackdrop = class(TSlavedLocation) // @RegisterStorableClass
    // XXX should assert that nobody can enter this one except using 'debug teleport'
   end;


   function CreateStoneSurface(): TThing;
   function CreateEarthSurface(): TThing;

implementation

uses
   things, broadcast;

constructor TNamedLocation.Create(Name, DefiniteName, IndefiniteName, Description: UTF8String);
begin
   inherited Create();
   FName := Name;
   FDefiniteName := DefiniteName;
   FIndefiniteName := IndefiniteName;
   FDescription := Description;
end;

constructor TNamedLocation.Read(Stream: TReadStream);
begin
   inherited;
   FName := Stream.ReadString();
   FDefiniteName := Stream.ReadString();
   FIndefiniteName := Stream.ReadString();
   FDescription := Stream.ReadString();
end;

procedure TNamedLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteString(FName);
   Stream.WriteString(FDefiniteName);
   Stream.WriteString(FIndefiniteName);
   Stream.WriteString(FDescription);
end;

class function TNamedLocation.CreateFromProperties(Properties: TTextStreamProperties): TNamedLocation;
var
   Name: UTF8String;
   DefiniteName, IndefiniteName, Description: UTF8String;
   StreamedLandmarks: TStreamedLandmarks;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnDefiniteName, DefiniteName) and
          Properties.HandleUniqueStringProperty(pnIndefiniteName, IndefiniteName) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          HandleLandmarkProperties(Properties, StreamedLandmarks)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnDefiniteName, pnIndefiniteName, pnDescription]);
   Result := Create(Name, DefiniteName, IndefiniteName, Description);
   StreamedLandmarks.Apply(Result);
end;

function TNamedLocation.GetName(Perspective: TAvatar): UTF8String;
begin
   Result := FName;
end;

function TNamedLocation.GetDefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := FDefiniteName;
end;

function TNamedLocation.GetIndefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := FIndefiniteName;
end;

function TNamedLocation.GetDescriptionSelf(Perspective: TAvatar): UTF8String;
begin
   Result := FDescription;
end;


constructor TSlavedLocation.Create(Master: TThing; Position: TThingPosition);
begin
   inherited Create();
   FMaster := Master;
   Add(FMaster, Position);
end;

constructor TSlavedLocation.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FMaster));
end;

procedure TSlavedLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FMaster);
end;

class function TSlavedLocation.CreateFromProperties(Properties: TTextStreamProperties): TSlavedLocation;
var
   Master: TThing;
   Position: TThingPosition;
   StreamedLandmarks: TStreamedLandmarks;
begin
   while (not Properties.Done) do
   begin
      if (TThing.HandleUniqueThingProperty(Properties, pnMaster, Master, TThing) and {BOGUS Hint: Local variable "Master" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingPosition>(pnPosition, Position) and {BOGUS Hint: Local variable "Position" does not seem to be initialized}
          HandleLandmarkProperties(Properties, StreamedLandmarks)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnMaster, pnPosition]);
   Result := Create(Master, Position);
   StreamedLandmarks.Apply(Result);
end;

function TSlavedLocation.GetName(Perspective: TAvatar): UTF8String;
begin
   Result := FMaster.GetName(Perspective);
end;

function TSlavedLocation.GetDefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := FMaster.GetDefiniteName(Perspective);
end;

function TSlavedLocation.GetIndefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := FMaster.GetIndefiniteName(Perspective);
end;

function TSlavedLocation.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := FMaster.IsPlural(Perspective);
end;

function TSlavedLocation.GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String;
var
   Ancestor: TAtom;
begin
   Ancestor := Context;
   while (Ancestor is TThing) do
   begin
      if (Ancestor = FMaster) then
      begin
         Result := '';
         exit;
      end;
      Ancestor := (Ancestor as TThing).Parent;
   end;
   Result := inherited;
end;

{$DEFINE PART:=Implementation}
{$DEFINE SUPERCLASS:=TNamedLocation}
{$INCLUDE surfacelocations.inc}
{$DEFINE SUPERCLASS:=TSlavedLocation}
{$INCLUDE surfacelocations.inc}
{$UNDEF SUPERCLASS}
{$UNDEF PART}

function TAirLocation.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := CanSurfaceHold(Manifest);
end;

function TAirLocation.CanPut(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar; var Message: TMessage): Boolean;
begin
   Assert(Message.IsValid);
   if (ThingPosition in [tpOn, tpIn]) then
      Result := GetBelow().CanPut(Thing, tpOn, Care, Perspective, Message)
   else
      Result := inherited; // at time of writing, this would always throw, since the superclass asserts that ThingPosition is tpOn or tpIn
end;

procedure TAirLocation.Put(Thing: TThing; Position: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar);
var
   Below: TAtom;
begin
   Below := GetBelow();
   Assert(Assigned(Below));
   DoBroadcastAll([Perspective, Self, Thing, Below], [C(M(@Thing.GetDefiniteName)), SP, MP(Thing, M('falls to'), M('fall to')), SP, M(@Below.GetDefiniteName)]);
   Below.Put(Thing, Position, Care, Perspective);
end;

function TAirLocation.GetBelow(): TAtom;
begin
   Assert(Length(FDirectionalLandmarks[cdDown]) > 0);
   Assert(loPermissibleNavigationTarget in FDirectionalLandmarks[cdDown][0].Options);
   Result := FDirectionalLandmarks[cdDown][0].Atom.GetSurface();
   if (not Assigned(Result)) then
      Result := FDirectionalLandmarks[cdDown][0].Atom;
   Assert(Assigned(Result));
end;


function CreateStoneSurface(): TThing;
begin
   Result := TSurface.Create('ground', '(ground/grounds ((hard (stone rock)@)% surface/surfaces) rock/rocks)@', 'The ground is a flat surface of stone.');
end;

function CreateEarthSurface(): TThing;
begin
   Result := TEarthGround.Create('ground', '(ground/grounds earth)@', 'The ground is a flat surface of earth.');
end;

initialization
{$INCLUDE registrations/locations.inc}
end.
