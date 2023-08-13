{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit locations;

interface

uses
   storable, physics, grammarian, thingdim, messages, textstream, properties;

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
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetDescriptionSelf(Perspective: TAvatar): UTF8String; override;
   end;

   TProxyLocation = class(TLocation)
    protected
      FSource: TThing;
      class function CreateFromProperties(Properties: TTextStreamProperties): TProxyLocation; override;
    public
      constructor Create(Source: TThing; Position: TThingPosition);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      class procedure DescribeProperties(Describer: TPropertyDescriber); override;
      function GetName(Perspective: TAvatar): UTF8String; override;
      function GetDefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetIndefiniteName(Perspective: TAvatar): UTF8String; override;
      function GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
   end;

{$DEFINE PART:=Interface}
{$DEFINE SUPERCLASS:=TNamedLocation}
{$INCLUDE surfacelocations.inc} // defines TSurfaceNamedLocation
{$DEFINE SUPERCLASS:=TProxyLocation}
{$INCLUDE surfacelocations.inc} // defines TSurfaceProxyLocation
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
      procedure Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar); override;
   end;

   TBackdrop = class(TProxyLocation) // @RegisterStorableClass
    // XXX should assert that nobody can enter this one except using 'debug teleport'
    public
      function GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String; override;
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

class procedure TNamedLocation.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnName, ptString);
   Describer.AddProperty(pnDefiniteName, ptString);
   Describer.AddProperty(pnIndefiniteName, ptString);
   Describer.AddProperty(pnDescription, ptString);
   Describer.AddProperty(pnLandmark, ptLandmark);
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


constructor TProxyLocation.Create(Source: TThing; Position: TThingPosition);
begin
   inherited Create();
   FSource := Source;
   Add(FSource, Position);
end;

constructor TProxyLocation.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FSource));
end;

procedure TProxyLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FSource);
end;

class function TProxyLocation.CreateFromProperties(Properties: TTextStreamProperties): TProxyLocation;
var
   Source: TThing;
   Position: TThingPosition;
   StreamedLandmarks: TStreamedLandmarks;
begin
   while (not Properties.Done) do
   begin
      if (TThing.HandleUniqueThingProperty(Properties, pnSource, Source, TThing) and {BOGUS Hint: Local variable "Source" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingPosition>(pnPosition, Position) and {BOGUS Hint: Local variable "Position" does not seem to be initialized}
          HandleLandmarkProperties(Properties, StreamedLandmarks)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnSource, pnPosition]);
   Result := Create(Source, Position);
   StreamedLandmarks.Apply(Result);
end;

class procedure TProxyLocation.DescribeProperties(Describer: TPropertyDescriber);
begin
   Describer.AddProperty(pnSource, ptThing);
   Describer.AddProperty(pnPosition, ptThingPosition);
   Describer.AddProperty(pnLandmark, ptLandmark);
end;

function TProxyLocation.GetName(Perspective: TAvatar): UTF8String;
begin
   Result := FSource.GetName(Perspective);
end;

function TProxyLocation.GetDefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := FSource.GetDefiniteName(Perspective);
end;

function TProxyLocation.GetIndefiniteName(Perspective: TAvatar): UTF8String;
begin
   Result := FSource.GetIndefiniteName(Perspective);
end;

function TProxyLocation.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := FSource.IsPlural(Perspective);
end;

function TProxyLocation.GetContextFragment(Perspective: TAvatar; PertinentPosition: TThingPosition; Context: TAtom = nil): UTF8String;
var
   Ancestor: TAtom;
begin
   Ancestor := Context;
   while (Ancestor is TThing) do
   begin
      if (Ancestor = FSource) then
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
{$DEFINE SUPERCLASS:=TProxyLocation}
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

procedure TAirLocation.Put(Thing: TThing; ThingPosition: TThingPosition; Care: TPlacementStyle; Perspective: TAvatar);
var
   Below: TAtom;
begin
   Below := GetBelow();
   Assert(Assigned(Below));
   DoBroadcastAll([Perspective, Self, Thing, Below], [C(M(@Thing.GetDefiniteName)), SP, MP(Thing, M('falls to'), M('fall to')), SP, M(@Below.GetDefiniteName)]);
   Below.Put(Thing, ThingPosition, Care, Perspective);
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


function TBackdrop.GetDescriptionRemoteDetailed(Perspective: TAvatar; Direction: TCardinalDirection; LeadingPhrase: UTF8String; Options: TLeadingPhraseOptions): UTF8String;
begin
   Result := LeadingPhrase + ', you see ' + FSource.GetDefiniteName(Perspective) + '.' +
             WithNewlineIfNotEmpty(FSource.GetBasicDescription(Perspective, psThereIsAThingThere, cdAllDirections - [ReverseCardinalDirection(Direction)]));
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
