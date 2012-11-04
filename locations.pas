{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit locations;

interface

uses
   storable, world, grammarian, thingdim;

type
   TNamedLocation = class(TLocation)
    protected
      FName: AnsiString;
      FDefiniteName: AnsiString;
      FIndefiniteName: AnsiString;
      FDescription: AnsiString;
    public
      constructor Create(Name, DefiniteName, IndefiniteName, Description: AnsiString);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
   end;

   TSlavedLocation = class(TLocation)
    protected
      FMaster: TThing;
    public
      constructor Create(Master: TThing; Position: TThingPosition);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
      function IsPlural(Perspective: TAvatar): Boolean; override;
   end;

{$DEFINE PART:=Interface}
{$DEFINE SUPERCLASS:=TNamedLocation}
{$INCLUDE surfacelocations.inc}
{$DEFINE SUPERCLASS:=TSlavedLocation}
{$INCLUDE surfacelocations.inc}
{$UNDEF SUPERCLASS}
{$UNDEF PART}


   TThresholdLocation = class(TSurfaceSlavedLocation)
    public
      constructor Create(Landmark: TThing; Surface: TThing);
      // XXXXXX make this give the landmark thing's description when the place is examined
      // XXXXXX make this fast-forward you across the landmark
   end;

   TAirLocation = class(TNamedLocation)
    // make sure this has a downwards reachable landmark if it's possible to put things in it, since they'll drop to it
    protected
      function GetBelow(): TAtom; virtual;
    public
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
      function CanInsideHold(const Manifest: TThingSizeManifest): Boolean; override;
      function CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean; override;
      procedure Put(Thing: TThing; Position: TThingPosition; Carefully: Boolean; Perspective: TAvatar); override;
   end;

   TBackdrop = class(TSlavedLocation) 
    // XXX should assert that nobody can enter this one
    public
      function GetInside(var PositionOverride: TThingPosition): TAtom; override;
   end;


   function CreateStoneSurface(): TThing;
   function CreateEarthSurface(): TThing;

implementation

uses
   things;

constructor TNamedLocation.Create(Name, DefiniteName, IndefiniteName, Description: AnsiString);
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
   FName := Stream.ReadAnsiString();
   FDefiniteName := Stream.ReadAnsiString();
   FIndefiniteName := Stream.ReadAnsiString();
   FDescription := Stream.ReadAnsiString();
end;

procedure TNamedLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FName);
   Stream.WriteAnsiString(FDefiniteName);
   Stream.WriteAnsiString(FIndefiniteName);
   Stream.WriteAnsiString(FDescription);
end;

function TNamedLocation.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := FName;
end;

function TNamedLocation.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := FDefiniteName;
end;

function TNamedLocation.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := FIndefiniteName;
end;

function TNamedLocation.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
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

function TSlavedLocation.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := FMaster.GetName(Perspective);
end;

function TSlavedLocation.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := FMaster.GetDefiniteName(Perspective);
end;

function TSlavedLocation.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := FMaster.GetIndefiniteName(Perspective);
end;

function TSlavedLocation.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FMaster.GetDescriptionSelf(Perspective);
end;

function TSlavedLocation.IsPlural(Perspective: TAvatar): Boolean;
begin
   Result := FMaster.IsPlural(Perspective);
end;


{$DEFINE PART:=Implementation}
{$DEFINE SUPERCLASS:=TNamedLocation}
{$INCLUDE surfacelocations.inc}
{$DEFINE SUPERCLASS:=TSlavedLocation}
{$INCLUDE surfacelocations.inc}
{$UNDEF SUPERCLASS}
{$UNDEF PART}


constructor TThresholdLocation.Create(Landmark: TThing; Surface: TThing);
begin
   inherited Create(Landmark, tpAt, Surface);
end;

// XXX navigation stuff


function TAirLocation.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Assert(PositionOverride = tpIn);
   PositionOverride := tpOn;
   Result := Self;
end;

function TAirLocation.CanInsideHold(const Manifest: TThingSizeManifest): Boolean;
begin
   Result := CanSurfaceHold(Manifest);
end;

function TAirLocation.CanPut(Thing: TThing; ThingPosition: TThingPosition; Perspective: TAvatar; var Message: AnsiString): Boolean;
begin
   if (ThingPosition in [tpOn, tpIn]) then
      Result := GetBelow().CanPut(Thing, tpOn, Perspective, Message)
   else
      Result := inherited; // at time of writing, this would always throw
end;

procedure TAirLocation.Put(Thing: TThing; Position: TThingPosition; Carefully: Boolean; Perspective: TAvatar);
var
   Below: TAtom;
begin
   Below := GetBelow();
   Assert(Assigned(Below));
   Perspective.AvatarMessage(Capitalise(Thing.GetDefiniteName(Perspective)) + ' falls to ' + Below.GetDefiniteName(Perspective) + '.');
   Below.Put(Thing, Position, False, Perspective);
end;

function TAirLocation.GetBelow(): TAtom;
begin
   Assert(Length(FDirectionalLandmarks[cdDown]) > 0);
   Assert(loReachable in FDirectionalLandmarks[cdDown][0].Options);
   Result := FDirectionalLandmarks[cdDown][0].Atom.GetSurface();
end;


function TBackdrop.GetInside(var PositionOverride: TThingPosition): TAtom;
begin
   Assert(PositionOverride = tpIn);
   Result := Self;
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
   RegisterStorableClass(TNamedLocation,                  2001);
   RegisterStorableClass(TSlavedLocation,                 2002);
   RegisterStorableClass(TSurfaceNamedLocation,           2003);
   RegisterStorableClass(TSurfaceSlavedLocation,          2004);
   RegisterStorableClass(TThresholdLocation,              2005);
   RegisterStorableClass(TAirLocation,                    2006);
   RegisterStorableClass(TBackdrop,                       2007);
end.