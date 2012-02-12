{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit locations;

interface

uses
   storable, world, grammarian;

type
   TOutdoorLocation = class(TLocation)
    protected
      FGround: TThing;
    public
      constructor Create(Surface: TThing);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetSurface(): TAtom; override;
   end;

   TFeaturelessOutdoorLocation = class(TOutdoorLocation)
    protected
      FName: AnsiString;
      FDefiniteName: AnsiString;
      FIndefiniteName: AnsiString;
      FDescription: AnsiString;
    public
      constructor Create(AName, ADefiniteName, AIndefiniteName, ADescription: AnsiString; Surface: TThing);
      destructor Destroy(); override;
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      function GetName(Perspective: TAvatar): AnsiString; override;
      function GetDefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetIndefiniteName(Perspective: TAvatar): AnsiString; override;
      function GetDescriptionSelf(Perspective: TAvatar): AnsiString; override;
   end;

   TFeaturelessStoneOutdoorLocation = class(TFeaturelessOutdoorLocation)
    public
      constructor Create(AName, ADefiniteName, AIndefiniteName, ADescription: AnsiString);
   end;

   TFeaturelessEarthOutdoorLocation = class(TFeaturelessOutdoorLocation)
    public
      constructor Create(AName, ADefiniteName, AIndefiniteName, ADescription: AnsiString);
   end;

implementation

uses
   things;

constructor TOutdoorLocation.Create(Surface: TThing);
begin
   inherited Create();
   FGround := Surface;
   Add(FGround, tpPartOfImplicit);
   SetConnectionForDirection(cdDown, FGround);
end;

destructor TOutdoorLocation.Destroy();
begin
   inherited;
end;

constructor TOutdoorLocation.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@FGround);
end;

procedure TOutdoorLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FGround);
end;

function TOutdoorLocation.GetSurface(): TAtom;
begin
   Result := FGround;
end;


constructor TFeaturelessOutdoorLocation.Create(AName, ADefiniteName, AIndefiniteName, ADescription: AnsiString; Surface: TThing);
begin
   inherited Create(Surface);
   FName := AName;
   FDefiniteName := ADefiniteName;
   FIndefiniteName := AIndefiniteName;
   FDescription := ADescription;
end;

destructor TFeaturelessOutdoorLocation.Destroy();
begin
   inherited;
end;

constructor TFeaturelessOutdoorLocation.Read(Stream: TReadStream);
begin
   inherited;
   FName := Stream.ReadAnsiString();
   FDefiniteName := Stream.ReadAnsiString();
   FIndefiniteName := Stream.ReadAnsiString();
   FDescription := Stream.ReadAnsiString();
end;

procedure TFeaturelessOutdoorLocation.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteAnsiString(FName);
   Stream.WriteAnsiString(FDefiniteName);
   Stream.WriteAnsiString(FIndefiniteName);
   Stream.WriteAnsiString(FDescription);
end;

function TFeaturelessOutdoorLocation.GetName(Perspective: TAvatar): AnsiString;
begin
   Result := FName;
end;

function TFeaturelessOutdoorLocation.GetDefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := FDefiniteName;
end;

function TFeaturelessOutdoorLocation.GetIndefiniteName(Perspective: TAvatar): AnsiString;
begin
   Result := FIndefiniteName;
end;

function TFeaturelessOutdoorLocation.GetDescriptionSelf(Perspective: TAvatar): AnsiString;
begin
   Result := FDescription;
end;


constructor TFeaturelessStoneOutdoorLocation.Create(AName, ADefiniteName, AIndefiniteName, ADescription: AnsiString);
begin
   inherited Create(AName, ADefiniteName, AIndefiniteName, ADescription,
                    TSurface.Create('ground', '(ground/grounds ((hard (stone rock)@)% surface/surfaces) rock/rocks)@', 'The ground is a flat surface of stone.'));
end;


constructor TFeaturelessEarthOutdoorLocation.Create(AName, ADefiniteName, AIndefiniteName, ADescription: AnsiString);
begin
   inherited Create(AName, ADefiniteName, AIndefiniteName, ADescription,
                    TEarthGround.Create('ground', '(ground/grounds earth)@', 'The ground is a flat surface of earth.'));
end;

initialization
   RegisterStorableClass(TFeaturelessOutdoorLocation,      2000);
   RegisterStorableClass(TFeaturelessStoneOutdoorLocation, 2001);
   RegisterStorableClass(TFeaturelessEarthOutdoorLocation, 2002);
end.