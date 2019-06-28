{$IFNDEF PART} {$FATAL surfacelocations.inc must be imported with PART defined} {$ENDIF}
{$IFNDEF SUPERCLASS} {$FATAL surfacelocations.inc must be imported with SUPERCLASS defined} {$ENDIF}

{$IF SUPERCLASS=TNamedLocation}
 {$DEFINE SUBCLASS:=TSurfaceNamedLocation}
 {$DEFINE CONSTRUCTORARGUMENTDECLARATIONS:=Name, DefiniteName, IndefiniteName, Description: UTF8String;}
 {$DEFINE CONSTRUCTORARGUMENTNAMES:=Name, DefiniteName, IndefiniteName, Description}
{$ELSEIF SUPERCLASS=TSlavedLocation}
 {$DEFINE SUBCLASS:=TSurfaceSlavedLocation}
 {$DEFINE CONSTRUCTORARGUMENTDECLARATIONS:=Master: TThing; Position: TThingPosition;}
 {$DEFINE CONSTRUCTORARGUMENTNAMES:=Master, Position}
{$ELSE}
 {$FATAL Unknown SUPERCLASS value.}
{$ENDIF}

{$IF PART=Interface}

   SUBCLASS = class(SUPERCLASS)
    protected
      FSurface: TThing;
      class function CreateFromProperties(Properties: TTextStreamProperties): SUBCLASS; override;
    public
      constructor Create(CONSTRUCTORARGUMENTDECLARATIONS Ground: TThing);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure EnumerateObtrusiveObstacles(List: TThingList); override;
      function GetSurface(): TThing; override;
   end;

{$ELSEIF PART=Implementation}

constructor SUBCLASS.Create(CONSTRUCTORARGUMENTDECLARATIONS Ground: TThing);
begin
   inherited Create(CONSTRUCTORARGUMENTNAMES);
   FSurface := Ground;
   Add(FSurface, tpPartOfImplicit);
   AddLandmark(cdDown, FSurface, [loPermissibleNavigationTarget, loConsiderDirectionUnimportantWhenFindingChildren, loNotVisibleFromBehind]);
end;

constructor SUBCLASS.Read(Stream: TReadStream);
begin
   inherited;
   Stream.ReadReference(@Pointer(FSurface));
end;

procedure SUBCLASS.Write(Stream: TWriteStream);
begin
   inherited;
   Stream.WriteReference(FSurface);
end;

class function SUBCLASS.CreateFromProperties(Properties: TTextStreamProperties): SUBCLASS;
var
   StreamedLandmarks: TStreamedLandmarks;
{$IF SUPERCLASS=TNamedLocation}
   Name: UTF8String;
   DefiniteName, IndefiniteName, Description: UTF8String;
   Ground: TThing;
begin
   while (not Properties.Done) do
   begin
      if (Properties.HandleUniqueStringProperty(pnName, Name) and
          Properties.HandleUniqueStringProperty(pnDefiniteName, DefiniteName) and
          Properties.HandleUniqueStringProperty(pnIndefiniteName, IndefiniteName) and
          Properties.HandleUniqueStringProperty(pnDescription, Description) and
          TThing.HandleUniqueThingProperty(Properties, pnGround, Ground, TThing) and {BOGUS Hint: Local variable "Ground" does not seem to be initialized}
          HandleLandmarkProperties(Properties, StreamedLandmarks)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnName, pnDefiniteName, pnIndefiniteName, pnDescription, pnGround]);
   Result := Create(Name, DefiniteName, IndefiniteName, Description, Ground);
{$ELSEIF SUPERCLASS=TSlavedLocation}
   Master: TThing;
   Position: TThingPosition;
   Ground: TThing;
begin
   while (not Properties.Done) do
   begin
      if (TThing.HandleUniqueThingProperty(Properties, pnMaster, Master, TThing) and {BOGUS Hint: Local variable "Master" does not seem to be initialized}
          Properties.specialize HandleUniqueEnumProperty<TThingPosition>(pnPosition, Position) and {BOGUS Hint: Local variable "Position" does not seem to be initialized}
          TThing.HandleUniqueThingProperty(Properties, pnGround, Ground, TThing) and {BOGUS Hint: Local variable "Ground" does not seem to be initialized}
          HandleLandmarkProperties(Properties, StreamedLandmarks)) then
       Properties.FailUnknownProperty();
   end;
   Properties.EnsureSeen([pnMaster, pnPosition, pnGround]);
   Result := Create(Master, Position, Ground);
{$ELSE}
 {$FATAL Unknown SUPERCLASS value.}
{$ENDIF}
   StreamedLandmarks.Apply(Result);
end;
      
procedure SUBCLASS.EnumerateObtrusiveObstacles(List: TThingList);
begin
   inherited;
   FSurface.EnumerateObtrusiveObstacles(List);
end;

function SUBCLASS.GetSurface(): TThing;
begin
   Result := FSurface;
end;

{$ELSE}
 {$FATAL Unknown PART value}
{$ENDIF}

{$UNDEF SUBCLASS}
