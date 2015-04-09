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
    public
      constructor Create(CONSTRUCTORARGUMENTDECLARATIONS Ground: TThing);
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
      procedure EnumerateObtrusiveObstacles(List: TThingList); override;
      function GetSurface(): TAtom; override;
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

procedure SUBCLASS.EnumerateObtrusiveObstacles(List: TThingList);
begin
   inherited;
   FSurface.EnumerateObtrusiveObstacles(List);
end;

function SUBCLASS.GetSurface(): TAtom;
begin
   Result := FSurface;
end;

{$ELSE}
 {$FATAL Unknown PART value}
{$ENDIF}

{$UNDEF SUBCLASS}
