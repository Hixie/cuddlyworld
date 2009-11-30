{$MACRO ON} { -*- text -*- }
{$IFNDEF PART}
{$MODE OBJFPC}
{$INCLUDE settings.inc}
unit thingdim;

{ This is way over-complicated. }
{ We should convert all this to using integers. }
{ (It should be possible to do that without having to change any other code.) }

interface
{$DEFINE PART:=INTERFACE}

const
   kMassScaleFactor = 5;
type
   TThingMass = (tmLight, { < 5kg < } tmHeavy, { < 25kg < } tmPonderous, { < 125kg < } tmLudicrous);
   TThingMassManifest = array[TThingMass] of Cardinal;

{$DEFINE DIMENSION:=Mass} {$INCLUDE thingdim.pas} {$UNDEF DIMENSION}

const
   kSizeScaleFactor = 10;
type
   TThingSize = (tsSmall, { < 0.1m (10cm) < } tsBig, { < 1m < } tsMassive, { < 10m < } tsGigantic, { < 100m < } tsLudicrous);
   TThingSizeManifest = array[TThingSize] of Cardinal;

{$DEFINE DIMENSION:=Size} {$INCLUDE thingdim.pas} {$UNDEF DIMENSION}

type
   TThingDensity = (tdLow, tdMedium, tdHigh);
const
   kDensityMap : array[TThingDensity, TThingSize] of TThingMass =
                      ((tmLight, tmLight, tmLight, tmLight, tmLight),
                       (tmLight, tmLight, tmHeavy, tmPonderous, tmLudicrous),
                       (tmHeavy, tmPonderous, tmLudicrous, tmLudicrous, tmLudicrous));

implementation
{$DEFINE PART:=IMPLEMENTATION}

uses sysutils;

{$DEFINE DIMENSION:=Mass} {$INCLUDE thingdim.pas} {$UNDEF DIMENSION}
{$DEFINE DIMENSION:=Size} {$INCLUDE thingdim.pas} {$UNDEF DIMENSION}

{$UNDEF PART}
end.
{$ELSE}

{$IF DIMENSION=Mass}
  {$DEFINE TThingDimensionManifest:=TThingMassManifest}
  {$DEFINE TThingDimension:=TThingMass}
  {$DEFINE kDimensionScaleFactor:=kMassScaleFactor}
{$ELSEIF DIMENSION=Size}
  {$DEFINE TThingDimensionManifest:=TThingSizeManifest}
  {$DEFINE TThingDimension:=TThingSize}
  {$DEFINE kDimensionScaleFactor:=kSizeScaleFactor}
{$ELSE}
  {$FATAL Unknown DIMENSION value.}
{$ENDIF}

{$IF PART=INTERFACE}

procedure Zero(var A: TThingDimensionManifest); inline;
operator := (const A: TThingDimension): TThingDimensionManifest; inline;
operator + (const A, B: TThingDimensionManifest): TThingDimensionManifest; inline;
operator + (const A: TThingDimensionManifest; const B: TThingDimension): TThingDimensionManifest; inline;
operator - (A, B: TThingDimensionManifest): TThingDimensionManifest; inline;
operator = (const A, B: TThingDimensionManifest): Boolean; inline;
operator > (const A, B: TThingDimensionManifest): Boolean; inline;
operator >= (const A, B: TThingDimensionManifest): Boolean; inline;
operator < (const A, B: TThingDimensionManifest): Boolean; inline;
operator <= (const A, B: TThingDimensionManifest): Boolean; inline;

{$ELSEIF PART=IMPLEMENTATION}

{$IFOPT C+}
procedure AssertIsRational(const A: TThingDimensionManifest);
var
   Index: TThingDimension;
begin
   for Index := Low(TThingDimension) to Pred(High(TThingDimension)) do
      Assert(A[Index] < kDimensionScaleFactor);
end;
{$ENDIF}

procedure Zero(var A: TThingDimensionManifest);
var
   Index: TThingDimension;
begin
   for Index := Low(TThingDimension) to High(TThingDimension) do
      A[Index] := 0;
end;

procedure Rationalise (var A: TThingDimensionManifest);
var
   Index: TThingDimension;
begin
   for Index := Low(TThingDimension) to Pred(High(TThingDimension)) do
   begin
      Inc(A[Succ(Index)], A[Index] div kDimensionScaleFactor);
      A[Index] := A[Index] mod kDimensionScaleFactor;
   end;
end;

operator := (const A: TThingDimension): TThingDimensionManifest;
begin
   Zero(Result);
   Result[A] := 1;
end;

operator + (const A, B: TThingDimensionManifest): TThingDimensionManifest;
var
   Index: TThingDimension;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   {$IFOPT C+} AssertIsRational(B); {$ENDIF}
   for Index := Low(TThingDimension) to High(TThingDimension) do
      Result[Index] := A[Index] + B[Index];
   Rationalise(Result);
end;

operator + (const A: TThingDimensionManifest; const B: TThingDimension): TThingDimensionManifest;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   Result := A;
   Inc(Result[B]);
   Rationalise(Result);
end;

operator - (A, B: TThingDimensionManifest): TThingDimensionManifest;
var
   Index, Subindex, Subsubindex: TThingDimension;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   {$IFOPT C+} AssertIsRational(B); {$ENDIF}
   for Index := Low(TThingDimension) to High(TThingDimension) do
   begin
      if (A[Index] < B[Index]) then
      begin
         Assert(Index < High(TThingDimension));
         for Subindex := Succ(Index) to High(TThingDimension) do
         begin
            if (A[Subindex] > 0) then
            begin
               A[Subindex] := A[SubIndex] - 1;
               if (Pred(Subindex) >= Succ(Index)) then
               begin
                  for Subsubindex := Pred(Subindex) downto Succ(Index) do
                  begin
                     Assert(A[Subsubindex] = 0);
                     A[Subsubindex] := kDimensionScaleFactor - 1;
                  end;
               end;
               A[Index] := A[Index] + kDimensionScaleFactor;
               Break;
            end;
         end;
      end;
      Assert(A[Index] >= B[Index]);
      Result[Index] := A[Index] - B[Index];
   end;
   {$IFOPT C+} AssertIsRational(Result); {$ENDIF}
end;

operator = (const A, B: TThingDimensionManifest): Boolean;
var
   Index: TThingDimension;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   {$IFOPT C+} AssertIsRational(B); {$ENDIF}
   for Index := Low(TThingDimension) to High(TThingDimension) do
   begin
      if (A[Index] <> B[Index]) then
      begin
         Result := False;
         Exit;
      end;
   end;
   Result := True;
end;

operator > (const A, B: TThingDimensionManifest): Boolean;
var
   Index: TThingDimension;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   {$IFOPT C+} AssertIsRational(B); {$ENDIF}
   for Index := High(TThingDimension) downto Low(TThingDimension) do
   begin
      if (A[Index] > B[Index]) then
      begin
         Result := True;
         Exit;
      end
      else
      if (A[Index] < B[Index]) then
      begin
         Result := False;
         Exit;
      end;
   end;
   Result := False;
end;

operator >= (const A, B: TThingDimensionManifest): Boolean;
var
   Index: TThingDimension;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   {$IFOPT C+} AssertIsRational(B); {$ENDIF}
   for Index := High(TThingDimension) downto Low(TThingDimension) do
   begin
      if (A[Index] > B[Index]) then
      begin
         Result := True;
         Exit;
      end
      else
      if (A[Index] < B[Index]) then
      begin
         Result := False;
         Exit;
      end;
   end;
   Result := True;
end;

operator < (const A, B: TThingDimensionManifest): Boolean;
var
   Index: TThingDimension;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   {$IFOPT C+} AssertIsRational(B); {$ENDIF}
   for Index := High(TThingDimension) downto Low(TThingDimension) do
   begin
      if (A[Index] < B[Index]) then
      begin
         Result := True;
         Exit;
      end
      else
      if (A[Index] > B[Index]) then
      begin
         Result := False;
         Exit;
      end;
   end;
   Result := False;
end;

operator <= (const A, B: TThingDimensionManifest): Boolean;
var
   Index: TThingDimension;
begin
   {$IFOPT C+} AssertIsRational(A); {$ENDIF}
   {$IFOPT C+} AssertIsRational(B); {$ENDIF}
   for Index := High(TThingDimension) downto Low(TThingDimension) do
   begin
      if (A[Index] < B[Index]) then
      begin
         Result := True;
         Exit;
      end
      else
      if (A[Index] > B[Index]) then
      begin
         Result := False;
         Exit;
      end;
   end;
   Result := True;
end;

{$ELSE}
  {$FATAL Unknown PART value.}
{$ENDIF}

{$UNDEF TThingDimensionManifest}
{$UNDEF TThingDimension}
{$UNDEF kDimensionScaleFactor}

{$ENDIF}
