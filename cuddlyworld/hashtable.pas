{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit hashtable;

interface

type
   generic THashTable <TKey, TValue> = class
    protected
     type
      PHashTableEntry = ^THashTableEntry;
      THashTableEntry = record
        Key: TKey;
        Value: TValue;
        Next: PHashTableEntry;
      end;
      THashFunction = function (Key: TKey): Cardinal;
     var
      FTable: array of PHashTableEntry;
      FZero: TValue;
      FCount: Cardinal;
      FHashFunction: THashFunction;
      procedure DoubleSize();
      procedure InternalAdd(var Table: array of PHashTableEntry; Key: TKey; Value: TValue);
    public
      constructor Create(AHashFunction: THashFunction);
      constructor Create(AHashFunction: THashFunction; APredictedCount: Cardinal);
      destructor Destroy(); override;
      procedure Add(Key: TKey; Value: TValue);
      function Get(Key: TKey): TValue;
      procedure Histogram(var F: Text);
   end;

function Integer32toHash32(Key: Cardinal): Cardinal; inline;
function Integer64toHash32(Key: QWord): Cardinal; inline;
function PtrUIntHash(Key: PtrUInt): Cardinal; inline;
function PointerHash(Key: Pointer): Cardinal; inline;

implementation

uses
   sysutils;

function Integer32toHash32(Key: Cardinal): Cardinal;
begin
   Assert(SizeOf(Cardinal) * 8 = 32);
   Result := Key;
   {$IFOPT Q+}
     {$DEFINE overflow_checks_on}
     {$OVERFLOWCHECKS OFF}
   {$ENDIF}
   { Robert Jenkins 32bit Integer Hash - http://burtleburtle.net/bob/hash/integer.html }
   Result := (Result  +  $7ed55d16)  +  (Result shl 12);
   Result := (Result xor $c761c23c) xor (Result shr 19);
   Result := (Result  +  $165667b1)  +  (Result shl  5);
   Result := (Result  +  $d3a2646c) xor (Result shl  9);
   Result := (Result  +  $fd7046c5)  +  (Result shl  3);
   Result := (Result xor $b55a4f09) xor (Result shr 16);
   {$IFDEF overflow_checks_on}
     {$OVERFLOWCHECKS ON}
     {$UNDEF overflow_checks_on}
   {$ENDIF}
end;

function Integer64toHash32(Key: QWord): Cardinal;
begin
   Assert(SizeOf(QWord) * 8 = 64);
   {$IFOPT Q+}
     {$DEFINE overflow_checks_on}
     {$OVERFLOWCHECKS OFF}
   {$ENDIF}
   { Thomas Wang's hash6432shift - http://www.concentric.net/~Ttwang/tech/inthash.htm }
   Key := (not Key) + (Key shl 18);
   Key := Key xor (Key shr 31);
   Key := Key * 21;
   Key := Key xor (Key shr 11);
   Key := Key + (Key shl 6);
   Key := Key xor (Key shr 22);
   Result := Cardinal(Key);
   {$IFDEF overflow_checks_on}
     {$OVERFLOWCHECKS ON}
     {$UNDEF overflow_checks_on}
   {$ENDIF}
end;

function PtrUIntHash(Key: PtrUInt): Cardinal;
begin
   {$IFOPT Q-}
     {$DEFINE overflow_checks_off}
     {$OVERFLOWCHECKS ON}
   {$ENDIF}
   {$IF SizeOf(PtrUInt) = SizeOf(Cardinal) }
     Result := Integer32toHash32(Key);
   {$ELSEIF SizeOf(PtrUInt) = SizeOf(QWord) }
     Result := Integer64toHash32(Key);
   {$ELSE}
     {$FATAL No hash function for pointer size on this platform. }
   {$ENDIF}
   {$IFDEF overflow_checks_off}
     {$OVERFLOWCHECKS OFF}
     {$UNDEF overflow_checks_off}
   {$ENDIF}
end;

function PointerHash(Key: Pointer): Cardinal;
begin
   Result := PtrUIntHash(PtrUInt(Key));
end;

constructor THashTable.Create(AHashFunction: THashFunction);
begin
   Create(AHashFunction, 8);
end;

constructor THashTable.Create(AHashFunction: THashFunction; APredictedCount: Cardinal);
begin
   inherited Create();
   Assert(Assigned(AHashFunction));
   FHashFunction := AHashFunction;
   SetLength(FTable, APredictedCount);
end;

destructor THashTable.Destroy();
var
   Index: Cardinal;
   Item, LastItem: PHashTableEntry;
begin
   for Index := 0 to Length(FTable)-1 do
   begin
      Item := FTable[Index];
      while (Assigned(Item)) do
      begin
         LastItem := Item;
         Item := Item^.Next;
         Dispose(LastItem);
      end;
   end;
   inherited;
end;

procedure THashTable.DoubleSize();
var
   NewTable: array of PHashTableEntry;
   Index: Cardinal;
   Item, LastItem: PHashTableEntry;
begin
   SetLength(NewTable, Length(FTable)*2);
   for Index := 0 to Length(FTable)-1 do
   begin
      Item := FTable[Index];
      while (Assigned(Item)) do
      begin
         InternalAdd(NewTable, Item^.Key, Item^.Value);
         LastItem := Item;
         Item := Item^.Next;
         Dispose(LastItem);
      end;
   end;
   FTable := NewTable;
end;

procedure THashTable.InternalAdd(var Table: array of PHashTableEntry; Key: TKey; Value: TValue);
var
   Hash: Cardinal;
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Hash := FHashFunction(Key) mod Length(Table);
   New(Entry);
   Entry^.Key := Key;
   Entry^.Value := Value;
   Entry^.Next := Table[Hash];
   Table[Hash] := Entry;
end;

procedure THashTable.Add(Key: TKey; Value: TValue);
begin
   Inc(FCount);
   if (FCount/Length(FTable) > 0.7) then
   begin
      { Wikipedia: "With a good hash function, the average lookup cost is nearly constant as the load factor increases from 0 up to 0.7 or so" }
      DoubleSize();
   end;
   InternalAdd(FTable, Key, Value);
end;

function THashTable.Get(Key: TKey): TValue;
var
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Key) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Entry^.Key = Key) then
      begin
         Result := Entry^.Value;
         Exit;
      end;
      Entry := Entry^.Next;
   end;
   Result := FZero;
end;

procedure THashTable.Histogram(var F: Text);
var
   Index: Cardinal;
   Item: PHashTableEntry;
begin
   Writeln(F, 'THashTable histogram:');
   for Index := 0 to Length(FTable)-1 do
   begin
      Write(F, Index: 5, ': ');
      Item := FTable[Index];
      while (Assigned(Item)) do
      begin
         Write(F, '#');
         Item := Item^.Next;
      end;
      Writeln(F);
   end;
   Writeln(F, 'Size: ' + IntToStr(Length(FTable)) + '; Count: ' + IntToStr(FCount));
end;

end.