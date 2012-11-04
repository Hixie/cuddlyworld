{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit hashtable;

interface

(*****************************************************************)
(** IF YOU EDIT THIS UNIT'S IMPLEMENTATION SECTION, MODIFY THE  **)
(** FOLLOWING LINE AS WELL TO FORCE A RECOMPILATION OF ANY      **)
(** SPECIALISATIONS OF THE HASH TABLE.                          **)
(*****************************************************************)

const
   ArbitraryConstant = 0;

type
   THashTableSizeInt = 0..MaxInt;

   generic THashTable <TKey, TValue> = class abstract
    protected
     type
      PPHashTableEntry = ^PHashTableEntry;
      PHashTableEntry = ^THashTableEntry;
      THashTableEntry = record
        Key: TKey;
        Value: TValue;
        Next: PHashTableEntry;
      end;
      THashFunction = function (Key: TKey): DWord;
     var
      FTable: array of PHashTableEntry;
      FZeroKey: TKey;
      FZeroValue: TValue;
      FCount: THashTableSizeInt;
      FHashFunction: THashFunction;
      procedure DoubleSize();
      procedure Resize(NewSize: THashTableSizeInt);
      procedure PrepareForSize(PredictedCount: THashTableSizeInt);
      procedure InternalAdd(var Table: array of PHashTableEntry; Key: TKey; Value: TValue);
      procedure Update(Key: TKey; Value: TValue); // will call Add() if the key isn't already present
      function Get(Key: TKey): TValue;
      function GetKeyForEntry(const Entry: Pointer): TKey;
      function GetValueForEntry(const Entry: Pointer): TValue;
      procedure AdvanceEnumerator(var Current: Pointer; var Index: THashTableSizeInt);
    public
      constructor Create(AHashFunction: THashFunction; PredictedCount: THashTableSizeInt = 8);
      destructor Destroy(); override;
      procedure Empty();
      procedure Remove(Key: TKey);
      function Has(Key: TKey): Boolean;
      procedure Add(Key: TKey; Value: TValue);
      property Items[Key: TKey]: TValue read Get write Update; default;
      {$IFDEF DEBUG} procedure Histogram(var F: Text); {$ENDIF}
      property Count: THashTableSizeInt read FCount;
   end;

   generic THashTableKeyEnumerator <THashTable, TKey> = class
    private
      FOwner: THashTable;
      FIndex: THashTableSizeInt;
      FCurrent: Pointer;
      function GetCurrent(): TKey;
    public
      constructor Create(Owner: THashTable);
      function MoveNext(): Boolean;
      property Current: TKey read GetCurrent;
   end;

   generic THashTableValueEnumerator <THashTable, TValue> = class
    private
      FOwner: THashTable;
      FIndex: THashTableSizeInt;
      FCurrent: Pointer;
      function GetCurrent(): TValue;
    public
      constructor Create(Owner: THashTable);
      function MoveNext(): Boolean;
      property Current: TValue read GetCurrent;
   end;

function Integer32Hash32(Key: DWord): DWord; inline;
function Integer64Hash32(Key: QWord): DWord; inline;
function PtrUIntHash32(Key: PtrUInt): DWord; inline;
function PointerHash32(Key: Pointer): DWord; inline;
function TMethodHash32(Key: TMethod): DWord; inline;
function AnsiStringHash32(Key: AnsiString): DWord; inline;


(* How to use THashTable ******************************************************************
 * This generates a TFoo to TBar hash table with name TFooHashTable.
 * Replace FooHash32 with one of the functions immediately above, based on TFoo's type

// first, define the hash table

type
   TFooHashTable = class(specialize THashTable <TFoo, TBar>)
    public
     constructor Create(PredictedCount: THashTableSizeInt = 8);
   end;

constructor TFooHashTable.Create(PredictedCount: THashTableSizeInt = 8);
begin
   inherited Create(@FooHash32, PredictedCount);
end;


// then, either have just a key or value enumerator; here's a value one:

type
   TFooHashTableValueEnumerator = specialize THashTableValueEnumerator <TFooHashTable, TFoo>;

operator enumerator(Operand: TFooHashTable): TFooHashTableValueEnumerator;
begin
   Result := TFooHashTableValueEnumerator.Create(Operand);
end;


// or have both, and require casting through Pointer to do actual enumeration, if you really need it:
// (as in  for Foo in TFooHashTableValueEnumerator(Pointer(HashTable)) do ...  )

type
   TFooHashTableKeyEnumerated = class(TFooHashTable);
   TFooHashTableKeyEnumerator = specialize THashTableKeyEnumerator <TFooHashTableKeyEnumerated, TFoo>;

operator enumerator(Operand: TFooHashTableKeyEnumerated): TFooHashTableKeyEnumerator;
begin
   Result := TFooHashTableKeyEnumerator.Create(Operand);
end;

type
   TFooHashTableValueEnumerated = class(TFooHashTable);
   TFooHashTableValueEnumerator = specialize THashTableValueEnumerator <TFooHashTableValueEnumerated, TBar>;

operator enumerator(Operand: TFooHashTableValueEnumerated): TFooHashTableValueEnumerator;
begin
   Result := TFooHashTableValueEnumerator.Create(Operand);
end;

******************************************************************************************)


implementation

{$IF SIZEOF(DWord) <> 4} {$ERROR DWord must be 32 bits wide.} {$ENDIF}
{$IF SIZEOF(QWord) <> 8} {$ERROR QWord must be 64 bits wide.} {$ENDIF}

uses
   sysutils;

function Integer32Hash32(Key: DWord): DWord;
begin
   Assert(SizeOf(DWord) * 8 = 32);
   Result := Key;
   {$IFOPT Q+}
     {$DEFINE overflow_checks_on}
     {$OVERFLOWCHECKS OFF}
   {$ENDIF}
   {$IFOPT R+}
     {$DEFINE range_checks_on}
     {$RANGECHECKS OFF}
   {$ENDIF}
   { Robert Jenkins 32bit Integer Hash - http://burtleburtle.net/bob/hash/integer.html }
   {$HINTS OFF} // because all this intentionally overflows
   Result := (Result  +  $7ed55d16)  +  (Result shl 12);
   Result := (Result xor $c761c23c) xor (Result shr 19);
   Result := (Result  +  $165667b1)  +  (Result shl  5);
   Result := (Result  +  $d3a2646c) xor (Result shl  9);
   Result := (Result  +  $fd7046c5)  +  (Result shl  3);
   Result := (Result xor $b55a4f09) xor (Result shr 16);
   {$HINTS ON}
   {$IFDEF overflow_checks_on}
     {$OVERFLOWCHECKS ON}
     {$UNDEF overflow_checks_on}
   {$ENDIF}
   {$IFDEF range_checks_on}
     {$RANGECHECKS ON}
     {$UNDEF range_checks_on}
   {$ENDIF}
end;

function Integer64Hash32(Key: QWord): DWord;
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
   Result := DWord(Key);
   {$IFDEF overflow_checks_on}
     {$OVERFLOWCHECKS ON}
     {$UNDEF overflow_checks_on}
   {$ENDIF}
end;

function PtrUIntHash32(Key: PtrUInt): DWord;
begin
   {$IFOPT Q-}
     {$DEFINE overflow_checks_off}
     {$OVERFLOWCHECKS ON}
   {$ENDIF}
   {$IF SizeOf(PtrUInt) = SizeOf(DWord) }
     Result := Integer32Hash32(Key);
   {$ELSEIF SizeOf(PtrUInt) = SizeOf(QWord) }
     Result := Integer64Hash32(Key);
   {$ELSE}
     {$FATAL No hash function for pointer size on this platform. }
   {$ENDIF}
   {$IFDEF overflow_checks_off}
     {$OVERFLOWCHECKS OFF}
     {$UNDEF overflow_checks_off}
   {$ENDIF}
end;

function PointerHash32(Key: Pointer): DWord;
begin
   {$HINTS OFF} // Otherwise it complains that casting Pointer to PtrUInt is not portable, but it is portable, by definition
   Result := PtrUIntHash32(PtrUInt(Key));
   {$HINTS ON}
end;

function TMethodHash32(Key: TMethod): DWord;
begin
   {$IF SizeOf(PtrUInt) = SizeOf(DWord) }
     Assert(SizeOf(Key) = SizeOf(QWord));
     Result := Integer64Hash32(QWord(Key));
   {$ELSEIF SizeOf(Pointer) = SizeOf(QWord) }
     // XXX no idea if this is an acceptable hash function
     // XXX should print out the hashtable histogram once there's a number of watchers
     {$HINTS OFF} // Otherwise it complains that casting Pointer to QWord is not portable, but we only go down this path if it is ok for this platform
     Result := Integer64Hash32(QWord(TMethod(Key).Code)) xor Integer64Hash32(QWord(TMethod(Key).Data));
     {$HINTS ON}
   {$ELSE}
     Result := PointerHash32(TMethod.Code) xor PointerHash32(TMethod.Data);
   {$ENDIF}
end;

function AnsiStringHash32(Key: AnsiString): DWord;
var
   Index: Cardinal;
begin
   {$IFOPT R+}
     {$DEFINE range_checks_on}
     {$RANGECHECKS OFF}
   {$ENDIF}

{$HINTS OFF} // not sure if the four hints for the next few lines are valid or not, but I'm guessing not.
   // djb2 from http://www.cse.yorku.ca/~oz/hash.html:
   Result := 5381;
   if (Length(Key) > 0) then
      for Index := 1 to Length(Key) do
         Result := Result shl 5 + Result + Ord(Key[Index]);
{$HINTS ON}

   // djb2 bis from http://www.cse.yorku.ca/~oz/hash.html:
   //Result := 5381;
   //if (Length(Key) > 0) then
   //   for Index := 1 to Length(Key) do
   //      Result := Result * 33 xor Ord(Key[Index]);

   // sdbm from http://www.cse.yorku.ca/~oz/hash.html:
   //Result := 0;
   //if (Length(Key) > 0) then
   //   for Index := 1 to Length(Key) do
   //      Result := Ord(Key[Index]) + (Result shl 6) + (Result shl 16) - Result;

   {$IFDEF range_checks_on}
     {$RANGECHECKS ON}
     {$UNDEF range_checks_on}
   {$ENDIF}
end;


constructor THashTable.Create(AHashFunction: THashFunction; PredictedCount: THashTableSizeInt = 8);
begin
   inherited Create();
   Assert(Assigned(AHashFunction));
   FHashFunction := AHashFunction;
   Assert(PredictedCount > 0);
   PrepareForSize(PredictedCount);
end;

destructor THashTable.Destroy();
begin
   Empty();
   inherited;
end;

procedure THashTable.Empty();
var
   Index: THashTableSizeInt;
   Item, LastItem: PHashTableEntry;
begin
   if (Length(FTable) > 0) then
      for Index := Low(FTable) to High(FTable) do
      begin
         Item := FTable[Index];
         while (Assigned(Item)) do
         begin
            LastItem := Item;
            Item := Item^.Next;
            Dispose(LastItem);
         end;
         FTable[Index] := nil;
      end;
   FCount := 0;
end;

procedure THashTable.DoubleSize();
begin
   Assert(Length(FTable) > 0);
   if (Length(FTable)*2 < High(THashTableSizeInt)) then
      Resize(Length(FTable) * 2) {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   else
   if (Length(FTable) < High(THashTableSizeInt)) then
      Resize(High(THashTableSizeInt));
end;

procedure THashTable.PrepareForSize(PredictedCount: THashTableSizeInt);
const
   LoadFactorLimit = 1/0.7; // Wikipedia: "With a good hash function, the average lookup cost is nearly constant as the load factor increases from 0 up to 0.7 or so"
begin
   Assert(PredictedCount > 0);
   if (PredictedCount * LoadFactorLimit < High(THashTableSizeInt)) then
      PredictedCount := Trunc(PredictedCount * LoadFactorLimit) {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   else
      PredictedCount := High(THashTableSizeInt);
   if (FCount > 0) then
      Resize(PredictedCount)
   else
      SetLength(FTable, PredictedCount);
end;

procedure THashTable.Resize(NewSize: THashTableSizeInt);
var
   NewTable: array of PHashTableEntry;
   Index: THashTableSizeInt;
   Item, LastItem: PHashTableEntry;
begin
   Assert(NewSize > 0);
   if (NewSize <> Length(FTable)) then
   begin
      SetLength(NewTable, NewSize);
      for Index := Low(FTable) to High(FTable) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
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
end;

procedure THashTable.InternalAdd(var Table: array of PHashTableEntry; Key: TKey; Value: TValue);
var
   Hash: DWord;
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Hash := FHashFunction(Key) mod Length(Table); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
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

(** This replaces (Entry^.Key = Key) with (CompareKeys(Entry^.Key, Key, SizeOf(TKey)))
    However, http://bugs.freepascal.org/bug_view_advanced_page.php?bug_id=21310 blocks this
function CompareKeys(var Key1, Key2; const Length: Cardinal): Boolean; inline;
type
   QWordArray = array[0..High(Longint) div SizeOf(QWord)] of QWord;
var
   Index: Cardinal;
begin
   case Length of
     SizeOf(Byte):  Result := Byte(Key1)  = Byte(Key2);
     SizeOf(Word):  Result := Word(Key1)  = Word(Key2);
     SizeOf(DWord): Result := DWord(Key1) = DWord(Key2);
     SizeOf(QWord): Result := QWord(Key1) = QWord(Key2);
    else
     Assert(Length mod SizeOf(QWord) = 0);
     for Index := 0 to (Length div SizeOf(QWord))-1 do
        if (QWordArray(Key1)[Index] <> QWordArray(Key2)[Index]) then
        begin
           Result := False;
           Exit;
        end;
     Result := True;
   end;
end;
*)

procedure THashTable.Remove(Key: TKey);
var
   Hash: DWord;
   Entry: PHashTableEntry;
   LastEntry: PPHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Hash := FHashFunction(Key) mod Length(FTable); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   Entry := FTable[Hash];
   LastEntry := @FTable[Hash];
   while (Assigned(Entry)) do
   begin
      if (Entry^.Key = Key) then
      begin
         LastEntry^ := Entry^.Next;
         Dispose(Entry);
         Dec(FCount);
         Exit;
      end;
      LastEntry := @Entry^.Next;
      Entry := Entry^.Next;
   end;
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
   Result := FZeroValue;
end;

function THashTable.Has(Key: TKey): Boolean;
var
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Key) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Entry^.Key = Key) then
      begin
         Result := True;
         Exit;
      end;
      Entry := Entry^.Next;
   end;
   Result := False;
end;

procedure THashTable.Update(Key: TKey; Value: TValue);
var
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Key) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Entry^.Key = Key) then
      begin
         Entry^.Value := Value;
         Exit;
      end;
      Entry := Entry^.Next;
   end;
   Add(Key, Value);
end;

{$IFDEF DEBUG}
procedure THashTable.Histogram(var F: Text);
var
   Index: THashTableSizeInt;
   Item: PHashTableEntry;
begin
   Assert(Length(FTable) > 0);
   Writeln(F, 'THashTable histogram:');
   for Index := Low(FTable) to High(FTable) do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   begin
      System.Write(F, Index: 5, ': ');
      Item := FTable[Index];
      while (Assigned(Item)) do
      begin
         System.Write(F, '#');
         Item := Item^.Next;
      end;
      Writeln(F);
   end;
   Writeln(F, 'Size: ' + IntToStr(Length(FTable)) + '; Count: ' + IntToStr(FCount));
end;
{$ENDIF}

function THashTable.GetKeyForEntry(const Entry: Pointer): TKey;
begin
   if (Assigned(Entry)) then
   begin
      Result := PHashTableEntry(Entry)^.Key;
   end
   else
   begin
      Result := FZeroKey;
   end;
end;

function THashTable.GetValueForEntry(const Entry: Pointer): TValue;
begin
   if (Assigned(Entry)) then
   begin
      Result := PHashTableEntry(Entry)^.Value;
   end
   else
   begin
      Result := FZeroValue;
   end;
end;

procedure THashTable.AdvanceEnumerator(var Current: Pointer; var Index: THashTableSizeInt);
begin
   if (Assigned(Current)) then
   begin // advance
      Current := PHashTableEntry(Current)^.Next
   end
   else
   begin // just started
      Assert(Index = 0);
      Current := FTable[Index];
   end;
   while ((not Assigned(Current)) and (Index+1 < Length(FTable))) do
   begin
      Inc(Index);
      Current := FTable[Index];
   end;
end;


constructor THashTableKeyEnumerator.Create(Owner: THashTable);
begin
   FOwner := Owner;
   FIndex := 0;
   FCurrent := nil;
end;

function THashTableKeyEnumerator.GetCurrent(): TKey;
begin
   Result := FOwner.GetKeyForEntry(FCurrent);
end;

function THashTableKeyEnumerator.MoveNext(): Boolean;
begin
   FOwner.AdvanceEnumerator(FCurrent, FIndex);
   Result := Assigned(FCurrent);
end;


constructor THashTableValueEnumerator.Create(Owner: THashTable);
begin
   FOwner := Owner;
   FIndex := 0;
   FCurrent := nil;
end;

function THashTableValueEnumerator.GetCurrent(): TValue;
begin
   Result := FOwner.GetValueForEntry(FCurrent);
end;

function THashTableValueEnumerator.MoveNext(): Boolean;
begin
   FOwner.AdvanceEnumerator(FCurrent, FIndex);
   Result := Assigned(FCurrent);
end;

end.