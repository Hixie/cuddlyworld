{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit storable;

interface

uses
   hashtable, sysutils;

type

   TStorable = class;
   StorableClass = class of TStorable;

   PPendingFixupItem = ^TPendingFixupItem;
   TPendingFixupItem = record
      Destination: PPointer;
      ObjectID: Cardinal;
      Next: PPendingFixupItem;
   end;

   TFixupHashTable = specialize THashTable<Cardinal, Pointer>;

   TReadStream = class
    protected
      FObjectsRead: TFixupHashTable;
      FPendingFixups: PPendingFixupItem;
      FInput: File;
      FVersion: Cardinal;
      procedure VerifyFieldType(FieldType: Byte);
      function ReadByte: Byte;
    public
      constructor Create(var AInput: File);
      destructor Destroy; override;
      function ReadObject: TStorable;
      function ReadCardinal: Cardinal;
      function ReadAnsiString: AnsiString;
      function ReadReference(Destination: PPointer): Boolean;
      function ReadClass: StorableClass;
      procedure FixupReferences();
      property Version: Cardinal read FVersion;
   end;

   TWriteStream = class
    protected
      FOutput: File;
      procedure WriteFieldType(FieldType: Byte);
      procedure WriteByte(Value: Byte);
    public
      constructor Create(var AOutput: File; Version: Cardinal);
      destructor Destroy; override;
      procedure WriteObject(Value: TStorable);
      procedure WriteCardinal(Value: Cardinal);
      procedure WriteAnsiString(Value: AnsiString);
      procedure WriteReference(Value: Pointer);
      procedure WriteClass(Value: StorableClass);
   end;

   TStorable = class
      {$IFOPT C+} FDebugCalledInherited: Boolean; {$ENDIF}
    public
      constructor Read(Stream: TReadStream); virtual;
      procedure Write(Stream: TWriteStream); virtual;
   end;

   EStorageError = class(Exception)
   end;

procedure RegisterStorableClass(AClass: StorableClass; ID: Cardinal);

procedure StoreObjectToFile(FileName: AnsiString; Value: TStorable; Version: Cardinal);
function ReadObjectFromFile(FileName: AnsiString): TStorable;

implementation

type
   TClassKeyToClassHashTable = specialize THashTable<Cardinal, StorableClass>;
   TClassToClassKeyHashTable = specialize THashTable<StorableClass, Cardinal>;

var
   ClassKeyToClassHash: TClassKeyToClassHashTable;
   ClassToClassKeyHash: TClassToClassKeyHashTable;

const { values with unlikely bit patterns }
   btSignature  = $AA;
   btStream     = $BB;
   btStreamEnd  = $DD;
   btObject     = $51;
   btObjectData = $52;
   btObjectEnd  = $54;
   btCardinal   = $61;
   btAnsiString = $62;
   btReference  = $64;
   btClass      = $68;

procedure RegisterStorableClass(AClass: StorableClass; ID: Cardinal);
begin
   Assert(ID <> 0, 'Class ID 0 is reserved');
   Assert(ID <> Cardinal(nil), 'Class ID Cardinal(nil) is reserved');
   Assert(not Assigned(ClassKeyToClassHash.Get(ID)), 'Class ID ' + IntToStr(ID) + ' used for both classes ' + AClass.ClassName() + ' and ' + ClassKeyToClassHash.Get(ID).ClassName());
   Assert(ClassToClassKeyHash.Get(AClass) = 0, 'Class ' + AClass.ClassName() + ' registered twice (first time as ID ' + IntToStr(ClassToClassKeyHash.Get(AClass)) + ', second time as ID ' + IntToStr(ID) + ')');
   ClassKeyToClassHash.Add(ID, AClass);
   ClassToClassKeyHash.Add(AClass, ID);
end;


procedure StoreObjectToFile(FileName: AnsiString; Value: TStorable; Version: Cardinal);
var
   F: File;
   Stream: TWriteStream;
begin
   Assign(F, FileName);
   Stream := nil;
   try
      Rewrite(F, 1);
      Stream := TWriteStream.Create(F, Version);
      Stream.WriteObject(Value);
   finally
      Stream.Free();
      Close(F);
   end;
end;

function ReadObjectFromFile(FileName: AnsiString): TStorable;
var
   F: File;
   Stream: TReadStream;
   IOResultValue: Word;
begin
   Assign(F, FileName);
   Stream := nil;
   try
      {$I-} Reset(F, 1); {$I+}
      IOResultValue := IOResult();
      if (IOResultValue = 2) then
      begin
         Result := nil;
         Exit;
      end;
      if (IOResultValue <> 0) then
         RunError(IOResultValue);
      Stream := TReadStream.Create(F);
      Result := Stream.ReadObject();
      Stream.FixupReferences();
   finally
      Stream.Free();
      Close(F);
   end;
end;


constructor TReadStream.Create(var AInput: File);
begin
   inherited Create();
   FInput := AInput;
   FObjectsRead := TFixupHashTable.Create(@IntegerHash);
   VerifyFieldType(btStream);
   FVersion := ReadCardinal();
end;

destructor TReadStream.Destroy;
var
   Next: PPendingFixupItem;
begin
   VerifyFieldType(btStreamEnd);
   while (Assigned(FPendingFixups)) do
   begin
      Next := FPendingFixups^.Next;
      Dispose(FPendingFixups);
      FPendingFixups := Next;
   end;
   FObjectsRead.Free();
   inherited;
end;

procedure TReadStream.VerifyFieldType(FieldType: Byte);
var
   Signature: Byte;
begin 
   if (ReadByte() <> btSignature) then
      raise EStorageError.Create('Stream inconsistent - sentinel not found');
   Signature := ReadByte();
   if (Signature <> FieldType) then
      raise EStorageError.Create('Stream inconsistent - expected type signature ' + IntToHex(FieldType, 2) + ' but found ' + IntToHex(Signature, 2));
end;

function TReadStream.ReadByte: Byte;
begin
   { The following statement is guaranteed to either set Result or throw an exception. }
   BlockRead(FInput, Result, SizeOf(Result));
end;

function TReadStream.ReadObject: TStorable;
var
   ClassValue: StorableClass;
   ObjectID: Cardinal;
begin
   VerifyFieldType(btObject);
   ClassValue := ReadClass();
   if (not Assigned(ClassValue)) then
   begin
      Result := nil;
   end
   else
   begin
      ObjectID := ReadCardinal();
      Result := ClassValue.Read(Self);
      {$IFOPT C+} Assert(Result.FDebugCalledInherited); {$ENDIF}
      FObjectsRead.Add(ObjectID, Result);
   end;
   VerifyFieldType(btObjectEnd);
end;

function TReadStream.ReadCardinal: Cardinal;
begin
   VerifyFieldType(btCardinal);
   { The following statement is guaranteed to either set Result or throw an exception. }
   BlockRead(FInput, Result, SizeOf(Result));
end;

function TReadStream.ReadAnsiString: AnsiString;
var
   Length: Cardinal;
begin
   VerifyFieldType(btAnsiString);
   Result := '';
   Length := ReadCardinal();
   if (Length > 0) then
   begin
      SetLength(Result, Length);
      BlockRead(FInput, Result[1], Length);
   end;
end;

function TReadStream.ReadReference(Destination: PPointer): Boolean;
var
   Item: PPendingFixupItem;
   ObjectID: Cardinal;
begin
   VerifyFieldType(btReference);
   ObjectID := ReadCardinal();
   Result := ObjectID <> Cardinal(nil);
   if (Result) then
   begin
      New(Item);
      Item^.Destination := Destination;
      Item^.ObjectID := ObjectID;
      Item^.Next := FPendingFixups;
      FPendingFixups := Item;
   end;
end;

function TReadStream.ReadClass: StorableClass;
var
   ClassID: Cardinal;
begin
   VerifyFieldType(btClass);
   ClassID := ReadCardinal();
   if (ClassID <> Cardinal(nil)) then
   begin
      Assert(Assigned(ClassKeyToClassHash.Get(ClassID)), 'Unknown class ID ' + IntToStr(ClassID));
      Result := ClassKeyToClassHash.Get(ClassID);
   end
   else
   begin
      Result := nil;
   end;
end;

procedure TReadStream.FixupReferences();
var
   Next: PPendingFixupItem;
begin
   while (Assigned(FPendingFixups)) do
   begin
      FPendingFixups^.Destination^ := FObjectsRead.Get(FPendingFixups^.ObjectID);
      Next := FPendingFixups^.Next;
      Dispose(FPendingFixups);
      FPendingFixups := Next;
   end;
end;


constructor TWriteStream.Create(var AOutput: File; Version: Cardinal);
begin
   inherited Create();
   FOutput := AOutput;
   WriteFieldType(btStream);
   WriteCardinal(Version);
end;

destructor TWriteStream.Destroy;
begin
   WriteFieldType(btStreamEnd);
   inherited;
end;

procedure TWriteStream.WriteFieldType(FieldType: Byte);
begin 
   WriteByte(btSignature);
   WriteByte(FieldType);
end;

procedure TWriteStream.WriteByte(Value: Byte);
begin
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteObject(Value: TStorable);
begin
   WriteFieldType(btObject);
   Assert(SizeOf(Cardinal) = SizeOf(Value));
   if (not Assigned(Value)) then
   begin
      WriteClass(nil);
   end
   else
   begin
      WriteClass(StorableClass(Value.ClassType));
      { Platform-specific; this will succeed so long as the assert above succeeds }
      WriteCardinal(Cardinal(Value));
      {$IFOPT C+} Value.FDebugCalledInherited := False; {$ENDIF}
      Value.Write(Self);
      {$IFOPT C+} Assert(Value.FDebugCalledInherited); {$ENDIF}
   end;
   WriteFieldType(btObjectEnd);
end;

procedure TWriteStream.WriteCardinal(Value: Cardinal);
begin
   WriteFieldType(btCardinal);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteAnsiString(Value: AnsiString);
begin
   WriteFieldType(btAnsiString);
   WriteCardinal(Length(Value));
   if (Length(Value) > 0) then
      BlockWrite(FOutput, Value[1], Length(Value));
end;

procedure TWriteStream.WriteReference(Value: Pointer);
begin
   Assert(SizeOf(Cardinal) = SizeOf(Value));
   WriteFieldType(btReference);
   { Platform-specific; this will succeed so long as the assert above succeeds }
   WriteCardinal(Cardinal(Value));
end;

procedure TWriteStream.WriteClass(Value: StorableClass);
var
   ClassID: Cardinal;
begin
   Assert(SizeOf(Cardinal) = SizeOf(Value));
   WriteFieldType(btClass);
   if (not Assigned(Value)) then
   begin
      WriteCardinal(Cardinal(nil));
   end
   else
   begin
      { Platform-specific; this will succeed so long as the assert above succeeds }
      ClassID := ClassToClassKeyHash.Get(StorableClass(Value));
      Assert(ClassID <> 0, 'Class ' + Value.ClassName() + ' not registered');
      WriteCardinal(ClassID);
   end;
end;


constructor TStorable.Read(Stream: TReadStream);
var
   S: AnsiString;
begin
   {$IFOPT C+} FDebugCalledInherited := True; {$ENDIF}
   Stream.VerifyFieldType(btObjectData);
   s := Stream.ReadAnsiString();
   Assert(S = ClassName);
end;

procedure TStorable.Write(Stream: TWriteStream);
begin
   {$IFOPT C+} FDebugCalledInherited := True; {$ENDIF}
   Stream.WriteFieldType(btObjectData);
   Stream.WriteAnsiString(ClassName);
end;


function StorableClassHash(Key: StorableClass): Cardinal;
begin
   Assert(SizeOf(Key) = SizeOf(Cardinal));
   { Platform-specific; this will succeed so long as the assert above succeeds }
   Result := IntegerHash(Cardinal(Key));
end;

initialization
   ClassKeyToClassHash := TClassKeyToClassHashTable.Create(@IntegerHash);
   ClassToClassKeyHash := TClassToClassKeyHashTable.Create(@StorableClassHash);
finalization
   ClassKeyToClassHash.Free();
   ClassToClassKeyHash.Free();
end.