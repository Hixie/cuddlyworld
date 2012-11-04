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
      ObjectID: PtrUInt;
      Next: PPendingFixupItem;
   end;

   TFixerProcedure = procedure (const Data: Pointer; const Value: Pointer) of object;

   PPendingFixerItem = ^TPendingFixerItem;
   TPendingFixerItem = record
      Fixer: TFixerProcedure;
      Data, Value: Pointer;
      Next: PPendingFixerItem;
   end;

   TFixupHashTable = specialize THashTable<PtrUInt, Pointer>;

   TReadStream = class
    protected
      FActive: Boolean;
      FObjectsRead: TFixupHashTable;
      FPendingFixups: PPendingFixupItem;
      FPendingFixers: PPendingFixerItem;
      FInput: File;
      FVersion: Cardinal;
      procedure VerifyFieldType(FieldType: Byte);
      function GetFilename(): AnsiString;
    public
      constructor Create(var AInput: File);
      destructor Destroy; override;
      procedure DisableChecks();
      function ReadByte: Byte;
      function ReadBoolean: Boolean;
      function ReadCardinal: Cardinal;
      function ReadInteger: Integer;
      function ReadPtrUInt: PtrUInt;
      function ReadDouble: Double;
      function ReadAnsiString: AnsiString;
      procedure ReadByteStream(var Buffer; Length: Cardinal);
      function ReadClass: StorableClass;
      function ReadObject: TStorable;
      procedure ReadReference(Destination: PPointer);
      procedure ReadReference(Fixer: TFixerProcedure; const Data: Pointer);
      procedure VerifySentinel();
      procedure FixupReferences();
      property Version: Cardinal read FVersion;
      property Filename: AnsiString read GetFilename;
   end;

   TWriteStream = class
    protected
      FOutput: File;
      procedure WriteFieldType(FieldType: Byte);
    public
      constructor Create(var AOutput: File; Version: Cardinal);
      destructor Destroy; override;
      procedure WriteByte(Value: Byte);
      procedure WriteBoolean(Value: Boolean);
      procedure WriteCardinal(Value: Cardinal);
      procedure WriteInteger(Value: Integer);
      procedure WritePtrUInt(Value: PtrUInt);
      procedure WriteDouble(Value: Double);
      procedure WriteAnsiString(Value: AnsiString);
      procedure WriteByteStream(var Buffer; Length: Cardinal);
      procedure WriteClass(Value: StorableClass);
      procedure WriteObject(Value: TStorable);
      procedure WriteReference(Value: Pointer);
      procedure WriteSentinel();
   end;

   TStorable = class abstract
    protected
      {$IFOPT C+} FDebugCalledInherited: Boolean; {$ENDIF}
    public
      constructor Create();
      constructor Read(Stream: TReadStream); virtual;
      procedure Write(Stream: TWriteStream); virtual;
   end;

   EStorageError = class(Exception)
   end;

procedure RegisterStorableClass(AClass: StorableClass);
procedure RegisterStorableClassSynonym(AClassName: AnsiString; RestoreClass: StorableClass);

procedure StoreObjectToFile(FileName: AnsiString; Value: TStorable; Version: Cardinal);
function ReadObjectFromFile(FileName: AnsiString): TStorable;

implementation

uses
   exceptions;

type
   TStorableClassesHashTable = specialize THashTable<AnsiString, StorableClass>;

var
   RegisteredClasses: TStorableClassesHashTable;

const { values with bit patterns unlikely to be found in typical data (so e.g. not $00, $01, $FF) }
   btSignature  = $AA;
   btStream     = $BB;
   btStreamEnd  = $DD;

   btBoolean    = $60;
   btCardinal   = $61;
   btInteger    = $62;
   btPtrUInt    = $63;
   btDouble     = $63;
   btAnsiString = $65;
   btByteStream = $66;

   btClass      = $80;
   btReference  = $81;

   btObject     = $51;
   btObjectData = $52;
   btObjectEnd  = $54;
   btSentinel   = $58;

const
   ciNoClass = 'nil';

procedure RegisterStorableClass(AClass: StorableClass);
begin
   Assert(not RegisteredClasses.Has(AClass.ClassName), AClass.ClassName + ' registered twice');
   RegisteredClasses[AClass.ClassName] := AClass;
end;

procedure RegisterStorableClassSynonym(AClassName: AnsiString; RestoreClass: StorableClass);
begin
   Assert(not RegisteredClasses.Has(AClassName), AClassName + ' registered twice');
   RegisteredClasses[AClassName] := RestoreClass;
end;


procedure StoreObjectToFile(FileName: AnsiString; Value: TStorable; Version: Cardinal);
var
   F, OldF: File;
   Stream: TWriteStream;
begin
   Assign(F, FileName + '.$$$');
   Stream := nil;
   try
      Rewrite(F, 1);
      Stream := TWriteStream.Create(F, Version);
      Stream.WriteObject(Value);
   finally
      Stream.Free();
      Close(F);
   end;
   Assign(OldF, FileName);
   if (FileExists(FileName)) then
      Erase(OldF);
   Rename(F, FileName);
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
      try
         Result := Stream.ReadObject();
         Stream.FixupReferences();
      except
         Stream.DisableChecks();
         raise;
      end;
   finally
      Stream.Free();
      Close(F);
   end;
end;


constructor TReadStream.Create(var AInput: File);
begin
   inherited Create();
   FActive := True;
   FInput := AInput;
   FObjectsRead := TFixupHashTable.Create(@PtrUIntHash32);
   VerifyFieldType(btStream);
   FVersion := ReadCardinal();
   if (ReadByte() <> SizeOf(PtrUInt)) then
      raise EStorageError.Create('Stream was written on a platform with a different pointer size.');
end;

destructor TReadStream.Destroy;
var
   NextFixup: PPendingFixupItem;
   NextFixer: PPendingFixerItem;
begin
   if (FActive) then
      VerifyFieldType(btStreamEnd);
   while (Assigned(FPendingFixups)) do
   begin
      NextFixup := FPendingFixups^.Next;
      Dispose(FPendingFixups);
      FPendingFixups := NextFixup;
   end;
   while (Assigned(FPendingFixers)) do
   begin
      NextFixer := FPendingFixers^.Next;
      Dispose(FPendingFixers);
      FPendingFixers := NextFixer;
   end;
   FObjectsRead.Free();
   inherited;
end;

procedure TReadStream.DisableChecks();
begin
   FActive := False;
end;

procedure TReadStream.VerifyFieldType(FieldType: Byte);
var
   Signature: Byte;
begin 
   if (ReadByte() <> btSignature) then
      raise EStorageError.Create('Stream inconsistent - type signature marker not found');
   Signature := ReadByte();
   if (Signature <> FieldType) then
      raise EStorageError.Create('Stream inconsistent - expected type signature ' + IntToHex(FieldType, 2) + ' but found ' + IntToHex(Signature, 2));
end;

function TReadStream.ReadByte: Byte;
begin
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadBoolean: Boolean;
begin
   VerifyFieldType(btBoolean);
   Result := ReadByte() = $01;
end;

function TReadStream.ReadCardinal: Cardinal;
begin
   VerifyFieldType(btCardinal);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadInteger: Integer;
begin
   VerifyFieldType(btInteger);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadPtrUInt: PtrUInt;
begin
   VerifyFieldType(btPtrUInt);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadDouble: Double;
begin
   VerifyFieldType(btDouble);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
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

procedure TReadStream.ReadByteStream(var Buffer; Length: Cardinal);
begin
   VerifyFieldType(btByteStream);
   BlockRead(FInput, Buffer, Length);
end;

function TReadStream.ReadClass: StorableClass;
var
   RestoreClassName: AnsiString;
begin
   VerifyFieldType(btClass);
   RestoreClassName := ReadAnsiString();
   if (RestoreClassName <> ciNoClass) then
   begin
      Assert(RegisteredClasses.Has(RestoreClassName), 'Tried to restore unregistered class ' + RestoreClassName);
      Result := RegisteredClasses[RestoreClassName];
   end
   else
   begin
      Result := nil;
   end;
end;

function TReadStream.ReadObject: TStorable;
var
   ClassValue: StorableClass;
   ObjectID: PtrUInt;
begin
   VerifyFieldType(btObject);
   ClassValue := ReadClass();
   if (not Assigned(ClassValue)) then
   begin
      Result := nil;
   end
   else
   begin
      ObjectID := ReadPtrUInt();
      Result := ClassValue.Read(Self);
      {$IFOPT C+} Assert(Result.FDebugCalledInherited); {$ENDIF} // it's initialised to false when the object is created
      FObjectsRead.Add(ObjectID, Result);
   end;
   VerifyFieldType(btObjectEnd);
end;

procedure TReadStream.ReadReference(Destination: PPointer);
var
   Item: PPendingFixupItem;
   ObjectID: PtrUInt;
begin
   VerifyFieldType(btReference);
   ObjectID := ReadPtrUInt();
   {$HINTS OFF} // Compiler thinks casting 'nil' to PtrUInt might be non-portable
   if (ObjectID <> PtrUInt(nil)) then
   {$HINTS ON}
   begin
      New(Item);
      Item^.Destination := Destination;
      Item^.ObjectID := ObjectID;
      Item^.Next := FPendingFixups;
      FPendingFixups := Item;
   end
   else
   begin
      Destination^ := nil;
   end;
end;

procedure TReadStream.ReadReference(Fixer: TFixerProcedure; const Data: Pointer);
var
   Item: PPendingFixerItem;
begin
   New(Item);
   Item^.Fixer := Fixer;
   Item^.Data := Data;
   ReadReference(@(Item^.Value));
   Item^.Next := FPendingFixers;
   FPendingFixers := Item;
end;

procedure TReadStream.VerifySentinel();
begin
   VerifyFieldType(btSentinel);
end;

procedure TReadStream.FixupReferences();
var
   NextFixup: PPendingFixupItem;
   NextFixer: PPendingFixerItem;
begin
   while (Assigned(FPendingFixups)) do
   begin
      FPendingFixups^.Destination^ := FObjectsRead.Get(FPendingFixups^.ObjectID);
      NextFixup := FPendingFixups^.Next;
      Dispose(FPendingFixups);
      FPendingFixups := NextFixup;
   end;
   while (Assigned(FPendingFixers)) do
   begin
      FPendingFixers^.Fixer(FPendingFixers^.Data, FPendingFixers^.Value);
      NextFixer := FPendingFixers^.Next;
      Dispose(FPendingFixers);
      FPendingFixers := NextFixer;
   end;
end;

function TReadStream.GetFilename(): AnsiString;
begin
   Result := FileRec(FInput).Name;
end;


constructor TWriteStream.Create(var AOutput: File; Version: Cardinal);
begin
   inherited Create();
   FOutput := AOutput;
   WriteFieldType(btStream);
   WriteCardinal(Version);
   WriteByte(SizeOf(PtrUInt));
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

procedure TWriteStream.WriteBoolean(Value: Boolean);
begin
   WriteFieldType(btBoolean);
   if (Value) then
      WriteByte($01)
   else
      WriteByte($00);
end;

procedure TWriteStream.WriteCardinal(Value: Cardinal);
begin
   WriteFieldType(btCardinal);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteInteger(Value: Integer);
begin
   WriteFieldType(btInteger);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WritePtrUInt(Value: PtrUInt);
begin
   WriteFieldType(btPtrUInt);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteDouble(Value: Double);
begin
   WriteFieldType(btDouble);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteAnsiString(Value: AnsiString);
begin
   WriteFieldType(btAnsiString);
   WriteCardinal(Length(Value));
   if (Length(Value) > 0) then
      BlockWrite(FOutput, Value[1], Length(Value));
end;

procedure TWriteStream.WriteByteStream(var Buffer; Length: Cardinal);
begin
   WriteFieldType(btByteStream);
   BlockWrite(FOutput, Buffer, Length);
end;

procedure TWriteStream.WriteClass(Value: StorableClass);
begin
   WriteFieldType(btClass);
   if (not Assigned(Value)) then
   begin
      WriteAnsiString(ciNoClass);
   end
   else
   begin
      Assert(RegisteredClasses.Has(Value.ClassName), 'Tried to store unregistered class ' + Value.ClassName);
      WriteAnsiString(Value.ClassName);
   end;
end;

procedure TWriteStream.WriteObject(Value: TStorable);
begin
   WriteFieldType(btObject);
   if (not Assigned(Value)) then
   begin
      WriteClass(nil);
   end
   else
   begin
      WriteClass(StorableClass(Value.ClassType));
      WritePtrUInt(PtrUInt(Value));
      {$IFOPT C+} Value.FDebugCalledInherited := False; {$ENDIF}
      Value.Write(Self);
      {$IFOPT C+} Assert(Value.FDebugCalledInherited); {$ENDIF}
   end;
   WriteFieldType(btObjectEnd);
end;

procedure TWriteStream.WriteReference(Value: Pointer);
begin
   WriteFieldType(btReference);
   {$HINTS OFF} // Compiler thinks casting a pointer to PtrUInt might not be portable
   WritePtrUInt(PtrUInt(Value));
   {$HINTS ON}
end;

procedure TWriteStream.WriteSentinel();
begin
   WriteFieldType(btSentinel);
end;


constructor TStorable.Create();
begin
   inherited;
   Assert(RegisteredClasses.Has(ClassName), 'Class ' + ClassName + ' has not been registered with RegisterStorableClass().');
end;

constructor TStorable.Read(Stream: TReadStream);
begin
   {$IFOPT C+} FDebugCalledInherited := True; {$ENDIF}
   Stream.VerifyFieldType(btObjectData);
end;

procedure TStorable.Write(Stream: TWriteStream);
begin
   {$IFOPT C+} FDebugCalledInherited := True; {$ENDIF}
   Stream.WriteFieldType(btObjectData);
end;


initialization
   RegisteredClasses := TStorableClassesHashTable.Create(@AnsiStringHash32);
finalization
   RegisteredClasses.Free();
end.