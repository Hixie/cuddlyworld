{$MODE OBJFPC} { -*- text -*- }
{$INCLUDE settings.inc}
unit lists;

interface

uses
   storable;

type
   TTraversalDirection = (tdForward, tdReverse);

   PListNode = ^TListNode;
   TListNode = record
      Next: PListNode;
      Previous: PListNode;
      Value: TStorable;
   end;

   TAbstractStorableList = class;

   generic TGenericStorableListEnumerator<TItem> = class
     protected
      FCurrentListNode: PListNode;
      FList: TAbstractStorableList;
      FDirection: TTraversalDirection;
      FAdvanced: Boolean;
     strict private { just "private" has no useful effect here since the generic is compiled in the context of whomever invokes us }
      function InternalGetCurrent(): TItem;
     public
      constructor Create(List: TAbstractStorableList; StartListNode: PListNode; Direction: TTraversalDirection);
      destructor Destroy(); override;
      function HasMore(): Boolean;
      procedure Remove();
      procedure RemoveRemainder();
      { Enumerator support }
      function MoveNext(): Boolean;
      property Current: TItem read InternalGetCurrent;
   end;

   TStorableListFlags = set of (slOwner, { if set, frees the contents of the list and writes objects to the stream; otherwise, doesn't free and writes references }
                                slDropDuplicates); { if set, duplicates are checked for and ignored when adding }

   TAbstractStorableList = class(TStorable)
     public { should be protected }
      { these members have to be visible since they are used by the generics below and the generics are compiled outside this unit }
      FFlags: TStorableListFlags;
      FFirstNode, FLastNode: PListNode;
      FLength, FActiveEnumerators: Cardinal;
      procedure InternalRemoveItem(Node: PListNode); { has to be visible by enumerator }
      procedure InternalRemoveFromItem(Node: PListNode; Direction: TTraversalDirection); { has to be visible by enumerator }
      {$IFOPT C+} procedure CheckLength(); {$ENDIF}
     public
      constructor Create(Flags: TStorableListFlags = []);
      destructor Destroy(); override;
      procedure Empty(); { without freeing }
      procedure Deduplicate();
      procedure FreeItems(); { and empty }
      procedure Write(Stream: TWriteStream); override;
      property Length: Cardinal read FLength;
   end;

   generic TStorableList<TItem, TEnumerator> = class(TAbstractStorableList)
     private { this is private to wherever we get replayed, sadly (so e.g. world.pas) }
      procedure InternalMergeList(List: TAbstractStorableList);
      procedure InternalAppendList(List: TAbstractStorableList);
      function InternalGetFirst(): TItem;
     public
      constructor Clone(Template: TAbstractStorableList; Flags: TStorableListFlags = []);
      constructor Read(Stream: TReadStream); override;
      procedure AppendItem(Item: TItem);
      procedure RemoveItem(Item: TItem);
      procedure AdoptItem(Enumerator: TEnumerator);
      procedure AdoptList(List: TAbstractStorableList); // need compile-time type check here -- see http://mantis.freepascal.org/view.php?id=11777
      function GetEnumerator(const Direction: TTraversalDirection = tdForward): TEnumerator;
      function Contains(Item: TItem): Boolean;
      property First: TItem read InternalGetFirst;
   end;

implementation

uses
   sysutils;

constructor TGenericStorableListEnumerator.Create(List: TAbstractStorableList; StartListNode: PListNode; Direction: TTraversalDirection);
begin
   inherited Create();
   FList := List;
   FCurrentListNode := StartListNode;
   FDirection := Direction;
   FAdvanced := True;
   Inc(FList.FActiveEnumerators);
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
end;

destructor TGenericStorableListEnumerator.Destroy();
begin
   Dec(FList.FActiveEnumerators);
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
   inherited;
end;

function TGenericStorableListEnumerator.MoveNext(): Boolean;
begin
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
   if (not FAdvanced) then
   begin
      Assert(Assigned(FCurrentListNode));
      case FDirection of
       tdForward: FCurrentListNode := FCurrentListNode^.Next;
       tdReverse: FCurrentListNode := FCurrentListNode^.Previous;
       else Assert(False, 'unknown direction');
      end;
   end
   else
   begin
      FAdvanced := False;
   end;
   Result := Assigned(FCurrentListNode);
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
end;

function TGenericStorableListEnumerator.HasMore(): Boolean;
begin
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
   if (FAdvanced) then
   begin
      Result := Assigned(FCurrentListNode);
   end
   else
   begin
      Assert(Assigned(FCurrentListNode));
      case FDirection of
       tdForward: Result := Assigned(FCurrentListNode^.Next);
       tdReverse: Result := Assigned(FCurrentListNode^.Previous);
       else Assert(False, 'unknown direction');
      end;
   end;
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
end;

function TGenericStorableListEnumerator.InternalGetCurrent(): TItem;
begin
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
   Assert(not FAdvanced);
   Assert(Assigned(FCurrentListNode));
   Result := TItem(FCurrentListNode^.Value);
end;

procedure TGenericStorableListEnumerator.Remove();
var
   Node: PListNode;
   {$IFOPT C+} BeforeLength: Cardinal; {$ENDIF}
begin
   Assert(not FAdvanced);
   Assert(Assigned(FCurrentListNode));
   Assert(Assigned(FList));
   Assert(FList.FActiveEnumerators = 1);
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
   {$IFOPT C+} BeforeLength := FList.Length; {$ENDIF}
   Node := FCurrentListNode;
   case FDirection of
    tdForward: FCurrentListNode := FCurrentListNode^.Next;
    tdReverse: FCurrentListNode := FCurrentListNode^.Previous;
    else Assert(False, 'unknown direction');
   end;
   FAdvanced := True;
   FList.InternalRemoveItem(Node);
   {$IFOPT C+} Assert(FList.Length = BeforeLength-1); {$ENDIF}
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
end;

procedure TGenericStorableListEnumerator.RemoveRemainder();
begin
   Assert(not FAdvanced);
   Assert(Assigned(FCurrentListNode));
   Assert(Assigned(FList));
   Assert(FList.FActiveEnumerators = 1);
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
   FList.InternalRemoveFromItem(FCurrentListNode, FDirection);
   FCurrentListNode := nil;
   FAdvanced := True;
   {$IFOPT C+} FList.CheckLength(); {$ENDIF}
end;


constructor TAbstractStorableList.Create(Flags: TStorableListFlags = []);
begin
   inherited Create();
   Assert(not ((slOwner in Flags) and (slDropDuplicates in Flags)));
   FFlags := Flags;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

destructor TAbstractStorableList.Destroy();
begin
   Assert(FActiveEnumerators = 0);
   {$IFOPT C+} CheckLength(); {$ENDIF}
   if (slOwner in FFlags) then
      FreeItems()
   else
      Empty();
   {$IFOPT C+} CheckLength(); {$ENDIF}
   inherited;
end;

procedure TAbstractStorableList.FreeItems();
var
   CurrentNode: PListNode;
begin
   Assert(slOwner in FFlags);
   {$IFOPT C+} CheckLength(); {$ENDIF}
   CurrentNode := FFirstNode;
   while (Assigned(CurrentNode)) do
   begin
      FFirstNode := CurrentNode^.Next;
      CurrentNode^.Value.Free();
      Dispose(CurrentNode);
      CurrentNode := FFirstNode;
   end;
   Assert(not Assigned(FFirstNode));
   FLastNode := nil;
   FLength := 0;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TAbstractStorableList.Empty();
var
   CurrentNode: PListNode;
begin
   {$IFOPT C+} CheckLength(); {$ENDIF}
   CurrentNode := FFirstNode;
   while (Assigned(CurrentNode)) do
   begin
      FFirstNode := CurrentNode^.Next;
      Dispose(CurrentNode);
      CurrentNode := FFirstNode;
   end;
   Assert(not Assigned(FFirstNode));
   FLastNode := nil;
   FLength := 0;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TAbstractStorableList.Deduplicate();
var
   Scan, Search: PListNode;
begin
   Assert(not (slDropDuplicates in FFlags)); { because why would you call this otherwise? }
   Assert(not (slOwner in FFlags)); { because slOwner is always slDropDuplicates }
   {$IFOPT C+} CheckLength(); {$ENDIF}
   if (not Assigned(FFirstNode)) then
      Exit;
   Scan := FFirstNode^.Next;
   while (Assigned(Scan)) do
   begin
      Search := FFirstNode;
      while (Search^.Value <> Scan^.Value) do
         Search := Search^.Next;
      if (Search <> Scan) then
      begin
         Scan^.Previous^.Next := Scan^.Next;
         if (Assigned(Scan^.Next)) then
         begin
            Scan^.Next^.Previous := Scan^.Previous;
         end
         else
         begin
            Assert(Scan = FLastNode);
            FLastNode := Scan^.Previous;
         end;
         Search := Scan;
         Scan := Scan^.Next;
         Dispose(Search);
         Dec(FLength);
      end
      else
      begin
         Scan := Scan^.Next;
      end;
   end;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TAbstractStorableList.Write(Stream: TWriteStream);
var
   CurrentNode: PListNode;
   {$IFOPT C+} WriteLength: Cardinal; {$ENDIF}
begin
   inherited;
   Assert(SizeOf(FFlags) <= SizeOf(Cardinal));
   {$IFOPT C+} CheckLength(); {$ENDIF}
   Stream.WriteCardinal(Cardinal(FFlags));
   Stream.WriteCardinal(FLength);
   {$IFOPT C+} WriteLength := 0; {$ENDIF}
   CurrentNode := FFirstNode;
   while (Assigned(CurrentNode)) do
   begin
      Assert(Assigned(CurrentNode^.Value));
      if (slOwner in FFlags) then
         Stream.WriteObject(TStorable(CurrentNode^.Value))
      else
         Stream.WriteReference(CurrentNode^.Value);
      CurrentNode := CurrentNode^.Next;
      {$IFOPT C+} Inc(WriteLength); {$ENDIF}
   end;
   Assert(WriteLength = FLength);
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TAbstractStorableList.InternalRemoveItem(Node: PListNode);
begin
   Assert(Assigned(FFirstNode));
   Assert(Assigned(FLastNode));
   Assert(Assigned(Node));
   {$IFOPT C+} CheckLength(); {$ENDIF}
   if (Assigned(Node^.Previous)) then
      Node^.Previous^.Next := Node^.Next
   else
      FFirstNode := Node^.Next;
   if (Assigned(Node^.Next)) then
      Node^.Next^.Previous := Node^.Previous
   else
      FLastNode := Node^.Previous;
   Dispose(Node);
   Dec(FLength);
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TAbstractStorableList.InternalRemoveFromItem(Node: PListNode; Direction: TTraversalDirection);
var
   Garbage: PListNode;
begin
   Assert(Assigned(FFirstNode));
   Assert(Assigned(FLastNode));
   Assert(Assigned(Node));
   {$IFOPT C+} CheckLength(); {$ENDIF}
   case Direction of
    tdForward:
       begin
          if (Assigned(Node^.Previous)) then
          begin
             Node^.Previous^.Next := nil;
             FLastNode := Node^.Previous;
             while (Assigned(Node)) do
             begin
                Garbage := Node;
                Node := Node^.Next;
                Dispose(Garbage);
                Dec(FLength);
             end;
          end
          else
             Empty();
       end;
    tdReverse:
       begin
          if (Assigned(Node^.Next)) then
          begin
             Node^.Next^.Previous := nil;
             FFirstNode := Node^.Next;
             while (Assigned(Node)) do
             begin
                Garbage := Node;
                Node := Node^.Previous;
                Dispose(Garbage);
                Dec(FLength);
             end;
          end
          else
             Empty();
       end;
    else Assert(False, 'unknown direction');
   end;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

{$IFOPT C+}
procedure TAbstractStorableList.CheckLength();
var
   Node: PListNode;
   TestLength: Cardinal;
begin
   Node := FFirstNode;
   TestLength := 0;
   while (Assigned(Node)) do
   begin
      Inc(TestLength);
      Node := Node^.Next;
   end;
   Assert(TestLength = FLength);
   Node := FLastNode;
   TestLength := 0;
   while (Assigned(Node)) do
   begin
      Inc(TestLength);
      Node := Node^.Previous;
   end;
   Assert(TestLength = FLength);
end;
{$ENDIF}


constructor TStorableList.Clone(Template: TAbstractStorableList; Flags: TStorableListFlags = []);
var
   TheirNode, OurNode: PListNode;
begin
   inherited;
   Assert(Assigned(Template));
   Assert(Template is Self.ClassType);
   Assert((not (slOwner in Flags)) or (not (slOwner in Template.FFlags)));
   {$IFOPT C+} CheckLength(); {$ENDIF}
   FFlags := Flags;
   TheirNode := Template.FFirstNode;
   while (Assigned(TheirNode)) do
   begin
      New(OurNode);
      OurNode^.Value := TheirNode^.Value;
      OurNode^.Previous := FLastNode;
      OurNode^.Next := nil;
      if (Assigned(FLastNode)) then     
      begin
         Assert(Assigned(FFirstNode));
         FLastNode^.Next := OurNode;
      end
      else
      begin
         FFirstNode := OurNode;
      end;
      FLastNode := OurNode;
      TheirNode := TheirNode^.Next;
      {$IFOPT C+} Inc(FLength); {$ENDIF}
   end;
   Assert(FLength = Template.Length);
   FLength := Template.Length;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

constructor TStorableList.Read(Stream: TReadStream);
var
   ReadLength, Index: Cardinal;
   {$IFOPT C+} Found: Boolean; {$ENDIF}
begin
   inherited;
   {$IFOPT C+} CheckLength(); {$ENDIF}
   FFlags := TStorableListFlags(Stream.ReadCardinal());
   ReadLength := Stream.ReadCardinal();
   if (ReadLength > 0) then
   begin
      if (slOwner in FFlags) then
      begin
         for Index := 0 to ReadLength-1 do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
            AppendItem(TItem(Stream.ReadObject()));
      end
      else
      begin
         for Index := 0 to ReadLength-1 do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         begin
            AppendItem(nil);
            Assert(Assigned(FFirstNode));
            Assert(Assigned(FLastNode));
            {$IFOPT C+} Found := {$ENDIF} Stream.ReadReference(@FLastNode^.Value);
            {$IFOPT C+} Assert(Found); {$ENDIF}
         end;
      end;
   end;
   Assert(FLength = ReadLength);
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TStorableList.AppendItem(Item: TItem);
var
   NewNode: PListNode;
begin
   Assert((not Assigned(Pointer(Item))) or (Item is TItem.ClassType));
   Assert(FActiveEnumerators = 0);
   {$IFOPT C+}
   if (slOwner in FFlags) then
      Assert(not Contains(Item));
   {$ENDIF}
   {$IFOPT C+} CheckLength(); {$ENDIF}
   if (slDropDuplicates in FFlags) then
   begin
      if (Contains(Item)) then
         Exit;
   end;
   New(NewNode);
   NewNode^.Value := Item;
   NewNode^.Previous := FLastNode;
   if (Assigned(FLastNode)) then
   begin
      Assert(Assigned(FFirstNode));
      FLastNode^.Next := NewNode;
   end
   else
   begin
      Assert(not Assigned(FFirstNode));
      FFirstNode := NewNode;
   end;
   NewNode^.Next := nil;
   FLastNode := NewNode;
   Inc(FLength);
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TStorableList.RemoveItem(Item: TItem);
var
   Node: PListNode;
begin
   Assert(Assigned(Pointer(Item)));
   Assert(Item is TItem.ClassType);
   Assert(Assigned(FFirstNode));
   Assert(Assigned(FLastNode));
   Assert(FActiveEnumerators = 0);
   {$IFOPT C+} CheckLength(); {$ENDIF}
   Node := FFirstNode;
   while ({$IFOPT C+} Assigned(Node) and {$ENDIF} (Node^.Value <> Item)) do
      Node := Node^.Next;
   Assert(Assigned(Node));
   InternalRemoveItem(Node);
   {$IFOPT C+}
   if ((slOwner in FFlags) or (slDropDuplicates in FFlags)) then
      Assert(not Contains(Item));
   {$ENDIF}
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TStorableList.AdoptItem(Enumerator: TEnumerator);
var
   Node: PListNode;
begin
   Assert(Assigned(Pointer(Enumerator)));
   Assert(not Enumerator.FAdvanced);
   Assert(Assigned(Enumerator.FCurrentListNode));
   Assert(Enumerator.FCurrentListNode^.Value is TItem);
   Assert(Assigned(Enumerator.FList));
   Assert(Enumerator.FList is Self.ClassType, 'Enumerator.FList has class ' + Enumerator.FList.ClassName + ' rather than expected type ' + Self.ClassName);
   Assert(Enumerator.FList <> Self);
   Assert(Enumerator.FList.FActiveEnumerators = 1);
   Assert(Enumerator.FList.FFlags = FFlags);
   Assert(FActiveEnumerators = 0);
   Assert(not (slDropDuplicates in FFlags), 'AdoptItem() doesn''t yet check for duplicates');
   {$IFOPT C+} CheckLength(); {$ENDIF}
   Node := Enumerator.FCurrentListNode;
   {$IFOPT C+}
   if (slOwner in FFlags) then
      Assert(not Contains(TItem(Node^.Value)));
   {$ENDIF}
   { First update iterator }
   case TTraversalDirection(Enumerator.FDirection) of
    tdForward: Enumerator.FCurrentListNode := Enumerator.FCurrentListNode^.Next;
    tdReverse: Enumerator.FCurrentListNode := Enumerator.FCurrentListNode^.Previous;
    else Assert(False, 'unknown direction');
   end;
   Enumerator.FAdvanced := True;
   { Then update source list }
   if (not Assigned(Node^.Previous)) then
   begin
      { item is at start of list }
      if (not Assigned(Node^.Next)) then
      begin
         { item is alone }
         Assert(Enumerator.FList.FLength = 1);
         Enumerator.FList.FFirstNode := nil;
         Enumerator.FList.FLastNode := nil;
      end
      else
      begin
         Assert(Enumerator.FList.FLength > 1);
         Enumerator.FList.FFirstNode := Node^.Next;
         Node^.Next^.Previous := nil;
      end;
   end
   else
   if (not Assigned(Node^.Next)) then
   begin
      { item is at end of list }
      Assert(Enumerator.FList.FLength > 1);
      Assert(Assigned(Node^.Previous));
      Enumerator.FList.FLastNode := Node^.Previous;
      Node^.Previous^.Next := nil;
   end
   else
   begin
      { item is in middle }
      Assert(Enumerator.FList.FLength >= 3);
      Assert(Assigned(Node^.Next));
      Assert(Assigned(Node^.Previous));
      Node^.Previous^.Next := Node^.Next;
      Node^.Next^.Previous := Node^.Previous;
   end;
   Dec(Enumerator.FList.FLength);
   { Finally, update us }
   if (Assigned(FLastNode)) then
   begin
      Assert(Assigned(FFirstNode));
      FLastNode^.Next := Node;
   end
   else
   if (not Assigned(FFirstNode)) then
   begin
      Assert(not Assigned(FLastNode));
      FFirstNode := Node;
   end;
   Node^.Previous := FLastNode;
   Node^.Next := nil;
   FLastNode := Node;
   Inc(FLength);
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TStorableList.AdoptList(List: TAbstractStorableList);
begin
   Assert(List is Self.ClassType);
   Assert(List.FActiveEnumerators = 0);
   {$IFOPT C+} CheckLength(); {$ENDIF}
   if (slDropDuplicates in FFlags) then
      InternalMergeList(List)
   else
      InternalAppendList(List);
   Assert(List.FLength = 0);
   Assert(not Assigned(List.FFirstNode));
   Assert(not Assigned(List.FLastNode));
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

{ if this becomes a bottleneck, maybe a hashtable would help }
procedure TStorableList.InternalMergeList(List: TAbstractStorableList);
var
   LastOriginal, CurrentOriginal, CurrentCandidate, NextCandidate: PListNode;
begin
   Assert(List is Self.ClassType);
   Assert(List.FFlags = FFlags);
   {$IFOPT C+} CheckLength(); {$ENDIF}
   if (Assigned(List.FFirstNode)) then
   begin
      Assert(Assigned(List.FLastNode));
      Assert(List.FLength > 0);
      if (Assigned(FLastNode)) then
      begin
         Assert(Assigned(FFirstNode));
         LastOriginal := FLastNode;
         CurrentCandidate := List.FFirstNode;
         while (Assigned(CurrentCandidate)) do
         begin
            NextCandidate := CurrentCandidate^.Next;
            CurrentOriginal := LastOriginal;
            while ((Assigned(CurrentOriginal)) and (CurrentOriginal^.Value <> CurrentCandidate^.Value)) do
               CurrentOriginal := CurrentOriginal^.Previous;
            if (Assigned(CurrentOriginal)) then
            begin
               Assert(CurrentOriginal^.Value = CurrentCandidate^.Value);
               Dispose(CurrentCandidate);
            end
            else
            begin
               FLastNode^.Next := CurrentCandidate;
               CurrentCandidate^.Previous := FLastNode;
               CurrentCandidate^.Next := nil;
               FLastNode := CurrentCandidate;
            end;
            CurrentCandidate := NextCandidate;
         end;
      end
      else
      begin
         Assert(not Assigned(FFirstNode));
         FFirstNode := List.FFirstNode;
         FLastNode := List.FLastNode;
         Inc(FLength, List.FLength);
      end;
      List.FFirstNode := nil;
      List.FLastNode := nil;
      List.FLength := 0;
   end
   else
   begin
      Assert(not Assigned(List.FLastNode));
      Assert(List.FLength = 0);
   end;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

procedure TStorableList.InternalAppendList(List: TAbstractStorableList);
{$IFOPT C+}
var
   Item: PListNode;
{$ENDIF}
begin
   {$IFOPT C+} CheckLength(); {$ENDIF}
   if (Assigned(List.FFirstNode)) then
   begin
      {$IFOPT C+}
      if (slOwner in FFlags) then
      begin
         Item := List.FFirstNode;
         while (Assigned(Item)) do
         begin
            Assert(not Contains(TItem(Item^.Value)));
            Item := Item^.Next;
         end;
      end;
      {$ENDIF}
      Assert(Assigned(List.FLastNode));
      Assert(List.FLength > 0);
      if (Assigned(FLastNode)) then
      begin
         FLastNode^.Next := List.FFirstNode;
         List.FFirstNode^.Previous := FLastNode;
      end
      else
      begin
         FFirstNode := List.FFirstNode;
      end;
      FLastNode := List.FLastNode;
      Inc(FLength, List.FLength);
      List.FFirstNode := nil;
      List.FLastNode := nil;
      List.FLength := 0;
   end
   else
   begin
      Assert(not Assigned(List.FLastNode));
      Assert(List.FLength = 0);
   end;
   {$IFOPT C+} CheckLength(); {$ENDIF}
end;

function TStorableList.Contains(Item: TItem): Boolean;
var
   Candidate: PListNode;
begin
   Assert(Item is TItem.ClassType);
   {$IFOPT C+} CheckLength(); {$ENDIF}
   Candidate := FFirstNode;
   while (Assigned(Candidate) and (Candidate^.Value <> Item)) do
      Candidate := Candidate^.Next;
   Result := Assigned(Candidate);
end;

function TStorableList.GetEnumerator(const Direction: TTraversalDirection = tdForward): TEnumerator;
begin
   {$IFOPT C+} CheckLength(); {$ENDIF}
   case Direction of
    tdForward: Result := TEnumerator.Create(Self, FFirstNode, Direction);
    tdReverse: Result := TEnumerator.Create(Self, FLastNode, Direction);
    else Assert(False, 'unknown direction');
   end;
end;

function TStorableList.InternalGetFirst(): TItem;
begin
   Assert(Assigned(FFirstNode));
   {$IFOPT C+} CheckLength(); {$ENDIF}
   Result := TItem(FFirstNode^.Value);
end;

end.