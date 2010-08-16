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

   TAbstractLinkedList = class(TStorable)
     public
      { these members have to be visible since they are used by the generics below and the generics are compiled outside this unit }
      FFirstNode, FLastNode: PListNode;
      FCount: Cardinal;
      {constructor Create();}
      destructor Destroy(); override;
      procedure FreeItems();
      procedure Write(Stream: TWriteStream); override;
   end;

   generic TGenericLinkedListEnumerator<TItem> = class
     var protected
      FCurrentListNode: PListNode;
      FList: TAbstractLinkedList;
      FDirection: TTraversalDirection;
      FAdvanced: Boolean;
      {$IFOPT C+} FCheckedCurrent: Boolean; {$ENDIF}
     public
      constructor Create(List: TAbstractLinkedList; StartListNode: PListNode; Direction: TTraversalDirection);
      function HasCurrent(): Boolean;
      function GetCurrent(): TItem;
      procedure Advance();
   end;

   generic TLinkedList<TItem, TEnumerator> = class(TAbstractLinkedList)
     public
      constructor Read(Stream: TReadStream); override;
      procedure AppendItem(Item: TItem);
      procedure AdoptItem(Enumerator: TEnumerator);
      procedure AdoptList(List: TAbstractLinkedList); // would be good to have a way to have a compile-time type check here -- see http://mantis.freepascal.org/view.php?id=11777
      function GetEnumerator(const Direction: TTraversalDirection = tdForward): TEnumerator;
   end;

implementation

uses
   sysutils;

{
constructor TAbstractLinkedList.Create();
begin
   inherited;
end;
}

destructor TAbstractLinkedList.Destroy();
var
   CurrentNode: PListNode;
begin
   CurrentNode := FFirstNode;
   while (Assigned(CurrentNode)) do
   begin
      FFirstNode := CurrentNode^.Next;
      Dispose(CurrentNode);
      CurrentNode := FFirstNode;
   end;
   inherited;
end;

procedure TAbstractLinkedList.FreeItems();
var
   CurrentNode: PListNode;
begin
   CurrentNode := FFirstNode;
   while (Assigned(CurrentNode)) do
   begin
      FFirstNode := CurrentNode^.Next;
      CurrentNode^.Value.Free();
      Dispose(CurrentNode);
      CurrentNode := FFirstNode;
   end;
end;

procedure TAbstractLinkedList.Write(Stream: TWriteStream);
var
   CurrentNode: PListNode;
   {$IFOPT C+} Count: Cardinal; {$ENDIF}
begin
   inherited;
   Stream.WriteCardinal(FCount);
   {$IFOPT C+} Count := 0; {$ENDIF}
   CurrentNode := FFirstNode;
   while (Assigned(CurrentNode)) do
   begin
      Stream.WriteObject(TStorable(CurrentNode^.Value));
      CurrentNode := CurrentNode^.Next;
      {$IFOPT C+} Inc(Count); {$ENDIF}
   end;
   Assert(Count = FCount);
end;


constructor TGenericLinkedListEnumerator.Create(List: TAbstractLinkedList; StartListNode: PListNode; Direction: TTraversalDirection);
begin
   inherited Create();
   FList := List;
   FCurrentListNode := StartListNode;
   FDirection := Direction;
end;

function TGenericLinkedListEnumerator.HasCurrent(): Boolean;
begin
   Assert(not FAdvanced);
   {$IFOPT C+}
   if (Assigned(FCurrentListNode)) then
      Assert(Assigned(FList.FFirstNode) and Assigned(FList.FLastNode));
   {$ENDIF}
   Result := Assigned(FCurrentListNode);
   {$IFOPT C+} FCheckedCurrent := True; {$ENDIF}
end;

function TGenericLinkedListEnumerator.GetCurrent(): TItem;
begin
   Assert(not FAdvanced);
   {$IFOPT C+}
   if (Assigned(FCurrentListNode)) then
      Assert(Assigned(FList.FFirstNode) and Assigned(FList.FLastNode));
   {$ENDIF}
   {$IFOPT C+} Assert(FCheckedCurrent); {$ENDIF}
   Assert(Assigned(FCurrentListNode));
   Result := TItem(FCurrentListNode^.Value);
end;

procedure TGenericLinkedListEnumerator.Advance();
begin
   if (not FAdvanced) then
   begin
      {$IFOPT C+} Assert(FCheckedCurrent); {$ENDIF}
      Assert(Assigned(FCurrentListNode));
      {$IFOPT C+}
      if (Assigned(FCurrentListNode)) then
         Assert(Assigned(FList.FFirstNode) and Assigned(FList.FLastNode));
      {$ENDIF}
      case FDirection of
       tdForward: FCurrentListNode := FCurrentListNode^.Next;
       tdReverse: FCurrentListNode := FCurrentListNode^.Previous;
       else raise EAssertionFailed.Create('unknown direction');
      end;
   end
   else
   begin
      FAdvanced := False;
   end;
   {$IFOPT C+} FCheckedCurrent := False; {$ENDIF}
end;



constructor TLinkedList.Read(Stream: TReadStream);
var
   Count, Index: Cardinal;
begin
   inherited;
   Count := Stream.ReadCardinal();
   if (Count > 0) then
      for Index := 0 to Count-1 do
         AppendItem(TItem(Stream.ReadObject()));
   Assert(FCount = Count);
end;

procedure TLinkedList.AppendItem(Item: TItem);
var
   NewNode: PListNode;
begin
   Assert(Item is TItem.ClassType);
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
   Inc(FCount);
end;

procedure TLinkedList.AdoptItem(Enumerator: TEnumerator);
var
   Node: PListNode;
begin
   Assert(not Enumerator.FAdvanced);
   Assert(Enumerator.FCheckedCurrent);
   Assert(Assigned(Enumerator.FCurrentListNode));
   Assert(Enumerator.FCurrentListNode^.Value is TItem);
   Assert(Assigned(Enumerator.FList));
   Assert(Enumerator.FList is Self.ClassType, 'Enumerator.FList has class ' + Enumerator.FList.ClassName + ' rather than expected type ' + Self.ClassName);
   Assert(Enumerator.FList <> Self);
   Node := Enumerator.FCurrentListNode;
   { First update iterator }
   case TTraversalDirection(Enumerator.FDirection) of
    tdForward: Enumerator.FCurrentListNode := Enumerator.FCurrentListNode^.Next;
    tdReverse: Enumerator.FCurrentListNode := Enumerator.FCurrentListNode^.Previous;
    else raise EAssertionFailed.Create('unknown direction');
   end;
   Enumerator.FAdvanced := True;
   { Then update source list }
   if (not Assigned(Node^.Previous)) then
   begin
      { item is at start of list }
      if (not Assigned(Node^.Next)) then
      begin
         { item is alone }
         Assert(Enumerator.FList.FCount = 1);
         Enumerator.FList.FFirstNode := nil;
         Enumerator.FList.FLastNode := nil;
      end
      else
      begin
         Assert(Enumerator.FList.FCount > 1);
         Enumerator.FList.FFirstNode := Node^.Next;
         Node^.Next^.Previous := nil;
      end;
   end
   else
   if (not Assigned(Node^.Next)) then
   begin
      { item is at end of list }
      Assert(Enumerator.FList.FCount > 1);
      Assert(Assigned(Node^.Previous));
      Enumerator.FList.FLastNode := Node^.Previous;
      Node^.Previous^.Next := nil;
   end
   else
   begin
      { item is in middle }
      Assert(Enumerator.FList.FCount >= 3);
      Assert(Assigned(Node^.Next));
      Assert(Assigned(Node^.Previous));
      Node^.Previous^.Next := Node^.Next;
      Node^.Next^.Previous := Node^.Previous;
   end;
   Dec(Enumerator.FList.FCount);
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
   Inc(FCount);
end;

procedure TLinkedList.AdoptList(List: TAbstractLinkedList);
begin
   Assert(List is Self.ClassType);
   if (Assigned(List.FFirstNode)) then
   begin
      Assert(Assigned(List.FLastNode));
      Assert(List.FCount > 0);
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
      Inc(FCount, List.FCount);
      List.FFirstNode := nil;
      List.FLastNode := nil;
      List.FCount := 0;
   end
   else
   begin
      Assert(not Assigned(List.FLastNode));
      Assert(List.FCount = 0);
   end;
end;

function TLinkedList.GetEnumerator(const Direction: TTraversalDirection = tdForward): TEnumerator;
begin
   case Direction of
    tdForward: Result := TEnumerator.Create(Self, FFirstNode, Direction);
    tdReverse: Result := TEnumerator.Create(Self, FLastNode, Direction);
    else raise EAssertionFailed.Create('unknown direction');
   end;
end;

end.