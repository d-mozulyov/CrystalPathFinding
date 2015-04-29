unit TestUnit;

{$i crystal_options.inc}

interface
  uses
  {$ifdef MSWINDOWS}
    {$if Defined(FPC) or (CompilerVersion < 23)}
      Windows, ClipBrd,
    {$else}
      Winapi.Windows, Vcl.ClipBrd,
    {$ifend}
  {$endif}
  SysUtils,
  CrystalPathFinding;



procedure RUN;
procedure ShowMessage(const S: string); overload;
procedure ShowMessage(const StrFmt: string; const Args: array of const); overload;

implementation

type
  TMapNodeBuffer = array[0..7] of PCPFNode;
  PMapNodeBuffer = ^TMapNodeBuffer;

  TFindPathLoopStore = record
    Buffer: TMapNodeBuffer;
    Self: Pointer;
    HexagonalFlag: NativeUInt;
    Info: TCPFInfo;

    {$ifdef CPUX86}
    ChildList: PWord;
    {$endif}

    Current: record
      Node: PCPFNode;
      Cell: PCPFCell;
      Coordinates: TCPFPoint;
      SortValue: Cardinal;
      Path: Cardinal;
    end;
    Top: record
      Node: PCPFNode;
      SortValue: Cardinal;
    end;
  end;


procedure __InsertNodes(var S: TFindPathLoopStore; NodeInfo: NativeUInt);
label
  next_current;
const
  COUNTER_OFFSET = 16;
var
  Node: PCPFNode;
  ChildNode: PCPFNode;
  ChildSortValue: Cardinal;

  PBufferHigh, PBufferBase, PBufferCurrent: ^PCPFNode;

  {$ifNdef CPUX86}
    Buffer: ^TMapNodeBuffer;
  {$endif}

  Store: TFindPathLoopStore;
begin
  //StoreEx.S := @S;
  Store.ChildList := Pointer(@S);
  Move(Store.ChildList^, Store, SizeOf(Store));

  {$ifNdef CPUX86}
    Buffer := @Store.Buffer;
  {$endif}

    // move buffered nodes to opened list
    if (NodeInfo and ($f shl COUNTER_OFFSET) = 0) then
      goto next_current;

    // internal child buffer sort by SortValue
    {
       for i = 1 to Count - 1 do
       begin
         for j := i-1 downto 0 do
         if (Buffer[j].SortValue > Buffer[j+1].SortValue) then
         begin
           Swap(Buffer[j], Buffer[j+1]);
         end else
         begin
           Break;
         end;
       end;
    }
    PBufferHigh := @{$ifdef CPUX86}Store.{$endif}Buffer[(NodeInfo shr COUNTER_OFFSET) and $f];
    PBufferBase := @{$ifdef CPUX86}Store.{$endif}Buffer[1];
    PBufferCurrent := @{$ifdef CPUX86}Store.{$endif}Buffer[0];
    while (PBufferBase <> PBufferHigh) do
    begin
      ChildNode := PBufferBase^;
      ChildSortValue := ChildNode.SortValue;

      Node := PBufferCurrent^;
      if (Node.SortValue > ChildSortValue) then
      begin
       repeat
          PMapNodeBuffer(PBufferCurrent)[1] := Node;
          if (PBufferCurrent = @{$ifdef CPUX86}Store.{$endif}Buffer[0]) then Break;

          Dec(PBufferCurrent);
          Node := PBufferCurrent^;
          if (Node.SortValue > ChildSortValue) then Continue;
          Inc(PBufferCurrent);
          Break;
        until (False);

        PBufferCurrent^ := ChildNode;
      end;

      PBufferCurrent := PBufferBase;
      Inc(PBufferBase);
    end;

    // insert sorted nodes
    // todo


next_current:
  Move(Store, Store.ChildList^, SizeOf(Store));
end;

var
  ITERATION: Integer = 0;

procedure InsertNodes(var S: TFindPathLoopStore; const SortValues: array of Cardinal);
var
  i: Integer;
  Node: PCPFNode;
begin
  Writeln;
  Inc(ITERATION);
  Write(ITERATION, ') send: [');
  for i := 0 to High(SortValues) do
  begin
    if (i <> 0) then Write(', ');
    Write(SortValues[i]);

    GetMem(Node, SizeOf(Node));
    Node.SortValue := SortValues[i];
    Node.Path := (ITERATION shl 16) + (i + 1);

    S.Buffer[i] := Node;
  end;
  Writeln(']');

  // call test function
  __InsertNodes(S, Length(SortValues) shl 16);

  // result
  Node := PCPFNode(S.Self).Next;
  while (Node.SortValue <> High(Cardinal)) do
  begin
    Writeln(Format('  Value: %d, Interation: %d-%d',
      [Node.SortValue, Node.Path shr 16, Node.Path and $ffff]));

    Node := Node.Next;
  end;
end;

procedure RUN_Inserts(var S: TFindPathLoopStore);
begin
  InsertNodes(S, [3, 3, 5, 4, 0, 0, 7]);
  InsertNodes(S, [3, 0, 0, 8, 6, 2, 3, 2]);
  InsertNodes(S, [0, 1, 2, 3, 4, 5, 8, 9]);
  InsertNodes(S, [1, 8, 9, 10]);
end;

procedure RUN;
var
  S: TFindPathLoopStore;

  First, Last: TCPFNode;
begin
  First.SortValue := 0;
  First.Path := 0;
  Last.SortValue := High(Cardinal);
  Last.Path :=  0;
  First.Next := @Last;
  Last.Prev := @First;

  S.Current.Node := @First;
  S.Current.SortValue := 0;
  S.Top.Node := @Last;
  S.Top.SortValue := High(Cardinal);

  S.Self := Pointer(@First);

  RUN_Inserts(S);

  Writeln;
  Write('Press Enter to quit');
  Readln;
//  ShowMessage('Test');
end;

procedure ShowMessage(const S: string);
var
  BreakPoint: string;
begin
  BreakPoint := S;

  {$ifdef MSWINDOWS}
    Clipboard.AsText := S;
    MessageBox(0, PChar(BreakPoint), 'Сообщение:', 0);
  {$endif}

  Halt;
end;

procedure ShowMessage(const StrFmt: string; const Args: array of const);
begin
  ShowMessage(Format(StrFmt, Args));
end;

initialization


end.
