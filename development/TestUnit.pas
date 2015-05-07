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
    TopGreatherNode: PCPFNode;
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
  PFindPathLoopStore = ^TFindPathLoopStore;


implementation


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
  Right: PCPFNode;
  {$if (not Defined(CPUX86)) or Defined(FPC)}
    Left: PCPFNode;
  {$ifend}

  {$ifNdef CPUX86}
    Buffer: ^TMapNodeBuffer;
    TopGreatherNode: PCPFNode;
  {$endif}

  Store: TFindPathLoopStore;
begin
  Move(S, Store, SizeOf(Store));

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
    {$ifdef CPUX86}Store.{$endif}TopGreatherNode := Store.Top.Node;
    PBufferCurrent := @{$ifdef CPUX86}Store.{$endif}Buffer[0];
    repeat
      // make same sort value list (ChildNode..Node)
      ChildNode := PBufferCurrent^;
      Inc(PBufferCurrent);
      Node := ChildNode;
      ChildSortValue := ChildNode.SortValue;
      while (PBufferCurrent <> PBufferHigh) do
      begin
        Right := PBufferCurrent^;
        if (Right.SortValue <> ChildSortValue) then Break;

        Node.Next := Right;
        Right.Prev := Node;

        Inc(PBufferCurrent);
        Node := Right;
      end;

      // insertion kinds
      if (ChildSortValue > Store.Current.SortValue) then
      begin
        if (ChildSortValue <= Store.Top.SortValue) then
        begin
          // before top
          Right := Store.Top.Node;
          Store.Top.Node := ChildNode;
          Store.Top.SortValue := ChildSortValue;
        end else
        begin
          // greater then top
          Right := {$ifdef CPUX86}Store.{$endif}TopGreatherNode;
          while (Right.SortValue < ChildSortValue) do Right := Right.Next;
          {$ifdef CPUX86}Store.{$endif}TopGreatherNode := Right;
        end;
      end else
      begin
        // after current
        Right := Store.Current.Node.Next;
      end;

      // insertion
      {$if (not Defined(CPUX86)) or Defined(FPC)}
        Left := Right.Prev;
        Node.Next := Right;
        ChildNode.Prev := Left;
        Left.Next := ChildNode;
        Right.Prev := Node;
      {$else}
        Node.Next := Right;
        with Right^ do
        begin
          ChildNode.Prev := Prev{Left};
          ChildNode.Prev{Left}.Next := ChildNode;
          Prev := Node;
        end;
      {$ifend}
    until (PBufferCurrent = PBufferHigh);

next_current:
  Move(Store, Store.Self^, SizeOf(Store));
end;

var
  NODE_INDEX: Integer = 0;
  ITERATION: Integer = 0;

procedure InsertNodes(var S: TFindPathLoopStore; const SortValues: array of Cardinal);
var
  i: Integer;
  Node: PCPFNode;
  Number: Integer;

  LastV, LastIter, LastSubIter,
  V, Iter, SubIter: Cardinal;
  Good: Boolean;

  function PtrToStr(P: Pointer): string;
  begin
    if (P = nil) then Result := 'nil'
    else
    Result := IntToHex(NativeUInt(P), 8);
  end;

  function NodeToString(const N: PCPFNode): string;
  begin
    Result := Format('%s  Value: %u, Iteration: %d-%d'{, Prev: %s, Next: %s'},
      [PtrToStr(N), N.SortValue, N.Path shr 16, N.Path and $ffff{,
       PtrToStr(N.Prev), PtrToStr(N.Next)}]);
  end;

  procedure WritelnCurrentTop;
  begin
    Writeln(Format('Current = %s (%u/%u), Top = %s (%u/%u)',
      [
        PtrToStr(S.Current.Node), S.Current.Node.SortValue, S.Current.SortValue,
        PtrToStr(S.Top.Node), S.Top.Node.SortValue, S.Top.SortValue
      ]));
  end;
begin
  Writeln;
  Inc(ITERATION);
  Write(ITERATION, ') send: [');
  for i := 0 to High(SortValues) do
  begin
    New(Node);
    Inc(NODE_INDEX);
    Node.SortValue := SortValues[i];
    Node.Path := (ITERATION shl 16) + (i + 1);
    Node.Prev := nil;
    Node.Next := nil;

    S.Buffer[i] := Node;

    if (i <> 0) then Write(', ');
    Write(SortValues[i]{Format('%u:%p', [SortValues[i], Node])});
  end;
  Writeln(']');
//  WritelnCurrentTop;

  // call test function
  Writeln('...');
  __InsertNodes(S, Length(SortValues) shl 16);

  // result
//  WritelnCurrentTop;
  LastV := 0;
  LastIter := $ffff;
  LastSubIter := $ffff;
  Number := 0;
  Node := PCPFNode(S.HexagonalFlag).Next;
  while (Node.SortValue <> High(Cardinal)) do
  begin
    if (Number > NODE_INDEX{ + 2}) then
    begin
      Writeln('FAIL !!!');
      Break;
    end;

    Inc(Number);
    Write(NodeToString(Node));

    V := Node.SortValue;
    Iter := Node.Path shr 16;
    SubIter := Node.Path and $ffff;
    Good := True;

    if (LastV > V) then Good := False;
    if (LastV = V) then
    begin
      if (LastIter < Iter) then Good := False;
      if (LastIter = Iter) then
      begin
        if (LastSubIter > SubIter) then Good := False;
      end;
    end;

    if (not Good) then
    begin
      Write(' FAIL !!!');
    end;

    LastV := V;
    LastIter := Iter;
    LastSubIter := SubIter;
    Writeln;
//    if (Node.SortValue = High(Cardinal)) then Break;
    Node := Node.Next;
  end;
  if (Number <> NODE_INDEX{ + 2}) then
    Writeln('FAIL NUMBER');

  WritelnCurrentTop;
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
  First.Prev := nil;
  First.Next := @Last;
  Last.Prev := @First;
  Last.Next := nil;

  FillChar(S, SizeOf(S), 0);
  S.Current.Node := @First;
  S.Current.SortValue := 0;
  S.Top.Node := @Last;
  S.Top.SortValue := High(Cardinal);

  S.Self := @S;
  S.HexagonalFlag := NativeUInt(@First);

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
