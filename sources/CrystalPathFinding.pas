unit CrystalPathFinding;

{******************************************************************************}
{ Copyright (c) 2011-2015 Dmitry Mozulyov                                      }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    }
{ THE SOFTWARE.                                                                }
{                                                                              }
{ email: softforyou@inbox.ru                                                   }
{ skype: dimandevil                                                            }
{ repository: https://github.com/d-mozulyov/CrystalPathFinding                 }
{******************************************************************************}

{ *********************************************************************** }
{ "Crystal Path Finding" (cpf) is a very small part of CrystalEngine,     }
{ that helps to find the shortest paths with A*/WA* algorithms.           }
{ *********************************************************************** }


{.$define CPFLOG}
{$define CPFAPI}
{$define CPFLIB}

{$ifdef CPFLIB}
  {$define CPFAPI}  
  {$undef CPFLOG}
{$endif}

// compiler directives
{$ifdef FPC}
  {$mode Delphi}
  {$asmmode Intel}
{$endif}
{$if CompilerVersion >= 24}
  {$LEGACYIFEND ON}
{$ifend}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$if CompilerVersion >= 15}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ifend}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$if (CompilerVersion < 23) and (not Defined(FPC))}
  {$define CPUX86}
{$ifend}
{$if (Defined(FPC)) or (CompilerVersion >= 17)}
  {$define INLINESUPPORT}
{$ifend}
{$if Defined(CPUX86) or Defined(CPUX64)}
   {$define CPUINTEL}
{$ifend}
{$if SizeOf(Pointer) = 8}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$if CompilerVersion >= 21}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ifend}
{$if (not Defined(FPC)) and (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
  {$define INTERNALCODEPAGE}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}


interface
  uses Types{$ifNdef CPFLIB}, SysUtils{$endif} {$ifdef CPFLOG}, Classes{$endif};

type
  // standard types
  {$ifdef FPC}
    PUInt64 = ^UInt64;
  {$else}
    {$if CompilerVersion < 15}
      UInt64 = Int64;
      PUInt64 = ^UInt64;
    {$ifend}
    {$if CompilerVersion < 19}
      NativeInt = Integer;
      NativeUInt = Cardinal;
    {$ifend}
    {$if CompilerVersion < 22}
      PNativeInt = ^NativeInt;
      PNativeUInt = ^NativeUInt;
    {$ifend}
  {$endif}
  {$if Defined(FPC) or (CompilerVersion < 23)}
  TExtended80Rec = Extended;
  PExtended80Rec = ^TExtended80Rec;
  {$ifend}

  // exception class
  {$ifNdef CPFLIB}
  ECrystalPathFinding = class(Exception)
  {$ifdef KOL}
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: NativeUInt); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    constructor CreateResFmt(Ident: NativeUInt; const Args: array of const); overload;
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const); overload;
  {$endif}
  end;
  {$endif}

  // map tile
  TPathMapTile = type byte;
  PPathMapTile = ^TPathMapTile;

  // map kind
  TPathMapKind = (mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal);
  PPathMapKind = ^TPathMapKind;

  // points as array
  PPointList = ^TPointList;
  TPointList = array[0..high(integer) div sizeof(TPoint) - 1] of TPoint;

  // result of find path function
  TPathMapResult = record
    Points: PPointList;
    Count: Cardinal;
    Distance: Double;
  end;
  PPathMapResult = ^TPathMapResult;


  //
  TPathMapWeights = {$ifdef CPFLIB}object{$else}class(TObject){$endif}
  private

  {$ifdef CPFLIB}
  public
    procedure Destroy;
  {$else}
    {$ifdef AUTOREFCOUNT}protected{$else}public{$endif}
    destructor Destroy; override;
  {$endif}
  public
    {$ifdef CPFLIB}procedure{$else}constructor{$endif}
    Create(const AHighTile: TPathMapTile);


  end;

  // compact cell coordinates
  PWPoint = ^TWPoint;
  TWPoint = packed record
    X: Word;
    Y: Word;
  end;

  // map cell
  TPathMapCell = packed record
    Tile: TPathMapTile;
    Mask: Byte;
    NodePtr: Cardinal;
  end;
  PPathMapCell = ^TPathMapCell;
  TPathMapCellArray = array[0..0] of TPathMapCell;
  PPathMapCellArray = ^TPathMapCellArray;

  // node type
  PPathMapNode = ^TPathMapNode;
  TPathMapNode = packed record
    Value: Cardinal; // todo оценка стоимости пути. F = G+H
    Path: Cardinal; // todo стоимость пути от стартовой точки. G. = F-H
    Point: TWPoint;  // cell coordinates
    Prev, Next: PPathMapNode;

    case Integer of
    0: (
         Tile: Byte;
         Mask: Byte;
         ParentMask: Byte;
         Parent: Byte;
       );
    1: (
         NodeInfo: Cardinal;
       );
  end;


  TPathMapNodeStorage = record
    NewNode: PPathMapNode;
    MaximumNode: PPathMapNode;

    Buffers: array[0..31] of Pointer;
    Current: NativeUInt;
    Allocated: NativeUInt;

    {$ifdef LARGEINT}
    // todo
    LargeModifier: NativeInt;
    {$endif}
  end;

  TPathMapInfo = record
    Cells: PPathMapCellArray;
    MapWidth: NativeInt;
    HeuristicsLine: NativeInt;
    HeuristicsDiagonal: NativeInt;
    //PointOffsets: array[0..7] of Cardinal;
    //CellOffsets: array[0..7] of NativeInt;

    //Nodes

    NodeStorage: TPathMapNodeStorage;

    StartPoint: TWPoint;
    FinishPoint: TWPoint;
  end;





  //
  TPathMap = {$ifdef CPFLIB}object{$else}class(TObject){$endif}
  private
    FInfo: TPathMapInfo;
    FFindResult: TPathMapResult;


    function DoFindPath: Boolean;
  {$ifdef CPFLIB}
  public
    procedure Destroy;
  {$else}
    {$ifdef AUTOREFCOUNT}protected{$else}public{$endif}
    destructor Destroy; override;
  {$endif}
  public
    {$ifdef CPFLIB}procedure{$else}constructor{$endif}
    Create(const AWidth, AHeight: Word; const AKind: TPathMapKind; const AHighTile: TPathMapTile);


  end;


{$ifdef CPFAPI}
type
  PCPFHandle = ^TCPFHandle;
  TCPFHandle = type NativeUInt;

  {$ifdef CPFLIB}
  TCPFAlloc = function(Size: NativeUInt): Pointer; cdecl;
  TCPFFree = procedure(P: Pointer); cdecl;
  TCPFRealloc = function(P: Pointer; Size: NativeUInt): Pointer; cdecl;
  TCPFException = procedure(Message: PWideChar; Address: Pointer); cdecl;
  TCPFCallbacks = packed record
    Alloc: TCPFAlloc;
    Free: TCPFFree;
    Realloc: TCPFRealloc;
    Exception: TCPFException;
  end;
  procedure cpfInitialize(const Callbacks: TCPFCallbacks); cdecl;
  {$endif}

  function  cpfCreateWeights(HighTile: Byte): TCPFHandle; cdecl;
  procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl;
  function  cpfWeightGet(HWeights: TCPFHandle; Tile: Byte): Single; cdecl;
  procedure cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single); cdecl;
  function  cpfCreateMap(Width, Height: Word; Kind: TPathMapKind = mkSimple; HighTile: Byte = 0): TCPFHandle; cdecl;
  procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl;
  procedure cpfMapClear(HMap: TCPFHandle); cdecl;
  procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt = 0); cdecl;
  function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): Byte; cdecl;
  procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte);
  function  cpfFindPath(HMap: TCPFHandle; Start, Finish: TPoint; HWeights: TCPFHandle = 0; ExcludePoints: PPoint = nil; ExcludePointsCount: NativeUInt = 0; SectorTest: Boolean = True; UseCache: Boolean = True): PPathMapResult; cdecl;
{$endif}

implementation
{$ifNdef CPFLIB}
  {$ifNdef KOL}uses SysConst{$endif};

var
  // todo FPC
  MemoryManager: {$if CompilerVersion < 18}TMemoryManager{$else}TMemoryManagerEx{$ifend};
{$endif}


{ ECrystalPathFinding }

{$if Defined(KOL) and (not Defined(CPFLIB)))}
constructor ECrystalPathFinding.Create(const Msg: string);
begin
  inherited Create(e_Custom, Msg);
end;

constructor ECrystalPathFinding.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, Msg, Args);
end;

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Str: string;
  end;

function EnumStringModules(Instance: NativeInt; Data: Pointer): Boolean;
var
  Buffer: array [0..1023] of Char;
begin
  with PStrData(Data)^ do
  begin
    SetString(Str, Buffer, Windows.LoadString(Instance, Ident, Buffer, sizeof(Buffer)));
    Result := Str = '';
  end;
end;

function FindStringResource(Ident: Integer): string;
var
  StrData: TStrData;
  Func: TEnumModuleFunc;
begin
  StrData.Ident := Ident;
  StrData.Str := '';
  Pointer(@Func) := @EnumStringModules;
  EnumResourceModules(Func, @StrData);
  Result := StrData.Str;
end;

function LoadStr(Ident: Integer): string;
begin
  Result := FindStringResource(Ident);
end;

constructor ECrystalPathFinding.CreateRes(Ident: NativeUInt);
begin
  inherited Create(e_Custom, LoadStr(Ident));
end;

constructor ECrystalPathFinding.CreateRes(ResStringRec: PResStringRec);
begin
  inherited Create(e_Custom, System.LoadResString(ResStringRec));
end;

constructor ECrystalPathFinding.CreateResFmt(Ident: NativeUInt;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, LoadStr(Ident), Args);
end;

constructor ECrystalPathFinding.CreateResFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, System.LoadResString(ResStringRec), Args);
end;
{$ifend}


{$if (not Defined(FPC)) and (CompilerVersion < 21)}
function ReturnAddress: Pointer;
asm
  mov eax, [ebp+4]
end;
{$ifend}


{$ifdef CPFLIB}
var
  CPFCallbacks: TCPFCallbacks;
{$endif}

type
  TPathMapWeightsPtr = {$ifdef CPFLIB}^{$endif}TPathMapWeights;
  TPathMapPtr = {$ifdef CPFLIB}^{$endif}TPathMap;
  TExceptionString = {$ifdef CPFLIB}PWideChar{$else}string{$endif};

procedure CPFException(const Message: TExceptionString; const Address: Pointer);
begin
  {$ifdef CPFLIB}
    if Assigned(CPFCallbacks.Exception) then
      CPFCallbacks.Exception(Message, Address);

     // todo Assert?
  {$else}
     raise ECrystalPathFinding.Create(Message) at Address;
  {$endif}
end;

{$ifdef CPFLIB}
procedure CPFExceptionFmt(const Fmt: PWideChar; const Args: array of Integer;
   const Address: Pointer);
var
  TextBuffer: array[0..2048 - 1] of WideChar;
  Dest, Src: PWideChar;
  X: Cardinal;
  Arg: Integer;
  L, R: PWideChar;
  C: WideChar;
begin
  Dest := @TextBuffer[0];
  Src := Fmt;
  Arg := 0;

  if (Src <> nil) then
  while (Src^ <> #0) do
  begin
    if (Src^ = '%') then
    begin
      if (Word(Src[1]) or $20 = Word('d')) and (Arg <= High(Args)) then
      begin
        Inc(Src, 2);
        X := Args[Arg];
        Inc(Arg);

        // sign
        if (Integer(X) < 0) then
        begin
          Dest^ := '-';
          Inc(Dest);
          X := Cardinal(-Integer(X));
        end;

        // fill inverted
        L := Dest;
        repeat
          PWord(Dest)^ := Word('0') + X mod 10;
          Inc(Dest);
          X := X div 10;
        until (X = 0);

        // invert
        R := Dest;
        Dec(R);
        while (NativeUInt(L) < NativeUInt(R)) do
        begin
          C := R^;
          R^ := L^;
          L^ := C;

          Inc(L);
          Dec(R);
        end;

        // scan next
        Continue;
      end;
    end;

    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;

  Dest^ := #0;
  CPFException(@TextBuffer[0], Address);
end;
{$else !CPFLIB}
procedure CPFExceptionFmt(const Fmt: string; const Args: array of const;
   const Address: Pointer);
begin
  CPFException(Format(Fmt, Args), Address);
end;
{$endif}

{$ifdef CPFLIB}
procedure RaiseCallbacks(const Address: Pointer);
begin
  CPFException('Callbacks not defined', Address);
end;
{$endif}

procedure RaiseOutOfMemory(const Address: Pointer);
begin
{$ifdef CPFLIB}
  CPFException('Out of memory', Address);
{$else}
  {$ifdef KOL}
    raise Exception.Create(e_OutOfMem, SOutOfMemory) at Address;
  {$else}
    raise EOutOfMemory.Create(SOutOfMemory) at Address;
  {$endif}
{$endif}
end;

procedure RaiseInvalidPointer(const Address: Pointer);
begin
{$ifdef CPFLIB}
  CPFException('Invalid pointer operation', Address);
{$else}
  {$ifdef KOL}
    raise Exception.Create(e_InvalidPointer, SInvalidPointer) at Address;
  {$else}
    raise EInvalidPointer.Create(SInvalidPointer) at Address;
  {$endif}
{$endif}
end;

function CPFAlloc(const Size: NativeUInt; const Address: Pointer): Pointer;
begin
  if (Size = 0) then
  begin
    Result := nil;
    Exit;
  end;

  {$ifdef CPFLIB}
    if not Assigned(CPFCallbacks.Alloc) then
      RaiseCallbacks(Address);

    Result := CPFCallbacks.Alloc(Size);
  {$else}
    Result := MemoryManager.GetMem(Size);
  {$endif}

  if (Result = nil) then
    RaiseOutOfMemory(Address);
end;

procedure CPFFree(const P: Pointer; const Address: Pointer);
begin
  if (P <> nil) then
  begin
  {$ifdef CPFLIB}
    if not Assigned(CPFCallbacks.Free) then
      RaiseCallbacks(Address);

    CPFCallbacks.Free(P);
  {$else}
    if (MemoryManager.FreeMem(P) <> 0) then
      RaiseInvalidPointer(Address);
  {$endif}
  end;
end;

function CPFRealloc(const P: Pointer; const Size: NativeUInt; const Address: Pointer): Pointer;
begin
  if (P = nil) then
  begin
    Result := CPFAlloc(Size, Address);
  end else
  if (Size = 0) then
  begin
    CPFFree(P, Address);
    Result := nil;
  end else
  begin
    {$ifdef CPFLIB}
      if not Assigned(CPFCallbacks.Realloc) then
        RaiseCallbacks(Address);

      Result := CPFCallbacks.Realloc(P, Size);
    {$else}
      Result := MemoryManager.ReallocMem(P, Size);
    {$endif}

    if (Result = nil) then
      RaiseOutOfMemory(Address);
  end;
end;


{$ifdef CPFLIB}
procedure cpfInitialize(const Callbacks: TCPFCallbacks); cdecl;
var
  Address: Pointer;
  Done: Boolean;
begin
  Address := ReturnAddress;

  with Callbacks do
  Done := Assigned(Alloc) and Assigned(Free) and Assigned(Realloc) and (Assigned(Exception));

  if (not Done) then
  begin
    if (not Assigned(CPFCallbacks.Exception)) then
      CPFCallbacks.Exception := Callbacks.Exception;

    RaiseCallbacks(Address);
  end;

  CPFCallbacks := Callbacks;
end;
{$endif .CPFLIB}


{$ifdef CPFAPI}
function  cpfCreateWeights(HighTile: Byte): TCPFHandle; cdecl;
begin
  // todo
  Result := 0;
end;

procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl;
begin
  // todo

  HWeights := 0;
end;

function  cpfWeightGet(HWeights: TCPFHandle; Tile: Byte): Single; cdecl;
begin
  // todo
  Result := 0;
end;

procedure cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single); cdecl;
begin
  // todo
end;

function  cpfCreateMap(Width, Height: Word; Kind: TPathMapKind = mkSimple; HighTile: Byte = 0): TCPFHandle; cdecl;
begin
  // todo
  Result := 0;
end;

procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl;
begin
  // todo

  HMap := 0;
end;

procedure cpfMapClear(HMap: TCPFHandle); cdecl;
begin
  // todo
end;

procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt = 0); cdecl;
begin
  // todo
end;

function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): Byte; cdecl;
begin
  // todo
  Result := 0;
end;

procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte);
begin
  // todo
end;

function  cpfFindPath(HMap: TCPFHandle; Start, Finish: TPoint; HWeights: TCPFHandle = 0; ExcludePoints: PPoint = nil; ExcludePointsCount: NativeUInt = 0; SectorTest: Boolean = True; UseCache: Boolean = True): PPathMapResult; cdecl;
begin
  // todo
  Result := nil;
end;
{$endif .CPFAPI}





const
  POINT_OFFSETS: array[0..7] of Types.TSmallPoint = (
    {0} (x: -1; y: -1),
    {1} (x:  0; y: -1),
    {2} (x: +1; y: -1),
    {3} (x: +1; y:  0),
    {4} (x: +1; y: +1),
    {5} (x:  0; y: +1),
    {6} (x: -1; y: +1),
    {7} (x: -1; y:  0)
  );


{ TPathMapWeights }

{$ifdef CPFLIB}procedure{$else}constructor{$endif}
  TPathMapWeights.Create(const AHighTile: TPathMapTile);
begin
  {$ifNdef CPFLIB}
    inherited Create;
  {$endif}


end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TPathMapWeights.Destroy;
begin


  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;



{ TPathMap }

{$ifdef CPFLIB}procedure{$else}constructor{$endif}
  TPathMap.Create(const AWidth, AHeight: Word; const AKind: TPathMapKind;
    const AHighTile: TPathMapTile);
begin
  {$ifNdef CPFLIB}
    inherited Create;
  {$endif}


end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TPathMap.Destroy;
begin


  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;

procedure CopyInfo(var Dest, Src: TPathMapInfo);
begin
  Dest := Src;
end;

function TPathMap.DoFindPath: Boolean;
//label
//  loop;
var
  Node: PPathMapNode;
  NodeInfo: NativeUInt;
  Childs: PWord;
  Child: NativeUInt;
  NodeXY, OffsetXY, ChildXY: Cardinal;
  ChildCell: PPathMapCell;
  ChildNodePtr: NativeUInt;
  ChildNode: PPathMapNode;
  Path: Cardinal;

  Store: record
    Buffer: array[0..7] of PPathMapNode;
    Self: Pointer;
    Info: TPathMapInfo;

    Node: PPathMapNode;
    NodeXY: TWPoint;
  end;
begin
//  Store.
  Store.Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});
  //Store.Info := TPathMapPtr(Store.Self).FInfo;
  CopyInfo(Store.Info, TPathMapPtr(Store.Self).FInfo);

  // todo
  //Node := nil;
  Store.NodeXY := Store.Info.StartPoint;
  NodeInfo := 0;


  repeat
    // Childs todo
    Childs := nil;

    // each child cell loop
    if (NodeInfo and $ff <> 0) then
    repeat
      // first available
      Child := Childs^;
      Inc(Childs);
      if (NodeInfo and Child = 0) then Continue;

      // clear flag, child number
      NodeInfo := NodeInfo and (not Child);
      Child := Child shr (8 + 4);

      // ChildXY coordinates
      OffsetXY := Cardinal(POINT_OFFSETS[Child]);
      NodeXY := Cardinal(Store.NodeXY{Node.Point});
      ChildXY := NodeXY + OffsetXY;
      OffsetXY := OffsetXY and $ffff0000;
      NodeXY := NodeXY and $ffff0000;
      ChildXY := Word(ChildXY) + NodeXY + OffsetXY;

      // found test
      if (ChildXY = Cardinal(Store.Info.FinishPoint)) then
      begin
        Result := True;
        Exit;
        // todo
      end;

      // ChildXY map cell
      ChildCell := @Store.Info.Cells[(NativeInt(ChildXY) shr 16) * Store.Info.MapWidth + Word(ChildXY)];
      ChildNodePtr := ChildCell.NodePtr;
      if (ChildNodePtr and 1 <> 0{Fixed}) then
      begin
        if (NodeInfo and $ff <> 0) then Continue;
        Break;
      end;

      // Path?
      Path := 0;

      // ChildNodePtr --> ChildNode
      if (ChildNodePtr = 0) then
      begin
        // allocate new node
        ChildNode := Store.Info.NodeStorage.NewNode;
        {$ifdef LARGEINT}
          ChildCell.NodePtr := NativeUInt(NativeInt(ChildNode) + Store.Info.NodeStorage.LargeModifier);
        {$else}
          ChildCell.NodePtr := NativeUInt(ChildNode);
        {$endif}

        Cardinal(ChildNode.Point) := ChildXY;
        // heuristics todo and so

        if (ChildNode = Store.Info.NodeStorage.MaximumNode) then
        begin
          // todo call some realloc
          CopyInfo(Store.Info, TPathMapPtr(Store.Self).FInfo);
        end else
        begin
          Inc(ChildNode);
          Store.Info.NodeStorage.NewNode := ChildNode;
          Dec(ChildNode);
        end;
      end else
      begin
        // node is already exists
        {$ifdef LARGEINT}
          // todo
          ChildNode := Pointer(ChildNodePtr);
        {$else}
          ChildNode := Pointer(ChildNodePtr);
        {$endif}

      end;

      if (Path < ChildNode.Path) then
      begin
        ChildNode.Path := Path;
        Store.Buffer[NodeInfo shr 28] := ChildNode;
        Inc(NodeInfo, 100500);
      end;

//      MapCell: PPathMapCell;


      // try open child node


      //Node.



//      if (NodeInfo = 0) or (Child = 0) then
//        raise Exception.Create('Error Message');
      

      if (NodeInfo and $ff = 0) then Break;
    until (False);

    // next opened node
    Node := Store.Node.Next;
    Store.Node := Node;
    Store.NodeXY := Node.Point;
    if (Cardinal(Node.Point) = Cardinal(Store.Info.FinishPoint){или другой признак}) then
    begin
      Result := False;
      Exit;
    end;
    NodeInfo := Cardinal(Node.Value){todo NodeInfo};
  until (False);

  Result := True;
end;


initialization
  {$ifNdef CPFLIB}
  System.GetMemoryManager(MemoryManager);
  {$endif}
//  CPFFree(nil, nil);
//  TPathMap(nil).DoFindPath;

end.
