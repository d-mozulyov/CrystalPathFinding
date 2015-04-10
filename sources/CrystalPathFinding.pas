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
{.$define CPFAPI}
{.$define CPFLIB}

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

implementation

type
  TPathMapPtr = {$ifNdef CPFLIB}^TPathMap{$else}TPathMap{$endif};


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


<<<<<<< HEAD
      if (Path < ChildNode.Path) then
=======
// самая главная функция TPathMap - поиск пути
// по сути заполняет данные и производит небольшие расчёты
// а основная поисковая функция - это конечно PathMap_FIND_PATH
function TPathMap.FindPath(const Start, Finish: TPoint; const Weights: TPathMapWeights;
         const ExcludePoints: PPoint; const ExcludePointsCount: integer;
         const SectorTest: boolean): PPathMapResult;
label
  exit_proc;
type
  TDwordArray = array[0..1] of dword;
var
  i: integer;
  Stack: PPathMapStack;
  WasPathFinding: boolean;
  PointInExcluded: boolean;
  StartNumber, FinishNumber: dword;
  CellStart, CellFinish: PPathMapCell;
  CurrentPoint: PPoint;
  MapWidth, MapHeight: dword;
  WorkWeights: TPathMapWeights;
  DwordWeights: ^TDwordArray;
  zero_tiles, one_weight: boolean;

  procedure assert_point(const P: TPoint; const id: string);
  begin
    EWrongParameter.Assert('Wrong ' + id + ' point (%d, %d). Map size = %dx%d', [P.X, P.Y, Self.Width, Self.Height]);
  end;
begin
  Result := nil;
  Stack := FStack;
  WasPathFinding := false;

  // проверки
  MapWidth := Self.Width;
  MapHeight := Self.Height;
  PointInExcluded := false;
  if (dword(Start.X) >= MapWidth) or (dword(Start.Y) >= MapHeight) then assert_point(Start, 'start');
  if (dword(Finish.X) >= MapWidth) or (dword(Finish.Y) >= MapHeight) then assert_point(Finish, 'finish');
  if (ExcludePoints <> nil) then
  begin
    if (ExcludePointsCount < 0) then
    EWrongParameter.Assert('Wrong exclude points count = %d', [ExcludePointsCount]);

    CurrentPoint := pointer(ExcludePoints);
    for i := 0 to ExcludePointsCount-1 do
    begin
      if (dword(CurrentPoint.X) >= MapWidth) or (dword(CurrentPoint.Y) >= MapHeight) then assert_point(CurrentPoint^, 'exclude');

      if (not PointInExcluded) then
      if ((CurrentPoint.X=Start.X)and(CurrentPoint.Y=Start.Y)) or
         ((CurrentPoint.X=Finish.X)and(CurrentPoint.Y=Finish.Y)) then PointInExcluded := true;

      inc(CurrentPoint);
    end;
  end;
  if (Weights = nil) then WorkWeights := FDefaultWeights
  else WorkWeights := Weights;
  DwordWeights := pointer(WorkWeights.Prepare(Self, zero_tiles, one_weight));


  // проверки на быстрое попадание/непопадание
  begin
    if (PointInExcluded) then goto exit_proc;
    StartNumber := dword(Start.Y*integer(MapWidth) + Start.X);
    CellStart := @Stack.Map[StartNumber];
    if (CellStart.Tile = $FF) or (CellStart.Mask = 0) then goto exit_proc;
    FinishNumber := dword(Finish.Y*integer(MapWidth) + Finish.X);
    CellFinish := @Stack.Map[FinishNumber];
    if (CellFinish.Tile = $FF) or (CellFinish.Mask = 0) then goto exit_proc;
    if (DwordWeights[CellStart.Tile*2] = 0) or (DwordWeights[CellFinish.Tile*2] = 0) then goto exit_proc;

    if (Start.X = Finish.X) and (Start.Y = Finish.Y) then
    begin
      if (Self.NodeBlocksCount = 0) then IncrementNodeBlockNumber();
      if (Self.NodeBlocksCount > 1) then ReleaseHighNodeBlock();

      Stack.FPathPoints^ := Start;
      Result := @Stack.FFindResult;
      Result.points := Pointer(Stack.FPathPoints);
      Result.points_count := 1;
      Result.distance := 0;
      exit; //goto exit_proc;
    end;

    if (SectorTest {$ifndef CPF_DLL}or FSectorsFilled{$endif}) then
    begin
      if (not FSectorsFilled) then
>>>>>>> master
      begin
        ChildNode.Path := Path;
        Store.Buffer[NodeInfo shr 28] := ChildNode;
        Inc(NodeInfo, 100500);
      end;

//      MapCell: PPathMapCell;


      // try open child node


      //Node.



      if (NodeInfo = 0) or (Child = 0) then
        raise Exception.Create('Error Message');
      

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
  TPathMap(nil).DoFindPath;

end.
