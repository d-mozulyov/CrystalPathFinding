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

  Dword = Cardinal;
  PDword = ^Dword;

  // тип тайла
  TPathMapTile = type byte;
  PPathMapTile = ^TPathMapTile;

  // тип карты
  TPathMapMode = (mmSimple, mmDiagonal, mmDiagonalEx, mmHexagonal45, mmHexagonal60);  

  // поиск пути возвращается такой структурой
  TPathMapResult = packed record
    points: PPoint;
    points_count: integer;
    distance: double;
  end;
  PPathMapResult = ^TPathMapResult;

  // блок нодов
  TPathMapNodeBlock = record
    FHandle: pointer;

    Nodes: pointer;
    Size: integer;
  end;

  // класс который содержит массив весов
  TPathMapWeights = class
  private
    FAllValuesHandle: pointer;

    FDwordValues: pointer;
    FFloatValues: pointer;

    FHighTile: TPathMapTile;
    FReady: boolean;
    FMapMode: TPathMapMode;
    FWeightMul: dword;
    FZeroTiles: boolean;
    FOneTileWeight: boolean;
    FMinLine, FMinDiagonal: dword;
    
    function Prepare(AMap: TObject; var zero_tiles, one_weight: boolean): pdword;
    function GetValue(const Tile: TPathMapTile): single;
    procedure SetValue(const Tile: TPathMapTile; const Value: single);
  public
    constructor Create(const AHighTile: TPathMapTile);
    destructor Destroy; override;

    property HighTile: TPathMapTile read FHighTile;
    property Values[const Tile: TPathMapTile]: single read GetValue write SetValue; default;
  end;


  // карта ячеек. информация, по которой можно искать кратчайшие пути по карте
  TPathMap = class
  private
    FMapHandle: pointer;
    FMapSize: integer;
    FSetsHandle: pointer;
    FSetsSize: integer;
    FSectorsHandle: pointer;

    FHeight: word;
    FWidth: word;
    FMode: TPathMapMode;    
    FHighTile: TPathMapTile;
    FWeightMul: dword;
    FSmartWeight: boolean;
    FDefaultMask: dword;
    FSectorsFilled: boolean;
    FDefaultWeights: TPathMapWeights;
    LastWeights: TPathMapWeights;

    // все самые важные данные хранятся тут
    FStackHandle: pointer;
    FStack: pointer;

    function  GetTile(const X, Y: word): TPathMapTile;
    procedure SetTile(const X, Y: word; const Value: TPathMapTile);
    procedure SetMode(const Value: TPathMapMode);
    procedure CalculateMasks(); // default mask + parent mask
    procedure UpdateCellMasks(const ChangedArea: TRect);
  private
    // менеджер памяти
    MinBlockSize: integer;
    MaxBlockSize: integer;
    TotalAllocatedNodes: integer; 
    NodeBlocks: array of TPathMapNodeBlock;
    NodeBlocksCount: integer;
    CurrentBlockNumber: integer;

    function  IncrementNodeBlockNumber(): pointer;//PPathMapNode;
    procedure ReleaseHighNodeBlock();
  public
    constructor Create(const AWidth, AHeight: word; const AMode: TPathMapMode=mmSimple; const AHighTile: TPathMapTile=0; const ASmartWeight: boolean=true);
    destructor Destroy; override;
    procedure Clear();
    procedure Update(const Tiles: PPathMapTile; const X, Y, Width, Height: word; const pitch: integer=0);

    property Width: word read FWidth;
    property Height: word read FHeight;
    property Mode: TPathMapMode read FMode write SetMode;
    property HighTile: TPathMapTile read FHighTile;
    property SmartWeight: boolean read FSmartWeight write FSmartWeight; // что-то меняется?
    property Tiles[const X, Y: word]: TPathMapTile read GetTile write SetTile; default;

    // самая главная функция - поиск пути
    function FindPath(const Start, Finish: TPoint; const Weights: TPathMapWeights=nil; const ExcludePoints: PPoint=nil; const ExcludePointsCount: integer=0; const SectorTest: boolean=true): PPathMapResult;
  end;

{$ifdef CPFAPI}
function  cpfCreateWeights(HighTile: byte): THandle; cdecl;
procedure cpfDestroyWeights(var HWeights: THandle); cdecl;
function  cpfWeightGet(HWeights: THandle; Tile: byte): single; cdecl;
procedure cpfWeightSet(HWeights: THandle; Tile: byte; Value: single); cdecl;
function  cpfCreateMap(Width, Height: word; Mode: TPathMapMode=mmSimple; HighTile: byte=0; SmartWeight: boolean=true): THandle; cdecl;
procedure cpfDestroyMap(var HMap: THandle); cdecl;
procedure cpfMapClear(HMap: THandle); cdecl;
procedure cpfMapUpdate(HMap: THandle; Tiles: pbyte; X, Y, Width, Height: word; pitch: integer=0); cdecl;
function  cpfMapGetTile(HMap: THandle; X, Y: word): byte; cdecl;
procedure cpfMapSetTile(HMap: THandle; X, Y: word; Value: byte); cdecl;
function  cpfFindPath(HMap: THandle; Start, Finish: TPoint; HWeights: THandle=0; ExcludePoints: PPoint=nil; ExcludePointsCount: integer=0; SectorTest: boolean=true): PPathMapResult; cdecl;
{$endif}

implementation

{$ifdef CPFAPI}
function  cpfCreateWeights(HighTile: byte): THandle; cdecl;
begin
  Result := THandle(TPathMapWeights.Create(HighTile));
end;

procedure cpfDestroyWeights(var HWeights: THandle); cdecl;
begin
  TPathMapWeights(HWeights).Free;
end;

function  cpfWeightGet(HWeights: THandle; Tile: byte): single; cdecl;
begin
  Result := TPathMapWeights(HWeights)[Tile]
end;

procedure cpfWeightSet(HWeights: THandle; Tile: byte; Value: single); cdecl;
begin
  TPathMapWeights(HWeights)[Tile] := Value;
end;

function  cpfCreateMap(Width, Height: word; Mode: TPathMapMode=mmSimple; HighTile: byte=0; SmartWeight: boolean=true): THandle; cdecl;
begin
  Result := THandle(TPathMap.Create(Width, Height, Mode, HighTile, SmartWeight));
end;

procedure cpfDestroyMap(var HMap: THandle); cdecl;
begin
  TPathMap(HMap).Free;
end;

procedure cpfMapClear(HMap: THandle); cdecl;
begin
  TPathMap(HMap).Clear;
end;

procedure cpfMapUpdate(HMap: THandle; Tiles: pbyte; X, Y, Width, Height: word; pitch: integer=0); cdecl;
begin
  TPathMap(HMap).Update(Pointer(Tiles), X, Y, Width, Height, pitch);
end;

function  cpfMapGetTile(HMap: THandle; X, Y: word): byte; cdecl;
begin
  Result := TPathMap(HMap)[X, Y];
end;

procedure cpfMapSetTile(HMap: THandle; X, Y: word; Value: byte); cdecl;
begin
  TPathMap(HMap)[X, Y] := Value;
end;

function  cpfFindPath(HMap: THandle; Start, Finish: TPoint; HWeights: THandle=0; ExcludePoints: PPoint=nil; ExcludePointsCount: integer=0; SectorTest: boolean=true): PPathMapResult; cdecl;
begin
  Result := TPathMap(HMap).FindPath(Start, Finish, TPathMapWeights(HWeights),
     ExcludePoints, ExcludePointsCount, SectorTest);
end;
{$endif .CPFAPI}


type
  TWPoint = packed record
    X, Y: word;
  end;
  PWPoint = ^TWPoint;

{$ifdef CPFLIB}
  Exception = class(TObject)
  private
    FMessage: string;
    FHelpContext: Integer;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of integer);
    property Message: string read FMessage write FMessage;
  end;
  ExceptClass = class of Exception;
{$endif}

  EWrongParameter = class(Exception)
  public
    class procedure Assert(const Message: string); overload;
    class procedure Assert(const FmtStr: string; const Args: array of {$ifdef CPFLIB}integer{$else}const{$endif}); overload;
  end;

{$ifdef CPFLIB}
constructor Exception.Create(const Msg: string);
begin
  FMessage := Msg;
  FHelpContext := 0;
end;

constructor Exception.CreateFmt(const Msg: string; const Args: array of integer);
var
  P, T: integer;
  S: string;
begin
  FMessage := Msg;
  FHelpContext := 0;

  T := 0;
  while true do
  begin
    P := Pos('%d', FMessage);
    if (P = 0) then break;

    Delete(FMessage, P, 2);
    Str(Args[T], S);
    Insert(S, FMessage, P);
    inc(T);
  end;
end;
{$endif}

class procedure EWrongParameter.Assert(const Message: string);
var
  CodeAddr : pointer;

  function ReturnAddr: Pointer;
  asm  mov eax,[ebp+4] end;
begin
  CodeAddr := ReturnAddr;
  raise Create(Message) at CodeAddr;
end;

class procedure EWrongParameter.Assert(const FmtStr: string; const Args: array of {$ifdef CPFLIB}integer{$else}const{$endif});
var
  CodeAddr : pointer;

  function ReturnAddr: Pointer;
  asm  mov eax,[ebp+4] end;
begin
  CodeAddr := ReturnAddr;
  raise CreateFmt(FmtStr, Args) at CodeAddr;
end;

procedure FillDword(var Dest; count: Integer; Value: dword);
asm
  test edx, edx
  jnle @1
  ret
@1:
  push edi
  mov edi, eax
  mov eax, ecx
  mov ecx, edx

  REP  STOSD

  pop edi
end;

procedure CopyDwords(Dest, Src: pointer; Count: integer);
asm
  test ecx, ecx
  jnle @1
  ret
@1:
  push edi
  push esi
  mov  edi, eax
  mov  esi, edx

  REP MOVSD

  pop esi
  pop edi
end;
// <-- ???

function AlignedAlloc(var MemHandle: pointer; const Size: integer; const Zero: boolean=false): pointer;
var
  AllocSize: integer;
begin
  // на <= 0 не буду проверять

  AllocSize := (Size + 64 +3) and -4;
  GetMem(MemHandle, AllocSize);
  if (Zero) then FillDword(MemHandle^, AllocSize shr 2, 0);

  Result := pointer((integer(MemHandle)+63)and -64);
end;




{ TPathMap }
const
  // различные маски и not_ версии
  // чтобы при and ..., mask - выключить эти биты
  _0 = (1 shl 0);
  _1 = (1 shl 1);
  _2 = (1 shl 2);
  _3 = (1 shl 3);
  _4 = (1 shl 4);
  _5 = (1 shl 5);
  _6 = (1 shl 6);
  _7 = (1 shl 7);
  not_0 = not _0;
  not_1 = not _1;
  not_2 = not _2;
  not_3 = not _3;
  not_4 = not _4;
  not_5 = not _5;
  not_6 = not _6;
  not_7 = not _7;

  BITS_3 = (1 shl 3)-1;
  BITS_5 = (1 shl 5)-1;
  TOP_MASK = byte(BITS_3 shl 0);
  RIGHT_MASK = byte(BITS_3 shl 2);
  BOTTOM_MASK = byte(BITS_3 shl 4);
  LEFT_MASK = byte(_0 or _6 or _7);
  NOT_TOP_MASK = not (TOP_MASK);
  NOT_RIGHT_MASK = not (RIGHT_MASK);
  NOT_BOTTOM_MASK = not (BOTTOM_MASK);
  NOT_LEFT_MASK = not (LEFT_MASK);

type
  // что-то типа "узла" при расчётах,
  // содержащего всю информацию
  PPathMapNode = ^TPathMapNode;
  TPathMapNode = packed record
    value: dword; // оценка стоимости пути. F = G+H
    path: dword; // стоимость пути от стартовой точки. G. = F-H
    point: TWPoint;  // координата ячейки карты

    less: PPathMapNode;
    greather: PPathMapNode;

    tile: byte;
    mask: byte;
    parent_mask: byte;
    parent: byte;
  end;

  // ячейка в карте: тайл, маска, сектор и <индекс> (дин. расчёт)
  TPathMapCell = packed record
    Tile: TPathMapTile;
    Mask: byte; // 8bit mask
    Node: PPathMapNode; // кэширование нода в карте (больше для изменений. Но так же катит для сбора пути!)
  end;
  PPathMapCell = ^TPathMapCell;

  // массив ячеек карты. внутри представляется именно линейным массивом
  TPathMapCellArray = array[0..0] of TPathMapCell;
  PPathMapCellArray = ^TPathMapCellArray;

  // буффер для секторов
  TPathMapSectorArray = array[0..0] of byte;
  PPathMapSectorArray = ^TPathMapSectorArray;


  // инфо для оффсетов
  TPathMapOffset = record
    offsX, offsY: integer;
    offsNumber: integer;
    offsPointer: integer;
  end;
  TPathMapOffsetArray = array[0..7] of TPathMapOffset;
  PPathMapOffsetArray = ^TPathMapOffsetArray;


  // базовая информация по линии (вокруг текущей ячейки 3 линии)
  // из 3х ячеек. нужно для быстрого обращения к памяти. заранее просчитанные указатели
  // указатели и данные могут быть не валидными
  // но расчёт по ним не будет вестись, потому что не пройдёт mask тест
  TPathMapLineInfo = packed record
     CellsPtr: pointer;
     SetPtr: pointer;
     SetData: pointer;

     SetOffset: byte;
     SetChanged: boolean;
     reserved: word;
  end;
  

  TPathMapAddingElement = record
    value: dword;
    node: PPathMapNode;
  end;


  // все самые важные для поиска перменные и таблицы
  // а так же внутренние переменные "в стеке"
  // находятся в Align-утом буфере
  TPathMapStack = record
    // -------------------------- регистры -----------------------------------
    _eax, _ebx, _ecx, _edx, _esi, _edi, _ebp, _esp: dword;


    // ---------------  служебные данные   --------------
      __buf__: integer;
      FNodes: PPathMapNode;
      FPathPoints: PPoint;
      IterationNumber: dword; // только для дебага

      FFindResult: TPathMapResult;
    // ---------------------------------------------------------------------

    // большие смещения (128)
    Offsets: TPathMapOffsetArray;

    // -- самые рабочие данные. align64 !!!
    TopLine, MiddleLine, BottomLine: TPathMapLineInfo;
    _return_address: pointer;
    FCurrentPath: dword; // текущий путь (помогает расчитать dest пути)    
    FHighNode: PPathMapNode; // нод, который можно смело брать и использовать (для добавления)
    FMaxHighNode: PPathMapNode; // если FHighNode больше чем FMaxHighNode, то необходим реаллок !
    //----------------------------

    // -- для заполнения узлов. align64
    parent_masks_ptr: pointer;
    parent_masks: array[0..7] of byte;
    parent_masks_advanced: array[0..7] of byte; // для "неправославных" гексов
    buf_node: dword;
    buf_path: dword;
    buf_point: twpoint;
    short_offsets: array[0..15] of shortint;
    CurrentPoint: TWPoint;
    FinishPoint: TWPoint;
    HeuristicsLine: dword;
    HeuristicsDiagonal: dword;

    // -- расчётные и справочные данные. тоже align64
      CurrentNode: PPathMapNode;
      CurrentValue: dword;
      TopNode: PPathMapNode;
      TopNodeValue: dword;  

      Self: TPathMap;
      Options: dword;
      Sectors: PPathMapSectorArray; // массив для sector test
      Map: PPathMapCellArray;
      BitSet: pointer; // рабочая маска по вcей карте. 0/opened(1)/rounded(2)/closed(3). 2бита на состояние
      MapWidth, MapHeight, CellsCount: integer;
      MinDirtyBits, MaxDirtyBits: pointer; // для быстрой чистки грязного BitSet
      StartPoint: TWPoint;

      FAddingBufferSize: dword;
    // блок 64 байта )
      FAddingBuffer: array[0..7] of TPathMapAddingElement;
    //----------------------------


    // ----  веса. данные + float веса. подсчёт пути. align64 ----
    _map_: pointer;
    _map_width_: dword;
    _options_: dword;
    StepsLine: dword;
    StepsDiagonal: dword;
    Values: psingle;
    { здесь располагается массив DWORD_весов и после single веса }
  end;
  PPathMapStack = ^TPathMapStack;


constructor TPathMap.Create(const AWidth, AHeight: word; const AMode: TPathMapMode;
                            const AHighTile: TPathMapTile; const ASmartWeight: boolean);
const
  CELLS_COUNT_LIMIT = 100*1000*1000; // сто миллионов. типа 10000*10000
var
  i, CellsCount: integer;
  Stack: PPathMapStack;
  dest_short_offset: pshortint;
begin
  inherited Create;

  if (AWidth = 0) or (AHeight = 0) then
  EWrongParameter.Assert('Wrong size of PathMap: %dx%d', [AWidth, AHeight]);

  CellsCount := integer(AWidth)*integer(AHeight);
  if (CellsCount > CELLS_COUNT_LIMIT) then
  EWrongParameter.Assert('Wrong size of PathMap: %dx%d,'#13 +
  'becouse cells count(%d) can''t be greater than cells count limit(%d)', [AWidth, AHeight, CellsCount, CELLS_COUNT_LIMIT]);

  if (AHighTile = $FF) then
  EWrongParameter.Assert('High tile can''t be equal 255(0xFF), becouse that const means "WALL"');


  // инициализация полей-свойств
  FWidth := AWidth;
  FHeight := AHeight;
  FMode := AMode;
  FHighTile := AHighTile;
  FSmartWeight := ASmartWeight;

  // заполнение "стека"
  FStack := AlignedAlloc(FStackHandle, sizeof(TPathMapStack)+(FHighTile+1)*4*sizeof(DWORD), true);
  Stack := FStack;
  Stack.Self := Self;
  Stack.MapWidth := AWidth;
  Stack.MapHeight := AHeight;
  Stack.CellsCount := CellsCount;

  FWeightMul := high(dword) div (dword(CellsCount)+1);
  if (FWeightMul > high(word)) then FWeightMul := high(word);
  FMapSize := dword(Stack.CellsCount*sizeof(TPathMapCell)+ 3) and dword(-4);
  Stack.Map := pointer(integer(AlignedAlloc(FMapHandle, FMapSize+64, true))+32);
  FSetsSize := ((Stack.CellsCount*2 + 31) and dword(-32)) div 8;
  Stack.BitSet := pointer(integer(AlignedAlloc(FSetsHandle, FSetsSize+64, true))+32);

  Stack._map_ := Stack.Map;
  Stack._map_width_ := Stack.MapWidth;
  Stack.Values := pointer(integer(FStack)+sizeof(TPathMapStack)+(FHighTile+1)*2*sizeof(DWORD));

  // смещения
  for i := 0 to 7 do
  with Stack.Offsets[i] do
  begin
    case i of
      0: begin offsX := -1; offsY := -1; end;
      1: begin offsX :=  0; offsY := -1; end;
      2: begin offsX := +1; offsY := -1; end;
      3: begin offsX := +1; offsY :=  0; end;
      4: begin offsX := +1; offsY := +1; end;
      5: begin offsX :=  0; offsY := +1; end;
      6: begin offsX := -1; offsY := +1; end;
      7: begin offsX := -1; offsY :=  0; end;
    end;

    offsNumber := offsY*Width + offsX;
    offsPointer := offsNumber * sizeof(TPathMapCell);
  end;

  // маленькие офсеты. для расчёта dest точек. экономия памяти. short_offsets
  dest_short_offset := Pointer(@Stack.short_offsets);
  for i := 0 to 7 do
  with Stack.Offsets[i] do
  begin
    dest_short_offset^ := offsX;
    inc(dest_short_offset);
    dest_short_offset^ := offsY;
    inc(dest_short_offset);    
  end;

  // дефолтная маска и "родительские маски"
  CalculateMasks();
  
  // заполнить поле дефолтными значениями
  FDefaultWeights := TPathMapWeights.Create(AHighTile);
  Clear();


  // менеджер памяти
  // todo
{    MinBlockSize: integer;
    MaxBlockSize: integer;
    CurrentMemoryLevel: integer;
    HighMemoryLevel: integer; }

  MinBlockSize := (Stack.CellsCount div 1000 + 3) and -4;
  if (MinBlockSize < 32) then MinBlockSize := 32;
  MaxBlockSize := (Stack.CellsCount div 5 + 3) and -4;
  if (MaxBlockSize < 32) then MaxBlockSize := 32;  

end;

destructor TPathMap.Destroy;
var
  i: integer;
begin
  // очистить все буферы
  if (FMapHandle <> nil) then FreeMem(FMapHandle);
  if (FSetsHandle <> nil) then FreeMem(FSetsHandle);
  if (FSectorsHandle <> nil) then FreeMem(FSectorsHandle);
  if (FStackHandle <> nil) then
  begin
    with PPathMapStack(FStack)^ do
    if (FPathPoints <> nil) then FreeMem(FPathPoints);

    FreeMem(FStackHandle);
  end;

  // блоки памяти
  for i := 0 to NodeBlocksCount-1 do
  FreeMem(NodeBlocks[i].FHandle);
  

  FDefaultWeights.Free;
  inherited;
end;


procedure TPathMap.Clear();
const
  size_of_cell = sizeof(TPathMapCell);
  size_of_2_cells = 2*size_of_cell;
var
  Stack: PPathMapStack;
  MapWidth, MapHeight: integer;
  Value, ValueAdvanced: dword;
  i, j, Pitch: integer;
  Cell: PPathMapCell;
  CellArray: PPathMapCellArray;

  // занулить тайлы, выставить default маски
  procedure ClearCells(const StartCell: PPathMapCell; const Value, ValueAdvanced: dword; const MapWidth, MapHeight: integer); far;
  asm
    push ebx
    push esi
    push edi

    // eax- указатель
    // edx- value
    // ecx- value_advanced
    // edi - счётчик Height
    // esi - счётчик Width

    mov edi, MapHeight
    @height_loop:
      mov esi, MapWidth
      sub eax, size_of_2_cells
      jmp @line_loop_continue

      @line_loop:
        mov [eax], edx
        mov [eax + size_of_cell], ecx

      @line_loop_continue:
        add eax, size_of_2_cells
        sub esi, 2
        jge @line_loop
      @line_loop_end:

      // для карт с нечётным Width
      add esi, 2
      jz @height_loop_continue    
      mov [eax], edx
      add eax, size_of_cell

    @height_loop_continue:
    dec edi
    jnz @height_loop


    pop edi
    pop esi
    pop ebx
  end;


begin
  Stack := PPathMapStack(FStack);
  MapWidth := Stack.MapWidth;
  MapHeight := Stack.MapHeight;

  // очистить множество
  with Stack^ do
  begin
    FillDword(BitSet^, FSetsSize shr 2, 0);
    MinDirtyBits := nil;
    MaxDirtyBits := nil;
  end;

  // SectorTest
  FSectorsFilled := true;
  if (Stack.Sectors <> nil) then FillDword(Stack.Sectors^, (Stack.CellsCount+3) shr 2, 0);


  // заполнение дефолтными значениями
  Value := FDefaultMask;
  ValueAdvanced := FDefaultMask;
  if (not (Mode in [mmHexagonal45, mmHexagonal60])) then
  begin
    // простой случай
    Value := Value shl 8;
    ValueAdvanced := ValueAdvanced shl 8;
  end else
  begin
    // гексы
    Value := Value shl 8;    
  end;                   
  ClearCells(pointer(Stack.Map), Value and $FF00, ValueAdvanced and $FF00, MapWidth, MapHeight);


  // откорректировать краевые ячейки!
  CellArray := Stack.Map;
  Pitch := MapWidth*size_of_cell;
  for i := 0 to MapHeight-1 do
  begin
    // сверху
    if (i = 0) then
    for j := 0 to MapWidth-1 do with CellArray[j] do Mask := Mask and NOT_TOP_MASK;

    // снизу
    if (i = MapHeight-1) then
    for j := 0 to MapWidth-1 do with CellArray[j] do
    begin
      Mask := Mask and NOT_BOTTOM_MASK;

      if (Mode = mmHexagonal45) then
      begin
        Mask := Mask and not (_7 or _3);
      end;
    end;  

    // слева/справа
    with CellArray[0] do Mask := Mask and NOT_LEFT_MASK;
    with CellArray[MapWidth-1] do
    begin
      Mask := Mask and NOT_RIGHT_MASK;

      if (Mode = mmHexagonal60) then
      begin
        Mask := Mask and not (_1 or _5);
      end;
    end;  


    // increment
    inc(integer(CellArray), Pitch);
  end;


  // для гексогональных полей нужно вычёркивать "краевые" пути
  if (Mode = mmHexagonal45) and (MapHeight > 1) then
  begin
    CellArray := Pointer(@Stack.Map[(MapHeight-2)*MapWidth]);

    for i := 0 to (MapWidth div 2)-1 do
    with CellArray[i*2+1] do Mask := Mask and not_5;
  end;  
  if (Mode = mmHexagonal60) and (MapWidth > 1) then
  begin
    Cell := @Stack.Map[2*Stack.MapWidth -1];

    for i := 0 to (MapHeight div 2)-1 do
    begin
      with Cell^ do Mask := Mask and not_3;
      inc(integer(Cell), Pitch*2);
    end;
  end;  
end;

// какая-то область изменилась, нужно проапдейтить available
// изменённой области и клеток по периметру
procedure TPathMap.UpdateCellMasks(const ChangedArea: TRect);
const
  AND_MASKS: array[0..3] of integer = (NOT_TOP_MASK, NOT_RIGHT_MASK, NOT_BOTTOM_MASK, NOT_LEFT_MASK);
label
  reset_bit;
var
  Map: PPathMapCellArray;
  Offsets: PPathMapOffsetArray;
  Rounded: boolean;
  Cell: PPathMapCell;
  i, j, k, AWidth, AHeight: integer;
  Left, Top, Right, Bottom: integer;
  X, Y: integer;
  mask, bit: dword;
begin
  Left := ChangedArea.Left;
  Top := ChangedArea.Top;
  Right := ChangedArea.Right;
  Bottom := ChangedArea.Bottom;

  if (Left <> 0) then dec(Left);
  if (Top <> 0) then dec(Top);
  if (Right <> Width) then inc(Right);
  if (Bottom <> Height) then inc(Bottom);

  AWidth := Self.Width;
  AHeight := Self.Height;
  Map := PPathMapStack(FStack).Map;
  Offsets := @PPathMapStack(FStack).Offsets;
  Rounded := (Mode = mmDiagonalEx);

  // цикл
  for j := Top to Bottom-1 do
  begin
    Cell := @Map[j*Self.Width + Left];

    for i := Left to Right-1 do
    begin
      if (Cell.Tile <> $FF) then
      begin
        mask := FDefaultMask;

        // если [i, j] неправославная, то использовать альтернативную Default маску
        // + клетка может быть вообще не доступной
        if (Mode = mmHexagonal45) and (i and 1 = 1) then
        begin
          mask := mask shr 8;
          if (j + 1 = AHeight) then mask := 0;
        end;
        if (Mode = mmHexagonal60) and (j and 1 = 1) then
        begin
          mask := mask shr 8;
          if (i + 1 = AWidth) then mask := 0;
        end;
        

        // вычёркивание недоспустимых путей
        for k := 0 to 7 do
        begin
          bit := 1 shl k;

          if (mask and bit <> 0) then
          with Offsets[k] do
          begin
            X := i + offsX;
            if (X < 0) then goto reset_bit;
            Y := j + offsY;
            if (Y < 0) then goto reset_bit;

            // для неправославных [X, Y] делается инкремент
            // чтобы в случае чего вычеркнуть их доступных на вход
            if (Mode = mmHexagonal45) and (X and 1 = 1) then inc(Y);
            if (Mode = mmHexagonal60) and (Y and 1 = 1) then inc(X);

            // выход за границы
            if (X >= AWidth) or (Y >= AHeight)
            or (PPathMapCell(integer(Cell)+offsPointer).Tile = $FF) then goto reset_bit;

            continue;
            reset_bit: mask := mask xor bit;
          end;
        end;

        // Rounded логика
        if (Rounded) then
        for k := 0 to 3 do
        if (mask and (1 shl (k*2+1)) = 0) then mask := mask and AND_MASKS[k];

        
        // результат
        Cell.Mask := byte(mask);
      end;

      inc(Cell);
    end;
  end;
end;


// default_mask + пересчитать массив "вычёркивающих" масок
// index - номер по которому из parent-а идёт путь в текущую
procedure TPathMap.CalculateMasks();
type
  TParentMasks = array[0..7] of byte;
  PParentMasks = ^TParentMasks;
var
  parent_masks, parent_masks_advanced: PParentMasks;
  Stack: PPathMapStack;
begin
  Stack := PPathMapStack(FStack);
  Stack.parent_masks_ptr := @Stack.parent_masks;
  FillDword(Stack.parent_masks_ptr^, 8*2 div 4, 0);

  parent_masks := Pointer(@Stack.parent_masks);
  parent_masks_advanced := Pointer(@Stack.parent_masks_advanced);
  case (Mode) of
    mmSimple:
    begin
      FDefaultMask := _1 or _3 or _5 or _7;

      parent_masks[0] := byte(not(_3 or _4 or _5));
      parent_masks[1] := byte(not(_4 or _5 or _6));
      parent_masks[2] := byte(not(_5 or _6 or _7));
      parent_masks[3] := byte(not(_6 or _7 or _0));
      parent_masks[4] := byte(not(_7 or _0 or _1));
      parent_masks[5] := byte(not(_0 or _1 or _2));
      parent_masks[6] := byte(not(_1 or _2 or _3));
      parent_masks[7] := byte(not(_2 or _3 or _4));
    end;

    mmDiagonal,
    mmDiagonalEx:
    begin
      FDefaultMask := $FF;

      parent_masks[0] := byte(not(_3 or _4 or _5));
      parent_masks[1] := byte(not(_3 or _4 or _5 or _6 or _7));
      parent_masks[2] := byte(not(_5 or _6 or _7));
      parent_masks[3] := byte(not(_5 or _6 or _7 or _0 or _1));
      parent_masks[4] := byte(not(_7 or _0 or _1));
      parent_masks[5] := byte(not(_0 or _1 or _2 or _3 or _7));
      parent_masks[6] := byte(not(_1 or _2 or _3));
      parent_masks[7] := byte(not(_1 or _2 or _3 or _4 or _5));
    end;

    mmHexagonal45:
    begin
      FDefaultMask := _0 or _1 or _2 or _3 or _5 or _7; // православный случай
      FDefaultMask := FDefaultMask or (_1 or _3 or _4 or _5 or _6 or _7) shl 8; // + неправославный случай

      parent_masks[0] := byte(not(_4 or _5 or _3));
      parent_masks[1] := byte(not(_5 or _3 or _7));
      parent_masks[2] := byte(not(_6 or _5 or _7));
      parent_masks[3] := byte(not(_7 or _6 or _1));
      parent_masks[5] := byte(not(_1 or _0 or _2));
      parent_masks[7] := byte(not(_3 or _1 or _4));

      parent_masks_advanced[1] := byte(not(_5 or _4 or _6));
      parent_masks_advanced[3] := byte(not(_7 or _5 or _0));
      parent_masks_advanced[4] := byte(not(_0 or _1 or _7));
      parent_masks_advanced[5] := byte(not(_1 or _3 or _7));
      parent_masks_advanced[6] := byte(not(_2 or _1 or _3));
      parent_masks_advanced[7] := byte(not(_3 or _2 or _5));
    end;

    mmHexagonal60:
    begin
      FDefaultMask := _0 or _1 or _3 or _5 or _6 or _7; // православный случай
      FDefaultMask := FDefaultMask or (_1 or _2 or _3 or _4 or _5 or _7) shl 8; // + неправославный случай

      parent_masks[6] := byte(not(_1 or _2 or _3));
      parent_masks[7] := byte(not(_1 or _3 or _5));
      parent_masks[0] := byte(not(_3 or _4 or _5));
      parent_masks[1] := byte(not(_4 or _5 or _7));
      parent_masks[3] := byte(not(_6 or _7 or _0));
      parent_masks[5] := byte(not(_7 or _1 or _2));

      parent_masks_advanced[7] := byte(not(_2 or _3 or _4));
      parent_masks_advanced[1] := byte(not(_3 or _5 or _6));
      parent_masks_advanced[2] := byte(not(_5 or _6 or _7));
      parent_masks_advanced[3] := byte(not(_1 or _5 or _7));
      parent_masks_advanced[4] := byte(not(_0 or _1 or _7));
      parent_masks_advanced[5] := byte(not(_0 or _1 or _3));
    end;
  end;

end;


function TPathMap.GetTile(const X, Y: word): TPathMapTile;
begin
  if (X >= Width) or (Y >= Height) then
  EWrongParameter.Assert('Wrong map coordinates (%d, %d). Map size = %dx%d', [X, Y, Width, Height]);

  Result := PPathMapStack(FStack).Map[Y*Width + X].Tile;
end;

procedure TPathMap.SetTile(const X, Y: word; const Value: TPathMapTile);
begin
  if (X >= Width) or (Y >= Height) then
  EWrongParameter.Assert('Wrong map coordinates (%d, %d). Map size = %dx%d', [X, Y, Width, Height]);

  if (Value > HighTile) and (Value <> $FF) then
  EWrongParameter.Assert('Wrong tile = %d in (%d, %d). High tile is %d', [Value, X, Y, HighTile]);


  with PPathMapStack(FStack).Map[Y*Width + X] do
  if (Tile <> Value) then
  begin
    if (Tile = $FF) or (Value = $FF) then FSectorsFilled := false;
    Tile := Value;
    UpdateCellMasks(Rect(X, Y, X+1, Y+1));
  end;
end;                       


procedure TPathMap.SetMode(const Value: TPathMapMode);
begin
  if (FMode <> Value) then
  begin
    FMode := Value;
    FSectorsFilled := false;
    LastWeights := nil; // чтобы правильно перезаполнился
    CalculateMasks();

    // прописать маски
    UpdateCellMasks(Rect(0, 0, Width, Height));
  end;
end;

procedure TPathMap.Update(const Tiles: PPathMapTile; const X, Y,
                          Width, Height: word; const pitch: integer);
label
  was_exception;
var
  Src, Dest, DoneDest: PPathMapTile;
  SrcPitch, DestPitch, i, j: integer;
  HighTile: TPathMapTile;
  Number, ExceptX, ExceptY: integer;
begin
  if (integer(X) + integer(Width) > integer(Self.Width)) or
     (integer(Y) + integer(Height) > integer(Self.Height)) then
  EWrongParameter.Assert('Wrong map coordinates (X: %d, Y: %d, Width: %d, Height: %d). Map size = %dx%d',
                         [X, Y, Width, Height, Self.Width, Self.Height]);


  // Pitch - дополнительные инкрементаторы для линии
  SrcPitch := pitch;
  if (SrcPitch = 0) then SrcPitch := Width;
  SrcPitch := SrcPitch - Width;
  DestPitch := (Self.Width-Width)*sizeof(TPathMapCell);

  // базовая инициализация
  Src := Tiles;
  Dest := Pointer(@PPathMapStack(FStack).Map[Y*Self.Width + X]);
  DoneDest := Pointer(@PPathMapStack(FStack).Map[(Y+Height-1)*Self.Width + X+Width]);
  HighTile := Self.HighTile;  
  for j := 1 to Height do
  begin
    for i := 1 to Width do
    begin
      if (Dest^ <> Src^) then
      begin
        if (Src^ <= HighTile) or (Src^ = $FF) then
        begin
          if (Src^ = $FF) or (Dest^ = $FF) then FSectorsFilled := false;
          Dest^ := Src^;
        end else
        goto was_exception;
      end;

      inc(Src);
      inc(integer(Dest), sizeof(TPathMapCell));
    end;

    inc(integer(Src), SrcPitch);
    inc(integer(Dest), DestPitch);
  end;
  dec(integer(Dest), DestPitch);

  // ошибка
  was_exception:
  if (Dest <> DoneDest) then
  begin
    Number := (integer(Dest)-integer(PPathMapStack(FStack).Map)) div sizeof(TPathMapCell);
    ExceptX := Number mod Self.Width;
    ExceptY := Number div Self.Width;
    EWrongParameter.Assert('Wrong tile = %d in (%d, %d). High tile is %d', [Src^, ExceptX, ExceptY, HighTile]);
  end;

  // проапдейтить маски
  Self.UpdateCellMasks(Bounds(X, Y, Width, Height));
end;


// либо выделить новый блок, либо взять уже имеющийся
function  TPathMap.IncrementNodeBlockNumber(): pointer;//PPathMapNode;
var
  Stack: PPathMapStack;

  
  // добавить новый блок, перелочить буфер точек
  procedure AddNewBlock();
  var
    BlockSize: integer;
  begin
    if (NodeBlocksCount = 0) then BlockSize := MinBlockSize
    else BlockSize := NodeBlocks[NodeBlocksCount-1].Size*2;

    if (BlockSize > MaxBlockSize) then BlockSize := MaxBlockSize;

    inc(NodeBlocksCount);
    SetLength(NodeBlocks, NodeBlocksCount);
    with NodeBlocks[NodeBlocksCount-1] do
    begin
      Nodes := AlignedAlloc(FHandle, BlockSize*sizeof(TPathMapNode));
      Size := BlockSize;
    end;

    inc(TotalAllocatedNodes, BlockSize);
    if (Stack.FPathPoints <> nil) then FreeMem(Stack.FPathPoints);
    GetMem(Stack.FPathPoints, TotalAllocatedNodes*sizeof(TPoint));
  end;
begin
  Stack := PPathMapStack(FStack);

  // для простых случаев когда память не выделена вообще CurrentBlockNumber как был нулём так и останется
  // позже уже будут реаллоки
  if (NodeBlocksCount <> 0) then inc(CurrentBlockNumber);
  if (CurrentBlockNumber = NodeBlocksCount) then AddNewBlock();

  // заполнение параметров
  with NodeBlocks[CurrentBlockNumber] do
  begin
    Result := Nodes;
    Stack.FMaxHighNode := pointer(integer(Nodes) + (Size-1)*sizeof(TPathMapNode));
  end;
end;


// блоки могут постепенно удаляться
procedure TPathMap.ReleaseHighNodeBlock();
var
  Stack: PPathMapStack;
begin
  if (NodeBlocksCount = 0) then exit;
  Stack := PPathMapStack(FStack);

  dec(NodeBlocksCount);
  FreeMem(NodeBlocks[NodeBlocksCount].FHandle);
  dec(TotalAllocatedNodes, NodeBlocks[NodeBlocksCount].Size);
  SetLength(NodeBlocks, NodeBlocksCount);

  if (TotalAllocatedNodes = 0) then
  begin
    FreeMem(Stack.FPathPoints);
    Stack.FPathPoints := nil;
  end else
  begin
    ReallocMem(Stack.FPathPoints, TotalAllocatedNodes*sizeof(TPoint));
  end;
end;


// заполнить карту информацией по секторам
// зануление -> рекурсии
procedure PathMapFillSectors(var Stack: TPathMapStack);
const
  size_of_cell = sizeof(TPathMapCell);
  BH_INCREMENT = 2 shl 8;
  MASK_LOOP_FINISH_8 = (8 *2) shl {sizeof(bl)}8;
asm
  mov TPathMapStack[eax]._ebx, ebx
  mov TPathMapStack[eax]._esi, esi
  mov TPathMapStack[eax]._edi, edi
  mov TPathMapStack[eax]._esp, esp
  mov TPathMapStack[eax]._ebp, ebp
  mov ebp, eax
// -----------------------

  // сначала идёт зануление всех Sector в ячейках
  mov ecx, TPathMapStack[EBP].CellsCount
  xor eax, eax
  add ecx, 3
  mov edi, TPathMapStack[EBP].Sectors
  shr ecx, 2
  REP STOSD

  // здесь происходит просмотр каждой клетки
  // и если она подходит для рекурсивного заполнения, то "вызывать" FILL_CELL_PROC
  {
    for i := 0 to CellsCount-1 do
    begin
      Cell := @Map[i];
      if (Cell.Sector = 0) and (Cell.Tile <> $FF) and (Cell.Mask <> 0) then
      begin
        CURRENT_SECTOR := (CURRENT_SECTOR+1) and $FF;
        FILL_CELL_PROC(Cell, Cell.Mask, CURRENT_SECTOR);
      end;
    end;
  }

  // esi - Sectors
  // eax - Map
  // edx - счётчик 0..TPathMapStack[EBP].CellsCount-1
  mov esi, TPathMapStack[EBP].Sectors
  xor edx, edx
  mov eax, TPathMapStack[EBP].Map

  mov TPathMapStack[EBP]._ECX, 0 // текущий CurrentSector. начнётся с 1
  @look_map_loop:
    lea ebx, [edx + edx*2]
    cmp byte ptr[esi+edx],0 //Sectors[i]
    jne @look_map_continue
    mov ebx, [eax + ebx*2]  // Map[edx*6]: Tile, Mask
    cmp bl, $FF
    je @look_map_continue
    shr ebx, 8
    and ebx, $000000FF
    jz @look_map_continue


    inc TPathMapStack[EBP]._ECX
    and TPathMapStack[EBP]._ECX, $000000FF
    jnz @1
    mov TPathMapStack[EBP]._ECX, 1
@1: mov TPathMapStack[EBP]._edx, edx


      // здесь происходит эмуляция рекурсивной функции
      // esi - Sectors[], eax - Map[]. _ECX - CurrentSector
      // FillCell(edx: CellNumber; ebx(bl): 8bit_mask);
      // edi - как 1 shl iteration - чтобы проверять bl(ebx) на маску
      // bh - итератор (0..7), только умноженный на 2 - чтобы получить доступ к TPathMapOffset (2*8 = sizeof struct)
      mov TPathMapStack[EBP]._EAX, 1 // счётчик рекурсии. аналог стекового взаимодействия
      @FILL_CELL_PROC:
         {
            Cell.Buffer := CURRENT_SECTOR
            for i := 0 to 7 do
            if (i in Mask) then
            begin
              DestCell := AroundCells[i]

              if (Dest.Sector = 0) and (DestCell.Tile <> $FF) and (DestCell.Mask <> 0)
              then FILL_CELL_PROC(DestCell);
            end;   
         }
         mov ecx, TPathMapStack[EBP]._ECX
         mov edi, 1
         mov [esi + edx], cl // := CurrentSector
         // xor bh, bh не делается потому что он 0
         @mask_iteration_loop:
           test ebx, edi // look bl mask
           jz @mask_iteration_continue

           // тест i-й ячейки
           mov edi, edx
           movzx ecx, bh
           add edi, [EBP + TPathMapStack.Offsets + ecx*8 + TPathMapOffset.offsNumber] // DestCellNumber

           lea ecx, [edi+edi*2]
           cmp byte ptr [esi+edi], 0
           jnz @restore_edi
           mov ecx, [eax + ecx*2]
           cmp cl, $FF // tile
           je @restore_edi
           shr ecx, 8  // mask
           jz @restore_edi

           // i-я ячейка подходит для рекурсии.
           // вызвать FILL_CELL_PROC
             push edx
             push ebx
               mov edx, edi // dest cell number
               mov ebx, ecx // dest mask
               inc TPathMapStack[EBP]._EAX
               and ebx, $000000FF
               jmp @FILL_CELL_PROC
               @after_recursion:
             pop ebx
             pop edx

         // "восстановить" edi
         @restore_edi:
           movzx ecx, bh
           mov edi, 1
           shr ecx, 1
           shl edi, cl
         @mask_iteration_continue:
         add ebx, BH_INCREMENT  //add bh, 2
         shl edi, 1
         test ebx, MASK_LOOP_FINISH_8
         jz @mask_iteration_loop
      dec TPathMapStack[EBP]._EAX
      jnz @after_recursion
      // <-- рекурсивная функция FILL_CELL_PROC

  mov edx, TPathMapStack[EBP]._edx
  @look_map_continue: // просмотр каждой клетки в карте
  inc edx
  cmp edx, TPathMapStack[EBP].CellsCount
  jne @look_map_loop

   
// -----------------------
  mov eax, ebp
  mov ebp, TPathMapStack[eax]._ebp
  mov esp, TPathMapStack[eax]._esp
  mov edi, TPathMapStack[eax]._edi
  mov esi, TPathMapStack[eax]._esi
  mov ebx, TPathMapStack[eax]._ebx
end;



{$ifdef CPFLOG}
var
  LogList: TMemoryStream;
  LogFileName: string;
  LogErrorDetected: boolean=false;
  LogListEnter: word = 13 or (10 shl 8);

procedure Log(const S: string); overload;
begin
  //LogList.Add(S);
  if (S <> '') then LogList.Write(pointer(S)^, Length(S));
  LogList.Write(LogListEnter, sizeof(LogListEnter));
end;

procedure Log(const Fmt: string; const Args: array of const); overload;
begin
  Log(Format(Fmt, Args));
end;

procedure Log(const Stack: TPathMapStack; const Node: PPathMapNode; temp: boolean=false); overload;
var
  S: string;
  Next: PPathMapNode;
begin
  with Node^ do
(* if (temp) then
  begin
    S := Format('[%2d,%2d]%d($%u) path: %d. heuristics: %d. tile: %d. parent: %d. mask: %2x. parent_mask: %2x.',
         [point.X, point.Y, value, dword(Node), path, value-path, tile, parent, mask, parent_mask]);
  end else  *)
  begin
  S := Format('[%2d,%2d]%d($%u. $%u--$%u) path: %d. tile: %d. parent: %d. mask: %2x. parent_mask: %2x.',
       //Format('[%2d,%2d]%d($%u.)'#9#9'$%u--$%u',
       [point.X, point.Y, value, dword(Node), dword(less), dword(greather),
       path, tile, parent, mask, parent_mask]);
  end;

  Log(S);

  if (not temp) then
  begin
    Next := Node.greather;
    if (Next <> nil) then
    begin
      if (Next.less <> Node) then
      begin
        LogErrorDetected := true;
        Log('<<--------- ОШИБКА less, greather !!! ----------');
        Log('');
      end;

      if (Node <> Stack.CurrentNode) and (Next.value < Node.value) then
      begin
        LogErrorDetected := true;
        Log('<<--------- ОШИБКА value !!! ----------');
        Log('');
      end;
    end;
  end;
end;


procedure BeginIteration(var Stack: TPathMapStack);
var
  i: integer;
  Hour, Min, Sec, MSec: Word;
//  Node: PPathMapNode;

  function NumberOf(const Point: TWPoint): dword;
  begin
    Result := Point.Y*Stack.MapWidth + Point.X;
  end;
begin
  if (Stack.CurrentNode = Stack.FNodes) then
  begin
    if (LogList <> nil) then LogList.Free;
    LogList := TMemoryStream.Create;
    LogErrorDetected := false;
    DecodeTime(Now, Hour, Min, Sec, MSec);
    LogFileName := 'LOG.txt';//Format('%2d-%2d-%2d.log', [Hour, Min, Sec]);

    with Stack do
    begin
      Log('Время %2d-%2d-%2d.', [Hour, Min, Sec]);
      Log('Карта (%dx%d) - %d точек.', [MapWidth, MapHeight, CellsCount]);
      Log('Ищем путь [%dx%d](%d) -->> [%dx%d](%d).', [StartPoint.X, StartPoint.Y,  NumberOf(StartPoint), FinishPoint.X, FinishPoint.Y,  NumberOf(FinishPoint)]);
      Log('');
    end;
  end;


  Log('---------------------------------------------------------');
  Log('Итерация: %d. TopNode[%2d,%2d]: %d($%u.)', [Stack.IterationNumber, Stack.TopNode.Point.X, Stack.TopNode.Point.Y, Stack.TopNodeValue, dword(Stack.TopNode)]);
  Log(Stack, Stack.CurrentNode);
  Log('');
  try
{    Log('Текущие:');
    Node := Stack.FNodes;
    while (Node <> nil) do
    begin
      Log(Stack, Node);
      if (Node = Stack.CurrentNode) then Log('***********************');

      Node := Node.greather;
    end;
    Log('');  }


    // adding
    Log('Буффер на добавление/изменение: %d', [Stack.FAddingBufferSize]);
    for i := 0 to integer(Stack.FAddingBufferSize)-1 do
    Log(Stack, Stack.FAddingBuffer[i].node, true);

  finally
    LogList.SaveToFile(LogFileName);
  end;
end;

procedure LogSortedAdding(var Stack: TPathMapStack);
var
  i: integer;
begin
//  exit;

  Log('');
  Log('Результат сортировки (%d элементов):', [Stack.FAddingBufferSize]);

  try
    for i := 0 to integer(Stack.FAddingBufferSize)-1 do
    Log(Stack, Stack.FAddingBuffer[i].node, true);
  finally
    LogList.SaveToFile(LogFileName);
  end;
end;

procedure EndIteration(var Stack: TPathMapStack);
var
  Node: PPathMapNode;
begin
//  exit;


  Log('');
  Log('Результат:');

  Node := Stack.CurrentNode;
  if (Node <> Stack.FNodes) then Log('...');
  Log(Stack, Node);
  Log('*************');

  Node := Node.greather;
  try
    while (Node <> nil) do
    begin
      Log(Stack, Node);
      Node := Node.greather;
    end;

    Log('---------------------------------------------------------');
  finally
    LogList.SaveToFile(LogFileName);
  end;
end;

procedure LogAllStack(var Stack: TPathMapStack);
var
  Node: PPathMapNode;
  current_value, value, count: dword;


  procedure Fixup();
  begin
    Log('%d'#9'x%d', [current_value, count]);
  end;
begin

  // общая информация
//  with Stack do
//  Log('Закрыто %d из %d. Буфер: %d. Клеток в карте: %d (%dx%d)', [FClosedElementsCount, FElementsCount, FElementBufferLength, CellsCount, MapWidth, MapHeight]);

  Log('');
  Log('*********************************************************************');
  Log('***************  ОБЩАЯ  ИНФОРМАЦИЯ  *********************************');
  Log('*********************************************************************');
  Log('');

  Node := Stack.FNodes;
  if (Node = nil) then exit;
  
  current_value := Node.value;
  count := 1;
  Node := Node.greather;
  while (Node <> nil) do
  begin
    value := Node.value;

    if (current_value = value) then inc(count)
    else
    begin
      Fixup;
      if (value < current_value) then
      begin
        //LogErrorDetected := true;
        //Log('---------  ОШИБКА  --------------', []);
        Log('--------------------------------------');
        Log('');
      end;  

      current_value := value;
      count := 1;
    end;

    Node := Node.greather;
  end;

  LogList.SaveToFile(LogFileName);
  LogList.Free;
  LogList := nil;

  if (LogErrorDetected) then
  Winexec(pchar('notepad.exe "' + LogFileName + '"'), SW_SHOWNORMAL);
end;
{$endif}



const
  // 8 опций (последний байт)
  OPTION_DIAGONAL = 1 shl 24;
  OPTION_ROUNDED = 1 shl 25;
  OPTION_HEXAGONAL_45 = 1 shl 26;
  OPTION_HEXAGONAL_60 = 1 shl 27;
  OPTION_SMART_WEIGHT = 1 shl 28;
  OPTION_EXCLUDED_POINTS = 1 shl 29;
  OPTION_ZERO_TILES = 1 shl 30;
  OPTION_ONE_WEIGHT = 1 shl 31;

  TEST_NOTSIMPLE_MAP = OPTION_DIAGONAL or OPTION_HEXAGONAL_45 or OPTION_HEXAGONAL_60;
  TEST_HEXAGONAL_MAP = OPTION_HEXAGONAL_45 or OPTION_HEXAGONAL_60;

  // "режимы". доступны 5 бит (начиная с 19)
  MODE_ADDING = 1 shl 19;
  MODE_SETCHANGED = 1 shl 20;
  MODE_WAS_LESS_CURRENT = 1 shl 23;
  NOT_MODE_SETCHANGED = not MODE_SETCHANGED;


  SQRT_2: extended = 1.4142135623730950488016887242097;
  HALF: extended = 0.5;
  NEED_BREAKPOINT: boolean=false;

(*procedure TestBiSet(var Stack: TPathMapStack);
var
  i, min, max: integer;
  chars: pchar;
begin
  max := 0;
  min := Stack.Self.FSetsSize;
  chars := pchar(Stack.BitSet);

  for i := 0 to integer(Stack.Self.FSetsSize)-1 do
  if (chars[i] <> #0) then
  begin
    if (i < min) then min := i;
    if (i > max) then max := i;
  end;

  if (max <> 0) then
  begin
    ShowError('ХУЙ! Min: %d(%p). Max: %d(%p).', [min, @chars[min], max, @chars[max]]);
  end;
end; *)


function GetHeuristics(var Stack: TPathMapStack; X, Y: integer): integer;
var
  less, orthodoxy: boolean;
  dx, dy, buf: integer;
 // Rast: integer;
begin
  less := false;

  dx := integer(Stack.FinishPoint.X)-X;
  if (dx < 0) then
  begin
    dx := -dx;
    if (Stack.Options and OPTION_HEXAGONAL_60 <> 0) then less := true;
  end;

  dy := integer(Stack.FinishPoint.Y)-Y;
  if (dy < 0) then
  begin
    dy := -dy;
    if (Stack.Options and OPTION_HEXAGONAL_45 <> 0) then less := true;
  end;

  // дополнительное смещение
  if (Stack.Options and TEST_HEXAGONAL_MAP <> 0) then
  begin
    if (Stack.Options and OPTION_HEXAGONAL_45 <> 0) then orthodoxy := (X and 1 = 0)
    else orthodoxy := (Y and 1 = 0);

    less := (less=orthodoxy);
  end;


  if (Stack.Options and OPTION_DIAGONAL <> 0) then
  begin
    // Diagonal or DiagonalEx
    if (dx <= dy) then
    begin
      Result := dword(dx)*(Stack.HeuristicsDiagonal) + dword((dy-dx))*(Stack.HeuristicsLine);
    end else
    begin
      Result := dword(dy)*(Stack.HeuristicsDiagonal) + dword((dx-dy))*(Stack.HeuristicsLine);
    end;
  end else
  begin
    // коэфициент расстояния
  //  Rast := dx + dy*2;
 //   if (dx > dy) then Rast := dy + dx*2;


    if (Stack.Options and TEST_NOTSIMPLE_MAP = 0) then
    begin
      // Simple
      Result := (dx+dy)*integer(Stack.HeuristicsLine{-1});// + Rast;
    end else
    begin
      if (Stack.Options and OPTION_HEXAGONAL_60 <> 0) then
      begin
        // swap dx<-->dy чтобы считать эвристику гексогональную как 45 угловую
        buf := dx;
        dx := dy;
        dy := buf;
      end;

      // Гексагональные считаются как 45. потому что для 60 свап уже был.
      Result := dx;

      if (dx = 0) then
      begin
        inc(Result, dy);
      end else
      begin
        dec(dy, dx shr 1);
        if (dy > 0) then
        begin
          inc(Result, dy);
          if (dx and 1 = 1) and (less) then dec(Result);
        end;
      end;

      Result := Result*integer((Stack.HeuristicsLine{-1}));// + Rast;
    end;

    // даже в самом худшем случае path + heuristics не привысит hing(dword).
  end;
end;

{$ifdef CPFLOG}
function CompareHeuristics(Value: integer; var Stack: TPathMapStack; const Point: TWPoint): integer;
begin
  Result := GetHeuristics(Stack, Point.X, Point.Y);

  if (Result <> Value) then
  begin
    LogErrorDetected := true;
    Log('<<-- Incorrect heuristics: %d. Correct value: %d.', [Value, Result]);
  end;
end;
{$endif}


// --------------------------------------------------------------------------
// Самая весомая функция - цикл поиска пути
// --------------------------------------------------------------------------
function PathMap_FIND_PATH(var Stack: TPathMapStack; const ExcludePoints: PPoint; const ExcludePointsCount: integer): PPathMapResult;
const
  rounded = 2;
  sizeof_node = sizeof(TPathMapNode);
  sizeof_stack = sizeof(TPathMapStack);
  log_from_iteration = {100000;//}0;

  close_current_point = (2) shl 2;
  left_right = _3 or _7;
  full_left_right = left_mask or right_mask;

  left_opened = (1) shl (0*2);
  center_opened = (1) shl (1*2);
  right_opened = (1) shl (2*2);
  left_closed = (2) shl (0*2);
  center_closed = (2) shl (1*2);
  right_closed = (2) shl (2*2);

  CELL_0 = 0;
  CELL_1 = 6;
  CELL_2 = 12;

  MODE_ADDING_SET_CHANGED = MODE_ADDING or MODE_SETCHANGED;
  LOOK_0 = (0 shl 16);
  LOOK_1 = (1 shl 16);
  LOOK_2 = (2 shl 16);
  LOOK_3 = (3 shl 16);
  LOOK_4 = (4 shl 16);
  LOOK_5 = (5 shl 16);
  LOOK_6 = (6 shl 16);
  LOOK_7 = (7 shl 16);
  CLEAR_LOOKS = not ($00070000 or MODE_ADDING);

asm
  mov TPathMapStack[eax]._ebx, ebx
  mov TPathMapStack[eax]._esi, esi
  mov TPathMapStack[eax]._edi, edi
  mov TPathMapStack[eax]._esp, esp
  mov TPathMapStack[eax]._ebp, ebp
  mov ebp, eax
// -----------------------

    // прежде чем начнётся расчёт, предыдущее битовое множество зануляется
    // туда заносятся excluded points (если OPTION_EXCLUDED_POINTS)
    // а так же выставляется StartNumber как opened
    mov TPathMapStack[EBP]._edx, edx
    mov TPathMapStack[EBP]._ecx, ecx

    
    // зануление FillDword(Stack.BitSet^, FSetsSize shr 2, 0)
    mov edi, TPathMapStack[EBP].MinDirtyBits
    mov ecx, TPathMapStack[EBP].MaxDirtyBits
    and edi, -4
    jz  @after_bitset_clear
      add ecx, 7  //3
      and ecx, -4
      sub ecx, edi

      xor eax, eax
      shr ecx, 2
      REP  STOSD
    @after_bitset_clear:

//    mov esp, TPathMapStack[EBP]._esp
//    mov eax, EBP
//    call TestBiSet


    // esp - BitSet
    // edi - MinDirtyBits
    // esi - MaxDirtyBits
    // рабочие: eax, edx, ecx, ebx
    mov esp, TPathMapStack[EBP].BitSet
    mov ebx, TPathMapStack[EBP].MapWidth
    movzx ecx, TPathMapStack[EBP].StartPoint.X
    add ebx, ebx
    movzx eax, TPathMapStack[EBP].StartPoint.Y
    mul ebx
    lea eax, [eax + ecx*2]
    mov ecx, eax
    mov edx, 1
    and ecx, 7
    shr eax, 3
    shl edx, cl
    lea edi, [esp + eax]
    mov esi, edi
    or [edi], edx


    // if (Stack.Options and OPTION_EXCLUDED_POINTS <> 0) then
    // IncludeExcludedPoints(Stack^, ExcludePoints, ExcludePointsCount);
    test TPathMapStack[EBP].Options, OPTION_EXCLUDED_POINTS
    jz @after_excluded_points
        mov ebx, TPathMapStack[EBP]._edx

        @exclude_points_loop:
          mov ecx, [ebx]   // X
          mov eax, [ebx+4] // Y
          mul TPathMapStack[EBP].MapWidth
          add eax, ecx // number
          mov edx, rounded
          add eax, eax // number*2
          mov ecx, eax
          shr eax, 3
          and ecx, 7
          add eax, esp // ptr
          shl edx, cl
          or [eax], edx

          // min bits
          cmp eax, edi
          jae @after_correction_min_dirty_bits
            mov edi, eax
          @after_correction_min_dirty_bits:

          // max bits
          cmp eax, esi
          jbe @after_correction_max_dirty_bits
            mov esi, eax
          @after_correction_max_dirty_bits:

        @exclude_points_continue:
          add ebx, 8
          dec TPathMapStack[EBP]._ecx
        jnz @exclude_points_loop
    @after_excluded_points:


    // запомнить "грязные" биты
    mov TPathMapStack[EBP].MinDirtyBits, edi
    mov TPathMapStack[EBP].MaxDirtyBits, esi

    // "очистить" буфер
    mov TPathMapStack[EBP].FAddingBufferSize, 0

    // EBX - всегда опции
    mov EBX, TPathMapStack[EBP].Options

    // TPathMapStack[EBP].greather - текущий рассматриваемый узел
    lea ESP, [ebp + TPathMapStack.FNodes - TPathMapNode.greather]
// *********************  ОСНОВНОЙ ЦИКЛ  ***********************************
@open_list_loop:
   {$ifdef CPFLOG}
   inc TPathMapStack[EBP].IterationNumber
   {$endif}

   mov ESP, TPathMapNode[ESP].greather
   test ESP, ESP
   jnz @after_jmp_false
     xor esp, esp
     jmp @return_false
   @after_jmp_false:

   // взять текущие параметры из нода
   mov esi, TPathMapNode[ESP].value
   mov edi, TPathMapNode[ESP].path
   mov eax, TPathMapNode[ESP].point
   mov ecx, dword ptr TPathMapNode[ESP].tile

   // если достигли цели
   cmp eax, TPathMapStack[EBP].FinishPoint
   jne @after_jmp_true
     xor edx, edx
     jmp @return_true
   @after_jmp_true:

   // parent_masks_ptr для гексов
   test EBX, TEST_HEXAGONAL_MAP
   jz @after_masks_ptr
     mov edx, eax // X
     test EBX, OPTION_HEXAGONAL_45
     jnz @calc_masks_ptr
       shr edx, 16 // Y
     @calc_masks_ptr:
     and edx, 1 // 1 = непровославный
     lea edx, [ebp + TPathMapStack.parent_masks + edx*8]
     mov TPathMapStack[EBP].parent_masks_ptr, edx
   @after_masks_ptr:


   // если маску * парент маску = 0, то в клетку не зайдут (наверно), следовательно не нужно тратить время на её lock
   mov edx, ecx
   shr ecx, 8
   and cl, ch
   jz @open_list_loop

   // сохранить в ebx: опции, тайл, маску
   and EBX, $FF000000 // может потом будут ещё опции
   shl edx, 8 // тайл
   and ecx, $000000FF // маска
   and edx, $0000FF00
   or  EBX, ecx
   or  EBX, edx

   // сохранить текущие параметры:
   mov TPathMapStack[EBP].CurrentNode, ESP
   mov TPathMapStack[EBP].CurrentValue, esi
   mov TPathMapStack[EBP].CurrentPoint, eax


   // current-top routine
   mov edx, TPathMapStack[EBP].TopNodeValue
   cmp esi, edx
   jb @after_top_routine
      mov esp, TPathMapStack[EBP].TopNode

      @top_routine:
         mov ecx, TPathMapNode[esp].greather
         test ecx, ecx
         jz @top_routine_break

         mov esp, ecx
         mov esi, TPathMapNode[esp].value
         cmp esi, edx
         je @top_routine
      @top_routine_break:
      mov TPathMapStack[EBP].TopNode, esp
      mov TPathMapStack[EBP].TopNodeValue, esi
   @after_top_routine:  


   // текущая позиция
   mov ecx, eax
   and ecx, $0000FFFF // X
   shr eax, 16 // Y
//----------
   mov NEED_BREAKPOINT, 0
   cmp ecx, 4 //x
   jne @after_breakpoint_routine
   cmp eax, 3 //y
   jne @after_breakpoint_routine
     mov NEED_BREAKPOINT, 1
   @after_breakpoint_routine:
//----------




   {
      CurrentNumber := NumberOfNode(CurrentNode)
      FIND_LINES_POINTERS (TopLine, MiddleLine, BottomLine)
      include_set(ClosetSet, MiddleLine.BitPtr[BitOffset+2])
      /* exit часть будет в самом конце цикла open_list_loop */
   }
   mov esp, ecx
   mov ecx, TPathMapStack[EBP].MapWidth
   mul ecx
   add eax, esp
   lea edx, [eax*2 - 2] // edx = (CurrentNumber-1)*2
   mov esp, TPathMapStack[EBP].Map
   lea eax, [ecx*2] // eax = MapWidth*2
   mov ecx, TPathMapStack[EBP].BitSet
   sub edx, eax // edx := LowMapNumber*2
   lea esi, [esp + edx*2] // esi := Map + edx*3
   add esi, edx

   mov TPathMapStack[EBP].FCurrentPath, edi
   mov edi, ecx // edi := bitset

   // FIND_LINES_POINTERS
   // заполнить информацию по указателям: CellsPtr, SetPtr, SetOffset
   // для линий TopLine, MiddleLine, BottomLine
   // на данный момент esi = LowMapPtr, edx = LowMapNumber*2, eax=MapWidth*2, edi = BitSet
   // esp/ecx используются как копии edx, чтобы разделить на SetPtr(ecx) и SetOffset(esp)
   mov esp, edx
   mov TPathMapStack[EBP].TopLine.CellsPtr, esi
   mov ecx, edx
   add esp, 8
   add ecx, 8
   and esp, 7
   shr ecx, 3
   lea esi, [esi + eax*2]
   add ecx, edi
   add esi, eax
   dec ecx
   mov dword ptr TPathMapStack[EBP].TopLine.SetOffset, esp
   add edx, eax
   mov TPathMapStack[EBP].TopLine.SetPtr, ecx
   mov esp, edx
   mov TPathMapStack[EBP].MiddleLine.CellsPtr, esi
   mov ecx, edx
   add esp, 8
   add ecx, 8
   and esp, 7
   shr ecx, 3
   lea esi, [esi + eax*2]
   add ecx, edi
   add esi, eax
   dec ecx
   mov dword ptr TPathMapStack[EBP].MiddleLine.SetOffset, esp
   add edx, eax
   mov TPathMapStack[EBP].MiddleLine.SetPtr, ecx
   mov esp, edx
   mov TPathMapStack[EBP].BottomLine.CellsPtr, esi
   and esp, 7
   shr edx, 3
   add edx, edi
   mov dword ptr TPathMapStack[EBP].BottomLine.SetOffset, esp
   mov TPathMapStack[EBP].BottomLine.SetPtr, edx




   // здесь происходит загрузка и коррекция маски
   // причём если вес dest тайла = 0, то занести клетку в список rounded
   // если сложный алгоритм (algorithm_exluded) то из маски вычёркиваются сразу большие области (LEFT, RIGHT, TOP, BOTTOM)
   {
     if (mask.available[left, right]) then load_data(middle_line)
     if (mask.available[top]) then load_data(top_line)
     if (mask.available[bottom]) then load_data(top_line)
   }
   // EBX: опции, тайл, маску
   mov EDI, EBP
   @mask_left_right:
     test ebx, full_left_right
     jz @mask_left_right_just_center_point //@mask_left_right_exit
        // прогрузить middle_line
        mov esi, TPathMapStack[EBP].MiddleLine.CellsPtr
        mov ESP, TPathMapStack[EDI].MiddleLine.SetPtr
        mov ecx, dword ptr TPathMapStack[EBP].MiddleLine.SetOffset

        mov ESP, [ESP]
        ror ESP, cl

        // закрыть ячейку
        or ESP, close_current_point
        mov TPathMapStack[EBP].MiddleLine.SetData, ESP
        mov TPathMapStack[EDI].MiddleLine.SetChanged, 1

        // левая ячейка закрыта/запрещена?
        test ESP, left_closed
        jz @left_closed_finish
          and ebx, not_7
          test ESP, left_opened
          jnz @left_closed_finish
          // ячейка - "excluded". для rounded особый механизм
          test EBX, OPTION_ROUNDED
          jz @left_closed_finish
          and ebx, NOT_LEFT_MASK
        @left_closed_finish:

        // правая ячейка закрыта/запрещена?
        test ESP, right_closed
        jz @right_closed_finish
          and ebx, not_3
          test ESP, right_opened
          jnz @right_closed_finish
          test EBX, OPTION_ROUNDED
          jz @right_closed_finish
          and ebx, NOT_RIGHT_MASK
        @right_closed_finish:

    test EBX, OPTION_ZERO_TILES
    jz @mask_left_right_exit
    test ebx, full_left_right
    jz @mask_left_right_exit

        // лево
        @zero_tile_left:
        test ebx, LEFT_MASK
        jz @zero_tile_left_end
          movzx ecx, byte ptr [esi + CELL_0 + TPathMapCell.Tile]
          cmp ecx, $ff
          je @zero_tile_left_end
          cmp [EBP + sizeof_stack + ecx*8], 0
          jnz @zero_tile_left_end
             or ESP, left_closed
             and ebx, not_7
             mov TPathMapStack[EBP].MiddleLine.SetData, ESP
             //mov TPathMapStack[EDI].MiddleLine.SetChanged, 1
             test EBX, OPTION_ROUNDED
             jz @zero_tile_left_end
             and ebx, NOT_LEFT_MASK
        @zero_tile_left_end:

        // право
        @zero_tile_right:
        test ebx, RIGHT_MASK
        jz @zero_tile_right_end
          movzx ecx, byte ptr [esi + CELL_2 + TPathMapCell.Tile]
          cmp ecx, $ff
          je @zero_tile_right_end
          cmp [EBP + sizeof_stack + ecx*8], 0
          jnz @zero_tile_right_end
             or ESP, right_closed
             and ebx, not_3
             mov TPathMapStack[EBP].MiddleLine.SetData, ESP
             //mov TPathMapStack[EDI].MiddleLine.SetChanged, 1
             test EBX, OPTION_ROUNDED
             jz @zero_tile_right_end
             and ebx, NOT_RIGHT_MASK
        @zero_tile_right_end:

        jmp @mask_left_right_exit
   @mask_left_right_just_center_point:
     // ячейки слева и справа заблокированы маской, но центральную клетку всёравно надо прогрузить
     // потому что потом берётся её tile
     // + надо закрыть ячейку в битовом множестве
        mov ESP, TPathMapStack[EDI].MiddleLine.SetPtr
        mov ecx, dword ptr TPathMapStack[EBP].MiddleLine.SetOffset

        // закрыть ячейку
        mov ESP, [ESP]
        ror ESP, cl        
        or ESP, close_current_point
        mov TPathMapStack[EBP].MiddleLine.SetData, ESP
        mov TPathMapStack[EDI].MiddleLine.SetChanged, 1  
   @mask_left_right_exit:
   @mask_top:
     test ebx, top_mask
     jz @mask_top_exit
        // прогрузить top_line
        mov esi, TPathMapStack[EBP].TopLine.CellsPtr
        mov ESP, TPathMapStack[EDI].TopLine.SetPtr
        mov ecx, dword ptr TPathMapStack[EBP].TopLine.SetOffset

        mov ESP, [ESP]
        ror ESP, cl
        mov TPathMapStack[EBP].TopLine.SetData, ESP


        test ESP, center_closed
        jz @1_closed_finish
          and ebx, not_1
          test ESP, center_opened
          jnz @1_closed_finish
          test EBX, OPTION_ROUNDED
          jz @1_closed_finish
          and ebx, NOT_TOP_MASK
          jmp @mask_top_exit
        @1_closed_finish:          

        test ESP, left_closed
        jz @0_closed_finish
          and ebx, not_0
        @0_closed_finish:

        test ESP, right_closed
        jz @2_closed_finish
          and ebx, not_2
        @2_closed_finish:

     test EBX, OPTION_ZERO_TILES
     jz @mask_top_exit
     test ebx, top_mask
     jz @mask_top_exit

        @zero_tile_top_left:
        test ebx, _0
        jz @zero_tile_top_left_end
           movzx ecx, byte ptr [esi + CELL_0 + TPathMapCell.Tile]
           cmp [EBP + sizeof_stack + ecx*8], 0
           jnz @zero_tile_top_left_end
             or ESP, left_closed
             and ebx, not_0
             mov TPathMapStack[EBP].TopLine.SetData, ESP
             mov TPathMapStack[EDI].TopLine.SetChanged, 1
        @zero_tile_top_left_end:


        @zero_tile_top_center:
          // центральный тайл смотрится в любом случае
          movzx ecx, byte ptr [esi + CELL_1 + TPathMapCell.Tile]
          cmp ecx, $ff
          je @zero_tile_top_center_end
          cmp [EBP + sizeof_stack + ecx*8], 0
          jnz @zero_tile_top_center_end
            or ESP, center_closed
            and ebx, not_1
            mov TPathMapStack[EBP].TopLine.SetData, ESP
            mov TPathMapStack[EDI].TopLine.SetChanged, 1
            test EBX, OPTION_ROUNDED
            jz @zero_tile_top_center_end
            and ebx, NOT_TOP_MASK
            jmp @mask_top_exit
        @zero_tile_top_center_end:

        @zero_tile_top_right:
        test ebx, _2
        jz @zero_tile_top_right_end
           movzx ecx, byte ptr [esi + CELL_2 + TPathMapCell.Tile]
           cmp [EBP + sizeof_stack + ecx*8], 0
           jnz @zero_tile_top_right_end
             or ESP, right_closed 
             and ebx, not_2
             mov TPathMapStack[EBP].TopLine.SetData, ESP
             mov TPathMapStack[EDI].TopLine.SetChanged, 1
        @zero_tile_top_right_end:

   @mask_top_exit:
   @mask_bottom:
     test ebx, bottom_mask
     jz @mask_bottom_exit
        // прогрузить bottom_line
        mov esi, TPathMapStack[EBP].BottomLine.CellsPtr
        mov ESP, TPathMapStack[EDI].BottomLine.SetPtr
        mov ecx, dword ptr TPathMapStack[EBP].BottomLine.SetOffset

        mov ESP, [ESP]
        ror ESP, cl
        mov TPathMapStack[EBP].BottomLine.SetData, ESP


        test ESP, center_closed
        jz @5_closed_finish
          and ebx, not_5
          test ESP, center_opened
          jnz @5_closed_finish
          test EBX, OPTION_ROUNDED
          jz @5_closed_finish
          and ebx, NOT_BOTTOM_MASK
          jmp @mask_bottom_exit
        @5_closed_finish:

        test ESP, left_closed
        jz @6_closed_finish
          and ebx, not_6
        @6_closed_finish:

        test ESP, right_closed
        jz @4_closed_finish
          and ebx, not_4
        @4_closed_finish:

     test EBX, OPTION_ZERO_TILES
     jz @mask_bottom_exit
     test ebx, bottom_mask
     jz @mask_bottom_exit

        @zero_tile_bottom_left:
        test ebx, _6
        jz @zero_tile_bottom_left_end
           movzx ecx, byte ptr [esi + CELL_0 + TPathMapCell.Tile]
           cmp [EBP + sizeof_stack + ecx*8], 0
           jnz @zero_tile_bottom_left_end
             or ESP, left_closed
             and ebx, not_6
             mov TPathMapStack[EBP].BottomLine.SetData, ESP
             mov TPathMapStack[EDI].BottomLine.SetChanged, 1
        @zero_tile_bottom_left_end:


        @zero_tile_bottom_center:
          // центральный тайл смотрится в любом случае
          movzx ecx, byte ptr [esi + CELL_1 + TPathMapCell.Tile]
          cmp ecx, $ff
          je @zero_tile_bottom_center_end
          cmp [EBP + sizeof_stack + ecx*8], 0
          jnz @zero_tile_bottom_center_end
            or ESP, center_closed
            and ebx, not_5
            mov TPathMapStack[EBP].BottomLine.SetData, ESP
            mov TPathMapStack[EDI].BottomLine.SetChanged, 1
            test EBX, OPTION_ROUNDED
            jz @zero_tile_bottom_center_end
            and ebx, NOT_BOTTOM_MASK
            jmp @mask_bottom_exit
        @zero_tile_bottom_center_end:

        @zero_tile_bottom_right:
        test ebx, _4
        jz @zero_tile_bottom_right_end
           movzx ecx, byte ptr [esi + CELL_2 + TPathMapCell.Tile]
           cmp [EBP + sizeof_stack + ecx*8], 0
           jnz @zero_tile_bottom_right_end
             or ESP, right_closed
             and ebx, not_4
             mov TPathMapStack[EBP].BottomLine.SetData, ESP
             mov TPathMapStack[EDI].BottomLine.SetChanged, 1
        @zero_tile_bottom_right_end:
   @mask_bottom_exit:



   // здесь происходит поочерёдный тест всех ячеек, но в обратном порядке
   // в зависимости от флага opened, производится калбек в добавление или изменение ячейки
   // EBP - указатель на все переменные
   // ESP - используется внутри как маска bitset
   // EBX - маска и ОПЦИИ. BH = CurrentTile
   // доступны регистры: EAX, EDX, ECX, ESI, EDI
   @look_bottom:
      test ebx, bottom_mask
      jz @look_bottom_end
      mov ESP, TPathMapStack[EBP].BottomLine.SetData

      @look_6:
      test ebx, _6
      jz @look_6_finish
        // заполнение персональных данных
        mov esi, TPathMapStack[EBP].BottomLine.CellsPtr
        // чист. and ebx, CLEAR_LOOKS
        {add esi, CELL_0}
        or  ebx, LOOK_6
        mov TPathMapStack[EBP]._return_address, OFFSET @look_6_finish

        mov ecx, left_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_6_finish:


      @look_5:
      test ebx, _5
      jz @look_5_finish
        mov esi, TPathMapStack[EBP].BottomLine.CellsPtr
        and ebx, CLEAR_LOOKS
        add esi, CELL_1
        or  ebx, LOOK_5
        mov TPathMapStack[EBP]._return_address, OFFSET @look_5_finish

        mov ecx, center_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_5_finish:


      @look_4:
      test ebx, _4
      jz @look_4_finish
        mov esi, TPathMapStack[EBP].BottomLine.CellsPtr
        and ebx, CLEAR_LOOKS
        add esi, CELL_2
        or  ebx, LOOK_4
        mov TPathMapStack[EBP]._return_address, OFFSET @look_4_finish

        mov ecx, right_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_4_finish:


      test EBX, MODE_SETCHANGED
      jz @look_bottom_end
        mov TPathMapStack[EBP].BottomLine.SetData, ESP
        and EBX, NOT_MODE_SETCHANGED
        mov TPathMapStack[EBP].BottomLine.SetChanged, 1
   @look_bottom_end:
   @look_middle:
      test ebx, left_right
      jz @look_middle_end
      mov ESP, TPathMapStack[EBP].MiddleLine.SetData


      @look_7:
      test ebx, _7
      jz @look_7_finish
        and ebx, CLEAR_LOOKS
        mov esi, TPathMapStack[EBP].MiddleLine.CellsPtr
        {add esi, CELL_0}
        or  ebx, LOOK_7
        mov TPathMapStack[EBP]._return_address, OFFSET @look_7_finish

        mov ecx, left_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_7_finish:


      @look_3:
      test ebx, _3
      jz @look_3_finish
        mov esi, TPathMapStack[EBP].MiddleLine.CellsPtr
        and ebx, CLEAR_LOOKS
        add esi, CELL_2
        or  ebx, LOOK_3
        mov TPathMapStack[EBP]._return_address, OFFSET @look_3_finish

        mov ecx, right_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_3_finish:


      test EBX, MODE_SETCHANGED
      jz @look_middle_end
        mov TPathMapStack[EBP].MiddleLine.SetData, ESP
        and EBX, NOT_MODE_SETCHANGED
        mov TPathMapStack[EBP].MiddleLine.SetChanged, 1
   @look_middle_end:
   @look_top:
      test ebx, top_mask
      jz @look_top_end
      mov ESP, TPathMapStack[EBP].TopLine.SetData


      @look_0:
      test ebx, _0
      jz @look_0_finish
        and ebx, CLEAR_LOOKS
        mov esi, TPathMapStack[EBP].TopLine.CellsPtr
        {add esi, CELL_0}
        or  ebx, LOOK_0
        mov TPathMapStack[EBP]._return_address, OFFSET @look_0_finish

        mov ecx, left_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_0_finish:

      @look_1:
      test ebx, _1
      jz @look_1_finish
        mov esi, TPathMapStack[EBP].TopLine.CellsPtr
        and ebx, CLEAR_LOOKS
        add esi, CELL_1
        or  ebx, LOOK_1
        mov TPathMapStack[EBP]._return_address, OFFSET @look_1_finish

        mov ecx, center_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_1_finish:

      @look_2:
      test ebx, _2
      jz @look_2_finish
        mov esi, TPathMapStack[EBP].TopLine.CellsPtr
        and ebx, CLEAR_LOOKS
        add esi, CELL_2
        or  ebx, LOOK_2
        mov TPathMapStack[EBP]._return_address, OFFSET @look_2_finish

        mov ecx, right_opened
        jmp @ADD_ELEMENT_TO_BUFFER
      @look_2_finish:


      test EBX, MODE_SETCHANGED
      jz @look_top_end
        mov TPathMapStack[EBP].TopLine.SetData, ESP
        and EBX, NOT_MODE_SETCHANGED
        mov TPathMapStack[EBP].TopLine.SetChanged, 1
   @look_top_end:

   
//-------------
@after_sub_procedure:
  // осуществить рутину с BitSet
  // для некоторых линий оно изменилось
  mov edx, EBP
  mov edi, TPathMapStack[EBP].MinDirtyBits
  mov esi, TPathMapStack[EDX].MaxDirtyBits

      mov eax, TPathMapStack[EBP].TopLine.SetPtr
      mov ESP, TPathMapStack[EDX].TopLine.SetData
      mov ecx, dword ptr TPathMapStack[EBP].TopLine.SetOffset
      test ecx, $0000FF00 // SetChanged
      jz @after_topline_set
        rol ESP, cl
        or  [eax], ESP

        cmp eax, edi
        jae @after_top_min
          mov edi, eax
        @after_top_min:
        cmp eax, esi
        jbe @after_top_max
          mov esi, eax
        @after_top_max:
      @after_topline_set:

      mov eax, TPathMapStack[EBP].MiddleLine.SetPtr
      mov ESP, TPathMapStack[EDX].MiddleLine.SetData
      mov ecx, dword ptr TPathMapStack[EBP].MiddleLine.SetOffset
      test ecx, $0000FF00 // SetChanged
      jz @after_middleline_set
        rol ESP, cl
        or  [eax], ESP

        cmp eax, edi
        jae @after_middle_min
          mov edi, eax
        @after_middle_min:
        cmp eax, esi
        jbe @after_middle_max
          mov esi, eax
        @after_middle_max:
      @after_middleline_set:

      mov eax, TPathMapStack[EBP].BottomLine.SetPtr
      mov ESP, TPathMapStack[EDX].BottomLine.SetData
      mov ecx, dword ptr TPathMapStack[EBP].BottomLine.SetOffset
      test ecx, $0000FF00 // SetChanged
      jz @after_bottomline_set
        rol ESP, cl
        or  [eax], ESP

        cmp eax, edi
        jae @after_bottom_min
          mov edi, eax
        @after_bottom_min:
        cmp eax, esi
        jbe @after_bottom_max
          mov esi, eax
        @after_bottom_max:
      @after_bottomline_set:

  mov TPathMapStack[EBP].MinDirtyBits, edi
  mov TPathMapStack[EDX].MaxDirtyBits, esi


  {$ifdef CPFLOG}
    mov eax, TPathMapStack[EBP].IterationNumber
    cmp TPathMapStack[EBP].IterationNumber, 1
    je @run_begin_iteration
    cmp TPathMapStack[EBP].IterationNumber, log_from_iteration
    jb @after_begin_iteration
    @run_begin_iteration:
      mov TPathMapStack[EBP]._eax, esp
      mov esp, TPathMapStack[EBP]._esp
        mov eax, ebp
        call BeginIteration
      mov esp, TPathMapStack[EBP]._eax
    @after_begin_iteration:
    mov EDX, EBP
  {$endif}


  mov ESP, TPathMapStack[EDX].CurrentNode
  mov edx, TPathMapStack[EBP].FAddingBufferSize
  test edx, edx
  {$ifdef CPFLOG}
    jz @PASTE_ADDING_BUFFER_END
  {$endif}
  jz @open_list_loop
  

  jmp @PASTE_ADDING_BUFFER
//----------------------------------------------------------
@ADD_ELEMENT_TO_BUFFER:
// "подпрограмма" - добавление текущего элемента
// в буфер на изменение или в буфер на добавление
// в EBX хранится всё ). esp - множество. ecx - opened_маска
// esi: PPathMapCell{Map}


   // минимизируем обращение к разной памяти
   mov edx, TPathMapStack[EBP].FCurrentPath


   // для начала аллоцируется или находится элемент (PPathMapNode)
   // ссылка на него хранится в eax
   // добавленные элементы заполняются данными после определения пути (в своей секции)
   // так же выставляется флаг MODE_ADDING и изменяется ESP (для добавляемых)
   test ESP, ECX  // opened flag
   jz @alloc_adding_temp
   @alloc_changing_temp:
     // на изменение
     mov edi, dword ptr TPathMapCell[esi].Tile     
     mov eax, TPathMapCell[esi].Node
   jmp @alloc_temp_end
   @alloc_adding_temp:
     // выставляем флаги
     or EBX, MODE_ADDING_SET_CHANGED
     or ESP, ECX

     // на добавление (eax)
     mov eax, TPathMapStack[EBP].FHighNode
     cmp eax, TPathMapStack[EBP].FMaxHighNode
     jna @after_increment_node_buffer_level
       mov TPathMapStack[EBP]._eax, esp
       mov esp, TPathMapStack[EBP]._esp
       push edx
       mov eax, TPathMapStack[EBP].Self
       call TPathMap.IncrementNodeBlockNumber // eax := CorrectFHighNode
       pop edx
       mov esp, TPathMapStack[EBP]._eax
     @after_increment_node_buffer_level:

     // изменить FHighNode (для следующих alloc-ов)
     lea ecx, [eax+sizeof_node]
     mov TPathMapStack[EBP].FHighNode, ecx

     // читаем тайл данные из карты, прописываем туда Node
     mov edi, dword ptr TPathMapCell[esi].Tile
     mov TPathMapCell[esi].Node, eax
   @alloc_temp_end:


   // расчитать PathValue (edx += ecx), tile - edi
   // для диагональных расчётов ebp временно сдвигается
   // esi - это "тайл-копия" чтобы позже положить в нод
   mov esi, edi   
   movzx ecx, bh
   and edi, $000000FF
   test ebx, $00010000 //test ecx, 1
   jnz @calc_weight_buf_1
     add ebp, 4
   @calc_weight_buf_1:
       mov ecx, [ebp + sizeof_stack + ecx*8] // вес пути до Dest ячейки = вес текущей ячейки (может быть диагональный)
       test ebx, OPTION_SMART_WEIGHT
       jz @calc_weight_end
         add ecx, [ebp + sizeof_stack + edi*8] // прибавить соседний вес, поделить на 2
         shr ecx, 1
   @calc_weight_end:
   test ebx, $00010000 //test ecx, 1
   jnz @calc_weight_buf_2
     sub ebp, 4
   @calc_weight_buf_2:
   add edx, ecx


   // велика вероятность того что нод либо добавляется либо изменяется
   // поэтому parent и parent_mask прописываем прямо сейчас
   // в esi нужно положить: 16-parent, 24-parent_mask
   // кроме того задействованы eax(нод), edx(value), ebx(всё :)), esp(set)
   // остаются только ecx и edi
   mov ecx, ebx
   and esi, $0000FFFF
   shr ecx, 16
   mov edi, TPathMapStack[EBP].parent_masks_ptr
   and ecx, 7
   mov edi, [edi + ecx - 2] 
   add ecx, 4
   and edi, $00FF0000
   and ecx, 7
   or  esi, edi
   shl ecx, 24
   or  esi, ecx

   
   // коррекция parent_mask (последний байт esi)
   // рабочие - только edi и ecx
   // mask - это второй (1) байт esi (или первый ebx - bl)
   test ebx, $00010000 // test ecx, 1
   jz @after_parent_mask_correction
   test EBX, OPTION_DIAGONAL
   jz @after_parent_mask_correction
   @parent_mask_correction:
      @look_minus_parent_mask:
        mov ecx, ebx
        shr ecx, 16
        // and ecx, 7
        lea ecx, [ecx+8-2]
        mov edi, 1
        and ecx, 7
        shl edi, cl
        test ebx, edi
        jnz @look_plus_parent_mask
          shl edi, 16
          or  esi, edi

      @look_plus_parent_mask:
        mov ecx, ebx
        shr ecx, 16
        // and ecx, 7
        lea ecx, [ecx+8+2]
        mov edi, 1
        and ecx, 7
        shl edi, cl
        test ebx, edi
        jnz @after_parent_mask_correction
          shl edi, 16
          or  esi, edi
   @after_parent_mask_correction:



   // обработать имеющийся путь(edx)
   // для изменяемых - сравнить и если что удалить из глобального буфера
   // для добавляемых - прописать path, найти point, расчитать эвристику, вписать value
   // eax - TPathMapNode, edx - path. esp служебный. ebx - маска и опции
   test ebx, MODE_ADDING
   jnz @fill_adding_path
   @fill_changing_path:
     // сравнить path с уже имеющимся
     // если меньше или равно - вон из цикла
     // иначе указать нового parent и занести eax в буфер на добавление
     mov edi, TPathMapStack[EBP].TopNode

     // короче ли путь
     mov ecx, TPathMapNode[eax].path
     sub edx, ecx
     jae @return_from_changing

     // TPathMapNode[eax] требует корректировки пути и возможно занесения в буфер на "добавление"
     add ecx, edx
     add edx, TPathMapNode[eax].value
     mov TPathMapNode[eax].value, edx
     mov TPathMapNode[eax].path, ecx
     mov ecx, TPathMapNode[eax].less
     mov dword ptr TPathMapNode[eax].tile, esi // tile/mask + parent, parent_mask

     cmp eax, edi //TPathMapStack[EBP].TopNode
     jne @after_change_top_value
       mov TPathMapStack[EBP].TopNodeValue, edx
     @after_change_top_value:

     cmp edx, TPathMapNode[ecx].value
     jae @return_from_changing

     // удалить из списка
     mov esi, TPathMapNode[eax].greather
     mov TPathMapNode[ecx].greather, esi
     test esi, esi
     jz @after_delete_from_list
     mov TPathMapNode[esi].less, ecx

     @after_delete_from_list:
     xor esi, esi
     jmp @fill_path_end

     // если не нужна обработка
     @return_from_changing:
        jmp TPathMapStack[EBP]._return_address //jmp @ADD_ELEMENT_TO_BUFFER_END
   @fill_adding_path:
     // добавляем узел. нужно прописать value, path, point и tile-dword
     // вместе с тем расчёт эвристики требует много регистров
     // поэтому мы сначала буферизируем нужные данные
     // а потом полностью заполняем узел
     // на выходе должен быть узел (eax) и его value (edx)
     mov TPathMapStack[ebp].buf_node, eax
     mov TPathMapStack[ebp].buf_path, edx

     // !!! можно использовать всего 4 регистра: eax, edx, ecx, edi

     // шаг1. находим DestPos, сохраняем в buf_point
     mov ecx, ebx
     shr ecx, 16
     mov edi, TPathMapStack[ebp].CurrentPoint
     and ecx, 7

     movsx eax, byte ptr [ebp + TPathMapStack.short_offsets + ecx*2]
     movsx edx, byte ptr [ebp + TPathMapStack.short_offsets + ecx*2+1]
     mov ecx, edi
     and edi, $0000FFFF
     shr ecx, 16
     add eax, edi
     add edx, ecx
     mov TPathMapStack[ebp].buf_point.X, ax
     mov TPathMapStack[ebp].buf_point.Y, dx

     // шаг 2. находим dX(ecx) и dY(edi). в случае необходимости прописываем флаг less. (eax |= 0x00010000)
     mov ecx, TPathMapStack[ebp].FinishPoint
     mov edi, ecx
     and ecx, $0000FFFF
     shr edi, 16
     sub ecx, eax
     jnl @after_dx
       neg ecx
       test EBX, OPTION_HEXAGONAL_60
       jz @after_dx
         or eax, $00010000 // флаг less
     @after_dx:

     sub edi, edx
     jnl @after_dy
       neg edi
       test EBX, OPTION_HEXAGONAL_45
       jz @after_dy
         or eax, $00010000 // флаг less
     @after_dy:

     // шаг 3. раскидать случай на разные case-ы эвристики
     // внутри каждого кейса в конечном счёте в eax должна появиться эвристика!
     @calculate_heuristics:
     test EBX, OPTION_DIAGONAL
     jz @calculate_simple_or_hexagonal_heuristics
     @diagonal_heuristics:
        // диагональная эвристика
        // eax = max-min
        // edi = min
        mov eax, ecx // eax := dX        
        sub eax, edi
        jge @mul_diagonal_heuristics
           { eax = min-max, edi = max }
           add edi, eax  //edi:=min
           neg eax  //eax:=max-min
        @mul_diagonal_heuristics:
           mul TPathMapStack[ebp].HeuristicsLine
           xchg eax, edi
           mul TPathMapStack[ebp].HeuristicsDiagonal
           add eax, edi
     jmp @calculate_heuristics_end
     @calculate_simple_or_hexagonal_heuristics:

        // для гексов необходимо определиться с флагом less
        // результат записывается в eax
        // вспомогательный регистр - edx 
        test EBX, TEST_HEXAGONAL_MAP
        jz @after_less_calculation
           { if (Stack.Options and OPTION_HEXAGONAL_45 <> 0) then not_orthodoxy := (X and 1 = 1)
             else not_orthodoxy := (Y and 1 = 1);
             less := (less <> not_orthodoxy); }

             test EBX, OPTION_HEXAGONAL_60
             jnz @calc_less
               mov edx, eax

             @calc_less:
               and edx, 1
               shr eax, 16
               xor eax, edx
        @after_less_calculation:


        // (edx)Rast := (ecx)dx + (edi)dy*2;
        // if (dx > dy) then Rast := dy + dx*2;
        {lea edx, [ecx + edi*2]
        cmp ecx, edi
        jna @after_rast
          lea edx, [edi + ecx*2]
        @after_rast: }


        // простая эвристика или гексагональная
        test EBX, TEST_HEXAGONAL_MAP
        jnz @calculate_hexagonal_heuristics
          // простая эвристика
          // Result := (dx+dy)*integer(Stack.HeuristicsLine{-1}) + Rast;

          lea eax, [ecx + edi]
     //     mov edi, edx
          mul TPathMapStack[ebp].HeuristicsLine
      //    add eax, edi
        jmp @calculate_heuristics_end  
        @calculate_hexagonal_heuristics:
          // гексагональная эвристика
          // if (Stack.Options and OPTION_HEXAGONAL_60 <> 0) then Swap(dx, dy)
          test EBX, OPTION_HEXAGONAL_60
          jz @after_swap_dx_dy
            xchg ecx, edi
          @after_swap_dx_dy:

          // уточнить флаг less (eax)
          // less := less and (dx and 1 = 1);
          and eax, ecx 

          // if (dx = 0) then Result := dy
          test ecx, ecx
          jnz @difficult_hexagonal_calculation
            mov eax, edi
            jmp @mul_hexagonal_heuristics
          // else
          @difficult_hexagonal_calculation:
            {
             Result := dx;
             dec(dy, dx shr 1);
             if (dy > 0) then
             begin
               inc(Result, dy);
               if (less) then dec(Result);
             end;
            }
            shl eax, 16
            or  eax, ecx  //mov ax, cx

            shr ecx, 1
            sub edi, ecx
            jg @hex_plus_dx_dy
              and eax, $0000FFFF // снимаем флаг less
              jmp @mul_hexagonal_heuristics
            @hex_plus_dx_dy:
              mov ecx, eax
              and eax, $0000FFFF
              shr ecx, 16 // ord(less)
              add eax, edi // result := result + dy
              sub eax, ecx // dec(result, ord(less));

          @mul_hexagonal_heuristics:
          // Result := Result*integer((Stack.HeuristicsLine{-1})) + Rast;
        //  mov edi, edx
          mul TPathMapStack[ebp].HeuristicsLine
       //   add eax, edi
        @calculate_hexagonal_heuristics_end:
     @calculate_heuristics_end:


     // сравнивать расчитанную эвристику с правильной функцией
     {$ifdef CPFLOG}
        mov TPathMapStack[ebp]._eax, esp
        mov esp, TPathMapStack[ebp]._esp
          mov edx, ebp
          mov ecx, TPathMapStack[ebp].buf_point
          call CompareHeuristics
        mov esp, TPathMapStack[ebp]._eax
     {$endif}


     // шаг Финиш. присвоить эвристику, заполнить узел данными
     mov edx, eax
     mov eax, TPathMapStack[ebp].buf_node
     mov edi, TPathMapStack[ebp].buf_path
     mov ecx, TPathMapStack[ebp].buf_point
     add edx, edi
     mov TPathMapNode[eax].value, edx
     mov TPathMapNode[eax].path, edi
     mov TPathMapNode[eax].point, ecx
     mov dword ptr TPathMapNode[eax].tile, esi
   @fill_path_end:



   // eax - заполненный нод, который лежит в буфере и требует размещения
   // edx - value
   // возможно узел можно разместисть в Left-Right
   // но если не так, то добавить в AddingBuffer


   cmp edx, TPathMapStack[EBP].CurrentValue
   ja @compare_with_top_node
     mov esi, TPathMapStack[EBP].CurrentNode
     mov edi, TPathMapNode[esi].greather
     jmp @paste_node

   // TopNode рутина
   @compare_with_top_node:
   mov edi, TPathMapStack[EBP].TopNode
   cmp edx, TPathMapStack[EBP].TopNodeValue
  // je @paste_after_top_node
  // jb @paste_before_top_node
   jbe @paste_before_top_node
   jmp @add_to_adding_buffer
   @paste_before_top_node:
     mov TPathMapStack[EBP].TopNode, eax
     mov esi, TPathMapNode[edi].less
     mov TPathMapStack[EBP].TopNodeValue, edx
{     jmp @paste_node
   @paste_after_top_node:
     // надо вставить узел eax перед edi
     mov esi, edi
     mov edi, TPathMapNode[esi].greather
   @paste_top_node_end: } 


   // вставить нод между esi и edi
   @paste_node:
   mov TPathMapNode[esi].greather, eax
   mov TPathMapNode[eax].less, esi
   mov TPathMapNode[eax].greather, edi
   test edi, edi
   jz @after_change_less
   mov TPathMapNode[edi].less, eax
   @after_change_less:
   jmp TPathMapStack[EBP]._return_address // jmp @ADD_ELEMENT_TO_BUFFER_END

//---
   // CurrentNode рутина
(*   mov esi, TPathMapStack[EBP].CurrentNode
   cmp edx, TPathMapStack[EBP].CurrentValue
   jb @paste_less_current
   je @paste_equal_current
   jmp @compare_with_top_node
   @paste_less_current:
     mov edi, TPathMapNode[esi].greather
     test EBX, MODE_WAS_LESS_CURRENT
     jnz @less_current_routine
       or  EBX, MODE_WAS_LESS_CURRENT
       jmp @paste_from_left_to_right

     @less_current_routine:
     // сложная рутина по выявлению наименьшего и "занулению" наибольшего
     // esi current, edi - [esi].greather
     mov ecx, TPathMapStack[EBP].CurrentValue
     cmp edx, TPathMapNode[edi].value
     jbe @less_current_routine_paste_current
       mov esi, edi
       mov TPathMapNode[eax].value, ecx
       mov edi, TPathMapNode[esi].greather
       jmp @paste_from_left_to_right
       
     @less_current_routine_paste_current:
     mov TPathMapNode[edi].value, ecx
     jmp @paste_from_left_to_right
   @paste_less_current_finish:
   @paste_equal_current:
     mov edi, TPathMapNode[esi].greather
     test EBX, MODE_WAS_LESS_CURRENT
     jz @paste_from_left_to_right
       mov esi, edi
       mov edi, TPathMapNode[esi].greather
   @paste_equal_current_end:


   // CurrentNode рутина: вставить узел. лево - esi. право - edi. центр - eax
   @paste_from_left_to_right:
     test edi, edi
     jz @after_change_right_less
       mov TPathMapNode[edi].less, eax
     @after_change_right_less:

     mov TPathMapNode[eax].less, esi
     mov TPathMapNode[eax].greather, edi
     mov TPathMapNode[esi].greather, eax
     
   jmp TPathMapStack[EBP]._return_address // jmp @ADD_ELEMENT_TO_BUFFER_END
   @paste_from_left_to_right_end:



//---
   // TopNode рутина
   @compare_with_top_node:
   mov edi, TPathMapStack[EBP].TopNode
   cmp edx, TPathMapStack[EBP].TopNodeValue
   je @paste_top_node
   jb @change_top_node
   jmp @add_to_adding_buffer
   @change_top_node:
     mov TPathMapStack[EBP].TopNode, eax
     mov TPathMapStack[EBP].TopNodeValue, edx
   @paste_top_node:
     // надо вставить узел eax перед edi
     mov esi, TPathMapNode[edi].less
     mov TPathMapNode[edi].less, eax
     mov TPathMapNode[eax].less, esi
     mov TPathMapNode[eax].greather, edi
     mov TPathMapNode[esi].greather, eax

   jmp TPathMapStack[EBP]._return_address // jmp @ADD_ELEMENT_TO_BUFFER_END
   @paste_top_node_end:   *)

   
//---
   // добавление в Adding - чтобы потом раскидать массивом
   @add_to_adding_buffer:
   mov ecx, TPathMapStack[EBP].FAddingBufferSize
   inc ecx
   mov TPathMapStack[EBP].FAddingBufferSize, ecx
   mov [EBP + TPathMapStack.FAddingBuffer + ecx*8 - 8], edx
   mov [EBP + TPathMapStack.FAddingBuffer + ecx*8 - 4], eax


@ADD_ELEMENT_TO_BUFFER_END: jmp TPathMapStack[EBP]._return_address

//----------------------------------------------------------




  
// ***************************************************************
// **********  добавление элементов >>> **************************
@PASTE_ADDING_BUFFER:

  // для повышения оптимизации
  // adding массив сначала сортируется
  cmp edx, 1
  je @after_sort_loop_break  
  { CurrentElement := @Stack.TempAddingBuffer[0];
    HighElement := @Stack.TempAddingBuffer[Stack.TempAddingBufferSize-1];
    while (CurrentElement <> HighElement) do
    begin
      MinValueElement := CurrentElement; // eax
      IteratorElement := CurrentElement; // ecx
      MinValue := MinValueElement.value; // ebx


      inc(IteratorElement);
      while (integer(IteratorElement) <= integer(HighElement)) do
      begin
        if (IteratorElement.value < MinValue) then
        begin
          MinValue := IteratorElement.value;
          MinValueElement := IteratorElement;
        end;
        inc(IteratorElement);
      end;

      if (MinValueElement <> CurrentElement) then
      begin
        Buf := MinValueElement^;
        MinValueElement^ := CurrentElement^;
        CurrentElement^ := Buf;
      end;

      inc(CurrentElement);
  end; }
  // CurrentElement - esi, HighElement - edi
  lea esi, TPathMapStack[EBP].FAddingBuffer
  lea edi, [esi + edx*8 -8]
  @adding_sort_loop:
    mov eax, esi   //MinValueElement
    mov ecx, esi   //IteratorElement
    mov ebx, TPathMapAddingElement[eax].value //MinValue

    jmp @adding_iterator_continue
    @adding_iterator_loop:
      mov esp, TPathMapAddingElement[ecx].value
      cmp esp, ebx
      jae @adding_iterator_continue
        mov ebx, esp
        mov eax, ecx
    @adding_iterator_continue:
      add ecx, 8
      cmp ecx, edi
    jbe @adding_iterator_loop

    cmp eax, esi
    je @adding_sort_loop_continue   
    // swap [esi] <--> [eax]
    {movq mm0, [esi]
    movq mm1, [eax]
    movq [eax], mm0
    movq [esi], mm1}
    mov esp, TPathMapAddingElement[esi].value
    mov TPathMapAddingElement[esi].value, ebx
    mov ebx, TPathMapAddingElement[esi].node
    mov TPathMapAddingElement[eax].value, esp
    mov esp, TPathMapAddingElement[eax].node
    mov TPathMapAddingElement[eax].node, ebx
    mov TPathMapAddingElement[esi].node, esp
    

  @adding_sort_loop_continue:
    add esi, 8
    cmp esi, edi
  jne @adding_sort_loop
  {$ifdef CPFLOG}
    cmp TPathMapStack[EBP].IterationNumber, log_from_iteration
    jb @after_log_sorted
    mov eax, ebp
    mov esp, TPathMapStack[EBP]._esp
    push edx
    call LogSortedAdding
    pop edx
    @after_log_sorted: 
  {$endif}
  @after_sort_loop_break:



  // цикл рассовывания текущего adding нода (esi)
  // после текущего dest нода (edi)
  mov edi, TPathMapStack[EBP].TopNode
  lea ecx, TPathMapStack[EBP].FAddingBuffer
  @adding_move_loop:
    // edi - узел, относительно которого (после) вставляется esi
    mov ebx, TPathMapAddingElement[ecx].value
    // ebx - value, относительно которого происходит "сортировка"
    mov esi, TPathMapAddingElement[ecx].node
    // esi - рассматриваемый узел, позицию которого возможно нужно поставить


    // цикл поиска куда можно вставить элемент
    // очень ресурсоёмкий в реалиях процесс
    mov eax, TPathMapNode[edi].greather
    test eax, eax
    jz  @find_node_pos_loop_break
    cmp ebx, TPathMapNode[eax].value
    jbe  @find_node_pos_loop_break
    @find_node_pos_loop:
       mov edi, eax
       mov eax, TPathMapNode[edi].greather
       test eax, eax
       jz  @find_node_pos_loop_break

       cmp ebx, TPathMapNode[eax].value
    ja @find_node_pos_loop
    @find_node_pos_loop_break:

    // нужно вставить esi нод
    // left - edi, right - eax
    mov TPathMapNode[edi].greather, esi
    mov TPathMapNode[esi].less, edi
    mov TPathMapNode[esi].greather, eax
    mov  edi, esi  // теперь "текущий" нод - вставленный

    test eax, eax
    jz @adding_move_loop_continue
    mov TPathMapNode[eax].less, esi        


  @adding_move_loop_continue:
  add ecx, 8
  dec edx
  jnz @adding_move_loop

@PASTE_ADDING_BUFFER_END:
// **********  <<< добавление элементов **************************

  {$ifdef CPFLOG}
  cmp TPathMapStack[EBP].IterationNumber, log_from_iteration
  jb @after_end_iteration
    mov eax, ebp
    mov esp, TPathMapStack[EBP]._esp
    call EndIteration
  @after_end_iteration: 
  {$endif}


  mov ESP, TPathMapStack[EBP].CurrentNode
  mov TPathMapStack[EBP].FAddingBufferSize, 0
  mov EBX, TPathMapStack[EBP].Options
  jmp @open_list_loop
// ***<<<<<<<***********  ОСНОВНОЙ ЦИКЛ  ***********************************


// возвращение результат
@return_false:
  xor eax, eax
  jmp @exit
@return_true:


  // ---------- вернуть точки, расчитать дистанцию ---------------
  // FPU: value, distance, 0.5


  // базовая инициализация управляющих структур
  // TPathMapStack[EBP].FPathPoints.points_count (а может и Distance) считается 
  // в конце по StepsLine/StepsDiagonal
  mov edi, TPathMapStack[EBP].FPathPoints
  mov TPathMapStack[EBP].FFindResult.points, edi
  mov TPathMapStack[EBP].StepsLine, 0
  mov TPathMapStack[EBP].StepsDiagonal, 0

  // current_point
  mov eax, TPathMapNode[esp].point

  // занести первую точку в массив
  mov ecx, eax
  mov edx, eax
  and ecx, $0000FFFF
  shr edx, 16
  mov TPoint[edi].X, ecx
  mov TPoint[edi].Y, edx
  add edi, 8

  // цикл
  fld HALF
  fldz
  @path_points_loop:
     // esp - СurrentNode
     // eax - CurrentPoint. чтобы найти DestPoint
     // ebx - parent(bl), tile(bh), options
     { команды мешаются. eax: y, ecx: x, edx: parent*2}
     mov ebx, dword ptr TPathMapNode[esp].tile
       mov ecx, eax
       mov edx, ebx
       and ecx, $0000FFFF
     rol ebx, 8
       shr edx, 24
     and ebx, $0000FFFF
       add edx, edx
     {потом делается: or ebx, TPathMapStack[EBP]._options_}
       mov esp, ebp
       mov esi, edx
       shr eax, 16

     // найти Dest точки
     add ecx, [EBP + TPathMapStack.Offsets + edx*8 + TPathMapOffset.offsX]
     add eax, [ESP + TPathMapStack.Offsets + esi*8 + TPathMapOffset.offsY]
     mov TPoint[edi].X, ecx
     mov TPoint[edi].Y, eax
     mov esi, TPathMapStack[EBP]._map_
     mov edx, TPathMapStack[ESP]._map_width_
     or  ebx, TPathMapStack[EBP]._options_

     // dest number (eax)
     mul edx
     add eax, ecx

     // esi := PPathMapNode(Number). edx := tile. ecx := IsDiaginal
     mov ecx, ebx
     add edi, 8
     movzx edx, bh
     not ecx
     lea eax, [eax + eax*2]
     and ecx, 1
     lea esi, [esi + eax*2]

     // инкрементировать "количество переходов"
     inc dword ptr [EBP + TPathMapStack.StepsLine + ecx*4]
     
     // если нужно считать стоимость перехода - идём в подсчёт
     test EBX, OPTION_ONE_WEIGHT
     jz @value_calculation

  @path_points_loop_continue:
    mov esp, TPathMapCell[esi].node
    mov eax, TPathMapNode[esp].point
    cmp eax, TPathMapStack[EBP].StartPoint
  jne @path_points_loop
  jmp @after_value_calculation

  // "функция" подсчёта стоимости перехода, добавления к текущему Distance
  // edx - cource tile, ebx - Options, esi - dest cell, ecx - диагональность
  @value_calculation:
    mov esp, TPathMapStack[EBP].Values
    test EBX, OPTION_DIAGONAL
    jz @after_table_correction
      lea esp, [esp + ecx*4]
    @after_table_correction:

    fld dword ptr [esp + edx*8]
    test EBX, OPTION_SMART_WEIGHT
    jz @value_calculation_finish
      movzx eax, byte ptr TPathMapCell[esi].tile
      cmp eax, edx
      je @value_calculation_finish
        fadd dword ptr [esp + eax*8]
        fmul st(0), st(2) // HALF
  @value_calculation_finish:
    faddp
  jmp @path_points_loop_continue
  @after_value_calculation:


  // Result
  lea eax, TPathMapStack[EBP].FFindResult
  mov edx, TPathMapStack[EBP].StepsLine
  add edx, TPathMapStack[EBP].StepsDiagonal
  inc edx
  mov TPathMapStack[EBP].FFindResult.points_count, edx


  // для карт высот с одним весом тайлом особый (простой) расчёт длинны
  test EBX, OPTION_ONE_WEIGHT
  jz @after_one_weight_routine
  @one_weight_routine:
    fstp st(0)
    mov esp, TPathMapStack[EBP].Values

    fild dword ptr TPathMapStack[EBP].StepsLine
    fmul dword ptr [esp]
    fild dword ptr TPathMapStack[EBP].StepsDiagonal
    fmul dword ptr [esp+4]
    faddp

  @after_one_weight_routine:

  // Distance
  fstp qword ptr TPathMapStack[EBP].FFindResult.distance
  ffree st(0) // 0.5


// -----------------------
@exit:
  mov ecx, ebp
  mov ebp, TPathMapStack[ecx]._ebp
  mov esp, TPathMapStack[ecx]._esp
  mov edi, TPathMapStack[ecx]._edi
  mov esi, TPathMapStack[ecx]._esi
  mov ebx, TPathMapStack[ecx]._ebx
end;





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
      Result.points := Stack.FPathPoints;
      Result.points_count := 1;
      Result.distance := 0;
      exit; //goto exit_proc;
    end;

    if (SectorTest {$ifndef CPF_DLL}or FSectorsFilled{$endif}) then
    begin
      if (not FSectorsFilled) then
      begin
        FSectorsFilled := true;
        if (FSectorsHandle = nil) then Stack.Sectors := AlignedAlloc(FSectorsHandle, Stack.CellsCount);

        PathMapFillSectors(Stack^);
      end;

      if (Stack.Sectors <> nil) and (Stack.Sectors[StartNumber] <> Stack.Sectors[FinishNumber]) then goto exit_proc;
    end;
  end;


  // на этом этапе нужно подготовить Stack к сложной расчётной функции
  // внутри которой определится путь от стартовой точки до конечной
  // или будет понятно, что путь не найден
  with Stack^ do
  begin
    Options := 0;
    if (FMode in [mmDiagonal, mmDiagonalEx]) then Options := Options or OPTION_DIAGONAL;
    if (FMode = mmDiagonalEx) then Options := Options or OPTION_ROUNDED;
    if (FMode = mmHexagonal45) then Options := Options or OPTION_HEXAGONAL_45;
    if (FMode = mmHexagonal60) then Options := Options or OPTION_HEXAGONAL_60;
    if (FSmartWeight) and (FHighTile <> 0) and (not one_weight) then Options := Options or OPTION_SMART_WEIGHT;
    if (ExcludePoints <> nil) and (ExcludePointsCount > 0) then Options := Options or OPTION_EXCLUDED_POINTS;
    if (zero_tiles) then Options := Options or OPTION_ZERO_TILES;
    if (one_weight) then Options := Options or dword(OPTION_ONE_WEIGHT);
  end;
  Stack._options_ := Stack.Options;


  // базовое заполнение параметров
  if (Self.NodeBlocksCount = 0) then IncrementNodeBlockNumber();
  with Stack^ do
  begin
    IterationNumber := 0;
    FNodes := NodeBlocks[0].Nodes;
    FHighNode := pointer(integer(FNodes)+sizeof(TPathMapNode));
    FMaxHighNode := pointer(integer(FNodes)+(NodeBlocks[0].Size-1)*sizeof(TPathMapNode));

    // в "алгоритм" точки засовываются в обратном порядке
    // чтобы потом правильно (снова в обратном порядке) собрать путь
    StartPoint.X := Finish.X;
    StartPoint.Y := Finish.Y;
    FinishPoint.X := Start.X;
    FinishPoint.Y := Start.Y;

    // первый узел
    FNodes.path := 0;
    FNodes.value := GetHeuristics(Stack^, Stack.StartPoint.X, Stack.StartPoint.Y);
    FNodes.Point.X := Finish.X;  //number := StartNumber;
    FNodes.Point.Y := Finish.Y;
    FNodes.tile := CellFinish.Tile;
    FNodes.mask := CellFinish.Mask;
    FNodes.parent := 8;
    FNodes.parent_mask := $FF;
    FNodes.less := nil;
    FNodes.greather := nil;
    Map[FinishNumber{Start}].Node := FNodes;

    TopNode := FNodes;
    TopNodeValue := FNodes.value;
    CurrentValue := FNodes.value;
  end;



  // заполнить битовое множество, запустить алгоритм на выполнение
  // заполнить массив маршрута
  WasPathFinding := true;  
  Result := PathMap_FIND_PATH(Stack^, ExcludePoints, ExcludePointsCount);

  {$ifdef CPFLOG}
  LogAllStack(Stack^);

  SHowMessage('Количество итераций - %d. Error detected: %d', [Stack.IterationNumber, ord(LogErrorDetected)]);
  if (not LogErrorDetected) then WinExec('Notepad.exe log.txt', SW_SHOWNORMAL);
  {$endif}

exit_proc:
   // чистка буфера
   if (Result = nil) and (not WasPathFinding) then
   begin
     ReleaseHighNodeBlock();
   end else
   begin
     // если выделено например 3 блока, а последний find занял только один блок (CurrentBlockNumber = 0)
     if (NodeBlocksCount - CurrentBlockNumber > 2) then ReleaseHighNodeBlock();
   end;
   CurrentBlockNumber := 0;
end;







{ TPathMapWeights }

constructor TPathMapWeights.Create(const AHighTile: TPathMapTile);
var
  i: integer;
  Value: psingle;
begin
  inherited Create;

  if (AHighTile = $FF) then
  EWrongParameter.Assert('High tile can''t be equal 255(0xFF), becouse that const means "WALL"');

  FHighTile := AHighTile;

  // Alloc arrays
  FDwordValues := AlignedAlloc(FAllValuesHandle, (AHighTile+1)*4*sizeof(dword){+ singles});
  FFloatValues := FDwordValues;
  inc(integer(FFloatValues), (AHighTile+1)*2*sizeof(dword));

  // базовое заполнение floats value
  Value := FFloatValues;
  for i := 0 to AHighTile do
  begin
    //FValues[i] := 1.0;
    Value^ := 1.0;
    inc(Value);
    Value^ := SQRT_2;
    inc(Value);
  end;
end;

destructor TPathMapWeights.Destroy;
begin
  FreeMem(FAllValuesHandle);

  inherited;
end;

function TPathMapWeights.GetValue(const Tile: TPathMapTile): single;
type
  TSingleArray = array[0..0] of single;
  PSingleArray = ^TSingleArray;
begin
  // проверка
  if (Tile > HighTile) then
  EWrongParameter.Assert('Wrong tile = %d. High tile is %d', [Tile, HighTile]);


  Result := PSingleArray(FFloatValues)[Tile*2];
end;

procedure TPathMapWeights.SetValue(const Tile: TPathMapTile; const Value: single);
type
  TSingleArray = array[0..0] of single;
  PSingleArray = ^TSingleArray;
begin
  // проверка
  if (Tile > HighTile) then
  EWrongParameter.Assert('Wrong tile = %d. High tile is %d', [Tile, HighTile]);

  PSingleArray(FFloatValues)[Tile*2] := Value;
  PSingleArray(FFloatValues)[Tile*2+1] := Value*SQRT_2;
  FReady := false;
end;

function TPathMapWeights.Prepare(AMap: TObject; var zero_tiles, one_weight: boolean): pdword;
type
  TDwordArray = array[0..0] of dword;
  TSingleArray = array[0..0] of single;
  PSingleArray = ^TSingleArray;
label
  _exit;
const
  FLT_MAX = 3.402823466e+38;
  ZERO_TILE_VALUE = 0.1;
  AVAILABLE_MAX_DIFFERENCE = 1000; //?
var
  Map: TPathMap absolute AMap;
  i: TPathMapTile;
  MapDiagonal{, MapHexagonal}: boolean;
  Value, MinValue, MaxValue: single;
  Divisor, HighValue: Extended;
  Dest: ^TDwordArray;
  FloatValues: PSingleArray;

  procedure ThrowDifference();
  var
    Dif, Min, Max: string;
  begin
    Str(HighValue:0:2, Dif);
    Str(MinValue:0:2, Min);
    Str(MaxValue:0:2, Max);

    EWrongParameter.Assert('Too big difference ('+Dif+') between minimum ('+Min+') and maximum ('+Max+') weights');
  end;

  function WeightValue(const X: extended): dword;
  begin
    Result := round(X * Divisor);

    if (Result = 0) then Result := 1
    else
    if (Result > FWeightMul) then Result := FWeightMul;
  end;
begin
  // проверка
  if (Map.HighTile > Self.HighTile) then
  EWrongParameter.Assert('Weight high tile = %d. Map high tile = %d', [Self.HighTile, Map.HighTile]);

  Result := pdword(FDwordValues);
  FloatValues := FFloatValues;

  if (not FReady) or (FMapMode <> Map.Mode) or (FWeightMul <> Map.FWeightMul) then
  begin
    FMapMode := Map.Mode;
    MapDiagonal := (FMapMode in [mmDiagonal, mmDiagonalEx]);
//    MapHexagonal := (FMapMode in [mmHexagonal45, mmHexagonal60]);
    FWeightMul := Map.FWeightMul;

    // поиск минимального максимального
    MinValue := FLT_MAX;
    MaxValue := 0;
    FZeroTiles := false;
    for i := 0 to HighTile do
    begin
      Value := FloatValues[i*2];

      if (Value >= ZERO_TILE_VALUE) then
      begin
        if (Value > MaxValue) then MaxValue := Value;
        if (Value < MinValue) then MinValue := Value;
      end else
      begin
        FZeroTiles := true;
      end;
    end;

    // если вообще нет тайлов с нормальным весом
    if (MaxValue < ZERO_TILE_VALUE) then
    begin
      FMinLine := 0;
      FMinDiagonal := 0;
      FOneTileWeight := true;
      FillDword(Result^, (Self.HighTile+1)*4, 0);
      goto _exit;
    end;

    HighValue := MaxValue/MinValue;
    if (HighValue > AVAILABLE_MAX_DIFFERENCE) then ThrowDifference();
    FOneTileWeight := (abs(HighValue-1) <= 0.001);

    // необходимо расчитать такие int-коэфициэнты, которые сохраняли бы
    // соотношения весов тайлов. от 1 до HighValue
    if (MapDiagonal) then HighValue := HighValue*SQRT_2;
    Divisor := FWeightMul{high(word)}/HighValue/MinValue;
    Dest := pointer(Result);

    for i := 0 to HighTile do
    begin
      Value := FloatValues[i*2];

      if (Value < ZERO_TILE_VALUE) then
      begin
        Dest[i*2] := 0;
        Dest[i*2+1] := 0;
      end else
      begin
        Dest[i*2] := WeightValue(Value);

        if (not MapDiagonal) then Dest[i*2+1] := Dest[i*2]
        else Dest[i*2+1] := WeightValue(Value * SQRT_2);
      end;
    end;

    // коэфициенты для эвристики
    FMinLine := WeightValue(MinValue);
    FMinDiagonal := 0;    
    if (MapDiagonal) then
    begin
      FMinDiagonal := WeightValue(MinValue * SQRT_2);
    end else
    begin
      //if (FMinLine > 1) then dec(FMinLine); // для эвристики на расстоянии
    end;

    
  _exit:
    FReady := true;
    Map.LastWeights := nil;
  end;
  zero_tiles := FZeroTiles;
  one_weight := FOneTileWeight;

  // копирование данных
  if (Map.LastWeights <> Self) then
  begin
    Map.LastWeights := Self;
    PPathMapStack(Map.FStack).HeuristicsLine := FMinLine;
    PPathMapStack(Map.FStack).HeuristicsDiagonal := FMinDiagonal;
    CopyDwords(pointer(integer(Map.FStack)+sizeof(TPathMapStack)), Result, (Map.HighTile+1)*4);
  end;
end;



end.


