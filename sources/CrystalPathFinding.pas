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

{.$define CPFAPI}
{.$define CPFLIB}
{.$define CPFDBG}
{.$define CPFLOG}

// compiler directives
{$ifdef FPC}
  {$mode Delphi}
  {$asmmode Intel}
  {$define INLINESUPPORT}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}
{$else}
  {$if CompilerVersion >= 24}
    {$LEGACYIFEND ON}
  {$ifend}
  {$if CompilerVersion >= 15}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ifend}
  {$if (CompilerVersion < 23)}
    {$define CPUX86}
  {$ifend}
  {$if (CompilerVersion >= 17)}
    {$define INLINESUPPORT}
  {$ifend}
  {$if CompilerVersion >= 21}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$ifend}
  {$if (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
    {$define INTERNALCODEPAGE}
  {$ifend}
{$endif}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$if Defined(CPUX86) or Defined(CPUX64)}
  {$define CPUINTEL}
{$ifend}
{$if Defined(CPUX64) or Defined(CPUARM64)}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}

{$ifdef CPF_GENERATE_LOOKUPS}
  {$undef CPFLIB}
  {$undef CPFLIBEX}
{$endif}

{$ifdef CPFLIBEX}
  {$define CPFLIB}
{$endif}

{$ifdef CPFLIB}
  {$define CPFAPI}
  {$undef CPFDBG}
  {$undef CPFLOG}
{$endif}

{$ifdef CPFLOG}
  {$define CPFDBG}
{$endif}

{$if Defined(FPC) and Defined(CPFLIB)}
  {$define FPCLIBRARY}
{$ifend}

interface
  uses Types
       {$ifNdef CPFLIB}
         {$ifdef KOL}
           , KOL, err
         {$else}
           , SysUtils
           {$if Defined(CPFDBG) or Defined(CPF_GENERATE_LOOKUPS)}, Classes{$ifend}
         {$endif}
       {$endif};

{$if Defined(FPC) or (CompilerVersion < 22) or Defined(CPFDBG) or (not Defined(CPFLIB))}
type
{$ifend}
  // standard types
  {$ifdef FPC}
    Integer = Longint;
    PInteger = ^Integer;
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
  {$ifdef CPFDBG}
    TPointDynArray = array of TPoint;
    PPointDynArray = ^TPointDynArray;
  {$endif}

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

const
  // map tile barrier
  TILE_BARRIER = Byte(0);

type
  // map kind
  TTileMapKind = (mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal);
  PTileMapKind = ^TTileMapKind;

  // points as array
  PPointList = ^TPointList;
  TPointList = array[0..High(Integer) div SizeOf(TPoint) - 1] of TPoint;

  // result of find path function
  TTileMapPath = record
    Index: NativeInt;
    Points: PPointList;
    Count: NativeInt;
    Distance: Double;
  end;

  // internal class
  TCPFExceptionString = {$ifdef CPFLIB}PWideChar{$else}string{$endif};
  {$ifdef CPFDBG}{$M+}{$endif}
  TCPFClass = {$ifdef CPFLIB}object{$else}class(TObject){$endif}
  protected
    FCallAddress: Pointer;

    procedure CPFException(const Message: TCPFExceptionString);
    procedure CPFExceptionFmt(const Fmt: TCPFExceptionString; const Args: array of {$ifdef CPFLIB}Integer{$else}const{$endif});
    procedure CPFGetMem(var P: Pointer; const Size: NativeUInt);
    procedure CPFFreeMem(var P: Pointer);
    procedure CPFReallocMem(var P: Pointer; const NewSize: NativeUInt);
  end;
  TCPFClassPtr = {$ifdef CPFLIB}^{$endif}TCPFClass;
  {$ifdef CPFDBG}{$M-}{$endif}

  // internal resizable memory buffer
  PCPFBuffer = ^TCPFBuffer;
  TCPFBuffer = object
  private
    FOwner: Pointer{TCPFClassPtr};
    FMemory: Pointer;
    FAllocatedSize: NativeUInt;
    function Realloc(const Size: NativeUInt): Pointer;
  public
    procedure Initialize(const Owner: TCPFClass); {$ifdef INLINESUPPORT}inline;{$endif}
    function Alloc(const Size: NativeUInt): Pointer; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure Free; {$ifdef INLINESUPPORT}inline;{$endif}
    property Memory: Pointer read FMemory;
  end;

  // internal tile weight storage
  PCPFWeightsInfo = ^TCPFWeightsInfo;
  TCPFWeightsInfo = object
  public
    Count: Cardinal;
    Singles: array[1..255] of Cardinal{Single};
    RefCount: Cardinal;
    UpdateId: Cardinal;

    {class} function NewInstance(const Address: Pointer): PCPFWeightsInfo;
    procedure Release(const Address: Pointer);
  public
    PrepareId: Cardinal;
    Minimum: Cardinal{Single};
    Scale: Double;

    procedure Prepare;
  end;

  // map weights
  TTileMapWeights = {$ifdef CPFLIB}object{$else}class{$endif}(TCPFClass)
  private
    FInfo: PCPFWeightsInfo;

    procedure RaiseBarrierTile;
    function GetValue(const Tile: Byte): Single;
    procedure SetValue(const Tile: Byte; const Value: Single);
  {$ifdef CPFLIB}
  public
    procedure Destroy;
  {$else}
    {$ifdef AUTOREFCOUNT}protected{$else}public{$endif}
    destructor Destroy; override;
  {$endif}
  public
    {$ifdef CPFLIB}procedure{$else}constructor{$endif} Create;
    procedure Clear;

    property Values[const Tile: Byte]: Single read GetValue write SetValue; default;
  end;
  TTileMapWeightsPtr = {$ifdef CPFLIB}^{$endif}TTileMapWeights;


  // compact cell coordinates
  PCPFPoint = ^TCPFPoint;
  TCPFPoint = packed record
    Y: Word;
    X: Word;
  end;

  // map cell
  TCPFCell = packed record
  case Boolean of
    False:
    (
       AllocatedFlags: Byte;
       Mask: Byte;
       _: Byte;
       Tile: Byte;
    );
    True: (NodePtr: Cardinal);
  end;
  PCPFCell = ^TCPFCell;
  TCPFCellArray = array[0..0] of TCPFCell;
  PCPFCellArray = ^TCPFCellArray;

  // node type
  PCPFNode = ^TCPFNode;
  TCPFNode = packed record
  case Boolean of
    False: (
              SortValue: Cardinal; // path + heuristics to finish point
              Path: Cardinal; // path from start point to the cell
              Prev, Next: PCPFNode;
              Coordinates: TCPFPoint;
              case Integer of
              0: (
                   ParentAndFlags: Byte
                   {
                     Parent:3;
                     KnownPath:1;
                     case Boolean of
                       False: (Way:3; ClockWise:1);
                        True: (KnownChild:3; Attainable:1);
                     end;
                   };
                   Mask: Byte;
                   ParentMask: Byte;
                   Tile: Byte;
                 );
              1: (NodeInfo: Cardinal);
            );
    True:  (
              nAttainableLength, _{Path}: Cardinal;
              AttainableDistance: Double;
           );
  end;

  PCPFOffsets = ^TCPFOffsets;
  TCPFOffsets = array[0..7] of NativeInt;

  TCPFInfo = record
    CellArray: PCPFCellArray;
    MapWidth: NativeInt;
    HeuristicsLine: NativeInt;
    HeuristicsDiagonal: NativeInt;
    CellOffsets: TCPFOffsets;
    FinishPoint: TCPFPoint;
    NodeAllocator: record
      NewNode: PCPFNode;
      NewNodeLimit: PCPFNode;
      {$ifdef LARGEINT}
      LargeModifier: NativeInt;
      {$endif}
      Count: NativeUInt;
      Buffers: array[0..31] of Pointer;
    end;
  end;
  TCPFNodeBuffers = array[0..31] of NativeUInt;
  PCPFNodeBuffers = ^TCPFNodeBuffers;

  TCPFStart = packed record
    XYDistance: Integer;
    Node: PCPFNode;
    {$ifNdef LARGEINT}
      __Align: Int64;
    {$endif}
    case Boolean of
      True: (KnownPathNode: PCPFNode; Distance: Double; X, Y: Word);
      False:(
        Length: NativeUInt;
        case Boolean of
          True: (DistanceAsInt64: Int64);
         False: (DistanceLowDword: Cardinal; DistanceHighDword, XY: Integer);
      );
  end;
  PCPFStart = ^TCPFStart;

  // path finding parameters
  TTileMapParams = record
    Starts: PPoint;
    StartsCount: NativeUInt;
    Finish: TPoint;
    Weights: TTileMapWeightsPtr;
    Excludes: PPoint;
    ExcludesCount: NativeUInt;
  end;
  PTileMapParams = ^TTileMapParams;

  // main path finding class
  TTileMap = {$ifdef CPFLIB}object{$else}class{$endif}(TCPFClass)
  private
    FInfo: TCPFInfo;
    FWidth: Word;
    FHeight: Word;
    FCellCount: NativeUInt;
    FKind: TTileMapKind;
    FSectorTest: Boolean;
    FCaching: Boolean;
    FSameDiagonalWeight: Boolean;
    FSectorOffsets: TCPFOffsets;
    FTileWeightScaleLine: Double;
    FTileWeightScaleDiagonal: Double;
    FTileWeightLimitLine: Cardinal;
    FTileWeightLimitDiagonal: Cardinal;
    FTileWeightMinimumLine: Cardinal;
    FTileWeightMinimumDiagonal: Cardinal;
    DEFAULT_DIAGONAL_WEIGHT_VALUE: Cardinal;

    procedure RaiseCoordinates(const X, Y: Integer; const Id: TCPFExceptionString);
    procedure UpdateCellMasks(const ChangedArea: TRect);
    function GetTile(const X, Y: Word): Byte;
    procedure SetTile(const X, Y: Word; Value: Byte);
    procedure GrowNodeAllocator(var Buffer: TCPFInfo);
    function DoFindPathLoop(StartNode: PCPFNode): PCPFNode;
    function DoFindPath(const ParamsPtr: NativeUInt{high bit is FullPath}): TTileMapPath;
  private
    FNodes: record
      Storage: record
        Buffers: array[0..31] of Pointer;
        Count: NativeUInt;
      end;
      AttainableTree: Boolean;
      HotPool: record
        First: TCPFNode;
        Last: TCPFNode;
        Unattainable: Boolean;
      end;
      ExcludedPool: record
        First: TCPFNode;
        Last: TCPFNode;
      end;
      HeuristedPool: record
        First: TCPFNode;
        Last: TCPFNode;
      end;
      UnattainablePool: record
        First: TCPFNode;
        Last: TCPFNode;
      end;
    end;
    FActualInfo: record
      TilesChanged: Boolean;
      SectorsChanged: Boolean;
      Sectors: PByte;
      ChangedFinishCount: Cardinal;
      PathlessFinishPoint: TPoint;

      Weights: record
        Current: PCPFWeightsInfo;
        UpdateId: Cardinal;
        Count: Cardinal;

        ScaleLine: Double;
        ScaleDiagonal: Double;
        case Boolean of
        False: (
                 SinglesDiagonal: array[0..255] of Single;
                 SinglesLine: array[0..255] of Single;
                 CardinalsDiagonal: array[0..255] of Cardinal;
                 CardinalsLine: array[0..255] of Cardinal;
                );
         True: (
                 SingleValues: array[0..1, 0..255] of Single;
                 CardinalValues: array[0..1, 0..255] of Cardinal;
               );
      end;
      Starts: record
        Buffer: TCPFBuffer;
        Count: NativeUInt;
      end;
      Excludes: record
        Buffer: TCPFBuffer;
        Count: NativeUInt;
      end;
      FoundPath: record
        Index: NativeInt;
        Buffer: TCPFBuffer;
        FullPath: Boolean;
        Length: NativeUInt;
        case Boolean of
         True: (Distance: Double);
         False: (DistanceAsInt64: Int64);
      end;
    end;
    function AllocateHeuristedNode(X, Y: NativeInt): PCPFNode;
    function ActualizeWeights(Weights: PCPFWeightsInfo; Compare: Boolean): Boolean;
    function ActualizeStarts(const Params: TTileMapParams{; Compare: Boolean}): Boolean;
    function ActualizeExcludes(Points: PPoint; Count: NativeUInt; Compare: Boolean): Boolean;
    procedure ActualizeSectors;
    procedure FreeStorageTopBuffer;
    procedure FreeAllocatedNodes(const ClearCells: Boolean);
    procedure ForgetAttainableTreeNodes;
    procedure ReleasePoolNodes(var PoolFirst, PoolLast: TCPFNode);
    procedure FlushHotPoolNodes;
    procedure UnlockExcludedNodes;
    procedure CacheAttainablePath(var StartPoint: TCPFStart; const FinishNode: PCPFNode);
    procedure FillAttainablePath(const StartNode, FinishNode: PCPFNode);
    procedure FillStandardPath(const StartNode, FinishNode: PCPFNode);
  {$ifdef CPFLIB}
  public
    procedure Destroy;
  {$else}
    {$ifdef AUTOREFCOUNT}protected{$else}public{$endif}
    destructor Destroy; override;
  {$endif}
  {$ifdef CPFDBG}
  published
    function CellInformation(const X, Y: Word): string;
    function NodeInformation(const Node: PCPFNode): string;
    function HotPoolInformation: string;
    procedure SaveHotPoolToFile(const FileName: string = 'HotPool.txt');
    function CachedAttainablePoints: TPointDynArray;
    function CachedUnattainablePoints: TPointDynArray;
  {$endif}
  public
    {$ifdef CPFLIB}procedure{$else}constructor{$endif}
      Create(const AWidth, AHeight: Word; const AKind: TTileMapKind; const ASameDiagonalWeight: Boolean = False);
    procedure Clear;
    procedure Update(const ATiles: PByte; const X, Y, AWidth, AHeight: Word; const Pitch: NativeInt = 0);

    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property Kind: TTileMapKind read FKind;
    property SameDiagonalWeight: Boolean read FSameDiagonalWeight;
    property SectorTest: Boolean read FSectorTest write FSectorTest;
    property Caching: Boolean read FCaching write FCaching;
    property Tiles[const X, Y: Word]: Byte read GetTile write SetTile; default;

    function FindPath(const Params: TTileMapParams; const FullPath: Boolean = True): TTileMapPath; overload;
    function FindPath(const Start, Finish: TPoint; const Weights: TTileMapWeightsPtr = nil;
      const Excludes: PPoint = nil; const ExcludesCount: NativeUInt = 0;
      const FullPath: Boolean = True): TTileMapPath; overload;
    function FindPath(const Starts: PPoint; const StartsCount: NativeUInt;
      const Finish: TPoint; const Weights: TTileMapWeightsPtr = nil;
      const Excludes: PPoint = nil; const ExcludesCount: NativeUInt = 0;
      const FullPath: Boolean = True): TTileMapPath; overload;
  end;
  TTileMapPtr = {$ifdef CPFLIB}^{$endif}TTileMap;


{$ifdef CPFAPI}
type
  PCPFHandle = ^TCPFHandle;
  TCPFHandle = type NativeUInt;

  {$ifdef CPFLIB}
  TCPFAlloc = function(Size: NativeUInt): Pointer; cdecl;
  TCPFFree = function(P: Pointer): Boolean; cdecl;
  TCPFRealloc = function(P: Pointer; Size: NativeUInt): Pointer; cdecl;
  TCPFException = procedure(Message: PWideChar; Address: Pointer); cdecl;
  TCPFCallbacks = packed record
    Alloc: TCPFAlloc;
    Free: TCPFFree;
    Realloc: TCPFRealloc;
    Exception: TCPFException;
  end;
  PCPFCallbacks = ^TCPFCallbacks;
  procedure cpfInitialize(const Callbacks: {$ifdef FPCLIBRARY}PCPFCallbacks{$else}TCPFCallbacks{$endif}); cdecl;
  {$endif}

  function  cpfCreateWeights: TCPFHandle; cdecl;
  procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl;
  function  cpfWeightGet(HWeights: TCPFHandle; Tile: Byte): Single; cdecl;
  procedure cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single); cdecl;
  procedure cpfWeightsClear(HWeights: TCPFHandle); cdecl;
  function  cpfCreateMap(Width, Height: Word; Kind: TTileMapKind; SameDiagonalWeight: Boolean = False): TCPFHandle; cdecl;
  procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl;
  procedure cpfMapClear(HMap: TCPFHandle); cdecl;
  procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt = 0); cdecl;
  function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): Byte; cdecl;
  procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte); cdecl;
  function  cpfFindPath(HMap: TCPFHandle; Params: PTileMapParams; SectorTest: Boolean = False; Caching: Boolean = True; FullPath: Boolean = True): TTileMapPath; cdecl;
{$endif}

implementation
{$ifNdef CPFLIB}
  {$ifNdef KOL}uses SysConst{$endif};

var
  MemoryManager: {$if Defined(FPC) or (CompilerVersion < 18)}TMemoryManager{$else}TMemoryManagerEx{$ifend};
{$endif}

const
  SQRT2: Double = 1.4142135623730950488016887242097;
  HALF: Double = 0.5;


{$ifdef CPFDBG}
var
  MAP_INFO: ^TCPFInfo;

function MaskToString(const Mask: Byte): string;
var
  i: Integer;
begin
  if (Mask = 0) then Result := 'none'
  else
  if (Mask = $FF) then Result := 'all'
  else
  begin
    Result := '';
    for i := 0 to 7 do
    if (Mask and (1 shl i) <> 0) then
      Result := Result + IntToStr(i);
  end;
end;

function MapCellIndex(Cell: PCPFCell): Integer;
begin
  if (MAP_INFO = nil) or (Cell = nil) then
  begin
    Result := 0;
  end else
  begin
    Result := (NativeInt(Cell) - NativeInt(MAP_INFO.CellArray)) div SizeOf(TCPFCell);
  end;
end;

function MapCellX(Cell: PCPFCell): Integer;
begin
  Result := MapCellIndex(Cell);

  if (Result <> 0) then
    Result := Result mod MAP_INFO.MapWidth;
end;

function MapCellY(Cell: PCPFCell): Integer;
begin
  Result := MapCellIndex(Cell);

  if (Result <> 0) then
    Result := Result div MAP_INFO.MapWidth;
end;

function MapCellPoint(Cell: PCPFCell): TPoint;
var
  Index: Integer;
begin
  Index := MapCellIndex(Cell);

  if (Index = 0) then
  begin
    Result := Point(0, 0);
  end else
  begin
    Result.X := Index mod MAP_INFO.MapWidth;
    Result.Y := Index div MAP_INFO.MapWidth;
  end;
end;
{$endif}

{$ifdef CPFLOG}
const
  BOM_UTF8: array[0..2] of Byte = ($EF, $BB, $BF);
  CRLF: Word = 13 + (10 shl 8);

var
  CpfLogFile: TFileStream;

procedure Log(const S: {$ifdef UNICODE}string{$else}WideString{$endif}); overload;
var
  UTF8: {$ifdef UNICODE}RawByteString{$else}AnsiString{$endif};
begin
  UTF8 := UTF8Encode(S);

  CpfLogFile.Write(Pointer(UTF8)^, Length(UTF8));
  CpfLogFile.Write(CRLF, SizeOf(CRLF));
end;

procedure Log(const FmtStr: {$ifdef UNICODE}string{$else}WideString{$endif}; const Args: array of const); overload;
begin
  Log({$ifdef UNICODE}Format{$else}WideFormat{$endif}(FmtStr, Args));
end;
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


// todo PFC?
{$if (Defined(FPC) or (CompilerVersion < 23)) and (not Defined(FPCLIBRARY))}
function ReturnAddress: Pointer;
asm
  mov eax, [ebp+4]
end;
{$ifend}

procedure ZeroMemory(Destination: Pointer; Length: NativeUInt);
{$if Defined(CPFLIB)}
label
  _2, done;
var
  P: PByte;
begin
  P := Destination;

  while (Length >= SizeOf(NativeUInt)) do
  begin
    PNativeUInt(P)^ := 0;

    Dec(Length, SizeOf(NativeUInt));
    Inc(P, SizeOf(NativeUInt));
  end;

  {$ifdef LARGEINT}
  if (Length >= SizeOf(Cardinal)) then
  begin
    PCardinal(P)^ := 0;

    Dec(Length, SizeOf(Cardinal));
    Inc(P, SizeOf(Cardinal));
  end;
  {$endif}

  if (Length <> 0) then
  begin
    if (Length and 1 <> 0) then
    begin
      P^ := 0;
      Inc(P);
      if (Length and 2 = 0) then goto done;
      goto _2;
    end else
    begin
      _2:
      PWord(P)^ := 0;
    end;
  end;
done:
end;
{$elseif Defined(INLINESUPPORT)} inline;
begin
  FillChar(Destination^, Length, 0);
end;
{$else .CPUX86}
asm
  xor ecx, ecx
  jmp System.@FillChar
end;
{$ifend}

{$ifdef CPFLIB}
procedure Move(const Source; var Dest; Count: NativeUInt);
label
  _2, done;
var
  S, D: PByte;
begin
  S := @Source;
  D := @Dest;

  while (Count >= SizeOf(NativeUInt)) do
  begin
    PNativeUInt(D)^ := PNativeUInt(S)^;

    Dec(Count, SizeOf(NativeUInt));
    Inc(S, SizeOf(NativeUInt));
    Inc(D, SizeOf(NativeUInt));
  end;

  {$ifdef LARGEINT}
  if (Count >= SizeOf(Cardinal)) then
  begin
    PCardinal(D)^ := PCardinal(S)^;

    Dec(Count, SizeOf(Cardinal));
    Inc(S, SizeOf(Cardinal));
    Inc(D, SizeOf(Cardinal));
  end;
  {$endif}

  if (Count <> 0) then
  begin
    if (Count and 1 <> 0) then
    begin
      D^ := S^;
      Inc(S);
      Inc(D);
      if (Count and 2 = 0) then goto done;
      goto _2;
    end else
    begin
      _2:
      PWord(D)^ := PWord(S)^;
    end;
  end;

done:
end;
{$endif}

procedure FillCardinal(Destination: PCardinal; Count, Value: NativeUInt);
begin
  {$ifdef LARGEINT}
    Value := Value or (Value shl 32);

    while (Count > 1) do
    begin
      PNativeUInt(Destination)^ := Value;

      Dec(Count, 2);
      Inc(Destination, 2);
    end;

    if (Count <> 0) then
      Destination^ := Value;
  {$else .SMALLINT}
    while (Count <> 0) do
    begin
      Destination^ := Value;

      Dec(Count);
      Inc(Destination);
    end;
  {$endif}
end;

{$if Defined(CPFLIB) or Defined(KOL)}
function CompareMem(_P1, _P2: Pointer; Length: NativeUInt): Boolean;
label
  _2, done, fail;
var
  P1, P2: PByte;
begin
  P1 := _P1;
  P2 := _P2;

  while (Length >= SizeOf(NativeUInt)) do
  begin
    if (PNativeUInt(P1)^ <> PNativeUInt(P2)^) then goto fail;

    Dec(Length, SizeOf(NativeUInt));
    Inc(P2, SizeOf(NativeUInt));
    Inc(P1, SizeOf(NativeUInt));
  end;

  {$ifdef LARGEINT}
  if (Length >= SizeOf(Cardinal)) then
  begin
    if (PCardinal(P1)^ <> PCardinal(P2)^) then goto fail;

    Dec(Length, SizeOf(Cardinal));
    Inc(P2, SizeOf(Cardinal));
    Inc(P1, SizeOf(Cardinal));
  end;
  {$endif}

  if (Length <> 0) then
  begin
    if (Length and 1 <> 0) then
    begin
      if (P1^ <> P2^) then goto fail;
      Inc(P2);
      Inc(P1);
      if (Length and 2 = 0) then goto done;
      goto _2;
    end else
    begin
      _2:
      if (PWord(P1)^ <> PWord(P2)^) then goto fail;
    end;
  end;

done:
  Result := True;
  Exit;
fail:
  Result := False;
end;
{$ifend}

function CPFRound(const X: Double): Integer; {$ifdef INLINESUPPORT}inline;{$endif}
const
  ROUND_CONST: Double = 6755399441055744.0;
var
  Buffer: Double;
begin
  Buffer := X + ROUND_CONST;
  Result := PInteger(@Buffer)^;
end;


{$ifdef CPFLIB}
var
  CPFCallbacks: TCPFCallbacks;
{$endif}

procedure CPFException(const Message: TCPFExceptionString; const Address: Pointer);
begin
  {$ifdef CPFLIB}
    if Assigned(CPFCallbacks.Exception) then
      CPFCallbacks.Exception(Message, Address);

     // guaranteed halt (Assert)
     {$ifNdef CPFLIBEX}
       System.ErrorAddr := Address;
       if (System.ExitCode = 0) then System.ExitCode := 207{reInvalidOp};
       System.Halt;
     {$endif}
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
  {$ifNdef CPFLIBEX}
    System.ExitCode := 203{reOutOfMemory};
  {$endif}
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
  {$ifNdef CPFLIBEX}
    System.ExitCode := 204{reInvalidPtr};
  {$endif}
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

    if (not CPFCallbacks.Free(P)) then
      RaiseInvalidPointer(Address);
  {$else}
    if (MemoryManager.FreeMem(P) <> 0) then
      RaiseInvalidPointer(Address);
  {$endif}
  end;
end;

function CPFRealloc({$ifNdef FPC}const{$endif} P: Pointer; const Size: NativeUInt; const Address: Pointer): Pointer;
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

{$ifdef FPCLIBRARY}
procedure __cpfInitialize(const Callbacks: PCPFCallbacks; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfInitialize(const Callbacks: TCPFCallbacks); cdecl;
{$endif}
var
  Address: Pointer;
  Done: Boolean;
begin
  Address := ReturnAddress;

  with Callbacks{$ifdef FPCLIBRARY}^{$endif} do
  Done := Assigned(Alloc) and Assigned(Free) and Assigned(Realloc) and (Assigned(Exception));

  if (not Done) then
  begin
    if (not Assigned(CPFCallbacks.Exception)) then
      CPFCallbacks.Exception := Callbacks.Exception;

    RaiseCallbacks(Address);
  end;

  CPFCallbacks := Callbacks{$ifdef FPCLIBRARY}^{$endif};
end;

{$ifdef FPCLIBRARY}
procedure cpfInitialize(const Callbacks: PCPFCallbacks); cdecl;
begin
  __cpfInitialize(Callbacks, {todo}@cpfInitialize);
end;
{$endif}
{$endif .CPFLIB}


{$ifdef CPFAPI}
function NewCPFClassInstance(AClass: {$ifNdef CPFLIB}TClass{$else}NativeUInt{$endif};
  Address: Pointer): TCPFHandle;
begin
  {$ifdef CPFLIB}
    Result := TCPFHandle(CPFAlloc(AClass, Address));
    ZeroMemory(Pointer(Result), AClass);
  {$else}
    Result := TCPFHandle(AClass.NewInstance);
  {$endif}
  TCPFClassPtr(Result).FCallAddress := Address;
end;

{$ifdef FPCLIBRARY}
function  __cpfCreateWeights(ReturnAddress: Pointer): TCPFHandle; cdecl;
{$else}
function  cpfCreateWeights: TCPFHandle; cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  Result := NewCPFClassInstance({$ifdef CPFLIB}SizeOf{$endif}(TTileMapWeights), Address);
  TTileMapWeightsPtr(Result).Create;
end;

{$ifdef FPCLIBRARY}
function  cpfCreateWeights: TCPFHandle; cdecl;
begin
  Result := __cpfCreateWeights({todo}@cpfCreateWeights);
end;
{$endif}

{$ifdef FPCLIBRARY}
procedure __cpfDestroyWeights(var HWeights: TCPFHandle; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl;
{$endif}
var
  Address: Pointer;
  Weights: Pointer;
begin
  Address := ReturnAddress;
  Weights := Pointer(HWeights);
  HWeights := 0;

  if (Weights <> nil) then
  begin
    TTileMapWeightsPtr(Weights).FCallAddress := Address;
    {$if Defined(AUTOREFCOUNT) and not Defined(CPFLIB)}
      TTileMapWeightsPtr(Weights).__ObjRelease;
    {$else}
      TTileMapWeightsPtr(Weights).Destroy;
      {$ifdef CPFLIB}
        CPFFree(Weights, Address);
      {$endif}
    {$ifend}
  end;
end;

{$ifdef FPCLIBRARY}
procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl;
begin
  __cpfDestroyWeights(HWeights, {todo}@cpfDestroyWeights);
end;
{$endif}

{$ifdef FPCLIBRARY}
function  __cpfWeightGet(HWeights: TCPFHandle; Tile: Byte; ReturnAddress: Pointer): Single; cdecl;
{$else}
function  cpfWeightGet(HWeights: TCPFHandle; Tile: Byte): Single; cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HWeights <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HWeights).FCallAddress := Address;

  Result := TTileMapWeightsPtr(HWeights).Values[Tile];
end;

{$ifdef FPCLIBRARY}
function  cpfWeightGet(HWeights: TCPFHandle; Tile: Byte): Single; cdecl;
begin
  Result := __cpfWeightGet(HWeights, Tile, {todo}@cpfWeightGet);
end;
{$endif}

{$ifdef FPCLIBRARY}
procedure __cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single); cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HWeights <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HWeights).FCallAddress := Address;

  TTileMapWeightsPtr(HWeights).Values[Tile] := Value;
end;

{$ifdef FPCLIBRARY}
procedure cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single); cdecl;
begin
  __cpfWeightSet(HWeights, Tile, Value, {todo}@cpfWeightSet);
end;
{$endif}

{$ifdef FPCLIBRARY}
procedure __cpfWeightsClear(HWeights: TCPFHandle; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfWeightsClear(HWeights: TCPFHandle); cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;

  if (HWeights <= $ffff) then
  begin
    RaiseInvalidPointer(Address);
  end else
  with TTileMapWeightsPtr(HWeights){$ifdef CPFLIB}^{$endif} do
  begin
    FCallAddress := Address;
    Clear;
  end;
end;

{$ifdef FPCLIBRARY}
procedure cpfWeightsClear(HWeights: TCPFHandle); cdecl;
begin
  __cpfWeightsClear(HWeights, {todo}@cpfWeightsClear);
end;
{$endif}

{$ifdef FPCLIBRARY}
function  __cpfCreateMap(Width, Height: Word; Kind: TTileMapKind;
  SameDiagonalWeight: Boolean; ReturnAddress: Pointer): TCPFHandle; cdecl;
{$else}
function  cpfCreateMap(Width, Height: Word; Kind: TTileMapKind;
  SameDiagonalWeight: Boolean): TCPFHandle; cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  Result := NewCPFClassInstance({$ifdef CPFLIB}SizeOf{$endif}(TTileMap), Address);
  TTileMapPtr(Result).Create(Width, Height, Kind, SameDiagonalWeight);
end;

{$ifdef FPCLIBRARY}
function  cpfCreateMap(Width, Height: Word; Kind: TTileMapKind;
  SameDiagonalWeight: Boolean): TCPFHandle; cdecl;
begin
  Result := __cpfCreateMap(Width, Height, Kind, SameDiagonalWeight, {todo}@cpfCreateMap);
end;
{$endif}

{$ifdef FPCLIBRARY}
procedure __cpfDestroyMap(var HMap: TCPFHandle; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl;
{$endif}
var
  Address: Pointer;
  Map: Pointer;
begin
  Address := ReturnAddress;
  Map := Pointer(HMap);
  HMap := 0;

  if (Map <> nil) then
  begin
    TTileMapWeightsPtr(Map).FCallAddress := Address;
    {$if Defined(AUTOREFCOUNT) and not Defined(CPFLIB)}
      TTileMapWeightsPtr(Map).__ObjRelease;
    {$else}
      TTileMapWeightsPtr(Map).Destroy;
      {$ifdef CPFLIB}
        CPFFree(Map, Address);
      {$endif}
    {$ifend}
  end;
end;

{$ifdef FPCLIBRARY}
procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl;
begin
  __cpfDestroyMap(HMap, {todo}@cpfDestroyMap);
end;
{$endif}

{$ifdef FPCLIBRARY}
procedure __cpfMapClear(HMap: TCPFHandle; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfMapClear(HMap: TCPFHandle); cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  TTileMapPtr(HMap).Clear;
end;

{$ifdef FPCLIBRARY}
procedure cpfMapClear(HMap: TCPFHandle); cdecl;
begin
  __cpfMapClear(HMap, {todo}@cpfMapClear);
end;
{$endif}

{$ifdef FPCLIBRARY}
procedure __cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt); cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  TTileMapPtr(HMap).Update(Tiles, X, Y, Width, Height, Pitch);
end;

{$ifdef FPCLIBRARY}
procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt); cdecl;
begin
  __cpfMapUpdate(HMap, Tiles, X, Y, Width, Height, Pitch, {todo}@cpfMapUpdate);
end;
{$endif}

{$ifdef FPCLIBRARY}
function  __cpfMapGetTile(HMap: TCPFHandle; X, Y: Word; ReturnAddress: Pointer): Byte; cdecl;
{$else}
function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): Byte; cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  Result := TTileMapPtr(HMap).Tiles[X, Y];
end;

{$ifdef FPCLIBRARY}
function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): Byte; cdecl;
begin
  Result := __cpfMapGetTile(HMap, X, Y, {todo}@cpfMapGetTile);
end;
{$endif}

{$ifdef FPCLIBRARY}
procedure __cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte; ReturnAddress: Pointer); cdecl;
{$else}
procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte); cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  TTileMapPtr(HMap).Tiles[X, Y] := Value;
end;

{$ifdef FPCLIBRARY}
procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte); cdecl;
begin
  __cpfMapSetTile(HMap, X, Y, Value, {todo}@cpfMapSetTile);
end;
{$endif}

{$ifdef FPCLIBRARY}
function  __cpfFindPath(HMap: TCPFHandle; Params: PTileMapParams; SectorTest, Caching, FullPath: Boolean; ReturnAddress: Pointer): TTileMapPath; cdecl;
{$else}
function  cpfFindPath(HMap: TCPFHandle; Params: PTileMapParams; SectorTest, Caching, FullPath: Boolean): TTileMapPath; cdecl;
{$endif}
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);

  if (Params = nil) then
  begin
    Result.Points := nil;
    Result.Count := 0;
    Result.Distance := 0;
    Exit;
  end;
  if (NativeUInt(Params) <= $ffff) then RaiseInvalidPointer(Address);

  TCPFClassPtr(HMap).FCallAddress := Address;
  TTileMapPtr(HMap).SectorTest := SectorTest;
  TTileMapPtr(HMap).Caching := Caching;
  Result := TTileMapPtr(HMap).DoFindPath(NativeUInt(Params) + NativeUInt(FullPath) shl {HIGH_NATIVE_BIT}{$ifdef LARGEINT}63{$else}31{$endif});
end;

{$ifdef FPCLIBRARY}
function  cpfFindPath(HMap: TCPFHandle; Params: PTileMapParams; SectorTest, Caching, FullPath: Boolean): TTileMapPath; cdecl;
begin
  Result := __cpfFindPath(HMap, Params, SectorTest, Caching, FullPath, {todo}@cpfFindPath);
end;
{$endif}

{$endif .CPFAPI}


{ TCPFClass }

procedure TCPFClass.CPFException(const Message: TCPFExceptionString);
begin
  CrystalPathFinding.CPFException(Message, FCallAddress);
end;

procedure TCPFClass.CPFExceptionFmt(const Fmt: TCPFExceptionString;
  const Args: array of {$ifdef CPFLIB}Integer{$else}const{$endif});
begin
  CrystalPathFinding.CPFExceptionFmt(Fmt, Args, FCallAddress);
end;

type
  PAligned16Info = ^TAligned16Info;
  TAligned16Info = record
    Handle: Pointer;
    Size: NativeUInt;
  end;

// aligned 16 alloc
procedure TCPFClass.CPFGetMem(var P: Pointer; const Size: NativeUInt);
var
  Handle: Pointer;
  V: PAligned16Info;
begin
  if (Size = 0) then
  begin
    P := nil;
  end else
  begin
    // allocate
    Handle := CrystalPathFinding.CPFAlloc(Size + SizeOf(TAligned16Info) + 16, FCallAddress);

    // allocate and align 16
    V := Pointer((NativeInt(Handle) + SizeOf(TAligned16Info) + 15) and -16);
    P := V;

    // store information
    Dec(V);
    V.Handle := Handle;
    V.Size := Size;
  end;
end;

// aligned 16 free
procedure TCPFClass.CPFFreeMem(var P: Pointer);
var
  V: PAligned16Info;
begin
  V := P;
  if (V <> nil) then
  begin
    P := nil;
    Dec(V);
    CrystalPathFinding.CPFFree(V.Handle, FCallAddress);
  end;
end;

// aligned 16 realloc
procedure TCPFClass.CPFReallocMem(var P: Pointer; const NewSize: NativeUInt);
var
  V: PAligned16Info;
  Info: TAligned16Info;
  Handle: Pointer;
  CopySize: NativeUInt;
begin
  V := P;
  if (V = nil) then
  begin
    CPFGetMem(P, NewSize);
  end else
  if (NewSize = 0) then
  begin
    CPFFreeMem(P);
  end else
  begin
    // store last information
    Dec(V);
    Info := V^;

    // try to realloc
    Handle := CrystalPathFinding.CPFRealloc(Info.Handle, NewSize + SizeOf(TAligned16Info) + 16, FCallAddress);
    V := Pointer(NativeUInt(Handle) + (NativeUInt(V) - NativeUInt(Info.Handle)));
    V.Handle := Handle;
    V.Size := NewSize;
    Inc(V);
    P := V;

    // failure align 16
    if ((NativeUInt(Handle) and 15) <> (NativeUInt(Info.Handle) and 15)) then
    begin
      CPFGetMem(P, NewSize);

      CopySize := Info.Size;
      if (NewSize < CopySize) then CopySize := NewSize;
      Move(V^, P^, CopySize);

      CrystalPathFinding.CPFFree(Handle, FCallAddress);
    end;
  end;
end;


{ TCPFBuffer }

procedure TCPFBuffer.Initialize(const Owner: TCPFClass);
begin
  FOwner := Pointer({$ifdef CPFLIB}@{$endif}Owner);
  FMemory := nil;
  FAllocatedSize := 0;
end;

function TCPFBuffer.Realloc(const Size: NativeUInt): Pointer;
begin
  if (FMemory <> nil) then
    TCPFClassPtr(FOwner).CPFFreeMem(FMemory);

  case Size of
       0..64: FAllocatedSize := 64;
     65..256: FAllocatedSize := 256;
    257..512: FAllocatedSize := 512;
  else
    FAllocatedSize := NativeUInt((NativeInt(Size) + 1023) and -1024);
  end;

  TCPFClassPtr(FOwner).CPFGetMem(FMemory, FAllocatedSize);
  Result := FMemory;
end;

function TCPFBuffer.Alloc(const Size: NativeUInt): Pointer;
begin
  if (Size <= FAllocatedSize) then
  begin
    Result := FMemory;
  end else
  begin
    Result := Realloc(Size);
  end;
end;

procedure TCPFBuffer.Free;
begin
  TCPFClassPtr(FOwner).CPFFreeMem(FMemory);
end;


type
  TYXSmallPoint = record
    y: SmallInt;
    x: SmallInt;
  end;

  TChildList = array[0..7] of Word;
  PChildList = ^TChildList;

const
  NODESTORAGE_INFO: array[0..31] of packed record
    Count: Cardinal;
    Previous: Cardinal;
  end = (
    { 0} (Count:     512; Previous:        0),
    { 1} (Count:     512; Previous:      512),
    { 2} (Count:    1024; Previous:     1024),
    { 3} (Count:    1024; Previous:     2048),
    { 4} (Count:    2048; Previous:     3072),
    { 5} (Count:    2048; Previous:     5120),
    { 6} (Count:    4096; Previous:     7168),
    { 7} (Count:    4096; Previous:    11264),
    { 8} (Count:    8192; Previous:    15360),
    { 9} (Count:    8192; Previous:    23552),
    {10} (Count:   16384; Previous:    31744),
    {11} (Count:   16384; Previous:    48128),
    {12} (Count:   32768; Previous:    64512),
    {13} (Count:   32768; Previous:    97280),
    {14} (Count:   32768; Previous:   130048),
    {15} (Count:   32768; Previous:   162816),
    {16} (Count:   65536; Previous:   195584),
    {17} (Count:   65536; Previous:   261120),
    {18} (Count:  131072; Previous:   326656),
    {19} (Count:  131072; Previous:   457728),
    {20} (Count:  262144; Previous:   588800),
    {21} (Count:  262144; Previous:   850944),
    {22} (Count:  524288; Previous:  1113088),
    {23} (Count:  524288; Previous:  1637376),
    {24} (Count: 1048576; Previous:  2161664),
    {25} (Count: 1048576; Previous:  3210240),
    {26} (Count: 1255424; Previous:  4258816),
    {27} (Count: 2097152; Previous:  5514240),
    {28} (Count: 2097152; Previous:  7611392),
    {29} (Count: 2097152; Previous:  9708544),
    {30} (Count: 2097152; Previous: 11805696),
    {31} (Count: 2097152; Previous: 13902848)
  );

  CELLCOUNT_LIMIT = 16*1000*1000;
  PATHLENGTH_LIMIT = 6666667;
  NATTANABLE_LENGTH_LIMIT = not Cardinal(PATHLENGTH_LIMIT);
  SORTVALUE_LIMIT = NATTANABLE_LENGTH_LIMIT - 1;
  FLAG_KNOWN_PATH = 1 shl 3;
  FLAG_ATTAINABLE = 1 shl 7;
  FLAGS_CLEAN_MASK = Integer(not ($1f shl 3));
  PATHLESS_TILE_WEIGHT = High(Cardinal) shr 1;

  {$ifdef LARGEINT}
    LARGE_NODEPTR_OFFSET = 32 - {0..31}5;
  {$endif}

  NODEPTR_FLAG_ALLOCATED = 1;
  NODEPTR_FLAG_HEURISTED = 2;
  NODEPTR_CLEAN_MASK = Integer((not 7) {$ifdef LARGEINT} and ((1 shl LARGE_NODEPTR_OFFSET) - 1){$endif});

  SECTOR_EMPTY = 0;
  SECTOR_PATHLESS = 1;

  {$ifdef LARGEINT}
    HIGH_NATIVE_BIT = 63;
  {$else}
    HIGH_NATIVE_BIT = 31;
  {$endif}

  _0 = (1 shl 0);
  _1 = (1 shl 1);
  _2 = (1 shl 2);
  _3 = (1 shl 3);
  _4 = (1 shl 4);
  _5 = (1 shl 5);
  _6 = (1 shl 6);
  _7 = (1 shl 7);

  NOT_TOP_MASK = not (_0 or _1 or _2);
  NOT_RIGHT_MASK = not (_2 or _3 or _4);
  NOT_BOTTOM_MASK = not (_4 or _5 or _6);
  NOT_LEFT_MASK = not (_6 or _7 or _0);

  DEFAULT_MASKS: array[TTileMapKind] of Word = (
    {mkSimple}     (_1 or _3 or _5 or _7) * $0101,
    {mkDiagonal}   $FF * $0101,
    {mkDiagonalEx} $FF * $0101,
    {mkHexagonal}  (_0 or _1 or _3 or _5 or _6 or _7) * $0100 +
                   (_1 or _2 or _3 or _4 or _5 or _7)
  );

  POINT_OFFSETS: array[0..7] of TYXSmallPoint = (
    {0} (y: -1; x: -1),
    {1} (y: -1; x:  0),
    {2} (y: -1; x: +1),
    {3} (y:  0; x: +1),
    {4} (y: +1; x: +1),
    {5} (y: +1; x:  0),
    {6} (y: +1; x: -1),
    {7} (y:  0; x: -1)
  );

  POINT_OFFSETS_INVERT: array[0..7] of TYXSmallPoint = (
    {0 --> 4} (y: +1; x: +1),
    {1 --> 5} (y: +1; x:  0),
    {2 --> 6} (y: +1; x: -1),
    {3 --> 7} (y:  0; x: -1),
    {4 --> 0} (y: -1; x: -1),
    {5 --> 1} (y: -1; x:  0),
    {6 --> 2} (y: -1; x: +1),
    {7 --> 3} (y:  0; x: +1)
  );

  MIN_WEIGHT_VALUE_LINE = Cardinal($3DCCCCCD){0.1};
  MAX_WEIGHT_VALUE_LINE = Cardinal($42C80000){100.0};
  DEFAULT_WEIGHT_VALUE_LINE = Cardinal($3F800000){1.0};
  ERROR_WEIGHT_VALUE = 'Invalid weight value. 0,0..0,1 - pathless, 0,1..100,0 - correct';

  CHILD_ARRAYS: array[0..31{way:3;clockwise:1;hexagonal:1}] of TChildList = (
   ($0864, $0440, $1080, $0220, $20A4, $0100, $40C0, $80E0),
   ($80E4, $40C0, $0100, $20A0, $0224, $1080, $0440, $0860),
   ($20A4, $1080, $40C0, $0860, $80E4, $0440, $0100, $0220),
   ($1080, $0860, $20A0, $0440, $40C0, $0220, $80E0, $0100),
   ($40C0, $20A0, $80E0, $1080, $0100, $0860, $0220, $0440),
   ($0224, $0100, $0440, $80E0, $0864, $40C0, $1080, $20A0),
   ($0440, $0220, $0860, $0100, $1080, $80E0, $20A0, $40C0),
   ($0100, $80E0, $0220, $40C0, $0440, $20A0, $0860, $1080),
   ($0864, $1080, $0440, $20A0, $0224, $40C0, $0100, $80E0),
   ($80E4, $0100, $40C0, $0220, $20A4, $0440, $1080, $0860),
   ($20A4, $40C0, $1080, $80E0, $0864, $0100, $0440, $0220),
   ($1080, $20A0, $0860, $40C0, $0440, $80E0, $0220, $0100),
   ($40C0, $80E0, $20A0, $0100, $1080, $0220, $0860, $0440),
   ($0224, $0440, $0100, $0860, $80E4, $1080, $40C0, $20A0),
   ($0440, $0860, $0220, $1080, $0100, $20A0, $80E0, $40C0),
   ($0100, $0220, $80E0, $0440, $40C0, $0860, $20A0, $1080),
   ($0860, $0440, $1080, $0220, $20A0, $0100, $40C0, $80E0),
   ($80E0, $40C0, $0100, $20A0, $0220, $1080, $0440, $0860),
   ($1080, $20A0, $40C0, $0860, $80E0, $0440, $0220, $0100),
   ($0860, $1080, $20A0, $0440, $40C0, $0220, $80E0, $0100),
   ($80E0, $40C0, $20A0, $1080, $0100, $0860, $0220, $0440),
   ($0100, $0220, $0440, $80E0, $0860, $40C0, $20A0, $1080),
   ($0860, $0440, $0220, $0100, $1080, $80E0, $20A0, $40C0),
   ($80E0, $0100, $0220, $40C0, $0440, $20A0, $0860, $1080),
   ($0860, $1080, $0440, $20A0, $0220, $40C0, $0100, $80E0),
   ($80E0, $0100, $40C0, $0220, $20A0, $0440, $1080, $0860),
   ($40C0, $20A0, $1080, $80E0, $0860, $0100, $0220, $0440),
   ($0860, $1080, $20A0, $40C0, $0440, $80E0, $0220, $0100),
   ($80E0, $40C0, $20A0, $0100, $1080, $0220, $0860, $0440),
   ($0440, $0220, $0100, $0860, $80E0, $1080, $20A0, $40C0),
   ($0860, $0440, $0220, $1080, $0100, $20A0, $80E0, $40C0),
   ($80E0, $0100, $0220, $0440, $40C0, $0860, $20A0, $1080)
  );

  SIMPLE_DIAGONAL_WAY_BITS: array[0..127{oddxy:1;simple:1;moveleft:1;movedown:1;way:3}] of Byte = (
    $80, $80, $80, $80, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00,
    $10, $10, $10, $10, $10, $10, $10, $10, $90, $90, $90, $90, $90, $90, $90, $90,
    $A0, $A0, $A0, $A0, $20, $20, $20, $20, $A0, $A0, $A0, $A0, $20, $20, $20, $20,
    $30, $30, $80, $20, $30, $30, $80, $20, $30, $30, $80, $20, $30, $30, $80, $20,
    $C0, $C0, $10, $A0, $C0, $C0, $10, $A0, $C0, $C0, $10, $A0, $C0, $C0, $10, $A0,
    $50, $50, $50, $50, $D0, $D0, $D0, $D0, $50, $50, $50, $50, $D0, $D0, $D0, $D0,
    $E0, $E0, $00, $D0, $E0, $E0, $00, $D0, $E0, $E0, $00, $D0, $E0, $E0, $00, $D0,
    $70, $70, $90, $50, $70, $70, $90, $50, $70, $70, $90, $50, $70, $70, $90, $50
  );

  SIMPLE_DIAGONAL_DXSMALLER_WAY_BITS: array[0..127{oddxy:1;simple:1;moveleft:1;movedown:1;way:3}] of Byte = (
    $80, $80, $80, $80, $80, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00,
    $10, $10, $10, $10, $10, $10, $10, $10, $90, $90, $90, $90, $90, $90, $90, $90,
    $A0, $A0, $A0, $A0, $20, $20, $20, $20, $A0, $A0, $A0, $A0, $20, $20, $20, $20,
    $B0, $B0, $80, $20, $B0, $B0, $80, $20, $B0, $B0, $80, $20, $B0, $B0, $80, $20,
    $40, $40, $10, $A0, $40, $40, $10, $A0, $40, $40, $10, $A0, $40, $40, $10, $A0,
    $50, $50, $50, $50, $D0, $D0, $D0, $D0, $50, $50, $50, $50, $D0, $D0, $D0, $D0,
    $60, $60, $00, $D0, $60, $60, $00, $D0, $60, $60, $00, $D0, $60, $60, $00, $D0,
    $F0, $F0, $90, $50, $F0, $F0, $90, $50, $F0, $F0, $90, $50, $F0, $F0, $90, $50
  );

  HEXAGONAL_WAY_BITS: array[0..255{oddyfinish:1;odddy:1;dxsmaller:1;moveleft:1;movedown:1;way:3}] of Byte = (
    $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $10, $10, $10, $10, $10, $10, $10, $10, $10, $10, $10, $10, $10, $10, $10, $10,
    $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90, $90,
    $A0, $A0, $A0, $20, $A0, $A0, $A0, $20, $20, $20, $A0, $20, $20, $20, $A0, $20,
    $A0, $A0, $A0, $20, $A0, $A0, $A0, $20, $20, $20, $A0, $20, $20, $20, $A0, $20,
    $30, $30, $30, $30, $B0, $B0, $B0, $B0, $30, $30, $30, $30, $B0, $B0, $B0, $B0,
    $30, $30, $30, $30, $B0, $B0, $B0, $B0, $30, $30, $30, $30, $B0, $B0, $B0, $B0,
    $C0, $C0, $C0, $C0, $40, $40, $40, $40, $C0, $C0, $C0, $C0, $40, $40, $40, $40,
    $C0, $C0, $C0, $C0, $40, $40, $40, $40, $C0, $C0, $C0, $C0, $40, $40, $40, $40,
    $50, $50, $50, $D0, $50, $50, $50, $D0, $D0, $D0, $50, $D0, $D0, $D0, $50, $D0,
    $50, $50, $50, $D0, $50, $50, $50, $D0, $D0, $D0, $50, $D0, $D0, $D0, $50, $D0,
    $E0, $E0, $E0, $E0, $60, $60, $60, $60, $E0, $E0, $E0, $E0, $60, $60, $60, $60,
    $E0, $E0, $E0, $E0, $60, $60, $60, $60, $E0, $E0, $E0, $E0, $60, $60, $60, $60,
    $70, $70, $70, $70, $F0, $F0, $F0, $F0, $70, $70, $70, $70, $F0, $F0, $F0, $F0,
    $70, $70, $70, $70, $F0, $F0, $F0, $F0, $70, $70, $70, $70, $F0, $F0, $F0, $F0
  );

  PARENT_BITS: array[0..63{oddy:1;hexagonal:1;simple:1;child:3}] of NativeUInt = (
    $00C70004, $00C70004, $00C70004, $00000004, $00000004, $00000004, $00000004, $00000004,
    $00070005, $00070005, $004F0005, $00970005, $00DF0005, $00DF0005, $00DF0005, $00DF0005,
    $001F0006, $001F0006, $00000006, $001F0006, $00000006, $00000006, $00000006, $00000006,
    $001C0007, $001C0007, $003E0007, $005D0007, $007F0007, $007F0007, $007F0007, $007F0007,
    $007C0000, $007C0000, $00000000, $007C0000, $00000000, $00000000, $00000000, $00000000,
    $00700001, $00700001, $00790001, $00F40001, $00FD0001, $00FD0001, $00FD0001, $00FD0001,
    $00F10002, $00F10002, $00F10002, $00000002, $00000002, $00000002, $00000002, $00000002,
    $00C10003, $00C10003, $00D50003, $00E30003, $00F70003, $00F70003, $00F70003, $00F70003
  );

  ROUNDED_MASKS: array[Byte] of Byte = (
    $00, $00, $02, $02, $00, $00, $02, $02, $08, $08, $0A, $0A, $08, $08, $0E, $0E,
    $00, $00, $02, $02, $00, $00, $02, $02, $08, $08, $0A, $0A, $08, $08, $0E, $0E,
    $20, $20, $22, $22, $20, $20, $22, $22, $28, $28, $2A, $2A, $28, $28, $2E, $2E,
    $20, $20, $22, $22, $20, $20, $22, $22, $38, $38, $3A, $3A, $38, $38, $3E, $3E,
    $00, $00, $02, $02, $00, $00, $02, $02, $08, $08, $0A, $0A, $08, $08, $0E, $0E,
    $00, $00, $02, $02, $00, $00, $02, $02, $08, $08, $0A, $0A, $08, $08, $0E, $0E,
    $20, $20, $22, $22, $20, $20, $22, $22, $28, $28, $2A, $2A, $28, $28, $2E, $2E,
    $20, $20, $22, $22, $20, $20, $22, $22, $38, $38, $3A, $3A, $38, $38, $3E, $3E,
    $80, $80, $82, $83, $80, $80, $82, $83, $88, $88, $8A, $8B, $88, $88, $8E, $8F,
    $80, $80, $82, $83, $80, $80, $82, $83, $88, $88, $8A, $8B, $88, $88, $8E, $8F,
    $A0, $A0, $A2, $A3, $A0, $A0, $A2, $A3, $A8, $A8, $AA, $AB, $A8, $A8, $AE, $AF,
    $A0, $A0, $A2, $A3, $A0, $A0, $A2, $A3, $B8, $B8, $BA, $BB, $B8, $B8, $BE, $BF,
    $80, $80, $82, $83, $80, $80, $82, $83, $88, $88, $8A, $8B, $88, $88, $8E, $8F,
    $80, $80, $82, $83, $80, $80, $82, $83, $88, $88, $8A, $8B, $88, $88, $8E, $8F,
    $E0, $E0, $E2, $E3, $E0, $E0, $E2, $E3, $E8, $E8, $EA, $EB, $E8, $E8, $EE, $EF,
    $E0, $E0, $E2, $E3, $E0, $E0, $E2, $E3, $F8, $F8, $FA, $FB, $F8, $F8, $FE, $FF
  );

{$ifdef CPF_GENERATE_LOOKUPS}
type
  TBytes16 = array[0..15] of Byte;
  PBytes16 = ^TBytes16;

var
  LookupsText: TStringList;

procedure LookupLine(const Line: string); overload;
begin
  LookupsText.Add('  ' + Line);
end;

procedure LookupLine; overload;
begin
  LookupsText.Add('');
end;

procedure LookupLineFmt(const FmtStr: string; const Args: array of const);
begin
  LookupLine(Format(FmtStr, Args));
end;

procedure LookupBytes16(const Buffer: TBytes16; const Finalize: Boolean);
var
  S: string;
begin
  S := Format('  $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x'+
              ', $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x',
    [Buffer[0], Buffer[1], Buffer[2], Buffer[3], Buffer[4], Buffer[5], Buffer[6], Buffer[7],
     Buffer[8], Buffer[9], Buffer[10], Buffer[11], Buffer[12], Buffer[13], Buffer[14], Buffer[15]]);

  if (not Finalize) then S := S + ',';
  LookupLine(S);
end;

function WayChildValue(Way: Integer): Byte;
var
  WayX, WayY: Integer;
begin
  // unpack 0..7 way to 0..2/0..2
  WayX := (Way + 1) mod 3;
  WayY := (Way + 1) div 3;

  // 0..2 way to -1..1
  if (WayX = 1) then WayX := -1
  else
  if (WayX = 2) then WayX := 1;

  if (WayY = 1) then WayY := -1
  else
  if (WayY = 2) then WayY := 1;

  // Start - Finish ==> Finish - Start
  WayX := -WayX;
  WayY := -WayY;

  // result
  if (WayX = 0) then
  begin
    if (WayY < 0) then
    begin
      Result := 1;
    end else
    // (WayY > 0) then
    begin
      Result := 5;
    end;
  end else
  if (WayY = 0) then
  begin
    if (WayX < 0) then
    begin
      Result := 7;
    end else
    // (WayX > 0) then
    begin
      Result := 3;
    end;
  end else
  if (WayX < 0) then
  begin
    if (WayY < 0) then
    begin
      Result := 0;
    end else
    // (WayY > 0) then
    begin
      Result := 6;
    end;
  end else
  // (WayX > 0) then
  begin
    if (WayY < 0) then
    begin
      Result := 2;
    end else
    // (WayY > 0) then
    begin
      Result := 4;
    end;
  end;
end;

procedure AddChildArray(WayChild: Integer; ClockWise, Hexagonal, Finalize: Boolean);
label
  filled;
const
  CHILD_VALUES: array[0..7] of Word = (
    ((1 shl 0) shl 8) or (0 shl 5),
    ((1 shl 1) shl 8) or (1 shl 5),
    ((1 shl 2) shl 8) or (2 shl 5),
    ((1 shl 3) shl 8) or (3 shl 5),
    ((1 shl 4) shl 8) or (4 shl 5),
    ((1 shl 5) shl 8) or (5 shl 5),
    ((1 shl 6) shl 8) or (6 shl 5),
    ((1 shl 7) shl 8) or (7 shl 5)
  );
  CLOCKWISE_SIGN: array[Boolean] of Integer = (-1, +1);
var
  ChildList: TChildList;
  Child: PWord;
  LineChild, Sign, i: Integer;
  MaySimpleFlag: Boolean;
  S: string;

  procedure AddChild(C: Integer; SimpleIncrement: Boolean = False);
  begin
    C := (C + 8) and 7;

    if (C <> LineChild) then
    begin
      Child^ := CHILD_VALUES[C];
      if (SimpleIncrement) then Child^ := Child^ or 4;
      Inc(Child);
    end;
  end;

  procedure AddChilds(const Childs: array of Integer);
  var
    i: Integer;
  begin
    for i := Low(Childs) to High(Childs) do
      AddChild(Childs[i]);
  end;

begin
  // default parameters
  Child := @ChildList[0];
  LineChild := -1;
  Sign := CLOCKWISE_SIGN[ClockWise];

  // difficult hexagonal up/down cases
  if (Hexagonal) and ((WayChild = 1) or (WayChild = 5)) then
  begin
    if (WayChild = 1) then
    begin
      // hexagonal up
      if (ClockWise) then
      begin
        AddChilds([2,1,0,3,7,4,5,6]);
      end else
      begin
        AddChilds([0,1,2,7,3,6,5,4]);
      end;
    end else
    begin
      // hexagonal down
      if (ClockWise) then
      begin
        AddChilds([6,5,4,7,3,0,1,2]);
      end else
      begin
        AddChilds([4,5,6,3,7,2,1,0]);
      end;
    end;

    goto filled;
  end;

  // hexagonal line childs
  if (Hexagonal) then
  case WayChild of
    2, 4: LineChild := 3;
    0, 6: LineChild := 7;
  end;

  // childs
  if (LineChild >= 0) then
  begin
    Child^ := CHILD_VALUES[LineChild];
    Inc(Child);
  end;
  MaySimpleFlag := (LineChild < 0) and (not Hexagonal) and (WayChild and 1 = 1);
  AddChild(WayChild, MaySimpleFlag);
  for i := 1 to 3 do
  begin
    AddChild(WayChild + i * Sign);
    AddChild(WayChild - i * Sign, MaySimpleFlag and (i = 2));
  end;
  // last child
  AddChild(WayChild + 4);

  // text
filled:
  S := Format(' ($%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x)',
    [ChildList[0], ChildList[1], ChildList[2], ChildList[3],
     ChildList[4], ChildList[5], ChildList[6], ChildList[7]]);

  if (not Finalize) then S := S + ',';
  LookupLine(S);
end;

procedure AddSimpleDiagonalWayBits(Result: PByte; Way: Byte;
  OddXY, Simple, MoveLeft, MoveDown, dXsmaller: Boolean);
const
  WAYCHILD_TO_WAY: array[0..7] of Byte = (7, 5, 6, 0, 3, 2, 4, 1);
var
  MoveRight, MoveUp: Boolean;
  WayChild: Byte;
  ClockWise: Boolean;
begin
  MoveRight := not MoveLeft;
  MoveUp := not MoveDown;
  WayChild := WayChildValue(Way);
  ClockWise := False{warn off};

  if (Simple) and (WayChild and 1 = 0) then
  begin
    case WayChild of
      0:
      if (not OddXY) then
      begin
        WayChild := 7;
        ClockWise := True;
      end else
      begin
        WayChild := 1;
        ClockWise := False;
      end;
      2:
      if (not OddXY) then
      begin
        WayChild := 3;
        ClockWise := False;
      end else
      begin
        WayChild := 1;
        ClockWise := True;
      end;
      4:
      if (not OddXY) then
      begin
        WayChild := 3;
        ClockWise := True;
      end else
      begin
        WayChild := 5;
        ClockWise := False;
      end;
      6:
      if (not OddXY) then
      begin
        WayChild := 7;
        ClockWise := False;
      end else
      begin
        WayChild := 5;
        ClockWise := True;
      end;
    end;
  end else
  case WayChild of
    1: ClockWise := not MoveRight;
    5: ClockWise := MoveRight;
    3: ClockWise := MoveUp;
    7: ClockWise := not MoveUp;

    0: ClockWise := dXsmaller;
    2: ClockWise := not dXsmaller;
    4: ClockWise := dXsmaller;
    6: ClockWise := not dXsmaller;
  end;

  Result^ := (WAYCHILD_TO_WAY[WayChild]{Way} shl 4) + (Byte(ClockWise) shl 7);
end;

procedure HexagonalWayBits(Result: PByte; Way: Byte;
  OddFinishY, OddDY, dXsmaller, MoveLeft, MoveDown: Boolean);
var
  MoveRight, MoveUp: Boolean;
  WayChild: Byte;
  ClockWise: Boolean;
begin
  MoveRight := not MoveLeft;
  MoveUp := not MoveDown;
  WayChild := WayChildValue(Way);

  case WayChild of
    0: ClockWise := dXsmaller;
    2: ClockWise := not dXsmaller;
    4: ClockWise := dXsmaller;
    6: ClockWise := not dXsmaller;

    3: ClockWise := MoveUp;
    7: ClockWise := not MoveUp;

    1:
    if (not OddDY) then
    begin
      ClockWise := not MoveRight;
    end else
    begin
      ClockWise := OddFinishY;
    end;

    5:
    if (not OddDY) then
    begin
      ClockWise := MoveRight;
    end else
    begin
      ClockWise := not OddFinishY;
    end;
  else
    ClockWise := False{warn off};
  end;

  Result^ := (Way shl 4) + (Byte(ClockWise) shl 7);
end;

procedure AddParentBits(Child: Integer; Finalize: Boolean);
var
  Parent: Integer;
  Buffer: array[0..7] of Cardinal;
  Simple, Hexagonal, OddY: Boolean;
  Item: PCardinal;
  S: string;

  procedure AddParentMask(ExcludedChilds: Byte);
  begin
    Item^ := (Cardinal(Byte(not ExcludedChilds)) shl 16) or Cardinal(Parent);
    Inc(Item);
  end;
begin
  Parent := (Child + 4) and 7;
  Item := @Buffer[0];

  for Simple := False to True do
  for Hexagonal := False to True do
  for OddY := False to True do
  begin
    if (Simple) then
    begin
      case Child of
        1: AddParentMask(_5);
        3: AddParentMask(_7);
        5: AddParentMask(_1);
        7: AddParentMask(_3);
      else
        AddParentMask($FF);
      end;
    end else
    if (not Hexagonal) then
    begin
      case Child of
        0: AddParentMask(_3 or _4 or _5);
        1: AddParentMask(_3 or _4 or _5 or _6 or _7);
        2: AddParentMask(_5 or _6 or _7);
        3: AddParentMask(_5 or _6 or _7 or _0 or _1);
        4: AddParentMask(_7 or _0 or _1);
        5: AddParentMask(_0 or _1 or _2 or _3 or _7);
        6: AddParentMask(_1 or _2 or _3);
        7: AddParentMask(_1 or _2 or _3 or _4 or _5);
      end;
    end else
    if (not OddY) then
    begin
      case Child of
        6: AddParentMask(_1 or _2 or _3);
        7: AddParentMask(_1 or _3 or _5);
        0: AddParentMask(_3 or _4 or _5);
        1: AddParentMask(_4 or _5 or _7);
        3: AddParentMask(_6 or _7 or _0);
        5: AddParentMask(_7 or _1 or _2);
      else
        AddParentMask($FF);
      end;
    end else
    begin
      case Child of
        7: AddParentMask(_2 or _3 or _4);
        1: AddParentMask(_3 or _5 or _6);
        2: AddParentMask(_5 or _6 or _7);
        3: AddParentMask(_1 or _5 or _7);
        4: AddParentMask(_0 or _1 or _7);
        5: AddParentMask(_0 or _1 or _3);
      else
        AddParentMask($FF);
      end;
    end;
  end;

  S := Format('  $%0.8x, $%0.8x, $%0.8x, $%0.8x, $%0.8x, $%0.8x, $%0.8x, $%0.8x',
    [Buffer[0], Buffer[1], Buffer[2], Buffer[3],
     Buffer[4], Buffer[5], Buffer[6], Buffer[7]]);

  if (not Finalize) then S := S + ',';
  LookupLine(S);
end;

procedure AddRoundMasks(Line: Integer; Finalize: Boolean);
var
  i: Integer;
  Buffer: TBytes16;
  Mask: Byte;
begin
  for i := 0 to 15 do
  begin
    Mask := Line * 16 + i;

    if (Mask and (1 shl 1) = 0) then Mask := Mask and NOT_TOP_MASK;
    if (Mask and (1 shl 3) = 0) then Mask := Mask and NOT_RIGHT_MASK;
    if (Mask and (1 shl 5) = 0) then Mask := Mask and NOT_BOTTOM_MASK;
    if (Mask and (1 shl 7) = 0) then Mask := Mask and NOT_LEFT_MASK;

    Buffer[i] := Mask;
  end;

  LookupBytes16(Buffer, Finalize);
end;

procedure GenerateLookups;
const
  MIN_WEIGHT_VALUE_LINE: Single = 0.1;
  MAX_WEIGHT_VALUE_LINE: Single = 100.0;
  DEFAULT_WEIGHT_VALUE_LINE: Single = 1;
var
  Way: Integer;
  Child: Integer;
  Line: Integer;

  ByteValue: PByte;
  BytesBuffer: array[0..15] of TBytes16;

  procedure LookupBytesBufferLines(Count: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      LookupBytes16(BytesBuffer[i], i = (Count - 1));
  end;

begin
  LookupsText := TStringList.Create;
  try
    LookupsText.Add('const');

    // weight consts
    FormatSettings.DecimalSeparator := '.';
    LookupLineFmt('MIN_WEIGHT_VALUE_LINE = Cardinal($%8x){%0.1f};', [PCardinal(@MIN_WEIGHT_VALUE_LINE)^, MIN_WEIGHT_VALUE_LINE]);
    LookupLineFmt('MAX_WEIGHT_VALUE_LINE = Cardinal($%8x){%0.1f};', [PCardinal(@MAX_WEIGHT_VALUE_LINE)^, MAX_WEIGHT_VALUE_LINE]);
    LookupLineFmt('DEFAULT_WEIGHT_VALUE_LINE = Cardinal($%8x){%0.1f};', [PCardinal(@DEFAULT_WEIGHT_VALUE_LINE)^, DEFAULT_WEIGHT_VALUE_LINE]);
    FormatSettings.DecimalSeparator := ',';
    LookupLineFmt('ERROR_WEIGHT_VALUE = ''Invalid weight value. 0,0..%0.1f - pathless, %0.1f..%0.1f - correct'';',
      [MIN_WEIGHT_VALUE_LINE, MIN_WEIGHT_VALUE_LINE, MAX_WEIGHT_VALUE_LINE]);

    // CHILD_ARRAYS
    LookupLine;
    LookupLine('CHILD_ARRAYS: array[0..31{way:3;clockwise:1;hexagonal:1}] of TChildList = (');
    for Way := 0 to 31 do
    begin
      AddChildArray(WayChildValue(Way and 7),
        {clockwise}Way and 8 <> 0,
        {hexagonal}Way >= 16,
        {finalize}Way = 31);
    end;
    LookupLine(');');

    // SIMPLE_DIAGONAL_WAY_BITS
    LookupLine;
    LookupLine('SIMPLE_DIAGONAL_WAY_BITS: array[0..127{oddxy:1;simple:1;moveleft:1;movedown:1;way:3}] of Byte = (');
    ByteValue := @BytesBuffer[0][0];
    for Way := 0 to 127 do
    begin
      AddSimpleDiagonalWayBits(ByteValue, (Way shr 4) and 7,
        Way and 1 <> 0, Way and 2 <> 0, Way and 4 <> 0, Way and 8 <> 0, False);
      Inc(ByteValue);
    end;
    LookupBytesBufferLines(8);
    LookupLine(');');

    // SIMPLE_DIAGONAL_DXSMALLER_WAY_BITS
    LookupLine;
    LookupLine('SIMPLE_DIAGONAL_DXSMALLER_WAY_BITS: array[0..127{oddxy:1;simple:1;moveleft:1;movedown:1;way:3}] of Byte = (');
    ByteValue := @BytesBuffer[0][0];
    for Way := 0 to 127 do
    begin
      AddSimpleDiagonalWayBits(ByteValue, (Way shr 4) and 7,
        Way and 1 <> 0, Way and 2 <> 0, Way and 4 <> 0, Way and 8 <> 0, True);
      Inc(ByteValue);
    end;
    LookupBytesBufferLines(8);
    LookupLine(');');

    // HEXAGONAL_WAY_BITS
    LookupLine;
    LookupLine('HEXAGONAL_WAY_BITS: array[0..255{oddyfinish:1;odddy:1;dxsmaller:1;moveleft:1;movedown:1;way:3}] of Byte = (');
    ByteValue := @BytesBuffer[0][0];
    for Way := 0 to 255 do
    begin
      HexagonalWayBits(ByteValue, (Way shr 5) and 7,
        Way and 1 <> 0, Way and 2 <> 0, Way and 4 <> 0, Way and 8 <> 0, Way and 16 <> 0);
      Inc(ByteValue);
    end;
    LookupBytesBufferLines(16);
    LookupLine(');');

    // PARENT_BITS
    LookupLine;
    LookupLine('PARENT_BITS: array[0..63{oddy:1;hexagonal:1;simple:1;child:3}] of NativeUInt = (');
    for Child := 0 to 7 do
    begin
      AddParentBits(Child, Child = 7);
    end;
    LookupLine(');');

    // ROUNDED_MASKS
    LookupLine;
    LookupLine('ROUNDED_MASKS: array[Byte] of Byte = (');
    for Line := 0 to 15 do
    begin
      AddRoundMasks(Line, Line = 15);
    end;
    LookupLine(');');

    LookupsText.SaveToFile('Lookup.txt');
  finally
    LookupsText.Free;
  end;
end;
{$endif}


{ TCPFWeightsInfo }

{class} function TCPFWeightsInfo.NewInstance(const Address: Pointer): PCPFWeightsInfo;
begin
  // allocate
  Result := CPFAlloc(SizeOf(TCPFWeightsInfo), Address);

  // fill
  Result.RefCount := 1;
  Result.UpdateId := 1;
  Result.PrepareId := 1;
  Result.Count := 0;
  Result.Scale := 1.0;

  // default values
  FillCardinal(@Result.Singles[1], Length(Result.Singles), DEFAULT_WEIGHT_VALUE_LINE);
end;

procedure TCPFWeightsInfo.Release(const Address: Pointer);
var
  Cnt: Cardinal;
begin
  if (@Self = nil) then Exit;

  Cnt := Self.RefCount;
  if (Cnt <> 1) then
  begin
    Dec(Cnt);
    Self.RefCount := Cnt;
  end else
  begin
    CPFFree(@Self, Address);
  end;
end;

procedure TCPFWeightsInfo.Prepare;
var
  Min: Cardinal{Single};
  Max: Cardinal{Single};
  PValue, PHighValue: PCardinal;
  Value: Cardinal;
  MinSingle, MaxSingle: Single;
begin
  if (UpdateId = PrepareId) then Exit;
  PrepareId := UpdateId;

  PValue := @Singles[1];
  PHighValue := @Singles[Count + 1];
  Min := DEFAULT_WEIGHT_VALUE_LINE;
  Max := DEFAULT_WEIGHT_VALUE_LINE;
  while (PValue <> PHighValue) do
  begin
    Value := PValue^;

    if (Value <> 0) then
    begin
      if (Value >= Max) then Max := Value
      else
      if (Value < Min) then Min := Value;
    end;

    Inc(PValue);
  end;

  PCardinal(@MinSingle)^ := Min;
  PCardinal(@MaxSingle)^ := Max;
  Minimum := Min;
  Scale := MinSingle / MaxSingle;
end;

{ TTileMapWeights }

{$ifdef CPFLIB}procedure{$else}constructor{$endif} TTileMapWeights.Create;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
    inherited Create;
  {$endif}

  FInfo := PCPFWeightsInfo(nil).NewInstance(FCallAddress)
end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TTileMapWeights.Destroy;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  FInfo.Release(FCallAddress);

  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;

procedure TTileMapWeights.RaiseBarrierTile;
begin
  CPFException('Invalid tile index 0, it means a barrier (TILE_BARRIER)');
end;

function TTileMapWeights.GetValue(const Tile: Byte): Single;
begin
  if (Tile = TILE_BARRIER) then
  begin
    {$ifNdef CPFLIB}
      FCallAddress := ReturnAddress;
    {$endif}
    RaiseBarrierTile;
    Result := 0;
  end else
  begin
    Result := PSingle(@FInfo.Singles[Tile])^;
  end;
end;

procedure TTileMapWeights.SetValue(const Tile: Byte;
  const Value: Single);
label
  fillcount;
var
  PValue: PCardinal;
  V: Cardinal;
  Index: NativeUInt;
  Info: PCPFWeightsInfo;
begin
  V := PCardinal(@Value)^;
  if (Tile = TILE_BARRIER) or (V > MAX_WEIGHT_VALUE_LINE) then
  begin
    {$ifNdef CPFLIB}
      FCallAddress := ReturnAddress;
    {$endif}
    if (Tile = TILE_BARRIER) then
    begin
      RaiseBarrierTile;
    end else
    begin
      CPFException(ERROR_WEIGHT_VALUE);
    end;
  end else
  begin
    Info := FInfo;
    V := V and (Integer(Byte(V < MIN_WEIGHT_VALUE_LINE)) - 1); // 0..0,1 --> 0
    Index := Tile;
    PValue := @Info.Singles[Index];

    if (PValue^ <> V) then
    begin
      PValue^ := V;
      Inc(Info.UpdateId);

      if (V = DEFAULT_WEIGHT_VALUE_LINE) then
      begin
        if (Index = Info.Count) then
        begin
          repeat
            Dec(PValue);
            Dec(Index);
            if (PValue = @Info.Count{Index = 0}) or
              (PValue^ <> DEFAULT_WEIGHT_VALUE_LINE) then Break;
          until (False);

          goto fillcount;
        end;
      end else
      if (Index > Info.Count) then
      begin
        fillcount:
        Info.Count := Index;
      end;
    end;
  end;
end;

procedure TTileMapWeights.Clear;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  if (FInfo.Count <> 0) then
  begin
    FillCardinal(@FInfo.Singles[1], FInfo.Count, DEFAULT_WEIGHT_VALUE_LINE);
    FInfo.Count := 0;
    Inc(FInfo.UpdateId);
  end;
end;


{ TTileMap }

{$ifdef CPFLIB}procedure{$else}constructor{$endif}
  TTileMap.Create(const AWidth, AHeight: Word; const AKind: TTileMapKind;
    const ASameDiagonalWeight: Boolean);
var
  i: NativeInt;
  Size: NativeUInt;
  Max, Min, PathLengthLimit: NativeUInt;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
    inherited Create;
  {$endif}

  {$ifdef CPFDBG}
    MAP_INFO := @FInfo;
  {$endif}

  // arguments test
  begin
    FCellCount := NativeUInt(AWidth) * NativeUInt(AHeight);

    if (AWidth <= 1) or (AHeight <= 1) then
      // ToDo: 1 line map
      CPFExceptionFmt('Incorrect map size: %dx%d', [AWidth, AHeight]);

    if (FCellCount > CELLCOUNT_LIMIT) then
      CPFExceptionFmt('Too large map size %dx%d, cell count limit is %d', [AWidth, AHeight, CELLCOUNT_LIMIT]);

    if (Ord(AKind) > Ord(High(TTileMapKind))) then
      CPFExceptionFmt('Incorrect map kind: %d, high value mkHexagonal is %d', [Ord(AKind), Ord(High(TTileMapKind))]);
  end;

  // fill params
  FWidth := AWidth;
  FHeight := AHeight;
  FKind := AKind;
  FInfo.MapWidth := AWidth;
  FSectorTest := False;
  FCaching := True;

  // offsets
  for i := 0 to 7 do
  begin
    FSectorOffsets[i] := POINT_OFFSETS[i].y * FInfo.MapWidth + POINT_OFFSETS[i].x;
    FInfo.CellOffsets[i] := SizeOf(TCPFCell) * FSectorOffsets[i];
  end;

  // path/weight limit params
  FSameDiagonalWeight := (not (AKind in [mkDiagonal, mkDiagonalEx])) or ASameDiagonalWeight;
  Max := AWidth;
  Min := AHeight;
  if (Max < Min) then
  begin
    Max := AHeight;
    Min := AWidth;
  end;
  PathLengthLimit := ((Min + 1) shr 1) * Max + (Min shr 1);
  FTileWeightScaleLine := SORTVALUE_LIMIT / PathLengthLimit;
  FTileWeightLimitLine := CPFRound(FTileWeightScaleLine) - 1;
  if (FSameDiagonalWeight) then
  begin
    FTileWeightScaleDiagonal := FTileWeightScaleLine;
    DEFAULT_DIAGONAL_WEIGHT_VALUE := DEFAULT_WEIGHT_VALUE_LINE;
    FTileWeightLimitDiagonal := FTileWeightLimitLine;
    FTileWeightMinimumLine := 1;
    FTileWeightMinimumDiagonal := 1;
  end else
  begin
    FTileWeightScaleDiagonal := SQRT2 * FTileWeightScaleLine;
    PSingle(@DEFAULT_DIAGONAL_WEIGHT_VALUE)^ := SQRT2;
    FTileWeightLimitDiagonal := CPFRound(FTileWeightLimitLine * SQRT2);
    FTileWeightMinimumLine := 2;
    FTileWeightMinimumDiagonal := 3;
  end;
  FActualInfo.Weights.Count := 255;

  // internal buffers
  FActualInfo.Starts.Buffer.Initialize(Self);
  FActualInfo.Excludes.Buffer.Initialize(Self);
  FActualInfo.FoundPath.Buffer.Initialize(Self);

  // failure hot node
  FNodes.HotPool.Last.Path := SORTVALUE_LIMIT;
  FNodes.HotPool.Last.SortValue := SORTVALUE_LIMIT;
  Cardinal(FNodes.HotPool.Last.Coordinates) := High(Cardinal);
  FNodes.HotPool.Last.NodeInfo := FLAG_KNOWN_PATH {+ not FLAG_ATTAINABLE};

  // allocate and fill cells
  // node storage initialization
  Size := FCellCount * SizeOf(TCPFCell);
  CPFGetMem(Pointer(FInfo.CellArray), Size);
  Clear;
end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TTileMap.Destroy;
var
  i: NativeInt;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  // tile Weights
  FActualInfo.Weights.Current.Release(FCallAddress);

  // internal buffers
  FActualInfo.Starts.Buffer.Free;
  FActualInfo.Excludes.Buffer.Free;
  FActualInfo.FoundPath.Buffer.Free;

  // node storage
  for i := NativeInt(FNodes.Storage.Count) - 1 downto 0 do
    CPFFreeMem(FNodes.Storage.Buffers[i]);

  // cells
  CPFFreeMem(Pointer(FInfo.CellArray));

  // sectors
  CPFFreeMem(Pointer(FActualInfo.Sectors));

  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;

procedure TTileMap.RaiseCoordinates(const X, Y: Integer; const Id: TCPFExceptionString);
const
  PREFIX = 'Invalid ';
  POSTFIX = ' point (%d, %d) on the %dx%d map';
{$ifNdef CPFLIB}
begin
  CPFExceptionFmt(PREFIX + Id + POSTFIX, [X, Y, Self.Width, Self.Height]);
end;
{$else}
var
  Buffer: array[0..1023] of WideChar;
  S: PWideChar;

  procedure IncludeString(Value: PWideChar);
  begin
    while (Value^ <> #0) do
    begin
      S^ := Value^;
      Inc(Value);
      Inc(S);
    end;
  end;
begin
  S := @Buffer[0];

  IncludeString(PREFIX);
  IncludeString(Id);
  IncludeString(POSTFIX);
  S^ := #0;

  CPFExceptionFmt(@Buffer[0], [X, Y, Self.Width, Self.Height]);
end;
{$endif}

{$ifdef CPFDBG}
function TTileMap.CellInformation(const X, Y: Word): string;
const
  STAR: array[Boolean] of string = ('', '*');
var
  Cell: PCPFCell;
  Node, N: PCPFNode;
  CellKind, CellInfo: string;
  WayBits: Integer;
  ChildList: PChildList;
  IsSimple: Boolean;
  Sector: Byte;
  Heuristics: Cardinal;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  if (X >= Self.Width) or (Y >= Self.Height) then
  begin
    Result := 'FAILURE COORDINATES';
    Exit;
  end;

  Cell := @Self.FInfo.CellArray[Self.FInfo.MapWidth * Y + X];
  if (Cell.NodePtr and NODEPTR_FLAG_ALLOCATED = 0) then
  begin
    CellKind := 'map cell';
    CellInfo := Format('(tile: %d, mask: %s)', [Cell.Tile, MaskToString(Cell.Mask)]);
    if (Cell.AllocatedFlags or Cell._ <> 0) then CellInfo := CellInfo + ' FAILURE flags';
  end else
  begin
    {$ifdef LARGEINT}
    NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
    {$endif}
    Node := PCPFNode(
              {$ifdef LARGEINT}NodeBuffers[Cell.NodePtr shr LARGE_NODEPTR_OFFSET] +{$endif}
              Cell.NodePtr and NODEPTR_CLEAN_MASK
              );

    if (Cell.NodePtr and NODEPTR_FLAG_HEURISTED = 0) then
    begin
      CellKind := 'allocated';
      CellInfo := Format('(tile: %d, mask: %s)', [Node.Tile, MaskToString(Node.Mask)]);
    end else
    begin
      if (Node.ParentMask = 0) then
      begin
        CellInfo := Format('locked (tile: %d, mask: %s', [Node.Tile, MaskToString(Node.Mask)]);
      end else
      begin
        CellInfo := Format('(tile: %d, mask: %s, parentmask: %s',
          [Node.Tile, MaskToString(Node.Mask), MaskToString(Node.ParentMask)]);
      end;

      if (Node.SortValue >= NATTANABLE_LENGTH_LIMIT) then
      begin
        CellKind := 'attainable cached';
        CellInfo := CellInfo + Format(', length: %d, distance: %0.2f, child: %d',
          [not Node.nAttainableLength, Node.AttainableDistance, (Node.NodeInfo shr 4) and 7]);
      end else
      begin
        WayBits := -1;

        if (Node.Path = SORTVALUE_LIMIT) then
        begin
          if (Node.SortValue <> SORTVALUE_LIMIT) or
            (Node.NodeInfo and FLAG_KNOWN_PATH = 0) or
            (Node.NodeInfo and FLAG_ATTAINABLE <> 0) then
          begin
            CellKind := 'FAILURE unattainable';
          end else
          begin
            CellKind := 'unattainable';
          end;
        end else
        if (Node.NodeInfo and FLAG_KNOWN_PATH <> 0) then
        begin
          if (Node.NodeInfo and FLAG_ATTAINABLE = 0) then
          begin
            CellKind := 'FAILURE attainable';
          end else
          begin
            CellKind := 'attainable';
            CellInfo := CellInfo + Format(', child: %d', [(Node.NodeInfo shr 4) and 7]);
          end;
        end else
        begin
          CellKind := 'heuristed';
          WayBits := ((Node.NodeInfo shr 4) and $f) + (Byte(Self.FKind = mkHexagonal) shl 4);
        end;

        // detect pool
        N := Node.Prev;
        while (N.Prev <> nil) do N := N.Prev;
        if (N = @FNodes.HotPool.First) then
        begin
          CellKind := CellKind + ' [hot]';
          CellInfo := CellInfo + Format(', parent: %d', [Node.ParentAndFlags and 7]);
        end else
        begin
          if (N = @FNodes.ExcludedPool.First) then CellKind := CellKind + ' [excluded]'
          else
          if (N = @FNodes.HeuristedPool.First) then CellKind := CellKind + ' [heuristed]'
          else
          if (N = @FNodes.UnattainablePool.First) then CellKind := CellKind + ' [unattainable]'
          else
          CellKind := CellKind + ' [UNKNOWN!!!]';
        end;

        // way (child list)
        if (WayBits <> -1) then
        begin
          IsSimple := (Self.Kind = mkSimple);
          ChildList := @CHILD_ARRAYS[WayBits];
          CellInfo := CellInfo + Format(', way: %d%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s',
            [(ChildList[0] shr 5) and 7, STAR[IsSimple and (ChildList[0] and 4 <> 0)],
             (ChildList[1] shr 5) and 7, STAR[IsSimple and (ChildList[1] and 4 <> 0)],
             (ChildList[2] shr 5) and 7, STAR[IsSimple and (ChildList[2] and 4 <> 0)],
             (ChildList[3] shr 5) and 7, STAR[IsSimple and (ChildList[3] and 4 <> 0)],
             (ChildList[4] shr 5) and 7, STAR[IsSimple and (ChildList[4] and 4 <> 0)],
             (ChildList[5] shr 5) and 7, STAR[IsSimple and (ChildList[5] and 4 <> 0)],
             (ChildList[6] shr 5) and 7, STAR[IsSimple and (ChildList[6] and 4 <> 0)],
             (ChildList[7] shr 5) and 7, STAR[IsSimple and (ChildList[7] and 4 <> 0)]
            ]);
        end;

        // path, sort value
        Heuristics := Node.SortValue - Node.Path;
        if (Node.SortValue = SORTVALUE_LIMIT) then
        begin
          CellInfo := CellInfo + Format(', SORTVALUE_LIMIT, heuristics: %0.2f [%d]',
            [Heuristics / FActualInfo.Weights.ScaleLine, Heuristics]);
        end else
        begin
          CellInfo := CellInfo + Format(', path: %0.2f [%d], sort value: %0.2f [%d], heuristics: %0.2f [%d]',
            [Node.Path / FActualInfo.Weights.ScaleLine, Node.Path,
             Node.SortValue / FActualInfo.Weights.ScaleLine, Node.SortValue,
             Heuristics / FActualInfo.Weights.ScaleLine, Heuristics]);
        end;
      end;

      CellInfo := CellInfo + ')';      
    end;

    if (Node.Coordinates.X <> X) or (Node.Coordinates.Y <> Y) then
      CellInfo := CellInfo + Format(' FIALURE COORDINATES [%d,%d]', [Node.Coordinates.X, Node.Coordinates.Y]);
  end;

  Result := Format('[%d,%d] %s %s', [X, Y, CellKind, CellInfo]);

  if (Self.SectorTest) then
  begin
    if (FActualInfo.SectorsChanged) then
      ActualizeSectors;

    Sector := PByte(NativeInt(FActualInfo.Sectors) + NativeInt(Self.Width) * Y + X)^;
    if (Sector <> SECTOR_PATHLESS) then
    begin
      Result := Result + Format(', sector: %d', [Sector]);
    end else
    begin
      Result := Result + ', sector: none';
    end;
  end;
end;

function TTileMap.NodeInformation(const Node: PCPFNode): string;
begin
  if (Node = nil) then
  begin
    Result := 'nil';
  end else
  begin
    Result := CellInformation(Node.Coordinates.X, Node.Coordinates.Y);
  end;
end;

function TTileMap.HotPoolInformation: string;
var
  Previous, Node: PCPFNode;
  WasUnlocked: Boolean;
begin
  Result := '';

  WasUnlocked := False;
  Previous := @FNodes.HotPool.First;
  Node := FNodes.HotPool.First.Next;
  repeat
    if (Result <> '') then
      Result := Result + #13#10;

    if (Node.ParentMask <> 0) and (not WasUnlocked) then
    begin
      WasUnlocked := True;
      Result := Result + #13#10'  --------  '#13#10#13#10;
    end;

    Result := Result + Format('[$%p] ', [Node]);

    if (Node.Prev <> Previous) then
      Result := Result + Format('FAILURE PREV $%p ', [Node.Prev]);

    if (Node.SortValue < Previous.SortValue) then
      Result := Result + 'FAILURE SORTVALUE ';

    if (Node = @FNodes.HotPool.Last) then
    begin
      Result := Result + ' HOT POOL LAST';
      Break;
    end else
    begin
      Result := Result + NodeInformation(Node);
    end;

    Previous := Node;
    Node := Node.Next;
  until (False);
end;

procedure TTileMap.SaveHotPoolToFile(const FileName: string);
var
  S: AnsiString;
  F: TFileStream;
begin
  S := AnsiString(HotPoolInformation);

  F := TFileStream.Create(FileName, fmCreate);
  try
    F.Write(Pointer(S)^, Length(S));
  finally
    F.Free;
  end;
end;

function TTileMap.CachedAttainablePoints: TPointDynArray;
var
  Cell: PCPFCell;
  NodeInfo: NativeUInt;
  Node: PCPFNode;
  Len, i: Integer;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  Result := nil;
  if (not FNodes.AttainableTree) then Exit;
  Len := 0;
  {$ifdef LARGEINT}
  NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  Cell := @FInfo.CellArray[0];
  for i := 0 to Self.FCellCount - 1 do
  begin
    NodeInfo := Cell.NodePtr;
    if (NodeInfo and NODEPTR_FLAG_HEURISTED <> 0) then
    begin
      {$ifdef LARGEINT}
        Node := Pointer(
          (NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET]) +
          (NodeInfo and NODEPTR_CLEAN_MASK) );
      {$else}
        Node := Pointer(NodeInfo and NODEPTR_CLEAN_MASK);
      {$endif}

      NodeInfo := Node.NodeInfo;
      if (NodeInfo and FLAG_KNOWN_PATH <> 0) then
      if (NodeInfo and FLAG_ATTAINABLE <> 0) then
      begin
        SetLength(Result, Len + 1);
        Result[Len].X := Node.Coordinates.X;
        Result[Len].Y := Node.Coordinates.Y;
        Inc(Len);
      end;
    end;

    Inc(Cell);
  end;
end;

function TTileMap.CachedUnattainablePoints: TPointDynArray;
var
  Cell: PCPFCell;
  NodeInfo: NativeUInt;
  Node: PCPFNode;
  Len, i: Integer;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  Result := nil;
  Len := 0;
  {$ifdef LARGEINT}
  NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  Cell := @FInfo.CellArray[0];
  for i := 0 to Self.FCellCount - 1 do
  begin
    NodeInfo := Cell.NodePtr;
    if (NodeInfo and NODEPTR_FLAG_HEURISTED <> 0) then
    begin
      {$ifdef LARGEINT}
        Node := Pointer(
          (NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET]) +
          (NodeInfo and NODEPTR_CLEAN_MASK) );
      {$else}
        Node := Pointer(NodeInfo and NODEPTR_CLEAN_MASK);
      {$endif}

      NodeInfo := Node.NodeInfo;
      if (NodeInfo and FLAG_KNOWN_PATH <> 0) then
      if (NodeInfo and FLAG_ATTAINABLE = 0) then
      begin
        SetLength(Result, Len + 1);
        Result[Len].X := Node.Coordinates.X;
        Result[Len].Y := Node.Coordinates.Y;
        Inc(Len);
      end;
    end;

    Inc(Cell);
  end;
end;
{$endif}


procedure TTileMap.UpdateCellMasks(const ChangedArea: TRect);
label
  clearbit, fillmask, nextcell;
var
  MaximumI, MaximumJ: Integer;
  X, Y, Left, Top, Right, Bottom: Integer;
  Rounded: Boolean;
  CellOffsets: PCPFOffsets;
  Cell: PCPFCell;
  FlagHexagonal: Integer;
  CellLineOffset: NativeUInt;

  i, j: Integer;
  CellInfo: NativeUInt;
  DefaultMask, Mask, Flags: Integer;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  MaximumI := Self.Width - 1;
  MaximumJ := Self.Height - 1;
  Rounded := (Self.Kind = mkDiagonalEx);
  CellOffsets := @Self.FInfo.CellOffsets;
  Cell := @Self.FInfo.CellArray[0];
  FlagHexagonal := Ord(Self.Kind = mkHexagonal);
  DefaultMask := DEFAULT_MASKS[Self.Kind];

  {$ifdef LARGEINT}
  NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  // around area
  X := ChangedArea.Right;
  Right := X + Ord(X <= MaximumI);
  X := ChangedArea.Bottom;
  Bottom := X + Ord(X <= MaximumJ);
  X := ChangedArea.Left;
  Left := X - Ord(X <> 0);
  X := ChangedArea.Top;
  Top := X;
  Dec(Top, Ord(X <> 0));

  // each cell loop
  X := MaximumI + 1{Width};
  Inc(Cell, Top * X{Width} + Left);
  CellLineOffset := (X{Width} - (Right - Left)) * SizeOf(TCPFCell);
  // for j := Top to Bottom - 1 do
  j := Top;
  repeat
    //for i := Left to Right - 1 do
    i := Left;
    repeat
      // tile barier test
      CellInfo := Cell.NodePtr;
      if (CellInfo and NODEPTR_FLAG_ALLOCATED = 0) then
      begin
        if {$ifdef LARGEINT}
             (CellInfo <= $ffffff)
           {$else}
             (CellInfo and Integer($ff000000){Tile} = 0{TILE_BARIER})
           {$endif} then goto nextcell;
      end else
      begin
        if (PCPFNode(
            {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
            CellInfo and NODEPTR_CLEAN_MASK
             ).Tile = 0) then goto nextcell;
      end;

      // default mask
      Mask := DefaultMask;
      if (FlagHexagonal and j <> 0) then
      begin
        Mask := Mask shl 8;
        if (i = MaximumI) then goto fillmask{Mask = 0};
      end;
      Mask := Mask and $ff00;

      // each child loop
      Flags := (1 shl 8){bit} + 0{child};
      repeat
        if (Mask and Flags <> 0) then
        begin
          CellInfo := Flags and 7;

          X := POINT_OFFSETS[CellInfo].x;
          Y := POINT_OFFSETS[CellInfo].y;
          Inc(Y, j);
          Inc(X, i);

          if (Cardinal(Y) > Cardinal(MaximumJ)) or
             (Cardinal(X) > Cardinal(MaximumI - (FlagHexagonal and Y))) then
            goto clearbit;

          // tile barier
          CellInfo := PCPFCell(NativeInt(Cell) + CellOffsets[{$ifdef CPUX86}Flags and 7{$else}CellInfo{$endif}]).NodePtr;
          if (CellInfo and NODEPTR_FLAG_ALLOCATED = 0) then
          begin
            if {$ifdef LARGEINT}
                 (CellInfo <= $ffffff)
               {$else}
                 (CellInfo and Integer($ff000000){Tile} = 0{TILE_BARIER})
               {$endif} then goto clearbit;
          end else
          begin
            if (PCPFNode(
                {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
                CellInfo and NODEPTR_CLEAN_MASK
                ).Tile = 0) then
            begin
              clearbit:
              Mask := (Mask xor Flags) and -8;
            end;
          end;
        end;

        X := Flags and -8;
        Inc(Flags);
        Inc(Flags, X);
      until (Mask shr 1 < X){Mask < X shl 1};

      // mask to byte
      Mask := Mask shr 8;

      // rounded
      if (Rounded) then
      begin
        Mask := ROUNDED_MASKS[Mask];
      end;

    fillmask:
      CellInfo := Cell.NodePtr;
      if (CellInfo and NODEPTR_FLAG_ALLOCATED <> 0) then
      begin
        PCPFNode(
            {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
            CellInfo and NODEPTR_CLEAN_MASK
          ).Mask := Mask;
      end else
      begin
        Cell.Mask := Mask;
      end;

    nextcell:
      {$ifdef CPUX86}
        X := i + 1;
        Inc(Cell);
        i := X;
        if (X = Right) then Break;
      {$else}
        Inc(i);
        Inc(Cell);
        if (i = Right) then Break;
      {$endif}
    until (False);

    // next line
    {$ifdef CPUX86}
      X := j + 1;
      Inc(NativeUInt(Cell), CellLineOffset);
      j := X;
      if (X = Bottom) then Break;
    {$else}
      Inc(j);
      Inc(NativeUInt(Cell), CellLineOffset);
      if (j = Bottom) then Break;
    {$endif}
  until (False);
end;

procedure TTileMapClearAroundMasks(const Self: TTileMap; Cell: PCPFCell);
const
  NOT_TOP_MASK = (CrystalPathFinding.NOT_TOP_MASK shl 8) or $ff;
  NOT_RIGHT_MASK = (CrystalPathFinding.NOT_RIGHT_MASK shl 8) or $ff;
  NOT_BOTTOM_MASK = (CrystalPathFinding.NOT_BOTTOM_MASK shl 8) or $ff;
  NOT_LEFT_MASK = (CrystalPathFinding.NOT_LEFT_MASK shl 8) or $ff;
var
  i: NativeInt;
  MaximumI, Size: NativeInt;
  NotRightMask: Integer;
begin
  NotRightMask := NOT_RIGHT_MASK;
  if (Self.Kind = mkHexagonal) then NotRightMask := NotRightMask and not ((_1 or _5) shl 8);

  MaximumI := Self.Width - 1;
  Cell.NodePtr := Cell.NodePtr and (NOT_LEFT_MASK and NOT_TOP_MASK);
  Inc(Cell);
  for i := 1 to MaximumI do
  begin
    Cell.NodePtr := Cell.NodePtr and NOT_TOP_MASK;
    Inc(Cell);
  end;
  Dec(Cell);
  Cell.NodePtr := Cell.NodePtr and (NotRightMask and NOT_TOP_MASK);
  Inc(Cell);

  // each line
  for i := 1 to NativeInt(Self.FHeight) - 2 do
  begin
    Cell.NodePtr := Cell.NodePtr and NOT_LEFT_MASK;
    Inc(Cell, MaximumI);
    Cell.NodePtr := Cell.NodePtr and NotRightMask;
    Inc(Cell);
  end;

  // bottom
  Cell.NodePtr := Cell.NodePtr and (NOT_LEFT_MASK and NOT_BOTTOM_MASK);
  Inc(Cell);
  for i := 1 to MaximumI do
  begin
    Cell.NodePtr := Cell.NodePtr and NOT_BOTTOM_MASK;
    Inc(Cell);
  end;
  Dec(Cell);  
  Cell.NodePtr := Cell.NodePtr and (NotRightMask and NOT_BOTTOM_MASK);

  // hexagonal odd right
  if (Self.Kind = mkHexagonal) and (MaximumI <> 0) then
  begin
    Dec(Cell, (NativeInt(Self.FHeight) - 2) * (MaximumI + 1));
    Size := (MaximumI + 1) * 2 * SizeOf(TCPFCell);

    for i := 0 to NativeInt(Self.Height shr 1) - 1 do
    begin
      Cell.NodePtr := Cell.NodePtr and NOT_RIGHT_MASK;
      Inc(NativeInt(Cell), Size);
    end;
  end; 
end;

procedure TTileMap.Clear;
const
  HEXAGONAL_CELL_EVEN = $01000000 or (_0 or _1 or _3 or _5 or _6 or _7) shl 8;
  HEXAGONAL_CELL_ODD = $01000000 or (_1 or _2 or _3 or _4 or _5 or _7) shl 8;
var
  i: NativeInt;
  Cell: PCPFCell;
  Size: NativeInt;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  // release nodes
  Self.FreeAllocatedNodes(False);
  Cardinal(Self.FInfo.FinishPoint) := High(Cardinal);
  Self.FActualInfo.TilesChanged := True;
  Self.FActualInfo.SectorsChanged := True;

  // cells and default masks
  if (Self.Kind = mkHexagonal) then
  begin
    Cell := @Self.FInfo.CellArray[0];
    Size := Self.Width * SizeOf(TCPFCell);

    for i := 0 to NativeInt(Self.Height shr 1) - 1 do
    begin
      FillCardinal(PCardinal(Cell), Size shr 2, HEXAGONAL_CELL_EVEN);
      Inc(NativeInt(Cell), Size);
      FillCardinal(PCardinal(Cell), Size shr 2, HEXAGONAL_CELL_ODD);
      Inc(NativeInt(Cell), Size);
    end;

    if (Self.Height and 1 <> 0) then
      FillCardinal(PCardinal(Cell), Size shr 2, HEXAGONAL_CELL_EVEN);
  end else
  begin
    FillCardinal(PCardinal(Self.FInfo.CellArray), Self.FCellCount,
     (Cardinal(DEFAULT_MASKS[Self.Kind]) and $ff00) or $01000000);
  end;

  // around cells
  TTileMapClearAroundMasks(Self, @Self.FInfo.CellArray[0]);

  // sectors
  if (Self.SectorTest) then
  begin
    Size := (SizeOf(Byte) * Self.FCellCount + 3) and -4;
    if (Self.FActualInfo.Sectors = nil) then Self.CPFGetMem(Pointer(Self.FActualInfo.Sectors), Size);
    FillCardinal(PCardinal(Self.FActualInfo.Sectors), Size shr 2, $02020202);
    Self.FActualInfo.SectorsChanged := False;
  end;
end;

function TTileMap.GetTile(const X, Y: Word): Byte;
var
  _X, _Y: Word;
  _Self: Pointer;
  CellInfo: NativeUInt;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  if (X >= Width) or (Y >= Height)  then
  begin
    _Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});
    _X := X;
    _Y := Y;

    {$ifNdef CPFLIB}
      TTileMapPtr(_Self).FCallAddress := ReturnAddress;
    {$endif}

    TTileMapPtr(_Self).RaiseCoordinates(_X, _Y, 'tile');
    Result := 0;
  end else
  begin
    CellInfo := Self.FInfo.CellArray[Width * Y + X].NodePtr;
    if (CellInfo and NODEPTR_FLAG_ALLOCATED <> 0) then
    begin
      {$ifdef LARGEINT}
      NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
      {$endif}
      Result := PCPFNode(
                {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
                CellInfo and NODEPTR_CLEAN_MASK
                 ).Tile;
    end else
    begin
      Result := CellInfo shr 24;
    end;
  end;
end;

procedure TTileMap.SetTile(const X, Y: Word; Value: Byte);
var
  _X, _Y: Word;
  _Self: Pointer;
  CellInfo, ValueInfo: NativeUInt;
  ChangedArea: TRect;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  CellInfo := Width;
  if (X >= Word(CellInfo){Width}) or (Y >= Height)  then
  begin
    _Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});
    _X := X;
    _Y := Y;

    {$ifNdef CPFLIB}
      TTileMapPtr(_Self).FCallAddress := ReturnAddress;
    {$endif}

    TTileMapPtr(_Self).RaiseCoordinates(_X, _Y, 'tile');
  end else
  begin
    ChangedArea.Left := X;
    ChangedArea.Top := Y;
    with Self.FInfo.CellArray[CellInfo{Width} * Y + X] do
    begin
      CellInfo := {Cell.}NodePtr;
      if (CellInfo and NODEPTR_FLAG_ALLOCATED <> 0) then
      begin
        {$ifdef LARGEINT}
        NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
        {$endif}
        with PCPFNode(
                  {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
                  CellInfo and NODEPTR_CLEAN_MASK
                   )^ do
        begin
          CellInfo := {Node.}NodeInfo shr 24;
          ValueInfo := Value;
          if (CellInfo = ValueInfo) then Exit;
          {Node.}Tile := ValueInfo;
        end;
      end else
      begin
        CellInfo := CellInfo shr 24;
        ValueInfo := Value;
        if (CellInfo = ValueInfo) then Exit;
        {Cell.}Tile := ValueInfo;
      end;
    end;

    Dec(CellInfo);
    Dec(ValueInfo);
    FActualInfo.TilesChanged := True;
    CellInfo := CellInfo or ValueInfo;
    if (NativeInt(CellInfo) = -1) then
    begin
      ChangedArea.Right := ChangedArea.Left + 1;
      ChangedArea.Bottom := ChangedArea.Top + 1;
      FActualInfo.SectorsChanged := True;
      UpdateCellMasks(ChangedArea);
    end;
  end;
end;

procedure TTileMap.Update(const ATiles: PByte; const X, Y, AWidth,
  AHeight: Word; const Pitch: NativeInt);
var
  SelfWidth, SelfHeight: Integer;
  Cell, TopLineCell, FinalCell: PCPFCell;
  CellWidthSize, CellLineOffset, TileLineOffset: NativeInt;
  PTile: PByte;
  CellInfo, ValueInfo: NativeUInt;
  FlagsChanged: NativeUInt;
  ChangedArea: TRect;

  Store: record
    Self: Pointer;
    ATiles: PByte;
  end;
  {$ifdef CPUX86}
  _Self: Pointer;
  {$endif}

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  Store.Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});
  {$ifdef CPUX86}
  Store.ATiles := ATiles;
  {$endif}

  with ChangedArea do
  begin
    Left := X;
    Right := Left + AWidth;
    Top := Y;
    Bottom := Top + AHeight;

    {$ifNdef CPFLIB}
      {$ifdef CPUX86}TTileMapPtr(Store.Self).{$endif}FCallAddress := ReturnAddress;
    {$endif}

    {$ifdef CPUX86}
      _Self := Store.Self;
      SelfWidth := TTileMapPtr(_Self).Width;
      SelfHeight := TTileMapPtr(_Self).Height;
    {$else}
      SelfWidth := Self.Width;
      SelfHeight := Self.Height;
    {$endif}

    if (Right > SelfWidth) or (Bottom > SelfHeight) or
      (Left = Right) or (Top = Bottom) then
    begin
      {$ifdef CPUX86}TTileMapPtr(_Self).{$endif}CPFExceptionFmt('Invalid point range (%d..%d, %d..%d) on the %dx%d map',
        [Left, Right - 1, Top, Bottom - 1, SelfWidth, SelfHeight]);
    end;
  end;

  {$ifdef LARGEINT}
  NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  Cell := @{$ifdef CPUX86}TTileMapPtr(Store.Self).{$endif}FInfo.CellArray[ChangedArea.Top * SelfWidth + ChangedArea.Left];
  CellWidthSize := AWidth * SizeOf(TCPFCell);
  CellLineOffset := (SelfWidth - AWidth) * SizeOf(TCPFCell);
  FinalCell := Pointer(NativeInt(Cell) + (AHeight * SelfWidth) * SizeOf(TCPFCell));
  if (Pitch = 0) then TileLineOffset := 0
  else TileLineOffset := (Pitch - AWidth);

  FlagsChanged := 0;
  PTile := {$ifdef CPUX86}Store.{$endif}ATiles;
  // for j := 1 to AHeight do
  repeat
    //for i := 1 to AWidth do
    TopLineCell := Pointer(NativeInt(Cell) + CellWidthSize);
    repeat
      CellInfo := Cell.NodePtr;
      if (CellInfo and NODEPTR_FLAG_ALLOCATED <> 0) then
      begin
        {$if (not Defined(CPUX86)) or Defined(FPC)}
        with PCPFNode(
                  {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
                  CellInfo and NODEPTR_CLEAN_MASK
                   )^ do
        begin
          CellInfo := NodeInfo shr 24;
          ValueInfo := PTile^;
          Tile := ValueInfo;
        end;
        {$else}
          CellInfo := CellInfo and NODEPTR_CLEAN_MASK;
          ValueInfo := NativeUInt(PTile^) or (PCPFNode(CellInfo).NodeInfo and $ff000000);
          PCPFNode(CellInfo).Tile := ValueInfo;
          CellInfo := ValueInfo shr 24;
          ValueInfo := Byte(ValueInfo);
        {$ifend}
      end else
      begin
        CellInfo := CellInfo shr 24;
        ValueInfo := PTile^;
        Cell.Tile := ValueInfo;
      end;

      Dec(CellInfo);
      Dec(ValueInfo);
      FlagsChanged := FlagsChanged or ((CellInfo or ValueInfo) shl 1) or
        NativeUInt(CellInfo <> ValueInfo);

      Inc(Cell);
      Inc(PTile);
    until (Cell = TopLineCell);

    Inc(NativeInt(Cell), CellLineOffset);
    Inc(PTile, TileLineOffset);
  until (Cell = FinalCell);

  // nothing changed case
  CellInfo := FlagsChanged;
  if (CellInfo and 1 = 0) then Exit;

  // update flags and masks
  with TTileMapPtr(Store.Self){$ifdef CPFLIB}^{$endif} do
  begin
    FActualInfo.TilesChanged := True;

    if (NativeInt(CellInfo or 1) = -1) then
    begin
      FActualInfo.SectorsChanged := True;
      UpdateCellMasks(ChangedArea);
    end;
  end;
end;

procedure TTileMap.GrowNodeAllocator(var Buffer: TCPFInfo);
var
  AllocatorCount: NativeUInt;
  Count: NativeUInt;
  LastItem: Boolean;
  Node: PCPFNode;
begin
  // check data
  AllocatorCount := Buffer.NodeAllocator.Count;
  if (@Buffer <> @FInfo) then
  if (Buffer.NodeAllocator.NewNode <> FInfo.NodeAllocator.NewNodeLimit) or
     (AllocatorCount <> FInfo.NodeAllocator.Count) then
    CPFException('Incorrect node allocator data');

  if (AllocatorCount >= High(FNodes.Storage.Buffers)) then
    RaiseOutOfMemory(FCallAddress);

  // nodes count in storage item
  Count := NODESTORAGE_INFO[AllocatorCount].Previous + NODESTORAGE_INFO[AllocatorCount].Count;
  LastItem := (Count >= FCellCount);
  if (LastItem) then Count := FCellCount;
  Dec(Count, NODESTORAGE_INFO[AllocatorCount].Previous);

  // allocate if needed
  if (AllocatorCount = FNodes.Storage.Count) then
  begin
    CPFGetMem(FNodes.Storage.Buffers[AllocatorCount] , Count * SizeOf(TCPFNode));
    Inc(FNodes.Storage.Count);
  end;
  FInfo.NodeAllocator.Buffers[AllocatorCount] := FNodes.Storage.Buffers[AllocatorCount];

  // params
  Node := FInfo.NodeAllocator.Buffers[AllocatorCount];
  Inc(FInfo.NodeAllocator.Count);
  FInfo.NodeAllocator.NewNode := Node;
  {$ifdef LARGEINT}
    FInfo.NodeAllocator.LargeModifier :=
      (NativeInt(AllocatorCount) shl LARGE_NODEPTR_OFFSET) - NativeInt(Node);
  {$endif}
  Inc(Node, NativeInt(Count) - 1 + Ord(LastItem));
  FInfo.NodeAllocator.NewNodeLimit := Node;

  //  copy to buffer
  if (@Buffer <> @FInfo) then
  begin
    Buffer.NodeAllocator.NewNode := FInfo.NodeAllocator.NewNode;
    Buffer.NodeAllocator.NewNodeLimit := FInfo.NodeAllocator.NewNodeLimit;
    Buffer.NodeAllocator.Count := FInfo.NodeAllocator.Count;
    {$ifdef LARGEINT}
      Buffer.NodeAllocator.LargeModifier := FInfo.NodeAllocator.LargeModifier;
    {$endif}
    Buffer.NodeAllocator.Buffers[AllocatorCount] := FInfo.NodeAllocator.Buffers[AllocatorCount];
  end;
end;

function TTileMap.AllocateHeuristedNode(X, Y: NativeInt): PCPFNode;
const
  TEMP_WAY_OFFSET = 5;
  MOVELEFT_OFFSET = 3;
  MOVELEFT_MASK = Integer(1 shl MOVELEFT_OFFSET);
  MOVEDOWN_OFFSET = 4;
  MOVEDOWN_MASK = Integer(1 shl MOVEDOWN_OFFSET);
var
  Cell: PCPFCell;
  Node, Right: PCPFNode;
  NodeInfo: NativeUInt;
  Mask: NativeInt;
  SortValue: Cardinal;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  Cell := @FInfo.CellArray[FInfo.MapWidth * Y + X];

  {$ifdef LARGEINT}
  NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  NodeInfo := Cell.NodePtr;
  if (NodeInfo and NODEPTR_FLAG_HEURISTED = 0) then
  begin
    if (NodeInfo and NODEPTR_FLAG_ALLOCATED = 0) then
    begin
      // allocate new node
      Node := FInfo.NodeAllocator.NewNode;
      Node.NodeInfo := NodeInfo;
      {$ifdef LARGEINT}
        Cell.NodePtr := NativeUInt(NativeInt(Node) + FInfo.NodeAllocator.LargeModifier +
          (NODEPTR_FLAG_HEURISTED + NODEPTR_FLAG_ALLOCATED));
      {$else}
        Cell.NodePtr := NativeUInt(Node) + (NODEPTR_FLAG_HEURISTED + NODEPTR_FLAG_ALLOCATED);
      {$endif}
      Cardinal(Node.Coordinates) := Y + (X shl 16);

      // set next allocable node
      if (Node <> FInfo.NodeAllocator.NewNodeLimit) then
      begin
        Inc(Node);
        FInfo.NodeAllocator.NewNode := Node;
        Dec(Node);
      end else
      begin
        GrowNodeAllocator(FInfo);
      end;
    end else
    begin
      // take allocated node, mark as heuristed
      {$ifdef LARGEINT}
        Node := Pointer(
          (NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET]) +
          (NodeInfo and NODEPTR_CLEAN_MASK) );
        Cell.NodePtr := NodeInfo + NODEPTR_FLAG_HEURISTED;
      {$else}
        Node := Pointer(NodeInfo and NODEPTR_CLEAN_MASK);
        Cell.NodePtr := Cardinal(Node) + (NODEPTR_FLAG_HEURISTED + NODEPTR_FLAG_ALLOCATED);
      {$endif}
    end;

    // heuristics data
    // (dX, dY) = Node.Coordinates - FInfo.FinishPoint;
    Mask := Cardinal(FInfo.FinishPoint);
    Y := Y - Word(Mask);
    X := X - (Mask shr 16);

    // Way
    Mask := 2*Byte(Y > 0) + (Y shr HIGH_NATIVE_BIT);
    Mask := Mask * 3;
    Mask := Mask + 2*Byte(X > 0);
    Mask := Mask - 1 + (X shr HIGH_NATIVE_BIT) + {point = finish fix}Ord(X or Y = 0);
    Node.NodeInfo := (Node.NodeInfo and FLAGS_CLEAN_MASK) or $00ff0000;
    NodeInfo := Mask shl TEMP_WAY_OFFSET;

    // flags: hexagonal, simple
    Mask := NativeInt(Self.FKind);
    Inc(NodeInfo, (Mask - 1) and 4);
    Inc(NodeInfo, ((Mask + 1) and 4) shr 1);

    // Y := Abs(dY)
    Mask := -(Y shr HIGH_NATIVE_BIT);
    Y := Y xor Mask;
    Dec(Y, Mask);
    NodeInfo := NodeInfo or NativeUInt(Mask and MOVEDOWN_MASK);

    // X := Abs(dX)
    Mask := -(X shr HIGH_NATIVE_BIT);
    X := X xor Mask;
    Dec(X, Mask);
    NodeInfo := NodeInfo or NativeUInt((Mask and MOVELEFT_MASK) xor MOVELEFT_MASK);

    // calculate
    Node.SortValue := SORTVALUE_LIMIT;
    if (NodeInfo and 2 = 0) then
    begin
      // flag: oddy
      NodeInfo := NodeInfo shr 1;
      Mask := (X xor Y) and 1;
      Mask := Mask + (NativeInt(NodeInfo) and 127);

      if (X >= Y) then
      begin
        Node.NodeInfo := Node.NodeInfo or SIMPLE_DIAGONAL_WAY_BITS[Mask];
        Node.Path := SORTVALUE_LIMIT -
           Cardinal(Y * FInfo.HeuristicsDiagonal + (X - Y) * FInfo.HeuristicsLine);
      end else
      begin
        Node.NodeInfo := Node.NodeInfo or SIMPLE_DIAGONAL_DXSMALLER_WAY_BITS[Mask];
        Node.Path := SORTVALUE_LIMIT -
           Cardinal(X * FInfo.HeuristicsDiagonal + (Y - X) * FInfo.HeuristicsLine);
      end;
    end else
    begin
      // flag: oddfinishy
      NodeInfo := NodeInfo + (NativeUInt(Cardinal(FInfo.FinishPoint)) and 1);

      // move left correction
      if (X = 0) then
      begin
        NodeInfo := NodeInfo and (not MOVELEFT_MASK);
        NodeInfo := NodeInfo or NativeUInt(((not NodeInfo) and Y and 1) shl MOVELEFT_OFFSET);
      end;

      // include bits
      NodeInfo := NodeInfo + NativeUInt((Y and 1) shl 1);
      NodeInfo := NodeInfo + NativeUInt(((X - Y) shr (HIGH_NATIVE_BIT - 2)) and 4);

      // total way bits/heuristics
      Mask := Mask xor NativeInt(NodeInfo);
      Node.NodeInfo := Node.NodeInfo or HEXAGONAL_WAY_BITS[NodeInfo - 2{node info flag hexagonal}];
      X := X - (Mask and Y and 1) - (Y shr 1);
      Node.Path := SORTVALUE_LIMIT -
         Cardinal(FInfo.HeuristicsLine * (Y + (X and ((X shr HIGH_NATIVE_BIT) - 1))));
    end;
  end else
  begin
    {$ifdef LARGEINT}
      Node := Pointer(
        (NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET]) +
        (NodeInfo and NODEPTR_CLEAN_MASK) );
    {$else}
      Node := Pointer(NodeInfo and NODEPTR_CLEAN_MASK);
    {$endif}

    // already in heuristed pool case
    SortValue := Node.SortValue;
    if (SortValue < NATTANABLE_LENGTH_LIMIT) then
    begin
      Result := Node;
      Exit;
    end;

    // clear sort value
    Node.Path := SORTVALUE_LIMIT - (SortValue - Node.Path);
    Node.SortValue := SORTVALUE_LIMIT;
  end;

  // add to heuristed pool
  Right := FNodes.HeuristedPool.First.Next;
  FNodes.HeuristedPool.First.Next := Node;
  Node.Prev := @FNodes.HeuristedPool.First;
  Node.Next := Right;
  Right.Prev := Node;

  // result
  Result := Node;
end;

function TTileMap.ActualizeWeights(Weights: PCPFWeightsInfo; Compare: Boolean): Boolean;
label
  return_false;
var
  LastCount, Count: Cardinal;
  V: Cardinal;
  i: NativeUInt;
  PWeight: PSingle;

  DefaultWeight: record
    Line: Cardinal;
    Diagonal: Cardinal;
  end;
begin
  LastCount := FActualInfo.Weights.Count;

  if (Weights = nil) then
  begin
    if (LastCount = 0) then
    begin
      Result := True;
      Exit;
    end;

    // release
    if (FActualInfo.Weights.Current <> nil) then
    begin
      FActualInfo.Weights.Current.Release(FCallAddress);
      FActualInfo.Weights.Current := nil;
    end;

    Count := 0;

    // consts
    FActualInfo.Weights.ScaleLine := {1*} FTileWeightScaleLine;
    FActualInfo.Weights.ScaleDiagonal := {1*} FTileWeightScaleDiagonal;
    DefaultWeight.Line := FTileWeightLimitLine;
    DefaultWeight.Diagonal := FTileWeightLimitDiagonal;
  end else
  if (FActualInfo.Weights.Current = Weights) and
    (FActualInfo.Weights.UpdateId = Weights.UpdateId) then
  begin
    Result := True;
    Exit;
  end else
  begin
    // prepair scale
    if (Weights.PrepareId <> Weights.UpdateId) then
      Weights.Prepare;

    // release/addref
    if (FActualInfo.Weights.Current <> Weights) then
    begin
      if (FActualInfo.Weights.Current <> nil) then
        FActualInfo.Weights.Current.Release(FCallAddress);
    end;
    FActualInfo.Weights.Current := Weights;
    Inc(Weights.RefCount);
    FActualInfo.Weights.UpdateId := Weights.UpdateId;

    // compare
    Count := Weights.Count;
    if (Compare) and (LastCount = Count) and
      (CompareMem(@FActualInfo.Weights.SinglesLine, @Weights.Singles, SizeOf(Single) * Count)) then
    begin
      Result := True;
      Exit;
    end;

    // consts
    FActualInfo.Weights.ScaleLine := Weights.Scale * FTileWeightScaleLine;
    FActualInfo.Weights.ScaleDiagonal := Weights.Scale * FTileWeightScaleDiagonal;
    DefaultWeight.Line := CPFRound({1*} FActualInfo.Weights.ScaleLine);
    DefaultWeight.Diagonal := CPFRound({1*} FActualInfo.Weights.ScaleDiagonal);
    if (DefaultWeight.Line > FTileWeightLimitLine) then DefaultWeight.Line := FTileWeightLimitLine
    else
    if (DefaultWeight.Line < FTileWeightMinimumLine) then DefaultWeight.Line := FTileWeightMinimumLine;
    if (DefaultWeight.Diagonal < FTileWeightMinimumDiagonal) then DefaultWeight.Diagonal := FTileWeightMinimumDiagonal;
  end;
  FActualInfo.Weights.Count := Count;

  // fill default values
  if (Count < LastCount) then
  begin
    V := LastCount - Count;

    FillCardinal(Pointer(@FActualInfo.Weights.SinglesLine[Count + 1]), V, DEFAULT_WEIGHT_VALUE_LINE);
    FillCardinal(Pointer(@FActualInfo.Weights.SinglesDiagonal[Count + 1]), V, {instance variable}DEFAULT_DIAGONAL_WEIGHT_VALUE);

    FillCardinal(@FActualInfo.Weights.CardinalsLine[Count + 1], V, DefaultWeight.Line);
    FillCardinal(@FActualInfo.Weights.CardinalsDiagonal[Count + 1], V, DefaultWeight.Diagonal);
  end;

  // copy and calculate weights
  if (Count <> 0) then
  begin
    Move(Weights.Singles[1], FActualInfo.Weights.SinglesLine[1], SizeOf(Single) * Count);

    for i := Count downto 1 do
    begin
      PWeight := @FActualInfo.Weights.SinglesLine[i];
      if (PCardinal(PWeight)^ = 0) then
      begin
        FActualInfo.Weights.CardinalsLine[i] := PATHLESS_TILE_WEIGHT;
        FActualInfo.Weights.CardinalsDiagonal[i] := PATHLESS_TILE_WEIGHT;
        Continue;
      end;

      V := CPFRound(PWeight^ * FActualInfo.Weights.ScaleLine);
      if (V > FTileWeightLimitLine) then V := FTileWeightLimitLine
      else
      if (V < FTileWeightMinimumLine) then V := FTileWeightMinimumLine;
      FActualInfo.Weights.CardinalsLine[i] := V;

      if (FSameDiagonalWeight) then
      begin
        FActualInfo.Weights.SinglesDiagonal[i] := PWeight^;
        FActualInfo.Weights.CardinalsDiagonal[i] := V;
      end else
      begin
        FActualInfo.Weights.SinglesDiagonal[i] := SQRT2 * PWeight^;

        V := CPFRound(PWeight^ * FActualInfo.Weights.ScaleDiagonal);
        if (V < FTileWeightMinimumDiagonal) then V := FTileWeightMinimumDiagonal;
        FActualInfo.Weights.CardinalsDiagonal[i] := V;
      end;
    end;

    // heuristics
    V := CPFRound(PSingle(@Weights.Minimum)^ * FActualInfo.Weights.ScaleLine);
    if (V > FTileWeightLimitLine) then FInfo.HeuristicsLine := FTileWeightLimitLine
    else
    if (V < FTileWeightMinimumLine) then FInfo.HeuristicsLine := FTileWeightMinimumLine
    else
    FInfo.HeuristicsLine := V;

    V := CPFRound(PSingle(@Weights.Minimum)^ * FActualInfo.Weights.ScaleDiagonal);
    if (V < FTileWeightMinimumDiagonal) then FInfo.HeuristicsDiagonal := FTileWeightMinimumDiagonal
    else
    FInfo.HeuristicsDiagonal := V;
  end else
  begin
    // default heuristics
    FInfo.HeuristicsLine := DefaultWeight.Line;
    FInfo.HeuristicsDiagonal := DefaultWeight.Diagonal;
  end;

  // simple map heuristics correction
  if (Self.Kind = mkSimple) then
    FInfo.HeuristicsDiagonal := FInfo.HeuristicsLine * 2;

return_false:
  Result := False;
end;

function TTileMap.ActualizeStarts(const Params: TTileMapParams{; Compare: Boolean}): Boolean;
label
  done;
var
  CompareBits: Integer;
  Point: PPoint;
  Count: NativeUInt;
  XY: Integer;
  X, Y, Mask: NativeInt;
  Start: PCPFStart;

  {$ifdef CPUX86}
  Store: record
    FinishCoordinates: Integer;
    HighPoint: PPoint;
  end;
  {$else}
  FinishCoordinates: NativeInt;
  HighPoint: PPoint;
  {$endif}
begin
  {$ifdef CPUX86}Store.{$endif}FinishCoordinates := (Params.Finish.Y shl 16) + Params.Finish.X;
  Point := Params.Starts;
  Count := Params.StartsCount;

  CompareBits := Byte(FActualInfo.Starts.Count <> Count){0 if the same};
  FActualInfo.Starts.Count := Count;
  if (Count = 0) then goto done;

  {$ifdef CPUX86}Store.{$endif}HighPoint := @PPointList(Point)[Count];
  Count := SizeOf(TCPFStart) * Count;
  if (FActualInfo.Starts.Buffer.FAllocatedSize < Count) then FActualInfo.Starts.Buffer.Realloc(Count);
  Start := FActualInfo.Starts.Buffer.FMemory;

  repeat
    XY := (Point.Y shl 16) + Point.X;
    CompareBits := CompareBits or (Start.XY - XY);
    Start.XY := XY;

    X := XY;
    Y := Word(X);
    X := X shr 16;
    {$ifdef CPUX86}
    Mask := Store.FinishCoordinates;
    Y := Y - Word(Mask);
    X := X - (Mask shr 16);
    {$else}
    Y := Y - Word(FinishCoordinates);
    X := X - (FinishCoordinates shr 16);
    {$endif}

    // Y := Abs(dY)
    Mask := -(Y shr HIGH_NATIVE_BIT);
    Y := Y xor Mask;
    Dec(Y, Mask);

    // X := Abs(dX)
    Mask := -(X shr HIGH_NATIVE_BIT);
    X := X xor Mask;
    Dec(X, Mask);

    if (X or Y >= (1 shl 15)) then
    begin
      Start.XYDistance := High(Integer);
    end else
    begin
      // Start.XYDistance := X*X + Y*Y;
      Y := Y * Y;
      X := X * X;
      Inc(Y, X);
      Start.XYDistance := Y;
    end;

    Inc(Point);
    Inc(Start);
  until (Point = {$ifdef CPUX86}Store.{$endif}HighPoint);

done:
  Result := (CompareBits = 0);
end;

function TTileMap.ActualizeExcludes(Points: PPoint; Count: NativeUInt; Compare: Boolean): Boolean;
var
  Size: NativeUInt;
begin
  Size := SizeOf(TPoint) * Count;

  if (Compare) and (Count = FActualInfo.Excludes.Count) and
    CompareMem(Points, FActualInfo.Excludes.Buffer.Memory, Size) then
  begin
    Result := True;
    Exit;
  end;

  FActualInfo.Excludes.Count := Count;
  Move(Points^, FActualInfo.Excludes.Buffer.Alloc(Size)^, Size);

  Result := False;
end;

type
  PFloodLineRec = ^TFloodLineRec;
  TFloodLineRec = record
    Next: PFloodLineRec;
    FirstSector: PByte;
    FirstCell: PCPFCell;
  case Boolean of
    True: (LastCell: PCPFCell);
    False:(SectorValues: NativeUInt);
  end;

  TFloodLineStorage = record
    Offsets: TCPFOffsets;
    QueueLast: PFloodLineRec;
    Pool: PFloodLineRec;
    SectorValues: NativeUInt;
  end;


procedure FillFloodLineRec(var Result: TFloodLineRec;
  Cell: PCPFCell; Sector: PByte{$ifdef LARGEINT}; NodeBuffers: PCPFNodeBuffers{$endif});
label
  _2, done;
var
  Mask: NativeUInt;
  Count: NativeUInt;
  SectorValues: NativeUInt;

  Store: record
    Cell: PCPFCell;
    Sector: PByte;
    SectorValues: NativeUInt;
  end;
begin
  Store.Cell := Cell;
  Store.Sector := Sector;
  Store.SectorValues := Result.SectorValues;

  // move left
  Inc(Cell);
  Inc(Sector);
  repeat
    Dec(Cell);
    Dec(Sector);
    Mask := Cell.NodePtr;
    if (Mask and NODEPTR_FLAG_ALLOCATED = 0) then
    begin
      if (Mask and (_7 shl 8) = 0) then Break;
    end else
    begin
      if (PCPFNode
        (
          {$ifdef LARGEINT}NodeBuffers[Mask shr LARGE_NODEPTR_OFFSET] +{$endif}
          Mask and NODEPTR_CLEAN_MASK
        ).NodeInfo and (_7 shl 8) = 0) then Break;
    end;
  until (False);

  // store first
  Result.FirstCell := Cell;
  Result.FirstSector := Sector;

  // move right
  Cell := Store.Cell;
  Sector := Store.Sector;
  Dec(Cell);
  Dec(Sector);
  repeat
    Inc(Cell);
    Inc(Sector);
    Mask := Cell.NodePtr;
    if (Mask and NODEPTR_FLAG_ALLOCATED = 0) then
    begin
      if (Mask and (_3 shl 8) = 0) then Break;
    end else
    begin
      if (PCPFNode
        (
          {$ifdef LARGEINT}NodeBuffers[Mask shr LARGE_NODEPTR_OFFSET] +{$endif}
          Mask and NODEPTR_CLEAN_MASK
        ).NodeInfo and (_3 shl 8) = 0) then Break;
    end;
  until (False);

  // store last
  Result.LastCell := Cell;

  // fill char Byte(SectorValues)
  Count := NativeUInt(Sector) + 1;
  Sector := Result.FirstSector;
  Dec(Count, NativeUInt(Sector));
  SectorValues := Store.SectorValues;

  while (Count >= SizeOf(NativeUInt)) do
  begin
    PNativeUInt(Sector)^ := SectorValues;

    Dec(Count, SizeOf(NativeUInt));
    Inc(Sector, SizeOf(NativeUInt));
  end;

  {$ifdef LARGEINT}
  if (Count >= SizeOf(Cardinal)) then
  begin
    PCardinal(Sector)^ := SectorValues;

    Dec(Count, SizeOf(Cardinal));
    Inc(Sector, SizeOf(Cardinal));
  end;
  {$endif}

  if (Count <> 0) then
  begin
    if (Count and 1 <> 0) then
    begin
      Sector^ := SectorValues;
      Inc(Sector);
      if (Count and 2 = 0) then goto done;
      goto _2;
    end else
    begin
      _2:
      PWord(Sector)^ := SectorValues;
    end;
  end;

done:
end;

procedure FloodTileMapSectors(var _Storage: TFloodLineStorage;
  BaseLineRec: PFloodLineRec
  {$ifdef LARGEINT}; NodeBuffers: PCPFNodeBuffers{$endif}); overload;
type
  TByteList = array[0..0] of Byte;
  PByteList = ^TByteList;
var
  CurrentLineRec: PFloodLineRec;
  LineRec: PFloodLineRec;

  Cell: PCPFCell;
  Sector: PByte;
  Mask, Flags, ChildMask: NativeUInt;
  Offset: NativeInt;

  Storage: TFloodLineStorage;
  Store: record
    {$ifdef CPUX86}
    CurrentLineRec: PFloodLineRec;
    {$endif}
    LastCell: PCPFCell;
    Cell: PCPFCell;
    BufferInitialized: Boolean;
  end;
  Buffer: array[0..127] of TFloodLineRec;
begin
  // stack parameters
  {$ifdef CPUX86}
  Store.CurrentLineRec := BaseLineRec;
  {$endif}
  Storage := _Storage;
  Store.BufferInitialized := False;

  // each line rec loop
  CurrentLineRec := {$ifdef CPUX86}Store.CurrentLineRec{$else}BaseLineRec{$endif};
  repeat
    // each cell loop
    Store.LastCell := CurrentLineRec.LastCell;
    Cell := CurrentLineRec.FirstCell;
    Sector := CurrentLineRec.FirstSector;
    Dec(Cell);
    Dec(Sector);
    repeat
      Inc(Cell);
      Inc(Sector);

      // mask (without left and right)
      Mask := Cell.NodePtr;
      if (Mask and NODEPTR_FLAG_ALLOCATED <> 0) then
      begin
        Mask := PCPFNode
          (
            {$ifdef LARGEINT}NodeBuffers[Mask shr LARGE_NODEPTR_OFFSET] +{$endif}
            Mask and NODEPTR_CLEAN_MASK
          ).NodeInfo;
      end;
      Mask := Mask and ($ff00 and not((_3 or _7) shl 8));

      // each child loop
      Flags := (1 shl 8){bit} + 0{child};
      if (Mask <> 0) then
      repeat
        // skip not childs
        while (Mask and Flags = 0) do
        begin
          Offset := Flags and -8;
          Inc(Flags);
          Inc(Flags, Offset);
        end;

        // check child
        Offset := Storage.Offsets[Flags and 7];
        if (PByteList(Sector)[Offset] = SECTOR_EMPTY) then
        begin
          ChildMask := PCPFCellArray(Cell)[Offset].NodePtr;
          if (ChildMask and NODEPTR_FLAG_ALLOCATED <> 0) then
          begin
            ChildMask := PCPFNode
              (
                {$ifdef LARGEINT}NodeBuffers[ChildMask shr LARGE_NODEPTR_OFFSET] +{$endif}
                ChildMask and NODEPTR_CLEAN_MASK
              ).NodeInfo;
          end;

          if {$ifdef LARGEINT}
               (ChildMask > $ffffff)
             {$else}
               (ChildMask and Integer($ff000000){Tile} <> 0{TILE_BARIER})
             {$endif} and
             (ChildMask and $ff00{Mask} <> 0) then
          begin
            // take new item and push to hot queue
            // initialize buffer or call next function iteration (with new buffer)
            Store.Cell := Cell;
            LineRec := Storage.Pool;
            if (LineRec = nil) then
            begin
              if (Store.BufferInitialized) then
              begin
                // call next function iteration (allocate new buffer items)
                {$ifdef CPUX86}
                CurrentLineRec := Store.CurrentLineRec;
                {$endif}
                CurrentLineRec.FirstCell := Store.Cell;
                FloodTileMapSectors(Storage, CurrentLineRec{$ifdef LARGEINT}, NodeBuffers{$endif});
                Exit;
              end else
              begin
                // add buffer items to the pool
                CurrentLineRec := @Buffer[0];
                LineRec := @Buffer[1 - 1];
                Dec(CurrentLineRec);
                repeat
                  Inc(CurrentLineRec);
                  Inc(LineRec);
                  CurrentLineRec.Next := LineRec;
                until (CurrentLineRec = @Buffer[High(Buffer)]);
                CurrentLineRec.Next := nil;
                LineRec := @Buffer[0];
                Store.BufferInitialized := True;
              end;
            end;

            Storage.Pool := LineRec.Next;
            Storage.QueueLast.Next := LineRec;
            Storage.QueueLast := LineRec;
            LineRec.Next := nil{hot queue end};
            LineRec.SectorValues := Storage.SectorValues;
            FillFloodLineRec(LineRec^, @PCPFCellArray(Store.Cell)[Offset],
              @PByteList(Sector)[Offset]{$ifdef LARGEINT}, NodeBuffers{$endif});
            Cell := Store.Cell;
          end else
          begin
            PByteList(Sector)[Offset] := SECTOR_PATHLESS;
          end;
        end;

        Mask := (Mask xor Flags) and -8;
        Offset := Flags and -8;
        Inc(Flags);
        Inc(Flags, Offset);        
      until (Mask = 0);
    until (Cell = Store.LastCell);

    // take next queue item
    {$ifdef CPUX86}
    CurrentLineRec := Store.CurrentLineRec;
    {$endif}
    LineRec := CurrentLineRec.Next;
    if (LineRec = nil) then Exit;

    // add empty item to pool
    CurrentLineRec.Next := Storage.Pool;
    Storage.Pool := CurrentLineRec;

    // next interation
    {$ifdef CPUX86}
    Store.CurrentLineRec := LineRec;
    {$endif}
    CurrentLineRec := LineRec;
  until (False);
end;

procedure FloodTileMapSectors(var Storage: TFloodLineStorage;
  const Cell: PCPFCell; const Sector: PByte
  {$ifdef LARGEINT}; NodeBuffers: PCPFNodeBuffers{$endif}); overload;
var
  FloodLineRec: TFloodLineRec;
  Buffer: array[0..7] of TFloodLineRec;
begin
  // initialize sector rec
  FloodLineRec.SectorValues := Storage.SectorValues;
  FillFloodLineRec(FloodLineRec, Cell, Sector{$ifdef LARGEINT}, NodeBuffers{$endif});

  // initialize queue
  Storage.QueueLast := @FloodLineRec;
  FloodLineRec.Next := nil;

  // initialize basic pool
  Buffer[0].Next := @Buffer[1];
  Buffer[1].Next := @Buffer[2];
  Buffer[2].Next := @Buffer[3];
  Buffer[3].Next := @Buffer[4];
  Buffer[4].Next := @Buffer[5];
  Buffer[5].Next := @Buffer[6];
  Buffer[6].Next := @Buffer[7];
  Buffer[7].Next := nil;
  Storage.Pool := @Buffer[0];

  // call flood recursion
  FloodTileMapSectors(Storage, @FloodLineRec{$ifdef LARGEINT}, NodeBuffers{$endif});
end;

procedure TTileMap.ActualizeSectors;
const
  SECTORS_VALUES_INCREMENT = NativeUInt({$ifdef LARGEINT}$0101010101010101{$else}$01010101{$endif});
var
  i: NativeUInt;
  Cell: PCPFCell;
  CellInfo: NativeUInt;
  Sector: PByte;
  Storage: TFloodLineStorage;
  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  if (FActualInfo.Sectors = nil) then
    CPFGetMem(Pointer(FActualInfo.Sectors), (SizeOf(Byte) * FCellCount + 3) and -4);

  ZeroMemory(FActualInfo.Sectors, SizeOf(Byte) * FCellCount);
  FActualInfo.SectorsChanged := False;

  Storage.Offsets := FSectorOffsets;
  Storage.SectorValues := SECTORS_VALUES_INCREMENT{next is 2};
  {$ifdef LARGEINT}
  NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  Cell := @FInfo.CellArray[0];
  Sector := FActualInfo.Sectors;
  for i := 1 to FCellCount do
  begin
    if (Sector^ = SECTOR_EMPTY{0}) then
    begin
      CellInfo := Cell.NodePtr;
      if (CellInfo and NODEPTR_FLAG_ALLOCATED <> 0) then
      begin
        CellInfo := PCPFNode
          (
            {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
            CellInfo and NODEPTR_CLEAN_MASK
          ).NodeInfo;
      end;

      if {$ifdef LARGEINT}
           (CellInfo <= $ffffff)
         {$else}
           (CellInfo and Integer($ff000000){Tile} = 0{TILE_BARIER})
         {$endif} or
         (CellInfo and $ff00{Mask} = 0) then
      begin
        Sector^ := SECTOR_PATHLESS;
      end else
      begin
        // CurrentSector = (CurrentSector==0xff)?2:CurrentSector+1;
        if (NativeInt(Storage.SectorValues) = -1) then
        begin
          Storage.SectorValues := SECTORS_VALUES_INCREMENT * 2;
        end else
        begin
          Inc(Storage.SectorValues, SECTORS_VALUES_INCREMENT);
        end;

        // call recursion
        FloodTileMapSectors(Storage, Cell, Sector{$ifdef LARGEINT}, NodeBuffers{$endif});
      end;
    end;

    Inc(Cell);
    Inc(Sector);
  end;
end;

procedure TTileMap.FreeStorageTopBuffer;
var
  Count: NativeUInt;
begin
  Count := FNodes.Storage.Count;
  if (Count > 2) and (Count > FInfo.NodeAllocator.Count) then
  begin
    Dec(Count);
    FNodes.Storage.Count := Count;
    CPFFreeMem(FNodes.Storage.Buffers[Count]);
  end;
end;

procedure FreeAllocatedNodeArray(const Info: TCPFInfo; Node, HighNode: PCPFNode);
var
  Cells: PCPFCellArray;
  MapWidth: NativeInt;
  Coordinates: NativeInt;
begin
  Cells := Info.CellArray;
  MapWidth := Info.MapWidth;

  // while (Node <> HN{HighNode}) do
  if (Node <> HighNode) then
  repeat
    Coordinates := Cardinal(Node.Coordinates);

    with Cells[(Coordinates shr 16){X} + MapWidth * {Y}Word(Coordinates)] do
    begin
      NodePtr := Node.NodeInfo and Integer($ff00ff00){mask and tile};
    end;

    Inc(Node);
  until (Node = HighNode);
end;

procedure TTileMap.FreeAllocatedNodes(const ClearCells{usually True}: Boolean);
var
  Node, HighNode: PCPFNode;
  Index: NativeUInt;
begin
  // no attanable tree nodes
  FNodes.AttainableTree := False;

  // hot pool
  FNodes.HotPool.First.Next := @FNodes.HotPool.Last;
  FNodes.HotPool.Last.Prev := @FNodes.HotPool.First;

  // excluded pool
  FNodes.ExcludedPool.First.Next := @FNodes.ExcludedPool.Last;
  FNodes.ExcludedPool.Last.Prev := @FNodes.ExcludedPool.First;

  // heuristed pool
  FNodes.HeuristedPool.First.Next := @FNodes.HeuristedPool.Last;
  FNodes.HeuristedPool.Last.Prev := @FNodes.HeuristedPool.First;

  // unattainable pool
  FNodes.UnattainablePool.First.Next := @FNodes.UnattainablePool.Last;
  FNodes.UnattainablePool.Last.Prev := @FNodes.UnattainablePool.First;

  // clear map cells
  if (ClearCells) and (FInfo.NodeAllocator.Count <> 0) then
  begin
    Index := FInfo.NodeAllocator.Count - 1;

    Node := FInfo.NodeAllocator.Buffers[Index];
    HighNode := FInfo.NodeAllocator.NewNode;
    FreeAllocatedNodeArray(FInfo, Node, HighNode);

    while (Index <> 0) do
    begin
      Dec(Index);

      HighNode := FInfo.NodeAllocator.Buffers[Index];
      Node := HighNode;
      Inc(HighNode, NODESTORAGE_INFO[Index].Count);

      FreeAllocatedNodeArray(FInfo, Node, HighNode);
    end;
  end;

  // clear a half of top node buffers
  Index{current count} := FNodes.Storage.Count;
  if (Index > 2) then
  begin
    FNodes.Storage.Count{new count} := 2 + (Index - 2) shr 1;

    while (Index <> FNodes.Storage.Count) do
    begin
      Dec(Index);
      CPFFreeMem(FNodes.Storage.Buffers[Index]);
    end;
  end;

  // fill allocator by first buffer
  FInfo.NodeAllocator.Count := 0;
  GrowNodeAllocator(FInfo);
end;


type
  PCellRec = ^TCellRec;
  TCellRec = record
    Next: PCellRec;
    Cell: PCPFCell;
  end;

  TCellRecStorage = record
    Offsets: TCPFOffsets;
    QueueLast: PCellRec;
    Pool: PCellRec;
  end;

procedure ReleaseAroundAttainableNodes(var _Storage: TCellRecStorage;
  BasicCellRec: PCellRec
  {$ifdef LARGEINT}; NodeBuffers: PCPFNodeBuffers{$endif});
var
  CurrentRec: PCellRec;
  Cell: PCPFCell;
  Mask, Flags: NativeUInt;
  NodeInfo: NativeUInt;

  NextRec: PCellRec;
  Node, Left, Right: PCPFNode;

  Store: record
    Storage: TCellRecStorage;
    CurrentRec: PCellRec;
    Cell: PCPFCell;
    Buffer: array[0..127] of TCellRec;
    BufferInitialized: Boolean;
  end;
begin
  // stack parameters
  Store.Storage := _Storage;
  Store.BufferInitialized := False;

  // each cell loop
  CurrentRec := BasicCellRec;
  repeat
    Cell := CurrentRec.Cell;

    // mask
    Mask := Cell.NodePtr;
    Mask := PCPFNode(
        {$ifdef LARGEINT}NodeBuffers[Mask shr LARGE_NODEPTR_OFFSET] +{$endif}
        Mask and NODEPTR_CLEAN_MASK
      ).NodeInfo and $ff00;

    // each child loop
    Flags := (1 shl 8){bit} + 0{child};
    if (Mask <> 0) then
    repeat
      // skip not childs
      while (Mask and Flags = 0) do
      begin
        NodeInfo := Flags and -8;
        Inc(Flags);
        Inc(Flags, NodeInfo);
      end;

      // look attainable (knownpath) child
      NodeInfo := PCPFCell(NativeInt(Cell) + Store.Storage.Offsets[Flags and 7]).NodePtr;
      if (NodeInfo and NODEPTR_FLAG_HEURISTED <> 0) and
         (PCPFNode(
            {$ifdef LARGEINT}NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
            NodeInfo and NODEPTR_CLEAN_MASK
          ).NodeInfo and FLAG_KNOWN_PATH <> 0) then
      begin
        // store cell
        Store.Cell := Cell;

        // check available pool item
        NextRec := Store.Storage.Pool;
        if (NextRec{Store.Storage.Pool} = nil) then
        begin
          if (Store.BufferInitialized) then
          begin
            // call next function iteration (allocate new buffer items)
            ReleaseAroundAttainableNodes(Store.Storage, CurrentRec{$ifdef LARGEINT}, NodeBuffers{$endif});
            Exit;
          end else
          begin
            Store.CurrentRec := CurrentRec;
            begin
              // add buffer items to the pool
              CurrentRec := @Store.Buffer[0];
              NextRec := @Store.Buffer[1 - 1];
              Dec(CurrentRec);
              repeat
                Inc(CurrentRec);
                Inc(NextRec);
                CurrentRec.Next := NextRec;
              until (CurrentRec = @Store.Buffer[High(Store.Buffer)]);
              CurrentRec.Next := nil;
              NextRec := @Store.Buffer[0];
              Store.BufferInitialized := True;
            end;
            CurrentRec := Store.CurrentRec;
          end;
        end;

        // cell, pool, queue
        Inc(NativeInt(Cell), Store.Storage.Offsets[Flags and 7]);
        Store.Storage.Pool := NextRec.Next;
        Store.Storage.QueueLast.Next := NextRec;
        Store.Storage.QueueLast := NextRec;
        NextRec.Next := nil{hot queue end};
        NextRec.Cell := Cell;

        // mark as allocated only
        NodeInfo := Cell.NodePtr and (not NODEPTR_FLAG_HEURISTED);
        Cell.NodePtr := NodeInfo;

        // remove from list
        Node := PCPFNode(
            {$ifdef LARGEINT}NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
            NodeInfo and NODEPTR_CLEAN_MASK
          );
        if (Node.SortValue < NATTANABLE_LENGTH_LIMIT) then
        begin
          Left := Node.Prev;
          Right := Node.Next;
          Left.Next := Right;
          Right.Prev := Left;
        end;

        // retrieve cell
        Cell := Store.Cell;
      end;

      Mask := (Mask xor Flags) and -8;
      NodeInfo := Flags and -8;
      Inc(Flags);
      Inc(Flags, NodeInfo);
    until (Mask = 0);


    // take next queue item
    NextRec := CurrentRec.Next;

    // add empty item to pool
    CurrentRec.Next := Store.Storage.Pool;
    Store.Storage.Pool := CurrentRec;

    // next interation
    CurrentRec := NextRec;
  until (NextRec = nil);
end;

procedure TTileMap.ForgetAttainableTreeNodes;
var
  Cell: PCPFCell;
  NodeInfo: NativeUInt;
  Node, Left, Right: PCPFNode;

  Storage: TCellRecStorage;
  CellRec: TCellRec;
  Buffer: array[0..7] of TCellRec;
  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  if (not FNodes.AttainableTree) then Exit;
  FNodes.AttainableTree := False;

  Cell := @FInfo.CellArray[FInfo.MapWidth * FInfo.FinishPoint.Y + FInfo.FinishPoint.X];
  NodeInfo := Cell.NodePtr;
  {$ifdef LARGEINT}
    NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
    Node := Pointer(
      (NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET]) +
      (NodeInfo and NODEPTR_CLEAN_MASK) );
  {$else}
    Node := Pointer(NodeInfo and NODEPTR_CLEAN_MASK);
  {$endif}
  Cell.NodePtr := NodeInfo and (not NODEPTR_FLAG_HEURISTED);

  // remove from list
  if (Node.SortValue < NATTANABLE_LENGTH_LIMIT) then
  begin
    Left := Node.Prev;
    Right := Node.Next;
    Left.Next := Right;
    Right.Prev := Left;
  end;

  // storage, pool, queue
  CellRec.Cell := Cell;
  Storage.Offsets := FInfo.CellOffsets;
  Storage.QueueLast := @CellRec;
  CellRec.Next := nil;
  Buffer[0].Next := @Buffer[1];
  Buffer[1].Next := @Buffer[2];
  Buffer[2].Next := @Buffer[3];
  Buffer[3].Next := @Buffer[4];
  Buffer[4].Next := @Buffer[5];
  Buffer[5].Next := @Buffer[6];
  Buffer[6].Next := @Buffer[7];
  Buffer[7].Next := nil;
  Storage.Pool := @Buffer[0];

  // recursion call
  ReleaseAroundAttainableNodes(Storage, @CellRec{$ifdef LARGEINT}, NodeBuffers{$endif});
end;

procedure TTileMap.ReleasePoolNodes(var PoolFirst, PoolLast: TCPFNode);
var
  Node: PCPFNode;
  Cells: PCPFCellArray;
  Cell: PCPFCell;
  Coordinates, MapWidth: NativeInt;
begin
  // take node list
  Node := PoolLast.Prev;
  if (Node = @PoolFirst) then Exit;
  Node.Next := nil;
  Node := PoolFirst.Next;
  PoolFirst.Next := @PoolLast;
  PoolLast.Prev := @PoolFirst;

  // clear flag loop
  Cells := FInfo.CellArray;
  MapWidth := FInfo.MapWidth;
  while (Node <> nil) do
  begin
    Coordinates := Cardinal(Node.Coordinates);
    Node := Node.Next;

    Cell := @Cells[(Coordinates shr 16){X} + MapWidth * {Y}Word(Coordinates)];
    Cell.NodePtr := Cell.NodePtr and (not NODEPTR_FLAG_HEURISTED);
  end;
end;

procedure TTileMap.FlushHotPoolNodes;
var
  LastPoolNode: PCPFNode;
  Node, Buffer: PCPFNode;
begin
  // start point nodes
  Buffer{Node} := FNodes.HotPool.First.Next;
  if (Buffer{Node} = @FNodes.HotPool.Last) then Exit;

  // mark list end (nil)
  FNodes.HotPool.Last.Prev.Next := nil;

  // clear hot pool
  FNodes.HotPool.First.Next := @FNodes.HotPool.Last;
  FNodes.HotPool.Last.Prev := @FNodes.HotPool.First;

  // process each node
  if (not FNodes.HotPool.Unattainable) then
  begin
    // first heuristed pool node
    Buffer{Node}.Prev := @FNodes.HeuristedPool.First;
    LastPoolNode := FNodes.HeuristedPool.First.Next;
    FNodes.HeuristedPool.First.Next := Buffer{Node};

    // retrieve parent mask (unlock)
    // clear sort value
    repeat
      Node := Buffer;
      Buffer := Buffer.Next;

      Node.ParentMask := $ff;
      Node.Path := SORTVALUE_LIMIT - (Node.SortValue - Node.Path);
      Node.SortValue := SORTVALUE_LIMIT;
    until (Buffer = nil);
  end else
  begin
    // first unattainable pool node
    Buffer{Node}.Prev := @FNodes.UnattainablePool.First;
    LastPoolNode := FNodes.UnattainablePool.First.Next;
    FNodes.UnattainablePool.First.Next := Buffer{Node};

    // retrieve parent mask (unlock)
    // mark as KnownPath Unattainable
    // clear node heuristics
    repeat
      Node := Buffer;
      Buffer := Buffer.Next;

      Node.NodeInfo := (Node.NodeInfo and FLAGS_CLEAN_MASK) or ($00ff0000{parent mask} or FLAG_KNOWN_PATH);
      Node.Path := SORTVALUE_LIMIT;
      Node.SortValue := SORTVALUE_LIMIT;
    until (Buffer = nil);
  end;

  // last heuristed or unattainable pool node
  Node.Next := LastPoolNode;
  LastPoolNode.Prev := Node;
end;

procedure TTileMap.UnlockExcludedNodes;
var
  Buffer, Node, Right: PCPFNode;
  NodeInfo: Cardinal;
begin
  // first excluded node
  Buffer{Node} := FNodes.ExcludedPool.First.Next;
  if (Buffer{Node} = @FNodes.ExcludedPool.Last) then Exit;

  // mark list end (nil)
  FNodes.ExcludedPool.Last.Prev.Next := nil;

  // clear excluded pool
  FNodes.ExcludedPool.First.Next := @FNodes.ExcludedPool.Last;
  FNodes.ExcludedPool.Last.Prev := @FNodes.ExcludedPool.First;

  // move to heuristed/unattanable pools loop
  repeat
    Node := Buffer;
    Buffer := Buffer.Next;

    NodeInfo := Node.NodeInfo;
    Node.NodeInfo := NodeInfo or $00ff0000{parent mask};
    if (NodeInfo and (FLAG_KNOWN_PATH or FLAG_ATTAINABLE) = FLAG_KNOWN_PATH{KnownPath, not Attainable}) then
    begin
      Right := FNodes.UnattainablePool.First.Next;
      Node.Prev := @FNodes.UnattainablePool.First;
      Node.Next := Right;
      FNodes.UnattainablePool.First.Next := Node;
    end else
    begin
      Right := FNodes.HeuristedPool.First.Next;
      Node.Prev := @FNodes.HeuristedPool.First;
      Node.Next := Right;
      FNodes.HeuristedPool.First.Next := Node;
    end;
  until (Buffer = nil);
end;

procedure TTileMap.CacheAttainablePath(var StartPoint: TCPFStart; const FinishNode: PCPFNode);
label
  attainable_cached, fill_result;
type
  TSingleWeigths = array[0..255] of Single;
var
  Cell: PCPFCell;
  Left, Right, L, R: PCPFNode;

  ParentNode, Node: PCPFNode;
  ParentNodeInfo, NodeInfo: NativeUInt;
  nLength: Cardinal;
  SingleWeigths: ^TSingleWeigths;
  _StartPoint: PCPFStart;

  Store: record
    {$ifdef CPUX86}
    CellOffsets: TCPFOffsets;
    StartPointNode: PCPFNode;
    SingleDiagonalWeigths: ^TSingleWeigths;
    {$endif}
    StartPoint: PCPFStart;
    Cell: PCPFCell;
  end;

  {$ifNdef CPUX86}
    CellOffsets: PCPFOffsets;
    HALF: Double;
    W1, W2: Double;
    StartPointNode: PCPFNode;
    SingleDiagonalWeigths: ^TSingleWeigths;
  {$endif}

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  // attainable tree flag
  FNodes.AttainableTree := True;

  // store parameters
  Store.StartPoint := @StartPoint;
  {$ifdef CPUX86}
    Store.StartPointNode := StartPoint.Node;
    Store.SingleDiagonalWeigths := Pointer(@FActualInfo.Weights.SinglesDiagonal);
  {$else}
    HALF := CrystalPathFinding.HALF;
    StartPointNode := StartPoint.Node;
    SingleDiagonalWeigths := Pointer(@FActualInfo.Weights.SinglesDiagonal);
  {$endif}

  {$ifdef LARGEINT}
    NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  // cache finish attainable
  if (FinishNode.SortValue < NATTANABLE_LENGTH_LIMIT) then
  begin
    Left := FinishNode.Prev;
    Right := FinishNode.Next;
    Left.Next := Right;
    Right.Prev := Left;
    FinishNode.nAttainableLength := not Cardinal(1);
    FinishNode.Path := not Cardinal(1){heuristics = 0};
    FinishNode.AttainableDistance := 0;
  end;

  {$ifdef CPUX86}
  Store.CellOffsets := FInfo.CellOffsets;
  {$else}
  CellOffsets := @FInfo.CellOffsets;
  {$endif}

  // remove from list: attainable --> cached
  Node := StartPoint.KnownPathNode;
  Cell := @FInfo.CellArray[NativeInt(Node.Coordinates.Y) * Width + Node.Coordinates.X];
  Store.Cell := Cell;
  ParentNode := nil;
  nLength := Node.SortValue;
  if (nLength >= NATTANABLE_LENGTH_LIMIT{Node = FinishNode}) then goto attainable_cached;
  repeat
    // remove node from list
    // store parent (prev field)
    Left := Node.Prev;
    Node.Prev := ParentNode;
    ParentNode{Right} := Node.Next;
    Left.Next := ParentNode{Right};
    ParentNode{Right}.Prev := Left;

    // child cell
    ParentNode := Node;
    Inc(NativeInt(Cell), {$ifdef CPUX86}Store.{$endif}CellOffsets[(NativeUInt(Node.NodeInfo) shr 4) and 7]);

    // cell node
    {$ifdef LARGEINT}
      NodeInfo := Cell.NodePtr;
      Node := Pointer(
        (NodeBuffers[NodeInfo shr LARGE_NODEPTR_OFFSET]) +
        (NodeInfo and NODEPTR_CLEAN_MASK) );
    {$else}
      Node := Pointer(Cell.NodePtr and NODEPTR_CLEAN_MASK);
    {$endif}

    // length of cached node
    nLength := Node.SortValue;
  until (nLength >= NATTANABLE_LENGTH_LIMIT);

  // cache: attainable <-- cached
  {$ifdef CPUX86}
  Cardinal(Cell) := nLength;
  {$else}
  NodeInfo := Node.NodeInfo;
  {$endif}
  repeat
    // parameters
    ParentNodeInfo := ParentNode.NodeInfo;
    SingleWeigths := {$ifdef CPUX86}Store.{$endif}SingleDiagonalWeigths;
    Inc(NativeInt(SingleWeigths), (ParentNodeInfo and (1 shl 4){child}) shl (10 - 4));

    // heuristics, length and distance, next node
    Dec({$ifdef CPUX86}Cardinal(Cell){$else}nLength{$endif}){increment};
    ParentNode.Path := {$ifdef CPUX86}Cardinal(Cell){$else}nLength{$endif} - (ParentNode.SortValue - ParentNode.Path){heurisctics};
    ParentNode.nAttainableLength := {$ifdef CPUX86}Cardinal(Cell){$else}nLength{$endif};
    {$ifdef CPUX86}
      ParentNodeInfo{W1} := NativeUInt(@SingleWeigths[ParentNodeInfo shr 24]);
      SingleWeigths{W2} := Pointer(@SingleWeigths[Node.NodeInfo shr 24]);
      PCPFNode(nLength){Left} := ParentNode.Prev;

      ParentNode.AttainableDistance := Node.AttainableDistance +
        HALF * ({W1}PSingle(ParentNodeInfo)^ + {W2}SingleWeigths[0]);
      Node := ParentNode;
      ParentNode := {Left}PCPFNode(nLength);
    {$else}
      W1 := SingleWeigths[ParentNodeInfo shr 24];
      W2 := SingleWeigths[NodeInfo shr 24];
      Left := ParentNode.Prev;

      ParentNode.AttainableDistance := Node.AttainableDistance + HALF * (W1 + W2);
      NodeInfo := ParentNodeInfo;
      Node := ParentNode;
      ParentNode := Left;
    {$endif}
  until ({$ifdef CPUX86}PCPFNode(nLength){$else}Left{$endif} = nil){Node = StartPoint.AttainableNode};

  // remove from list and cache: start node <-- attainable
attainable_cached:
  if (Node = {$ifdef CPUX86}Store.{$endif}StartPointNode) then goto fill_result;  
  Cell := Store.Cell;
  NodeInfo := Node.NodeInfo;
  repeat
    // parent cell
    Inc(NativeInt(Cell), {$ifdef CPUX86}Store.{$endif}CellOffsets[NodeInfo and 7]);

    // cell node
    {$ifdef LARGEINT}
      ParentNodeInfo := Cell.NodePtr;
      ParentNode := Pointer(
        (NodeBuffers[ParentNodeInfo shr LARGE_NODEPTR_OFFSET]) +
        (ParentNodeInfo and NODEPTR_CLEAN_MASK) );
    {$else}
      ParentNode := Pointer(Cell.NodePtr and NODEPTR_CLEAN_MASK);
    {$endif}

    // child and flags (unlock if needed)
    ParentNodeInfo := ((NativeUInt(ParentNode.NodeInfo) and FLAGS_CLEAN_MASK) or
      ($00ff0000 + FLAG_KNOWN_PATH + FLAG_ATTAINABLE)) +
      (((NodeInfo + 4){invert parent} and 7) shl 4);
    ParentNode.NodeInfo := ParentNodeInfo;

    // remove from list
    L := ParentNode.Prev;
    R := ParentNode.Next;
    L.Next := R;
    R.Prev := L;
    {$if Defined(CPUX86) and (not Defined(FPC))}
    ParentNode := Pointer(Cell.NodePtr and NODEPTR_CLEAN_MASK);
    {$ifend}

    // store heuristics
    {$ifdef CPUX86}
    nLength := Node.nAttainableLength;
    {$endif}
    Dec(nLength){increment};
    {$ifdef CPUX86}
      NodeInfo := (ParentNode.SortValue - ParentNode.Path){heurisctics};
      ParentNode.Path := nLength - NodeInfo;
    {$else}
      ParentNode.Path := nLength - (ParentNode.SortValue - ParentNode.Path){heurisctics};
    {$endif}

    // length and distance
    SingleWeigths := {$ifdef CPUX86}Store.{$endif}SingleDiagonalWeigths;
    ParentNode.nAttainableLength := nLength;
    Inc(NativeInt(SingleWeigths), (ParentNodeInfo and (1 shl 4){child}) shl (10 - 4));
    {$ifdef CPUX86}
      ParentNode.AttainableDistance := Node.AttainableDistance +
        HALF * (SingleWeigths[ParentNodeInfo shr 24] + SingleWeigths[Node.NodeInfo shr 24]);
    {$else}
      W1 := SingleWeigths[ParentNodeInfo shr 24];
      W2 := SingleWeigths[NodeInfo shr 24];
      ParentNode.AttainableDistance := Node.AttainableDistance + HALF * (W1 + W2);
    {$endif}
    NodeInfo := ParentNodeInfo;
    Node := ParentNode;
  until (ParentNode = {$ifdef CPUX86}Store.{$endif}StartPointNode);

  // result length and distance
 (* with Store.StartPoint^ do
  begin
    Length := not {$ifdef CPUX86}{Parent}Node.nAttainableLength{$else}nLength{$endif};
    Distance := {Parent}Node.AttainableDistance;
  end;*)
fill_result:
  _StartPoint := Store.StartPoint;
  _StartPoint.Length := not {$ifdef CPUX86}{Parent}Node.nAttainableLength{$else}nLength{$endif};
  _StartPoint.Distance := {Parent}Node.AttainableDistance;
end;

procedure TTileMap.FillAttainablePath(const StartNode, FinishNode: PCPFNode);
var
  Node: PCPFNode;
  Cell: PCPFCell;
  Point: PPoint;
  CellOffsets: PCPFOffsets;
  N: NativeUInt;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
begin
  // parameters
  Node := StartNode;
  Cell := @FInfo.CellArray[NativeInt(Node.Coordinates.Y) * Width + Node.Coordinates.X];
  Point := FActualInfo.FoundPath.Buffer.Memory;
  CellOffsets := @FInfo.CellOffsets;
  {$ifdef LARGEINT}
  NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
  {$endif}

  // each node loop
  repeat
    // coordinates
    {$ifdef CPUX86}
      Point.Y := Node.Coordinates.Y;
      Point.X := Node.Coordinates.X;
    {$else}
      N := Cardinal(Node.Coordinates);
      {$ifdef LARGEINT}
        PNativeUInt(Point)^ := (NativeUInt(Word(N)) shl 32) + (N shr 16);
      {$else}
        Point.Y := Word(N);
        Point.X := N shr 16;
      {$endif}
    {$endif}
    Inc(Point);

    // known child cell
    Inc(NativeInt(Cell), CellOffsets[(Node.NodeInfo shr 4) and 7]);

    // cell node
    {$ifdef LARGEINT}
      N := Cell.NodePtr;
      Node := Pointer(
        (NodeBuffers[N shr LARGE_NODEPTR_OFFSET]) +
        (N and NODEPTR_CLEAN_MASK) );
    {$else}
      Node := Pointer(Cell.NodePtr and NODEPTR_CLEAN_MASK);
    {$endif}
  until (Node = FinishNode);

  // finish node coordinates
  N := Cardinal(Node.Coordinates);
  {$ifdef LARGEINT}
    PNativeUInt(Point)^ := (NativeUInt(Word(N)) shl 32) + (N shr 16);
  {$else}
    Point.Y := Word(N);
    Point.X := N shr 16;
  {$endif}
end;

procedure TTileMap.FillStandardPath(const StartNode, FinishNode: PCPFNode);
type
  TWeightCounts = packed record
    Diagonal: Integer;
    Line: Integer;
  end;
  TWeightCountsBuffer = packed record
  case Boolean of
    False: (WeightCounts: array[0..256 - 1] of TWeightCounts);
    True: (Values: array[0..256*2 - 1] of Integer);
  end;
var
  Buffer: TWeightCountsBuffer;
  CellOffsets: PCPFOffsets;
  Cell: PCPFCell;
  Node: PCPFNode;
  NodePtrs, N, X: NativeUInt;
  Length, i: NativeUInt;
  WeightCounts: ^TWeightCounts;
  Point: PPoint;

  Store: record
    FinishNode: PCPFNode;
    HighTile: NativeUInt;
    LastLine: NativeUInt{Boolean};
    {$ifdef CPUX86}
      StartNode: PCPFNode;
      Length: NativeUInt;
    {$else}
      _Self: Pointer;
    {$endif}
  end;
  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
  {$ifNdef CPUX86}
    _Self: Pointer;
    _Buffer: ^TWeightCountsBuffer;
  {$endif}
begin
  // basic parameters
  {$ifdef CPUX86}Store.StartNode := StartNode;{$endif}
  Node := FinishNode;
  Store.FinishNode := FinishNode;
  {$ifNdef CPUX86}
  _Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});
  Store._Self := _Self;
  with TTileMapPtr(_Self){$ifdef CPFLIB}^{$endif} do
  {$endif}
  begin
    Cell := @FInfo.CellArray[NativeInt(Node.Coordinates.Y) * Width + Node.Coordinates.X];
    {$ifNdef CPUX86}
    CellOffsets := @FInfo.CellOffsets;
    {$endif}
    {$ifdef LARGEINT}
    NodeBuffers := Pointer(@FInfo.NodeAllocator.Buffers);
    {$endif}
  end;

  // detect high tile(s) and path length
  N := Node.NodeInfo;
  {$ifdef CPUX86}
    NodePtrs := (N and $ff000000) + 1;
  {$else}
    Length := 1;
    NodePtrs := N;
  {$endif}
  repeat
    // next cell
    {$ifdef CPUX86}
    CellOffsets := @FInfo.CellOffsets;
    {$endif}
    Inc(NativeInt(Cell), CellOffsets[N and 7{parent}]);

    // cell node
    {$ifdef LARGEINT}
      N := Cell.NodePtr;
      Node := Pointer(
        (NodeBuffers[N shr LARGE_NODEPTR_OFFSET]) +
        (N and NODEPTR_CLEAN_MASK) );
    {$else}
      Node := Pointer(Cell.NodePtr and NODEPTR_CLEAN_MASK);
    {$endif}

    // tile and length increment
    N := Node.NodeInfo;
    {$ifdef CPUX86}
      Inc(NodePtrs);
      NodePtrs := NodePtrs or (N and $ff000000);
    {$else}
      Inc(Length);
      NodePtrs := NodePtrs or N;
    {$endif}
  until ({$ifdef CPUX86}Store.{$endif}StartNode = Node);

  // path points buffer allocate
  {$ifdef CPUX86}
    Length := NodePtrs and $ffffff;
  {$endif}
  {$ifNdef CPUX86}
  _Self := Store._Self;
  with TTileMapPtr(_Self){$ifdef CPFLIB}^{$endif} do
  {$endif}
  begin
    FActualInfo.FoundPath.Length := Length;
    Length{Size} := Length * SizeOf(TPoint);
    {$ifdef CPUX86}Store.Length := Length;{$endif}
    with FActualInfo.FoundPath.Buffer do
    if (Length{Size} > FAllocatedSize) then Realloc(Length{Size});
  end;

  // clear weigths counters
  NodePtrs := NodePtrs shr 24;
  WeightCounts := @Buffer.WeightCounts[1];
  Store.HighTile := NodePtrs;
  for i := 1 to NodePtrs{HighTile} do
  begin
    PInt64(WeightCounts)^ := 0;
    Inc(WeightCounts);
  end;

  // fill points and counters
  Node := Store.FinishNode;
  {$ifNdef CPUX86}
  _Self := Store._Self;
  with TTileMapPtr(_Self){$ifdef CPFLIB}^{$endif} do
  {$endif}
  begin
    Cell := @FInfo.CellArray[NativeInt(Node.Coordinates.Y) * Width + Node.Coordinates.X];
    Point := FActualInfo.FoundPath.Buffer.FMemory;
  end;
  Inc(NativeUInt(Point), {$ifdef CPUX86}Store.{$endif}Length{Size});
  // finish point
  Dec(Point);
  N := Cardinal(Node.Coordinates);
  {$ifdef LARGEINT}
    PNativeUInt(Point)^ := (NativeUInt(Word(N)) shl 32) + (N shr 16);
  {$else}
    Point.Y := Word(N);
    Point.X := N shr 16;
  {$endif}
  N := Node.NodeInfo;
  {$ifdef CPUX86}
    Inc(Buffer.Values[((N shr 23) and -2) + (N and 1)]);
  {$else}
    _Buffer := @Buffer;
    Inc(_Buffer.Values[((N shr 23) and -2) + (N and 1)]);
  {$endif}
  Store.LastLine := N and 1;
  repeat
    // next parent cell/node
    {$ifdef CPUX86}
    CellOffsets := @FInfo.CellOffsets;
    {$endif}
    Inc(NativeInt(Cell), CellOffsets[N and 7{parent}]);
    {$ifdef LARGEINT}
      N := Cell.NodePtr;
      Node := Pointer(
        (NodeBuffers[N shr LARGE_NODEPTR_OFFSET]) +
        (N and NODEPTR_CLEAN_MASK) );
    {$else}
      Node := Pointer(Cell.NodePtr and NODEPTR_CLEAN_MASK);
    {$endif}

    // coordinates
    Dec(Point);
    N := Cardinal(Node.Coordinates);
    {$ifdef LARGEINT}
      PNativeUInt(Point)^ := (NativeUInt(Word(N)) shl 32) + (N shr 16);
    {$else}
      Point.Y := Word(N);
      Point.X := N shr 16;
    {$endif}

    // counters
    N := Node.NodeInfo;
    X := (N shr 23) and -2;
    {$ifdef CPUX86}
      Inc(Buffer.Values[X + Store.LastLine]);
      Store.LastLine := N and 1;
      Inc(Buffer.Values[X + (N and 1)]);
    {$else}
      _Buffer := @Buffer;
      Inc(_Buffer.Values[X + Store.LastLine]);
      Store.LastLine := N and 1;
      Inc(_Buffer.Values[X + (N and 1)]);
    {$endif}
  until ({$ifdef CPUX86}Store.{$endif}StartNode = Node);

  // correction
  {$ifdef CPUX86}
    Dec(Buffer.Values[X + (N and 1)]);
  {$else}
    Dec(_Buffer.Values[X + (N and 1)]);
  {$endif}

  // distance
  {$ifNdef CPUX86}
  _Self := Store._Self;
  with TTileMapPtr(_Self){$ifdef CPFLIB}^{$endif} do
  {$endif}
  begin
    FActualInfo.FoundPath.Distance := 0;
    WeightCounts := @Buffer.WeightCounts[1];
    for i := 1 to Store.HighTile do
    begin
      if (PInt64(WeightCounts)^ <> 0) then
      begin
        FActualInfo.FoundPath.Distance := FActualInfo.FoundPath.Distance + HALF *
          ((WeightCounts.Diagonal * FActualInfo.Weights.SinglesDiagonal[i]) +
          (WeightCounts.Line * FActualInfo.Weights.SinglesLine[i]));
      end;

      Inc(WeightCounts);
    end;
  end;
end;

// flags: oddyfinish, hexagonal, simple, moveleft, movedown (see TTileMap.AllocateHeuristedNode()
function CalculatePathLoopFlags(Kind, Start, Finish: NativeInt): NativeInt;
const
  MOVELEFT_OFFSET = 3;
  MOVELEFT_MASK = Integer(1 shl MOVELEFT_OFFSET);
  MOVEDOWN_OFFSET = 4;
  MOVEDOWN_MASK = Integer(1 shl MOVEDOWN_OFFSET);
var
  X, Y, Mask: NativeInt;
begin
  // oddyfinish, hexagonal, simple
  Result := ((Kind - 1) and 4) + (((Kind + 1) and 4) shr 1) + (Finish and 1);

  // (dX, dY) = Start.Coordinates - Finish.Coordinates;
  Y := Word(Start);
  X := (Start shr 16);
  Y := Y - Word(Finish);
  X := X - (Finish shr 16);

  // Y := Abs(dY)
  Mask := -(Y shr HIGH_NATIVE_BIT);
  Y := Y xor Mask;
  Dec(Y, Mask);
  Result := Result or (Mask and MOVEDOWN_MASK);

  // X := Abs(dX)
  Mask := -(X shr HIGH_NATIVE_BIT);
  X := X xor Mask;
  Dec(X, Mask);
  Result := Result or ((Mask and MOVELEFT_MASK) xor MOVELEFT_MASK);

  // hexagonal correction
  if (Result and 2{hexagonal} <> 0) and (X = 0) then
  begin
    Result := Result and (not MOVELEFT_MASK);
    Result := Result or (((not Result) and Y and 1) shl MOVELEFT_OFFSET);
  end;
end;

function TTileMap.DoFindPathLoop(StartNode: PCPFNode): PCPFNode;
label
  nextchild_continue, nextchild,
  heuristics_data,
  next_current, current_initialize;
const
  NODEPTR_FLAGS = NODEPTR_FLAG_HEURISTED + NODEPTR_FLAG_ALLOCATED;
  PARENT_BITS_CLEAR_MASK = not Cardinal($00ff0000 + 7);
  COUNTER_OFFSET = 16;
  TEMP_WAY_OFFSET = 5;
  TEMP_WAY_CLEAR_MASK = not Integer(7 shl TEMP_WAY_OFFSET);
type
  TMapNodeBuffer = array[0..7] of PCPFNode;
  PMapNodeBuffer = ^TMapNodeBuffer;
  TCardinalList = array[0..High(Integer) div SizeOf(Cardinal) - 1] of Cardinal;
  PCardinalList = ^TCardinalList;
var
  Node: PCPFNode;
  NodeFlags: NativeUInt;
  ChildList: PWord;
  Child: NativeUInt;
  Cell: PCPFCell;
  ParentBits: NativeUInt;
  ChildNodeInfo: NativeUInt;
  ChildNode: PCPFNode;
  TileWeights: PCardinalList;
  Path: Cardinal;
  NodeXY, OffsetXY, ChildXY: Cardinal;
  X, Y, Mask: NativeInt;

  Buffer: PMapNodeBuffer;
  CellOffsets: ^TCPFOffsets;

  ChildSortValue: Cardinal;
  PBufferHigh, PBufferBase, PBufferCurrent: ^PCPFNode;
  Right: PCPFNode;
  {$ifNdef CPUX86}
    Left: PCPFNode;
  {$endif}

  Store: record
    Buffer: TMapNodeBuffer;
    Self: Pointer;
    Flags: NativeUInt;
    Info: TCPFInfo;
    CardinalsDiagonal: Pointer;

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

  {$ifNdef CPUX86}
    TopGreatherNode: PCPFNode;
  {$endif}

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
    NODEPTR_MODIFIER: NativeInt;
  {$endif}
begin
  // store Self
  Store.Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});

  // flags: oddyfinish, hexagonal, simple, moveleft, movedown
  Store.Flags := CalculatePathLoopFlags(NativeInt(FKind), Cardinal(StartNode.Coordinates), Cardinal(Self.FInfo.FinishPoint));

  // information copy
  Store.CardinalsDiagonal := @Self.FActualInfo.Weights.CardinalsDiagonal;
  Move(Self.FInfo, Store.Info,
    (SizeOf(Store.Info) - 32 * SizeOf(Pointer)) +
    (Self.FInfo.NodeAllocator.Count * SizeOf(Pointer)) );

  {$ifNdef CPUX86}
    Buffer := @Store.Buffer;
  {$endif}

  {$ifdef LARGEINT}
    NODEPTR_MODIFIER := Store.Info.NodeAllocator.LargeModifier + NODEPTR_FLAGS;
  {$endif}

  // Top initialization
  Node := StartNode.Next;
  Store.Top.Node := Node;
  Store.Top.SortValue := SORTVALUE_LIMIT{Node.SortValue};

  // finding loop from Start to Finish
  Node := StartNode;
  {$ifdef FPC}
    // cell
    NodeFlags{XY} := Cardinal(Node.Coordinates);
    Cardinal(Store.Current.Coordinates) := NodeFlags{XY};
    Cell := @Store.Info.CellArray[(NativeInt(NodeFlags) shr 16){X} + Store.Info.MapWidth * {Y}Word(NodeFlags)];
    Store.Current.Cell := Cell;

    // store pointer and path
    Store.Current.Node := Node;
    Store.Current.Path := Node.Path;
    ChildSortValue := Node.SortValue;
    Store.Current.SortValue := ChildSortValue;

    // node info
    NodeFlags := Cardinal(Node.NodeInfo);
  {$else}
    goto current_initialize;
  {$endif}
  repeat
    // lock
    Node.ParentMask := 0;

    // next top node and sort value
    if (ChildSortValue = Store.Top.SortValue){Node = Store.Top.Node} then
    begin
      repeat
        Node := Node.Next;
        Path := Node.SortValue;
      until (Path > ChildSortValue);

      Store.Top.Node := Node;
      Store.Top.SortValue := Path;
    end;

    // basic child list (way, clockwise)
    ChildList := Pointer(@CHILD_ARRAYS);
    Inc(NativeUInt(ChildList), NodeFlags{Node.NodeInfo} and $f0);

    // reinitialize Node.NodeInfo (from parentflags, mask, parentmask, tile):
    {
      is finish.Y odd: 1;
      is hexagonal: 1;
      is simple: 1;
      is move left: 1;
      is move down: 1;
      three zero/temporary bits: 3;

      mask & parent mask: 8;
      stored childs counter: 8;
      tile: 8;
    }
    X := Store.Flags;
    NodeFlags := (NodeFlags and Integer($ff000000)) or NativeUInt(X){Store.Flags} or
      ((NodeFlags and (NodeFlags shr 8)) and $ff00);

    // hexagonal child list offset
    Inc(NativeUInt(ChildList), (X and 2) shl 7);

    // each child cell loop
    {$ifdef CPUX86}
    Store.ChildList := ChildList;
    {$endif}
    nextchild_continue:
    if (NodeFlags and $ff00 <> 0) then
    repeat
      // first available, skip 2 in some simple cases
      {$ifdef CPUX86}
      ChildList := Store.ChildList;
      {$endif}
      nextchild:
      X := NodeFlags and 4;
      Child := ChildList^;
      Inc(ChildList);
      X := X and Child;
      Child := Child and Integer(not 4);
      Inc(NativeInt(ChildList), X);
      if (NodeFlags and Child = 0) then goto nextchild;
      {$ifdef CPUX86}
      Store.ChildList := ChildList;
      {$endif}

      // clear child bit, get child number
      NodeFlags := NodeFlags and (not Child);
      Child := (Child shr 5) and 7;

      // child map cell
      Cell := Store.Current.Cell;
      CellOffsets := @Store.Info.CellOffsets;
      Inc(NativeInt(Cell), CellOffsets[Child]);

      // parent bits
      Child := Child shl 3;
      Child := Child + (NodeFlags and (4+2)) + (NativeUInt(Cardinal(Store.Current.Coordinates)) and 1);
      ParentBits := PARENT_BITS[Child];

      // allocated new or use exists
      ChildNodeInfo := Cell.NodePtr;
      if (ChildNodeInfo and NODEPTR_FLAG_HEURISTED = 0) then
      begin
        if (ChildNodeInfo and NODEPTR_FLAG_ALLOCATED = 0) then
        begin
          // child node info (without heuristics data)
          ChildNodeInfo := ChildNodeInfo and Integer($ff00ff00);
          ParentBits := ParentBits or ChildNodeInfo;
          if (ChildNodeInfo and $ff00 = 0) then goto nextchild_continue;

          // child path
          TileWeights := Store.CardinalsDiagonal;
          Inc(NativeInt(TileWeights), (ParentBits and 1) shl 10);
          Path := TileWeights[NodeFlags shr 24];
          Inc(Path, TileWeights[ParentBits shr 24]);
          if (Path > PATHLESS_TILE_WEIGHT) then goto nextchild_continue;
          Path := (Path shr 1) + Store.Current.Path;

          // allocate new node
          ChildNode := Store.Info.NodeAllocator.NewNode;
          {$ifdef LARGEINT}
            Cell.NodePtr := NativeUInt(NativeInt(ChildNode) + NODEPTR_MODIFIER);
          {$else}
            Cell.NodePtr := NativeUInt(ChildNode) + NODEPTR_FLAGS;
          {$endif}
          ChildNode.Path := Path;
          ChildNode.NodeInfo := ParentBits;

          // child coordinates
          OffsetXY := Cardinal(POINT_OFFSETS_INVERT[ParentBits and 7]);
          NodeXY := Cardinal(Store.Current.Coordinates);
          ChildXY := NodeXY + OffsetXY;
          OffsetXY := OffsetXY and $ffff0000;
          NodeXY := NodeXY and $ffff0000;
          ChildXY := Word(ChildXY) + NodeXY + OffsetXY;
          Cardinal(ChildNode.Coordinates) := ChildXY;

          // set next allocable node
          if (ChildNode <> Store.Info.NodeAllocator.NewNodeLimit) then
          begin
            Inc(ChildNode);
            Store.Info.NodeAllocator.NewNode := ChildNode;
            Dec(ChildNode);
          end else
          begin
            {$ifdef CPUX86}
              NativeUInt(Store.TopGreatherNode) := NodeFlags;
            {$endif}
            TTileMapPtr(Store.Self).GrowNodeAllocator(Store.Info);
            {$ifdef LARGEINT}
              NODEPTR_MODIFIER := Store.Info.NodeAllocator.LargeModifier + NODEPTR_FLAGS;
            {$endif}
            {$ifdef CPUX86}
              NodeFlags := NativeUInt(Store.TopGreatherNode);
            {$endif}
          end;
          goto heuristics_data;
        end else
        begin
          // already node allocated and coordinates filled
          // need to fill path/parent and heuristics/way data
          {$ifdef LARGEINT}
            NodeBuffers := Pointer(@Store.Info.NodeAllocator.Buffers);
            ChildNode := Pointer(
              (NodeBuffers[ChildNodeInfo shr LARGE_NODEPTR_OFFSET]) +
              (ChildNodeInfo and NODEPTR_CLEAN_MASK) );
          {$else}
            ChildNode := Pointer(ChildNodeInfo and NODEPTR_CLEAN_MASK);
          {$endif}

          // child node info (without heuristics data)
          TileWeights{ChildNodeInfo} := Pointer(NativeUInt(ChildNode.NodeInfo) and Integer($ff00ff00));
          ParentBits := ParentBits or NativeUInt(TileWeights){ChildNodeInfo};
          if (NativeUInt(TileWeights){ChildNodeInfo} and $ff00 = 0) then goto nextchild_continue;

          // child path
          TileWeights := Store.CardinalsDiagonal;
          Inc(NativeInt(TileWeights), (ParentBits and 1) shl 10);
          Path := TileWeights[NodeFlags shr 24];
          Inc(Path, TileWeights[ParentBits shr 24]);
          if (Path > PATHLESS_TILE_WEIGHT) then goto nextchild_continue;
          Path := (Path shr 1) + Store.Current.Path;

          // heuristed flag
          {$ifdef CPUX86}
            ChildNode := Pointer(Cell.NodePtr);
            Cell.NodePtr := Cardinal(ChildNode) + NODEPTR_FLAG_HEURISTED;
            ChildNode := Pointer(Cardinal(ChildNode) and NODEPTR_CLEAN_MASK);
          {$else}
            Cell.NodePtr := ChildNodeInfo + NODEPTR_FLAG_HEURISTED;
          {$endif}
          ChildNode.Path := Path;
          ChildNode.NodeInfo := ParentBits;

        heuristics_data:
          // (dX, dY) = ChildNode.Coordinates - Store.Info.FinishPoint;
          X := Cardinal(ChildNode.Coordinates);
          Y := Word(X);
          X := X shr 16;
          Mask := Cardinal(Store.Info.FinishPoint);
          Y := Y - Word(Mask);
          X := X - (Mask shr 16);

          // Way
          Mask := 2*Byte(Y > 0) + (Y shr HIGH_NATIVE_BIT);
          Mask := Mask * 3;
          Mask := Mask + 2*Byte(X > 0);
          Mask := Mask - 1 + (X shr {$ifdef CPUX86}31{$else}HIGH_NATIVE_BIT{$endif}); // Delphi compiler optimization bug
          NodeFlags := NodeFlags or NativeUInt(Mask shl TEMP_WAY_OFFSET);

          // Y := Abs(dY)
          Mask := -(Y shr HIGH_NATIVE_BIT);
          Y := Y xor Mask;
          Dec(Y, Mask);

          // X := Abs(dX)
          Mask := -(X shr HIGH_NATIVE_BIT);
          X := X xor Mask;
          Dec(X, Mask);

          // calculate heuristics
          if (NodeFlags and 2 = 0) then
          begin
            // simple, diagonal, diagonalex
            if (X >= Y) then
            begin
              ChildNode.SortValue := {$ifdef CPUX86}ChildNode.{$endif}Path +
                 Cardinal(Y * Store.Info.HeuristicsDiagonal + (X - Y) * Store.Info.HeuristicsLine);
              ChildNode.NodeInfo := ChildNode.NodeInfo or SIMPLE_DIAGONAL_WAY_BITS[NativeInt((NodeFlags shr 1) and 127) + ((X xor Y) and 1)];
              NodeFlags := NodeFlags and TEMP_WAY_CLEAR_MASK;
            end else
            begin
              ChildNode.SortValue := {$ifdef CPUX86}ChildNode.{$endif}Path +
                 Cardinal(X * Store.Info.HeuristicsDiagonal + (Y - X) * Store.Info.HeuristicsLine);
              ChildNode.NodeInfo := ChildNode.NodeInfo or SIMPLE_DIAGONAL_DXSMALLER_WAY_BITS[NativeInt((NodeFlags shr 1) and 127) + ((X xor Y) and 1)];
              NodeFlags := NodeFlags and TEMP_WAY_CLEAR_MASK;
            end;
          end else
          begin
            // hexagonal
            ChildNode.NodeInfo := ChildNode.NodeInfo or HEXAGONAL_WAY_BITS[
              NativeInt(NodeFlags and ((31 shl 3) or 1)) +
              (((X - Y) shr (HIGH_NATIVE_BIT - 2)) and 4) +
              ((Y and 1) shl 1)
            ];
            X := X - ((Mask xor NativeInt(NodeFlags)) and Y and 1) - (Y shr 1);
            ChildNode.SortValue := {$ifdef CPUX86}ChildNode.{$endif}Path +
               Cardinal(Store.Info.HeuristicsLine * (Y + (X and ((X shr HIGH_NATIVE_BIT) - 1))));
            NodeFlags := NodeFlags and TEMP_WAY_CLEAR_MASK;
          end;
        end;
      end else
      begin
        {$ifdef LARGEINT}
          NodeBuffers := Pointer(@Store.Info.NodeAllocator.Buffers);
          ChildNode := Pointer(
            (NodeBuffers[ChildNodeInfo shr LARGE_NODEPTR_OFFSET]) +
            (ChildNodeInfo and NODEPTR_CLEAN_MASK) );
        {$else}
          ChildNode := Pointer(ChildNodeInfo and NODEPTR_CLEAN_MASK);
        {$endif}

        // child node info (with new parent bits)
        ChildNodeInfo := ChildNode.NodeInfo;
        ParentBits := ParentBits + (ChildNodeInfo and PARENT_BITS_CLEAR_MASK);
        if (ChildNodeInfo and $ff00 = 0) then goto nextchild_continue;

        // child locked/excluded test
        if (ChildNodeInfo and $ff0000 = 0) then goto nextchild_continue;

        // child path
        Cell{TileWeights} := Store.CardinalsDiagonal;
        Inc(NativeInt(Cell{TileWeights}), (ParentBits and 1) shl 10);
        Path := PCardinalList(Cell{TileWeights})[NodeFlags shr 24] +
                PCardinalList(Cell{TileWeights})[ParentBits shr 24];
        Path := (Path shr 1) + Store.Current.Path;

        // continue if the path is shorter
        NodeXY{ChildPath} := ChildNode.Path;
        if (Path >= NodeXY{ChildPath}) then goto nextchild_continue;

        // new parameters
        ChildNode.NodeInfo := ParentBits;
        ChildXY{ChildSortValue} := ChildNode.SortValue;
        ChildNode.Path := Path;
        ChildNode.SortValue := Path + {heuristics}(ChildXY{ChildSortValue} - NodeXY{ChildPath});

        // remove from opened list
        if (ChildXY{ChildSortValue} < NATTANABLE_LENGTH_LIMIT) then
        begin
          Node{Left} := ChildNode.Prev;
          Right := ChildNode.Next;
          Node{Left}.Next := Right;
          Right.Prev := Node{Left};

          if (ChildNode = Store.Top.Node) then
          begin
            Store.Top.Node := Right;
            Store.Top.SortValue := Right.SortValue;
          end;
        end;
      end;

      // add child node to buffer
      {$ifdef CPUX86}
      Buffer := @Store.Buffer;
      {$endif}
      Buffer[(NodeFlags shr COUNTER_OFFSET) and 7] := ChildNode;
      Inc(NodeFlags, (1 shl COUNTER_OFFSET));

      // goto nextchild_continue;
      if (NodeFlags and $ff00 = 0) then Break;
    until (False);

    // move buffered nodes to opened list
    if (NodeFlags and ($f shl COUNTER_OFFSET) = 0) then
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
    {$ifdef CPUX86}
    Buffer := @Store.Buffer;
    {$endif}
    PBufferHigh := @Buffer[(NodeFlags shr COUNTER_OFFSET) and $f];
    PBufferBase := @Store.Buffer[1];
    PBufferCurrent := @Store.Buffer[0];
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
      {$ifNdef CPUX86}
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
      {$endif}
    until (PBufferCurrent = PBufferHigh);

    // next opened node
  next_current:
    Node := Store.Current.Node.Next;
  current_initialize:
    // cell
    NodeFlags{XY} := Cardinal(Node.Coordinates);
    Cardinal(Store.Current.Coordinates) := NodeFlags{XY};
    Cell := @Store.Info.CellArray[(NativeInt(NodeFlags) shr 16){X} + Store.Info.MapWidth * {Y}Word(NodeFlags)];
    Store.Current.Cell := Cell;

    // store pointer and path
    Store.Current.Node := Node;
    Store.Current.Path := Node.Path;
    ChildSortValue := Node.SortValue;
    Store.Current.SortValue := ChildSortValue;

    // node info
    NodeFlags := Cardinal(Node.NodeInfo);
    if (NodeFlags and FLAG_KNOWN_PATH <> 0) then Break;
  until (False);

  // Result
  Result := Node;

  // actualize node allocator (new node pointer)
  TTileMapPtr(Store.Self).FInfo.NodeAllocator.NewNode := Store.Info.NodeAllocator.NewNode;
end;

function TTileMap.DoFindPath(const ParamsPtr: NativeUInt{high bit is FullPath}): TTileMapPath;
const
  FLAG_CACHING = (1 shl 0);
  FLAG_FINISH = (1 shl 1);
  FLAG_CLEAN = (1 shl 2);
  FLAG_TILEWEIGHTS = (1 shl 3);
  FLAG_HEURISTICS = (1 shl 4);
  FLAG_EXCLUDES = (1 shl 5);
  FLAG_STARTS = (1 shl 6);

  FLAGS_HEURISTED_POOL = FLAG_CACHING or FLAG_FINISH or FLAG_HEURISTICS;
  FLAGS_UNATTAINABLE_POOL = FLAG_CACHING or FLAG_FINISH or FLAG_TILEWEIGHTS or FLAG_EXCLUDES;
  // FLAGS_HOT_POOL = FLAGS_HEURISTED_POOL / FLAGS_UNATTAINABLE_POOL;
  FLAGS_EXCLUDED_POOL = FLAG_EXCLUDES or FLAGS_HEURISTED_POOL or FLAGS_UNATTAINABLE_POOL;
  FLAGS_ATTAINABLE_NODES = FLAG_CACHING or FLAG_FINISH or FLAG_TILEWEIGHTS or FLAG_EXCLUDES;

  CHANGED_FINISH_INTERVAL = 8;
label
  weights_flags, nodes_initialized, path_found, path_not_found,
  change_beststart, next_start, fill_result;
var
  i: NativeUInt;
  MapWidth, MapHeight: Cardinal;
  Cell: PCPFCell;
  CellInfo, NodeInfo: NativeUInt;
  FinishX, FinishY: Integer;
  S: PPoint;
  Flags: NativeUInt;

  Params: TTileMapParams;
  Store: record
    FullPath: Boolean;
    FinishSector: Byte;
    AttainableAlgorithm: Boolean;
    HLine, HDiagonal: NativeInt;
  end;
  ResultPath: ^TTileMapPath;

  FoundPaths: NativeUInt;
  StartPoint, BestStartPoint: PCPFStart;
  Node, N: PCPFNode;
  FinishNode: PCPFNode;

  {$ifdef LARGEINT}
    NodeBuffers: PCPFNodeBuffers;
  {$endif}
  {$ifNdef CPUX86}
    _Self: Pointer;
  {$endif}
begin
  // stack copy parameters to one register save
  Flags := ParamsPtr;
  Store.FullPath := Boolean(Flags shr HIGH_NATIVE_BIT);
  Params := PTileMapParams(Flags and ((NativeUInt(1) shl HIGH_NATIVE_BIT) - 1))^;

{$ifNdef CPUX86}
  _Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});
  {$ifNdef CPUX86}with TTileMapPtr(_Self){$ifdef CPFLIB}^{$endif} do{$endif}
begin
{$endif}
  // test start points coordinates
  MapWidth := Width;
  MapHeight := Height;
  S := Params.Starts;
  for i := 1 to Params.StartsCount do
  begin
    if (Cardinal(S.X) >= MapWidth) or (Cardinal(S.Y) >= MapHeight) then
    begin
      RaiseCoordinates(S.X, S.Y, 'start');
      Exit;
    end;

    Inc(S);
  end;

  // test finish point coordinates
  FinishX := Params.Finish.X;
  {$ifNdef CPUX86}
  FinishY := Params.Finish.Y;
  {$endif}
  if (Cardinal(FinishX) >= MapWidth) or (Cardinal({$ifdef CPUX86}Params.Finish.Y{$else}FinishY{$endif}) >= MapHeight) then
  begin
    RaiseCoordinates(Params.Finish.X, Params.Finish.Y, 'finish');
    Exit;
  end;

  // pathless finish tile
  Cell := @FInfo.CellArray[Integer(MapWidth) * {$ifdef CPUX86}Params.Finish.Y{$else}FinishY{$endif} + FinishX];
  CellInfo := Cell.NodePtr;
  if (CellInfo and NODEPTR_FLAG_ALLOCATED <> 0) then
  begin
    {$ifdef LARGEINT}
    NodeBuffers := Pointer(@TTileMapPtr(_Self).FInfo.NodeAllocator.Buffers);
    {$endif}
    CellInfo := PCPFNode(
              {$ifdef LARGEINT}NodeBuffers[CellInfo shr LARGE_NODEPTR_OFFSET] +{$endif}
              CellInfo and NODEPTR_CLEAN_MASK
               ).NodeInfo;
  end;
  if {$ifdef LARGEINT}
       (CellInfo <= $ffffff)
     {$else}
       (CellInfo and Integer($ff000000){Tile} = 0{TILE_BARIER})
     {$endif} or
    (CellInfo and $ff00{Mask} = 0) or
    ((Params.Weights <> nil) and (Params.Weights.FInfo.Singles[CellInfo shr 24] = 0)) then
      FinishX := -1{finish pathless flag};

  // test excluded points coordinates
  S := Params.Excludes;
  for i := 1 to Params.ExcludesCount do
  begin
    if (Cardinal(S.X) >= MapWidth) or (Cardinal(S.Y) >= MapHeight) then
    begin
      RaiseCoordinates(S.X, S.Y, 'excluded');
      Exit;
    end;

    if (S.X = FinishX) and (S.Y = {$ifdef CPUX86}Params.Finish.Y{$else}FinishY{$endif}) then
      FinishX := -1{finish pathless flag};

    Inc(S);
  end;

  // no start points or finish pathless case
  if (Params.StartsCount = 0) or (FinishX < 0) then
  begin
    ResultPath := @Result;
    ResultPath.Index := 0;
    ResultPath.Points := nil;
    ResultPath.Count := 0;
    ResultPath.Distance := 0;
    Exit;
  end;

  // start is finish case
  S := Params.Starts;
  {$ifdef CPUX86}
  FinishY := Params.Finish.Y;
  {$endif}
  for i := Params.StartsCount downto 1 do
  begin
    if (S.X = FinishX) and (S.Y = FinishY) then
    begin
      FActualInfo.PathlessFinishPoint.X := FinishX;
      FActualInfo.PathlessFinishPoint.Y := FinishY;
      ResultPath := @Result;
      ResultPath.Index := (NativeUInt(S) - NativeUInt(Params.Starts)) shr 3{div SizeOf(TPoint)};
      ResultPath.Points := Pointer(@FActualInfo.PathlessFinishPoint);
      ResultPath.Count := 1;
      ResultPath.Distance := 0;
      Exit;
    end;

    Inc(S);
  end;

  // actualization and flags
  Flags := NativeUInt(FCaching) xor 1;{FLAG_CACHING}
  begin
    // finish point
    if (0 <> Cardinal(FInfo.FinishPoint) - Cardinal(FinishX shl 16 + FinishY)) then
    begin
      Flags := Flags or FLAG_FINISH;

      // allocated nodes
      Inc(FActualInfo.ChangedFinishCount);
      if (FActualInfo.ChangedFinishCount shl (Flags and 1) >= CHANGED_FINISH_INTERVAL) then
      begin
        FActualInfo.ChangedFinishCount := 0;

        if (FInfo.NodeAllocator.Count > 2) then
        begin
          Flags := Flags or FLAG_CLEAN;
          FreeAllocatedNodes(True);
        end else
        if (FNodes.Storage.Count > 2) then
        begin
          FreeStorageTopBuffer;
        end;
      end;
    end;

    // tiles + sectors
    if (FActualInfo.TilesChanged) then
    begin
      Flags := Flags or FLAG_TILEWEIGHTS;
      FActualInfo.TilesChanged := False;
    end;

    // actualize sectors
    if (SectorTest) then
    begin
      if (FActualInfo.Sectors = nil) or (FActualInfo.SectorsChanged) then
        ActualizeSectors;

      Store.FinishSector := PByte(NativeInt(FActualInfo.Sectors) +
        NativeInt(Width) * Params.Finish.Y + Params.Finish.X)^;
    end else
    begin
      if (FActualInfo.Sectors <> nil) then
        CPFFreeMem(Pointer(FActualInfo.Sectors));

      Store.FinishSector := SECTOR_EMPTY;
    end;

    // excluded points
    if (FActualInfo.Excludes.Count or Params.ExcludesCount <> 0) then
    begin
      if (not ActualizeExcludes(Params.Excludes, Params.ExcludesCount, 0 = Flags and FLAG_CLEAN)) then
        Flags := Flags or FLAG_EXCLUDES;
    end;

    // tile weights
    if (Params.Weights = nil) then
    begin
      if (FActualInfo.Weights.Count <> 0) then
      begin
        Store.HLine := FInfo.HeuristicsLine;
        Store.HDiagonal := FInfo.HeuristicsDiagonal;
        if (not ActualizeWeights(nil, 0 = Flags and (FLAG_CACHING or FLAG_FINISH{ or FLAG_CLEAN}))) then goto weights_flags;
      end;
    end else
    if (FActualInfo.Weights.Current <> Params.Weights.FInfo) or
      (FActualInfo.Weights.UpdateId <> Params.Weights.FInfo.UpdateId) then
    begin
      Store.HLine := FInfo.HeuristicsLine;
      Store.HDiagonal := FInfo.HeuristicsDiagonal;

      if (not ActualizeWeights(Params.Weights.FInfo, 0 = Flags and (FLAG_CACHING or FLAG_FINISH{ or FLAG_CLEAN}))) then
      begin
      weights_flags:
        Flags := Flags or FLAG_TILEWEIGHTS;

        if (Store.HLine <> FInfo.HeuristicsLine) or (Store.HDiagonal <> FInfo.HeuristicsDiagonal) then
          Flags := Flags or FLAG_HEURISTICS;
      end;
    end;

    // start points
    if (Params.StartsCount = 1) and (FActualInfo.Starts.Count = 1) then
    begin
      S := Params.Starts;
      FinishX{XY} := (S.Y shl 16) + S.X;

      StartPoint := FActualInfo.Starts.Buffer.Memory;
      if (StartPoint.XY <> FinishX{XY}) then
      begin
        StartPoint.XY := FinishX{XY};
        // StartPoint.XYDistance := any value;
        Flags := Flags or FLAG_STARTS;
      end;
    end else
    begin
      if (not ActualizeStarts(Params{, 0 = Flags})) then
        Flags := Flags or FLAG_STARTS;
    end;
  end;

  // nothing is changed case
  if (Flags = 0) then
  begin
    if (Byte(FActualInfo.FoundPath.FullPath) >= Byte(Store.FullPath)) then goto fill_result
    else
    goto nodes_initialized;
  end;
  if (Flags = FLAG_STARTS{start points changed only}) then goto nodes_initialized;

  // cleanup initialized node pools (contains NODEPTR_FLAG_HEURISTED)
  if (Flags and FLAG_CLEAN = 0) then
  begin
    // attainable points
    if (Flags and FLAGS_ATTAINABLE_NODES <> 0) and (FNodes.AttainableTree) then
      ForgetAttainableTreeNodes;

    // excluded points
    if (Flags and FLAGS_EXCLUDED_POOL <> 0) and
      (FNodes.ExcludedPool.First.Next <> @FNodes.ExcludedPool.Last) then
    begin
      if (Flags and (FLAGS_HEURISTED_POOL and FLAGS_UNATTAINABLE_POOL) <> 0) then
      begin
        ReleasePoolNodes(FNodes.ExcludedPool.First, FNodes.ExcludedPool.Last);
      end else
      begin
        UnlockExcludedNodes;
      end;
    end;

    // heuristed pool (+ excluded + hot)
    if (Flags and FLAGS_HEURISTED_POOL <> 0) then
    begin
      if (FNodes.HeuristedPool.First.Next <> @FNodes.HeuristedPool.Last) then
        ReleasePoolNodes(FNodes.HeuristedPool.First, FNodes.HeuristedPool.Last);

      if (FNodes.HotPool.First.Next <> @FNodes.HotPool.Last) and
        (not FNodes.HotPool.Unattainable) then
        ReleasePoolNodes(FNodes.HotPool.First, FNodes.HotPool.Last);
    end;

    // unattainable pool (+ hot)
    if (Flags and FLAGS_UNATTAINABLE_POOL <> 0) then
    begin
      if (FNodes.UnattainablePool.First.Next <> @FNodes.UnattainablePool.Last) then
        ReleasePoolNodes(FNodes.UnattainablePool.First, FNodes.UnattainablePool.Last);

      if (FNodes.HotPool.First.Next <> @FNodes.HotPool.Last) and
        (FNodes.HotPool.Unattainable) then
        ReleasePoolNodes(FNodes.HotPool.First, FNodes.HotPool.Last);
    end;
  end;

  // new finish point
  FInfo.FinishPoint.X := Params.Finish.X;
  FInfo.FinishPoint.Y := Params.Finish.Y;

  // (re)initialize excluded points
  if (Flags and (FLAG_CLEAN or FLAGS_EXCLUDED_POOL) = 0) then goto nodes_initialized;
  Flags := Params.ExcludesCount;
  S := Params.Excludes;
  if (Flags <> 0) then
  begin
    // hot pool nodes
    if (FNodes.HotPool.First.Next <> @FNodes.HotPool.Last) then
      ReleasePoolNodes(FNodes.HotPool.First, FNodes.HotPool.Last);

    // each excluded point loop
    repeat
      N := AllocateHeuristedNode(S.X, S.Y);
      N.ParentMask := 0{locked flag};

      // remove from heuristed/unattainable/excluded pool
      Node := N.Prev;
      Node.Next := N.Next;
      Node.Next.Prev := Node;

      // add to excluded pool
      Node := FNodes.ExcludedPool.First.Next;
      FNodes.ExcludedPool.First.Next := N;
      N.Prev := @FNodes.ExcludedPool.First;
      N.Next := Node;
      Node.Prev := N;

      Dec(Flags);
      Inc(S);
    until (Flags = 0);
  end;

nodes_initialized:
  // basic path initialization  
  FActualInfo.FoundPath.Index := 0;
  FActualInfo.FoundPath.Length := 0;
  FActualInfo.FoundPath.Distance := 0;
  if (Store.FinishSector = SECTOR_PATHLESS) then goto fill_result;

  // finish point node
  Node := AllocateHeuristedNode(Params.Finish.X, Params.Finish.Y);
  Node.NodeInfo := Node.NodeInfo or {mark anyway}(FLAG_KNOWN_PATH + FLAG_ATTAINABLE);
  FinishNode := Node;

  // each start point find algorithm
  FoundPaths := Params.StartsCount;
  StartPoint := FActualInfo.Starts.Buffer.Memory;
  Store.AttainableAlgorithm := (FoundPaths > 1) or (FCaching);
  for i := Params.StartsCount downto 1 do
  begin
    // sector test
    if (Store.FinishSector <> SECTOR_EMPTY{Self.SectorTest}) then
    begin
      if (Store.FinishSector <> PByte(NativeInt(FActualInfo.Sectors) +
        NativeInt(Width) * StartPoint.Y + StartPoint.X)^) then
        goto path_not_found;
    end;

    // hot nodes
    if (FNodes.HotPool.First.Next <> @FNodes.HotPool.Last) then
      FlushHotPoolNodes;

    // allocate start node
    Node := AllocateHeuristedNode(StartPoint.X, StartPoint.Y);
    NodeInfo := Node.NodeInfo;
    StartPoint.Node := Node;
    if {$ifdef LARGEINT}
       (NodeInfo <= $ffffff)
       {$else}
       (NodeInfo and Integer($ff000000){Tile} = 0{TILE_BARIER})
       {$endif} or
      (NodeInfo and $ff00{Mask} = 0) or
      (NodeInfo and Integer($00ff0000) = 0{excluded test}) or
      (FActualInfo.Weights.CardinalsLine[NodeInfo shr 24] = PATHLESS_TILE_WEIGHT) then goto path_not_found;
    if (NodeInfo and FLAG_KNOWN_PATH <> 0) then
    begin
      StartPoint.KnownPathNode := Node;
      if (NodeInfo and FLAG_ATTAINABLE <> 0) then
      begin
        goto path_found;
      end else
      begin
        goto path_not_found;
      end;
    end;

    // remove from heuristed pool
    N := Node.Prev;
    N.Next := Node.Next;
    N.Next.Prev := N;

    // add to hot pool
    FNodes.HotPool.First.Next := Node;
    FNodes.HotPool.Last.Prev := Node;
    Node.Prev := @FNodes.HotPool.First;
    Node.Next := @FNodes.HotPool.Last;

    // zero path
    Node.SortValue := Node.SortValue - Node.Path;
    Node.Path := 0;

    // call find path loop
    StartPoint.KnownPathNode := DoFindPathLoop(Node);
    if (StartPoint.KnownPathNode.NodeInfo and FLAG_ATTAINABLE <> 0) then
    begin
      FNodes.HotPool.Unattainable := False;
    
    path_found:
      if (Store.AttainableAlgorithm) then
      begin
        CacheAttainablePath(StartPoint^, FinishNode);
      end else
      begin
        FActualInfo.FoundPath.FullPath := True;
        FillStandardPath(StartPoint.Node, FinishNode);
        goto fill_result;
      end;
    end else
    begin
      FNodes.HotPool.Unattainable := True;
    
    path_not_found:
      StartPoint.DistanceAsInt64 := $7fefffffffffffff{MaxDouble};
      Dec(FoundPaths);
    end;

    Inc(StartPoint);
  end;

  // find the best start point (attainable algorithm)
  // distance and distanceXY
  if (FoundPaths = 0) then goto fill_result;
  StartPoint := FActualInfo.Starts.Buffer.Memory;
  BestStartPoint := StartPoint;
  Inc(StartPoint);
  for i := Params.StartsCount downto 2 do
  begin
    {$ifdef LARGEINT}
      NodeInfo := StartPoint.DistanceAsInt64;
      Dec(NodeInfo, BestStartPoint.DistanceAsInt64);
      if (NativeInt(NodeInfo) < 0) then goto change_beststart;
      if (NativeInt(NodeInfo) > 0) then goto next_start;
      if (StartPoint.XYDistance >= BestStartPoint.XYDistance) then goto next_start;
    {$else}
      FinishX := StartPoint.DistanceHighDword - BestStartPoint.DistanceHighDword;
      if (FinishX < 0) then goto change_beststart;
      if (FinishX > 0) then goto next_start;
      FinishX := StartPoint.DistanceLowDword;
      if (Cardinal(FinishX) < BestStartPoint.DistanceLowDword) then goto change_beststart;
      if (Cardinal(FinishX) > BestStartPoint.DistanceLowDword) then goto next_start;
      if (StartPoint.XYDistance >= BestStartPoint.XYDistance) then goto next_start;
    {$endif}
  change_beststart:
     BestStartPoint := StartPoint;

  next_start:
    Inc(StartPoint);
  end;

  // fill best start point parameters and path (attainable algorithm)
  FActualInfo.FoundPath.Index := (NativeUInt(BestStartPoint) - NativeUInt(FActualInfo.Starts.Buffer.Memory)) shr 5;// div SizeOf(TCPFStart);
  FActualInfo.FoundPath.Distance := BestStartPoint.Distance;
  FActualInfo.FoundPath.Length := BestStartPoint.Length;
  if (Store.FullPath) then
  begin
    FActualInfo.FoundPath.FullPath := True;
    // Alloc(Length * SizeOf(TPoint))
    NodeInfo := FActualInfo.FoundPath.Length * SizeOf(TPoint);
    if (NodeInfo > FActualInfo.FoundPath.Buffer.FAllocatedSize) then FActualInfo.FoundPath.Buffer.Realloc(NodeInfo);
    FillAttainablePath(BestStartPoint.Node, FinishNode);
  end else
  begin
    FActualInfo.FoundPath.FullPath := False;
    // Alloc(2 * SizeOf(TPoint))
    NodeInfo := 2 * SizeOf(TPoint);
    if (NodeInfo > FActualInfo.FoundPath.Buffer.FAllocatedSize) then FActualInfo.FoundPath.Buffer.Realloc(NodeInfo);

    S := FActualInfo.FoundPath.Buffer.Memory;
    S.X := BestStartPoint.X;
    S.Y := BestStartPoint.Y;
    Inc(S);

    NodeInfo := BestStartPoint.Node.NodeInfo;
    NodeInfo{Child} := (NodeInfo shr 4) and 7;
    S.X := BestStartPoint.X + POINT_OFFSETS[NodeInfo].x;
    S.Y := BestStartPoint.Y + POINT_OFFSETS[NodeInfo].y;
  end;

  // fill found path result
fill_result:
  ResultPath := @Result;
  ResultPath.Index := FActualInfo.FoundPath.Index;
  ResultPath.Points := FActualInfo.FoundPath.Buffer.Memory;
  ResultPath.Count := FActualInfo.FoundPath.Length;
  ResultPath.Distance := FActualInfo.FoundPath.Distance;
  if (ResultPath.Count = 0) then
    ResultPath.Points := nil;
{$ifNdef CPUX86}
end;
{$endif}
end;

function TTileMap.FindPath(const Params: TTileMapParams; const FullPath: Boolean): TTileMapPath;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  Result := DoFindPath(NativeUInt(@Params) + NativeUInt(FullPath) shl HIGH_NATIVE_BIT);
end;

function TTileMap.FindPath(const Start, Finish: TPoint;
  const Weights: TTileMapWeightsPtr; const Excludes: PPoint;
  const ExcludesCount: NativeUInt; const FullPath: Boolean): TTileMapPath;
var
  Params: TTileMapParams;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  Params.Starts := @Start;
  Params.StartsCount := 1;
  Params.Finish := Finish;
  Params.Weights := Weights;
  Params.Excludes := Excludes;
  Params.ExcludesCount := ExcludesCount;

  Result := DoFindPath(NativeUInt(@Params) + NativeUInt(FullPath) shl HIGH_NATIVE_BIT);
end;

function TTileMap.FindPath(const Starts: PPoint; const StartsCount: NativeUInt;
   const Finish: TPoint; const Weights: TTileMapWeightsPtr;
   const Excludes: PPoint; const ExcludesCount: NativeUInt;
   const FullPath: Boolean): TTileMapPath;
var
  Params: TTileMapParams;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  Params.Starts := Starts;
  Params.StartsCount := StartsCount;
  Params.Finish := Finish;
  Params.Weights := Weights;
  Params.Excludes := Excludes;
  Params.ExcludesCount := ExcludesCount;

  Result := DoFindPath(NativeUInt(@Params) + NativeUInt(FullPath) shl HIGH_NATIVE_BIT);
end;

initialization
  {$ifdef CPF_GENERATE_LOOKUPS}
    GenerateLookups;
    Halt;
  {$endif}
  {$ifdef CPFLOG}
    CpfLogFile := TFileStream.Create('CpfLog.txt', fmCreate);
    CpfLogFile.Write(BOM_UTF8, SizeOf(BOM_UTF8));
  {$endif}
  {$ifNdef CPFLIB}
    {$WARNINGS OFF} // deprecated warning bug fix (like Delphi 2010 compiler)
    System.GetMemoryManager(MemoryManager);
  {$endif}


finalization
  {$ifdef CPFLOG}
    CpfLogFile.Free;
  {$endif}

end.
