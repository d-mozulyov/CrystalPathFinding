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
{.$define CPFLOG}

{$ifdef CPFLIB}
  {$define CPFAPI}  
  {$undef CPFLOG}
{$endif}

// compiler directives
{$ifdef FPC}
  {$mode Delphi}
  {$asmmode Intel}
  {$define INLINESUPPORT}
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
  {$undef CPFLOG}
  {$undef CPFAPI}
  {$undef CPFLIB}
{$endif}

interface
  uses Types
       {$ifNdef CPFLIB}
         {$ifdef KOL}
           , KOL, err
         {$else}
           , SysUtils
           {$if Defined(CPFLOG) or Defined(CPF_GENERATE_LOOKUPS)}, Classes{$ifend}
         {$endif}
       {$endif};

type
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
  TPathMapTile = type Byte;
  PPathMapTile = ^TPathMapTile;

  // map kind
  TPathMapKind = (mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal);
  PPathMapKind = ^TPathMapKind;

  // points as array
  PPointList = ^TPointList;
  TPointList = array[0..High(Integer) div SizeOf(TPoint) - 1] of TPoint;

  // result of find path function
  TPathMapResult = record
    Points: PPointList;
    Count: Cardinal;
    Distance: Double;
  end;
  PPathMapResult = ^TPathMapResult;

  // internal class
  TExceptionString = {$ifdef CPFLIB}PWideChar{$else}string{$endif};
  TCPFClass = {$ifdef CPFLIB}object{$else}class(TObject){$endif}
  protected
    FCallAddress: Pointer;

    procedure CPFException(const Message: TExceptionString);
    procedure CPFExceptionFmt(const Fmt: PWideChar; const Args: array of {$ifdef CPFLIB}Integer{$else}const{$endif});
    function CPFAlloc(const Size: NativeUInt): Pointer;
    procedure CPFFree(const P: Pointer);
    function CPFRealloc(const P: Pointer; const Size: NativeUInt): Pointer;
  end;
  TCPFClassPtr = {$ifdef CPFLIB}^{$endif}TCPFClass;

  // map weights
  TPathMapWeights = {$ifdef CPFLIB}object{$else}class{$endif}(TCPFClass)
  private
    FReferenceCount: Integer;
    FHighTile: TPathMapTile;

    function GetValue(const Tile: TPathMapTile): Single;
    procedure SetValue(const Tile: TPathMapTile; const Value: Single);
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

    property HighTile: TPathMapTile read FHighTile;
    property Values[const Tile: TPathMapTile]: Single read GetValue write SetValue; default;
  end;
  TPathMapWeightsPtr = {$ifdef CPFLIB}^{$endif}TPathMapWeights;

  // compact cell coordinates
  PCPFPoint = ^TCPFPoint;
  TCPFPoint = packed record
    Y: Word;
    X: Word;
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
    SortValue: Cardinal; // path + heuristics to start point
    Path: Cardinal; // path from start point to the cell
    Coordinates: TCPFPoint;
    Prev, Next: PPathMapNode;
    case Integer of
    0: (
         ParentAndFlags: Byte
         {
           Parent:3;
           case Boolean of
             False: (Way:3; ParentDependent:1);
              True: (Child:3; Attainable:1);
           end;
           CachedPath:1;
         };
         Mask: Byte;
         ParentMask: Byte;
         Tile: Byte;
       );
    1: (NodeInfo: Cardinal);
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

  PCardinalList = ^TCardinalList;
  TCardinalList = array[0..High(Integer) div SizeOf(Cardinal) - 1] of Cardinal;

  TPathMapInfo = record
    Cells: PPathMapCellArray;
    MapWidth: NativeInt;
    HeuristicsLine: NativeInt;
    HeuristicsDiagonal: NativeInt;
    TileWeights: array[0..1] of PCardinalList;
    CellOffsets: array[0..7] of NativeInt;

    //Nodes

    NodeStorage: TPathMapNodeStorage;

    StartPoint: TCPFPoint;
    FinishPoint: TCPFPoint;
  end;

  // main path finding class
  TPathMap = {$ifdef CPFLIB}object{$else}class{$endif}(TCPFClass)
  private
    FFindResult: TPathMapResult;
    FInfo: TPathMapInfo;
    FWidth: Word;
    FHeight: Word;
    FKind: TPathMapKind;
    FHighTile: TPathMapTile;
    FSectorTest: Boolean;
    FCaching: Boolean;

    function GetTile(const X, Y: Word): TPathMapTile;
    procedure SetTile(const X, Y: Word; const Value: TPathMapTile);
    procedure SetSectorTest(const Value: Boolean);
    procedure SectorTestChanged;
    procedure SetCaching(const Value: Boolean);
    procedure CachingChanged;
    function AllocateNode(const X, Y: NativeInt): PPathMapNode;
    function DoFindPath(MapSelf: Pointer; StartNode: PPathMapNode): PPathMapNode;
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
    procedure Clear();
    procedure Update(const ATiles: PPathMapTile; const X, Y, AWidth, AHeight: Word; const Pitch: NativeInt = 0);

    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property Kind: TPathMapKind read FKind;
    property HighTile: TPathMapTile read FHighTile;
    property SectorTest: Boolean read FSectorTest write SetSectorTest;
    property Caching: Boolean read FCaching write SetCaching;
    property Tiles[const X, Y: Word]: TPathMapTile read GetTile write SetTile; default;

    function FindPath(const Start, Finish: TPoint; const Weights: TPathMapWeightsPtr = nil;
      const ExcludedPoints: PPoint = nil; const ExcludedPointsCount: NativeUInt = 0): PPathMapResult;
  end;
  TPathMapPtr = {$ifdef CPFLIB}^{$endif}TPathMap;

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
  procedure cpfInitialize(const Callbacks: TCPFCallbacks); cdecl;
  {$endif}

  function  cpfCreateWeights(HighTile: TPathMapTile): TCPFHandle; cdecl;
  procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl;
  function  cpfWeightGet(HWeights: TCPFHandle; Tile: TPathMapTile): Single; cdecl;
  procedure cpfWeightSet(HWeights: TCPFHandle; Tile: TPathMapTile; Value: Single); cdecl;
  function  cpfCreateMap(Width, Height: Word; Kind: TPathMapKind = mkSimple; HighTile: TPathMapTile = 0): TCPFHandle; cdecl;
  procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl;
  procedure cpfMapClear(HMap: TCPFHandle); cdecl;
  procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PPathMapTile; X, Y, Width, Height: Word; Pitch: NativeInt = 0); cdecl;
  function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): TPathMapTile; cdecl;
  procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: TPathMapTile); cdecl;
  function  cpfFindPath(HMap: TCPFHandle; Start, Finish: TPoint; HWeights: TCPFHandle = 0; ExcludedPoints: PPoint = nil; ExcludedPointsCount: NativeUInt = 0; SectorTest: Boolean = True; Caching: Boolean = True): PPathMapResult; cdecl;
{$endif}

implementation
{$ifNdef CPFLIB}
  {$ifNdef KOL}uses SysConst{$endif};

var
  MemoryManager: {$if Defined(FPC) or (CompilerVersion < 18)}TMemoryManager{$else}TMemoryManagerEx{$ifend};
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


{$if Defined(FPC) or (CompilerVersion < 23)}
// todo PFC
function ReturnAddress: Pointer;
asm
  mov eax, [ebp+4]
end;
{$ifend}

procedure ZeroMemory(Destination: Pointer; Length: NativeUInt);
{$ifdef CPFLIB}
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
    Cardinal(P)^ := 0;

    Dec(Length, SizeOf(Cardinal));
    Inc(P, SizeOf(Cardinal));
  end;
  {$endif}

  case Length of
    3:
    begin
      PWord(P)^ := 0;
      Inc(P, SizeOf(Word));
      P^ := 0;
    end;
    2: PWord(P)^ := 0;
    1: P^ := 0;
  end;
end;
{$else}
  {$ifdef INLINESUPPORT}inline;{$endif}
begin
  FillChar(Destination^, Length, 0);
end;
{$endif}


{$ifdef CPFLIB}
var
  CPFCallbacks: TCPFCallbacks;
{$endif}

procedure CPFException(const Message: TExceptionString; const Address: Pointer);
begin
  {$ifdef CPFLIB}
    if Assigned(CPFCallbacks.Exception) then
      CPFCallbacks.Exception(Message, Address);

     // guaranteed halt (Assert)
     System.ErrorAddr := Address;
     if (System.ExitCode = 0) then System.ExitCode := 207{reInvalidOp};
     System.Halt;
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
  System.ExitCode := 203{reOutOfMemory};
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
  System.ExitCode := 204{reInvalidPtr};
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

function  cpfCreateWeights(HighTile: TPathMapTile): TCPFHandle; cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  Result := NewCPFClassInstance({$ifdef CPFLIB}SizeOf{$endif}(TPathMapWeights), Address);
  TPathMapWeightsPtr(Result).Create(HighTile);
end;

procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl;
var
  Address: Pointer;
  Weights: Pointer;
begin
  Address := ReturnAddress;
  Weights := Pointer(HWeights);
  HWeights := 0;

  if (Weights <> nil) then
  begin
    TPathMapWeightsPtr(Weights).FCallAddress := Address;
    {$if Defined(AUTOREFCOUNT) and not Defined(CPFLIB)}
      TPathMapWeightsPtr(Weights).__ObjRelease;
    {$else}
      TPathMapWeightsPtr(Weights).Destroy;
      {$ifdef CPFLIB}
        CPFFree(Weights, Address);
      {$endif}
    {$ifend}
  end;
end;

function  cpfWeightGet(HWeights: TCPFHandle; Tile: TPathMapTile): Single; cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HWeights <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HWeights).FCallAddress := Address;

  Result := TPathMapWeightsPtr(HWeights).Values[Tile];
end;

procedure cpfWeightSet(HWeights: TCPFHandle; Tile: TPathMapTile; Value: Single); cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HWeights <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HWeights).FCallAddress := Address;

  TPathMapWeightsPtr(HWeights).Values[Tile] := Value;
end;

function  cpfCreateMap(Width, Height: Word; Kind: TPathMapKind = mkSimple; HighTile: TPathMapTile = 0): TCPFHandle; cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  Result := NewCPFClassInstance({$ifdef CPFLIB}SizeOf{$endif}(TPathMap), Address);
  TPathMapPtr(Result).Create(Width, Height, Kind, HighTile);
end;

procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl;
var
  Address: Pointer;
  Map: Pointer;
begin
  Address := ReturnAddress;
  Map := Pointer(HMap);
  HMap := 0;

  if (Map <> nil) then
  begin
    TPathMapWeightsPtr(Map).FCallAddress := Address;
    {$if Defined(AUTOREFCOUNT) and not Defined(CPFLIB)}
      TPathMapWeightsPtr(Map).__ObjRelease;
    {$else}
      TPathMapWeightsPtr(Map).Destroy;
      {$ifdef CPFLIB}
        CPFFree(Map, Address);
      {$endif}
    {$ifend}
  end;
end;

procedure cpfMapClear(HMap: TCPFHandle); cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  TPathMapPtr(HMap).Clear;
end;

procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PPathMapTile; X, Y, Width, Height: Word; Pitch: NativeInt = 0); cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  TPathMapPtr(HMap).Update(Tiles, X, Y, Width, Height, Pitch);
end;

function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): TPathMapTile; cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  Result := TPathMapPtr(HMap).Tiles[X, Y];
end;

procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: TPathMapTile); cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;

  TPathMapPtr(HMap).Tiles[X, Y] := Value;
end;

function  cpfFindPath(HMap: TCPFHandle; Start, Finish: TPoint; HWeights: TCPFHandle = 0; ExcludedPoints: PPoint = nil; ExcludedPointsCount: NativeUInt = 0; SectorTest: Boolean = True; Caching: Boolean = True): PPathMapResult; cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);
  TCPFClassPtr(HMap).FCallAddress := Address;
  if (HWeights <> 0) then
  begin
    if (HWeights <= $ffff) then RaiseInvalidPointer(Address);
    TCPFClassPtr(HWeights).FCallAddress := Address;
  end;

  if (TPathMapPtr(HMap).SectorTest <> SectorTest) then
    TPathMapPtr(HMap).SectorTestChanged;

  if (TPathMapPtr(HMap).Caching <> Caching) then
    TPathMapPtr(HMap).CachingChanged;

  Result := TPathMapPtr(HMap).FindPath(Start, Finish, TPathMapWeightsPtr(HWeights),
    ExcludedPoints, ExcludedPointsCount);
end;
{$endif .CPFAPI}


{ TCPFClass }

procedure TCPFClass.CPFException(const Message: TExceptionString);
begin
  CrystalPathFinding.CPFException(Message, FCallAddress);
end;

procedure TCPFClass.CPFExceptionFmt(const Fmt: PWideChar;
  const Args: array of {$ifdef CPFLIB}Integer{$else}const{$endif});
begin
  CrystalPathFinding.CPFExceptionFmt(Fmt, Args, FCallAddress);
end;

function TCPFClass.CPFAlloc(const Size: NativeUInt): Pointer;
begin
  Result := CrystalPathFinding.CPFAlloc(Size, FCallAddress);
end;

procedure TCPFClass.CPFFree(const P: Pointer);
begin
  CrystalPathFinding.CPFFree(P, FCallAddress);
end;

function TCPFClass.CPFRealloc(const P: Pointer;
  const Size: NativeUInt): Pointer;
begin
  Result := CrystalPathFinding.CPFRealloc(P, Size, FCallAddress);
end;


type
  TYXSmallPoint = record
    y: SmallInt;
    x: SmallInt;
  end;

  TChildArray = array[0..7] of Word;
  PChildArray = ^TChildArray;

const
  _0 = (1 shl 0);
  _1 = (1 shl 1);
  _2 = (1 shl 2);
  _3 = (1 shl 3);
  _4 = (1 shl 4);
  _5 = (1 shl 5);
  _6 = (1 shl 6);
  _7 = (1 shl 7);

  UNSUPPORTED_TILE_WEIGHT = High(Cardinal) shr 1;

  FLAG_ATTAINABLE = 1 shl 6;
  FLAG_KNOWN_PATH = 1 shl 7;

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

  CHILD_ARRAYS: array[0..15{Parent*2 + FLAG_CLOCKWISE}] of TChildArray = (
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000),
    ($0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000)
  );

  CHILD_ARRAYS_OFFSETS: array[0..127{parent:3,way:3,dependent:1}] of Byte = (
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  );

  PARENT_BITS: array[0..{Delta9*}{Hexagonal}4*{Child}8 - 1] of NativeUInt = (
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
  );

  WAY_BITS: array[0..8] of NativeUInt{Byte} = (
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
  );


{$ifdef CPF_GENERATE_LOOKUPS}
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

procedure GenerateLookups;
begin
  LookupsText := TStringList.Create;
  try
    LookupsText.Add('const');


    LookupsText.SaveToFile('Lookup.txt');
  finally
    LookupsText.Free;
  end;
end;
{$endif}

{ TPathMapWeights }

{$ifdef CPFLIB}procedure{$else}constructor{$endif}
  TPathMapWeights.Create(const AHighTile: TPathMapTile);
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
    inherited Create;
  {$endif}


end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TPathMapWeights.Destroy;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  if (FReferenceCount <> 0) then
    CPFExceptionFmt('The weights are used by %d maps', [FReferenceCount]);

  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;


function TPathMapWeights.GetValue(const Tile: TPathMapTile): Single;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  Result := 0;
end;

procedure TPathMapWeights.SetValue(const Tile: TPathMapTile;
  const Value: Single);
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}


end;


{ TPathMap }

{$ifdef CPFLIB}procedure{$else}constructor{$endif}
  TPathMap.Create(const AWidth, AHeight: Word; const AKind: TPathMapKind;
    const AHighTile: TPathMapTile);
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
    inherited Create;
  {$endif}


end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TPathMap.Destroy;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}


  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;

procedure TPathMap.Clear;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}


end;

function TPathMap.GetTile(const X, Y: Word): TPathMapTile;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}


  Result := 0;
end;

procedure TPathMap.SetTile(const X, Y: Word; const Value: TPathMapTile);
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

end;

procedure TPathMap.Update(const ATiles: PPathMapTile; const X, Y, AWidth,
  AHeight: Word; const Pitch: NativeInt);
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

end;

procedure TPathMap.SetSectorTest(const Value: Boolean);
begin
  if (FSectorTest <> Value) then
  begin
    {$ifNdef CPFLIB}
      FCallAddress := ReturnAddress;
    {$endif}

    SectorTestChanged;
  end;
end;

procedure TPathMap.SectorTestChanged;
begin
  FSectorTest := not FSectorTest;
  // todo
end;

procedure TPathMap.SetCaching(const Value: Boolean);
begin
  if (FCaching <> Value) then
  begin
    {$ifNdef CPFLIB}
      FCallAddress := ReturnAddress;
    {$endif}

    CachingChanged;
  end;
end;

procedure TPathMap.CachingChanged;
begin
  FCaching := not FCaching;
  // todo
end;

procedure CopyInfo(var Dest, Src: TPathMapInfo);
begin
  Dest := Src;
end;

function TPathMap.DoFindPath(MapSelf: Pointer; StartNode: PPathMapNode): PPathMapNode;
label
  nextchild_continue, nextchild, child_tobuffer, next_current, current_initialize;
const
  DELTA_CLEAR_MASK = not Cardinal(((1 shl 4) - 1) shl 3);
  COUNTER_OFFSET = 16;
type
  TMapNodeBuffer = array[0..7] of PPathMapNode;
var
  Node: PPathMapNode;
  NodeInfo: NativeUInt;
  ChildList: PWord;
  Child: NativeUInt;
  Cell: PPathMapCell;
  ChildNodeInfo: NativeUInt;
  ChildNodePtr: NativeUInt;
  ChildNode: PPathMapNode;
  TileWeights: PCardinalList;
  Path: Cardinal;
  NodeXY, OffsetXY, ChildXY: Cardinal;

  Store: record
    Buffer: TMapNodeBuffer;
    Self: Pointer;
    HexagonalFlag: NativeUInt;
    Info: TPathMapInfo;

    {$ifdef CPUX86}
    ChildList: PWord;
    {$endif}

    Current: record
      Node: PPathMapNode;
      Cell: PPathMapCell;
      Coordinates: TCPFPoint;
      SortValue: Cardinal;
      Path: Cardinal;
    end;
    Top: record
      Node: PPathMapNode;
      SortValue: Cardinal;
    end;
  end;

  {$ifNdef CPUX86}
    Buffer: ^TMapNodeBuffer;
  {$endif}
begin
  Store.Self := MapSelf;
  Store.HexagonalFlag := NativeUInt(TPathMapPtr(MapSelf).FKind = mkHexagonal) * 2;
  CopyInfo(Store.Info, TPathMapPtr(MapSelf).FInfo);

  {$ifNdef CPUX86}
  Buffer := @Store.Buffer;
  {$endif}

  // Top initialization
  Node := StartNode.Next;
  Store.Top.Node := Node;
  Node.Prev := StartNode;
  Store.Top.SortValue := UNSUPPORTED_TILE_WEIGHT;

  // finding loop from Start to Finish
  Node := StartNode;
  goto current_initialize;
  repeat
    // lock
    Node.ParentMask := 0;

    // todo next top?

    // child list
    ChildList := Pointer(NativeUInt(CHILD_ARRAYS_OFFSETS[NodeInfo and 127]));
    Inc(NativeUInt(ChildList), NativeUInt(@CHILD_ARRAYS));

    // reinitialize NodeInfo (from parentflags, mask, parentmask, tile):
    //   - bit hexagonal << 1
    //   - mask
    //   - stored childs counter
    //   - tile
    NodeInfo := ((NodeInfo and (NodeInfo shr 8)) and Integer($ff00ff00)) or Store.HexagonalFlag;

    // each child cell loop
    goto nextchild;
    nextchild_continue:
    if (NodeInfo and $ff00 <> 0) then
    repeat
      // first available
      {$ifdef CPUX86}
      ChildList := Store.ChildList;
      {$endif}
      nextchild:
      Child := ChildList^;
      Inc(ChildList);
      if (NodeInfo and Child = 0) then goto nextchild;
      {$ifdef CPUX86}
      Store.ChildList := ChildList;
      {$endif}

      // clear child bit, get child number
      NodeInfo := NodeInfo and (not Child);
      Child := (Child shr 4) and 7;

      // child map cell
      Cell := Store.Current.Cell;
      Inc(NativeInt(Cell), Store.Info.CellOffsets[Child]);

      // tile, mask --> (0, mask, 0, tile)
      ChildNodeInfo := PWord(Cell)^;
      ChildNodeInfo := (ChildNodeInfo + (ChildNodeInfo shl 24)) and Integer($00ff00ff);

      // (parent, mask, parentmask, tile)
      Child := (Child shl 2) + (NodeInfo and 2) + (NativeUInt(Cardinal(Store.Current.Coordinates)) and 1);
      ChildNodeInfo := ChildNodeInfo or PARENT_BITS[Child];

      // locked & (mask <--> parentmask) test
      if (ChildNodeInfo and ((ChildNodeInfo and $ff00) shl 8) = 0) then
        goto nextchild_continue;

      // path of current(tile) --> child(tile)
      TileWeights := Store.Info.TileWeights[ChildNodeInfo{parent} and 1];
      Path := TileWeights[NodeInfo shr 24];
      Inc(Path, TileWeights[ChildNodeInfo shr 24]);

      // unsupported tiles test
      if (Path > UNSUPPORTED_TILE_WEIGHT) then
        goto nextchild_continue;

      // from start point path
      Path := Store.Current.Path + (Path shr 1);

      // if node is exists
      ChildNodePtr := Cell.NodePtr;
      // ???cache variants
      if (ChildNodePtr <> 0) then
      begin
        {$ifdef LARGEINT}
          // todo
          ChildNode := Pointer(ChildNodePtr);
        {$else}
          ChildNode := Pointer(ChildNodePtr);
        {$endif}

        if (Path >= ChildNode.Path) then
        begin
          if (NodeInfo and $ff00 <> 0) then Continue;
          Break;
        end;

        // new parent bits
        ChildNode.NodeInfo := (ChildNode.NodeInfo and DELTA_CLEAR_MASK) or ChildNodeInfo;

        // new Path and SortValue
        ChildNode.SortValue := Path + (ChildNode.SortValue - ChildNode.Path);

        // remove todo
        //

        goto child_tobuffer;
      end;

      // allocate new node
      ChildNode := Store.Info.NodeStorage.NewNode;
      {$ifdef LARGEINT}
        Cell.NodePtr := NativeUInt(NativeInt(ChildNode) + Store.Info.NodeStorage.LargeModifier);
      {$else}
        Cell.NodePtr := NativeUInt(ChildNode);
      {$endif}

      // child coordinates
      OffsetXY := Cardinal(POINT_OFFSETS_INVERT[ChildNodeInfo and 7]);
      NodeXY := Cardinal(Store.Current.Coordinates);
      ChildXY := NodeXY + OffsetXY;
      OffsetXY := OffsetXY and $ffff0000;
      NodeXY := NodeXY and $ffff0000;
      ChildXY := Word(ChildXY) + NodeXY + OffsetXY;
      Cardinal(ChildNode.Coordinates) := ChildXY;

      // heuristics and so on
      // todo

      // set next allocable node
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

    child_tobuffer:
      {$ifdef CPUX86}Store.{$endif}Buffer[(NodeInfo shr COUNTER_OFFSET) and 7] := ChildNode;
      Inc(NodeInfo, (1 shl COUNTER_OFFSET));

      // goto nextchild_continue;
      if (NodeInfo and $ff00 = 0) then Break;
    until (False);

    // move buffered nodes to opened list
    if (NodeInfo and (7 shl COUNTER_OFFSET) = 0) then
      goto next_current;

    // sort
    // todo

    // next opened node
  next_current:
    Node := Store.Current.Node.Next;
  current_initialize:
    // store pointer and path
    Store.Current.Node := Node;
    Store.Current.SortValue := Node.SortValue;
    Store.Current.Path := Node.Path;

    // cell
    NodeInfo{XY} := Cardinal(Node.Coordinates);
    Cardinal(Store.Current.Coordinates) := NodeInfo{XY};
    Cell := @Store.Info.Cells[(NativeInt(NodeInfo) shr 16){X} + Store.Info.MapWidth * {Y}Word(NodeInfo)];
    Store.Current.Cell := Cell;

    // node info
    NodeInfo := Cardinal(Node.NodeInfo);
    if (NodeInfo and FLAG_KNOWN_PATH <> 0) then Break;
  until (False);

  // Result
  Result := Node;

  // store current Info
  CopyInfo(TPathMapPtr(Store.Self).FInfo, Store.Info);
end;

function TPathMap.AllocateNode(const X, Y: NativeInt): PPathMapNode;
var
  Coordinates: NativeUInt;
  Cell: PPathMapCell;
  ChildNodePtr: NativeUInt;
  NodeInfo: NativeUInt;
begin
  Coordinates := Y + (X shl 16);
  Cell := @FInfo.Cells[X + FInfo.MapWidth * Y];

  ChildNodePtr := Cell.NodePtr; // clear bits?
  if (ChildNodePtr = 0) then
  begin
    Result := FInfo.NodeStorage.NewNode;
    Cardinal(Result.Coordinates) := Coordinates;
    {$ifdef LARGEINT}
      Cell.NodePtr := NativeUInt(NativeInt(Result) + FInfo.NodeStorage.LargeModifier);
    {$else}
      Cell.NodePtr := NativeUInt(Result);
    {$endif}

    // tile, mask --> (0, mask, 0, tile)
    NodeInfo := PWord(Cell)^;
    NodeInfo := (NodeInfo + (NodeInfo shl 24)) and Integer($00ff00ff);
    Result.NodeInfo := NodeInfo;

    // set next allocable node
    if (Result = FInfo.NodeStorage.MaximumNode) then
    begin
      // todo call some realloc
    end else
    begin
      Inc(Result);
      FInfo.NodeStorage.NewNode := Result;
      Dec(Result);
    end;
  end else
  begin
    Cell.NodePtr := ChildNodePtr;

    {$ifdef LARGEINT}
      // todo
      Result := Pointer(ChildNodePtr);
    {$else}
      Result := Pointer(ChildNodePtr);
    {$endif}
  end;
end;

function TPathMap.FindPath(const Start, Finish: TPoint;
  const Weights: TPathMapWeightsPtr; const ExcludedPoints: PPoint;
  const ExcludedPointsCount: NativeUInt): PPathMapResult;
label
  calculate_result;
var
  StartNode, FinishNode, Node: PPathMapNode;
  FailureNode: TPathMapNode;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  // todo

  // get and inspect/fill start node
  StartNode := AllocateNode(Start.X, Start.Y);
  if (StartNode.NodeInfo and FLAG_KNOWN_PATH <> 0) then
  begin
    Node := StartNode;
    goto calculate_result;
  end;
  // todo
  StartNode.Path := 0;
  StartNode.SortValue := 0{Path} + 0{CalculateHeuristics};

  // cache finish point and mark as known attainable
  FinishNode := AllocateNode(Start.X, Start.Y);
  FinishNode.NodeInfo := FLAG_KNOWN_PATH or FLAG_ATTAINABLE;

  // run finding loop
  FailureNode.NodeInfo := FLAG_KNOWN_PATH {+ !FLAG_ATTAINABLE};
  StartNode.Next := @FailureNode;
  FailureNode.Prev := StartNode;
  Node := DoFindPath({$ifdef CPFLIB}@Self{$else}Self{$endif}, StartNode);
  if (FCaching) then
  begin
    // MarkCachedPath(StartNode,
  end else
  begin
    // ?
  end;

calculate_result:
  if (Node.NodeInfo and FLAG_ATTAINABLE = 0) then
  begin
    Result := nil;
  end else
  begin
    Result := @FFindResult;
    // ToDo

    // todo calculate
  end;
end;





initialization
  {$ifdef CPF_GENERATE_LOOKUPS}
    GenerateLookups;
    Halt;
  {$endif}
  {$ifNdef CPFLIB}
    {$WARNINGS OFF} // deprecated warning bug fix (like Delphi 2010 compiler)
    System.GetMemoryManager(MemoryManager);
  {$endif}
  {$if Defined(DEBUG) and not Defined(CPFLIB)}
  TPathMap(nil).DoFindPath(nil, nil);
  {$ifend}

end.
