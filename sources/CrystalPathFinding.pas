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
    PointsCount: NativeUInt;
    Distance: Double;
  end;
  PPathMapResult = ^TPathMapResult;

  // internal class
  TCPFExceptionString = {$ifdef CPFLIB}PWideChar{$else}string{$endif};
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
  end;

  // internal tile weight storage
  PCPFWeights = ^TCPFWeights;
  TCPFWeights = object
  public
    {class} function NewInstance(const ACount: Cardinal; const Address: Pointer): PCPFWeights;
    procedure Release(const Address: Pointer);
  public
    RefCount: Cardinal;
    UpdateId: Cardinal;
    Count: Cardinal;
    Values: array[0..0] of Single;
  end;

  // map weights
  TPathMapWeights = {$ifdef CPFLIB}object{$else}class{$endif}(TCPFClass)
  private
    FWeights: PCPFWeights;
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
  TCPFMapCell = packed record
    Tile: TPathMapTile;
    Mask: Byte;
    NodePtr: Cardinal;
  end;
  PCPFMapCell = ^TCPFMapCell;
  TCPFMapCellArray = array[0..0] of TCPFMapCell;
  PCPFMapCellArray = ^TCPFMapCellArray;

  // node type
  PCPFMapNode = ^TCPFMapNode;
  TCPFMapNode = packed record
  case Boolean of
    False: (
              SortValue: Cardinal; // path + heuristics to finish point
              Path: Cardinal; // path from start point to the cell
              Prev, Next: PCPFMapNode;
              Coordinates: TCPFPoint;
              case Integer of
              0: (
                   ParentAndFlags: Byte
                   {
                     Parent:3;
                     case Boolean of
                       False: (Way:3);
                        True: (KnownChild:3);
                     end;
                     KnownPath:1;
                     Attainable:1;
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


  TCPFNodeStorage = record
    NewNode: PCPFMapNode;
    MaximumNode: PCPFMapNode;

    {$ifdef LARGEINT}
    LargeModifier: NativeInt;
    {$endif}

    Allocated: NativeUInt;
    Buffers: array[0..31] of NativeUInt{Pointer};
  end;

  PCardinalList = ^TCardinalList;
  TCardinalList = array[0..High(Integer) div SizeOf(Cardinal) - 1] of Cardinal;

  TCPFMapInfo = record
    Cells: PCPFMapCellArray;
    MapWidth: NativeInt;
    HeuristicsLine: NativeInt;
    HeuristicsDiagonal: NativeInt;
    TileWeights: array[0..1] of PCardinalList;
    CellOffsets: array[0..7] of NativeInt;
    FinishPoint: TCPFPoint;
    NodeStorage: TCPFNodeStorage;
  end;

  // path finding parameters
  TPathMapFindParameters = record
    StartPoints: PPoint;
    StartPointsCount: NativeUInt;
    Finish: TPoint;
    Weights: TPathMapWeightsPtr;
    ExcludedPoints: PPoint;
    ExcludedPointsCount: NativeUInt;
  end;
  PPathMapFindParameters = ^TPathMapFindParameters;

  // main path finding class
  TPathMap = {$ifdef CPFLIB}object{$else}class{$endif}(TCPFClass)
  private
    FFindResult: TPathMapResult;
    FFindResultOnePoint: TPoint;
    FInfo: TCPFMapInfo;
    FWidth: Word;
    FHeight: Word;
    FCellCount: NativeUInt;
    FPathLengthLimit: NativeUInt;
    FKind: TPathMapKind;
    FHighTile: TPathMapTile;
    FSectors: PByte;
    FSectorTest: Boolean;
    FCaching: Boolean;

    procedure RaiseCoordinates(const X, Y: Integer; const Id: TCPFExceptionString);
    function GetTile(const X, Y: Word): TPathMapTile;
    procedure SetTile(const X, Y: Word; const Value: TPathMapTile);
    procedure GrowNodeStorage(var Buffer: TCPFNodeStorage);
//    function AllocateNode(const X, Y: NativeInt): PCPFMapNode;
    function CalculateHeuristics(const Start, Finish: TPoint): NativeInt;
//    procedure ClearMapCells(Node: PCPFMapNode; Count: NativeUInt); overload;
//    procedure ClearMapCells(Node: PCPFMapNode{as list}); overload;
    function DoFindPathLoop(StartNode: PCPFMapNode): PCPFMapNode;
    function DoFindPath(const Parameters: TPathMapFindParameters): PPathMapResult;
  private
    FWeights: record
      Current: PCPFWeights;
      UpdateId: Cardinal;
      Default: PCPFWeights;
    end;
    FStartPoints: record
      Buffer: TCPFBuffer;
      Count: NativeUInt;
    end;
    FExcludedPoints: record
      Buffer: TCPFBuffer;
      Count: NativeUInt;
    end;
    FFinishPoint: TPoint;
    FFoundPath: record
      Buffer: TCPFBuffer;
      Length: NativeUInt;
    end;
    // todo


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
    property SectorTest: Boolean read FSectorTest write FSectorTest;
    property Caching: Boolean read FCaching write FCaching;
    property Tiles[const X, Y: Word]: TPathMapTile read GetTile write SetTile; default;

    function FindPath(const Parameters: TPathMapFindParameters): PPathMapResult; overload;
    function FindPath(const Start, Finish: TPoint; const Weights: TPathMapWeightsPtr = nil;
      const ExcludedPoints: PPoint = nil; const ExcludedPointsCount: NativeUInt = 0): PPathMapResult; overload;
    function FindPath(const StartPoints: PPoint; const StartPointsCount: NativeUInt;
      const Finish: TPoint; const Weights: TPathMapWeightsPtr = nil;
      const ExcludedPoints: PPoint = nil; const ExcludedPointsCount: NativeUInt = 0): PPathMapResult; overload;
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
  function  cpfFindPath(HMap: TCPFHandle; Parameters: PPathMapFindParameters; SectorTest: Boolean = False; Caching: Boolean = True): PPathMapResult; cdecl;
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
    Cardinal(P)^ := 0;

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

procedure RaiseInvalidTile(const Tile, HighTile: TPathMapTile; const Address: Pointer);
begin
  CPFExceptionFmt('Invalid tile %d, high tile is %d', [Tile, HighTile], Address);
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

function  cpfFindPath(HMap: TCPFHandle; Parameters: PPathMapFindParameters; SectorTest: Boolean = False; Caching: Boolean = True): PPathMapResult; cdecl;
var
  Address: Pointer;
begin
  Address := ReturnAddress;
  if (HMap <= $ffff) then RaiseInvalidPointer(Address);

  if (Parameters = nil) then
  begin
    Result := nil;
    Exit;
  end;
  if (NativeUInt(Parameters) <= $ffff) then RaiseInvalidPointer(Address);

  TCPFClassPtr(HMap).FCallAddress := Address;
  TPathMapPtr(HMap).SectorTest := SectorTest;
  TPathMapPtr(HMap).Caching := Caching;
  Result := TPathMapPtr(HMap).DoFindPath(Parameters^);
end;
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
       0..32: FAllocatedSize := 32;
     33..128: FAllocatedSize := 128;
    129..512: FAllocatedSize := 512;
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

{ TCPFWeights }

{class} function TCPFWeights.NewInstance(const ACount: Cardinal; const Address: Pointer): PCPFWeights;
var
  One: Cardinal;
begin
  // allocate
  Result := CPFAlloc(SizeOf(TCPFWeights) - SizeOf(Single) + ACount * SizeOf(Single),
    Address);

  // fill
  Result.RefCount := 1;
  Result.UpdateId := 1;
  Result.Count := ACount;

  // default values
  PSingle(@One)^ := 1.0;
  FillCardinal(Pointer(@Result.Values[0]), ACount, One);
end;

procedure TCPFWeights.Release(const Address: Pointer);
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


type
  TYXSmallPoint = record
    y: SmallInt;
    x: SmallInt;
  end;

  TChildArray = array[0..7] of Word;
  PChildArray = ^TChildArray;

const
  NODESTORAGE_INFO: array[0..31] of packed record
    Count: Cardinal;
    Previous: Cardinal;
  end = (
    { 0} (Count: 512; Previous: 0),
    { 1} (Count: 512; Previous: 512),
    { 2} (Count: 1024; Previous: 1024),
    { 3} (Count: 1024; Previous: 2048),
    { 4} (Count: 2048; Previous: 3072),
    { 5} (Count: 2048; Previous: 5120),
    { 6} (Count: 4096; Previous: 7168),
    { 7} (Count: 8192; Previous: 11264),
    { 8} (Count: 16384; Previous: 19456),
    { 9} (Count: 32768; Previous: 35840),
    {10} (Count: 65536; Previous: 68608),
    {11} (Count: 131072; Previous: 134144),
    {12} (Count: 262144; Previous: 265216),
    {13} (Count: 524288; Previous: 527360),
    {14} (Count: 1048576; Previous: 1051648),
    {15} (Count: 1276672; Previous: 2100224),
    {16} (Count: 2097152; Previous: 3376896),
    {17} (Count: 2097152; Previous: 5474048),
    {18} (Count: 2097152; Previous: 7571200),
    {19} (Count: 2097152; Previous: 9668352),
    {20} (Count: 2097152; Previous: 11765504),
    {21} (Count: 4194304; Previous: 13862656),
    {22} (Count: 4194304; Previous: 18056960),
    {23} (Count: 4194304; Previous: 22251264),
    {24} (Count: 4194304; Previous: 26445568),
    {25} (Count: 4194304; Previous: 30639872),
    {26} (Count: 4194304; Previous: 34834176),
    {27} (Count: 4194304; Previous: 39028480),
    {28} (Count: 4194304; Previous: 43222784),
    {29} (Count: 4194304; Previous: 47417088),
    {30} (Count: 4194304; Previous: 51611392),
    {31} (Count: 4194304; Previous: 55805696)
  );

  CELLCOUNT_LIMIT = 60*1000*1000;
  PATHLENGTH_LIMIT = (CELLCOUNT_LIMIT div 2) + 1;
  NATTANABLE_LENGTH_LIMIT = not Cardinal(PATHLENGTH_LIMIT);

  PATHLESS_TILE_WEIGHT = High(Cardinal) shr 1;

  FLAG_KNOWN_PATH = 1 shl 6;
  FLAG_ATTAINABLE = 1 shl 7;

  {$ifdef LARGEINT}
    LARGE_NODEPTR_OFFSET = 32 - {0..31}5;
  {$endif}

  NODEPTR_FLAG_CALCULATED = 1;
  NODEPTR_FLAG_LARGE = 2;
  NODEPTR_CLEAN_MASK = Integer((not 7) {$ifdef LARGEINT} and ((1 shl LARGE_NODEPTR_OFFSET) - 1){$endif});

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

  CHILD_ARRAYS: array[0..11{4 diagonal + 4 clockwise optional}] of TChildArray = (
   ($0100, $8070, $0210, $4060, $0420, $2050, $0830, $1040),
   ($0210, $0100, $0420, $8070, $0830, $4060, $1040, $2050),
   ($0210, $0420, $0100, $0830, $8070, $1040, $4060, $2050),
   ($0420, $0830, $0210, $1040, $0100, $2050, $8070, $4060),
   ($0830, $0420, $1040, $0210, $2050, $0100, $4060, $8070),
   ($0830, $1040, $0420, $2050, $0210, $4060, $0100, $8070),
   ($1040, $2050, $0830, $4060, $0420, $8070, $0210, $0100),
   ($2050, $1040, $4060, $0830, $8070, $0420, $0100, $0210),
   ($2050, $4060, $1040, $8070, $0830, $0100, $0420, $0210),
   ($4060, $2050, $8070, $1040, $0100, $0830, $0210, $0420),
   ($8070, $4060, $0100, $2050, $0210, $1040, $0420, $0830),
   ($8070, $0100, $4060, $0210, $2050, $0420, $1040, $0830)
  );

  CHILD_ARRAYS_OFFSETS: array[0..63{parent:3,way:3}] of Byte = (
    $B0, $B0, $B0, $A0, $A0, $A0, $A0, $A0,
    $40, $40, $40, $40, $50, $50, $50, $40,
    $10, $10, $20, $20, $20, $10, $10, $10,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $30, $30, $30, $30, $30, $30, $30, $30,
    $60, $60, $60, $60, $60, $60, $60, $60,
    $90, $90, $90, $90, $90, $90, $90, $90,
    $60, $60, $60, $60, $60, $60, $60, $60
  );

  PARENT_BITS: array[0..31{oddy:1;hexagonal:1;child:3}] of NativeUInt = (
    $00C70004, $00C70004, $00C70004, $00000004,
    $00070005, $00070005, $004F0005, $00970005,
    $001F0006, $001F0006, $00000006, $001F0006,
    $001C0007, $001C0007, $003E0007, $005D0007,
    $007C0000, $007C0000, $00000000, $007C0000,
    $00700001, $00700001, $00790001, $00F40001,
    $00F10002, $00F10002, $00F10002, $00000002,
    $00C10003, $00C10003, $00D50003, $00E30003
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

procedure AddChildArray(Base: Integer; Clockwise, Finalize: Boolean);
const
  CHILD_VALUES: array[0..7] of Word = (
    ((1 shl 0) shl 8) or (0 shl 4),
    ((1 shl 1) shl 8) or (1 shl 4),
    ((1 shl 2) shl 8) or (2 shl 4),
    ((1 shl 3) shl 8) or (3 shl 4),
    ((1 shl 4) shl 8) or (4 shl 4),
    ((1 shl 5) shl 8) or (5 shl 4),
    ((1 shl 6) shl 8) or (6 shl 4),
    ((1 shl 7) shl 8) or (7 shl 4)
  );

var
  S: string;
  i, Sign: integer;
  ChildArray: TChildArray;
  Child: PWord;

  procedure AddChild(const N: Integer);
  begin
    Child^ := CHILD_VALUES[(N + 8) and 7];
    Inc(Child);
  end;
begin
  Sign := 1;
  if (not Clockwise) then Sign := -1;

  Child := @ChildArray[0];
  AddChild(Base);
  for i := 1 to 3 do
  begin
    AddChild(Base + Sign * i);
    AddChild(Base - Sign * i);
  end;
  AddChild(Base + 4);

  S := Format(' ($%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x, $%0.4x)',
    [ChildArray[0], ChildArray[1], ChildArray[2], ChildArray[3],
     ChildArray[4], ChildArray[5], ChildArray[6], ChildArray[7]]);

  if (not Finalize) then S := S + ',';
  LookupLine(S);
end;

procedure AddTwoChildArrays(Base: Integer; Finalize: Boolean);
begin
  AddChildArray(Base, False, False);
  AddChildArray(Base, True, Finalize);
end;

procedure AddChildArrayOffsets(WayX, WayY: Integer; Finalize: Boolean);
var
  Buffer: array[0..7] of Byte;
  Offset: PByte;
  S: string;
  Parent, WayChild: Integer;
  N: Byte;
begin
  if (WayX = 1) then WayX := -1
  else
  if (WayX = 2) then WayX := 1;

  if (WayY = 1) then WayY := -1
  else
  if (WayY = 2) then WayY := 1;


  if (WayX < 0) then
  begin
    if (WayY < 0) then
    begin
      WayChild := 0;
    end else
    if (WayY = 0) then
    begin
      WayChild := 7;
    end else
    // (WayY > 0) then
    begin
      WayChild := 6;
    end;
  end else
  if (WayX = 0) then
  begin
    if (WayY < 0) then WayChild := 1
    else WayChild := 4;
  end else
  // (WayX > 0) then
  begin
    if (WayY < 0) then
    begin
      WayChild := 2;
    end else
    if (WayY = 0) then
    begin
      WayChild := 3;
    end else
    // (WayY > 0) then
    begin
      WayChild := 4;
    end;
  end;

  Offset := @Buffer[0];
  for Parent := 0 to 7 do
  begin
    N := (WayChild shr 1) * 3;

    if (WayChild and 1 <> 0) then
    begin
      Inc(N);

      if (Parent = ((WayChild - 1 + 4) and 7)) or
        (Parent = ((WayChild - 2 + 4) and 7)) or
        (Parent = ((WayChild - 3 + 4) and 7)) then
        Inc(N);
    end;

    Offset^ := N * SizeOf(TChildArray);
    Inc(Offset);
  end;

  S := Format('  $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x, $%0.2x',
    [Buffer[0], Buffer[1], Buffer[2], Buffer[3],
     Buffer[4], Buffer[5], Buffer[6], Buffer[7]]);

  if (not Finalize) then S := S + ',';
  LookupLine(S);
end;

procedure AddParentBits(Child: Integer; Finalize: Boolean);
var
  Parent: Integer;
  Buffer: array[0..3] of Cardinal;
  Hexagonal, OddY: Boolean;
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

  for Hexagonal := False to True do
  for OddY := False to True do
  begin
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

  S := Format('  $%0.8x, $%0.8x, $%0.8x, $%0.8x',
    [Buffer[0], Buffer[1], Buffer[2], Buffer[3]]);

  if (not Finalize) then S := S + ',';
  LookupLine(S);
end;

procedure GenerateLookups;
var
  Way, WayX, WayY: Integer;
  Child: Integer;
begin
  LookupsText := TStringList.Create;
  try
    LookupsText.Add('const');

    // CHILD_ARRAYS
    LookupLine('CHILD_ARRAYS: array[0..11{4 diagonal + 4 clockwise optional}] of TChildArray = (');
    begin
      AddChildArray(0, False, False);
      AddTwoChildArrays(1, False);
      AddChildArray(2, True, False);
      AddTwoChildArrays(3, False);
      AddChildArray(4, True, False);
      AddTwoChildArrays(5, False);
      AddChildArray(6, False, False);
      AddTwoChildArrays(7, True);
    end;
    LookupLine(');');

    // CHILD_ARRAYS_OFFSETS
    LookupLine;
    LookupLine('CHILD_ARRAYS_OFFSETS: array[0..63{parent:3,way:3}] of Byte = (');
    for Way := 0 to 7 do
    begin
      WayX := (Way + 1) mod 3;
      WayY := (Way + 1) div 3;

      AddChildArrayOffsets(WayX, WayY, Way = 7);
    end;
    LookupLine(');');

    // PARENT_BITS
    LookupLine;
    LookupLine('PARENT_BITS: array[0..31{oddy:1;hexagonal:1;child:3}] of NativeUInt = (');
    for Child := 0 to 7 do
    begin
      AddParentBits(Child, Child = 7);
    end;
    LookupLine(');');


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

  if (AHighTile = $FF) then
    CPFException('High tile can''t be equal 255(0xFF), it means a barrier');

  FHighTile := AHighTile;

  FWeights := PCPFWeights(nil).NewInstance(NativeUInt(FHighTile) + 1, FCallAddress)
end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TPathMapWeights.Destroy;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  FWeights.Release(FCallAddress);

  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;


function TPathMapWeights.GetValue(const Tile: TPathMapTile): Single;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  if (Tile > HighTile) then
  begin
    RaiseInvalidTile(Tile, HighTile, FCallAddress);
    Result := 0;
  end else
  begin
    Result := FWeights.Values[Tile];
  end;
end;

procedure TPathMapWeights.SetValue(const Tile: TPathMapTile;
  const Value: Single);
var
  PValue: PSingle;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  if (Tile > HighTile) then
  begin
    RaiseInvalidTile(Tile, HighTile, FCallAddress);
  end else
  begin
    PValue := @FWeights.Values[Tile];
    if (PValue^ <> Value) then
    begin
      PValue^ := Value;
      Inc(FWeights.UpdateId);
    end;
  end;
end;


{ TPathMap }

{$ifdef CPFLIB}procedure{$else}constructor{$endif}
  TPathMap.Create(const AWidth, AHeight: Word; const AKind: TPathMapKind;
    const AHighTile: TPathMapTile);
var
  i: NativeInt;
  Size: NativeUInt;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
    inherited Create;
  {$endif}

  // arguments test
  begin
    FCellCount := NativeUInt(AWidth) * NativeUInt(AHeight);

    if (AWidth <= 1) or (AHeight <= 1) then
      // ToDo: 1 line map
      CPFExceptionFmt('Incorrect map size: %dx%d', [AWidth, AHeight]);

    if (FCellCount > CELLCOUNT_LIMIT) then
      CPFExceptionFmt('Too large map size %dx%d, cell count limit is %d', [AWidth, AHeight, CELLCOUNT_LIMIT]);

    if (Ord(AKind) > Ord(High(TPathMapKind))) then
      CPFExceptionFmt('Incorrect map kind: %d, high value mkHexagonal is %d', [Ord(AKind), Ord(High(TPathMapKind))]);

    if (AHighTile = $FF) then
      CPFException('High tile can''t be equal 255(0xFF), it means a barrier');
  end;

  // fill parameters
  FWidth := AWidth;
  FHeight := AHeight;
  FKind := AKind;
  FHighTile := AHighTile;
  FPathLengthLimit := (FCellCount div 2) + 1;
  FInfo.MapWidth := AWidth;
  FSectorTest := False;
  FCaching := True;

  // offsets
  for i := 0 to 7 do
  FInfo.CellOffsets[i] := SizeOf(TCPFMapCell) *
    (POINT_OFFSETS[i].y * FInfo.MapWidth + POINT_OFFSETS[i].x);

  // internal buffers
  FStartPoints.Buffer.Initialize(Self);
  FExcludedPoints.Buffer.Initialize(Self);
  FFoundPath.Buffer.Initialize(Self);

  // default tile weights
  FWeights.Default := PCPFWeights(nil).NewInstance(FHighTile + 1, FCallAddress);

  // allocate and fill cells
  Size := FCellCount * SizeOf(TCPFMapCell);
  CPFGetMem(Pointer(FInfo.Cells), Size);
  ZeroMemory(FInfo.Cells, Size);
  Clear;

  // allocate first
  GrowNodeStorage(FInfo.NodeStorage);
end;

{$ifdef CPFLIB}procedure{$else}destructor{$endif}
  TPathMap.Destroy;
var
  i: NativeInt;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  // tile weigths
  FWeights.Current.Release(FCallAddress);
  FWeights.Default.Release(FCallAddress);

  // internal buffers
  FStartPoints.Buffer.Free;
  FExcludedPoints.Buffer.Free;
  FFoundPath.Buffer.Free;

  // node storage
  for i := FInfo.NodeStorage.Allocated - 1 downto 0 do
    CPFFreeMem(Pointer(FInfo.NodeStorage.Buffers[i]));

  // cells
  CPFFreeMem(Pointer(FInfo.Cells));

  // sectors
  CPFFreeMem(Pointer(FSectors));

  {$ifNdef CPFLIB}
    inherited;
  {$endif}
end;

procedure TPathMap.RaiseCoordinates(const X, Y: Integer; const Id: TCPFExceptionString);
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

procedure TPathMap.GrowNodeStorage(var Buffer: TCPFNodeStorage);
var
  Allocated: NativeUInt;
  Count: NativeUInt;
  LastAllocation: Boolean;
  Node: PCPFMapNode;
begin
  Allocated := Buffer.Allocated;
  if (@Buffer <> @FInfo.NodeStorage) then
  if (Buffer.NewNode <> FInfo.NodeStorage.MaximumNode) or
     (Allocated <> FInfo.NodeStorage.Allocated) then
    CPFException('Incorrect node storage data');

  if (Allocated >= High(FInfo.NodeStorage.Buffers)) then
    RaiseOutOfMemory(FCallAddress);

  Count := NODESTORAGE_INFO[Allocated].Previous + NODESTORAGE_INFO[Allocated].Count;
  LastAllocation := (Count >= FCellCount);
  if (LastAllocation) then Count := FCellCount;
  Dec(Count, NODESTORAGE_INFO[Allocated].Previous);

  CPFGetMem(Pointer(FInfo.NodeStorage.Buffers[Allocated]), Count * SizeOf(TCPFMapNode));
  FInfo.NodeStorage.Allocated := Allocated + 1;
  Node := Pointer(FInfo.NodeStorage.Buffers[Allocated]);
  FInfo.NodeStorage.NewNode := Node;

  {$ifdef LARGEINT}
    FInfo.NodeStorage.LargeModifier :=
      (NativeInt(Allocated) shl LARGE_NODEPTR_OFFSET) - NativeInt(Node);
  {$endif}

  Inc(Node, NativeInt(Count) - 1 + Ord(LastAllocation));
  FInfo.NodeStorage.MaximumNode := Node;

  //  copy to buffer
  if (@Buffer <> @FInfo.NodeStorage) then
  begin
    Buffer.NewNode := FInfo.NodeStorage.NewNode;
    Buffer.MaximumNode := FInfo.NodeStorage.MaximumNode;
    Buffer.Allocated := Allocated + 1{FInfo.NodeStorage.Allocated};
    {$ifdef LARGEINT}
      Buffer.LargeModifier := FInfo.NodeStorage.LargeModifier;
    {$endif}
    Buffer.Buffers[Allocated] := FInfo.NodeStorage.Buffers[Allocated];
  end;
end;

(*function TPathMap.AllocateNode(const X, Y: NativeInt): PCPFMapNode;
type
  TNodeStorageBuffers = array[0..31] of NativeUInt;
var
  Coordinates: NativeUInt;
  Cell: PCPFMapCell;
  ChildNode: PCPFMapNode;
  NodeInfo: NativeUInt;

  {$ifdef LARGEINT}
    NodeStorageBuffers: ^TNodeStorageBuffers;
    NODEPTR_MODIFIER: NativeInt;
  {$else}
const
    NODEPTR_MODIFIER = NODEPTR_FLAG_CALCULATED;
  {$endif}
begin
  Coordinates := Y + (X shl 16);
  Cell := @FInfo.Cells[X + FInfo.MapWidth * Y];

  // clear bits?
  ChildNode := Pointer(NativeUInt(Cell.NodePtr));
  if (NativeInt(ChildNode) and NODEPTR_FLAG_CALCULATED = 0) then
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
      GrowNodeStorage(FInfo.NodeStorage);
    end else
    begin
      Inc(Result);
      FInfo.NodeStorage.NewNode := Result;
      Dec(Result);
    end;



  end else
  begin
    // NodePtr --> Pointer
    {$ifdef LARGEINT}
      NodeStorageBuffers := Pointer(@FInfo.NodeStorage.Buffers);
      ChildNode := Pointer(
        (NodeStorageBuffers[NativeUInt(ChildNode) shr LARGE_NODEPTR_OFFSET]) +
        (NativeUInt(ChildNode) and NODEPTR_CLEAN_MASK) );
    {$else}
      ChildNode := Pointer(NativeInt(ChildNode) and NODEPTR_CLEAN_MASK);
    {$endif}
  end;

  Result := ChildNode;
end; *)

function TPathMap.CalculateHeuristics(const Start, Finish: TPoint): NativeInt;
var
  dX, dY, X, Y: NativeInt;
  Mask: NativeInt;
begin
  dY := Start.Y - Finish.Y;
  dX := Start.X - Finish.X;

  // Y := Abs(dY)
  Mask := -(dY shr HIGH_NATIVE_BIT);
  Y := (dY xor Mask) - Mask;

  // X := Abs(dY)
  Mask := -(dX shr HIGH_NATIVE_BIT);
  X := (dX xor Mask) - Mask;

  if (Self.FKind < mkHexagonal) then
  begin
    if (X <= Y) then
    begin
      Result := X * FInfo.HeuristicsDiagonal + (Y - X) * FInfo.HeuristicsLine;
    end else
    begin
      Result := Y * FInfo.HeuristicsDiagonal + (X - Y) * FInfo.HeuristicsLine;
    end;
  end else
  begin
    X := X - ((Mask xor Finish.Y) and Y and 1) - (Y shr 1);
    Result := Y + (X and ((X shr HIGH_NATIVE_BIT) - 1));
    Result := Result * FInfo.HeuristicsLine;
  end;
end;

(*procedure TPathMap.ClearMapCells(Node: PCPFMapNode; Count: NativeUInt);
var
  TopNode: PCPFMapNode;
  NodeInfo{XY}: NativeUInt;
  MapWidth: NativeInt;
  Cells: PCPFMapCellArray;
begin
  TopNode := Node;
  Inc(TopNode, Count);

  Cells := FInfo.Cells;
  MapWidth := FInfo.MapWidth;

  while (Node <> TopNode) do
  begin
    NodeInfo{XY} := Cardinal(Node.Coordinates);
    Cells[(NativeInt(NodeInfo) shr 16){X} + MapWidth * {Y}Word(NodeInfo)].NodePtr := 0;

    Inc(Node);
  end;
end;

procedure TPathMap.ClearMapCells(Node: PCPFMapNode{as list});
var
  NodeInfo{XY}: NativeUInt;
  MapWidth: NativeInt;
  Cells: PCPFMapCellArray;
begin
  Cells := FInfo.Cells;
  MapWidth := FInfo.MapWidth;

  while (Node <> nil) do
  begin
    NodeInfo{XY} := Cardinal(Node.Coordinates);
    Cells[(NativeInt(NodeInfo) shr 16){X} + MapWidth * {Y}Word(NodeInfo)].NodePtr := 0;

    Node := Node.Next;
  end;
end; *)


type
  TMapNodeBuffer = array[0..7] of PCPFMapNode;
  PMapNodeBuffer = ^TMapNodeBuffer;

  TFindPathLoopStore = record
    Buffer: TMapNodeBuffer;
    Self: Pointer;
    HexagonalFlag: NativeUInt;
    Info: TCPFMapInfo;

    {$ifdef CPUX86}
    ChildList: PWord;
    {$endif}

    Current: record
      Node: PCPFMapNode;
      Cell: PCPFMapCell;
      Coordinates: TCPFPoint;
      SortValue: Cardinal;
      Path: Cardinal;
    end;
    Top: record
      Node: PCPFMapNode;
      SortValue: Cardinal;
    end;
  end;


procedure TestNodesInsert(NodeInfo: NativeUInt{$ifdef TEST_INSERT}; const InitStore: TFindPathLoopStore{$endif});
label
  next_current;
const
  COUNTER_OFFSET = 16;
var
  ChildNode: PCPFMapNode;
  {$ifNdef CPUX86}
    ChildSortValue: Cardinal;
  {$endif}
  Left: PCPFMapNode;

  PBufferHigh, PBufferBase, PBufferCurrent: ^PCPFMapNode;

  {$ifdef TEST_INSERT}
  i: Cardinal;
  {$endif}

  {$ifNdef CPUX86}
    Buffer: ^TMapNodeBuffer;
  {$endif}

  Store: TFindPathLoopStore;

begin
  {$ifdef TEST_INSERT}
    Store := InitStore;
  {$else}
    ZeroMemory(@Store, SizeOf(Store));
  {$endif}

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
      {$ifNdef CPUX86}ChildSortValue := ChildNode.SortValue;{$endif}

      Left := PBufferCurrent^;
      if (Left.SortValue > {$ifNdef CPUX86}ChildSortValue{$else}ChildNode.SortValue{$endif}) then
      begin
       repeat
          PMapNodeBuffer(PBufferCurrent)[1] := Left;
          if (PBufferCurrent = @{$ifdef CPUX86}Store.{$endif}Buffer[0]) then Break;

          Dec(PBufferCurrent);
          Left := PBufferCurrent^;
          if (Left.SortValue > {$ifNdef CPUX86}ChildSortValue{$else}ChildNode.SortValue{$endif}) then Continue;
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



  {$ifdef TEST_INSERT}
    for i := 0 to ((NodeInfo shr COUNTER_OFFSET) and $f) - 1 do
    if (Store.Buffer[i].SortValue <> i) then
      raise Exception.CreateFmt('Store.Buffer[%d].Node.SortValue = %d',
        [i, Store.Buffer[i].SortValue]);

   raise Exception.Create('�������� ������ �� �������!');
  {$endif}

next_current:
end;

{$ifdef TEST_INSERT}
procedure TestNodesInsertEx;
const
  VALUES: array[0..7] of Cardinal =
    (5, 6, 4, 0, 1, 7, 2, 3);
var
  InitStore: TFindPathLoopStore;
  NodeBuffer: array[0..7] of TCPFMapNode;
  i: Cardinal;
begin
  for i := 0 to 7 do
  begin
    NodeBuffer[i].SortValue := VALUES[i];
    InitStore.Buffer[i] := @NodeBuffer[i];
  end;

  TestNodesInsert(8 shl 16, InitStore);
end;
{$endif}


function TPathMap.DoFindPathLoop(StartNode: PCPFMapNode): PCPFMapNode;
label
  nextchild_continue, nextchild,
  heuristics_data,
  next_current, current_initialize;
const
  PARENT_BITS_CLEAR_MASK = not Cardinal(((1 shl 5) - 1) shl 3);
  COUNTER_OFFSET = 16;
type
  TMapNodeBuffer = array[0..7] of PCPFMapNode;
  TNodeStorageBuffers = array[0..31] of NativeUInt;
var
  Node: PCPFMapNode;
  NodeInfo: NativeUInt;
  ChildList: PWord;
  Child: NativeUInt;
  Cell: PCPFMapCell;
  ChildNodeInfo: NativeUInt;
  ChildNode: PCPFMapNode;
  TileWeights: PCardinalList;
  Path: Cardinal;
  NodeXY, OffsetXY, ChildXY: Cardinal;
  dX, dY, X, Y: NativeInt;
  Mask: NativeInt;

  ChildSortValue, ChildPath: Cardinal;
  Left, Right: PCPFMapNode;

  PBufferHigh, PBufferBase, PBufferCurrent: ^PCPFMapNode;

  Store: TFindPathLoopStore;(*record
    Buffer: TMapNodeBuffer;
    Self: Pointer;
    HexagonalFlag: NativeUInt;
    Info: TCPFMapInfo;

    {$ifdef CPUX86}
    ChildList: PWord;
    {$endif}

    Current: record
      Node: PCPFMapNode;
      Cell: PCPFMapCell;
      Coordinates: TCPFPoint;
      SortValue: Cardinal;
      Path: Cardinal;
    end;
    Top: record
      Node: PCPFMapNode;
      SortValue: Cardinal;
    end;
  end;*)

  {$ifNdef CPUX86}
    Buffer: PMapNodeBuffer;
  {$endif}

  {$ifdef LARGEINT}
    NodeStorageBuffers: ^TNodeStorageBuffers;
    NODEPTR_MODIFIER: NativeInt;
  {$else}
const
    NODEPTR_MODIFIER = NODEPTR_FLAG_CALCULATED;
  {$endif}
begin
  Store.Self := Pointer({$ifdef CPFLIB}@Self{$else}Self{$endif});
  Store.HexagonalFlag := NativeUInt(Self.FKind = mkHexagonal) * 2;
  Move(Self.FInfo, Store.Info,
    (SizeOf(Store.Info) - 32 * SizeOf(Pointer)) +
    (Self.FInfo.NodeStorage.Allocated * SizeOf(Pointer)) );

  {$ifNdef CPUX86}
    Buffer := @Store.Buffer;
  {$endif}

  {$ifdef LARGEINT}
    NODEPTR_MODIFIER := Store.Info.NodeStorage.LargeModifier +
      (NODEPTR_FLAG_LARGE + NODEPTR_FLAG_CALCULATED);
  {$endif}

  // Top initialization
  Node := StartNode.Next;
  Store.Top.Node := Node;
  Node.Prev := StartNode;
  Store.Top.SortValue := NATTANABLE_LENGTH_LIMIT - 1;

  // finding loop from Start to Finish
  Node := StartNode;
  goto current_initialize;
  repeat
    // lock
    Node.ParentMask := 0;

    // todo next top?

    // child list
    ChildList := Pointer(NativeUInt(CHILD_ARRAYS_OFFSETS[NodeInfo and 63]));
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

      // (parent:3, mask, parentmask, tile)
      Child := (Child shl 2) + (NodeInfo and 2) + (NativeUInt(Cardinal(Store.Current.Coordinates)) and 1);
      ChildNodeInfo := ChildNodeInfo or PARENT_BITS[Child];

      // path of current(tile) --> child(tile)
      TileWeights := Store.Info.TileWeights[ChildNodeInfo{parent} and 1];
      Path := TileWeights[NodeInfo shr 24];
      Inc(Path, TileWeights[ChildNodeInfo shr 24]);

      // pathless tiles test
      if (Path > PATHLESS_TILE_WEIGHT) then
        goto nextchild_continue;

      // from start point path
      Path := Store.Current.Path + (Path shr 1);

      // if node is allocated/initialized
      ChildNode := Pointer(NativeUInt(Cell.NodePtr));
      if (NativeInt(ChildNode) and NODEPTR_FLAG_CALCULATED = 0) then
      begin
        // mask & parentmask test
        if (ChildNodeInfo and ((ChildNodeInfo and $ff00) shl 8) = 0) then
          goto nextchild_continue;

        if (ChildNode = nil) then
        begin
          // allocate new node
          ChildNode := Store.Info.NodeStorage.NewNode;

          {$ifdef LARGEINT}
            Cell.NodePtr := NativeUInt(NativeInt(ChildNode) + NODEPTR_MODIFIER);
          {$else}
            Cell.NodePtr := NativeUInt(ChildNode) + NODEPTR_MODIFIER;
          {$endif}
          ChildNode.Path := Path;
          ChildNode.NodeInfo := ChildNodeInfo;

          // child coordinates
          OffsetXY := Cardinal(POINT_OFFSETS_INVERT[ChildNodeInfo and 7]);
          NodeXY := Cardinal(Store.Current.Coordinates);
          ChildXY := NodeXY + OffsetXY;
          OffsetXY := OffsetXY and $ffff0000;
          NodeXY := NodeXY and $ffff0000;
          ChildXY := Word(ChildXY) + NodeXY + OffsetXY;
          Cardinal(ChildNode.Coordinates) := ChildXY;

          // set next allocable node
          if (ChildNode <> Store.Info.NodeStorage.MaximumNode) then
          begin
            Inc(ChildNode);
            Store.Info.NodeStorage.NewNode := ChildNode;
            Dec(ChildNode);
          end else
          begin
            TPathMapPtr(Store.Self).GrowNodeStorage(Store.Info.NodeStorage);
            {$ifdef LARGEINT}
              NODEPTR_MODIFIER := Store.Info.NodeStorage.LargeModifier +
                (NODEPTR_FLAG_LARGE + NODEPTR_FLAG_CALCULATED);
            {$endif}
          end;
          goto heuristics_data;
        end else
        begin
          // already node allocated and coordinates filled
          // need to fill path/parent and heuristics/way data
          {$ifdef LARGEINT}
            Cell.NodePtr := NativeUInt(NativeInt(ChildNode) + NODEPTR_MODIFIER - NODEPTR_FLAG_LARGE);
          {$else}
            Cell.NodePtr := NativeUInt(ChildNode) + NODEPTR_MODIFIER;
          {$endif}
          ChildNode.Path := Path;
          ChildNode.NodeInfo := ChildNodeInfo;

        heuristics_data:
          // (dX, dY) = ChildNode.Coordinates - Store.Info.FinishPoint;
          dX := Cardinal(ChildNode.Coordinates);
          dY := Word(dX);
          dX := dX shr 16;
          Mask := Cardinal(Store.Info.FinishPoint);
          dY := dY - Word(Mask);
          dX := dX - (Mask shr 16);

          // Way
          Mask := 2*Byte(dY > 0);
          X := 2*Byte(dX > 0);
          Inc(Mask, dY shr HIGH_NATIVE_BIT);
          Inc(X, dX shr HIGH_NATIVE_BIT);
          Mask := 3 * Mask;
          ChildNode.NodeInfo := ChildNode.NodeInfo or Cardinal((X - 1 + {3 *} Mask) shl 3);

          // Y := Abs(dY)
          Mask := -(dY shr HIGH_NATIVE_BIT);
          Y := (dY xor Mask) - Mask;

          // X := Abs(dY)
          Mask := -(dX shr HIGH_NATIVE_BIT);
          X := (dX xor Mask) - Mask;

          // calculate
          if (Store.HexagonalFlag and 1 = 0) then
          begin
            if (X <= Y) then
            begin
              ChildNode.SortValue := {$ifdef CPUX86}ChildNode.{$endif}Path +
                 Cardinal(X * Store.Info.HeuristicsDiagonal + (Y - X) * Store.Info.HeuristicsLine);
            end else
            begin
              ChildNode.SortValue := {$ifdef CPUX86}ChildNode.{$endif}Path +
                 Cardinal(Y * Store.Info.HeuristicsDiagonal + (X - Y) * Store.Info.HeuristicsLine);
            end;
          end else
          begin
            X := X - ((Mask xor PNativeInt(@Store.Info.FinishPoint)^) and Y and 1) - (Y shr 1);
            ChildNode.SortValue := {$ifdef CPUX86}ChildNode.{$endif}Path +
               Cardinal(Store.Info.HeuristicsLine * (Y + (X and ((X shr HIGH_NATIVE_BIT) - 1))));
          end;
        end;
      end else
      begin
        // NodePtr --> Pointer
        {$ifdef LARGEINT}
          NodeStorageBuffers := Pointer(@Store.Info.NodeStorage.Buffers);
          ChildNode := Pointer(
            (NodeStorageBuffers[NativeUInt(ChildNode) shr LARGE_NODEPTR_OFFSET]) +
            (NativeUInt(ChildNode) and NODEPTR_CLEAN_MASK) );
        {$else}
          ChildNode := Pointer(NativeInt(ChildNode) and NODEPTR_CLEAN_MASK);
        {$endif}

        // continue if the path is shorter
        ChildPath := ChildNode.Path;
        if (Path >= ChildPath) then
          goto nextchild_continue;

        // locked & (mask <--> parentmask) + knownpath test
        ChildNodeInfo := (ChildNode.NodeInfo and PARENT_BITS_CLEAR_MASK) or ChildNodeInfo;
        if ((ChildNodeInfo and FLAG_KNOWN_PATH) or
           (ChildNodeInfo and ((ChildNodeInfo and $ff00) shl 8)) = 0) then
          goto nextchild_continue;

        // sort value as hot knownpath node marker
        ChildSortValue := ChildNode.SortValue;

        // new parent bits
        ChildNode.NodeInfo := ChildNodeInfo;

        // new Path and SortValue
        ChildNode.SortValue := Path + {heuristics}(ChildSortValue - ChildPath);
        ChildNode.Path := Path;

        // remove from opened list
        if (ChildSortValue < NATTANABLE_LENGTH_LIMIT) then
        begin
          Left := ChildNode.Prev;
          Right := ChildNode.Next;
          Left.Next := Right;
          Right.Prev := Left;
        end;
      end;

      // add child node to buffer
      {$ifdef CPUX86}Store.{$endif}Buffer[(NodeInfo shr COUNTER_OFFSET) and 7] := ChildNode;
      Inc(NodeInfo, (1 shl COUNTER_OFFSET));

      // goto nextchild_continue;
      if (NodeInfo and $ff00 = 0) then Break;
    until (False);

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
      {$ifNdef CPUX86}ChildSortValue := ChildNode.SortValue;{$endif}

      Left := PBufferCurrent^;
      if (Left.SortValue > {$ifNdef CPUX86}ChildSortValue{$else}ChildNode.SortValue{$endif}) then
      begin
       repeat
          PMapNodeBuffer(PBufferCurrent)[1] := Left;
          if (PBufferCurrent = @{$ifdef CPUX86}Store.{$endif}Buffer[0]) then Break;

          Dec(PBufferCurrent);
          Left := PBufferCurrent^;
          if (Left.SortValue > {$ifNdef CPUX86}ChildSortValue{$else}ChildNode.SortValue{$endif}) then Continue;
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

  // actualize node storage (new node pointer)
  TPathMapPtr(Store.Self).FInfo.NodeStorage.NewNode := Store.Info.NodeStorage.NewNode;
end;

function TPathMap.DoFindPath(const Parameters: TPathMapFindParameters): PPathMapResult;
var
  i: NativeUInt;
  MapWidth, MapHeight: Cardinal;
  FinishX, FinishY: Integer;
  S: PPoint;
begin
  // test start points coordinates
  MapWidth := Self.Width;
  MapHeight := Self.Height;
  S := Parameters.StartPoints;
  if (Parameters.StartPointsCount <> 0) then
  for i := 0 to Parameters.StartPointsCount - 1 do
  begin
    if (Cardinal(S.X) >= MapWidth) or (Cardinal(S.Y) >= MapHeight) then
      RaiseCoordinates(S.X, S.Y, 'start');

    Inc(S);
  end;

  // test finish point coordinates
  S := @Parameters.Finish;
  if (Cardinal(S.X) >= MapWidth) or (Cardinal(S.Y) >= MapHeight) then
    RaiseCoordinates(S.X, S.Y, 'finish');

  // test excluded points coordinates
  S := Parameters.ExcludedPoints;
  if (Parameters.ExcludedPointsCount <> 0) then
  for i := 0 to Parameters.ExcludedPointsCount - 1 do
  begin
    if (Cardinal(S.X) >= MapWidth) or (Cardinal(S.Y) >= MapHeight) then
      RaiseCoordinates(S.X, S.Y, 'excluded');

    Inc(S);
  end;

  // no start points case
  if (Parameters.StartPointsCount = 0) then
  begin
    Result := nil;
    Exit;
  end;

  // start is finish case
  FinishX := Parameters.Finish.X;
  FinishY := Parameters.Finish.Y;
  S := Parameters.StartPoints;
  for i := 0 to Parameters.StartPointsCount - 1 do
  begin
    if (S.X = FinishX) and (S.Y = FinishY) then
    begin
      FFindResultOnePoint.X := FinishX;
      FFindResultOnePoint.Y := FinishY;
      FFindResult.Points := Pointer(@FFindResultOnePoint);
      FFindResult.PointsCount := 1;
      FFindResult.Distance := 0;
      Result := @FFindResult;
      Exit;
    end;

    Inc(S);
  end;



end;
(*label
  calculate_result;
var
  StartNode, FinishNode, Node: PCPFMapNode;
  FailureNode: TCPFMapNode;
  Start: PPoint;
begin
  // todo

  Start := Parameters.StartPoints;

  // todo

  // get and inspect/fill start node
  StartNode := nil;//AllocateNode(Start.X, Start.Y);
  if (StartNode.NodeInfo and FLAG_KNOWN_PATH <> 0) then
  begin
    Node := StartNode;
    goto calculate_result;
  end;
  // todo
  StartNode.Path := 0;
  StartNode.SortValue := 0{Path} + CalculateHeuristics(Start^, Parameters.Finish);

  // cache finish point and mark as known attainable
  FinishNode := nil;//AllocateNode(Start.X, Start.Y);
  FinishNode.NodeInfo := FLAG_KNOWN_PATH or FLAG_ATTAINABLE;

  // run finding loop
  FailureNode.NodeInfo := FLAG_KNOWN_PATH {+ !FLAG_ATTAINABLE};
  StartNode.Next := @FailureNode;
  FailureNode.Prev := StartNode;
  Node := DoFindPathLoop(StartNode);
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
end; *)

function TPathMap.FindPath(const Parameters: TPathMapFindParameters): PPathMapResult;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  Result := DoFindPath(Parameters);
end;

function TPathMap.FindPath(const Start, Finish: TPoint;
  const Weights: TPathMapWeightsPtr; const ExcludedPoints: PPoint;
  const ExcludedPointsCount: NativeUInt): PPathMapResult;
var
  Parameters: TPathMapFindParameters;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  Parameters.StartPoints := @Start;
  Parameters.StartPointsCount := 1;
  Parameters.Finish := Finish;
  Parameters.Weights := Weights;
  Parameters.ExcludedPoints := ExcludedPoints;
  Parameters.ExcludedPointsCount := ExcludedPointsCount;
  Result := FindPath(Parameters);
end;

function TPathMap.FindPath(const StartPoints: PPoint; const StartPointsCount: NativeUInt;
   const Finish: TPoint; const Weights: TPathMapWeightsPtr = nil;
   const ExcludedPoints: PPoint = nil; const ExcludedPointsCount: NativeUInt = 0): PPathMapResult;
var
  Parameters: TPathMapFindParameters;
begin
  {$ifNdef CPFLIB}
    FCallAddress := ReturnAddress;
  {$endif}

  Parameters.StartPoints := StartPoints;
  Parameters.StartPointsCount := StartPointsCount;
  Parameters.Finish := Finish;
  Parameters.Weights := Weights;
  Parameters.ExcludedPoints := ExcludedPoints;
  Parameters.ExcludedPointsCount := ExcludedPointsCount;
  Result := FindPath(Parameters);
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
    {$ifdef TEST_INSERT}
      TestNodesInsertEx;
    {$else}
      //TestNodesInsert(0);
    {$endif}
  TPathMap(nil).DoFindPath(TPathMapFindParameters(nil^));
  TPathMapPtr(nil).DoFindPathLoop(nil);
  {$ifend}

end.
