{ *********************************************************************** }
{ "Crystal Path Finding" (cpf) is a very small part of CrystalEngine,     }
{ that helps to find the shortest paths with A*/WA* algorithms.           }
{                                                                         }
{ email: softforyou@inbox.ru                                              }
{ skype: dimandevil                                                       }
{ repository: https://github.com/d-mozulyov/CrystalPathFinding            }
{ *********************************************************************** }

{.$define NOEXCEPTIONS}

unit cpf;

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
{$endif}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}

interface
  uses Types 
       {$ifNdef NOEXCEPTIONS}
         {$ifdef KOL}
           , KOL, err
         {$else}
           , SysUtils
         {$endif}
       {$endif};

{$if Defined(FPC) or (CompilerVersion < 22) or (not Defined(NOEXCEPTIONS))}
type
{$ifend}
  // standard types
  {$ifdef FPC}
    Integer = Longint;
    PInteger = ^Integer;
  {$else}
    {$if CompilerVersion < 19}
      NativeInt = Integer;
      NativeUInt = Cardinal;
    {$ifend}
    {$if CompilerVersion < 22}
      PNativeInt = ^NativeInt;
      PNativeUInt = ^NativeUInt;
    {$ifend}
  {$endif}

  // exception class
  {$ifNdef NOEXCEPTIONS}
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
  // handle type
  PCPFHandle = ^TCPFHandle;
  TCPFHandle = type NativeUInt;

  // kind of map
  TTileMapKind = (mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal);

  // points as array
  PPointList = ^TPointList;
  TPointList = array[0..High(Integer) div SizeOf(TPoint) - 1] of TPoint;

  // result of find path function
  TTileMapPath = packed record
    Index: NativeUInt;
    Points: PPointList;
    Count: NativeInt;
    Distance: Double;
  end;

  // path finding parameters
  TTileMapParams = record
    Starts: PPoint;
    StartsCount: NativeUInt;
    Finish: TPoint;
    Weights: TCPFHandle;
    Excludes: PPoint;
    ExcludesCount: NativeUInt;
  end;
  PTileMapParams = ^TTileMapParams;

  // object oriented Weights interface
  TTileMapWeights = class(TObject)
  private
    FHandle: TCPFHandle;

    function GetValue(const Tile: Byte): Single; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure SetValue(const Tile: Byte; const Value: Single); {$ifdef INLINESUPPORT}inline;{$endif}
  {$ifdef AUTOREFCOUNT}protected{$else}public{$endif}
    destructor Destroy; override;
  public
    constructor Create;
    procedure Clear;

    property Handle: TCPFHandle read FHandle;
    property Values[const Tile: Byte]: Single read GetValue write SetValue; default;
  end;

  // object oriented Map interface
  TTileMap = class(TObject)
  private
    FHandle: TCPFHandle;
    FWidth: Word;
    FHeight: Word;
    FKind: TTileMapKind;
    FSameDiagonalWeight: Boolean;
    FSectorTest: Boolean;
    FCaching: Boolean;

    function GetTile(const X, Y: Word): Byte; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure SetTile(const X, Y: Word; const Value: Byte); {$ifdef INLINESUPPORT}inline;{$endif}
  {$ifdef AUTOREFCOUNT}protected{$else}public{$endif}
    destructor Destroy; override;
  public
    constructor Create(const AWidth, AHeight: Word; const AKind: TTileMapKind; const ASameDiagonalWeight: Boolean = False);
    procedure Clear; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure Update(const ATiles: PByte; const X, Y, AWidth, AHeight: Word; const Pitch: NativeInt = 0); {$ifdef INLINESUPPORT}inline;{$endif}

    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property Kind: TTileMapKind read FKind;
    property SameDiagonalWeight: Boolean read FSameDiagonalWeight;
    property SectorTest: Boolean read FSectorTest write FSectorTest;
    property Caching: Boolean read FCaching write FCaching;
    property Handle: TCPFHandle read FHandle;
    property Tiles[const X, Y: Word]: Byte read GetTile write SetTile; default;

    function FindPath(const Params: TTileMapParams; const FullPath: Boolean = True): TTileMapPath; overload; {$ifdef INLINESUPPORT}inline;{$endif}
    function FindPath(const Start, Finish: TPoint; const Weights: TCPFHandle = 0;
      const Excludes: PPoint = nil; const ExcludesCount: NativeUInt = 0;
      const FullPath: Boolean = True): TTileMapPath; overload; {$ifdef INLINESUPPORT}inline;{$endif}
    function FindPath(const Starts: PPoint; const StartsCount: NativeUInt;
      const Finish: TPoint; const Weights: TCPFHandle = 0;
      const Excludes: PPoint = nil; const ExcludesCount: NativeUInt = 0;
      const FullPath: Boolean = True): TTileMapPath; overload; {$ifdef INLINESUPPORT}inline;{$endif}
  end;


{ Dynamic link library API }

const
  cpf_lib = 'cpf.dll';

function  cpfCreateWeights: TCPFHandle; cdecl; external cpf_lib;
procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl; external cpf_lib;
function  cpfWeightGet(HWeights: TCPFHandle; Tile: Byte): Single; cdecl; external cpf_lib;
procedure cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single); cdecl; external cpf_lib;
procedure cpfWeightsClear(HWeights: TCPFHandle); cdecl; external cpf_lib;
function  cpfCreateMap(Width, Height: Word; Kind: TTileMapKind; SameDiagonalWeight: Boolean = False): TCPFHandle; cdecl; external cpf_lib;
procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl; external cpf_lib;
procedure cpfMapClear(HMap: TCPFHandle); cdecl; external cpf_lib;
procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt = 0); cdecl; external cpf_lib;
function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): Byte; cdecl; external cpf_lib;
procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte); cdecl; external cpf_lib;
function  cpfFindPath(HMap: TCPFHandle; Params: PTileMapParams; SectorTest: Boolean = False; Caching: Boolean = True; FullPath: Boolean = True): TTileMapPath; cdecl; external cpf_lib;

implementation
var
  MemoryManager: {$if Defined(FPC) or (CompilerVersion < 18)}TMemoryManager{$else}TMemoryManagerEx{$ifend};

  
{ ECrystalPathFinding }  
  
{$if Defined(KOL) and (not Defined(NOEXCEPTIONS)))}
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


{ TTileMapWeights }

constructor TTileMapWeights.Create;
begin
  inherited Create;
  FHandle := cpfCreateWeights;
end;

destructor TTileMapWeights.Destroy;
begin
  cpfDestroyWeights(FHandle);
  inherited;
end;

function TTileMapWeights.GetValue(const Tile: Byte): Single;
begin
  Result := cpfWeightGet(FHandle, Tile);
end;

procedure TTileMapWeights.SetValue(const Tile: Byte;
  const Value: Single);
begin
  cpfWeightSet(FHandle, Tile, Value);
end;

procedure TTileMapWeights.Clear;
begin
  cpfWeightsClear(FHandle);
end;


{ TTileMap }

constructor TTileMap.Create(const AWidth, AHeight: Word; const AKind: TTileMapKind;
  const ASameDiagonalWeight: Boolean);
begin
  inherited Create;

  FWidth := AWidth;
  FHeight := AHeight;
  FKind := AKind;
  FSameDiagonalWeight := (not (AKind in [mkDiagonal, mkDiagonalEx])) or ASameDiagonalWeight;
  FSectorTest := False;
  FCaching := True;

  FHandle := cpfCreateMap(AWidth, AHeight, AKind, ASameDiagonalWeight);
end;

destructor TTileMap.Destroy;
begin
  cpfDestroyMap(FHandle);
  inherited;
end;

function TTileMap.GetTile(const X, Y: Word): Byte;
begin
  Result := cpfMapGetTile(FHandle, X, Y);
end;

procedure TTileMap.SetTile(const X, Y: Word; const Value: Byte);
begin
  cpfMapSetTile(FHandle, X, Y, Value);
end;

procedure TTileMap.Update(const ATiles: PByte; const X, Y, AWidth,
  AHeight: Word; const Pitch: NativeInt);
begin
  cpfMapUpdate(FHandle, ATiles, X, Y, AWidth, AHeight, Pitch);
end;

procedure TTileMap.Clear;
begin
  cpfMapClear(FHandle);
end;

function TTileMap.FindPath(const Params: TTileMapParams; const FullPath: Boolean): TTileMapPath;
begin
  Result := cpfFindPath(FHandle, @Params, FSectorTest, FCaching, FullPath);
end;

function TTileMap.FindPath(const Start, Finish: TPoint;
  const Weights: TCPFHandle; const Excludes: PPoint;
  const ExcludesCount: NativeUInt; const FullPath: Boolean): TTileMapPath;
var
  Params: TTileMapParams;
begin
  Params.Starts := @Start;
  Params.StartsCount := 1;
  Params.Finish := Finish;
  Params.Weights := Weights;
  Params.Excludes := Excludes;
  Params.ExcludesCount := ExcludesCount;

  Result := cpfFindPath(FHandle, @Params, FSectorTest, FCaching, FullPath);
end;

function TTileMap.FindPath(const Starts: PPoint; const StartsCount: NativeUInt;
   const Finish: TPoint; const Weights: TCPFHandle;
   const Excludes: PPoint; const ExcludesCount: NativeUInt;
   const FullPath: Boolean): TTileMapPath;
var
  Params: TTileMapParams;
begin
  Params.Starts := Starts;
  Params.StartsCount := StartsCount;
  Params.Finish := Finish;
  Params.Weights := Weights;
  Params.Excludes := Excludes;
  Params.ExcludesCount := ExcludesCount;

  Result := cpfFindPath(FHandle, @Params, FSectorTest, FCaching, FullPath);
end;


{ Low level callbacks }

type
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
  
function CPFAlloc(Size: NativeUInt): Pointer; cdecl;
begin
  Result := MemoryManager.GetMem(Size);
end;

function CPFFree(P: Pointer): Boolean; cdecl;
begin
  Result := (MemoryManager.FreeMem(P) = 0);
end;

function CPFRealloc(P: Pointer; Size: NativeUInt): Pointer; cdecl;
begin
  Result := MemoryManager.ReallocMem(P, Size);
end;

procedure CPFException(Message: PWideChar; Address: Pointer); cdecl;
var
  Text: string;
begin
  Text := Message;

  {$ifdef NOEXCEPTIONS}
    // Assert(False, Text, Address);
    if Assigned(AssertErrorProc) then
    begin
      AssertErrorProc(Text, 'cpf.pas', 0, Address)
    end else
    begin
      System.ErrorAddr := Address;
      System.ExitCode := 207{reInvalidOp};
      System.Halt;
    end;
  {$else}
    raise ECrystalPathFinding.Create(Text) at Address;
  {$endif}
end;


const
  CPFCallbacks: TCPFCallbacks = (
    Alloc: CPFAlloc;
    Free: CPFFree;
    Realloc: CPFRealloc;
    Exception: CPFException; 
  );

procedure cpfInitialize(const Callbacks: TCPFCallbacks); cdecl; external cpf_lib;    


initialization
  {$WARNINGS OFF} // deprecated warning bug fix (like Delphi 2010 compiler)
  System.GetMemoryManager(MemoryManager);
  cpfInitialize(CPFCallbacks);

end.
