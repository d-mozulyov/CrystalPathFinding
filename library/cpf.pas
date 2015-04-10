{ *********************************************************************** }
{ "Crystal Path Finding" (cpf) is a very small part of CrystalEngine,     }
{ that helps to find the shortest paths with A*/WA* algorithms.           }
{                                                                         }
{ email: softforyou@inbox.ru                                              }
{ skype: dimandevil                                                       }
{ repository: https://github.com/d-mozulyov/CrystalPathFinding            }
{ *********************************************************************** }

{.$define NOEXCEPTION}

unit cpf;

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
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}

interface
  uses Types 
       {$ifNdef NOEXCEPTION}
         {$ifdef KOL}
           , KOL, err
         {$else}
           , SysUtils
         {$endif}
       {$endif};

type
  // kind of map 
  TPathMapKind = (mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal);

  // points as array
  PPointList = ^TPointList;
  TPointList = array[0..High(Integer) div sizeof(TPoint) - 1] of TPoint;

  // native types
  {$ifNdef FPC}
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
  {$ifNdef NOEXCEPTION}
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
  
  // Result of FindPath() function
  TPathMapResult = packed record
    Points: PPointList;
    PointsCount: NativeUInt;
    Distance: Double;
  end;
  PPathMapResult = ^TPathMapResult;

  // handle type
  PCPFHandle = ^TCPFHandle;  
  TCPFHandle = type NativeUInt;    
  
const
  cpf_lib = 'cpf.dll';

function  cpfCreateWeights(HighTile: Byte): TCPFHandle; cdecl; external cpf_lib;  
procedure cpfDestroyWeights(var HWeights: TCPFHandle); cdecl; external cpf_lib;  
function  cpfWeightGet(HWeights: TCPFHandle; Tile: Byte): Single; cdecl; external cpf_lib;  
procedure cpfWeightSet(HWeights: TCPFHandle; Tile: Byte; Value: Single); cdecl; external cpf_lib;  
function  cpfCreateMap(Width, Height: Word; Kind: TPathMapKind = mkSimple; HighTile: Byte = 0): TCPFHandle; cdecl; external cpf_lib;  
procedure cpfDestroyMap(var HMap: TCPFHandle); cdecl; external cpf_lib;  
procedure cpfMapClear(HMap: TCPFHandle); cdecl; external cpf_lib;  
procedure cpfMapUpdate(HMap: TCPFHandle; Tiles: PByte; X, Y, Width, Height: Word; Pitch: NativeInt = 0); cdecl; external cpf_lib;  
function  cpfMapGetTile(HMap: TCPFHandle; X, Y: Word): Byte; cdecl; external cpf_lib;  
procedure cpfMapSetTile(HMap: TCPFHandle; X, Y: Word; Value: Byte); external cpf_lib;  
function  cpfFindPath(HMap: TCPFHandle; Start, Finish: TPoint; HWeights: TCPFHandle = 0; ExcludePoints: PPoint = nil; ExcludePointsCount: NativeUInt = 0; SectorTest: Boolean = True; UseCache: Boolean = True): PPathMapResult; cdecl; external cpf_lib;  

implementation
var
  // todo FPC
  MemoryManager: {$if CompilerVersion < 18}TMemoryManager{$else}TMemoryManagerEx{$ifend};

  
{ ECrystalPathFinding }  
  
{$if Defined(KOL) and (not Defined(NOEXCEPTION)))}
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
  Result := MemoryManager.FreeMem(P) <> 0;
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

  {$ifdef NOEXCEPTION}
    {$ASSERTIONS ON}
    Assert(False, Text);
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
  System.GetMemoryManager(MemoryManager);
  cpfInitialize(CPFCallbacks);

end.
