{ *********************************************************************** }
{ "Crystal Path Finding" (cpf) is a very small part of CrystalEngine,     }
{ that helps to find the shortest paths with A*/WA* algorithms.           }
{                                                                         }
{ email: softforyou@inbox.ru                                              }
{ skype: dimandevil                                                       }
{ repository: https://github.com/d-mozulyov/CrystalPathFinding            }
{ *********************************************************************** }


unit cpf;

interface
  uses Types;


type
  // mode of map 
  TPathMapMode = (mmSimple, mmDiagonal, mmDiagonalEx, mmHexagonal45, mmHexagonal60);

  // points as array
  PPointList = ^TPointList;
  TPointList = array[0..high(integer) div sizeof(TPoint) - 1] of TPoint;

  // Result of FindPath() function
  TPathMapResult = packed record
    points: PPointList;
    points_count: integer;
    distance: double;
  end;
  PPathMapResult = ^TPathMapResult;


const
  cpf_lib = 'cpf.dll';

function  cpfCreateWeights(HighTile: byte): THandle; cdecl; external cpf_lib;
procedure cpfDestroyWeights(var HWeights: THandle); cdecl; external cpf_lib;
function  cpfWeightGet(HWeights: THandle; Tile: byte): single; cdecl; external cpf_lib;
procedure cpfWeightSet(HWeights: THandle; Tile: byte; Value: single); cdecl; external cpf_lib;
function  cpfCreateMap(Width, Height: word; Mode: TPathMapMode=mmSimple; HighTile: byte=0; SmartWeight: boolean=true): THandle; cdecl; external cpf_lib;
procedure cpfDestroyMap(var HMap: THandle); cdecl; external cpf_lib;
procedure cpfMapClear(HMap: THandle); cdecl; external cpf_lib;
procedure cpfMapUpdate(HMap: THandle; Tiles: pbyte; X, Y, Width, Height: word; pitch: integer=0); cdecl; external cpf_lib;
function  cpfMapGetTile(HMap: THandle; X, Y: word): byte; cdecl; external cpf_lib;
procedure cpfMapSetTile(HMap: THandle; X, Y: word; Value: byte); cdecl; external cpf_lib;
function  cpfFindPath(HMap: THandle; Start, Finish: TPoint; HWeights: THandle=0; ExcludePoints: PPoint=nil; ExcludePointsCount: integer=0; SectorTest: boolean=true): PPathMapResult; cdecl; external cpf_lib;

implementation

end.
