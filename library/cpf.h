/* ******************************************************************* */
/* "Crystal Path Finding" (cpf) is a very small part of CrystalEngine, */
/* that helps to find the shortest paths with A*-WA* algorithms.       */
/*                                                                     */
/* email: softforyou@inbox.ru                                          */
/* skype: dimandevil                                                   */
/* repository: https://github.com/d-mozulyov/CrystalPathFinding        */
/* ******************************************************************* */

#ifndef CRYSTAL_PATH_FINDING_H
#define CRYSTAL_PATH_FINDING_H

#include <Windows.h>

// used types
typedef POINT TPoint;
typedef unsigned short word;
typedef size_t TCPFHandle;
typedef unsigned char TPathMapTile;
typedef unsigned char TPathMapKind; enum {mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal}; /*sizeof(TPathMapKind) = 1*/

struct TPathMapResult
{
	TPoint* Points;
    size_t  PointsCount;
    double  Distance;
};

ToDo: cpfInitialize

//  initialization/finalization  routine
namespace cpf_routine
{
  HINSTANCE cpf_dll=0;

  typedef TCPFHandle (*CPF_PROC_CREATE_WEIGHTS)(TPathMapTile HighTile);
  typedef void (*CPF_PROC_DESTROY_WEIGHTS)(TCPFHandle& HWeights);
  typedef float (*CPF_PROC_WEIGHT_GET)(TCPFHandle HWeights, TPathMapTile Tile);
  typedef void (*CPF_PROC_WEIGHT_SET)(TCPFHandle HWeights, TPathMapTile Tile, float Value);
  typedef TCPFHandle (*CPF_PROC_CREATE_MAP)(word Width, word Height, TPathMapKind Kind, TPathMapTile HighTile);
  typedef void (*CPF_PROC_DESTROY_MAP)(TCPFHandle& HMap);
  typedef void (*CPF_PROC_MAP_CLEAR)(TCPFHandle HMap);
  typedef TPathMapTile (*CPF_PROC_MAP_GET_TILE)(TCPFHandle HMap, word X, word Y);
  typedef void (*CPF_PROC_MAP_SET_TILE)(TCPFHandle HMap, word X, word Y, TPathMapTile Value);
  typedef void (*CPF_PROC_MAP_UPDATE)(TCPFHandle HMap, TPathMapTile* Tiles, word X, word Y, word Width, word Height, signed size_t Pitch);
  typedef TPathMapResult* (*CPF_PROC_FIND_PATH)(TCPFHandle HMap, TPoint Start, TPoint Finish, TCPFHandle Weights, TPoint* ExcludedPoints, size_t ExcludedPointsCount, bool SectorTest, bool UseCache);

  CPF_PROC_CREATE_WEIGHTS __cpfCreateWeights = NULL;
  CPF_PROC_DESTROY_WEIGHTS __cpfDestroyWeights = NULL;
  CPF_PROC_WEIGHT_GET __cpfWeightGet = NULL;
  CPF_PROC_WEIGHT_SET __cpfWeightSet = NULL;
  CPF_PROC_CREATE_MAP __cpfCreateMap = NULL;
  CPF_PROC_DESTROY_MAP __cpfDestroyMap = NULL;
  CPF_PROC_MAP_CLEAR __cpfMapClear = NULL;
  CPF_PROC_MAP_GET_TILE __cpfMapGetTile = NULL;
  CPF_PROC_MAP_SET_TILE __cpfMapSetTile = NULL;
  CPF_PROC_MAP_UPDATE __cpfMapUpdate = NULL;
  CPF_PROC_FIND_PATH __cpfFindPath = NULL;


struct TCpfInitializator
{
  TCpfInitializator()
  {
    if (cpf_dll) return;
    cpf_dll = LoadLibraryA("cpf.dll");
    if (!cpf_dll) return;

    __cpfCreateWeights = (CPF_PROC_CREATE_WEIGHTS)GetProcAddress(cpf_dll, "cpfCreateWeights");
    __cpfDestroyWeights = (CPF_PROC_DESTROY_WEIGHTS)GetProcAddress(cpf_dll, "cpfDestroyWeights");
    __cpfWeightGet = (CPF_PROC_WEIGHT_GET)GetProcAddress(cpf_dll, "cpfWeightGet");
    __cpfWeightSet = (CPF_PROC_WEIGHT_SET)GetProcAddress(cpf_dll, "cpfWeightSet");
    __cpfCreateMap = (CPF_PROC_CREATE_MAP)GetProcAddress(cpf_dll, "cpfCreateMap");
    __cpfDestroyMap = (CPF_PROC_DESTROY_MAP)GetProcAddress(cpf_dll, "cpfDestroyMap");
    __cpfMapClear = (CPF_PROC_MAP_CLEAR)GetProcAddress(cpf_dll, "cpfMapClear");
    __cpfMapUpdate = (CPF_PROC_MAP_UPDATE)GetProcAddress(cpf_dll, "cpfMapUpdate");
    __cpfMapGetTile = (CPF_PROC_MAP_GET_TILE)GetProcAddress(cpf_dll, "cpfMapGetTile");
    __cpfMapSetTile = (CPF_PROC_MAP_SET_TILE)GetProcAddress(cpf_dll, "cpfMapSetTile");
    __cpfFindPath = (CPF_PROC_FIND_PATH)GetProcAddress(cpf_dll, "cpfFindPath");
  }

  ~TCpfInitializator()
  {
     if (cpf_dll)
     {
       FreeLibrary(cpf_dll);
       cpf_dll = 0;
     }
  }
};

  TCpfInitializator CpfInitializator;
};




//  -------------------------  used functions  --------------------------------
TCPFHandle cpfCreateWeights(TPathMapTile HighTile)/*;*/{return cpf_routine::__cpfCreateWeights(HighTile);}
void    cpfDestroyWeights(TCPFHandle& HWeights)/*;*/{cpf_routine::__cpfDestroyWeights(HWeights);}
float   cpfWeightGet(TCPFHandle HWeights, TPathMapTile Tile)/*;*/{return cpf_routine::__cpfWeightGet(HWeights, Tile);}
void    cpfWeightSet(TCPFHandle HWeights, TPathMapTile Tile, float Value)/*;*/{cpf_routine::__cpfWeightSet(HWeights, Tile, Value);}
TCPFHandle cpfCreateMap(word Width, word Height, TPathMapKind Kind=mkSimple, TPathMapTile HighTile=0)/*;*/{return cpf_routine::__cpfCreateMap(Width, Height, Mode, HighTile);}
void    cpfDestroyMap(TCPFHandle& HMap)/*;*/{cpf_routine::__cpfDestroyMap(HMap);}
void    cpfMapClear(TCPFHandle HMap)/*;*/{cpf_routine::__cpfMapClear(HMap);}
TPathMapTile cpfMapGetTile(TCPFHandle HMap, word X, word Y)/*;*/{return cpf_routine::__cpfMapGetTile(HMap, X, Y);}
void    cpfMapSetTile(TCPFHandle HMap, word X, word Y, TPathMapTile Value)/*;*/{cpf_routine::__cpfMapSetTile(HMap, X, Y, Value);};
void    cpfMapUpdate(TCPFHandle HMap, TPathMapTile* Tiles, word X, word Y, word Width, word Height, signed size_t Pitch=0)/*;*/{cpf_routine::__cpfMapUpdate(HMap, Tiles, X, Y, Width, Height, Pitch);}
TPathMapResult* cpfFindPath(TCPFHandle HMap, TPoint Start, TPoint Finish, TCPFHandle Weights=0, TPoint* ExcludedPoints=NULL, size_t ExcludedPointsCount=0, bool SectorTest=true, bool UseCache=true)/*;*/{return cpf_routine::__cpfFindPath(HMap, Start, Finish, Weights, ExcludedPoints, ExcludedPointsCount, SectorTest, UseCache);}



#endif
