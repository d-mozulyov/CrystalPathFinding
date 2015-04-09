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
typedef unsigned char byte;
typedef unsigned short word;
typedef unsigned long THandle;
typedef byte TPathMapMode; enum {mmSimple, mmDiagonal, mmDiagonalEx, mmHexagonal45, mmHexagonal60}; /*sizeof(TPathMapMode) = 1*/

struct TPathMapResult
{
	TPoint* points;
    long    points_count;
    double  distance;
};


//  initialization/finalization  routine
namespace cpf_routine
{
  HINSTANCE cpf_dll=0;

  typedef THandle (*CPF_PROC_CREATE_WEIGHTS)(byte HighTile);
  typedef void (*CPF_PROC_DESTROY_WEIGHTS)(THandle& HWeights);
  typedef float (*CPF_PROC_WEIGHT_GET)(THandle HWeights, byte Tile);
  typedef void (*CPF_PROC_WEIGHT_SET)(THandle HWeights, byte Tile, float Value);
  typedef THandle (*CPF_PROC_CREATE_MAP)(word Width, word Height, TPathMapMode Mode, byte HighTile, bool SmartWeight);
  typedef void (*CPF_PROC_DESTROY_MAP)(THandle& HMap);
  typedef void (*CPF_PROC_MAP_CLEAR)(THandle HMap);
  typedef byte (*CPF_PROC_MAP_GET_TILE)(THandle HMap, word X, word Y);
  typedef void (*CPF_PROC_MAP_SET_TILE)(THandle HMap, word X, word Y, byte Value);
  typedef void (*CPF_PROC_MAP_UPDATE)(THandle HMap, byte* Tiles, word X, word Y, word Width, word Height, long pitch);
  typedef TPathMapResult* (*CPF_PROC_FIND_PATH)(THandle HMap, TPoint Start, TPoint Finish, THandle Weights, TPoint* ExcludePoints, long ExcludePointsCount, bool SectorTest);

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
THandle cpfCreateWeights(byte HighTile)/*;*/{return cpf_routine::__cpfCreateWeights(HighTile);}
void    cpfDestroyWeights(THandle& HWeights)/*;*/{cpf_routine::__cpfDestroyWeights(HWeights);}
float   cpfWeightGet(THandle HWeights, byte Tile)/*;*/{return cpf_routine::__cpfWeightGet(HWeights, Tile);}
void    cpfWeightSet(THandle HWeights, byte Tile, float Value)/*;*/{cpf_routine::__cpfWeightSet(HWeights, Tile, Value);}
THandle cpfCreateMap(word Width, word Height, TPathMapMode Mode=mmSimple, byte HighTile=0, bool SmartWeight=true)/*;*/{return cpf_routine::__cpfCreateMap(Width, Height, Mode, HighTile, SmartWeight);}
void    cpfDestroyMap(THandle& HMap)/*;*/{cpf_routine::__cpfDestroyMap(HMap);}
void    cpfMapClear(THandle HMap)/*;*/{cpf_routine::__cpfMapClear(HMap);}
byte    cpfMapGetTile(THandle HMap, word X, word Y)/*;*/{return cpf_routine::__cpfMapGetTile(HMap, X, Y);}
void    cpfMapSetTile(THandle HMap, word X, word Y, byte Value)/*;*/{cpf_routine::__cpfMapSetTile(HMap, X, Y, Value);};
void    cpfMapUpdate(THandle HMap, byte* Tiles, word X, word Y, word Width, word Height, long pitch=0)/*;*/{cpf_routine::__cpfMapUpdate(HMap, Tiles, X, Y, Width, Height, pitch);}
TPathMapResult* cpfFindPath(THandle HMap, TPoint Start, TPoint Finish, THandle Weights=0, TPoint* ExcludePoints=NULL, int ExcludePointsCount=0, bool SectorTest=true)/*;*/{return cpf_routine::__cpfFindPath(HMap, Start, Finish, Weights, ExcludePoints, ExcludePointsCount, SectorTest);}



#endif
