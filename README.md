# CrystalPathFinding (CPF)
The fastest and most simple A*/WA* library

При проектировании системы учитывались реальные игровые моменты, реальные условия простой библиотеки. Она кстати не ориентирована на мегасерьёзные проекты. Существует карта тайлов, каждый тайл имеет свой вес, необходимой найти кратчайшее расстояние из точки A в точку B. Существуют так же препятствия - клетки, прохождение через которые невозможно.

В реальных играх приходится искать пути для многих игровых сущностей. Например танки одинаково хорошо идут по земле, асфальту, гальке. А прохождение по воде например или по горам - для них представляет большую сложность. Пехотинец хорошо идёт по асфальту, но очень плохо по болотам. Катера прекрасно перемещаются по воде и вообще не перемещаются по суше. Поэтому в библиотеке для поиска путей используется понятие массива весов. В реальных условиях для танка массив весов один, для пехотинца - второй, для катеров - третий.

API очень простое, привожу его на 2х распространённых языках: С/C++ и Pascal

typedef POINT TPoint;
typedef unsigned char byte;
typedef unsigned short word;
typedef unsigned long THandle;
typedef byte TPathMapMode; enum {mmSimple, mmDiagonal, mmDiagonalEx, mmHexagonal45, mmHexagonal60}; /*sizeof(TPathMapMode) = 1*/

TPathMapMode = (mmSimple, mmDiagonal, mmDiagonalEx, mmHexagonal45, mmHexagonal60);

Создание массива весов. (Weights). 
HighTile - максимальный используемый номер тайла.
   THandle cpfCreateWeights(byte HighTile);
   function cpfCreateWeights(HighTile: byte): THandle; 
   
Удаление массива весов. (Weights).
   void cpfDestroyWeights(THandle& HWeights); 
   procedure cpfDestroyWeights(var HWeights: THandle);    
   
Изменение веса для конкретного тайла. 
Например веса танков могут быть следующие. Земля = 1.0, асфальт = 1.2, галька = 1.5, булыжники = 2.8, вода = 0.0 
Все веса <= 0.1 считаются как непроходимые (0.0). В нашем примере непроходимыми считаются тайлы воды
   float cpfWeightGet(THandle HWeights, byte Tile); 
   void cpfWeightSet(THandle HWeights, byte Tile, float Value); 
   function  cpfWeightGet(HWeights: THandle; Tile: byte): single; 
   procedure cpfWeightSet(HWeights: THandle; Tile: byte; Value: single);    
   
Создание карты. 
   Width - ширина карты 
   Height - высота карты 
   HighTile - максимальный используемый номер тайла. Значение 255 (0xFF) зарезервировано для препятствий 
   SmartWeight - "умный расчёт весов". Стоимость перехода из тайла A в тайл B обычно равен весу A. Но в реальных игровых условиях соседствующие тайлы мешаются и стоимость перехода в таких "умных" случаях равен (весA + весB) / 2. 
   Mode различает следующие значения: 
   mmSimple - простая карта. Различаются ходы влево, вправо, вверх, вниз 
   mmDiagonal - кроме простых переходов, добавляются ещё и диагональные 
   mmDiagonalEx - диагональные переходы с огибанием препятствий 
   mmHexagonal45 - гексагональное поле "с углом поворота" гекса 45° 
   mmHexagonal60 - гексагональное поле "с углом поворота" гекса 60°

   THandle cpfCreateMap(word Width, word Height, TPathMapMode Mode=mmSimple, byte HighTile=0, bool SmartWeight=true);
   function  cpfCreateMap(Width, Height: word; Mode: TPathMapMode=mmSimple; HighTile: byte=0; SmartWeight: boolean=true): THandle;
   
Удаление карты
   void cpfDestroyMap(THandle& HMap);
   procedure cpfDestroyMap(var HMap: THandle);
   
Очистка карты (все тайлы становятся равными 0)
   void cpfMapClear(THandle HMap); 
   procedure cpfMapClear(HMap: THandle);   
   
Изменение тайлов карты
   byte cpfMapGetTile(THandle HMap, word X, word Y); 
   void cpfMapSetTile(THandle HMap, word X, word Y, byte Value); 
   function  cpfMapGetTile(HMap: THandle; X, Y: word): byte; 
   procedure cpfMapSetTile(HMap: THandle; X, Y: word; Value: byte);   
   
Изменение прямоугольной области карты 
   Map - карта 
   Tiles - массив тайлов, которые необходимо занести в карту 
   X, Y, Width, Height - прямоугольная область карты, которую меняем 
   pitch - смещение в массиве Tiles для каждой линии. Если pitch оставить 0, то он будет равен Width
   void cpfMapUpdate(THandle HMap, byte* Tiles, word X, word Y, word Width, word Height, int pitch=0); 
   procedure cpfMapUpdate(HMap: THandle; Tiles: pbyte; X, Y, Width, Height: word; pitch: integer=0);   
  
Самая главная функция библиотеки - это конечно cpfFindPath (поиск пути) 
   HMap - карта, в которой производим поиск 
   Start - стартовая точка пути 
   Finish - конечная точка пути 
   Weights - массив весов, для которого ищется кратчайший путь. Если указать 0, то вес каждого тайла будет считаться как 1.0 
   ExcludePoints - исключаемые точки. Мощная особенность библиотеки, позволяющая дополнительно указывать "препятствия" во время расчёта пути. Например вы программируете алгоритм, в котором танки не могут проходить сквозь танки. В этом случае разумно не менять карту для каждого очередного расчёта, а указать "исключаемые точки" в параметре функции. 
   ExcludePointsCount - количество исключаемых точек 
   SectorTest - очень интересный флаг. Если карта представляет собой несколько "областей", причём из одной нельзя попасть в другую, то функция поиска в этом случае будет выполняться очень долго, т.к. алгоритм переберёт все возможные варианты. SectorTest проверяет принадлежность точек одному сектору и не запускает сложный алгоритм поиска если цель изначально недостижима. Но алгоритм требует анализа карты и разбиения на сектора, поэтому на больших картах и/или часто изменяющихся, алгоритм может дать провал производительности

В качестве результата функции возвращается указатель на структуру TPathMapResult. 
Если вернулся NULL - то путь не найден 
   points - массив точек пути от стартовой до конечной 
   points_count - размер массива точек 
   distance - расстояние от стартовой точки до конечной, по найденному маршруту  
   
TPathMapResult* cpfFindPath(THandle HMap, TPoint Start, TPoint Finish, THandle Weights=0, TPoint* ExcludePoints=NULL, int ExcludePointsCount=0, bool SectorTest=true);
function cpfFindPath(HMap: THandle; Start, Finish: TPoint; Weights: THandle=0; ExcludePoints: PPoint=nil; ExcludePointsCount: integer=0; SectorTest: boolean=true): PPathMapResult; 

   struct TPathMapResult
   {
     TPoint* points;
     int points_count;
     double distance;
   };

   TPathMapResult = record
     points: PPoint;
     points_count: integer;
     distance: double;
   end;   