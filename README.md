# CrystalPathFinding (CPF)
Эффективная и простая библиотека с открытым исходным кодом, предназначенная для поиска кратчайших путей по алгоритмам A*/WA* для карт, основанных на тайлах, с 4 (simple), 8 (diagonal/diagonalex) или 6 (hexagonal) соседями.

Особенности библиотеки:
* Кроссплатформенность: Windows x86/x64, Linux x86/x64, Mac OS, iOS, Android, Windows Mobile, а так же остальные платформы, доступные компиляторам Delphi и FreePascal 
* Экстремально высокая производительность 
* Умное построение пути 
* Лимит 16млн ячеек (например 4000x4000) 
* Секторальный тест 
* На выбор: ООП или процедурный интерфейс 
* Поддержка кеширования и поиска из нескольких стартовых точек 
* До 255 разновидностей тайлов, вес от 0.1 до 100.0 
* Содержит демонстрационные проекты

# Help
Библиотека содержит 2 главных объекта: TTileMap (карта тайлов) и TTileMapWeights (массив весов тайлов). Тайл может принимать любое значение от 1 до 255. Вес тайла указывается в диапазоне от 0.1 до 100.0, если вес тайла от 0.0 до 0.1 - то тайл считается непроходимым. Если при поиске TTileMapWeights не указан, то все веса считаются единичными. Тайл с номером 0 (TILE_BARRIER) считается барьером, проход через который невозможен. Режимы карт mkDiagonal и mkDiagonalEx отличаются тем, как пути огибают тайлы-барьеры.

Параметр SectorTest (секторальный тест) позволяет существенно увеличить производительность поиска если карта поделена на "области", и построить путь из одной области в другую невозможно.

Параметр Caching (кеширование) рекомендуется всегда устанавливать в true, если некоторое время предполагается не менять целевую точку и массив весов.

Большое значение имеет массив исключаемых точек (Excludes). С помощью этого параметра можно организовать сложные поиски, в которых например одни игровые единицы не соприкасаются с другими. В таких случаях рекомендуется перестраивать пути при смене целевой точки, численного состава или местоположения хотя бы одной игровой единицы. Соответственно если интересует не весь путь игровой единицы, а только следующая точка - рекомендуется параметр поиска FullPath указывать false (см. пример ниже).

##### Basic types and contants
```c
  // handle type
  typedef size_t TCPFHandle;

  // kind of map
  typedef unsigned char TTileMapKind; enum {mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal}; 

  // result of find path function
  struct TTileMapPath
  {
      size_t  Index;  
	  TPoint* Points;
      size_t  Count;
      double  Distance;
  };

  // path finding parameters
  struct TTileMapParams
  {
      TPoint* Starts;
      size_t StartsCount;
      TPoint Finish;
      TCPFHandle Weights;
      TPoint* Excludes;
      size_t ExcludesCount;
  };

  // map tile barrier
  #define TILE_BARRIER 0  
```
##### Tile weights object
```c
  struct TTileMapWeights
  {
	// constructor and destructor
	TTileMapWeights();
	~TTileMapWeights();

    // internal descriptor
    TCPFHandle Handle; 

    // tile weight values
	float getValue(unsigned char Tile);
	void setValue(unsigned char Tile, float Value);
  }; 
```

##### Tile map object
```c
  struct TTileMap
  {
	// constructor and destructor
	TTileMap(unsigned short MapWidth, unsigned short MapHeight, TTileMapKind MapKind, bool SameMapDiagonalWeight = false);
	~TTileMap();

    // internal descriptor
    TCPFHandle Handle; 

    // basic parameters
    unsigned short Width;
	unsigned short Height;
	TTileMapKind Kind;
	bool SameDiagonalWeight;

    // important variable parameters!
	bool SectorTest;
	bool Caching;

	// update methods
	void Clear();
	void Update(unsigned char* Tiles, unsigned short X, unsigned short Y, 
	  unsigned short UpdateWidth, unsigned UpdateHeight, size_i Pitch = 0);
	unsigned char getTile(unsigned short X, unsigned short Y);
	void setTile(unsigned short X, unsigned short Y, unsigned char Value);

	// path finding
	TTileMapPath FindPath(const TTileMapParams Params, bool FullPath = true);
	TTileMapPath FindPath(const TPoint Start, const TPoint Finish,
	  TCPFHandle Weights = 0, TPoint* Excledes = NULL, size_t ExcludesCount = 0, bool FullPath = true);
	TTileMapPath FindPath(TPoint* Starts, size_t StartsCount, const TPoint Finish,
	  TCPFHandle Weights = 0, TPoint* Excledes = NULL, size_t ExcludesCount = 0, bool FullPath = true);
  }; 
```

##### Example of many game units paths updating
```c
void Game::UpdateBotsPaths()
{
   // clear one point paths
   for (int i = 0; i < Bots.Count; i++)
     Bots[i].NextPoint = Bots.Point;
   
   // finding next path point for every unit
   for (int i = 0; i < Bots.Count; i++)
   {
      // detect excluded points
      vector<TPoint> Excludes;
      for (int j = 0; j < Bots.Count; j++)
      if (i != j)
      {
         Excludes.push_back(Bots[j].Point);

         if (Bots[j].Point != Bots[j].NextPoint)
           Excludes.push_back(Bots[j].NextPoint);
      }
    
      // fill parameters, find path (one next point)
      TTileMapParams Params;
      Params.Starts = &Bots[i].Point;
      Params.StartsCount = 1;
      Params.Finish = this.TargetPoint;
      Params.Weights = NULL/* this.Weights.Handle */;
      Params.ExcludesCount = Excludes.size(); 
      if (Params.ExcludesCount) Params.Excludes = &Excludes[0];
      TTileMapPath Path = this.Map.FindPath(Params, false/* false means next point needed only */);
      if (Path.Count > 1)
        Bots[i].NextPoint = Path.Points[1];
   }
}
```