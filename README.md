# CrystalPathFinding (CPF)
Simple and effective library with an open source intended for the searching of the shortest paths by algorithms A\*/WA\* for maps based on tiles with 4 (simple), 8 (diagonal/diagonalex) or 6 (hexagonal) neighbors. 

Library features:
* Cross-platform:  Windows x86/x64, Linux x86/x64, Mac OS, iOS, Android, Windows Mobile, and also other platforms accessible to the compilers Delphi and FreePascal
* Extremely high performance
* Smart constructing of a path
* Limit is 16 million of cells (e.g. 4000x4000)
* Sector test 
* Optional: OOP or procedural interface 
* Caching and search from several starting points support
* Up to 255 types of tiles, weight is from 0.1 to 100.0 
* Contains demonstration projects

[Demo_x86](http://dmozulyov.ucoz.net/cpf_Demo_x86.zip), [Demo_x64](http://dmozulyov.ucoz.net/cpf_Demo_x64.zip)
![](http://dmozulyov.ucoz.net/cpf_screenshot.png)
![](http://dmozulyov.ucoz.net/cpf_excluded.png)
![](http://dmozulyov.ucoz.net/cpf_ways.png)

# Help
The library contains 2 main objects: TTileMap (map of tiles) and TTileMapWeights (tile weights array). The tile can take any value from 1 till 255. The tile’s weight is indicated in the range from 0.1 to 100.0, if the tile weight is from 0.0 to 0.1 then the tile is considered to be impassable. If the TTileMapWeights is not indicated during the searching then all the weights are considered to be 1. The tile with number 0 (TILE_BARRIER) is considered to be a barrier, passing through which is impossible. The modes of the maps mkDiagonal and mkDiagonalEx are different in a way how the paths round the tile-barriers. 

The "SectorTest" parameter helps to improve the search speed dramatically if the map is divided into "areas" and to build the path from one "area" to another is impossible. 

The "Caching" parameter is always recommended to set to `true` if it is not supposed to change the destination point or weights array. 

The array of Excludes has a large value. It is possible to organize complex searches with the help of this feature in which, for example, one game unit do not contact with the others.  In such cases it is recommended to rebuild the paths by changing the destination point, unit strength or the position of at least one of the game units. Accordingly, if not the whole path of the game unit interests you but only the next point then the serch parameter “FullPath” is recommended to set to `false` (watch the example below).  

##### Basic types and contants
```c
  typedef unsigned short ushort;
  typedef unsigned char uchar;
  struct TPoint
  {
      long X;
      long Y;
      TPoint(){}
      TPoint(long x, long y):X(x),Y(y){}
  };  

  // handle type
  typedef size_t TCPFHandle;

  // kind of map
  typedef uchar TTileMapKind; enum {mkSimple, mkDiagonal, mkDiagonalEx, mkHexagonal}; 

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
	float getValue(uchar Tile);
	void setValue(uchar Tile, float Value);
  }; 
```

##### Tile map object
```c
  struct TTileMap
  {
	// constructor and destructor
	TTileMap(ushort AWidth, ushort AHeight, TTileMapKind AKind, bool ASameDiagonalWeight = false);
	~TTileMap();

    // internal descriptor
    TCPFHandle Handle; 

    // basic parameters
    ushort Width;
	ushort Height;
	TTileMapKind Kind;
	bool SameDiagonalWeight;

    // important variable parameters!
	bool SectorTest;
	bool Caching;

	// update methods
	void Clear();
	void Update(uchar* ATiles, ushort X, ushort Y, ushort AWidth, ushort AHeight, ptrdiff_t Pitch = 0);
	uchar getTile(ushort X, ushort Y);
	void setTile(ushort X, ushort Y, uchar Value);

	// path finding
	TTileMapPath FindPath(const TTileMapParams Params, bool FullPath = true);
	TTileMapPath FindPath(const TPoint Start, const TPoint Finish,
	  TCPFHandle Weights = 0, TPoint* Excludes = NULL, size_t ExcludesCount = 0, bool FullPath = true);
	TTileMapPath FindPath(TPoint* Starts, size_t StartsCount, const TPoint Finish,
	  TCPFHandle Weights = 0, TPoint* Excludes = NULL, size_t ExcludesCount = 0, bool FullPath = true);
  }; 
```

##### Simple example
[http://www.policyalmanac.org/games/aStarTutorial.htm](http://www.policyalmanac.org/games/aStarTutorial.htm)
![](http://www.policyalmanac.org/games/aStarT7.jpg)
```c
  TTileMap Map(8, 6, mkDiagonalEx);

  Map.setTile(4, 1, TILE_BARRIER);
  Map.setTile(4, 2, TILE_BARRIER);
  Map.setTile(4, 3, TILE_BARRIER);

  TPoint Start(2, 2);
  TPoint Finish(6, 2);
  printf("A* Pathfinding [%d, %d] --> [%d, %d]...\n", Start.X, Start.Y, Finish.X, Finish.Y);
  TTileMapPath Path = Map.FindPath(Start, Finish);

  printf("Distance = %0.2f (%d points):\n", Path.Distance, Path.Count);
  for (size_t i = 0; i < Path.Count; i++)
    printf("[%d, %d]\n", Path.Points[i].X, Path.Points[i].Y);
```
Output:
```
A* Pathfinding [2, 2] --> [6, 2]... 
Distance = 6.83 (7 points): 
[2, 2] 
[3, 3] 
[3, 4] 
[4, 4] 
[5, 4] 
[6, 3] 
[6, 2]
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