unit Unit1;


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
{$if (CompilerVersion < 23) and (not Defined(FPC))}
  {$define CPUX86}
{$ifend}
{$if (Defined(FPC)) or (CompilerVersion >= 17)}
  {$define INLINESUPPORT}
{$ifend}
{$if Defined(CPUX86) or Defined(CPUX64)}
   {$define CPUINTEL}
{$ifend}
{$if SizeOf(Pointer) = 8}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$if CompilerVersion >= 21}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ifend}
{$if (not Defined(FPC)) and (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
  {$define INTERNALCODEPAGE}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}

interface

{$define USECPFDLL}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Math, ExtCtrls, StdCtrls, Spin, JPEG,
  {$ifdef USECPFDLL}cpf{$else}CrystalPathFinding{$endif};

{$R xp_manifest.res}

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    GroupBox1: TGroupBox;
    pbTile0: TPaintBox;
    pbTile1: TPaintBox;
    pbTile2: TPaintBox;
    pbTile3: TPaintBox;
    sbTile0: TScrollBar;
    lbTile0: TLabel;
    cbUseWeights: TCheckBox;
    sbTile1: TScrollBar;
    lbTile1: TLabel;
    sbTile2: TScrollBar;
    lbTile2: TLabel;
    sbTile3: TScrollBar;
    lbTile3: TLabel;
    GroupBox3: TGroupBox;
    pbClear: TPaintBox;
    pbExclude: TPaintBox;
    GroupBox4: TGroupBox;
    seIterationsCount: TSpinEdit;
    btnTestSpeed: TButton;
    btnRandom: TButton;
    btnClear: TButton;
    lbDistance: TLabel;
    rgMapMode: TRadioGroup;
    GroupBox5: TGroupBox;
    cbSectorTest: TCheckBox;
    cbSmartWeight: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnTileClick(Sender: TObject);
    procedure OnTilePaint(Sender: TObject);
    procedure OnTileWeightChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure OnMapOptionChanged(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnTestSpeedClick(Sender: TObject);
    procedure cbUseWeightsClick(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btnClearClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
    procedure cbSectorTestClick(Sender: TObject);
    procedure rgMapModeClick(Sender: TObject);
  private
    // внутренние данные
    UseWeights: boolean;
    SectorTest: boolean;
    ExcludedPoints: array of TPoint;
    FStartPoint: TPoint;
    FFinishPoint: TPoint;
    CursorPoint: TPoint;
    MousePressed: TMouseButton;
    FTileMode: byte;
    FClearMode: byte;
    FMapMode: TPathMapMode;

    procedure RepaintBoxes(const PaintBoxes: array of TPaintBox);
    function  ExcludePointPos(const Value: TPoint): integer;
    procedure AddExcludedPoint(const Value: TPoint);
    procedure DeleteExcludedPoint(const Value: TPoint);
    function  MapToScreen(X, Y: integer; center: boolean=true): TPoint;
    function  ScreenToMap(X, Y: integer): TPoint;
    procedure SetStartPoint(const Value: TPoint);
    procedure SetFinishPoint(const Value: TPoint);
    procedure SetClearMode(const Value: byte);
    procedure SetTileMode(const Value: byte);
    procedure SetMapMode(const Value: TPathMapMode);
  private
    // основные методы
    procedure SaveMap();
    procedure RecreateMap();
    procedure FillMapBitmap(const PathMapResult: PPathMapResult; const jpg_file_name: string = '');
    procedure ExecutePathFinding();

    property TileMode: byte read FTileMode write SetTileMode;
    property ClearMode: byte read FClearMode write SetClearMode;
    property MapMode: TPathMapMode read FMapMode write SetMapMode; 
    property StartPoint: TPoint read FStartPoint write SetStartPoint;
    property FinishPoint: TPoint read FFinishPoint write SetFinishPoint;
  end;


const
  TILES_COUNT = 4;
  TILE_SIZE = 30;
  FULL_TILE_SIZE = TILE_SIZE + TILE_SIZE div 3;
  MAP_WIDTH = 30;
  MAP_HEIGHT = 20;


var
  Form1: TForm1;
  TILE_MAP: array[0..MAP_HEIGHT-1, 0..MAP_WIDTH-1] of byte;

  ProjectPath: string;
  MapBitmap: TBitmap;
  TileBitmaps: array[0..TILES_COUNT*3-1] of TBitmap;
  WhiteCell, GreyCell: TBitmap;
  MaskHex45, MaskHex60: TBitmap;


  // рабочие объекты библиотеки cpf
  HWeights: THandle;
  HMap: THandle;
implementation

{$R *.dfm}



procedure TForm1.FormCreate(Sender: TObject);
var
  i, Len: integer;
  F: TFileStream;
  SmallCell: TBitmap;

  function LoadBitmap(const FileName: string): TBitmap;
  begin
    Result := TBitmap.Create;
    Result.LoadFromFile(FileName);
  end;

  function ReadInt: integer;
  begin F.Read(Result, sizeof(Result)); end;

  function ReadBool: boolean;
  begin F.Read(Result, sizeof(Result)); end;

begin
  randomize;
  Application.Title := 'Crystal Path Finding';
  ProjectPath := IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)));

  for i := 0 to TILES_COUNT-1 do
  begin
    SmallCell := TBitmap.Create;
    TileBitmaps[i*3] := LoadBitmap(ProjectPath + IntToStr(i) + '_.bmp');
    TileBitmaps[i*3+1] := SmallCell;
    TileBitmaps[i*3+2] := LoadBitmap(ProjectPath + IntToStr(i) + '.bmp');

    SmallCell.PixelFormat := pf24bit;
    SmallCell.Width := TILE_SIZE;
    SmallCell.Height := TILE_SIZE;
    SetStretchBltMode(SmallCell.Canvas.Handle, HALFTONE);
    SmallCell.Canvas.CopyRect(Rect(0, 0, TILE_SIZE, TILE_SIZE), TileBitmaps[i*3+2].Canvas, Rect(0, 0, FULL_TILE_SIZE, FULL_TILE_SIZE));
  end;
  WhiteCell := LoadBitmap('white.bmp');
  GreyCell := LoadBitmap('grey.bmp');
  MaskHex45 := LoadBitmap('hex45.bmp');
  MaskHex60 := LoadBitmap('hex60.bmp');


  // инициализация рабочих данных
  HWeights := cpfCreateWeights(TILES_COUNT-1);
  UseWeights := true;
  SectorTest := true;
  sbTile0.Position := round(1.0 *20);
  sbTile1.Position := round(1.5 *20);
  sbTile2.Position := round(2.5 *20);
  sbTile3.Position := round(6.0 *20);
  StartPoint := Point(5, 9);
  FinishPoint := Point(24, 9);
  MousePressed := mbMiddle; // типа и не левая и не правая

  // загрузка из файла
  if (FileExists(ProjectPath+'SAVE.dat')) then
  begin
    F := TFileStream.Create(ProjectPath+'SAVE.dat', fmShareDenyNone);
    F.Read(TILE_MAP, sizeof(TILE_MAP));
    F.Read(FStartPoint, sizeof(FStartPoint));  
    F.Read(FFinishPoint, sizeof(FFinishPoint));
    F.Read(FTileMode, sizeof(FTileMode));
    F.Read(FClearMode, sizeof(FClearMode));
    F.Read(FMapMode, sizeof(FMapMode));

    cbUseWeights.Checked := ReadBool;
    cbSmartWeight.Checked := ReadBool;
    cbSectorTest.Checked := ReadBool;
    rgMapMode.ItemIndex := byte(FMapMode);

    sbTile0.Position := ReadInt;
    sbTile1.Position := ReadInt;
    sbTile2.Position := ReadInt;
    sbTile3.Position := ReadInt;
    seIterationsCount.Value := ReadInt;

    Len := ReadInt;
    SetLength(ExcludedPoints, Len);
    if (Len <> 0) then F.Read(pointer(ExcludedPoints)^, Len*sizeof(TPoint));
    F.Free;
  end; 

  // создать буфер отрисовки (начиная с этого шага каждое изменение будет перерисовывать буфер)
  MapBitmap := TBitmap.Create;
  MapBitmap.PixelFormat := pf24bit;
  MapBitmap.Width := MAP_WIDTH*TILE_SIZE;
  MapBitmap.Height := MAP_HEIGHT*TILE_SIZE;

  // создать карту с текущими параметрами (и перерисовать)
  RecreateMap();
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  // сохранение
  SaveMap();

  // удаление созданных объектов
  cpfDestroyWeights(HWeights);
  cpfDestroyMap(HMap);

  for i := 0 to TILES_COUNT*3-1 do
  TileBitmaps[i].Free;

  MapBitmap.Free;

  WhiteCell.Free;
  GreyCell.Free;
  MaskHex45.Free;
  MaskHex60.Free;
end;

procedure TForm1.SaveMap();
var
  F: TFileStream;
  Len: integer;

  procedure WriteInt(Value: integer);
  begin F.Write(Value, sizeof(Value)); end;

  procedure WriteBool(Value: boolean);
  begin F.Write(Value, sizeof(Value)); end;  
begin
  F := TFileStream.Create(ProjectPath+'SAVE.dat', fmCreate);
  F.Write(TILE_MAP, sizeof(TILE_MAP));
  F.Write(FStartPoint, sizeof(FStartPoint));
  F.Write(FFinishPoint, sizeof(FFinishPoint));
  F.Write(FTileMode, sizeof(FTileMode));
  F.Write(FClearMode, sizeof(FClearMode));
  F.Write(FMapMode, sizeof(MapMode));

  WriteBool(cbUseWeights.Checked);
  WriteBool(cbSmartWeight.Checked);
  WriteBool(cbSectorTest.Checked);   

  WriteInt(sbTile0.Position);
  WriteInt(sbTile1.Position);
  WriteInt(sbTile2.Position);
  WriteInt(sbTile3.Position);
  WriteInt(seIterationsCount.Value);

  Len := Length(ExcludedPoints);
  WriteInt(Len);
  if (Len <> 0) then F.Write(pointer(ExcludedPoints)^, Len*sizeof(TPoint));
  F.Free;
end;  



procedure TForm1.btnClearClick(Sender: TObject);
var
  BufMapBitmap: TBitmap;
begin
  BufMapBitmap := MapBitmap;
  MapBitmap := nil;

  begin
    ZeroMemory(@TILE_MAP, sizeof(TILE_MAP));
    sbTile0.Position := round(1.0 *20);
    sbTile1.Position := round(1.5 *20);
    sbTile2.Position := round(2.5 *20);
    sbTile3.Position := round(6.0 *20);
    StartPoint := Point(5, 9);
    FinishPoint := Point(24, 9);
    ExcludedPoints := nil;
    cbUseWeights.Checked := true;
    cbSmartWeight.Checked := true;
    MapMode := mmSimple;
    cbSectorTest.Checked := true;
    seIterationsCount.Value := 1000;
  end;

  MapBitmap := BufMapBitmap;
  TileMode := 0;
  ClearMode := 0;
  RecreateMap();
end;

procedure TForm1.btnRandomClick(Sender: TObject);
var
  i, j, value: integer;
  BufMapBitmap: TBitmap;

  function random_bool: boolean;
  begin Result := (random(2) <> 0); end;

  function random_point: TPoint;
  begin
    Result.X := random(MAP_WIDTH);
    Result.Y := random(MAP_HEIGHT);
  end;
begin
  BufMapBitmap := MapBitmap;
  MapBitmap := nil;

  begin
    //ZeroMemory(@TILE_MAP, sizeof(TILE_MAP));
    for i := 0 to MAP_WIDTH-1 do
    for j := 0 to MAP_HEIGHT-1 do
    begin
      value := random(TILES_COUNT+1);
      if (value = TILES_COUNT) then value := $FF;
      TILE_MAP[j, i] := value;
    end;
    sbTile0.Position := random(200);
    sbTile1.Position := random(200);
    sbTile2.Position := random(200);
    sbTile3.Position := random(200);
    StartPoint := random_point;
    FinishPoint := random_point;
    ExcludedPoints := nil;
    for i := 0 to random(20) do AddExcludedPoint(random_point);
    cbUseWeights.Checked := random_bool;
    cbSmartWeight.Checked := random_bool;
    MapMode := TPathMapMode(random(byte(high(TPathMapMode))+1));
    cbSectorTest.Checked := random_bool;
    seIterationsCount.Value := random(100000)+1;
  end;

  MapBitmap := BufMapBitmap;
  TileMode := 0;
  ClearMode := 0;
  RecreateMap();
end;

procedure TForm1.RepaintBoxes(const PaintBoxes: array of TPaintBox);
var
  i: integer;
  PaintBox: TPaintBox;
begin
  for i := 0 to High(PaintBoxes) do
  begin
    PaintBox := PaintBoxes[i];
    if (Assigned(PaintBox.OnPaint)) then PaintBox.OnPaint(PaintBox);
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close();
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  TPaintBox(Sender).Canvas.Draw(0, 0, MapBitmap);
end;

procedure TForm1.RecreateMap();
begin
  if (HMap <> 0) then cpfDestroyMap(HMap);

  // создать карту
  HMap := cpfCreateMap(MAP_WIDTH, MAP_HEIGHT, MapMode, TILES_COUNT-1,
                       cbSmartWeight.Checked);

  // заполнить
  cpfMapUpdate(HMap, @TILE_MAP[0, 0], 0, 0, MAP_WIDTH, MAP_HEIGHT);

  // пересчитать пути (перерисовать)
  ExecutePathFinding();
end;

procedure TForm1.cbUseWeightsClick(Sender: TObject);
begin
  UseWeights := cbUseWeights.Checked;
  ExecutePathFinding();
end;

procedure TForm1.cbSectorTestClick(Sender: TObject);
begin
  SectorTest := cbSectorTest.Checked;
  ExecutePathFinding();
end;

procedure TForm1.OnMapOptionChanged(Sender: TObject);
begin
  if (MapBitmap <> nil) then RecreateMap();
end;

procedure TForm1.OnTileClick(Sender: TObject);
var
  TileNum: integer;
begin
  TileNum := TPaintBox(Sender).Tag;

  if (TileNum < 0) then
  begin
    // правая кнопка мыши
    ClearMode := -TileNum -1;
  end else
  begin
    // конкретный тайл
    TileMode := TileNum;
  end;
end;

procedure TForm1.rgMapModeClick(Sender: TObject);
begin
  MapMode := TPathMapMode(rgMapMode.ItemIndex);
end;

procedure TForm1.OnTilePaint(Sender: TObject);
var
  PaintBox: TPaintBox;
  TileNum: integer;
  Color: TColor;
  Active: boolean;
  PaintBoxRect: TRect;
begin
  PaintBox := TPaintBox(Sender);
  TileNum := PaintBox.Tag;
  Color := TColor($FFFFFFFF);
  PaintBoxRect := Rect(0, 0, PaintBox.Width, PaintBox.Height);

  if (TileNum >= 0) then
  begin
    if (cpfWeightGet(HWeights, TileNum) < 0.1) then Color := clWhite;
    Active := (TileMode=TileNum);
  end else
  begin
    if (TileNum = -1) then Color := clBlack
    else Color := clGray;

    Active := (ClearMode = (-TileNum-1));
  end;

  if (Color = TColor($FFFFFFFF)) then
  begin
    PaintBox.Canvas.Draw(0, 0, TileBitmaps[TileNum*3]);
  end else
  begin
    PaintBox.Canvas.Brush.Style := bsSolid;
    PaintBox.Canvas.Brush.Color := Color;
    PaintBox.Canvas.FillRect(PaintBoxRect);
  end;

  if (Active) then
  begin
    with PaintBoxRect do
    begin
      inc(Left, 2);
      inc(Top, 2);
      dec(Right, 1);
      dec(Bottom, 1);
    end;         
    PaintBox.Canvas.Pen.Width := 4;
    PaintBox.Canvas.Pen.Color := clBlue;
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.Rectangle(PaintBoxRect);
  end;
end;

procedure TForm1.OnTileWeightChange(Sender: TObject);
var
  TileNum: byte;
  Weight: single;
begin
  TileNum := TScrollBar(Sender).Tag;
  Weight := TScrollBar(Sender).Position / 20;
  TLabel(FindComponent('lbTile' + IntToStr(TileNum))).Caption := Format('%0.2f', [Weight]);

  cpfWeightSet(HWeights, TileNum, Weight);
  RepaintBoxes([TPaintBox(FindComponent('pbTile' + IntToStr(TileNum)))]);
  if (UseWeights) then ExecutePathFinding();
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbMiddle)or (MousePressed = Button) then exit;

  // снять фокус с контролов
  Windows.SetFocus(0);

  // инициализировать нажатую кнопку
  MousePressed := Button;

  // сделать действие по кнопке
  CursorPoint := ScreenToMap(X, Y);
  PaintBox1MouseMove(nil{показатель, что первый раз}, Shift, X, Y);
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
begin
  if (MousePressed = Button) then
  begin
    MousePressed := mbMiddle;
    ExecutePathFinding();
  end;  
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P, LastCursorPoint: TPoint;
begin
  if (MousePressed = mbMiddle) then exit;
  P := ScreenToMap(X, Y);
  if (P.X < 0) or (P.X >= MAP_WIDTH) or (P.Y < 0) or (P.Y >= MAP_HEIGHT) then exit;
  if (Sender <> nil) and (CursorPoint.X = P.X) and (CursorPoint.Y = P.Y) then exit;
  LastCursorPoint := CursorPoint;
  CursorPoint := P;

  // точки
  if (MousePressed = mbLeft) and (LastCursorPoint.X = StartPoint.X) and (LastCursorPoint.Y = StartPoint.Y) then
  begin
    FStartPoint := P;
    ExecutePathFinding();
    exit;
  end;
  if (MousePressed = mbLeft) and (LastCursorPoint.X = FinishPoint.X) and (LastCursorPoint.Y = FinishPoint.Y) then
  begin
    FFinishPoint := P;
    ExecutePathFinding();
    exit;
  end;

  if (MousePressed = mbLeft) then
  begin
    // тайл
    if (TILE_MAP[P.Y, P.X] <> TileMode) then
    begin
      TILE_MAP[P.Y, P.X] := TileMode;
      cpfMapSetTile(HMap, P.X, P.Y, TileMode);
      ExecutePathFinding();
    end;
  end else
  begin
    // препятствие
    if (ClearMode = 0) then
    begin
      if (TILE_MAP[P.Y, P.X] <> $FF) then
      begin
        TILE_MAP[P.Y, P.X] := $FF;
        cpfMapSetTile(HMap, P.X, P.Y, $FF);
        ExecutePathFinding();
      end;
    end else
    begin
      if (ExcludePointPos(P) >= 0) then DeleteExcludedPoint(P)
      else AddExcludedPoint(P);

      ExecutePathFinding();
    end;
  end;
end;

function TForm1.ExcludePointPos(const Value: TPoint): integer;
begin
  for Result := 0 to Length(ExcludedPoints)-1 do
  with ExcludedPoints[Result] do
  if (X = Value.X) and (Y = Value.Y) then exit;

  Result := -1;
end;

procedure TForm1.AddExcludedPoint(const Value: TPoint);
var
  Len: integer;
begin
  if (ExcludePointPos(Value) >= 0) then exit;

  Len := Length(ExcludedPoints);
  SetLength(ExcludedPoints, Len+1);
  ExcludedPoints[Len] := Value;
end;

procedure TForm1.DeleteExcludedPoint(const Value: TPoint);
var
  P, Len: integer;
begin
  P := ExcludePointPos(Value);
  if (P < 0) then exit;

  Len := Length(ExcludedPoints)-1;
  if (P <> Len) then ExcludedPoints[P] := ExcludedPoints[Len];
  SetLength(ExcludedPoints, Len);
end;

function  TForm1.MapToScreen(X, Y: integer; center: boolean=true): TPoint;
begin
  Result.X := X * TILE_SIZE;
  Result.Y := Y * TILE_SIZE;

  case (MapMode) of
mmHexagonal45: begin
                 if (X and 1 = 1) then inc(Result.Y, TILE_SIZE div 2);

                 if (center) then
                 begin
                   inc(Result.X, (TILE_SIZE*4 div 3) div 2 - 1);
                   inc(Result.Y, TILE_SIZE div 2 + 1);
                 end;
               end;
mmHexagonal60: begin
                 if (Y and 1 = 1) then inc(Result.X, TILE_SIZE div 2);

                 if (center) then
                 begin
                   inc(Result.X, TILE_SIZE div 2 + 1);
                   inc(Result.Y, (TILE_SIZE*4 div 3) div 2 - 1);
                 end;
               end;
else
  if (center) then
  begin
   inc(Result.X, TILE_SIZE div 2);
   inc(Result.Y, TILE_SIZE div 2);
  end;    
end;

end;

function  TForm1.ScreenToMap(X, Y: integer): TPoint;
begin
  Result.X := X div TILE_SIZE;
  Result.Y := Y div TILE_SIZE;

  if (MapMode = mmHexagonal45) then
  begin
    if (Result.X and 1 = 1) then
    begin
      Result.Y := (Y-TILE_SIZE div 2) div TILE_SIZE;

      if (Result.Y = MAP_HEIGHT-1) then Result.Y := -1;
      if (Y < TILE_SIZE div 2) then Result.Y := -1;
    end;  

    if (X mod TILE_SIZE < (TILE_SIZE div 3)) then
    begin
      Result.X := low(integer);
      Result.Y := low(integer);
    end;
  end;
  if (MapMode = mmHexagonal60) then
  begin
    if (Result.Y and 1 = 1) then
    begin
      Result.X := (X-TILE_SIZE div 2) div TILE_SIZE;

      if (Result.X = MAP_WIDTH-1) then Result.X := -1;
      if (X < TILE_SIZE div 2) then Result.X := -1;
    end;

    if (Y mod TILE_SIZE < (TILE_SIZE div 3)) then
    begin
      Result.X := low(integer);
      Result.Y := low(integer);
    end;
  end;

end;

procedure TForm1.SetStartPoint(const Value: TPoint);
begin
  if (FStartPoint.X <> Value.X) or (FStartPoint.Y <> Value.Y) then
  begin
    FStartPoint := Value;
    ExecutePathFinding();
  end;
end;

procedure TForm1.SetFinishPoint(const Value: TPoint);
begin
  if (FFinishPoint.X <> Value.X) or (FFinishPoint.Y <> Value.Y) then
  begin
    FFinishPoint := Value;
    ExecutePathFinding();
  end;
end;

procedure TForm1.SetTileMode(const Value: byte);
begin
  if (FTileMode = Value) then exit;
  FTileMode := Value;
  if (MapBitmap <> nil) then RepaintBoxes([pbTile0, pbTile1, pbTile2, pbTile3]);
end;

procedure TForm1.SetClearMode(const Value: byte);
begin
  if (FClearMode = Value) then exit;
  FClearMode := Value;
  if (MapBitmap <> nil) then RepaintBoxes([pbClear, pbExclude]);
end;

procedure TForm1.SetMapMode(const Value: TPathMapMode);
var
  P: TPoint;
begin
  if (FMapMode = Value) then exit;
  FMapMode := Value;
  rgMapMode.ItemIndex := byte(Value);

  // корректировка точек
  P := ScreenToMap(FStartPoint.X*TILE_SIZE + TILE_SIZE div 2, FStartPoint.Y*TILE_SIZE + TILE_SIZE div 2);
  if (P.X = -1) then dec(FStartPoint.X);
  if (P.Y = -1) then dec(FStartPoint.Y);
  P := ScreenToMap(FFinishPoint.X*TILE_SIZE + TILE_SIZE div 2, FFinishPoint.Y*TILE_SIZE + TILE_SIZE div 2);
  if (P.X = -1) then dec(FFinishPoint.X);
  if (P.Y = -1) then dec(FFinishPoint.Y);   

  // пересоздание карты, запуск расчёта
  if (MapBitmap <> nil) then RecreateMap();
end;


// самая главная функция - расчитать путь
// по умолчанию отражает результат
procedure TForm1.ExecutePathFinding();
var
  Weights: THandle;
  PathMapResult: PPathMapResult;
begin
  if (MapBitmap = nil) then exit;

  // расчитать
  Weights := HWeights;
  if (not UseWeights) then Weights := 0;
  try
    PathMapResult := cpfFindPath(HMap, StartPoint, FinishPoint, Weights, PPoint(ExcludedPoints), Length(ExcludedPoints), SectorTest);
  except
    SaveMap();
    FillMapBitmap(nil, ProjectPath+'map_exception.jpg');
    raise;
  end;  

  // label расстояние
  lbDistance.Visible := (PathMapResult <> nil);
  if (PathMapResult <> nil) then lbDistance.Caption := Format('Расстояние: %0.2f', [PathMapResult.Distance]);

  // прорисовать битмап
  FillMapBitmap(PathMapResult);

  //PaintBox1.Repaint();
  RepaintBoxes([PaintBox1]);
end;


procedure TForm1.FillMapBitmap(const PathMapResult: PPathMapResult; const jpg_file_name: string = '');
var
  TileNum: byte;
  Hexagonal: boolean;
  BitmapMask: TBitmap;
  i, j: integer;
  P: TPoint;
  Canvas: TCanvas;
  PathPoint: PPoint;
  JI: TJpegImage;
  used_tiles: array[0..TILES_COUNT-1] of boolean;


  procedure DrawBitmap(Bitmap: TBitmap);
  begin
    if (BitmapMask = nil) then
    begin
      Canvas.Draw(P.X, P.Y, Bitmap);
    end else
    begin
      MaskBlt(Canvas.Handle, P.X, P.Y, FULL_TILE_SIZE, FULL_TILE_SIZE,
              Bitmap.Canvas.Handle, 0, 0,
              BitmapMask.Handle, 0, 0, MakeRop4($00AA0029, SrcCopy));
    end;
  end;

  procedure DrawTile(X, Y: integer; TileNum: byte {255 - excluded});
  begin
    P := MapToScreen(X, Y, false);
    if (P.X >= MAP_WIDTH*TILE_SIZE - TILE_SIZE div 2) or
       (P.Y >= MAP_HEIGHT*TILE_SIZE - TILE_SIZE div 2) then exit;


    // без тайла
    if (TileNum = 255) or (not used_tiles[TileNum]) then
    begin
      if (Hexagonal) then
      begin
        if (TileNum = 255) then DrawBitmap(GreyCell)
        else DrawBitmap(WhiteCell);
      end else
      begin
        if (TileNum = 255) then Canvas.Brush.Color := clGray
        else Canvas.Brush.Color := clWhite;

        Canvas.FillRect(Bounds(P.X, P.Y, TILE_SIZE, TILE_SIZE));
      end;

      exit;
    end;

    // с тайлом
    DrawBitmap(TileBitmaps[TileNum*3 + 1 + ord(Hexagonal)]);
  end;

  procedure LineTo(const offsX, offsY: integer);
  begin
    inc(P.X, offsX);
    inc(P.Y, offsY);
    Canvas.LineTo(P.X, P.Y);    
  end;

begin
  // анализ доступности тайлов
  // какие-то тайлы могут восприниматься как препятствия
  for TileNum := 0 to TILES_COUNT-1 do
  used_tiles[TileNum] := (not UseWeights) or (cpfWeightGet(HWeights, TileNum) >= 0.1);


  // подготовка данных для гексагонального режима
  BitmapMask := nil;
  Hexagonal := (MapMode in [mmHexagonal45, mmHexagonal60]);
  if (Hexagonal) then
  begin
    if (MapMode = mmHexagonal45) then BitmapMask := MaskHex45
    else BitmapMask := MaskHex60;

  end;

  // очистить
  Canvas := MapBitmap.Canvas;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Rect(0, 0, MapBitmap.Width, MapBitmap.Height));

  // прорисовать тайлы
  for i := 0 to MAP_WIDTH-1 do
  for j := 0 to MAP_HEIGHT-1 do
  begin
    TileNum := TILE_MAP[j, i];
    if (TileNum < TILES_COUNT) then DrawTile(i, j, TileNum);
  end;

  // ExcludedPoints
  for i := 0 to Length(ExcludedPoints)-1 do
  with ExcludedPoints[i] do
  DrawTile(X, Y, 255);


  // линии
  Canvas.Pen.Width := 1;
  if (not Hexagonal) then
  begin
    Canvas.Pen.Color := clGray;

    for i := 1 to MAP_WIDTH-1 do
    begin
      Canvas.MoveTo(i*TILE_SIZE, 0);
      Canvas.LineTo(i*TILE_SIZE, MAP_HEIGHT*TILE_SIZE);
    end;
    for j := 1 to MAP_HEIGHT-1 do
    begin
      Canvas.MoveTo(0, j*TILE_SIZE);
      Canvas.LineTo(MAP_WIDTH*TILE_SIZE, j*TILE_SIZE);
    end;
  end else
  for i := 0 to MAP_WIDTH-1 do
  for j := 0 to MAP_HEIGHT-1 do
  begin
    // прорисовка каждого тайла в хексогональных полях
    Canvas.Pen.Color := TColor($B0B0B0);    
    P := MapToScreen(i, j, false);

    // стартовая точка
    if (MapMode = mmHexagonal45) then inc(P.X, TILE_SIZE div 3)
    else inc(P.X, TILE_SIZE div 2);
    Canvas.MoveTo(P.X, P.Y);

    // прорисовка 6 линий
    if (MapMode = mmHexagonal45) then
    begin
      LineTo((TILE_SIZE div 3)*2, 0);
      LineTo(TILE_SIZE div 3, TILE_SIZE div 2);
      LineTo(-TILE_SIZE div 3, TILE_SIZE div 2);
      LineTo(-(TILE_SIZE div 3)*2, 0);
      LineTo(-TILE_SIZE div 3, -TILE_SIZE div 2);
      LineTo(TILE_SIZE div 3, -TILE_SIZE div 2);
    end else
    begin
      LineTo(TILE_SIZE div 2, TILE_SIZE div 3);
      LineTo(0, (TILE_SIZE div 3)*2);
      LineTo(-TILE_SIZE div 2, TILE_SIZE div 3);
      LineTo(-TILE_SIZE div 2, -TILE_SIZE div 3);
      LineTo(0, -(TILE_SIZE div 3)*2);
      LineTo(TILE_SIZE div 2, -TILE_SIZE div 3);            
    end;
  end; 

  // путь
  if (PathMapResult <> nil) and (PathMapResult.points_count <> 1) then
  begin
    PathPoint := PathMapResult.points;
    Canvas.Pen.Width := 2;
    Canvas.Pen.Color := clRed;
    with MapToScreen(PathPoint.X, PathPoint.Y) do Canvas.MoveTo(X, Y);

    for i := 1 to PathMapResult.points_count-1 do
    begin
      inc(PathPoint);
      with MapToScreen(PathPoint.X, PathPoint.Y) do Canvas.LineTo(X, Y);
    end;
  end;

  // StartPoint, FinishPoint
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Color := clBlue;
  if (MousePressed = mbLeft) and (CursorPoint.X = StartPoint.X) and (CursorPoint.Y = StartPoint.Y) then Canvas.Brush.Color := clAqua;
  Canvas.Pen.Color := clRed;
  P := MapToScreen(StartPoint.X, StartPoint.Y);
  Canvas.Ellipse(Bounds(P.X -9, P.Y -9, 20, 20));
  Canvas.Pen.Width := 2;
  Canvas.Brush.Color := clRed;
  if (MousePressed = mbLeft) and (CursorPoint.X = FinishPoint.X) and (CursorPoint.Y = FinishPoint.Y) then Canvas.Brush.Color := clFuchsia;
  Canvas.Pen.Color := clBlue;
  P := MapToScreen(FinishPoint.X, FinishPoint.Y);
  Canvas.Ellipse(Bounds(P.X -9, P.Y -9, 20, 20));

  // save
  if (jpg_file_name <> '') then
  begin
    JI := TJpegImage.Create;
    JI.Assign(MapBitmap);
      JI.SaveToFile(jpg_file_name);
    JI.Free;
  end; 
end;


procedure TForm1.btnTestSpeedClick(Sender: TObject);
var
  i, Count: integer;
  Time: dword;
  Weights: THandle;
begin
  if (MapBitmap = nil) then exit;

  Weights := HWeights;
  if (not UseWeights) then Weights := 0;

  Count := seIterationsCount.Value;
  Time := GetTickCount;
    for i := 0 to Count-1 do // ExecutePathFinding(Show = false);
    cpfFindPath(HMap, StartPoint, FinishPoint, Weights, PPoint(ExcludedPoints), Length(ExcludedPoints), SectorTest);
  Time := GetTickCount-Time;

  ShowMessageFmt('Кратчайший путь был расчитан %d раз за %d миллисекунд', [Count, Time]);
end;






end.
