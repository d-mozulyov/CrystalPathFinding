unit DemoUnit1;


// compiler directives
{$ifdef FPC}
  {$mode Delphi}
  {$asmmode Intel}
  {$define INLINESUPPORT}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}  
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
{$if SizeOf(Pointer) = 8}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Types, Dialogs, Math, ExtCtrls, StdCtrls, Spin, JPEG,
  {$ifdef USECPFDLL}cpf{$else}CrystalPathFinding{$endif};

{$R xp_manifest.res}

type
  TTestingMode = (tmOne, tmOneCaching, tmMany, tmManyStandard, tmManyStandardCaching);
  {$ifdef USECPFDLL}
  TPointDynArray = array of TPoint;
  {$endif}

type
  TMainForm = class(TForm)
    pbMap: TPaintBox;
    cbMapKind: TComboBox;
    cbTestingMode: TComboBox;
    btnClear: TButton;
    btnRandom: TButton;
    btnSave: TButton;
    gbBarrierMode: TGroupBox;
    pbBarrier: TPaintBox;
    pbExclude: TPaintBox;
    gbTileWeigths: TGroupBox;
    cbUseWeights: TCheckBox;
    pbTile1: TPaintBox;
    lbTile1: TLabel;
    sbTile1: TScrollBar;
    pbTile2: TPaintBox;
    lbTile2: TLabel;
    sbTile2: TScrollBar;
    pbTile3: TPaintBox;
    lbTile3: TLabel;
    sbTile3: TScrollBar;
    pbTile4: TPaintBox;
    lbTile4: TLabel;
    sbTile4: TScrollBar;
    gpOptions: TGroupBox;
    cbSameDiagonalWeight: TCheckBox;
    cbSectorTest: TCheckBox;
    cbFullPath: TCheckBox;
    gbPerformanceTest: TGroupBox;
    seIterationsCount: TSpinEdit;
    btnPerformanceTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pbMapPaint(Sender: TObject);
    procedure pbMapDblClick(Sender: TObject);
    procedure pbMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnTileClick(Sender: TObject);
    procedure OnTilePaint(Sender: TObject);
    procedure OnTileWeightChange(Sender: TObject);
    procedure cbMapKindChange(Sender: TObject);
    procedure cbSectorTestClick(Sender: TObject);
    procedure cbUseWeightsClick(Sender: TObject);
    procedure cbTestingModeChange(Sender: TObject);
    procedure cbSameDiagonalWeightClick(Sender: TObject);
    procedure cbFullPathClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnPerformanceTestClick(Sender: TObject);
  private
    // update/repaint/execute
    FUpdateCounter: Integer;
    FActualMap: Boolean;
    FCursorPoint: TPoint;
    FMousePressed: TMouseButton;
    FPathPointBuffer: TPointDynArray;
    {$ifNdef USECPFDLL}
    FCachedAttainablePoints: TPointDynArray;
    FCachedUnattainablePoints: TPointDynArray;
    {$endif}

    function ScreenToMap(X, Y: Integer): TPoint;
    function MapToScreen(X, Y: Integer; Center: Boolean = True): TPoint;
    procedure MapPointCorrect(var Point: TPoint);
    procedure RepaintBoxes(const PaintBoxes: array of TPaintBox);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure TryUpdate;
    function ExecutePathFinding: TTileMapPath;
    procedure FillMapBitmap(const Path: TTileMapPath; const JpgFileName: string = '');
    procedure SaveMap;
  private
    // internal data
    FStartPoint: TPoint;
    FFinishPoint: TPoint;
    FTileMode: Byte;
    FBarrierMode: Byte;
    FMapKind: TTileMapKind;
    FUseWeights: Boolean;
    FSectorTest: Boolean;
    FExcludedPoints: TPointDynArray;
    FTestingMode: TTestingMode;
    FSameDiagonalWeight: Boolean;
    FFullPath: Boolean;
    FManyStartPoints: TPointDynArray;

    procedure InitializeManyStartPoints;
    procedure SetStartPoint(const Value: TPoint);
    procedure SetFinishPoint(const Value: TPoint);
    procedure SetTileMode(const Value: Byte);
    procedure SetBarrierMode(const Value: Byte);
    procedure SetMapKind(const Value: TTileMapKind);
    procedure SetUseWeights(const Value: Boolean);
    procedure SetSectorTest(const Value: Boolean);
    procedure SetTestingMode(const Value: TTestingMode);
    procedure SetSameDiagonalWeight(const Value: Boolean);
    procedure SetFullPath(const Value: Boolean);
    procedure AddExcludedPoint(const Value: TPoint);
    procedure DeleteExcludedPoint(const Value: TPoint);
  public
    property StartPoint: TPoint read FStartPoint write SetStartPoint;
    property FinishPoint: TPoint read FFinishPoint write SetFinishPoint;
    property TileMode: Byte read FTileMode write SetTileMode;
    property BarrierMode: Byte read FBarrierMode write SetBarrierMode;
    property MapKind: TTileMapKind read FMapKind write SetMapKind;
    property UseWeights: Boolean read FUseWeights write SetUseWeights;
    property SectorTest: Boolean read FSectorTest write SetSectorTest;
    property TestingMode: TTestingMode read FTestingMode write SetTestingMode;
    property SameDiagonalWeight: Boolean read FSameDiagonalWeight write SetSameDiagonalWeight;
    property FullPath: Boolean read FFullPath write SetFullPath;
  end;


const
  TILES_COUNT = 4;
  TILE_SIZE = 30;
  FULL_TILE_SIZE = TILE_SIZE + TILE_SIZE div 3;
  MAP_WIDTH = 30;
  MAP_HEIGHT = 20;
  DISTANCE_TEXT_COLOR = {Deeppink}TColor($009314FF);

var
  MainForm: TMainForm;
  TILE_MAP: array[0..MAP_HEIGHT - 1, 0..MAP_WIDTH - 1] of Byte;

  ProjectPath: string;
  MapBitmap: TBitmap;
  TileBitmaps: array[0..TILES_COUNT * 4 - 1] of TBitmap;
  WhiteCell, GreyCell: TBitmap;
  MaskHexagonal: TBitmap;

  // main CPF library objects: map and tile weights
  Weights: TTileMapWeights;
  Map: TTileMap;

implementation

{$R *.dfm}

// some file compatibility routine
procedure IncrementTiles;
var
  i: Integer;
  P: PByte;
begin
  P := @TILE_MAP[0, 0];
  for i := 1 to MAP_HEIGHT * MAP_WIDTH do
  begin
    Inc(P^);
    Inc(P);
  end;
end;

// some file compatibility routine
procedure DecrementTiles;
var
  i: Integer;
  P: PByte;
begin
  P := @TILE_MAP[0, 0];
  for i := 1 to MAP_HEIGHT * MAP_WIDTH do
  begin
    Dec(P^);
    Inc(P);
  end;
end;

// Format('%0.2f') with DecimalSeparator = '.'
function LocalFloatToStr(const F: Extended): string;
var
  Buffer: ShortString;
begin
  Str(F:0:2, Buffer);
  Result := string(Buffer);
end;

// dynamic array index of point
function PointIndex(const P: TPoint; const Points: TPointDynArray): Integer;
begin
  for Result := 0 to Length(Points) - 1 do
  with Points[Result] do
  if (X = P.X) and (Y = P.Y) then Exit;

  Result := -1;
end;

// use dialog form to edit point coordinates
function EditPoint(const Point: TPoint; const PointName: string): TPoint;
var
  Dialog: TForm;
  SpinX, SpinY: TSpinEdit;
  OnKeyDown: TKeyEvent;

  procedure DialogOnKeyDown(Dialog: TForm; Sender: TObject; var Key: Word;
    Shift: TShiftState) far;
  begin
    if (Key = VK_ESCAPE) then Dialog.Close;
  end;

  procedure SpinOnKeyDown(Dialog: TForm; Sender: TObject; var Key: Word;
    Shift: TShiftState) far;
  begin
    if (Key = VK_RETURN) then Dialog.ModalResult := mrOk;
  end;

begin
  Result := Point;

  Dialog := TForm.Create(nil);
  try
    TMethod(OnKeyDown).Data := Dialog;  
    Dialog.Caption := Format('%s point editor [%d, %d]', [PointName, Point.X, Point.Y]);
    Dialog.BorderStyle := bsDialog;
    Dialog.ClientHeight := 95;
    Dialog.ClientWidth := 242;
    Dialog.KeyPreview := True;
    TMethod(OnKeyDown).Code := @DialogOnKeyDown;
    Dialog.OnKeyDown := OnKeyDown;
    Dialog.Position := poScreenCenter;

    // buttons
    with TButton.Create(Dialog) do
    begin
      Parent := Dialog;
      Caption := 'Ok';
      SetBounds(66, 64, 75, 25);
      ModalResult := mrOk;
    end;
    with TButton.Create(Dialog) do
    begin
      Parent := Dialog;
      Caption := 'Cancel';
      SetBounds(153, 64, 75, 25);
      ModalResult := mrCancel;
    end;

    // spin edits
    TMethod(OnKeyDown).Code := @SpinOnKeyDown;
    SpinX := TSpinEdit.Create(Dialog);
    SpinX.Parent := Dialog;
    SpinX.SetBounds(44, 19, 79, 22);
    SpinX.MaxValue := 29;
    SpinX.Value := Point.X;
    SpinX.OnKeyDown := OnKeyDown;
    SpinY := TSpinEdit.Create(Dialog);
    SpinY.Parent := Dialog;
    SpinY.SetBounds(151, 19, 79, 22);
    SpinY.MaxValue := 19;
    SpinY.Value := Point.Y;
    SpinY.OnKeyDown := OnKeyDown;

    // labels
    with TLabel.Create(Dialog) do
    begin
      Parent := Dialog;
      Caption := 'X:';
      SetBounds(29, 23, 10, 13);
    end;
    with TLabel.Create(Dialog) do
    begin
      Parent := Dialog;
      Caption := 'Y:';
      SetBounds(136, 23, 10, 13);
    end;

    // show dialog
    if (Dialog.ShowModal = mrOk) then
    begin
      Result.X := SpinX.Value;
      Result.Y := SpinY.Value;
    end;
  finally
    Dialog.Free;
  end;
end;

function TMainForm.ScreenToMap(X, Y: Integer): TPoint;
begin
  Result.X := X div TILE_SIZE;
  Result.Y := Y div TILE_SIZE;

  if (MapKind = mkHexagonal{60}) then
  begin
    if (Result.Y and 1 = 1) then
    begin
      Result.X := (X-TILE_SIZE div 2) div TILE_SIZE;

      if (Result.X = MAP_WIDTH - 1) then Result.X := -1;
      if (X < TILE_SIZE div 2) then Result.X := -1;
    end;

    if (Y mod TILE_SIZE < (TILE_SIZE div 3)) then
    begin
      Result.X := Low(Integer);
      Result.Y := Low(Integer);
    end;
  end;
end;

function TMainForm.MapToScreen(X, Y: Integer; Center: Boolean = True): TPoint;
begin
  Result.X := X * TILE_SIZE;
  Result.Y := Y * TILE_SIZE;

  if (MapKind = mkHexagonal{60}) then
  begin
    if (Y and 1 = 1) then
      Inc(Result.X, TILE_SIZE div 2);

    if (Center) then
    begin
      Inc(Result.X, TILE_SIZE div 2 + 1);
      Inc(Result.Y, (TILE_SIZE * 4 div 3) div 2 - 1);
    end;
  end else
  if (Center) then
  begin
    Inc(Result.X, TILE_SIZE div 2);
    Inc(Result.Y, TILE_SIZE div 2);
  end;
end;

procedure TMainForm.MapPointCorrect(var Point: TPoint);
begin
  if (Point.X < 0) then Point.X := 0;
  if (Point.Y < 0) then Point.Y := 0;
  if (Point.X >= MAP_WIDTH) then Point.X := MAP_WIDTH - 1;
  if (Point.Y >= MAP_HEIGHT) then Point.Y := MAP_HEIGHT - 1;

  if (MapKind = mkHexagonal{60}) and (Point.Y and 1 = 1) and
    (Point.X = MAP_WIDTH - 1) then Dec(Point.X);
end;

procedure TMainForm.RepaintBoxes(const PaintBoxes: array of TPaintBox);
var
  i: Integer;
  PaintBox: TPaintBox;
begin
  for i := 0 to High(PaintBoxes) do
  begin
    PaintBox := PaintBoxes[i];
    if (Assigned(PaintBox.OnPaint)) then PaintBox.OnPaint(PaintBox);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
type
  TBGR = packed record
    B, G, R: Byte;
  end;
  PBGR = ^TBGR;
var
  i, Len: Integer;
  FileName: string;
  F: TFileStream;
  GreyTile, SmallCell: TBitmap;
  MapKindValue: Byte;

  function LoadBitmap(const FileName: string): TBitmap;
  var
    F: TStream;
  begin
    Result := TBitmap.Create;
    {$ifdef DEBUG}
      F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    {$else}
      F := TResourceStream.Create(hInstance, 'bmp' + ChangeFileExt(FileName, ''), RT_RCDATA);
    {$endif}
    try
      Result.LoadFromStream(F);
    finally
      F.Free;
    end;
  end;

  procedure MixGreyBitmap(Dest, Src: PBGR; const Width, Height: Integer);
  const
    GREY_VALUE = 200;
    GREY_ALPHA = 128;
  var
    Gap, i, j: Integer;
  begin
    Gap := (4 - (Width * 3)) mod 4;

    for j := 0 to Height - 1 do
    begin
      for i := 0 to Width - 1 do
      begin
        Dest.B := Src.B + (GREY_ALPHA * (GREY_VALUE - Src.B) shr 8);
        Dest.G := Src.G + (GREY_ALPHA * (GREY_VALUE - Src.G) shr 8);
        Dest.R := Src.R + (GREY_ALPHA * (GREY_VALUE - Src.R) shr 8);

        Inc(Dest);
        Inc(Src);
      end;

      Inc(NativeInt(Dest), Gap);
      Inc(NativeInt(Src), Gap);
    end;
  end;

  function ReadInt: Integer;
  begin
    F.ReadBuffer(Result, SizeOf(Result));
  end;

  function ReadByte(const DefaultValue: Byte): Byte;
  begin
    if (F.Read(Result, SizeOf(Result)) <> SizeOf(Result)) then
      Result := DefaultValue;
  end;

  function ReadBool: Boolean;
  begin
    F.ReadBuffer(Result, SizeOf(Result));
  end;

begin
  Randomize;
  FMousePressed := mbMiddle{neutral value};
  Application.Title := 'CrystalPathFinding (CPF)';
  ProjectPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  Weights := TTileMapWeights.Create;

  // bitmaps
  for i := 0 to TILES_COUNT - 1 do
  begin
    GreyTile := TBitmap.Create;
    SmallCell := TBitmap.Create;
    TileBitmaps[i * 4] := LoadBitmap(IntToStr(i) + '_.bmp'{60x60});
    TileBitmaps[i * 4 + 1] := GreyTile;
    TileBitmaps[i * 4 + 2] := SmallCell;
    TileBitmaps[i * 4 + 3] := LoadBitmap(IntToStr(i) + '.bmp'{40x40});

    GreyTile.PixelFormat := pf24bit;
    GreyTile.Width := TileBitmaps[i * 4].Width;
    GreyTile.Height := TileBitmaps[i * 4].Height;
    MixGreyBitmap(GreyTile.ScanLine[GreyTile.Height - 1],
      TileBitmaps[i * 4].ScanLine[GreyTile.Height - 1], GreyTile.Width, GreyTile.Height);

    SmallCell.PixelFormat := pf24bit;
    SmallCell.Width := TILE_SIZE;
    SmallCell.Height := TILE_SIZE;
    SetStretchBltMode(SmallCell.Canvas.Handle, HALFTONE);
    SmallCell.Canvas.CopyRect(Rect(0, 0, TILE_SIZE, TILE_SIZE),
      TileBitmaps[i * 4 + 3].Canvas, Rect(0, 0, FULL_TILE_SIZE, FULL_TILE_SIZE));
  end;
  WhiteCell := LoadBitmap('white.bmp');
  GreyCell := LoadBitmap('grey.bmp');
  MaskHexagonal := LoadBitmap('hex60.bmp');

  // initialize (load) pframeters
  BeginUpdate;
  try
    // empty map, default parameters
    btnClear.Click;

    // loading last map state from local "SAVE.dat" file
    FileName := ProjectPath + 'SAVE.dat';
    if (FileExists(FileName)) then
    begin
      F := TFileStream.Create(FileName, fmShareDenyNone);
      try
        F.Read(TILE_MAP, SizeOf(TILE_MAP));
        IncrementTiles;{compatibility routine};
        F.Read(FStartPoint, SizeOf(FStartPoint));
        F.Read(FFinishPoint, SizeOf(FFinishPoint));
        F.Read(FTileMode, SizeOf(FTileMode));
        Inc(FTileMode);
        F.Read(FBarrierMode, SizeOf(FBarrierMode));
        F.Read(MapKindValue, SizeOf(MapKindValue));
        if (MapKindValue > Byte(High(TTileMapKind))) then MapKindValue := Byte(High(TTileMapKind));
        MapKind := TTileMapKind(MapKindValue);

        UseWeights := ReadBool;
        ReadBool{SmartWeight: compatibility skip only};
        SectorTest := ReadBool;

        sbTile1.Position := ReadInt;
        sbTile2.Position := ReadInt;
        sbTile3.Position := ReadInt;
        sbTile4.Position := ReadInt;
        seIterationsCount.Value := ReadInt;

        Len := ReadInt;
        if (Len > 0) then
        begin
          SetLength(FExcludedPoints, Len);
          F.Read(pointer(FExcludedPoints)^, Len * SizeOf(TPoint));

          for i := 0 to Len - 1 do
            MapPointCorrect(FExcludedPoints[i]);
        end;

        TestingMode := TTestingMode(ReadByte(0));
        SameDiagonalWeight := (ReadByte(0) <> 0);
        FullPath := (ReadByte(1) <> 0);
      finally
        F.Free;
      end;
    end;

    // raster map buffer
    MapBitmap := TBitmap.Create;
    MapBitmap.PixelFormat := pf24bit;
    MapBitmap.Width := MAP_WIDTH * TILE_SIZE;
    MapBitmap.Height := MAP_HEIGHT * TILE_SIZE;
  finally
    // create map, find path, repaint
    EndUpdate;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // force enable/disable scrollbars (VCL bug fix)
  sbTile1.Enabled := FUseWeights;
  sbTile2.Enabled := FUseWeights;
  sbTile3.Enabled := FUseWeights;
  sbTile4.Enabled := FUseWeights;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  // save current map state
  SaveMap;

  // remove allocated instances
  Weights.Free;
  Map.Free;

  for i := 0 to TILES_COUNT * 4 - 1 do
  TileBitmaps[i].Free;

  MapBitmap.Free;

  WhiteCell.Free;
  GreyCell.Free;
  MaskHexagonal.Free;
end;

procedure TMainForm.FormDblClick(Sender: TObject);
begin
  {$if Defined(DEBUG) and (not Defined(USECPFDLL))}
  Map.SaveHotPoolToFile;
  {$ifend}
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

procedure TMainForm.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TMainForm.EndUpdate;
begin
  Dec(FUpdateCounter);
  TryUpdate;
end;

// main method!
// execute finding path algorithm
// and repaint map
procedure TMainForm.TryUpdate;
var
  Path: TTileMapPath;
begin
  if (FUpdateCounter <> 0) then Exit;

  // recreate map, fill tiles
  if (not FActualMap) then
  begin
    FActualMap := True;
    FreeAndNil(Map);
    Map := TTileMap.Create(MAP_WIDTH, MAP_HEIGHT, FMapKind, FSameDiagonalWeight);
    Map.Update(@TILE_MAP[0, 0], 0, 0, MAP_WIDTH, MAP_HEIGHT);
  end;

  // find path
  try
    Path := ExecutePathFinding;
    {$ifNdef USECPFDLL}
    FCachedAttainablePoints := Map.CachedAttainablePoints;
    FCachedUnattainablePoints := Map.CachedUnattainablePoints;
    {$endif}
  except
    SaveMap;
    Path.Count := 0;
    FillMapBitmap(Path, ProjectPath + 'map_exception.jpg');
    raise;
  end;

  // repaint map
  FillMapBitmap(Path);
  RepaintBoxes([pbMap]);
end;

function TMainForm.ExecutePathFinding: TTileMapPath;
var
  Params: TTileMapParams;
  Buffer: TTileMapPath;
  Size, i: Integer;
begin
  // basic parameters
  Map.Caching := FTestingMode in [tmOneCaching, tmManyStandardCaching];
  Map.SectorTest := FSectorTest;
  Params.Finish := FinishPoint;
  if (FUseWeights) then Params.Weights := Weights{$ifdef USECPFDLL}.Handle{$endif}
  else Params.Weights := {$ifNdef USECPFDLL}nil{$else}0{$endif};
  Params.Excludes := PPoint(FExcludedPoints);
  Params.ExcludesCount := Length(FExcludedPoints);

  // find path
  if (FTestingMode <> tmMany) then
  begin
    if (FTestingMode in [tmOne, tmOneCaching]) then
    begin
      Params.Starts := @StartPoint;
      Params.StartsCount := 1;
    end else
    begin
      Params.Starts := Pointer(FManyStartPoints);
      Params.StartsCount := Length(FManyStartPoints);
    end;

    Result := Map.FindPath(Params, FFullPath);
  end else
  begin
    // emulate many start points finding (tmMany)
    Result.Count := 0;
    Result.Distance := MaxDouble;
    Params.StartsCount := 1;
    Size := Length(FPathPointBuffer);

    for i := 0 to Length(FManyStartPoints) - 1 do
    begin
      Params.Starts := @FManyStartPoints[i];
      Buffer := Map.FindPath(Params, FFullPath);

      if (Buffer.Count <> 0) and (Buffer.Distance < Result.Distance) then
      begin
        Result.Index := Buffer.Index;
        Result.Count := Buffer.Count;
        Result.Distance := Buffer.Distance;

        if (Size < Buffer.Count) then
        begin
          Size := Buffer.Count;
          SetLength(FPathPointBuffer, Size);
        end;

        Result.Points := Pointer(FPathPointBuffer);
        Move(Buffer.Points^, Result.Points^, SizeOf(TPoint) * Buffer.Count);
      end;
    end;
  end;
end;

procedure TMainForm.FillMapBitmap(const Path: TTileMapPath; const JpgFileName: string = '');
var
  Index: Byte;
  Hexagonal: Boolean;
  BitmapMask: TBitmap;
  i, j: Integer;
  PointsCount: Integer;
  P: TPoint;
  Canvas: TCanvas;
  Text: string;
  TextWidth: Integer;
  JI: TJpegImage;
  UsedTiles: array[1..TILES_COUNT] of Boolean;

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

  procedure DrawTile(X, Y: integer; Index: Byte {TILE_BARRIER = 0});
  begin
    P := MapToScreen(X, Y, False);
    if (P.X >= MAP_WIDTH * TILE_SIZE - TILE_SIZE div 2) or
       (P.Y >= MAP_HEIGHT * TILE_SIZE - TILE_SIZE div 2) then Exit;

    // no tile
    if (Index = TILE_BARRIER) or (not UsedTiles[Index]) then
    begin
      if (Hexagonal) then
      begin
        if (Index = TILE_BARRIER) then DrawBitmap(GreyCell)
        else DrawBitmap(WhiteCell);
      end else
      begin
        if (Index = TILE_BARRIER) then Canvas.Brush.Color := clGray
        else Canvas.Brush.Color := clWhite;

        Canvas.FillRect(Bounds(P.X, P.Y, TILE_SIZE, TILE_SIZE));
      end;

      Exit;
    end;

    // tile
    DrawBitmap(TileBitmaps[(Index - 1) * 4 + 2 + Ord(Hexagonal)]);
  end;

  procedure LineTo(const offsX, offsY: Integer);
  begin
    Inc(P.X, offsX);
    Inc(P.Y, offsY);
    Canvas.LineTo(P.X, P.Y);
  end;

  procedure DrawCellGrid(const X, Y: Integer);
  begin
    if (not Hexagonal) then
    begin
      // basic coordinates
      P := MapToScreen(X, Y, False);
      Canvas.MoveTo(P.X, P.Y);

      // 4 lines around
      LineTo(TILE_SIZE, 0);
      LineTo(0, TILE_SIZE);
      LineTo(-TILE_SIZE, 0);
      LineTo(0, -TILE_SIZE);
    end else
    begin
      P := MapToScreen(X, Y, False);
      Inc(P.X, TILE_SIZE div 2);
      Canvas.MoveTo(P.X, P.Y);

      // 6 lines around
      LineTo(TILE_SIZE div 2, TILE_SIZE div 3);
      LineTo(0, (TILE_SIZE div 3) * 2);
      LineTo(-TILE_SIZE div 2, TILE_SIZE div 3);
      LineTo(-TILE_SIZE div 2, -TILE_SIZE div 3);
      LineTo(0, -(TILE_SIZE div 3) * 2);
      LineTo(TILE_SIZE div 2, -TILE_SIZE div 3);
    end;
  end;

begin
  // analize pathless tiles
  for Index := 1 to TILES_COUNT do
  UsedTiles[Index] := (not FUseWeights) or (Weights[Index] >= 0.1);

  // hexagonal mask
  BitmapMask := nil;
  Hexagonal := (MapKind = mkHexagonal);
  if (Hexagonal) then BitmapMask := MaskHexagonal;

  // clear bitmap
  Canvas := MapBitmap.Canvas;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Rect(0, 0, MapBitmap.Width, MapBitmap.Height));

  // render tiles
  for i := 0 to MAP_WIDTH - 1 do
  for j := 0 to MAP_HEIGHT - 1 do
  begin
    Index := TILE_MAP[j, i];
    if (Index >= 1) and (Index <= TILES_COUNT) then
      DrawTile(i, j, Index);
  end;

  // excluded points
  for i := 0 to Length(FExcludedPoints) - 1 do
  with FExcludedPoints[i] do
  DrawTile(X, Y, TILE_BARRIER);

  // standird grid lines
  Canvas.Pen.Width := 1;
  if (not Hexagonal) then
  begin
    Canvas.Pen.Color := clGray;

    for i := 1 to MAP_WIDTH - 1 do
    begin
      Canvas.MoveTo(i * TILE_SIZE, 0);
      Canvas.LineTo(i * TILE_SIZE, MAP_HEIGHT * TILE_SIZE);
    end;
    for j := 1 to MAP_HEIGHT - 1 do
    begin
      Canvas.MoveTo(0, j * TILE_SIZE);
      Canvas.LineTo(MAP_WIDTH * TILE_SIZE, j * TILE_SIZE);
    end;
  end else
  // each hexagonal tile render
  begin
    Canvas.Pen.Color := TColor($B0B0B0);

    for i := 0 to MAP_WIDTH - 1 do
    for j := 0 to MAP_HEIGHT - 1 do
      DrawCellGrid(i, j);
  end;

  // cached grid lines
  {$ifNdef USECPFDLL}
  begin
    Canvas.Pen.Color := clMaroon;
    for i := 0 to Length(FCachedUnattainablePoints) - 1 do
    with FCachedUnattainablePoints[i] do
      DrawCellGrid(X, Y);

    Canvas.Pen.Color := DISTANCE_TEXT_COLOR;
    for i := 0 to Length(FCachedAttainablePoints) - 1 do
    with FCachedAttainablePoints[i] do
      DrawCellGrid(X, Y);
  end;
  {$endif}

  // path
  PointsCount := Path.Count;
  if (not FullPath) and (PointsCount > 2) then PointsCount := 2;
  if (PointsCount > 1) then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Pen.Color := clRed;
    with MapToScreen(Path.Points[0].X, Path.Points[0].Y) do
      Canvas.MoveTo(X, Y);

    for i := 1 to PointsCount - 1 do
    with MapToScreen(Path.Points[i].X, Path.Points[i].Y) do
      Canvas.LineTo(X, Y);
  end;

  // start points, finish point
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := clBlue;
  for i := 1 to Length(FManyStartPoints) - 1 do
  begin
    P := MapToScreen(FManyStartPoints[i].X, FManyStartPoints[i].Y);
    Canvas.Ellipse(Bounds(P.X - 4, P.Y - 4, 10, 10));
  end;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 2;
  if (FMousePressed = mbLeft) and (FCursorPoint.X = FStartPoint.X) and
    (FCursorPoint.Y = FStartPoint.Y) then Canvas.Brush.Color := clAqua;
  Canvas.Pen.Color := clRed;
  P := MapToScreen(StartPoint.X, StartPoint.Y);
  Canvas.Ellipse(Bounds(P.X - 9, P.Y - 9, 20, 20));
  Canvas.Pen.Width := 2;
  Canvas.Brush.Color := clRed;
  if (FMousePressed = mbLeft) and (FCursorPoint.X = FFinishPoint.X) and
    (FCursorPoint.Y = FFinishPoint.Y) then Canvas.Brush.Color := clFuchsia;
  Canvas.Pen.Color := clBlue;
  P := MapToScreen(FinishPoint.X, FinishPoint.Y);
  Canvas.Ellipse(Bounds(P.X - 9, P.Y - 9, 20, 20));

  // distance
  if (Path.Count <> 0) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := DISTANCE_TEXT_COLOR;
    Canvas.Font.Style := [fsBold];
    Text := LocalFloatToStr(Path.Distance);
    TextWidth := Canvas.TextWidth(Text);

    P := MapToScreen(FinishPoint.X, FinishPoint.Y);
    P.Y := P.Y - TILE_SIZE;
    if (P.Y < 0) then
      P.Y := P.Y + TILE_SIZE;
    P.X := P.X + TILE_SIZE div 4;
    if (P.X + TextWidth > MapBitmap.Width - 2) then
      P.X := MapBitmap.Width - 2 - TextWidth;

    Canvas.TextOut(P.X, P.Y, Text);
  end;

  // save
  if (JpgFileName <> '') then
  begin
    JI := TJpegImage.Create;
    try
      JI.Assign(MapBitmap);
      JI.SaveToFile(JpgFileName);
    finally
      JI.Free;
    end;
  end;
end;

procedure TMainForm.SaveMap;
var
  F: TFileStream;
  Len: Integer;
  TileMode, MapKind: Byte;

  procedure WriteInt(Value: Integer);
  begin
    F.WriteBuffer(Value, SizeOf(Value));
  end;

  procedure WriteByte(Value: Byte);
  begin
    F.WriteBuffer(Value, SizeOf(Value));
  end;

  procedure WriteBool(Value: Boolean);
  begin
    F.WriteBuffer(Value, SizeOf(Value));
  end;  
begin
  F := TFileStream.Create(ProjectPath + 'SAVE.dat', fmCreate);
  try
    // compatibility routine
    begin
      DecrementTiles;
      try
        F.Write(TILE_MAP, SizeOf(TILE_MAP));
      finally
        IncrementTiles;
      end;
      TileMode := FTileMode - 1;
      MapKind := Byte(FMapKind);
      if (FMapKind = mkHexagonal) then Inc(MapKind);
    end;

    F.Write(FStartPoint, SizeOf(FStartPoint));
    F.Write(FFinishPoint, SizeOf(FFinishPoint));
    F.Write(TileMode{modified}, SizeOf(TileMode));
    F.Write(FBarrierMode, SizeOf(FBarrierMode));
    F.Write(MapKind{modified}, SizeOf(MapKind));

    WriteBool(FUseWeights);
    WriteBool(True{SmartWeight: compatibility skip only});
    WriteBool(FSectorTest);

    WriteInt(sbTile1.Position);
    WriteInt(sbTile2.Position);
    WriteInt(sbTile3.Position);
    WriteInt(sbTile4.Position);
    WriteInt(seIterationsCount.Value);

    Len := Length(FExcludedPoints);
    WriteInt(Len);
    if (Len <> 0) then F.Write(Pointer(FExcludedPoints)^, Len * SizeOf(TPoint));

    WriteByte(Byte(FTestingMode));
    WriteBool(FSameDiagonalWeight);
    WriteBool(FFullPath);
  finally
    F.Free;
  end;
end;  

procedure TMainForm.InitializeManyStartPoints;
const
  OFFSETS: array[0..4] of TPoint =
   ((X: 0; Y: 0), (X: -2; Y: -2), (X: 2; Y: -2), (X: 2; Y: 2), (X: -2; Y: 2));
var
  i: Integer;

  procedure AddPoint(const X, Y: Integer);
  var
    P: TPoint;
    Len: Integer;
  begin
    P.X := X;
    P.Y := Y;
    MapPointCorrect(P);

    if (P.X = X) and (P.Y = Y) then
    begin
      Len := Length(FManyStartPoints);
      SetLength(FManyStartPoints, Len + 1);
      FManyStartPoints[Len] := P;
    end;
  end;

begin
  FManyStartPoints := nil;

  if (FTestingMode in [tmMany, tmManyStandard, tmManyStandardCaching]) then
  begin
    for i := Low(OFFSETS) to High(OFFSETS) do
      AddPoint(FStartPoint.X + OFFSETS[i].X, FStartPoint.Y + OFFSETS[i].Y);
  end;
end;

procedure TMainForm.SetStartPoint(const Value: TPoint);
var
  P: TPoint;
begin
  P := Value;
  MapPointCorrect(P);
  if (FStartPoint.X <> P.X) or (FStartPoint.Y <> P.Y) then
  begin
    FStartPoint := P;
    InitializeManyStartPoints;
    TryUpdate;
  end;
end;

procedure TMainForm.SetFinishPoint(const Value: TPoint);
var
  P: TPoint;
begin
  P := Value;
  MapPointCorrect(P);
  if (FFinishPoint.X <> P.X) or (FFinishPoint.Y <> P.Y) then
  begin
    FFinishPoint := P;
    TryUpdate;
  end;
end;

procedure TMainForm.SetTileMode(const Value: Byte);
begin
  if (FTileMode = Value) then Exit;
  FTileMode := Value;
  Windows.SetFocus(TScrollBar(FindComponent('sbTile' + IntToStr(TileMode))).Handle);
  RepaintBoxes([pbTile1, pbTile2, pbTile3, pbTile4]);
end;

procedure TMainForm.SetBarrierMode(const Value: Byte);
begin
  if (FBarrierMode = Value) then Exit;
  FBarrierMode := Value;
  RepaintBoxes([pbBarrier, pbExclude]);
end;

procedure TMainForm.SetMapKind(const Value: TTileMapKind);
var
  i: Integer;
begin
  if (FMapKind = Value) then Exit;
  FMapKind := Value;
  cbMapKind.ItemIndex := Byte(Value);
  cbSameDiagonalWeight.Enabled := (Value in [mkDiagonal, mkDiagonalEx]);
  InitializeManyStartPoints;

  // points correction
  MapPointCorrect(FStartPoint);
  MapPointCorrect(FFinishPoint);
  for i := 0 to Length(FExcludedPoints) - 1 do
    MapPointCorrect(FExcludedPoints[i]);

  // recreate, find path, repaint
  FActualMap := False;
  TryUpdate;
end;

procedure TMainForm.SetUseWeights(const Value: Boolean);
begin
  if (FUseWeights = Value) then Exit;
  FUseWeights := Value;
  cbUseWeights.Checked := Value;
  sbTile1.Enabled := Value;
  sbTile2.Enabled := Value;
  sbTile3.Enabled := Value;
  sbTile4.Enabled := Value;
  Windows.SetFocus(TScrollBar(FindComponent('sbTile' + IntToStr(TileMode))).Handle);
  RepaintBoxes([pbTile1, pbTile2, pbTile3, pbTile4]);
  TryUpdate;
end;

procedure TMainForm.SetSectorTest(const Value: Boolean);
begin
  if (FSectorTest = Value) then Exit;
  FSectorTest := Value;
  cbSectorTest.Checked := Value;
  TryUpdate;
end;

procedure TMainForm.SetTestingMode(const Value: TTestingMode);
begin
  if (FTestingMode = Value) then Exit;
  if (FTestingMode in [tmOneCaching, tmManyStandardCaching]) <>
    (Value in [tmOneCaching, tmManyStandardCaching]) then FActualMap := False;
  FTestingMode := Value;
  cbTestingMode.ItemIndex := Byte(Value);
  InitializeManyStartPoints;
  TryUpdate;
end;

procedure TMainForm.SetSameDiagonalWeight(const Value: Boolean);
begin
  if (FSameDiagonalWeight = Value) then Exit;
  FSameDiagonalWeight := Value;
  cbSameDiagonalWeight.Checked := Value;
  FActualMap := False;
  TryUpdate;
end;

procedure TMainForm.SetFullPath(const Value: Boolean);
begin
  if (FFullPath = Value) then Exit;
  FFullPath := Value;
  cbFullPath.Checked := Value;
  TryUpdate;
end;

procedure TMainForm.AddExcludedPoint(const Value: TPoint);
var
  Len: integer;
begin
  if (PointIndex(Value, FExcludedPoints) >= 0) then Exit;

  Len := Length(FExcludedPoints);
  SetLength(FExcludedPoints, Len + 1);
  FExcludedPoints[Len] := Value;

  TryUpdate;
end;

procedure TMainForm.DeleteExcludedPoint(const Value: TPoint);
var
  P, Len: integer;
begin
  P := PointIndex(Value, FExcludedPoints);
  if (P < 0) then Exit;

  Len := Length(FExcludedPoints) - 1;
  if (P <> Len) then FExcludedPoints[P] := FExcludedPoints[Len];
  SetLength(FExcludedPoints, Len);

  TryUpdate;
end;

procedure TMainForm.pbMapPaint(Sender: TObject);
begin
  TPaintBox(Sender).Canvas.Draw(0, 0, MapBitmap);
end;

procedure TMainForm.pbMapDblClick(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := pbMap.ScreenToClient(P);
  P := ScreenToMap(P.X, P.Y);

  if (P.X = StartPoint.X) and (P.Y = StartPoint.Y) then
  begin
    StartPoint := EditPoint(StartPoint, 'Start');
    FMousePressed := mbMiddle;
  end else
  if (P.X = FinishPoint.X) and (P.Y = FinishPoint.Y) then
  begin
    FinishPoint := EditPoint(FinishPoint, 'Finish');
    FMousePressed := mbMiddle;
  end;
end;

procedure TMainForm.pbMapMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbMiddle) or (FMousePressed = Button) or (ssDouble in Shift) then Exit;

  // unfocus controls
  Windows.SetFocus(0);

  // store button
  FMousePressed := Button;

  // process mouse action
  FCursorPoint := ScreenToMap(X, Y);
  pbMapMouseMove(nil{the first time}, Shift, X, Y);
end;

procedure TMainForm.pbMapMouseUp(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
begin
  if (FMousePressed = Button) then
    FMousePressed := mbMiddle;
end;

procedure TMainForm.pbMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P, LastCursorPoint: TPoint;

  procedure SetTileValue(const X, Y: Integer; const Value: Byte);
  begin
    if (TILE_MAP[Y, X] <> Value) then
    begin
      TILE_MAP[Y, X] := Value;
      Map[X, Y] := Value;
      FActualMap := False;
      TryUpdate;
    end;
  end;
begin
  P := ScreenToMap(X, Y);
  if (P.X < 0) or (P.X >= MAP_WIDTH) or (P.Y < 0) or (P.Y >= MAP_HEIGHT) then Exit;
  if (Sender <> nil) and (FCursorPoint.X = P.X) and (FCursorPoint.Y = P.Y) then Exit;
  {$if Defined(DEBUG) and (not Defined(USECPFDLL))}
    Caption := Map.CellInformation(P.X, P.Y);
  {$ifend}
  LastCursorPoint := FCursorPoint;
  FCursorPoint := P;
  if (FMousePressed = mbMiddle) then Exit;

  // points
  if (FMousePressed = mbLeft) and (LastCursorPoint.X = FStartPoint.X) and
    (LastCursorPoint.Y = FStartPoint.Y) then
  begin
    StartPoint := P;
    Exit;
  end;
  if (FMousePressed = mbLeft) and (LastCursorPoint.X = FFinishPoint.X) and
    (LastCursorPoint.Y = FFinishPoint.Y) then
  begin
    FinishPoint := P;
    Exit;
  end;

  if (FMousePressed = mbLeft) then
  begin
    // tile
    SetTileValue(P.X, P.Y, TileMode);
  end else
  begin
    // barrier
    if (BarrierMode = 0) then
    begin
      SetTileValue(P.X, P.Y, TILE_BARRIER);
    end else
    // excluded point (call TryUpdate automatically)
    begin
      if (PointIndex(P, FExcludedPoints) >= 0) then DeleteExcludedPoint(P)
      else AddExcludedPoint(P);
    end;
  end;
end;

procedure TMainForm.OnTileClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := TPaintBox(Sender).Tag;

  if (Index <= 0) then
  begin
    // change clear mode
    BarrierMode := -Index;
  end else
  begin
    // change current tile
    TileMode := Index;
  end;
end;

procedure TMainForm.OnTilePaint(Sender: TObject);
var
  PaintBox: TPaintBox;
  Index: Integer;
  Color: TColor;
  Active: Boolean;
  PaintBoxRect: TRect;
begin
  PaintBox := TPaintBox(Sender);
  Index := PaintBox.Tag;
  Color := TColor($FFFFFFFF);
  PaintBoxRect := Rect(0, 0, PaintBox.Width, PaintBox.Height);

  if (Index > 0) then
  begin
    if (Weights[Index] < 0.1) then Color := clWhite;
    Active := (TileMode = Index);
  end else
  begin
    if (Index = 0) then Color := clBlack
    else Color := clGray;

    Active := (BarrierMode = (-Index));
  end;

  if (Color = TColor($FFFFFFFF)) then
  begin
    PaintBox.Canvas.Draw(0, 0, TileBitmaps[(Index - 1) * 4 + Ord(not FUseWeights)]);
  end else
  begin
    PaintBox.Canvas.Brush.Style := bsSolid;
    PaintBox.Canvas.Brush.Color := Color;
    PaintBox.Canvas.FillRect(PaintBoxRect);
  end;

  if (Active) then
  begin
    PaintBox.Canvas.Pen.Width := 4;
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.Pen.Color := clBlue;
    if (Index > 0) and (not FUseWeights) then PaintBox.Canvas.Pen.Color := clSilver;

    with PaintBoxRect do
    begin
      Inc(Left, 2);
      Inc(Top, 2);
      Dec(Right, 1);
      Dec(Bottom, 1);
    end;

    PaintBox.Canvas.Rectangle(PaintBoxRect);
  end;
end;

procedure TMainForm.OnTileWeightChange(Sender: TObject);
var
  Index: Byte;
  Weight: Single;
begin
  Index := TScrollBar(Sender).Tag;
  Weight := TScrollBar(Sender).Position / 20;
  TLabel(FindComponent('lbTile' + IntToStr(Index))).Caption := LocalFloatToStr(Weight);

  Weights[Index] := Weight;
  TileMode := Index;
  RepaintBoxes([TPaintBox(FindComponent('pbTile' + IntToStr(Index)))]);
  if (FUseWeights) then TryUpdate;
end;

procedure TMainForm.cbMapKindChange(Sender: TObject);
begin
  MapKind := TTileMapKind(cbMapKind.ItemIndex);
end;

procedure TMainForm.cbSectorTestClick(Sender: TObject);
begin
  SectorTest := cbSectorTest.Checked;
end;

procedure TMainForm.cbUseWeightsClick(Sender: TObject);
begin
  UseWeights := cbUseWeights.Checked;
end;

procedure TMainForm.cbTestingModeChange(Sender: TObject);
begin
  TestingMode := TTestingMode(cbTestingMode.ItemIndex);
end;

procedure TMainForm.cbSameDiagonalWeightClick(Sender: TObject);
begin
  SameDiagonalWeight := cbSameDiagonalWeight.Checked;
end;

procedure TMainForm.cbFullPathClick(Sender: TObject);
begin
  FullPath := cbFullPath.Checked;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  BeginUpdate;
  try
    FActualMap := False;
    FillChar(TILE_MAP, SizeOf(TILE_MAP), 1);
    StartPoint := Point(5, 9);
    FinishPoint := Point(24, 9);
    TileMode := 1;
    BarrierMode := 0;
    MapKind := mkSimple;
    UseWeights := True;
    SectorTest := True;
    FExcludedPoints := nil;

    sbTile1.Position := Round(1.0 * 20);
    sbTile2.Position := Round(1.5 * 20);
    sbTile3.Position := Round(2.5 * 20);
    sbTile4.Position := Round(6.0 * 20);
    seIterationsCount.Value := 10000;

    TestingMode := tmOne;
    SameDiagonalWeight := False;
    FullPath := True;
  finally
    EndUpdate;
  end;
end;

procedure TMainForm.btnRandomClick(Sender: TObject);
var
  i, j: Integer;

  function RandomBool: Boolean;
  begin
    Result := (Random(2) <> 0);
  end;

  function RandomPoint: TPoint;
  begin
    Result.X := Random(MAP_WIDTH);
    Result.Y := Random(MAP_HEIGHT);
    MapPointCorrect(Result);
  end;
begin
  BeginUpdate;
  try
    FActualMap := False;
    for i := 0 to MAP_WIDTH - 1 do
    for j := 0 to MAP_HEIGHT - 1 do
    begin
      TILE_MAP[j, i] := Random(TILES_COUNT + 1);
    end;
    MapKind := TTileMapKind(Random(Byte(High(TTileMapKind)) + 1));

    StartPoint := RandomPoint;
    FinishPoint := RandomPoint;
    TileMode := 1;
    BarrierMode := 0;
    UseWeights := True;
    TestingMode := TTestingMode(Random(Byte(High(TTestingMode)) + 1));
    sbTile1.Position := Random(200);
    sbTile2.Position := Random(200);
    sbTile3.Position := Random(200);
    sbTile4.Position := Random(200);

    FExcludedPoints := nil;
    for i := 0 to Random(20) do AddExcludedPoint(RandomPoint);
    
    seIterationsCount.Value := 10000;
  finally
    EndUpdate;
  end;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  SaveMap;
end;

procedure TMainForm.btnPerformanceTestClick(Sender: TObject);
var
  i, Count: Integer;
  Time: Cardinal;
begin
  Count := seIterationsCount.Value;
  Time := GetTickCount;
  begin
    for i := 1 to Count do
      ExecutePathFinding;
  end;
  Time := GetTickCount - Time;

  ShowMessageFmt('The shortest path was calculated %d times for %d milliseconds', [Count, Time]);
end;


end.
