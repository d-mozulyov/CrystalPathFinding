unit DemoUnit1;


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

{.$define USECPFDLL}

{$ifdef CPFDBG}
  {$undef USECPFDLL}
{$endif}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Types, Dialogs, Math, ExtCtrls, StdCtrls, Spin, JPEG,
  {$ifdef USECPFDLL}cpf{$else}CrystalPathFinding{$endif};

{$R xp_manifest.res}

type
  TTestingMode = (tmOne, tmOneCaching, tmMany, tmManyStandard, tmManyStandardCaching);

type
  TMainForm = class(TForm)
    pbMap: TPaintBox;
    gbTileWeigths: TGroupBox;
    pbTile1: TPaintBox;
    pbTile2: TPaintBox;
    pbTile3: TPaintBox;
    pbTile4: TPaintBox;
    sbTile1: TScrollBar;
    lbTile1: TLabel;
    cbUseWeights: TCheckBox;
    sbTile2: TScrollBar;
    lbTile2: TLabel;
    sbTile3: TScrollBar;
    lbTile3: TLabel;
    sbTile4: TScrollBar;
    lbTile4: TLabel;
    gbBarrierMode: TGroupBox;
    pbBarrier: TPaintBox;
    pbExclude: TPaintBox;
    gbPerformanceTest: TGroupBox;
    seIterationsCount: TSpinEdit;
    btnPerformanceTest: TButton;
    cbTestingMode: TComboBox;
    cbMapKind: TComboBox;
    btnSave: TButton;
    btnRandom: TButton;
    btnClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnTileClick(Sender: TObject);
    procedure OnTilePaint(Sender: TObject);
    procedure OnTileWeightChange(Sender: TObject);
    procedure pbMapPaint(Sender: TObject);
    procedure pbMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnPerformanceTestClick(Sender: TObject);
    procedure cbUseWeightsClick(Sender: TObject);
    procedure pbMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btnClearClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
    procedure pbMapDblClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbTestingModeChange(Sender: TObject);
    procedure cbMapKindChange(Sender: TObject);
  private
    // update/repaint/execute
    FUpdateCounter: Integer;
    FActualMap: Boolean;
    FCursorPoint: TPoint;
    FMousePressed: TMouseButton;

    function ScreenToMap(X, Y: Integer): TPoint;
    function MapToScreen(X, Y: Integer; Center: Boolean = True): TPoint;
    procedure MapPointCorrect(var Point: TPoint);
    procedure RepaintBoxes(const PaintBoxes: array of TPaintBox);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure TryUpdate;
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
    FTestingMode: TTestingMode;
    FExcludedPoints: array of TPoint;

    procedure SetStartPoint(const Value: TPoint);
    procedure SetFinishPoint(const Value: TPoint);
    procedure SetTileMode(const Value: Byte);
    procedure SetBarrierMode(const Value: Byte);
    procedure SetMapKind(const Value: TTileMapKind);
    procedure SetUseWeights(const Value: Boolean);
    procedure SetTestingMode(const Value: TTestingMode);
    function ExcludePointPos(const Value: TPoint): Integer;
    procedure AddExcludedPoint(const Value: TPoint);
    procedure DeleteExcludedPoint(const Value: TPoint);
  public
    property StartPoint: TPoint read FStartPoint write SetStartPoint;
    property FinishPoint: TPoint read FFinishPoint write SetFinishPoint;
    property TileMode: Byte read FTileMode write SetTileMode;
    property BarrierMode: Byte read FBarrierMode write SetBarrierMode;
    property MapKind: TTileMapKind read FMapKind write SetMapKind;
    property UseWeights: Boolean read FUseWeights write SetUseWeights;
    property TestingMode: TTestingMode read FTestingMode write SetTestingMode;
  end;


const
  TILES_COUNT = 4;
  TILE_SIZE = 30;
  FULL_TILE_SIZE = TILE_SIZE + TILE_SIZE div 3;
  MAP_WIDTH = 30;
  MAP_HEIGHT = 20;

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

  function LoadBitmap(const FileName: string): TBitmap;
  begin
    Result := TBitmap.Create;
    Result.LoadFromFile(FileName);
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

      Inc(Integer(Dest), Gap);
      Inc(Integer(Src), Gap);
    end;
  end;

  function ReadInt: Integer;
  begin
    F.ReadBuffer(Result, SizeOf(Result));
  end;

  function ReadByte: Byte;
  begin
    F.ReadBuffer(Result, SizeOf(Result));
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
        F.Read(FMapKind, SizeOf(FMapKind));
        if (Byte(FMapKind) > Byte(High(TTileMapKind))) then FMapKind := High(TTileMapKind);
        cbMapKind.ItemIndex := Byte(FMapKind);

        UseWeights := ReadBool;
        ReadBool{compatibility skip only};
        TestingMode := TTestingMode(ReadByte);

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
  Params: TTileMapParams;
begin
  if (FUpdateCounter <> 0) then Exit;

  // recreate map, fill tiles
  if (not FActualMap) then
  begin
    FActualMap := True;
    FreeAndNil(Map);
    Map := TTileMap.Create(MAP_WIDTH, MAP_HEIGHT, FMapKind);
    Map.Update(@TILE_MAP[0, 0], 0, 0, MAP_WIDTH, MAP_HEIGHT);
  end;

  // find path
  try
    Params.Starts := @StartPoint;
    Params.StartsCount := 1;
    Params.Finish := FinishPoint;
    if (FUseWeights) then Params.Weights := Weights
    else Params.Weights := nil;
    Params.Excludes := PPoint(FExcludedPoints);
    Params.ExcludesCount := Length(FExcludedPoints);

    Map.SectorTest := True;
    Map.Caching := FTestingMode in [tmOneCaching, tmManyStandardCaching];
    Path := Map.FindPath(Params);
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

procedure TMainForm.FillMapBitmap(const Path: TTileMapPath; const JpgFileName: string = '');
var
  Index: Byte;
  Hexagonal: Boolean;
  BitmapMask: TBitmap;
  i, j: Integer;
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

  // lines
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
  for i := 0 to MAP_WIDTH - 1 do
  for j := 0 to MAP_HEIGHT - 1 do
  begin
    // each hexagonal tile render
    Canvas.Pen.Color := TColor($B0B0B0);
    P := MapToScreen(i, j, false);

    // basic coordinates
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

  // path
  if (Path.Count > 1) then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Pen.Color := clRed;
    with MapToScreen(Path.Points[0].X, Path.Points[0].Y) do
      Canvas.MoveTo(X, Y);

    for i := 1 to Path.Count - 1 do
    with MapToScreen(Path.Points[i].X, Path.Points[i].Y) do
      Canvas.LineTo(X, Y);
  end;

  // start point, finish point
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Color := clBlue;
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
    Canvas.Font.Color := {Deeppink}$9314FF;
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
    WriteBool(True{smartweight compatibility only});
    WriteByte(Byte(FTestingMode));

    WriteInt(sbTile1.Position);
    WriteInt(sbTile2.Position);
    WriteInt(sbTile3.Position);
    WriteInt(sbTile4.Position);
    WriteInt(seIterationsCount.Value);

    Len := Length(FExcludedPoints);
    WriteInt(Len);
    if (Len <> 0) then F.Write(Pointer(FExcludedPoints)^, Len * SizeOf(TPoint));
  finally
    F.Free;
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
  RepaintBoxes([pbTile1, pbTile2, pbTile3, pbTile4]);
  TryUpdate;
end;

procedure TMainForm.SetTestingMode(const Value: TTestingMode);
begin
  if (FTestingMode = Value) then Exit;
  FTestingMode := Value;
  cbTestingMode.ItemIndex := Byte(Value);
  TryUpdate;
end;

function TMainForm.ExcludePointPos(const Value: TPoint): integer;
begin
  for Result := 0 to Length(FExcludedPoints) - 1 do
  with FExcludedPoints[Result] do
  if (X = Value.X) and (Y = Value.Y) then Exit;

  Result := -1;
end;

procedure TMainForm.AddExcludedPoint(const Value: TPoint);
var
  Len: integer;
begin
  if (ExcludePointPos(Value) >= 0) then Exit;

  Len := Length(FExcludedPoints);
  SetLength(FExcludedPoints, Len + 1);
  FExcludedPoints[Len] := Value;

  TryUpdate;
end;

procedure TMainForm.DeleteExcludedPoint(const Value: TPoint);
var
  P, Len: integer;
begin
  P := ExcludePointPos(Value);
  if (P < 0) then Exit;

  Len := Length(FExcludedPoints) - 1;
  if (P <> Len) then FExcludedPoints[P] := FExcludedPoints[Len];
  SetLength(FExcludedPoints, Len);

  TryUpdate;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
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
  {$ifdef DEBUG}
    Caption := Map.CellInformation(P.X, P.Y);
  {$endif}
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
      if (ExcludePointPos(P) >= 0) then DeleteExcludedPoint(P)
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
    with PaintBoxRect do
    begin
      Inc(Left, 2);
      Inc(Top, 2);
      Dec(Right, 1);
      Dec(Bottom, 1);
    end;
    PaintBox.Canvas.Pen.Width := 4;
    PaintBox.Canvas.Pen.Color := clBlue;
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.Rectangle(PaintBoxRect);
  end;
end;

procedure TMainForm.cbMapKindChange(Sender: TObject);
begin
  MapKind := TTileMapKind(cbMapKind.ItemIndex);
end;

procedure TMainForm.cbTestingModeChange(Sender: TObject);
begin
  TestingMode := TTestingMode(cbTestingMode.ItemIndex);
end;

procedure TMainForm.cbUseWeightsClick(Sender: TObject);
begin
  UseWeights := cbUseWeights.Checked;
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
  RepaintBoxes([TPaintBox(FindComponent('pbTile' + IntToStr(Index)))]);
  if (FUseWeights) then TryUpdate;
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
    TestingMode := tmOne;
    FExcludedPoints := nil;

    sbTile1.Position := Round(1.0 * 20);
    sbTile2.Position := Round(1.5 * 20);
    sbTile3.Position := Round(2.5 * 20);
    sbTile4.Position := Round(6.0 * 20);
    seIterationsCount.Value := 10000;
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
  Params: TTileMapParams;
begin
  Count := seIterationsCount.Value;
  Time := GetTickCount;
  begin
    Params.Starts := @StartPoint;
    Params.StartsCount := 1;
    Params.Finish := FinishPoint;
    if (FUseWeights) then Params.Weights := Weights
    else Params.Weights := nil;
    Params.Excludes := PPoint(FExcludedPoints);
    Params.ExcludesCount := Length(FExcludedPoints);

    // Execute path finding
    Map.SectorTest := True;
    Map.Caching := FTestingMode in [tmOneCaching, tmManyStandardCaching];
    for i := 1 to Count do
      Map.FindPath(Params);
  end;
  Time := GetTickCount - Time;

  ShowMessageFmt('The shortest path was calculated %d times for %d milliseconds', [Count, Time]);
end;


end.
