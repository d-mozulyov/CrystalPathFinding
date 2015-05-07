program bitmap_test;

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

uses
  Types,
  Windows,
  SysUtils,
  Classes,
  Graphics,
  cpf in '..\library\cpf.pas';

function MessageBox(const Caption: string; const Fmt: string; const Args: array of const; const Flags: integer): integer;
begin
  Result := Windows.MessageBox(GetForegroundWindow,
            pchar(Format(Fmt, Args)), pchar(Caption), Flags); 
end;

procedure ShowMessage(const Fmt: string; const Args: array of const);
begin
  MessageBox('Сообщение:', Fmt, Args, 0);
end;

procedure ShowError(const Fmt: string; const Args: array of const);
begin
  MessageBox('Ошибка:', Fmt, Args, MB_ICONERROR);
end;

procedure ShowInformation(const Fmt: string; const Args: array of const);
begin
  MessageBox('Информация:', Fmt, Args, MB_ICONINFORMATION);
end;

function  YesNoQuestion(const Fmt: string; const Args: array of const; const DefYesButton: boolean): boolean;
var
  Flags: integer;
begin
  Flags := MB_ICONQUESTION or MB_YESNO;
  if (not DefYesButton) then Flags := Flags or MB_DEFBUTTON2;

  Result := (IDYES = MessageBox('Вопрос:', Fmt, Args, Flags));
end;


const
  FileName = 'bitmap_test.bmp';
  DestFileName = 'bitmap_test_Result.bmp';
  MAX_CELLS_COUNT = 100000000;

var
  Bitmap: TBitmap;
  BitmapWidth: integer;
  BitmapHeight: integer;
  BitmapData: pointer;
  BitmapPitch: integer;
  CellsCount: integer;

  Map: TCPFHandle;
  Time: dword;
  found: boolean;
  Start, Finish: TPoint;
  i: integer;
  RedValue: byte;
  Params: TTileMapParams;
  Path: TTileMapPath;



// в визуальном предсталении белые - не 0, чёрные - 0
// для карты все белые нужно сделать 1
procedure InitializeBitmapTiles();
var
  i: integer;
  Dest: pbyte;
begin
  Dest := BitmapData;

  for i := 1 to BitmapHeight*BitmapPitch do
  begin
    if (Dest^ <> 0) then Dest^ := 1;
    inc(Dest);
  end;
end;

function BitmapPixel(const X, Y: integer): pbyte;
begin
  Result := pbyte(integer(BitmapData)+BitmapPitch*(BitmapHeight-1-Y)+X);
end;


begin

  if (not FileExists(FileName)) then
  begin
    ShowError('Файл "%s" не найден !', [FileName]);
    exit;
  end;

Bitmap := TBitmap.Create;
Map := 0;
try
  Bitmap.LoadFromFile(FileName);
  BitmapWidth := Bitmap.Width;
  BitmapHeight := Bitmap.Height;
  CellsCount := BitmapWidth*BitmapHeight;

  if (BitmapWidth = 0) or (BitmapHeight = 0) then
  begin
    ShowError('Недопустимые размеры изображения: %dx%d', [BitmapWidth, BitmapHeight]);
    exit;
  end;

  if (CellsCount > MAX_CELLS_COUNT) then
  begin
    ShowError('Изображение слишком большое(%dx%d). Количество клеток(%d) превышает максимальное(%d)',
             [BitmapWidth, BitmapHeight, CellsCount, MAX_CELLS_COUNT]);
    exit;         
  end;

  if (not YesNoQuestion('Изображение(%dx%d) содержит %d клеток. Расчёты могут занять некоторое время.'#13+
                        'Хотите продолжить ?', [BitmapWidth, BitmapHeight, CellsCount], false)) then exit;

  Bitmap.PixelFormat := pf8bit;
  BitmapData := Bitmap.ScanLine[BitmapHeight-1];
  BitmapPitch := (BitmapWidth+3) and -4;


  // загрузка карты
  InitializeBitmapTiles();
  Map := cpfCreateMap(BitmapWidth, BitmapHeight, mkSimple);
  cpfMapUpdate(Map, BitmapPixel(0, 0), 0, 0, BitmapWidth, BitmapHeight, -BitmapPitch);
  
  // поиск точек Start/Finish
  found := false;
  Start.Y := 0;
  for i := 0 to BitmapWidth-1 do
  if BitmapPixel(i, Start.Y)^ = 0 then
  begin
    Start.X := i;
    found := true;
    break;
  end;
  if (not found) then
  begin
    ShowError('Стартовая точка не найдена', []);
    exit;
  end;
  found := false;
  Finish.Y := BitmapHeight-1;
  for i := BitmapWidth-1 downto 0 do
  if BitmapPixel(i, Finish.Y)^ = 0 then
  begin
    Finish.X := i;
    found := true;
    break;
  end;
  if (not found) then
  begin
    ShowError('Конечная точка не найдена', []);
    exit;
  end;

  // расчёт
  Time := GetTickCount;
    Params.Starts := @Start;
    Params.StartsCount := 1;
    Params.Finish := Finish;
    Params.Weights := 0;
    Params.Excludes := nil;
    Params.ExcludesCount := 0;
    Path := cpfFindPath(Map, @Params, false, false);
  Time := GetTickCount-Time;

  // результат
  if (Path.Count = 0) then
  begin
    DeleteFile(DestFileName);
    ShowError('Путь не найден !', []);
  end else
  begin
    // красный
    Bitmap.Canvas.Pixels[Path.Points[0].X, Path.Points[0].Y] := clRed;
    RedValue := BitmapPixel(Path.Points[0].X, Path.Points[0].Y)^;
    for i := 1 to Path.Count-1 do
      BitmapPixel(Path.Points[i].X, Path.Points[i].Y)^ := RedValue;

    Bitmap.SaveToFile(DestFileName);
    ShowInformation('Путь расчитан за %d миллисекунд и результат сохранён в файл "%s"'#13+
                    'Расстояние: %0.2f. (количество точек - %d)',
                    [Time, DestFileName, Path.Distance, Path.Count]);
  end;
finally
  cpfDestroyMap(Map);
  Bitmap.Free;
end;

end.
