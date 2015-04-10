program bitmap_test;

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

  Map: THandle;
  Time: dword;
  found: boolean;
  Start, Finish: TPoint;
  Points: PPointList;
  i: integer;
  RedValue: byte;
  FindResult: PPathMapResult;



// в визуальном предсталении белые - 255, чёрные - 0
// а для карты нужно наоборот. 0 - это клетка тайла. 255 - препятствие
procedure InvertBitmap();
var
  i: integer;
  Dest: pbyte;
begin
  Dest := BitmapData;

  for i := 1 to BitmapHeight*BitmapPitch do
  begin
    if (Dest^ = 0) then Dest^ := 255 else Dest^ := 0;
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
  InvertBitmap();
  Map := cpfCreateMap(BitmapWidth, BitmapHeight, mmSimple, 0, false);
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
    FindResult := cpfFindPath(Map, Start, Finish, 0, nil, 0, false);
  Time := GetTickCount-Time;

  // результат
  if (FindResult = nil) then
  begin
    DeleteFile(DestFileName);
    ShowError('Путь не найден !', []);
  end else
  begin
    // всё нормально
    InvertBitmap();

    // красный
    Points := FindResult.points;
    Bitmap.Canvas.Pixels[Points[0].X, Points[0].Y] := clRed;
    RedValue := BitmapPixel(Points[0].X, Points[0].Y)^;
    for i := 1 to FindResult.points_count-1 do
      BitmapPixel(Points[i].X, Points[i].Y)^ := RedValue;

    Bitmap.SaveToFile(DestFileName);
    ShowInformation('Путь расчитан за %d миллисекунд и результат сохранён в файл "%s"'#13+
                    'Расстояние: %0.2f. (количество точек - %d)',
                    [Time, DestFileName, FindResult.Distance, FindResult.points_count]);
  end;
finally
  cpfDestroyMap(Map);
  Bitmap.Free;
end;

end.
