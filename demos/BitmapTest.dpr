program BitmapTest;


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

function MessageBox(const Caption: string; const Fmt: string;
  const Args: array of const; const Flags: Integer): Integer;
begin
  Result := Windows.MessageBox(GetForegroundWindow,
            PChar(Format(Fmt, Args)), PChar(Caption), Flags); 
end;

procedure ShowMessage(const Fmt: string; const Args: array of const);
begin
  MessageBox('Information:', Fmt, Args, 0);
end;

procedure ShowError(const Fmt: string; const Args: array of const);
begin
  MessageBox('Error:', Fmt, Args, MB_ICONERROR);
end;

procedure ShowInformation(const Fmt: string; const Args: array of const);
begin
  MessageBox('Information:', Fmt, Args, MB_ICONINFORMATION);
end;

function  YesNoQuestion(const Fmt: string; const Args: array of const;
  const DefYesButton: Boolean): Boolean;
var
  Flags: Integer;
begin
  Flags := MB_ICONQUESTION or MB_YESNO;
  if (not DefYesButton) then Flags := Flags or MB_DEFBUTTON2;

  Result := (IDYES = MessageBox('Question:', Fmt, Args, Flags));
end;


const
  FileName = 'BitmapTest.bmp';
  DestFileName = 'BitmapTest_Result.bmp';
  CELLCOUNT_LIMIT = 16 * 1000 * 1000;

var
  Bitmap: TBitmap;
  BitmapWidth: Integer;
  BitmapHeight: Integer;
  BitmapData: Pointer;
  BitmapPitch: Integer;
  CellCount: Integer;

  Map: TCPFHandle;
  Time: Cardinal;
  Found: Boolean;
  Start, Finish: TPoint;
  i: Integer;
  Pix: PByte;
  RedValue: Byte;
  Params: TTileMapParams;
  Path: TTileMapPath;


function BitmapPixel(const X, Y: Integer): PByte;
begin
  Result := BitmapData;
  Inc(Result, BitmapPitch * (BitmapHeight - 1 - Y) + X);
end;

begin
  // file exists test
  if (not FileExists(FileName)) then
  begin
    ShowError('File "%s" not found', [FileName]);
    Exit;
  end;

Bitmap := TBitmap.Create;
Map := 0;
try
  // loading
  Bitmap.LoadFromFile(FileName);
  BitmapWidth := Bitmap.Width;
  BitmapHeight := Bitmap.Height;
  CellCount := BitmapWidth * BitmapHeight;

  if (BitmapWidth <= 1) or (BitmapHeight <= 1) then
  begin
    ShowError('Incorrect map size: %dx%d', [BitmapWidth, BitmapHeight]);
    Exit;
  end;

  if (CellCount > CELLCOUNT_LIMIT) then
  begin
    ShowError('Too large map size %dx%d, cell count limit is %d', [BitmapWidth, BitmapHeight, CELLCOUNT_LIMIT]);
    Exit;         
  end;

  Bitmap.PixelFormat := pf8bit;
  BitmapData := Bitmap.ScanLine[BitmapHeight - 1];
  BitmapPitch := (BitmapWidth + 3) and -4;

  // map tiles loading
  Pix := BitmapData;
  for i := 1 to BitmapHeight * BitmapPitch do
  begin
    if (Pix^ <> 0) then Pix^ := 1;
    Inc(Pix);
  end;
  Map := cpfCreateMap(BitmapWidth, BitmapHeight, mkSimple);
  cpfMapUpdate(Map, BitmapPixel(0, 0), 0, 0, BitmapWidth, BitmapHeight, -BitmapPitch);
  
  // take Start/Finish points
  Found := False;
  Start.Y := 0;
  for i := 0 to BitmapWidth - 1 do
  if (BitmapPixel(i, Start.Y)^ = 0) then
  begin
    Start.X := i;
    Found := True;
    Break;
  end;
  if (not Found) then
  begin
    ShowError('Start point not found', []);
    Exit;
  end;
  Found := false;
  Finish.Y := BitmapHeight - 1;
  for i := BitmapWidth - 1 downto 0 do
  if (BitmapPixel(i, Finish.Y)^ = 0) then
  begin
    Finish.X := i;
    Found := True;
    Break;
  end;
  if (not Found) then
  begin
    ShowError('Finish point not found', []);
    exit;
  end;

  // finding path
  Time := GetTickCount;
    Params.Starts := @Start;
    Params.StartsCount := 1;
    Params.Finish := Finish;
    Params.Weights := 0;
    Params.Excludes := nil;
    Params.ExcludesCount := 0;
    Path := cpfFindPath(Map, @Params, False, False);
  Time := GetTickCount - Time;

  // result
  if (Path.Count = 0) then
  begin
    DeleteFile(DestFileName);
    ShowError('Path not found!', []);
  end else
  begin
    // red points
    Bitmap.Canvas.Pixels[Path.Points[0].X, Path.Points[0].Y] := clRed;
    RedValue := BitmapPixel(Path.Points[0].X, Path.Points[0].Y)^;
    for i := 1 to Path.Count - 1 do
      BitmapPixel(Path.Points[i].X, Path.Points[i].Y)^ := RedValue;

    Bitmap.SaveToFile(DestFileName);
    ShowInformation('Processing time is %d milliseconds, the path was saved to "%s" file'#13+
                    'Distance: %0.2f (%d points)',
                    [Time, DestFileName, Path.Distance, Path.Count]);
  end;
finally
  cpfDestroyMap(Map);
  Bitmap.Free;
end;

end.
