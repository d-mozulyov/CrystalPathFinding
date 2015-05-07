unit TestUnit;

{$i crystal_options.inc}

interface
  uses
    Types,
    {$ifdef MSWINDOWS}
      {$if Defined(FPC) or (CompilerVersion < 23)}
        Windows, ClipBrd,
      {$else}
        Winapi.Windows, Vcl.ClipBrd,
      {$ifend}
    {$endif}
    SysUtils,
    CrystalPathFinding;



procedure RUN;
procedure ShowMessage(const S: string); overload;
procedure ShowMessage(const StrFmt: string; const Args: array of const); overload;

implementation

procedure RUN;
var
  Map: TPathMap;
  Weights: TPathMapWeights;
  Start, Finish: TPoint;
  Path: PPathMapResult;
  i: Integer;
begin
  Weights := TPathMapWeights.Create;
  try
    Map := TPathMap.Create(10, 5, mkDiagonal);
    try
      Start := Point(0, 0);
      Finish := Point(Map.Width - 1, Map.Height - 1);

      Writeln(Format('Map find path from (%d,%d) to (%d,%d)...', [Start.X, Start.Y, Finish.X, Finish.Y]));
      Path := Map.FindPath(Start, Finish, Weights);

      if (Path = nil) then
      begin
        Writeln('Path not found.');
      end else
      begin
        Writeln(Format('Points: %d, Distance: %0.2f.', [Path.PointsCount, Path.Distance]));

        for i := 0 to Path.PointsCount - 1 do
        with Path.Points[i] do
         Writeln(Format('%d)  [%d, %d]', [i, X, Y]));
      end;

    finally
      Map.Free;
    end;
  finally
    Weights.Free;
  end;

  ShowMessage('Done!');
end;

procedure ShowMessage(const S: string);
var
  BreakPoint: string;
begin
  BreakPoint := S;

  {$ifdef MSWINDOWS}
    Clipboard.AsText := S;
    MessageBox(0, PChar(BreakPoint), 'Сообщение:', 0);
  {$endif}

  Halt;
end;

procedure ShowMessage(const StrFmt: string; const Args: array of const);
begin
  ShowMessage(Format(StrFmt, Args));
end;

initialization


end.
