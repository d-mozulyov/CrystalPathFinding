unit TestUnit;

{$i crystal_options.inc}

interface
  uses
  {$ifdef MSWINDOWS}
    {$if Defined(FPC) or (CompilerVersion < 23)}Windows{$else}Winapi.Windows{$ifend},
  {$endif}
  SysUtils,
  CrystalPathFinding;



procedure RUN;
procedure ShowMessage(const S: string); overload;
procedure ShowMessage(const StrFmt: string; const Args: array of const); overload;

implementation


procedure RUN;
begin


  ShowMessage('Test');
end;

procedure ShowMessage(const S: string);
var
  BreakPoint: string;
begin
  BreakPoint := S;

  {$ifdef MSWINDOWS}
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
