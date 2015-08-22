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
begin

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
