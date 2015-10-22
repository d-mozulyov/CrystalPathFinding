unit TestUnit;

{$i crystal_options.inc}

interface
  uses {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif}{$endif},
       {$ifdef UNITSCOPENAMES}
         System.Types, System.SysUtils, Vcl.ClipBrd,
       {$else}
         SysUtils, ClipBrd,
       {$endif}
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
