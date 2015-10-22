program Demo1;

{$R 'Demo1Bitmaps.res' 'Demo1Bitmaps.rc'}

uses
  {$if CompilerVersion >= 23}Vcl.Forms{$else}Forms{$endif},
  DemoUnit1 in 'DemoUnit1.pas' {MainForm},
  CrystalPathFinding in '..\sources\CrystalPathFinding.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
