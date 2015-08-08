program Demo1;

uses
  Forms,
  DemoUnit1 in 'DemoUnit1.pas' {MainForm},
  CrystalPathFinding in '..\sources\CrystalPathFinding.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
