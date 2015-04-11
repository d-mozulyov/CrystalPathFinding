library cpf;

uses
  CrystalPathFinding in '..\..\sources\CrystalPathFinding.pas';

exports
  cpfInitialize,
  cpfCreateWeights,
  cpfDestroyWeights,
  cpfWeightGet,
  cpfWeightSet,
  cpfCreateMap,
  cpfDestroyMap,
  cpfMapClear,
  cpfMapUpdate,
  cpfMapGetTile,
  cpfMapSetTile,
  cpfFindPath;


//function MessageBoxW(hWnd: LongWord; lpText, lpCaption: PWideChar; uType: LongWord): LongInt; stdcall; external 'user32.dll' name 'MessageBoxW';


procedure cpfTest;
begin

end;

exports
  cpfTest;

begin

end.
