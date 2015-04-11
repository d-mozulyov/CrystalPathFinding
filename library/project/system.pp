unit System;
interface
type
  TGUID = Byte;
  HRESULT = type LongInt;
  PWideChar = ^WideChar;    
var
  _HandleFinally : Byte;
implementation


{$ifdef FPC}
procedure fpc_LibInitializeUnits;[public,alias:'FPC_LIBINITIALIZEUNITS'];
begin
(*  IsLibrary:=true;
  { must also be set to true for packages when implemented }
  ModuleIsLib:=true;
  internal_initializeunits;*)
end;
{$endif}

end.
