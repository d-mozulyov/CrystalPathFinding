unit SysInit;
interface
var
  _HandleFinally : Byte;
  procedure ExitProcess(uExitCode: Cardinal); stdcall; external 'kernel32.dll' name '_ExitProcess@4';
implementation
initialization
//... код ...
  ExitProcess(0);
end.
