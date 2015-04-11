unit types;

  interface

type
  PByte = ^Byte;
  PWord = ^Word;
  Integer = LongInt;
  PInteger = ^Integer;
  Cardinal = LongWord;
  PCardinal = ^Cardinal;
  
  TPoint = packed record
    X, Y: Integer; 
  end;
  PPoint = ^TPoint;
  
  UInt64 = QWord;
  
  {$if Defined(CPUX64) or Defined(CPUARM64)}
    NativeInt = Int64;
	NativeUInt = UInt64;
  {$else}
    NativeInt = Integer;
	NativeUInt = Cardinal;    
  {$ifend}    
  PNativeInt = ^NativeInt;
  PNativeUInt = ^NativeUInt;

  TSmallPoint = packed
  record
     x : SmallInt;
     y : SmallInt;
  end;
  PSmallPoint = ^TSmallPoint;


implementation
  
end.
