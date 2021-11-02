{ .$DEFINE DELAYED_LOAD_DLL }
{ .$IFDEF USE_INLINE }

{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

const
  cvversion           = '454';
  opencv_delphi_dll = 'opencv_delphi' + cvversion + {$IFDEF DEBUG} 'd' + {$ENDIF} '.dll';
  opencv_world_dll  = 'opencv_world' + cvversion + {$IFDEF DEBUG} 'd' + {$ENDIF} '.dll';

Type
  BOOL = LongBool;
  size_t = NativeUInt;
  psize_t = ^size_t;
  Int = integer;
  pInt = ^Int;
  pUChar = type pByte;
  pMatOp = type Pointer;
  pMatAllocator = type Pointer;
  pUMatData = type Pointer;
  pUCharConst = pUChar;
  PointerConst = type Pointer;

