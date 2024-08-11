{$IF not defined(CVOPT)}
{$DEFINE CVOPT}
//
{$IF not defined(PACKAGE) and (not defined(WIN64)) and (CompilerVersion < 34.0)}
{$MESSAGE FATAL 'cv.opencv must be compiled as Delphi 10.4 Sydney WIN64'}
{$IFEND}
//
// Define version of OpenCV
{ .$DEFINE OPENCV_455 }
{ .$DEFINE OPENCV_460 }
{ .$DEFINE OPENCV_470 }
{. $DEFINE OPENCV_480}
{$DEFINE OPENCV_4100}
//
{ .$I jedi\jedi.inc }
//
{$IFDEF DEBUG}
{ }{$DEFINE USE_DEBUG_DLLS} // Use debug OpenCV and VisualStudio's DLLs.
{$IFEND}
//
{$IFDEF RELEASE}
{ }{$DEFINE DELAYED_LOAD_DLL}
{ }{$DEFINE USE_INLINE}
{ }{$UNDEF  USE_DEBUG_DLLS} // Do not use debug OpenCV and VisualStudio's DLLs. Else - release OpenCV and VisualStudio's DLLs.
{$ENDIF}
//
{$DEFINE USE_TYPEINFO}
//
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
//
{$WARN SYMBOL_PLATFORM OFF} // turns  off all warnings about the platform directive on symbols in the current unit.
{$WRITEABLECONST ON}        // typed constants can be modified - ON
{$POINTERMATH ON}
{$MINENUMSIZE 4}
{$TYPEDADDRESS ON}
//
{$ELSE}
//
{$MESSAGE FATAL 'Duplicate included file - opt.inc'}
//
{$IFEND} // CVOPT
