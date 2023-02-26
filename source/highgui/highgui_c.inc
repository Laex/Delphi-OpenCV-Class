(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the 'License');
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an 'AS IS' BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

{$ifndef OPENCV_HIGHGUI_H}
{$define OPENCV_HIGHGUI_H}

{$include 'core/core_c.inc'}
{$include 'imgproc/imgproc_c.inc'}

(** @addtogroup highgui_c
  @{
  *)

(****************************************************************************************\
*                                  Basic GUI functions                                   *
\****************************************************************************************)
//YV
const
//-----------New for Qt
(* For font *)
//enum {
        CV_FONT_LIGHT           = 25;//QFont::Light,
        CV_FONT_NORMAL          = 50;//QFont::Normal,
        CV_FONT_DEMIBOLD        = 63;//QFont::DemiBold,
        CV_FONT_BOLD            = 75;//QFont::Bold,
        CV_FONT_BLACK           = 87; //QFont::Black
//};

//enum {
        CV_STYLE_NORMAL         = 0;//QFont::StyleNormal,
        CV_STYLE_ITALIC         = 1;//QFont::StyleItalic,
        CV_STYLE_OBLIQUE        = 2; //QFont::StyleOblique
//};
(* ---------*)

{TODO:cvFontQt}
//for color cvScalar(blue_component, green_component, red_component[, alpha_component])
//and alpha= 0 <-> 0xFF (not transparent <-> transparent)
//CVAPI(CvFont) cvFontQt(const char* nameFont, int pointSize CV_DEFAULT(-1), CvScalar color CV_DEFAULT(cvScalarAll(0)), int weight CV_DEFAULT(CV_FONT_NORMAL),  int style CV_DEFAULT(CV_STYLE_NORMAL), int spacing CV_DEFAULT(0));

//CVAPI(void) cvAddText(const CvArr* img, const char* text, CvPoint org, CvFont *arg2);

//CVAPI(void) cvDisplayOverlay(const char* name, const char* text, int delayms CV_DEFAULT(0));
//CVAPI(void) cvDisplayStatusBar(const char* name, const char* text, int delayms CV_DEFAULT(0));

//CVAPI(void) cvSaveWindowParameters(const char* name);
//CVAPI(void) cvLoadWindowParameters(const char* name);
//CVAPI(int) cvStartLoop(int ( *pt2Func)(int argc, char *argv[]), int argc, char* argv[]);
//CVAPI(void) cvStopLoop( void );

//typedef void (CV_CDECL *CvButtonCallback)(int state, void* userdata);
//enum {CV_PUSH_BUTTON = 0, CV_CHECKBOX = 1, CV_RADIOBOX = 2};
//CVAPI(int) cvCreateButton( const char* button_name CV_DEFAULT(NULL),CvButtonCallback on_change CV_DEFAULT(NULL), void* userdata CV_DEFAULT(NULL) , int button_type CV_DEFAULT(CV_PUSH_BUTTON), int initial_button_state CV_DEFAULT(0));
//----------------------


(* this function is used to set some external parameters in case of X Window *)
//CVAPI(int) cvInitSystem( int argc, char** argv );
function cvInitSystem(argc:int; argv:pcvchar):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7825
{$ELSE}
name 'cvInitSystem'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(int) cvStartWindowThread( void );
function cvStartWindowThread():int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7991
{$ELSE}
name 'cvStartWindowThread'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

const
// ---------  YV ---------
//enum
//{
    //These 3 flags are used by cvSet/GetWindowProperty
    CV_WND_PROP_FULLSCREEN = 0; //to change/get window's fullscreen property
    CV_WND_PROP_AUTOSIZE   = 1; //to change/get window's autosize property
    CV_WND_PROP_ASPECTRATIO= 2; //to change/get window's aspectratio property
    CV_WND_PROP_OPENGL     = 3; //to change/get window's opengl support
    CV_WND_PROP_VISIBLE    = 4;

    //These 2 flags are used by cvNamedWindow and cvSet/GetWindowProperty
    CV_WINDOW_NORMAL       = $00000000; //the user can resize the window (no constraint)  / also use to switch a fullscreen window to a normal size
    CV_WINDOW_AUTOSIZE     = $00000001; //the user cannot resize the window, the size is constrainted by the image displayed
    CV_WINDOW_OPENGL       = $00001000; //window with opengl support

    //Those flags are only for Qt
    CV_GUI_EXPANDED         = $00000000; //status bar and tool bar
    CV_GUI_NORMAL           = $00000010; //old fashious way

    //These 3 flags are used by cvNamedWindow and cvSet/GetWindowProperty
    CV_WINDOW_FULLSCREEN   = 1;//change the window to fullscreen
    CV_WINDOW_FREERATIO    = $00000100;//the image expends as much as it can (no ratio constraint)
    CV_WINDOW_KEEPRATIO    = $00000000;//the ration image is respected.
//};

(* create window *)
//CVAPI(int) cvNamedWindow( const char* name, int flags CV_DEFAULT(CV_WINDOW_AUTOSIZE) );
function cvNamedWindow(const name:pcvchar; flags:int=CV_WINDOW_AUTOSIZE):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7860
{$ELSE}
name 'cvNamedWindow'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Set and Get Property of the window *)
//CVAPI(void) cvSetWindowProperty(const char* name, int prop_id, double prop_value);
procedure cvSetWindowProperty(const name:pcvchar; prop_id:int; prop_value:double);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7975
{$ELSE}
name 'cvSetWindowProperty'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(double) cvGetWindowProperty(const char* name, int prop_id);
function cvGetWindowProperty(const name:pcvchar; prop_id:int):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7801
{$ELSE}
name 'cvGetWindowProperty'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* display image within window (highgui windows remember their content) *)
//CVAPI(void) cvShowImage( const char* name, const CvArr* image );
procedure cvShowImage(const name:pcvchar; const image:pCvArr);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7977
{$ELSE}
name 'cvShowImage'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* resize/move window *)
//CVAPI(void) cvResizeWindow( const char* name, int width, int height );
procedure cvResizeWindow(const name:pcvchar; width:int; height:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7918
{$ELSE}
name 'cvResizeWindow'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvMoveWindow( const char* name, int x, int y );
procedure cvMoveWindow(const name:pcvchar; x:int; y:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7855
{$ELSE}
name 'cvMoveWindow'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(* destroy window and all the trackers associated with it *)
//CVAPI(void) cvDestroyWindow( const char* name );
procedure cvDestroyWindow(const name:pcvchar);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7721
{$ELSE}
name 'cvDestroyWindow'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvDestroyAllWindows(void);
procedure cvDestroyAllWindows();
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7720
{$ELSE}
name 'cvDestroyAllWindows'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* get native window handle (HWND in case of Win32 and Widget in case of X Window) *)
//CVAPI(void*) cvGetWindowHandle( const char* name );
function cvGetWindowHandle(const name:pcvchar):pointer;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7799
{$ELSE}
name 'cvGetWindowHandle'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* get name of highgui window given its native handle *)
//CVAPI(const char*) cvGetWindowName( void* window_handle );
function cvGetWindowName(window_handle:pvoid):pcvchar;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7800
{$ELSE}
name 'cvGetWindowName'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


type
//void (CV_CDECL *CvTrackbarCallback)(int pos);
TCvTrackbarCallback = procedure(pos:int);

(* create trackbar and display it on top of given window, set callback *)
//CVAPI(int) cvCreateTrackbar( const char* trackbar_name, const char* window_name,
//                             int* value, int count, CvTrackbarCallback on_change CV_DEFAULT(NULL));
function cvCreateTrackbar(const trackbar_name:pcvchar; const window_name:pcvchar; value:pint; count:int; on_change:TCvTrackbarCallback= nil):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7712
{$ELSE}
name 'cvCreateTrackbar'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

type
//void (CV_CDECL *CvTrackbarCallback2)(int pos, void* userdata);
TCvTrackbarCallback2 = procedure(pos:int;userdata:pointer);

//CVAPI(int) cvCreateTrackbar2( const char* trackbar_name, const char* window_name,
//                              int* value, int count, CvTrackbarCallback2 on_change,
//                              void* userdata CV_DEFAULT(0));
function cvCreateTrackbar2(const trackbar_name:pcvchar; const window_name:pcvchar; value:pint;
         count:int; on_change:TCvTrackbarCallback2; userdata:pvoid=0):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7713
{$ELSE}
name 'cvCreateTrackbar2'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* retrieve or set trackbar position *)
//CVAPI(int) cvGetTrackbarPos( const char* trackbar_name, const char* window_name );
function cvGetTrackbarPos(const trackbar_name:pcvchar; const window_name:pcvchar):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7798
{$ELSE}
name 'cvGetTrackbarPos'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvSetTrackbarPos( const char* trackbar_name, const char* window_name, int pos );
procedure cvSetTrackbarPos(const trackbar_name:pcvchar; const window_name:pcvchar; pos:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7974
{$ELSE}
name 'cvSetTrackbarPos'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvSetTrackbarMax(const char* trackbar_name, const char* window_name, int maxval);
procedure cvSetTrackbarMax(const trackbar_name:pcvchar; const window_name:pcvchar; maxval:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7972
{$ELSE}
name 'cvSetTrackbarMax'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvSetTrackbarMin(const char* trackbar_name, const char* window_name, int minval);
procedure cvSetTrackbarMin(const trackbar_name:pcvchar; const window_name:pcvchar; minval:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7973
{$ELSE}
name 'cvSetTrackbarMin'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

const
//enum
//{
    CV_EVENT_MOUSEMOVE      =0;
    CV_EVENT_LBUTTONDOWN    =1;
    CV_EVENT_RBUTTONDOWN    =2;
    CV_EVENT_MBUTTONDOWN    =3;
    CV_EVENT_LBUTTONUP      =4;
    CV_EVENT_RBUTTONUP      =5;
    CV_EVENT_MBUTTONUP      =6;
    CV_EVENT_LBUTTONDBLCLK  =7;
    CV_EVENT_RBUTTONDBLCLK  =8;
    CV_EVENT_MBUTTONDBLCLK  =9;
    CV_EVENT_MOUSEWHEEL     =10;
    CV_EVENT_MOUSEHWHEEL    =11;
//};

//enum
//{
    CV_EVENT_FLAG_LBUTTON   =1;
    CV_EVENT_FLAG_RBUTTON   =2;
    CV_EVENT_FLAG_MBUTTON   =4;
    CV_EVENT_FLAG_CTRLKEY   =8;
    CV_EVENT_FLAG_SHIFTKEY  =16;
    CV_EVENT_FLAG_ALTKEY    =32;
//};

{TODO:CV_GET_WHEEL_DELTA}
//#define CV_GET_WHEEL_DELTA(flags) ((short)((flags >> 16) & $ffff)) // upper 16 bits

type
//void (CV_CDECL *CvMouseCallback )(int event, int x, int y, int flags, void* param);
TCvMouseCallback = procedure(event, x, y, flags:int; param:pointer);

(* assign callback for mouse events *)
//CVAPI(void) cvSetMouseCallback( const char* window_name, CvMouseCallback on_mouse,
//                                void* param CV_DEFAULT(NULL));
procedure cvSetMouseCallback(const window_name:pcvchar; on_mouse:TCvMouseCallback; param:pvoid= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7958
{$ELSE}
name 'cvSetMouseCallback'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* wait for key event infinitely (delay<=0) or for "delay" milliseconds *)
//CVAPI(int) cvWaitKey(int delay CV_DEFAULT(0));
function cvWaitKey(delay:int=0):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 8006
{$ELSE}
name 'cvWaitKey'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// OpenGL support

type
//void (CV_CDECL *CvOpenGlDrawCallback)(void* userdata);
TCvOpenGlDrawCallback = procedure(userdata:Pointer);

//CVAPI(void) cvSetOpenGlDrawCallback(const char* window_name, CvOpenGlDrawCallback callback, void* userdata CV_DEFAULT(NULL));
procedure cvSetOpenGlDrawCallback(const window_name:pcvchar; callback:TCvOpenGlDrawCallback; userdata:pvoid= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7962
{$ELSE}
name 'cvSetOpenGlDrawCallback'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvSetOpenGlContext(const char* window_name);
procedure cvSetOpenGlContext(const window_name:pcvchar);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7961
{$ELSE}
name 'cvSetOpenGlContext'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvUpdateWindow(const char* window_name);
procedure cvUpdateWindow(const window_name:pcvchar);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 8004
{$ELSE}
name 'cvUpdateWindow'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(****************************************************************************************\

*                              Obsolete functions/synonyms                               *
\****************************************************************************************)

{TODO:cvAddSearchPath}
//#define cvAddSearchPath(path)
//#define cvvInitSystem cvInitSystem
//#define cvvNamedWindow cvNamedWindow
//#define cvvShowImage cvShowImage
//#define cvvResizeWindow cvResizeWindow
//#define cvvDestroyWindow cvDestroyWindow
//#define cvvCreateTrackbar cvCreateTrackbar
//#define cvvAddSearchPath cvAddSearchPath
//#define cvvWaitKey(name) cvWaitKey(0)
//#define cvvWaitKeyEx(name,delay) cvWaitKey(delay)
//#define HG_AUTOSIZE CV_WINDOW_AUTOSIZE
//#define set_preprocess_func cvSetPreprocessFuncWin32
//#define set_postprocess_func cvSetPostprocessFuncWin32


//CVAPI(void) cvSetPreprocessFuncWin32_(const void* callback);
procedure cvSetPreprocessFuncWin32_(const callback:pvoid);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7964
{$ELSE}
name 'cvSetPreprocessFuncWin32_'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CVAPI(void) cvSetPostprocessFuncWin32_(const void* callback);
procedure cvSetPostprocessFuncWin32_(const callback:pvoid);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7963
{$ELSE}
name 'cvSetPostprocessFuncWin32_'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{TODO:cvSetPreprocessFuncWin32}
//#define cvSetPreprocessFuncWin32(callback) cvSetPreprocessFuncWin32_((const void*)(callback))
//#define cvSetPostprocessFuncWin32(callback) cvSetPostprocessFuncWin32_((const void*)(callback))

{$endif}
