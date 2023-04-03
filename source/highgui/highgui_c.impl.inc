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

{$IF defined(OPENCV_HIGHGUI_H) and (not defined(OPENCV_HIGHGUI_H_IMPL))}
{$DEFINE OPENCV_HIGHGUI_H_IMPL}

function cvInitSystem(argc: int; argv: pcvchar): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInitSystem(argc, argv);
{$IFEND}
end;

function cvStartWindowThread(): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvStartWindowThread;
{$IFEND}
end;

function cvNamedWindow(const name: pcvchar; flags: int = CV_WINDOW_AUTOSIZE): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvNamedWindow(name, flags);
{$IFEND}
end;

procedure cvSetWindowProperty(const name: pcvchar; prop_id: int; prop_value: double);
begin
{$IF not defined(PACKAGE)}
  _cvSetWindowProperty(name, prop_id, prop_value);
{$IFEND}
end;

function cvGetWindowProperty(const name: pcvchar; prop_id: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetWindowProperty(name, prop_id);
{$IFEND}
end;

procedure cvShowImage(const name: pcvchar; const image: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvShowImage(name, image);
{$IFEND}
end;

procedure cvResizeWindow(const name: pcvchar; width: int; height: int);
begin
{$IF not defined(PACKAGE)}
  _cvResizeWindow(name, width, height);
{$IFEND}
end;

procedure cvMoveWindow(const name: pcvchar; x: int; y: int);
begin
{$IF not defined(PACKAGE)}
  _cvMoveWindow(name, x, y);
{$IFEND}
end;

procedure cvDestroyWindow(const name: pcvchar);
begin
{$IF not defined(PACKAGE)}
  _cvDestroyWindow(name);
{$IFEND}
end;

procedure cvDestroyAllWindows();
begin
{$IF not defined(PACKAGE)}
  _cvDestroyAllWindows();
{$IFEND}
end;

function cvGetWindowHandle(const name: pcvchar): pointer;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetWindowHandle(name);
{$IFEND}
end;

function cvGetWindowName(window_handle: pvoid): pcvchar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetWindowName(window_handle);
{$IFEND}
end;

function cvCreateTrackbar(const trackbar_name: pcvchar; const window_name: pcvchar; value: pint; count: int; on_change: TCvTrackbarCallback = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateTrackbar(trackbar_name, window_name, value, count, on_change);
{$IFEND}
end;

function cvCreateTrackbar2(const trackbar_name: pcvchar; const window_name: pcvchar; value: pint; count: int; on_change: TCvTrackbarCallback2; userdata: pvoid = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateTrackbar2(trackbar_name, window_name, value, count, on_change, userdata);
{$IFEND}
end;

function cvGetTrackbarPos(const trackbar_name: pcvchar; const window_name: pcvchar): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetTrackbarPos(trackbar_name, window_name);
{$IFEND}
end;

procedure cvSetTrackbarPos(const trackbar_name: pcvchar; const window_name: pcvchar; pos: int);
begin
{$IF not defined(PACKAGE)}
  _cvSetTrackbarPos(trackbar_name, window_name, pos);
{$IFEND}
end;

procedure cvSetTrackbarMax(const trackbar_name: pcvchar; const window_name: pcvchar; maxval: int);
begin
{$IF not defined(PACKAGE)}
  _cvSetTrackbarMax(trackbar_name, window_name, maxval);
{$IFEND}
end;

procedure cvSetTrackbarMin(const trackbar_name: pcvchar; const window_name: pcvchar; minval: int);
begin
{$IF not defined(PACKAGE)}
  _cvSetTrackbarMin(trackbar_name, window_name, minval);
{$IFEND}
end;

procedure cvSetMouseCallback(const window_name: pcvchar; on_mouse: TCvMouseCallback; param: pvoid = nil);
begin
{$IF not defined(PACKAGE)}
  _cvSetMouseCallback(window_name, on_mouse, param);
{$IFEND}
end;

function cvWaitKey(delay: int = 0): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvWaitKey(delay);
{$IFEND}
end;

procedure cvSetOpenGlDrawCallback(const window_name: pcvchar; callback: TCvOpenGlDrawCallback; userdata: pvoid = nil);
begin
{$IF not defined(PACKAGE)}
  _cvSetOpenGlDrawCallback(window_name, callback, userdata);
{$IFEND}
end;

procedure cvSetOpenGlContext(const window_name:pcvchar);
begin
{$IF not defined(PACKAGE)}
  _cvSetOpenGlContext(window_name);
{$IFEND}
end;

procedure cvUpdateWindow(const window_name:pcvchar);
begin
{$IF not defined(PACKAGE)}
  _cvUpdateWindow(window_name);
{$IFEND}
end;

procedure cvSetPreprocessFuncWin32_(const callback:pvoid);
begin
{$IF not defined(PACKAGE)}
  _cvSetPreprocessFuncWin32_(callback);
{$IFEND}
end;

procedure cvSetPostprocessFuncWin32_(const callback:pvoid);
begin
{$IF not defined(PACKAGE)}
  _cvSetPostprocessFuncWin32_(callback);
{$IFEND}
end;

{$INCLUDE 'core/core_c.impl.inc'}
{$INCLUDE 'imgproc/imgproc_c.impl.inc'}
{$IFEND}
