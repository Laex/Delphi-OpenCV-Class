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

{$IF defined(OPENCV_HIGHGUI_HPP) and (not defined(OPENCV_HIGHGUI_HPP_IMPL))}
{$DEFINE OPENCV_HIGHGUI_HPP_IMPL}

function createTrackbar(const trackbarname: String; const winname: String; Value: pInt; COUNT: Int; onChange: TTrackbarCallback = nil; userdata: Pointer = nil): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_createTrackbar(CppString(trackbarname), CppString(winname), Value, COUNT, onChange, userdata);
{$IFEND}
end;

procedure namedWindow(const winname: String; flags: WindowFlags = WINDOW_AUTOSIZE);
begin
{$IF not defined(PACKAGE)}
  proc_namedWindow(CppString(winname), flags);
{$IFEND}
end;

procedure destroyWindow(const winname: String);
begin
{$IF not defined(PACKAGE)}
  proc_destroyWindow(CppString(winname));
{$IFEND}
end;

procedure destroyAllWindows();
begin
{$IF not defined(PACKAGE)}
  proc_destroyAllWindows();
{$IFEND}
end;

function startWindowThread: Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_startWindowThread;
{$IFEND}
end;

function waitKeyEx(delay: Int = 0): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_waitKeyEx(delay);
{$IFEND}
end;

function waitKey(delay: Int = 0): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_waitKey(delay);
{$IFEND}
end;

function pollKey(): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_pollKey();
{$IFEND}
end;

procedure imshow(const winname: String; const Mat: TInputArray);
begin
{$IF not defined(PACKAGE)}
  proc_imshow_1(CppString(winname), Mat);
{$IFEND}
end;

procedure resizeWindow(const winname: String; width, height: Int);
begin
{$IF not defined(PACKAGE)}
  proc_resizeWindow_1(CppString(winname), width, height);
{$IFEND}
end;

procedure resizeWindow(const winname: String; const size:TSize);
begin
{$IF not defined(PACKAGE)}
  proc_resizeWindow(CppString(winname), size);
{$IFEND}
end;

procedure moveWindow(const winname: String; x, y: Int);
begin
{$IF not defined(PACKAGE)}
  proc_moveWindow(CppString(winname), x, y);
{$IFEND}
end;

function getWindowProperty(const winname: String; prop_id: Int): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_getWindowProperty(CppString(winname), prop_id);
{$IFEND}
end;

function getWindowImageRect(const winname: String): TRect;
begin
{$IF not defined(PACKAGE)}
  Result := func_getWindowImageRect(CppString(winname));
{$IFEND}
end;

procedure setMouseCallback(const winname: String; onMouse: TMouseCallback; userdata: Pointer = nil);
begin
{$IF not defined(PACKAGE)}
  _cvSetMouseCallback(pCvChar(AnsiString(winname)), onMouse, userdata);
{$IFEND}
end;

function getMouseWheelDelta(flags: Int): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getMouseWheelDelta(flags);
{$IFEND}
end;

function selectROI(const windowName: String; const img: TInputArray; showCrosshair: bool = true; fromCenter: bool = false): TRect;
begin
{$IF not defined(PACKAGE)}
  Result := func_selectROI(CppString(windowName), img, showCrosshair, fromCenter);
{$IFEND}
end;

function selectROI(const img: TInputArray; showCrosshair: bool = true; fromCenter: bool = false): TRect;
begin
{$IF not defined(PACKAGE)}
  Result := func_selectROI_1(img, showCrosshair, fromCenter);
{$IFEND}
end;

procedure selectROIs(const windowName: String; const img: TInputArray; const boundingBoxes: vector<TRect>; showCrosshair: bool = true; fromCenter: bool = false);
begin
{$IF not defined(PACKAGE)}
  proc_selectROIs(CppString(windowName), img, boundingBoxes, showCrosshair, fromCenter);
{$IFEND}
end;

function getTrackbarPos(const trackbarname: String; const winname: String): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getTrackbarPos(CppString(trackbarname), CppString(winname));
{$IFEND}
end;

procedure setTrackbarPos(const trackbarname: String; const winname: String; pos: Int);
begin
{$IF not defined(PACKAGE)}
  proc_setTrackbarPos(CppString(trackbarname), CppString(winname), pos);
{$IFEND}
end;

procedure setTrackbarMax(const trackbarname: String; const winname: String; maxval: Int);
begin
{$IF not defined(PACKAGE)}
  proc_setTrackbarMax(CppString(trackbarname), CppString(winname), maxval);
{$IFEND}
end;

procedure setTrackbarMin(const trackbarname: String; const winname: String; minval: Int);
begin
{$IF not defined(PACKAGE)}
  proc_setTrackbarMin(CppString(trackbarname), CppString(winname), minval);
{$IFEND}
end;

procedure setOpenGlDrawCallback(const winname: String; const onOpenGlDraw: TOpenGlDrawCallback; userdata: Pointer = nil);
begin
{$IF not defined(PACKAGE)}
  proc_setOpenGlDrawCallback(CppString(winname), onOpenGlDraw, userdata);
{$IFEND}
end;

procedure setOpenGlContext(const winname: String);
begin
{$IF not defined(PACKAGE)}
  proc_setOpenGlContext(CppString(winname));
{$IFEND}
end;

procedure updateWindow(const winname: String);
begin
{$IF not defined(PACKAGE)}
  proc_updateWindow(CppString(winname));
{$IFEND}
end;

procedure addText(const img: TMat; const text: String; const org: TPoint; const nameFont: String; pointSize: Int { = -1 }; const color: TScalar { = Scalar::all(0) }; weight: Int = Int(QT_FONT_NORMAL); style: Int = Int(QT_STYLE_NORMAL); spacing: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_addText(img, CppString(text), org, CppString(nameFont), pointSize, color, weight, style, spacing);
{$IFEND}
end;

procedure displayOverlay(const winname: String; const text: String; delayms: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_displayOverlay(CppString(winname), CppString(text), delayms);
{$IFEND}
end;

procedure displayStatusBar(const winname: String; const text: String; delayms: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_displayStatusBar(CppString(winname), CppString(text), delayms);
{$IFEND}
end;

procedure saveWindowParameters(const windowName: String);
begin
{$IF not defined(PACKAGE)}
  proc_saveWindowParameters(CppString(windowName));
{$IFEND}
end;

procedure loadWindowParameters(const windowName: String);
begin
{$IF not defined(PACKAGE)}
  proc_loadWindowParameters(CppString(windowName));
{$IFEND}
end;

function startLoop(pt2Func: Tpt2Func; argc: Int; argv: Targv): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_startLoop(pt2Func, argc, argv);
{$IFEND}
end;

procedure stopLoop();
begin
{$IF not defined(PACKAGE)}
  proc_stopLoop();
{$IFEND}
end;

function createButton(const bar_name: String; on_change: TButtonCallback; userdata: Pointer = nil; &type: Int = Int(QT_PUSH_BUTTON); initial_button_state: bool = false): Int;
begin
{$IF not defined(PACKAGE)}
  Result := func_createButton(CppString(bar_name), on_change, userdata, &type, initial_button_state);
{$IFEND}
end;

procedure setWindowProperty(const winname:String; prop_id:int; prop_value:double);
begin
{$IF not defined(PACKAGE)}
  proc_setWindowProperty(CppString(winname),prop_id,prop_value);
{$IFEND}
end;

procedure setWindowTitle(const winname: String; const title: String);
begin
{$IF not defined(PACKAGE)}
  proc_setWindowTitle(CppString(winname), CppString(title));
{$IFEND}
end;


{$I 'highgui/highgui_c.impl.inc'}

{$IFEND}
