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

{$IFDEF OPENCV_HIGHGUI_HPP}

procedure namedWindow(const winname: CppString; flags: WindowFlags);
begin
  namedWindow(winname, Int(flags));
end;

procedure namedWindow(const winname: String; flags: WindowFlags);
begin
  namedWindow(CppString(winname), Int(flags));
end;

procedure destroyWindow(const winname: String);
begin
  destroyWindow(CppString(winname));
end;

procedure imshow(const winname: String; const Mat: TInputArray);
begin
  imshow(CppString(winname), Mat);
end;

procedure resizeWindow(const winname: String; width, height: Int);
begin
  resizeWindow(CppString(winname), width, height);
end;

procedure moveWindow(const winname: String; x, y: Int);
begin
  moveWindow(CppString(winname), x, y);
end;

procedure setMouseCallback(const winname: String; onMouse: TMouseCallback; userdata: Pointer = nil);
begin
  setMouseCallback(CppString(winname), onMouse, userdata);
end;

function createTrackbar(const trackbarname: String; const winname: String; var Value: Int; COUNT: Int; onChange: TTrackbarCallback = nil; userdata: Pointer = nil): Int;
begin
  Result := createTrackbar(trackbarname, winname, @Value, COUNT, onChange, userdata);
end;

{$ENDIF}