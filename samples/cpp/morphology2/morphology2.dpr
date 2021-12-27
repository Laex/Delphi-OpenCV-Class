  (*
    This file is part of Delphi-OpenCV-Class project.
    https://github.com/Laex/Delphi-OpenCV-Class

    It is subject to the license terms in the LICENSE file found in the top-level directory
    of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

    Copyright 2021, Laentir Valetov, laex@bk.ru

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
  *)
program morphology2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help;
begin
  printf( //
    '\nShow off image morphology: erosion, dialation, open and close\n' + //
    'Call:\n   ' + argv[0] + ' [image]\n' + //
    'This program also shows use of rect, ellipse and cross kernels\n\n');
  printf(                                            //
    'Hot keys: \n' +                                 //
    '\tESC - quit the program\n' +                   //
    '\tr - use rectangle structuring element\n' +    //
    '\te - use elliptic structuring element\n' +     //
    '\tc - use cross-shaped structuring element\n' + //
    '\tSPACE - loop through all the options\n');
end;

Var
  src, dst: TMat;

  element_shape: MorphShapes = MORPH_RECT;

    // the address of variable which receives trackbar position update
  max_iters: int        = 10;
  open_close_pos: int   = 0;
  erode_dilate_pos: int = 0;

procedure OpenClose(a: int; p: Pointer);
begin
  Var
    n: int := open_close_pos;
  Var
    an: int := abs(n);
  Var
    element: TMat := getStructuringElement(element_shape, Size(an * 2 + 1, an * 2 + 1), Point(an, an));
  if (n < 0) then
    morphologyEx(src, dst, MORPH_OPEN, element)
  else
    morphologyEx(src, dst, MORPH_CLOSE, element);
  imshow('Open/Close', dst);
end;

procedure ErodeDilate(a: int; p: Pointer);
begin
  Var
    n: int := erode_dilate_pos;
  Var
    an: int := abs(n);
  Var
    element: TMat := getStructuringElement(element_shape, Size(an * 2 + 1, an * 2 + 1), Point(an, an));
  if (n < 0) then
    erode(src, dst, element)
  else
    dilate(src, dst, element);
  imshow('Erode/Dilate', dst);
end;

begin
  try
    help;

    var
      filename: string;

    if (ParamCount > 0) and (FileExists(ParamStr(1))) then
      filename := ParamStr(1)
    else
      filename := OpenCVData + 'baboon.jpg';

    src := imread(filename, IMREAD_COLOR);

    if (src.empty()) then
      Halt(1);

      // create windows for output images
    namedWindow('Open/Close', 1);
    namedWindow('Erode/Dilate', 1);

    open_close_pos := max_iters;
    erode_dilate_pos := max_iters;

    createTrackbar('iterations', 'Open/Close', @open_close_pos, max_iters * 2 + 1, OpenClose);
    setTrackbarMin('iterations', 'Open/Close', -max_iters);
    setTrackbarMax('iterations', 'Open/Close', max_iters);
    setTrackbarPos('iterations', 'Open/Close', 0);

    createTrackbar('iterations', 'Erode/Dilate', @erode_dilate_pos, max_iters * 2 + 1, ErodeDilate);
    setTrackbarMin('iterations', 'Erode/Dilate', -max_iters);
    setTrackbarMax('iterations', 'Erode/Dilate', max_iters);
    setTrackbarPos('iterations', 'Erode/Dilate', 0);

    While True do
    begin
      OpenClose(open_close_pos, nil);
      ErodeDilate(erode_dilate_pos, nil);
      Var
        c: char := chr(waitKey(0));

      if (c = #27) then
        break;
      if (c = 'e') then
        element_shape := MORPH_ELLIPSE
      else if (c = 'r') then
        element_shape := MORPH_RECT
      else if (c = 'c') then
        element_shape := MORPH_CROSS
      else if (c = ' ') then
        element_shape := MorphShapes((int(element_shape) + 1) mod 3);
    end;

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
