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
program Smoothing;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cv.resource,
  cv.opencv;

const
  EXIT_FAILURE = 1;

Var
  /// Global Variables
  DELAY_CAPTION: Integer = 1500;
  DELAY_BLUR: Integer = 100;
  MAX_KERNEL_LENGTH: Integer = 31;
  src, dst: TMat;
  window_name: string = 'Smoothing Demo';
  filename: String;

function display_dst(delay: Integer): Integer;
Var
  c: Integer;
begin
  imshow(window_name, dst);
  c := waitKey(delay);
  if (c >= 0) then
    Exit(-1);
  Result := 0;
end;

function display_caption(const caption: String): Integer;
begin
  dst := TMat.zeros(src.size, src.&type);
  putText(dst, caption, Point(src.cols div 4, src.rows div 2), FONT_HERSHEY_COMPLEX, 1, Scalar(255, 255, 255));
  Result := display_dst(DELAY_CAPTION);
end;

begin
  try

    namedWindow(window_name, WINDOW_AUTOSIZE);

    // Load the source image
    if ParamCount > 0 then
      filename := ParamStr(1)
    else
      filename := OpenCVData + 'lena.jpg';

    src := imread(filename, IMREAD_COLOR);
    if (src.empty()) then
    begin
      WriteLn(' Error opening image');
      WriteLn(' Usage:');
      WriteLn(' ', ExtractFileName(ParamStr(0)), ' [image_name-- default ' + OpenCVData + 'lena.jpg]');
      Halt(EXIT_FAILURE);
    end;

    if (display_caption('Original Image') <> 0) then
      Halt(0);

    dst := src.clone();
    if (display_dst(DELAY_CAPTION) <> 0) then
      Halt(0);

    /// Applying Homogeneous blur
    if (display_caption('Homogeneous Blur') <> 0) then
      Halt(0);

    // ![blur]
    Var
    i := 1;

    While i < MAX_KERNEL_LENGTH do
    begin
      blur(src, dst, size(i, i), Point(-1, -1));
      if (display_dst(DELAY_BLUR) <> 0) then
        Halt(0);
      i := i + 2;
    end;
    // ![blur]

    /// Applying Gaussian blur
    if (display_caption('Gaussian Blur') <> 0) then
      Halt(0);

    // ![gaussianblur]
    i := 1;
    While i < MAX_KERNEL_LENGTH do
    begin
      GaussianBlur(src, dst, size(i, i), 0, 0);
      if (display_dst(DELAY_BLUR) <> 0) then
        Halt(0);
      i := i + 2;
    end;
    // ![gaussianblur]

    /// Applying Median blur
    if (display_caption('Median Blur') <> 0) then
      Halt(0);

    // ![medianblur]
    i := 1;
    While i < MAX_KERNEL_LENGTH do
    begin
      medianBlur(src, dst, i);
      if (display_dst(DELAY_BLUR) <> 0) then
        Halt(0);
      i := i + 2;
    end;
    // ![medianblur]

    /// Applying Bilateral Filter
    if (display_caption('Bilateral Blur') <> 0) then
      Halt(0);

    // ![bilateralfilter]
    i := 1;
    While i < MAX_KERNEL_LENGTH do
    begin
      bilateralFilter(src, dst, i, i * 2, i / 2);
      if (display_dst(DELAY_BLUR) <> 0) then
        Halt(0);
      i := i + 2;
    end;
    // ![bilateralfilter]

    /// Done
    display_caption('Done!');

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
