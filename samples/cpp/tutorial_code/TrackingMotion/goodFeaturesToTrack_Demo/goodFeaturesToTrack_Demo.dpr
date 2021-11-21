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
program goodFeaturesToTrack_Demo;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

const
  EXIT_FAILURE = 1;

Var
  /// Global variables
  src, src_gray: TMat;

  maxCorners: int = 23;
  maxTrackbar: int = 100;

  rng: TRNG;

const
  source_window = 'Image';

  (* *
    * @function goodFeaturesToTrack_Demo.cpp
    * @brief Apply Shi-Tomasi corner detector
  *)
procedure goodFeaturesToTrackProc(a: int; p: Pointer);

Var
  pp: TOutputArray;

begin
  /// Parameters for Shi-Tomasi algorithm
  maxCorners := MAX(maxCorners, 1);
  Var
    corners: Vector<TPoint2f>;
  Var
    qualityLevel: double := 0.01;
  Var
    minDistance: double := 10;
  Var
    blockSize: int := 3;
  Var
    gradientSize: int := 3;
  Var
    useHarrisDetector: bool := false;
  Var
    k: double := 0.04;

    /// Copy the source image
  Var
    cpy: TMat := src.clone();

  /// Apply corner detection
  goodFeaturesToTrack(src_gray, corners, maxCorners, qualityLevel, minDistance, TMat.Mat, blockSize, gradientSize, useHarrisDetector, k);

  /// Draw corners detected
  WriteLn('** Number of corners detected: ', corners.size());
  Var
    radius: int := 4;
  for Var i := 0 to corners.size() - 1 do
    circle(cpy, corners[i], radius, Scalar(rng.uniform(0, 255), rng.uniform(0, 256), rng.uniform(0, 256)), int(FILLED));

  /// Show what you got
  namedWindow(source_window);
  imshow(source_window, cpy);
end;

begin
  try
    rng := 12345;

    /// Load source image and convert it to gray
    // CommandLineParser parser( argc, argv, "{@input | pic3.png | input image}" );
    // src = imread( samples::findFile( parser.get<String>( "@input" ) ) );
    if ParamCount > 0 then
      src := imread(ParamStr(1))
    else
      src := imread(OpenCVData + 'pic3.png');

    if (src.empty()) then
    begin
      WriteLn('Could not open or find the image!');
      WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <Input image>');
      Halt(1);
    end;
    cvtColor(src, src_gray, COLOR_BGR2GRAY);

    /// Create Window
    namedWindow(source_window);

    /// Create Trackbar to set the number of corners
    createTrackbar(' MAX corners: ', source_window, @maxCorners, maxTrackbar, goodFeaturesToTrackProc);

    imshow(source_window, src);

    goodFeaturesToTrackProc(0, nil);

    waitKey();

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
