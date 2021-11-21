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
program cornerDetector_Demo;

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
  myHarris_dst, myHarris_copy, Mc: TMat;
  myShiTomasi_dst, myShiTomasi_copy: TMat;

  myShiTomasi_qualityLevel: int = 50;
  myHarris_qualityLevel: int = 50;
  max_qualityLevel: int = 100;

  myHarris_minVal, myHarris_maxVal: double;
  myShiTomasi_minVal, myShiTomasi_maxVal: double;

  rng: TRNG;

const
  myHarris_window    = 'My Harris corner detector';
  myShiTomasi_window = 'My Shi Tomasi corner detector';

  (* *
    * @function myShiTomasi_function
  *)
procedure myShiTomasi_function(a: int; p: pointer);
begin
  myShiTomasi_copy := src.clone();
  myShiTomasi_qualityLevel := MAX(myShiTomasi_qualityLevel, 1);

  for Var i: int := 0 to src_gray.rows - 1 do
    for Var j: int := 0 to src_gray.cols - 1 do
      if (myShiTomasi_dst.at<float>(i, j) > myShiTomasi_minVal + (myShiTomasi_maxVal - myShiTomasi_minVal) * myShiTomasi_qualityLevel / max_qualityLevel) then
        circle(myShiTomasi_copy, Point(j, i), 4, Scalar(rng.uniform(0, 256), rng.uniform(0, 256), rng.uniform(0, 256)), int(FILLED));
  imshow(myShiTomasi_window, myShiTomasi_copy);
end;

(* *
  * @function myHarris_function
*)
procedure myHarris_function(a: int; p: pointer);
begin
  myHarris_copy := src.clone();
  myHarris_qualityLevel := MAX(myHarris_qualityLevel, 1);

  for Var i: int := 0 to src_gray.rows - 1 do
    for Var j: int := 0 to src_gray.cols - 1 do
      if (Mc.at<float>(i, j) > myHarris_minVal + (myHarris_maxVal - myHarris_minVal) * myHarris_qualityLevel / max_qualityLevel) then
        circle(myHarris_copy, Point(j, i), 4, Scalar(rng.uniform(0, 256), rng.uniform(0, 256), rng.uniform(0, 256)), int(FILLED));
  imshow(myHarris_window, myHarris_copy);
end;

begin
  try
    rng := 12345;

    /// Load source image and convert it to gray
    (*
      Var
      parser: TCommandLineParser := '{@input | ' + OpenCVData + 'building.jpg | input image}';
      src := imread(parser.get<String>('@input'));
    *)
    if ParamCount > 0 then
      src := imread(ParamStr(1))
    else
      src := imread(OpenCVData + 'building.jpg');

    if (src.empty()) then
    begin
      WriteLn('Could not open or find the image!\n');
      WriteLn('Usage: ', ExtractFileName(ParamStr(0)), ' <Input image>');
      Halt(1);
    end;
    cvtColor(src, src_gray, COLOR_BGR2GRAY);

    /// Set some parameters
    Var
      blockSize: int := 3;
    Var
      apertureSize: int := 3;

      /// My Harris matrix -- Using cornerEigenValsAndVecs
    cornerEigenValsAndVecs(src_gray, myHarris_dst, blockSize, apertureSize);

    (* calculate Mc *)
    Mc := TMat.Mat(src_gray.size, CV_32FC1);
    for Var i: int := 0 to src_gray.rows - 1 do
      for Var j: int := 0 to src_gray.cols - 1 do
      begin
        Var
          lambda_1: float := pVec6f(myHarris_dst.pt<Vec6f>(i, j))[0];
        Var
          lambda_2: float := pVec6f(myHarris_dst.pt<Vec6f>(i, j))[1];

          // Mc.st<float>(i, j, lambda_1 * lambda_2 - 0.04 * ((lambda_1 + lambda_2) * (lambda_1 + lambda_2)));
        pFloat(Mc.pt<float>(i, j))^ := { v; // } lambda_1 * lambda_2 - 0.04 * ((lambda_1 + lambda_2) * (lambda_1 + lambda_2));
      end;

    minMaxLoc(Mc, myHarris_minVal, myHarris_maxVal);

    (* Create Window and Trackbar *)
    namedWindow(myHarris_window);
    createTrackbar('Quality Level:', myHarris_window, @myHarris_qualityLevel, max_qualityLevel, myHarris_function);
    myHarris_function(0, nil);

    /// My Shi-Tomasi -- Using cornerMinEigenVal
    cornerMinEigenVal(src_gray, myShiTomasi_dst, blockSize, apertureSize);

    minMaxLoc(myShiTomasi_dst, myShiTomasi_minVal, myShiTomasi_maxVal);

    (* Create Window and Trackbar *)
    namedWindow(myShiTomasi_window);
    createTrackbar('Quality Level:', myShiTomasi_window, @myShiTomasi_qualityLevel, max_qualityLevel, myShiTomasi_function);
    myShiTomasi_function(0, nil);

    waitKey();

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
