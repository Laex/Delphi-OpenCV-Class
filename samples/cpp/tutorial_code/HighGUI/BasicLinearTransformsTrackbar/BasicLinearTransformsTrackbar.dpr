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
program BasicLinearTransformsTrackbar;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

(* * Global Variables *)
const
  alpha_max: int = 5;
  beta_max: int  = 125;

Var
  alpha: int; (* *< Simple contrast control *)
  beta: int;  (* *< Simple brightness control *)

  (* * Matrices to store images *)
  image: TMat;

  (* *
    * @function on_trackbar
    * @brief Called whenever any of alpha or beta changes
  *)
procedure on_trackbar(a: int; p: pointer);
begin
  Var
    new_image: TMat := TMat.zeros(image.size, image.&type);
    for Var y: int := 0 to image.rows - 1 do   //
      for Var x: int := 0 to image.cols - 1 do //
      for Var c: int := 0 to 2 do              //
      pVec3b(new_image.pt<TVec3b>(y, x))^[c] := saturate_cast<uchar>.cast(alpha * (pVec3b(image.pt<TVec3b>(y, x))^[c]) + beta);
  imshow('New Image', new_image);
end;

begin
  try
    /// Read image given by user
    Var
      imageName: String := OpenCVData + 'lena.jpg'; // by default
    if (argc > 1) then
      imageName := argv[1];
    image := imread(imageName);

    /// Initialize values
    alpha := 1;
    beta := 0;

    /// Create Windows
    namedWindow('Original Image', 1);
    namedWindow('New Image', 1);

    /// Create Trackbars
    createTrackbar('Contrast', 'New Image', @alpha, alpha_max, on_trackbar);
    createTrackbar('Brightness', 'New Image', @beta, beta_max, on_trackbar);

    /// Show some stuff
    imshow('Original Image', image);
    imshow('New Image', image);

    /// Wait until user press some key
    waitKey();
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
