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
program CannyDetector_Demo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cv.resource,
  cpp.utils,
  cv.opencv;

Var
  // ![variables]
  src, src_gray: TMat;
  dst, detected_edges: TMat;

  lowThreshold: int = 0;

const
  max_lowThreshold: int = 100;
  ratio: int            = 3;
  kernel_size: int      = 3;
  window_name           = 'Edge Map';
  // ![variables]

procedure CannyThreshold(a: int; p: pointer);
begin
  // ![reduce_noise]
  /// Reduce noise with a kernel 3x3
  blur(src_gray, detected_edges, Size(3, 3));
  // ![reduce_noise]

  // ![canny]
  /// Canny detector
  Canny(detected_edges, detected_edges, lowThreshold, lowThreshold * ratio, kernel_size);
  // ![canny]

  /// Using Canny's output as a mask, we display our result
  // ![fill]
  dst.assign(TScalar.all(0));
  // ![fill]

  // ![copyto]
  src.copyTo(dst, detected_edges);
  // ![copyto]

  // ![display]
  imshow(window_name, dst);
  // ![display]
end;

begin
  try
    // ![load]
    Var
      filename: string := OpenCVData + 'fruits.jpg';
    if (ParamCount > 0) and FileExists(ParamStr(1)) then
      filename := ParamStr(1);
    src := imread(filename, IMREAD_COLOR); // Load an image

    if (src.empty()) then
    begin
      cout + 'Could not open or find the image!\n' + endl;
      cout + 'Usage: ' + argv[0] + ' <Input image>' + endl;
      Halt(1);
    end;
    // ![load]

    // ![create_mat]
    /// Create a matrix of the same type and size as src (for dst)
    dst.create(src.Size, src.&type);
    // ![create_mat]

    // ![convert_to_gray]
    cvtColor(src, src_gray, COLOR_BGR2GRAY);
    // ![convert_to_gray]

    // ![create_window]
    namedWindow(window_name, WINDOW_AUTOSIZE);
    // ![create_window]

    // ![create_trackbar]
    /// Create a Trackbar for user to enter threshold
    createTrackbar('Min Threshold:', window_name, @lowThreshold, max_lowThreshold, CannyThreshold);
    // ![create_trackbar]

    /// Show the image
    CannyThreshold(0, nil);

    /// Wait until user exit program by pressing a key
    waitKey(0);

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
