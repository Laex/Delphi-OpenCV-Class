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
program edge;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

Var
  edgeThresh: int = 1;
  edgeThreshScharr: int = 1;

  image, gray, blurImage, edge1, edge2, cedge: TMat;

  window_name1: cvStdString;
  window_name2: cvStdString;

  // define a trackbar callback
procedure onTrackbar(a: int; p: pointer);
begin
  blur(gray, blurImage, Size(3, 3));

  // Run the edge detector on grayscale
  Canny(blurImage, edge1, edgeThresh, edgeThresh * 3, 3);
  cedge := TScalar.all(0);

  image.copyTo(cedge, edge1);
  imshow(window_name1, cedge);

  /// Canny detector with scharr
  Var
    dx, dy: TMat;
  Scharr(blurImage, dx, CV_16S, 1, 0);
  Scharr(blurImage, dy, CV_16S, 0, 1);
  Canny(dx, dy, edge2, edgeThreshScharr, edgeThreshScharr * 3);
  /// Using Canny's output as a mask, we display our result
  cedge := TScalar.all(0);
  image.copyTo(cedge, edge2);
  imshow(window_name2, cedge);
end;

procedure help;
begin
  WriteLn('This sample demonstrates Canny edge detection'#13#10 + 'Call:'#13#10 + ExtractFileName(ParamStr(0)) + 'image_name -- Default is fruits.jpg');
end;

begin
  try
    window_name1 := 'Edge map : Canny default (Sobel gradient)';
    window_name2 := 'Edge map : Canny with custom gradient (Scharr)';

    help;

    Var
      filename: cvStdString;
    if ParamCount > 0 then
      filename := ParamStr(1)
    else
      filename := OpenCVData + 'fruits.jpg';

    image := imread(filename, IMREAD_COLOR);
    if (image.empty()) then
    begin
      WriteLn('Cannot read image file: ', string(filename));
      help;
      Halt(1);
    end;
    cedge.create(image.Size, image.&type);
    cvtColor(image, gray, COLOR_BGR2GRAY);

    // Create a window
    namedWindow(window_name1, WINDOW_AUTOSIZE);
    namedWindow(window_name2, WINDOW_AUTOSIZE);

    // create a toolbar
    createTrackbar('Canny threshold default', window_name1, @edgeThresh, 100, onTrackbar);
    createTrackbar('Canny threshold Scharr', window_name2, @edgeThreshScharr, 400, onTrackbar);

    // Show the image
    onTrackbar(0, nil);

    // Wait for a key stroke; the same function arranges events processing
    waitKey(0);

  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
