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
program morph_lines_detection;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

const
  EXIT_FAILURE = 1;

procedure show_wait_destroy(const winname: String; img: TMat);
begin
  imshow(winname, img);
  moveWindow(winname, 500, 0);
  waitKey(0);
  destroyWindow(winname);
end;

Var
  fn: String;
  kernel: TMat;
  edges: TMat;

begin
  try
    // ! [load_image]
    // CommandLineParser parser(argc, argv, 'begin@input | notes.png | input imageend;');

    if ParamCount > 0 then
      fn := ParamStr(1)
    else
      fn := OpenCVData + 'notes.png';

    var
      src: TMat := imread(fn, IMREAD_COLOR);

    if (src.empty()) then
    begin
      WriteLn('Could not open or find the image!');
      WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' <Input image>');
      Halt(1);
    end;

    // Show source image
    imshow('src', src);
    // ! [load_image]

    // ! [gray]
    // Transform source image to gray if it is not already
    Var
      gray: TMat;

    if (src.channels() = 3) then
      cvtColor(src, gray, COLOR_BGR2GRAY)
    else
      gray := src;

    // Show gray image
    show_wait_destroy('gray', gray);
    // ! [gray]

    // ! [bin]
    // Apply adaptiveThreshold at the bitwise_not of gray, notice the ~ symbol
    Var
      bw: TMat;
    adaptiveThreshold(not gray, bw, 255, ADAPTIVE_THRESH_MEAN_C, THRESH_BINARY, 15, -2);

    // Show binary image
    show_wait_destroy('binary', bw);
    // ! [bin]

    // ! [init]
    // Create the images that will use to extract the horizontal and vertical lines
    Var
      horizontal: TMat := bw.clone();
    Var
      vertical: TMat := bw.clone();
      // ! [init]

      // ! [horiz]
      // Specify size on horizontal axis
    Var
      horizontal_size: integer := horizontal.cols div 30;

      // Create structure element for extracting horizontal lines through morphology operations
    Var
      horizontalStructure: TMat := getStructuringElement(MORPH_RECT, Size(horizontal_size, 1));

      // Apply morphology operations
    erode(horizontal, horizontal, horizontalStructure, Point(-1, -1));
    dilate(horizontal, horizontal, horizontalStructure, Point(-1, -1));

    // Show extracted horizontal lines
    show_wait_destroy('horizontal', horizontal);
    // ! [horiz]

    // ! [vert]
    // Specify size on vertical axis
    Var
      vertical_size: integer := vertical.rows div 30;

      // Create structure element for extracting vertical lines through morphology operations
    Var
      verticalStructure: TMat := getStructuringElement(MORPH_RECT, Size(1, vertical_size));

      // Apply morphology operations
    erode(vertical, vertical, verticalStructure, Point(-1, -1));
    dilate(vertical, vertical, verticalStructure, Point(-1, -1));

    // Show extracted vertical lines
    show_wait_destroy('vertical', vertical);
    // ! [vert]

    // ! [smooth]
    // Inverse vertical image
    bitwise_not(vertical, vertical);
    show_wait_destroy('vertical_bit', vertical);

    // Extract edges and smooth image according to the logic
    // 1. extract edges
    // 2. dilate(edges)
    // 3. src.copyTo(smooth)
    // 4. blur smooth img
    // 5. smooth.copyTo(src, edges)

    // Step 1
    { Var
      edges: TMat; }

    adaptiveThreshold(vertical, edges, 255, ADAPTIVE_THRESH_MEAN_C, THRESH_BINARY, 3, -2);
    show_wait_destroy('edges', edges);

    // Step 2
    { Var }
    kernel { : TMat } := TMat.ones(2, 2, CV_8UC1);

    dilate(edges, edges, kernel);
    show_wait_destroy('dilate', edges);

    // Step 3
    Var
      smooth: TMat;
    vertical.copyTo(smooth);

    // Step 4
    blur(smooth, smooth, Size(2, 2));

    // Step 5
    smooth.copyTo(vertical, edges);

    // Show final result
    show_wait_destroy('smooth - final', vertical);
    // ! [smooth]
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
