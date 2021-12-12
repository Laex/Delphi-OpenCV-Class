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
program cvdecolor;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

begin
  try
    // CommandLineParser parser( argc, argv, '{@input | HappyFish.jpg | input image}' );
    Var
      src: TMat;
    if ParamCount = 0 then
      src := imread(OpenCVData + 'HappyFish.jpg', IMREAD_COLOR)
    else
      src := imread(ParamStr(1), IMREAD_COLOR);
    if (src.empty()) then
    begin
      Writeln('Could not open or find the image!');
      Writeln('Usage: ', ParamStr(0), ' <Input image>');
      Halt(1);
    end;

    Var
      gray, color_boost: TMat;

    decolor(src, gray, color_boost);
    imshow('Source Image', src);
    imshow('grayscale', gray);
    imshow('color_boost', color_boost);
    waitKey(0);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
