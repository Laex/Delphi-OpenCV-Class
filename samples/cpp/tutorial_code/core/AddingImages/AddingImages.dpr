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
program AddingImages;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CV.Resource,
  cv.opencv;

const
  EXIT_FAILURE = 1;

Var
  alpha: double = 0.5;
  beta, input: double;
  src1, src2, dst: TMat;

begin
  try

    /// Ask the user enter alpha
    WriteLn(' Simple Linear Blender ');
    WriteLn('-----------------------');
    WriteLn(' * Enter alpha[0.0 - 1.0]: ');
    Readln(input);

    // We use the alpha provided by the user if it is between 0 and 1
    if (input >= 0) and (input <= 1) then
      alpha := input;

    // ![load]
    /// Read images ( both have to be of the same size and type )
    src1 := imread(OpenCVData + 'LinuxLogo.jpg');
    src2 := imread(OpenCVData + 'WindowsLogo.jpg');
    // ![load]

    if (src1.empty()) then
    begin
      WriteLn('Error loading src1');
      Halt(EXIT_FAILURE);
    end;
    if (src2.empty()) then
    begin
      WriteLn('Error loading src2');
      Halt(EXIT_FAILURE);
    end;

    // ![blend_images]
    beta := (1.0 - alpha);
    addWeighted(src1, alpha, src2, beta, 0.0, dst);
    // ![blend_images]

    // ![display]
    imshow('Linear Blend', dst);
    waitKey(0);
    // ![display]
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
