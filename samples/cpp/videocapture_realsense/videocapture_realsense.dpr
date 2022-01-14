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
program videocapture_realsense;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

begin
  try
    Var
      capture: TVideoCapture := TVideoCapture.capture(0, CAP_INTELPERC);
    While True do
    begin
      Var
       depthMap, image, irImage, adjMap: TMat;

      capture.grab();
      capture.retrieve(depthMap, CAP_INTELPERC_DEPTH_MAP);
      capture.retrieve(image, CAP_INTELPERC_IMAGE);
      capture.retrieve(irImage, CAP_INTELPERC_IR_MAP);

      if not depthMap.empty then
        normalize(depthMap, adjMap, 0, 255, NORM_MINMAX, CV_8UC1);

      if not adjMap.empty then
        applyColorMap(adjMap, adjMap, COLORMAP_JET);

      if not image.empty then
        imshow('RGB', image);
      if not irImage.empty then
        imshow('IR', irImage);
      if not adjMap.empty then
        imshow('DEPTH', adjMap);

      if (waitKey(30) >= 0) then
        break;
    end;

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
