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
program dis_opticalflow;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  CVResource,
  opencv_world;

begin
  try
    // CommandLineParser parser(argc, argv, "{ @video  | vtest.avi  | use video as input }");
    // string filename = samples::findFileOrKeep(parser.get<string>("@video"));

    Var
      filename: string;
    if ParamCount > 0 then
      filename := ParamStr(1)
    else
      filename := OpenCVData + 'vtest.avi';

    Var
      cap: TVideoCapture;
    cap.open(filename);

    if (not cap.isOpened()) then
    begin
      WriteLn('ERROR: Cannot open file ', filename);
      Halt(1);
    end;

    Var
      prevgray, gray, rgb, frame: TMat;
    Var
      flow: TMat;
    Var
      flow_uv: TArray<TMat> := [TMat.Mat(), TMat.Mat()];
    Var
      mag, ang: TMat;
    Var
      hsv_split: TArray<TMat> := [TMat.Mat(), TMat.Mat(), TMat.Mat()];
    Var
      hsv: TMat;

    Var
      algorithm: TPtr<TDenseOpticalFlow> := TDISOpticalFlow.create<TDenseOpticalFlow>(TDISOpticalFlow.PRESET_MEDIUM);

    while (true) do
    begin
      cap > frame;
      if (frame.empty()) then
        break;

      cvtColor(frame, gray, COLOR_BGR2GRAY);

      if (not prevgray.empty()) then
      begin
        algorithm.v.calc(prevgray, gray, flow);
        split(flow, flow_uv);
        multiply(flow_uv[1], -1, flow_uv[1]);
        cartToPolar(flow_uv[0], flow_uv[1], mag, ang, true);
        normalize(mag, mag, 0, 1, NORM_MINMAX);
        hsv_split[0] := ang;
        hsv_split[1] := mag;
        hsv_split[2] := TMat.ones(ang.size, ang.&type);
        merge(hsv_split, { 3, } hsv);
        cvtColor(hsv, rgb, COLOR_HSV2BGR);
        imshow('flow', rgb);
        imshow('orig', frame);
      end;

      if (waitKey(20) > 0) then
        break;
      swap(prevgray, gray);
    end;

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
