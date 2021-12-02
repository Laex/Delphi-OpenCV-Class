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
program segment_objects;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  CVResource,
  opencv_world;

procedure refineSegments(const img: TMat; const mask: TMat; Var dst: TMat);
begin
  Var
    niters: int := 3;

  Var
    contours: vector<vector<TPoint>>;
  Var
    hierarchy: vector<Vec4i>;

  Var
    temp: TMat;

  dilate(mask, temp, TMat.Mat(), Point(-1, -1), niters);
  erode(temp, temp, TMat.Mat(), Point(-1, -1), niters * 2);
  dilate(temp, temp, TMat.Mat(), Point(-1, -1), niters);

  findContours(temp, contours, hierarchy, RETR_CCOMP, CHAIN_APPROX_SIMPLE);

  dst := TMat.zeros(img.size, CV_8UC3);

  if (contours.size() = 0) then
    exit;

  // iterate through all the top-level contours,
  // draw each connected component with its own random color
  Var
    idx: int := 0;
  Var
    largestComp: int := 0;
  Var
    maxArea: double := 0;

  idx := hierarchy[idx][0];

  while idx >= 0 do
  begin

    Var
      c: vector<TPoint> := contours[idx];
    Var
      area: double := abs(contourArea(TMat(c)));

    if (area > maxArea) then
    begin
      maxArea     := area;
      largestComp := idx;
    end;

    idx := hierarchy[idx][0];
  end;
  Var
    color: TScalar := Scalar(0, 0, 255);

  Var
    i: TInputArrayOfArrays := contours;

  drawContours(dst, contours, largestComp, color, 1, LINE_8, hierarchy);
end;

begin
  try
    WriteLn( //
      'This program demonstrated a simple method of connected components clean up of background subtraction'#13#10 + //
      'When the program starts, it begins learning the background.'#13#10 + //
      'You can toggle background learning on and off by hitting the space bar.'#13#10 + //
      'Call'#13#10, //
      ExtractFileName(ParamStr(0)), ' [video file, else it reads camera 0]');

    Var
      cap: TVideoCapture;
    Var
      update_bg_model: bool := true;

    if (ParamCount = 0) then
      cap.open(0)
    else if (ParamCount > 0) then
    begin
      if (FileExists(ParamStr(1))) then
        cap.open(ParamStr(1))
      else
      begin
        Var
          CamIndex: Integer := 0;
        TryStrToInt(ParamStr(1), CamIndex);
        cap.open(CamIndex);
      end;
    end;

    if (not cap.isOpened()) then
    begin
      WriteLn('Can not open camera or video file');
      Halt(1);
    end;

    Var
      tmp_frame, bgmask, out_frame: TMat;

    cap > tmp_frame;
    if (tmp_frame.empty()) then
    begin
      WriteLn('can not read data from the video source');
      Halt(1);
    end;

    namedWindow('video', 1);
    namedWindow('segmented', 1);

    Var
      bgsubtractor: TPtr<TBackgroundSubtractorMOG2> := createBackgroundSubtractorMOG2();
    bgsubtractor.v.setVarThreshold(10);

    While true do
    begin
      cap > tmp_frame;
      if (tmp_frame.empty()) then
        break;
      bgsubtractor.v.apply(tmp_frame, bgmask, int(update_bg_model));
      refineSegments(tmp_frame, bgmask, out_frame);
      imshow('video', tmp_frame);
      imshow('segmented', out_frame);
      Var
        keycode: char := char(waitKey(30));
      if (keycode = #27) then
        break;
      if (keycode = ' ') then
      begin
        update_bg_model := not update_bg_model;
        WriteLn('Learn background is in state = ', update_bg_model);
      end;
    end;

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
