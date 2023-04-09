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
program cv_laplace;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help;
begin
  cout + //
    '\nThis program demonstrates Laplace point/edge detection using OpenCV function Laplacian()\n' + //
    'It captures from the camera of your choice: 0, 1, ... default 0\n' + //
    'Call:\n' + //
    argv[0] + ' -c <camera #, default 0> -p <index of the frame to be decoded/captured next>\n' + endl;
end;

const
  GAUSSIAN = 0;
  BLUR     = 1;
  MEDIAN   = 2;

Var
  sigma: int      = 3;
  smoothType: int = GAUSSIAN;

begin
  try
    help;

    Var
      cap: TVideoCapture;
    Var
      pos: int := 0;
    Var
      i: int := 1;
    Var
      camera: String := '0';
    While i <= ParamCount do
    begin
      if SameText('-c', ParamStr(i)) then
      begin
        if (i < ParamCount) and (isIntNumber(ParamStr(i + 1))) then
          camera := ParamStr(i + 1);
        inc(i);
      end
      else if SameText('-p', ParamStr(i)) then
      begin
        if (i < ParamCount) and (isIntNumber(ParamStr(i + 1))) then
          pos := ParamStr(i + 1).ToInteger;
        inc(i);
      end
      else
        Halt(1);
      inc(i);
    end;

    cap.open(camera.ToInteger);

    if (not cap.isOpened()) then
    begin
      cerr + 'Can''t open camera/video stream: ' + camera + endl;
      Halt(1);
    end;
    cout +                //
      'Video ' + camera + //
      ': width=' + cap.get(CAP_PROP_FRAME_WIDTH).ToString + //
      ', height=' + cap.get(CAP_PROP_FRAME_HEIGHT).ToString + //
      ', nframes=' + cap.get(CAP_PROP_FRAME_COUNT).ToString + endl;

    if (pos <> 0) then
    begin
      cout + 'seeking to frame #' + pos.ToString + endl;
      if (not cap.&set(CAP_PROP_POS_FRAMES, pos)) then
        cerr + 'ERROR: seekeing is not supported' + endl;
    end;

    namedWindow('Laplacian', WINDOW_AUTOSIZE);
    createTrackbar('Sigma', 'Laplacian', @sigma, 15, nil);

    Var
      smoothed, laplace, result: TMat;

    While True do
    begin
      Var
        frame: TMat;
      cap > frame;
      if (frame.empty()) then
        break;

      Var
        ksize: int := (sigma * 5) or 1;
      if (smoothType = GAUSSIAN) then
        GaussianBlur(frame, smoothed, Size(ksize, ksize), sigma, sigma)
      else if (smoothType = BLUR) then
        cv.opencv.BLUR(frame, smoothed, Size(ksize, ksize))
      else
        medianBlur(frame, smoothed, ksize);

      Laplacian(smoothed, laplace, CV_16S, 5);
      convertScaleAbs(laplace, result, (sigma + 1) * 0.25);
      imshow('Laplacian', result);

      Var
        c: char := chr(waitKey(30));
      if (c = ' ') then
      begin
        if smoothType = GAUSSIAN then
          smoothType := BLUR
        else if smoothType = BLUR then
          smoothType := MEDIAN
        else
          smoothType := GAUSSIAN;
      end
      else if (c = 'q') or (c = 'Q') or (c = #27) then
        break;
    end;

  except
    on e: Exception do
    begin
      WriteLn(e.ClassName, ': ', e.Message);
      Readln;
    end;
  end;

end.
