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
program phase_corr;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

begin
  try
    Var
      video: TVideoCapture := 0;

    Var
      frame, curr, prev, curr64f, prev64f, hann: TMat;
    Var
      key: char;

    repeat

      video > frame;
      cvtColor(frame, curr, COLOR_RGB2GRAY);

      if (prev.empty()) then
      begin
        prev := curr.clone();
        createHanningWindow(hann, curr.size, CV_64F);
      end;

      prev.convertTo(prev64f, CV_64F);
      curr.convertTo(curr64f, CV_64F);

      Var
        shift: TPoint2d := phaseCorrelate(prev64f, curr64f, hann);
      Var
        radius: double := sqrt(shift.x * shift.x + shift.y * shift.y);

      if (radius > 5) then
      begin
        // draw a circle and line indicating the shift direction...
        Var
          center: TPoint := Point(curr.cols shr 1, curr.rows shr 1);
        circle(frame, center, Trunc(radius), Scalar(0, 255, 0), 3, LINE_AA);
        line(frame, center, Point(center.x + Trunc(shift.x), center.y + Trunc(shift.y)), Scalar(0, 255, 0), 3, LINE_AA);
      end;

      imshow('phase shift', frame);
      key := char(waitKey(2));

      prev := curr.clone();
    until (key = #27); // Esc to exit...

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
