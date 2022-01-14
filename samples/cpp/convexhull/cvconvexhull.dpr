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
program cvconvexhull;

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
    cout + //
      '\nThis sample program demonstrates the use of the convexHull() function\n' + //
      'Call:\n' + //
      argv[0] + endl;

    Var
      img: TMat := TMat.mat(500, 500, CV_8UC3);
    Var
      rng: TRNG := theRNG();

    While True do
    begin
      Var
        i: int;
      Var
        count: UInt := rng;
      count := (count mod 100) + 1;

      Var
        points: vector<TPoint>;

      for Var j:int := 0 to count - 1 do
      begin
        Var
          pt: TPoint;
        pt.x := rng.uniform(img.cols div 4, (img.cols * 3) div 4);
        pt.y := rng.uniform(img.rows div 4, (img.rows * 3) div 4);

        points.push_back(pt);
      end;

      Var
        hull: vector<TPoint>;
      convexhull(points, hull, True);

      img.Assign(TScalar.all(0));
      for Var j:int := 0 to count - 1 do
        circle(img, points[j], 3, Scalar(0, 0, 255), Int(FILLED), LINE_AA);

      Var
        a:TInputArrayOfArrays;

      polylines(img, hull, True, Scalar(0, 255, 0), 1, Int(LINE_AA));
      imshow('hull', img);

      Var
        key: char := chr(waitKey());
      if (key = #27) or (key = 'q') or (key = 'Q') then // 'ESC'
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
