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
program contours2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help();
begin
  cout //
    + '\nThis program illustrates the use of findContours and drawContours\n' //
    + 'The original image is put up along with the image of drawn contours\n' //
    + 'Usage:\n';
  cout        //
    + argv[0] //
    + '\nA trackbar is put up which controls the contour level from -3 to 3\n' //
    + endl;
end;

const
  w: int = 500;

Var
  levels: int = 3;

  contours: vector<vector<TPoint>>;
  hierarchy: vector<TVec4i>;

procedure on_trackbar(a: int; p: pointer);
begin
  Var
    cnt_img: TMat := TMat.zeros(w, w, CV_8UC3);
  Var
    _levels: int := levels - 3;

  drawContours(cnt_img, contours, iif.iif<int>(_levels <= 0, 3, -1), Scalar(128, 255, 255), 3, LINE_AA, hierarchy, abs(_levels));

  imshow('contours', cnt_img);
end;

begin
  try
    help();

    Var
      img: TMat := TMat.zeros(w, w, CV_8UC1);
        // Draw 6 faces
    for Var i: int := 0 to 5 do
    begin
      Var
        dx: int := (i mod 2) * 250 - 30;
      Var
        dy: int := (i div 2) * 150;
      Var
        white: TScalar := Scalar(255);
      Var
        black: TScalar := Scalar(0);

      if (i = 0) then
      begin
        for Var j: int := 0 to 9 do
        begin
          Var
            angle: double := (j + 5) * CV_PI / 21;
          line(img, Point(cvRound(dx + 100 + j * 10 - 80 * cos(angle)), cvRound(dy + 100 - 90 * sin(angle))), Point(cvRound(dx + 100 + j * 10 - 30 * cos(angle)), cvRound(dy + 100 - 30 * sin(angle))),
            white, 1, 8, 0);
        end;
      end;

      ellipse(img, Point(dx + 150, dy + 100), Size(100, 70), 0, 0, 360, white, -1, 8, 0);
      ellipse(img, Point(dx + 115, dy + 70), Size(30, 20), 0, 0, 360, black, -1, 8, 0);
      ellipse(img, Point(dx + 185, dy + 70), Size(30, 20), 0, 0, 360, black, -1, 8, 0);
      ellipse(img, Point(dx + 115, dy + 70), Size(15, 15), 0, 0, 360, white, -1, 8, 0);
      ellipse(img, Point(dx + 185, dy + 70), Size(15, 15), 0, 0, 360, white, -1, 8, 0);
      ellipse(img, Point(dx + 115, dy + 70), Size(5, 5), 0, 0, 360, black, -1, 8, 0);
      ellipse(img, Point(dx + 185, dy + 70), Size(5, 5), 0, 0, 360, black, -1, 8, 0);
      ellipse(img, Point(dx + 150, dy + 100), Size(10, 5), 0, 0, 360, black, -1, 8, 0);
      ellipse(img, Point(dx + 150, dy + 150), Size(40, 10), 0, 0, 360, black, -1, 8, 0);
      ellipse(img, Point(dx + 27, dy + 100), Size(20, 35), 0, 0, 360, white, -1, 8, 0);
      ellipse(img, Point(dx + 273, dy + 100), Size(20, 35), 0, 0, 360, white, -1, 8, 0);
    end;
      // show the faces
    namedWindow('image', 1);
    imshow('image', img);
      // Extract the contours so that
    Var
      contours0: vector<vector<TPoint>>;

    findContours(img, contours0, hierarchy, RETR_TREE, CHAIN_APPROX_SIMPLE);

    contours.resize(contours0.Size());

    for Var k: int := 0 to contours0.Size() - 1 do
    begin
      Var
        p: vector<TPoint>;
      approxPolyDP(TMat.Mat<TPoint>(contours0[k]), p, 3, true);
      contours[k] := p;
    end;

    namedWindow('contours', 1);
    createTrackbar('levels+3', 'contours', @levels, 7, on_trackbar);

    on_trackbar(0, nil);
    waitKey();

  except
    on e: Exception do
    begin
      WriteLn(e.ClassName, ': ', e.Message);
      Readln;
    end;
  end;

end.
