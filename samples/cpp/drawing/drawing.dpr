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
program drawing;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

const
  EXIT_FAILURE = 1;

procedure help;
begin
  WriteLn('This program demonstrates OpenCV drawing and text output functions.'#13#10'Usage:'#13#10 + ExtractFileName(ParamStr(0)));
end;

function randomColor(const rng: TRNG): TScalar;
begin
  Var
    icolor: unsigned := rng;
  Result := Scalar(icolor and 255, (icolor shr 8) and 255, (icolor shr 16) and 255);
end;

Var
  pt: array [0 .. 1, 0 .. 2] of TPoint;
  ppt: array [0 .. 1] of ^TPoint;
  npt: array [0 .. 1] of Int;

begin
  try
    help;

    Var
      wndname: CppString := 'Drawing Demo';
    Var
      NUMBER: Int := 100;
    Var
      DELAY: Int := 5;
    Var
      lineType: lineTypes := LINE_AA; // change it to LINE_8 to see non-antialiased graphics
    Var
      i: Int;
    Var
      width: Int := 1000;
    Var
      height: Int := 700;
    Var
      x1: Int := -width div 2;
    Var
      x2: Int := width * 3 div 2;
    Var
      y1: Int := -height div 2;
    Var
      y2: Int := (height * 3) div 2;
    Var
      rng: TRNG := $FFFFFFFF;

    Var
      image: TMat := TMat.zeros(height, width, CV_8UC3);

    imshow(wndname, image);
    waitKey(DELAY);

    for i := 0 to NUMBER * 2 - 1 do
    begin
      Var
        pt1: TPoint;
      Var
        pt2: TPoint;
      pt1.x := rng.uniform(x1, x2);
      pt1.y := rng.uniform(y1, y2);
      pt2.x := rng.uniform(x1, x2);
      pt2.y := rng.uniform(y1, y2);

      Var
        arrowed: Int := rng.uniform(0, 6);

      if (arrowed < 3) then
        line(image, pt1, pt2, randomColor(rng), rng.uniform(1, 10), lineType)
      else
        arrowedLine(image, pt1, pt2, randomColor(rng), rng.uniform(1, 10), lineType);

      imshow(wndname, image);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
    end;

    for i := 0 to NUMBER * 2 - 1 do
    begin
      Var
        pt1: TPoint;
      Var
        pt2: TPoint;
      pt1.x := rng.uniform(x1, x2);
      pt1.y := rng.uniform(y1, y2);
      pt2.x := rng.uniform(x1, x2);
      pt2.y := rng.uniform(y1, y2);
      Var
        thickness: Int := rng.uniform(-3, 10);
      Var
        marker: Int := rng.uniform(0, 10);
      Var
        marker_size: Int := rng.uniform(30, 80);

      if (marker > 5) then
        rectangle(image, pt1, pt2, randomColor(rng), MAX(thickness, -1), lineType)
      else
        drawMarker(image, pt1, randomColor(rng), MarkerTypes(marker), marker_size);

      imshow(wndname, image);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
    end;

    for i := 0 to NUMBER - 1 do
    begin
      Var
        center: TPoint;
      center.x := rng.uniform(x1, x2);
      center.y := rng.uniform(y1, y2);
      Var
        axes: TSize;
      axes.width := rng.uniform(0, 200);
      axes.height := rng.uniform(0, 200);
      Var
        angle: double := rng.uniform(0, 180);

      ellipse(image, center, axes, angle, angle - 100, angle + 200, randomColor(rng), rng.uniform(-1, 9), lineType);

      imshow(wndname, image);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
    end;

    for i := 0 to NUMBER - 1 do
    begin
      // Point pt[2][3];
      pt[0][0].x := rng.uniform(x1, x2);
      pt[0][0].y := rng.uniform(y1, y2);
      pt[0][1].x := rng.uniform(x1, x2);
      pt[0][1].y := rng.uniform(y1, y2);
      pt[0][2].x := rng.uniform(x1, x2);
      pt[0][2].y := rng.uniform(y1, y2);
      pt[1][0].x := rng.uniform(x1, x2);
      pt[1][0].y := rng.uniform(y1, y2);
      pt[1][1].x := rng.uniform(x1, x2);
      pt[1][1].y := rng.uniform(y1, y2);
      pt[1][2].x := rng.uniform(x1, x2);
      pt[1][2].y := rng.uniform(y1, y2);

      ppt[0] := @pt[0];
      ppt[1] := @pt[1];
      npt[0] := 3;
      npt[1] := 3;

      polylines(image, @ppt, @npt, 2, true, randomColor(rng), rng.uniform(1, 10), lineType);

      imshow(wndname, image);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
    end;

    for i := 0 to NUMBER - 1 do
    begin
      // Point pt[2][3];
      pt[0][0].x := rng.uniform(x1, x2);
      pt[0][0].y := rng.uniform(y1, y2);
      pt[0][1].x := rng.uniform(x1, x2);
      pt[0][1].y := rng.uniform(y1, y2);
      pt[0][2].x := rng.uniform(x1, x2);
      pt[0][2].y := rng.uniform(y1, y2);
      pt[1][0].x := rng.uniform(x1, x2);
      pt[1][0].y := rng.uniform(y1, y2);
      pt[1][1].x := rng.uniform(x1, x2);
      pt[1][1].y := rng.uniform(y1, y2);
      pt[1][2].x := rng.uniform(x1, x2);
      pt[1][2].y := rng.uniform(y1, y2);

      ppt[0] := @pt[0];
      ppt[1] := @pt[1];
      npt[0] := 3;
      npt[1] := 3;

      fillPoly(image, @ppt, @npt, 2, randomColor(rng), lineType);

      imshow(wndname, image);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
    end;

    for i := 0 to NUMBER - 1 do
    begin
      Var
        center: TPoint;
      center.x := rng.uniform(x1, x2);
      center.y := rng.uniform(y1, y2);

      circle(image, center, rng.uniform(0, 300), randomColor(rng), rng.uniform(-1, 9), lineType);

      imshow(wndname, image);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
    end;

    for i := 1 to NUMBER - 1 do
    begin
      Var
        org: TPoint;
      org.x := rng.uniform(x1, x2);
      org.y := rng.uniform(y1, y2);

      putText(image, 'Testing text rendering', org, HersheyFonts(rng.uniform(0, 8)), rng.uniform(0, 100) * 0.05 + 0.1, randomColor(rng), rng.uniform(1, 10), lineType);

      imshow(wndname, image);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
    end;
    Var
      textsize: TSize := getTextSize('OpenCV forever!', FONT_HERSHEY_COMPLEX, 3, 5, nil);

    Var
      org: TPoint := Point((width - textsize.width) div 2, (height - textsize.height) div 2);

    Var
      image2: TMat;
    i := 0;
    While i < 255 do
    begin
      image2 := image - TScalar.all(i);
      putText(image2, 'OpenCV forever!', org, FONT_HERSHEY_COMPLEX, 3, Scalar(i, i, 255), 5, lineType);

      imshow(wndname, image2);
      if (waitKey(DELAY) >= 0) then
        Halt(0);
      i := i + 2;
    end;

    waitKey();

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
