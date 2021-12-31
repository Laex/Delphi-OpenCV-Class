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
program distrans;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

Var
  maskSize0: DistanceTransformMasks = DIST_MASK_5;
  voronoiType: int                  = -1;
  edgeThresh: int                   = 100;
  distType0: DistanceTypes          = DIST_L1;

    // The output and temporary images
  gray: TMat;

procedure onTrackbar(a: int; p: pointer);
begin
  Var
    colors: TArray<TScalar> := [ //
      Scalar(0, 0, 0), Scalar(255, 0, 0), Scalar(255, 128, 0), //
      Scalar(255, 255, 0), Scalar(0, 255, 0), Scalar(0, 128, 255), //
      Scalar(0, 255, 255), Scalar(0, 0, 255), Scalar(255, 0, 255) //
      ];

  Var
    maskSize: DistanceTransformMasks := iif.iif<DistanceTransformMasks>(voronoiType >= 0, DIST_MASK_5, maskSize0);
  Var
    distType: DistanceTypes := iif.iif<DistanceTypes>(voronoiType >= 0, DIST_L2, distType0);

  Var
    edge: TMat := gray >= edgeThresh;
  Var
    dist, labels, dist8u: TMat;

  if (voronoiType < 0) then
    distanceTransform(edge, dist, distType, maskSize)
  else
    distanceTransform(edge, dist, labels, distType, maskSize, DistanceTransformLabelTypes(voronoiType));

  if (voronoiType < 0) then
  begin
      // begin 'painting' the distance transform result
    dist := dist * 5000;
    pow(dist, 0.5, dist);

    Var
      dist32s, dist8u1, dist8u2: TMat;

    dist.convertTo(dist32s, CV_32S, 1, 0.5);
    dist32s := dist32s and TScalar.all(255);

    dist32s.convertTo(dist8u1, CV_8U, 1, 0);
    dist32s := dist32s * (-1);

    dist32s := dist32s + TScalar.all(255);
    dist32s.convertTo(dist8u2, CV_8U);

    Var
      planes: TArray<TMat> := [dist8u1, dist8u2, dist8u2];
    merge(planes, 3, dist8u);
  end
  else
  begin
    dist8u.create(labels.size, CV_8UC3);
    for Var i: int := 0 to labels.rows - 1 do
    begin
      Var
        ll: pInt := labels.ptr(i);
      Var
        dd: pfloat := dist.ptr(i);
      Var
        d: puchar := dist8u.ptr(i);
      for Var j: int := 0 to labels.cols - 1 do
      begin
        Var
          idx: int := iif.iif<int>((ll[j] = 0) or (dd[j] = 0), 0, ((ll[j] - 1) mod 8) + 1);
        Var
          scale: float := 1 / (1 + dd[j] * dd[j] * 0.0004);
        Var
          b: int := cvRound(colors[idx][0] * scale);
        Var
          g: int := cvRound(colors[idx][1] * scale);
        Var
          r: int := cvRound(colors[idx][2] * scale);
        d[j * 3] := b;
        d[j * 3 + 1] := g;
        d[j * 3 + 2] := r;
      end;
    end;
  end;

  imshow('Distance Map', dist8u);
end;

begin
  try

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
