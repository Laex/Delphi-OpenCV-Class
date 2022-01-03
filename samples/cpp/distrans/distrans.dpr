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
    merge(@planes[0], 3, dist8u);
  end
  else
  begin
    dist8u.create(labels.size, CV_8UC3);
    for Var i: int := 0 to labels.rows - 1 do
    begin
      Var
        ll: pInt := labels.pt<int>(i);
      Var
        dd: pfloat := dist.pt<Float>(i);
      Var
        d: puchar := dist8u.pt<uchar>(i);
      for Var j: int := 0 to labels.cols - 1 do
      begin
        Var
          idx: int := iif.iif<int>((ll[j] = 0) or (dd[j] = 0), 0, ((ll[j] - 1) mod 8) + 1);
        Var
          scale: Float := 1 / (1 + dd[j] * dd[j] * 0.0004);
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

procedure help;
begin
  printf( //
    '\nProgram to demonstrate the use of the distance transform function between edge images.\n' + //
    'Usage:\n' + //
    argv[0] + ' [image_name -- default image is stuff.jpg]\n' + //
    '\nHot keys: \n' +                                     //
    '\tESC - quit the program\n' +                         //
    '\tC - use C/Inf metric\n' +                           //
    '\tL1 - use L1 metric\n' +                             //
    '\tL2 - use L2 metric\n' +                             //
    '\t3 - use 3x3 mask\n' +                               //
    '\t5 - use 5x5 mask\n' +                               //
    '\t0 - use precise distance transform\n' +             //
    '\tv - switch to Voronoi diagram mode\n' +             //
    '\tp - switch to pixel-based Voronoi diagram mode\n' + //
    '\tSPACE - loop through all the modes\n\n');
end;

begin
  try
    help;

    Var
      filename: string := OpenCVData + 'stuff.jpg';
    if (ParamCount > 0) and FileExists(ParamStr(1)) then
      filename := ParamStr(1);
    gray := imread(filename, IMREAD_GRAYSCALE);
    if (gray.empty()) then
    begin
      printf('Cannot read image file: ' + filename + '\n');
      Halt(1);
    end;

    namedWindow('Distance Map', 1);
    createTrackbar('Brightness Threshold', 'Distance Map', @edgeThresh, 255, onTrackbar, nil);

    while True do
    begin
      // Call to update the view
      onTrackbar(0, nil);

      Var
        c: char := chr(waitKey(0));

      if (c = #27) then
        break;

      if (c = 'c') or (c = 'C') or (c = '1') or (c = '2') or (c = '3') or (c = '5') or (c = '0') then
        voronoiType := -1;

      if (c = 'c') or (c = 'C') then
        distType0 := DIST_C
      else if (c = '1') then
        distType0 := DIST_L1
      else if (c = '2') then
        distType0 := DIST_L2
      else if (c = '3') then
        maskSize0 := DIST_MASK_3
      else if (c = '5') then
        maskSize0 := DIST_MASK_5
      else if (c = '0') then
        maskSize0 := DIST_MASK_PRECISE
      else if (c = 'v') then
        voronoiType := 0
      else if (c = 'p') then
        voronoiType := 1
      else if (c = ' ') then
      begin
        if (voronoiType = 0) then
          voronoiType := 1
        else if (voronoiType = 1) then
        begin
          voronoiType := -1;
          maskSize0 := DIST_MASK_3;
          distType0 := DIST_C;
        end
        else if (distType0 = DIST_C) then
          distType0 := DIST_L1
        else if (distType0 = DIST_L1) then
          distType0 := DIST_L2
        else if (maskSize0 = DIST_MASK_3) then
          maskSize0 := DIST_MASK_5
        else if (maskSize0 = DIST_MASK_5) then
          maskSize0 := DIST_MASK_PRECISE
        else if (maskSize0 = DIST_MASK_PRECISE) then
          voronoiType := 0;
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
