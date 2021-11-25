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
program optical_flow;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$R *.res}

uses
  System.SysUtils,
  CVResource,
  opencv_world;

const
  EXIT_FAILURE = 1;

Var
  pp1, pp2: TPoint2f;
  // prevPts: TInputArray;

begin
  try
    (*
      const
      about = //
      'This sample demonstrates Lucas-Kanade Optical Flow calculation.'#13#10 + //
      'The example file can be downloaded from:'#13#10 + //
      '  https://www.bogotobogo.com/python/OpenCV_Python/images/mean_shift_tracking/slow_traffic_small.mp4';
      const
      keys =                                            //
      '{ h help |      | print this help message }' + //
      '{ @image | vtest.avi | path to image file }';
      var
      parser: TCommandLineParser;
      parser.CommandLineParser(keys);
      parser.about(about);
      if (parser.has('help')) then
      begin
      parser.printMessage();
      Halt(0);
      end;
      Var
      filename: cppstring := parser.get<string>('@image');
      if (not parser.check()) then
      begin
      parser.printErrors();
      Halt(0);
      end;
    *)
    Var
      capture: TVideoCapture;

    capture.open(OpenCVData + 'vtest.avi');

    if (not capture.isOpened()) then
    begin
      // error in opening the video input
      Writeln('Unable to open file!');
      Halt(0);
    end;

    // Create some random colors
    Var
      colors: Vector<TScalar>;
    Var
      rng: TRNG;
    for Var i: int := 0 to 100 - 1 do
    begin
      Var
        r: int := rng.uniform(0, 256);
      Var
        g: int := rng.uniform(0, 256);
      Var
        b: int := rng.uniform(0, 256);
      colors.push_back(Scalar(r, g, b));
    end;

    Var
      old_frame, old_gray: TMat;
    Var
      p0, p1: Vector<TPoint2f>;

      // Take first frame and find corners in it
    capture.read(old_frame);
    cvtColor(old_frame, old_gray, COLOR_BGR2GRAY);
    goodFeaturesToTrack(old_gray, p0, 100, 0.3, 7, TMat.Mat, 7, false, 0.04);

    // Create a mask image for drawing purposes
    Var
      mask: TMat := TMat.zeros(old_frame.size, old_frame.&type);

      // Var
      // j: Integer := 0;

    while (true) do
    begin
      Var
        frame, frame_gray: TMat;

      capture.read(frame);
      if (frame.empty()) then
        break;
      cvtColor(frame, frame_gray, COLOR_BGR2GRAY);

      // calculate optical flow
      Var
        status: Vector<uchar>;
      Var
        err: Vector<float>;
      Var
      criteria := TTermCriteria.TermCriteria(TTermCriteria.COUNT + TTermCriteria.EPS, 10, 0.03);

      calcOpticalFlowPyrLK(old_gray, frame_gray, p0, p1, status, err, size(15, 15), 2, criteria);

      Var
        good_new: Vector<TPoint2f>;

      for Var i: int := 0 to p0.size() - 1 do
      begin
        // Select good points
        if (status[i] = 1) then
        begin
          good_new.push_back(p1[i]);
          // draw the tracks

          pp1 := p0[i];
          pp2 := p1[i];

          line(mask, p1[i], p0[i], colors[i], 2);
          circle(frame, p1[i], 5, colors[i], -1);
        end;
      end;
      Var
        img: TMat;

      add(frame, mask, img);

      imshow('Frame', img);

      Var
        keyboard: int := waitKey(30);
      if (keyboard = int('q')) or (keyboard = 27) then
        break;

      // Now update the previous frame and previous points
      old_gray := frame_gray.clone();
      p0 := good_new;
    end;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
