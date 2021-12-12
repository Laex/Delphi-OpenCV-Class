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
program camshiftdemo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

Var
  image: TMat;

  backprojMode: bool = false;
  selectObject: bool = false;
  trackObject: int   = 0;
  showHist: bool     = true;
  origin: TPoint;
  selection: TRect;
  vmin: int = 10;
  vmax: int = 256;
  smin: int = 30;

procedure onMouse(event, x, y, v: int; p: pointer);
begin
  if (selectObject) then
  begin
    selection.x := MIN(x, origin.x);
    selection.y := MIN(y, origin.y);
    selection.width := abs(x - origin.x);
    selection.height := abs(y - origin.y);

    selection := selection and Rect(0, 0, image.cols, image.rows);
  end;

  case MouseEventTypes(event) of
    EVENT_LBUTTONDOWN:
      begin
        origin := Point(x, y);
        selection := Rect(x, y, 0, 0);
        selectObject := true;
      end;
    EVENT_LBUTTONUP:
      begin
        selectObject := false;
        if (selection.width > 0) and (selection.height > 0) then
          trackObject := -1; // Set up CAMShift properties in main() loop
      end;

  end;
end;

Var
  hot_keys: string =                               //
    '\n\nHot keys: \n' +                           //
    '\tESC - quit the program\n' +                 //
    '\tc - stop the tracking\n' +                  //
    '\tb - switch to/from backprojection view\n' + //
    '\th - show/hide object histogram\n' +         //
    '\tp - pause video\n' +                        //
    'To initialize tracking, select the object with mouse\n';

procedure help;
begin
  cout + '\nThis is a demo that shows mean-shift based tracking\n' + //
    'You select a color objects such as your face and it tracks it.\n' + //
    'This reads from video camera (0 by default, or the camera number the user enters\n' + //
    'Usage: \n\t';
  cout + argv[0] + ' [camera number]\n';
  cout + hot_keys;
end;

Var
  maskroi: TMat;
  roi: TMat;
  hist: TMat;
  hsize: int             = 16;
  hranges: TArray<float> = [0, 180];
  buf: TMat;
  val: int;
  histimg: TMat;

begin
  try
    help;

    Var
      cap: TVideoCapture;
    Var
      trackWindow: TRect;
      // Var
      // hsize: int := 16;
      // Var
      // hranges: TArray<float> := [0, 180];
    Var
      phranges: pfloat := @hranges[0];

    if ParamCount = 0 then
      cap.open(0)
    else
      cap.open(ParamStr(1).ToInteger);

    if (not cap.isOpened()) then
    begin
      cout + '***Could not initialize capturing...***\n';
      cout + 'Current parameter''s value: \n';
      Halt(1);
    end;
    cout + hot_keys;
    namedWindow('Histogram', 0);
    namedWindow('CamShift Demo', 0);
    setMouseCallback('CamShift Demo', onMouse, nil);
    createTrackbar('Vmin', 'CamShift Demo', vmin, 256, nil);
    createTrackbar('Vmax', 'CamShift Demo', vmax, 256, nil);
    createTrackbar('Smin', 'CamShift Demo', smin, 256, nil);

    Var
      frame, hsv, hue, mask, { hist, } backproj: TMat;
      // Var
    histimg { : TMat } := TMat.zeros(200, 320, CV_8UC3);
    Var
      paused: bool := false;

    While true do
    begin
      if (not paused) then
      begin
        cap > frame;
        if (frame.empty()) then
          break;
      end;

      frame.copyTo(image);

      if (not paused) then
      begin
        cvtColor(image, hsv, COLOR_BGR2HSV);

        if (trackObject <> 0) then
        begin
          Var
            _vmin: int := vmin;
          Var
            _vmax: int := vmax;

          inRange(hsv, Scalar(0, smin, MIN(_vmin, _vmax)), Scalar(180, 256, MAX(_vmin, _vmax)), mask);
          Var
            ch: TArray<int> := [0, 0];

          hue.create(hsv.size, hsv.depth);
          // 1124024320
          mixChannels(hsv, 1, hue, 1, @ch[0], 1);

          if (trackObject < 0) then
          begin
            // Object has been selected by user, set up CAMShift search properties once
            // Var
            roi { : TMat } := TMat.Mat(hue, selection);
            // Var
            maskroi { : TMat } := TMat.Mat(mask, selection);

            calcHist(roi, 1, nil, maskroi, hist, 1, @hsize, @phranges);
            normalize(hist, hist, 0, 255, NORM_MINMAX);

            trackWindow := selection;
            trackObject := 1; // Don't set up again, unless user selects new ROI

            histimg.assign(TScalar.all(0));
            Var
              binW: int := histimg.cols div hsize;
              // Var
            buf { : TMat } := TMat.Mat(1, hsize, CV_8UC3);

            for Var i := 0 to hsize - 1 do
            begin
              Var
                v: TVec3b := [(i * 180) div hsize, 255, 255];
              buf.st<TVec3b>(i, v);
            end;

            cvtColor(buf, buf, COLOR_HSV2BGR);

            for Var i := 0 to hsize - 1 do
            begin
              // Var
              val { : int } := Trunc(hist.at<float>(i) * histimg.rows / 255);

              rectangle(                                   //
                histimg,                                   //
                Point(i * binW, histimg.rows),             //
                Point((i + 1) * binW, histimg.rows - val), //
                Scalar(buf.at<TVec3b>(i)),                  //
                -1,                                        //
                8                                          //
                );
            end;
          end;

          // Perform CAMShift
          calcBackProject(@hue, 1, nil, hist, backproj, @phranges);

          backproj := backproj and mask;
          Var
            trackBox: TRotatedRect := CamShift(backproj, trackWindow, TTermCriteria.TermCriteria(TTermCriteria.EPS or TTermCriteria.COUNT, 10, 1));
          if (trackWindow.area() <= 1) then
          begin
            Var
              cols: int := backproj.cols;
            Var
              rows: int := backproj.rows;
            var
              r: int := (MIN(cols, rows) + 5) div 6;
            trackWindow := Rect(trackWindow.x - r, trackWindow.y - r, trackWindow.x + r, trackWindow.y + r) and Rect(0, 0, cols, rows);
          end;

          if (backprojMode) then
            cvtColor(backproj, image, COLOR_GRAY2BGR);
          ellipse(image, trackBox, Scalar(0, 0, 255), 3, LINE_AA);
        end;
      end
      else if (trackObject < 0) then
        paused := false;

      if selectObject and (selection.width > 0) and (selection.height > 0) then
      begin
        // Var
        roi := TMat.Mat(image, selection);
        bitwise_not(roi, roi);
      end;
      imshow('CamShift Demo', image);
      imshow('Histogram', histimg);

      var
        c: char := Chr(waitKey(10));
      if (c = #27) then
        break;
      case c of
        'b':
          backprojMode := not backprojMode;
        'c':
          begin
            trackObject := 0;
            histimg.Assign(TScalar.all(0));
          end;
        'h':
          begin
            showHist := not showHist;
            if (not showHist) then
              destroyWindow('Histogram')
            else
              namedWindow('Histogram', 1);
          end;
        'p':
          paused := not paused;
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
