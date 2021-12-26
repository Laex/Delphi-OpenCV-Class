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
program peopledetect;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

Type
  TDetector = record
  public type
    TMode = (Default, Daimler);
  public
  Var
    m: TMode;
    hog, hog_d: THOGDescriptor;
  public
      (*
        Detector() : m(Default), hog(), hog_d(Size(48, 96), Size(16, 16), Size(8, 8), Size(8, 8), 9)
        {
        hog.setSVMDetector(HOGDescriptor::getDefaultPeopleDetector());
        hog_d.setSVMDetector(HOGDescriptor::getDaimlerPeopleDetector());
        }
      *)
    class operator Initialize(out Dest: TDetector);

      // void toggleMode() { m = (m == Default ? Daimler : Default); }
    procedure toggleMode;
      // string modeName() const { return (m == Default ? "Default" : "Daimler"); }
    function modeName: string;
      // vector<Rect> detect(InputArray img)
      // {
      // // Run the detector with default parameters. to get a higher hit-rate
      // // (and more false alarms, respectively), decrease the hitThreshold and
      // // groupThreshold (set groupThreshold to 0 to turn off the grouping completely).
      // vector<Rect> found;
      // if (m == Default)
      // hog.detectMultiScale(img, found, 0, Size(8,8), Size(), 1.05, 2, false);
      // else if (m == Daimler)
      // hog_d.detectMultiScale(img, found, 0, Size(8,8), Size(), 1.05, 2, true);
      // return found;
      // }
    function detect(const img: TInputArray): vector<TRect>;
      // void adjustRect(Rect & r) const
      // {
      // // The HOG detector returns slightly larger rectangles than the real objects,
      // // so we slightly shrink the rectangles to get a nicer output.
      // r.x += cvRound(r.width*0.1);
      // r.width = cvRound(r.width*0.8);
      // r.y += cvRound(r.height*0.07);
      // r.height = cvRound(r.height*0.8);
      // }
    procedure adjustRect(Var r: TRect);
  end;

    { TDetector }

procedure TDetector.adjustRect(Var r: TRect);
begin
    // The HOG detector returns slightly larger rectangles than the real objects,
    // so we slightly shrink the rectangles to get a nicer output.
  r.x := r.x + cvRound(r.width * 0.1);
  r.width := cvRound(r.width * 0.8);
  r.y := r.y + cvRound(r.height * 0.07);
  r.height := cvRound(r.height * 0.8);
end;

function TDetector.detect(const img: TInputArray): vector<TRect>;
begin
    // Run the detector with default parameters. to get a higher hit-rate
    // (and more false alarms, respectively), decrease the hitThreshold and
    // groupThreshold (set groupThreshold to 0 to turn off the grouping completely).
  if (m = Default) then
    hog.detectMultiScale(img, Result, 0, Size(8, 8), Size(), 1.05, 2, false)
  else if (m = Daimler) then
    hog_d.detectMultiScale(img, Result, 0, Size(8, 8), Size(), 1.05, 2, true);
end;

class operator TDetector.Initialize(out Dest: TDetector);
begin
  Dest.m := TDetector.TMode.Default;
  THOGDescriptor.HOGDescriptor(Dest.hog_d, Size(48, 96), Size(16, 16), Size(8, 8), Size(8, 8), 9);
  Dest.hog.setSVMDetector(THOGDescriptor.getDefaultPeopleDetector());
  Dest.hog_d.setSVMDetector(THOGDescriptor.getDaimlerPeopleDetector());
end;

Var
  keys: string = // '{ help h   |   | print help message }'#13#10+//
    '{ camera c | 0 | capture video from camera (device index starting from 0) }'#13#10 + //
    '{ video v  |   | use video as input }';

procedure help;
begin
  Writeln('This sample demonstrates the use of the HoG descriptor.');
  Writeln(keys);
end;

function TDetector.modeName: string;
begin
  if m = Default then
    Result := 'Default'
  else
    Result := 'Daimler';
end;

procedure TDetector.toggleMode;
begin
  if m = Default then
    m := Daimler
  else
    m := Default;
end;

begin
  try
    help;

    Var
      cap: TVideoCapture;
    if (ParamCount > 0) then
    begin
      if (isIntNumber(ParamStr(1))) then
        cap.open(ParamStr(1).ToInteger)
      else if FileExists(ParamStr(1)) then
        cap.open(ParamStr(1))
    end
    else
      cap.open(0);

    if (not cap.isOpened()) then
    begin
      cout + 'Can not open video stream' + endl;
      Halt(2);
    end;

    cout + 'Press "q" or <ESC> to quit.' + endl;
    cout + 'Press <space> to toggle between Default and Daimler detector' + endl;
    Var
      detector: TDetector;
    Var
      frame: TMat;
    While true do
    begin
      cap > frame;
      if (frame.empty()) then
      begin
        cout + 'Finished reading: empty frame' + endl;
        break;
      end;
      Var
        t: int64 := getTickCount();
      Var
        found: vector<TRect> := detector.detect(frame);
      t := getTickCount() - t;

        // show the window
      begin
        var
          buf: String := 'Mode: ' + detector.modeName() + ' ||| ' + 'FPS: ' +
          Format('%2.2f',[(getTickFrequency() / t)]);
        putText(frame, buf, Point(10, 30), FONT_HERSHEY_PLAIN, 2.0, Scalar(0, 0, 255), 2, LINE_AA);
      end;
      for var i := 0 to found.Size - 1 do
      begin
        Var
          r: TRect := found[i];
        detector.adjustRect(r);
        rectangle(frame, r.tl(), r.br(), Scalar(0, 255, 0), 2);
      end;
      imshow('People detector', frame);

        // interact with user
      Var
        key: char := chr(waitKey(1));
      if (key = #27) or (key = 'q') then // ESC
      begin
        cout + 'Exit requested' + endl;
        break;
      end
      else if (key = ' ') then
        detector.toggleMode();
    end;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
