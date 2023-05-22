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
program videocapture_basic;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  cv.resource,
  cpp.utils,
  cv.opencv;

begin
  try
    Var
      frame: TMat;
      // --- INITIALIZE VIDEOCAPTURE
    Var
      cap: TVideoCapture;
      // open the default camera using default API
      // cap.open(0);
      // OR advance usage: select any API backend
    Var
      deviceID: int := 0; // 0 = open default camera
    Var
      apiID: TVideoCaptureAPIs := CAP_ANY; // 0 = autodetect default API
      // open selected camera using selected API
    cap.open(deviceID, apiID);
    // check if we succeeded
    if (not cap.isOpened()) then
    begin
      Writeln('ERROR! Unable to open camera');
      Halt(1);
    end;

    // --- GRAB AND WRITE LOOP
    Writeln('Start grabbing');
    Writeln('Press any key to terminate');
    while True do
    begin
      // wait for a new frame from camera and store it into 'frame'
      cap.read(frame);
      // check if we succeeded
      if (frame.empty()) then
      begin
        Writeln('ERROR! blank frame grabbed');
        break;
      end;
      // show live and wait for a key with timeout long enough to show images
      imshow('Live', frame);
      if (waitKey(5) >= 0) then
        break;
    end;
    // the camera will be deinitialized automatically in VideoCapture destructor

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
