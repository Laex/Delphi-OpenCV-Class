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
program bgfg_segm;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

Type
  TPtrTBackgroundSubtractor = TPtr<TBackgroundSubtractor>;

begin
  try
    WriteLn('This sample demonstrates background segmentation.');
    WriteLn('Usage: ', argv[0], ' <params>');
    WriteLn(#9'-c or -camera <index>          - use video stream from camera (device index starting from 0), default 0');
    WriteLn(#9'-fn or -file_name <filename>   - use video file as input');
    WriteLn(#9'-m or -method <method>         - method: background subtraction algorithm (knn or mog2), default mog2');

    Var
      camera: Int := 0;
    Var
      filename: String;
    Var
      method: String := 'mog2';

    Var
      i: Int := 1;
    While i <= ParamCount do
    begin
      Var
        s: string := ParamStr(i);
      if ((s = '-c') or (s = '-camera')) and isIntNumber(ParamStr(i + 1)) then
      begin
        camera := ParamStr(i + 1).ToInteger;
        inc(i);
      end
      else if ((s = '-fn') or (s = '-file_name')) and FileExists(ParamStr(i + 1)) then
      begin
        filename := ParamStr(i + 1);
        inc(i);
      end
      else if ((s = '-m') or (s = '-method')) and ((ParamStr(i + 1) = 'knn') or (ParamStr(i + 1) = 'mog2')) then
      begin
        method := ParamStr(i + 1);
        inc(i);
      end
      else
        Halt(1);

      inc(i);
    end;

    Var
      cap: TVideoCapture;

    if (filename.Length = 0) then
      cap.open(camera)
    else
      cap.open(filename);
    if (not cap.isOpened()) then
    begin
      cout + 'Can not open video stream: ' + iif.iif<string>(filename.Length = 0, '<camera>', filename) + endl;
      Halt(2);
    end;

    Var
      model: TPtr<TBackgroundSubtractor>;
    if (method = 'knn') then
      model := TPtrTBackgroundSubtractor(createBackgroundSubtractorKNN())
    else if (method = 'mog2') then
      model := TPtrTBackgroundSubtractor(createBackgroundSubtractorMOG2());
    if (model = False) then
    begin
      cout + 'Can not create background model using provided method:  ' + method + endl;
      Halt(3);
    end;

    cout + 'Press <space> to toggle background model update' + endl;
    cout + 'Press ''s'' to toggle foreground mask smoothing' + endl;
    cout + 'Press ESC or ''q'' to exit' + endl;
    Var
      doUpdateModel: bool := true;
    Var
      doSmoothMask: bool := False;

    Var
      inputFrame, frame, foregroundMask, foreground, background: TMat;
    While true do
    begin
      // prepare input frame
      cap > inputFrame;
      if (inputFrame.empty()) then
      begin
        cout + 'Finished reading: empty frame' + endl;
        break;
      end;
      Var
        scaledSize: TSize := Size(640, 640 * inputFrame.rows div inputFrame.cols);
      resize(inputFrame, frame, scaledSize, 0, 0, INTER_LINEAR);

      // pass the frame to background model
      model.v.apply(frame, foregroundMask, iif.iif<Double>(doUpdateModel, -1, 0));

      // show processed frame
      imshow('image', frame);

      // show foreground image and mask (with optional smoothing)
      if (doSmoothMask) then
      begin
        GaussianBlur(foregroundMask, foregroundMask, Size(11, 11), 3.5, 3.5);
        threshold(foregroundMask, foregroundMask, 10, 255, THRESH_BINARY);
      end;
      if (foreground.empty()) then
        foreground.create(scaledSize, frame.&type);
      foreground.assign(TScalar.all(0));
      frame.copyTo(foreground, foregroundMask);
      imshow('foreground mask', foregroundMask);
      imshow('foreground image', foreground);

      // show background image
      model.v.getBackgroundImage(background);
      if (not background.empty()) then
        imshow('mean background image', background);

      // interact with user
      Var
        key: char := chr(waitKey(30));
      if (key = #27) or (key = 'q') then // ESC
      begin
        cout + 'Exit requested' + endl;
        break;
      end
      else if (key = ' ') then
      begin
        doUpdateModel := not doUpdateModel;
        cout + 'Toggle background update: ' + iif.iif<string>(doUpdateModel, 'ON', 'OFF') + endl;
      end
      else if (key = 's') then
      begin
        doSmoothMask := not doSmoothMask;
        cout + 'Toggle foreground mask smoothing: ' + iif.iif<string>(doSmoothMask, 'ON', 'OFF') + endl;
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
