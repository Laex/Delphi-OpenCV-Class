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
program videocapture_microphone;

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
    Var
      audioData: vector<TMat>;
    Var
      cap: TVideoCapture;
    Var
      params: vector<int> := [CAP_PROP_AUDIO_STREAM, 0, CAP_PROP_VIDEO_STREAM, -1];

    cap.open(0, CAP_MSMF, params);
    if (not cap.isOpened()) then
    begin
      cerr + 'ERROR! Can''t to open microphone' + endl;
      Halt(1);
    end;

    const
      audioBaseIndex: int = Trunc(cap.get(CAP_PROP_AUDIO_BASE_INDEX));
    const
      numberOfChannels: int = Trunc(cap.get(CAP_PROP_AUDIO_TOTAL_CHANNELS));
    cout + 'CAP_PROP_AUDIO_DATA_DEPTH: ' + depthToString(Trunc(cap.get(CAP_PROP_AUDIO_DATA_DEPTH))) + endl;
    cout + 'CAP_PROP_AUDIO_SAMPLES_PER_SECOND: ' + cap.get(CAP_PROP_AUDIO_SAMPLES_PER_SECOND) + endl;
    cout + 'CAP_PROP_AUDIO_TOTAL_CHANNELS: ' + numberOfChannels + endl;
    cout + 'CAP_PROP_AUDIO_TOTAL_STREAMS: ' + cap.get(CAP_PROP_AUDIO_TOTAL_STREAMS) + endl;

    const
      cvTickFreq: double = getTickFrequency();
    var
      sysTimeCurr: int64 := getTickCount();
    Var
      sysTimePrev: int64 := sysTimeCurr;
    while ((sysTimeCurr - sysTimePrev) / cvTickFreq) < 10 do
    begin
      if (cap.grab()) then
      begin
        for Var nCh: int := 0 to numberOfChannels - 1 do
        begin
          cap.retrieve(frame, audioBaseIndex + nCh);
          audioData.push_back(frame);
          sysTimeCurr := getTickCount();
        end
      end
      else
      begin
        cerr + 'Grab error' + endl;
        break;
      end;
    end;
    Var
      numberOfSamles: int := 0;

      for var i: int := 0 to audioData.size - 1 do numberOfSamles := numberOfSamles + audioData[i].cols;

    cout + 'Number of samples: ' + numberOfSamles + endl;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
