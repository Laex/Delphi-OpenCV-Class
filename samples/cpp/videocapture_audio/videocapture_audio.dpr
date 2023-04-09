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
program videocapture_audio;

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
      filename: string := 'C:\Users\All Users\Documents\Embarcadero\Studio\22.0\Samples\CPP\Mobile Snippets\PlayAudioFile\soundsample.mp3';
    if (ParamCount > 0) and FileExists(ParamStr(1)) then
      filename := ParamStr(1);

    Var
      frame: TMat;
    Var
      audioData: vector<vector<TMat>>;
    Var
      cap: TVideoCapture;
    Var
      params: vector<int> := [int(CAP_PROP_AUDIO_STREAM), 0, int(CAP_PROP_VIDEO_STREAM), -1, int(CAP_PROP_AUDIO_DATA_DEPTH), CV_16S];

    cap.open(filename, CAP_MSMF, params);
    if (not cap.isOpened()) then
    begin
      cerr + 'ERROR! Can''t to open file: ' + filename + endl;
      Halt(1);
    end;

    const
      audioBaseIndex: int = Trunc(cap.get(CAP_PROP_AUDIO_BASE_INDEX));
    const
      numberOfChannels: int = Trunc(cap.get(CAP_PROP_AUDIO_TOTAL_CHANNELS));
    cout + 'CAP_PROP_AUDIO_DATA_DEPTH: ' + String(depthToString(Trunc(cap.get(CAP_PROP_AUDIO_DATA_DEPTH)))) + endl;
    cout + 'CAP_PROP_AUDIO_SAMPLES_PER_SECOND: ' + cap.get(CAP_PROP_AUDIO_SAMPLES_PER_SECOND) + endl;
    cout + 'CAP_PROP_AUDIO_TOTAL_CHANNELS: ' + numberOfChannels + endl;
    cout + 'CAP_PROP_AUDIO_TOTAL_STREAMS: ' + cap.get(CAP_PROP_AUDIO_TOTAL_STREAMS) + endl;

    Var
      numberOfSamples: int := 0;
    audioData.resize(numberOfChannels);
    While True do
    begin
      if (cap.grab()) then
      begin
        for Var nCh: int := 0 to numberOfChannels - 1 do
        begin
          cap.retrieve(frame, audioBaseIndex + nCh);
          audioData[nCh].push_back(frame);
          numberOfSamples := numberOfSamples + frame.cols;
        end
      end
      else
        break;
    end;

    cout + 'Number of samples: ' + numberOfSamples + endl;

  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
