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

program clahe;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  system.SysUtils,
  cv.opencv,
  cpp.utils;

procedure Help;
begin
  WriteLn('Usage : clahe [options]');
  WriteLn('Available options:');
  WriteLn('{ i input    |                    | specify input image }');
  WriteLn('{ c camera   |  0                 | specify camera id   }');
  WriteLn('{ o output   | clahe_output.jpg   | specify output save path}');
  WriteLn('{ h help     |                    | print help message }');
  Halt;
end;

var
  pFilter: TPtr<TCLAHE>;
  tilesize, cliplimit: int;

procedure TSize_Callback(pos: int; data: pointer);
begin
  if (pos = 0) then
    pFilter.v.setTilesGridSize(Size(1, 1))
  else
    pFilter.v.setTilesGridSize(Size(tilesize, tilesize));
end;

procedure Clip_Callback(v: int; data: pointer);
begin
  pFilter.v.setClipLimit(cliplimit);
end;

Var
  infile: String;
  outfile: String = 'clahe_output.jpg';
  camid: int = 0;
  capture: TVideoCapture;
  frame, outframe: TMat;
  cur_clip: int;
  cur_tilesize: TSize;
  key: ansichar;
  p: string;

begin
  var
    i: int := 1;
  while i <= ParamCount do
  begin
    p := paramstr(i);
    if ((p = 'i') or (p = 'input')) then
    begin
      if (i < ParamCount) then
      begin
        inc(i);
        infile := paramstr(i);
        if not FileExists(infile) then
          Help;
      end
      else
        Help;
    end
    else if ((p = 'c') or (p = 'camera')) then
    begin
      if (i < ParamCount) and (TryStrToInt(paramstr(i + 1), camid)) then
        inc(i)
      else
        Help;
    end
    else if ((p = 'o') or (p = 'output')) then
    begin
      if (i < ParamCount) then
      begin
        inc(i);
        outfile := paramstr(i);
      end
      else
        Help;
    end
    else if ((p = 'h') or (p = 'help')) then
      Help;
    inc(i);
  end;

  namedWindow('CLAHE');
  createTrackbar('Tile Size', 'CLAHE', @tilesize, 32, TSize_Callback);
  createTrackbar('Clip Limit', 'CLAHE', @cliplimit, 20, Clip_Callback);

  pFilter := createCLAHE();

  cur_clip     := Trunc(pFilter.v.getClipLimit());
  cur_tilesize := pFilter.v.getTilesGridSize();
  setTrackbarPos('Tile Size', 'CLAHE', cur_tilesize.width);
  setTrackbarPos('Clip Limit', 'CLAHE', cur_clip);

  if not infile.IsEmpty then
  begin
    imread(infile).copyTo(frame);
    if frame.empty() then
    begin
      WriteLn('error read image: ', infile);
      Halt;
    end
  end
  else
    capture.open(camid);

  WriteLn('Controls:');
  WriteLn(#9'o - save output image');
  // WriteLn( #9'm - switch OpenCL <-> CPU mode');
  WriteLn(#9'ESC - exit\');

  While True do
  begin
    if (capture.isOpened()) then
      capture.read(frame)
    else
      imread(infile).copyTo(frame);
    if (frame.empty()) then
    begin
      waitKey();
      break;
    end;

    cvtColor(frame, frame, COLOR_BGR2GRAY);
    pFilter.v.apply(frame, outframe);

    imshow('CLAHE', outframe);

    key := ansichar(waitKey(3));
    if (key = 'o') then
    begin
      imwrite(outfile, outframe);
      WriteLn('Store frame to ', outfile);
    end
    else if (key = #27) then
      break
    else if (key = 'm') then
    begin
      // ocl::setUseOpenCL(!cv::ocl::useOpenCL());
      // cout << 'Switched to ' << (ocl::useOpenCL() ? 'OpenCL enabled' : 'CPU') << ' mode\n';
    end
  end;

end.
