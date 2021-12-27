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
program cvinpaint;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinApi.Windows,
  System.SysUtils,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help;
begin
  cout + '\nCool inpainging demo. Inpainting repairs damage to images by floodfilling the damage \n' + //
    'with surrounding image areas.\n' + //
    'Using OpenCV version %s\n' + CV_VERSION + '\n' + //
    'Usage:\n' + argv[0] + ' [image_name -- Default fruits.jpg]\n' + endl;

  cout + 'Hot keys: \n' +                         //
    '\tESC - quit the program\n' +                //
    '\tr - restore the original image\n' +        //
    '\ti or SPACE - run inpainting algorithm\n' + //
    '\t\t(before running it, paint something on the image)\n' + endl;
end;

Var
  img, inpaintMask: TMat;
  prevPt: TPoint;

procedure onMouse(event: MouseEventTypes; x, y, flags: int; p: pointer);
begin
  if (event = EVENT_LBUTTONUP) or ((flags and int(EVENT_FLAG_LBUTTON)) = 0) then
    prevPt := Point(-1, -1)
  else if (event = EVENT_LBUTTONDOWN) then
    prevPt := Point(x, y)
  else if (event = EVENT_MOUSEMOVE) and ((flags and int(EVENT_FLAG_LBUTTON)) <> 0) then
  begin
    Var
      pt: TPoint := Point(x, y);
    if (prevPt.x < 0) then
      prevPt := pt;
    line(inpaintMask, prevPt, pt, TScalar.all(255), 5, 8, 0);
    line(img, prevPt, pt, TScalar.all(255), 5, 8, 0);
    prevPt := pt;
    imshow('image', img);
  end;
end;

begin
  try
    help;

    prevPt := Point(-1, -1);

    Var
      filename: string := OpenCVData + 'fruits.jpg';
    if (ParamCount > 0) and (FileExists(ParamStr(1))) then
      filename := ParamStr(1);
    Var
      img0: TMat := imread(filename, IMREAD_COLOR);
    if (img0.empty()) then
    begin
      cout + 'Couldn''t open the image ' + filename + '. Usage: inpaint <image_name>\n' + endl;
      Halt(0);
    end;

    namedWindow('image', WINDOW_AUTOSIZE);

    img := img0.clone();
    inpaintMask := TMat.zeros(img.size, CV_8U);

    imshow('image', img);
    setMouseCallback('image', onMouse, nil);

    While True do
    begin
      Var
        c: char := chr(waitKey());

      if (c = #27) then
        break;

      if (c = 'r') then
      begin
        inpaintMask.assign(TScalar.all(0));
        img0.copyTo(img);
        imshow('image', img);
      end;

      if (c = 'i') or (c = ' ') then
      begin
        Var
          inpainted: TMat;
        inpaint(img, inpaintMask, inpainted, 3, INPAINT_TELEA);
        imshow('inpainted image', inpainted);
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
