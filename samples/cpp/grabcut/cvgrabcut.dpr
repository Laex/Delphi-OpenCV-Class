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
program cvgrabcut;

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
  cout + //
    '\nThis program demonstrates GrabCut segmentation -- select an object in a region\n' + //
    'and then grabcut will attempt to segment it out.\n' + //
    'Call:\n' +                                            //
    argv[0] + ' <image_name>\n' +                          //
    '\nSelect a rectangular area around the object you want to segment\n' + //
    '\nHot keys: \n' +                                     //
    '\tESC - quit the program\n' +                         //
    '\tr - restore the original image\n' +                 //
    '\tn - next iteration\n' +                             //
    '\n' +                                                 //
    '\tleft mouse button - set rectangle\n' +              //
    '\n' +                                                 //
    '\tCTRL+left mouse button - set GC_BGD pixels\n' +     //
    '\tSHIFT+left mouse button - set GC_FGD pixels\n' +    //
    '\n' +                                                 //
    '\tCTRL+right mouse button - set GC_PR_BGD pixels\n' + //
    '\tSHIFT+right mouse button - set GC_PR_FGD pixels\n' + endl;
end;

Var
  RED: TScalar;
  PINK: TScalar;
  BLUE: TScalar;
  LIGHTBLUE: TScalar;
  GREEN: TScalar;

  BGD_KEY: MouseEventFlags = EVENT_FLAG_CTRLKEY;
  FGD_KEY: MouseEventFlags = EVENT_FLAG_SHIFTKEY;

procedure getBinMask(const comMask: TMat; Var binMask: TMat);
begin
  if (comMask.empty()) or (comMask.&type <> CV_8UC1) then
  begin
    WriteLn('Error: comMask is empty or has incorrect type (not CV_8UC1)');
    Halt(1);
  end;
  if (binMask.empty()) or (binMask.rows <> comMask.rows) or (binMask.cols <> comMask.cols) then
    binMask.create(comMask.size, CV_8UC1);
  binMask := comMask and 1;
end;

Type
  pGCApplication = ^TGCApplication;

  TGCApplication = record
  public const
    NOT_SET        = 0;
    IN_PROCESS     = 1;
    &SET           = 2;
    radius: int    = 2;
    thickness: int = -1;
  public
    procedure reset();
    procedure setImageAndWinName(const _image: TMat; const _winName: string);
    procedure showImage();
    procedure mouseClick(event: int; x, y, flags: int; param: pointer);
    function nextIter(): int;
    function getIterCount(): int; inline;
  private
    procedure setRectInMask();
    procedure setLblsInMask(flags: int; p: TPoint; isPr: bool);
  private
    winName: string;
    image: TMat;
    mask: TMat;
    bgdModel, fgdModel: TMat;

    rectState, lblsState, prLblsState: uchar;
    isInitialized: bool;

    r: TRect;
    fgdPxls, bgdPxls, prFgdPxls, prBgdPxls: vector<TPoint>;
    iterCount: int;
  end;

Var
  gcapp: TGCApplication;

  { TGCApplication }

function TGCApplication.getIterCount: int;
begin
  Result := iterCount;
end;

procedure TGCApplication.mouseClick(event, x, y, flags: int; param: pointer);
// Var
// p: pGCApplication;
begin
  // p := param;
  // TODO add bad args check
  case MouseEventTypes(event) of
    EVENT_LBUTTONDOWN: // set rect or GC_BGD(GC_FGD) labels
      begin
        Var
          isb: bool := (flags and int(BGD_KEY)) <> 0;
        Var
          isf: bool := (flags and int(FGD_KEY)) <> 0;
        if (rectState = NOT_SET) and (not isb) and (not isf) then
        begin
          rectState := IN_PROCESS;
          r := Rect(x, y, 1, 1);
        end;
        if ((isb) or (isf)) and (rectState = &SET) then
          lblsState := IN_PROCESS;
      end;
    EVENT_RBUTTONDOWN: // set GC_PR_BGD(GC_PR_FGD) labels
      begin
        Var
          isb: bool := (flags and int(BGD_KEY)) <> 0;
        Var
          isf: bool := (flags and int(FGD_KEY)) <> 0;
        if ((isb) or (isf)) and (rectState = &SET) then
          prLblsState := IN_PROCESS;
      end;
    EVENT_LBUTTONUP:
      begin
        if (rectState = IN_PROCESS) then
        begin
          if (r.x = x) or (r.y = y) then
            rectState := NOT_SET
          else
          begin
            r := Rect(Point(r.x, r.y), Point(x, y));
            rectState := &SET;
            setRectInMask();
            Assert(bgdPxls.empty() and fgdPxls.empty() and prBgdPxls.empty() and prFgdPxls.empty());
          end;
          showImage();
        end;
        if (lblsState = IN_PROCESS) then
        begin
          setLblsInMask(flags, Point(x, y), false);
          lblsState := &SET;
          nextIter();
          showImage();
        end
        else
        begin
          if (rectState = &SET) then
          begin
            nextIter();
            showImage();
          end;
        end;
      end;
    EVENT_RBUTTONUP:
      begin
        if (prLblsState = IN_PROCESS) then
        begin
          setLblsInMask(flags, Point(x, y), true);
          prLblsState := &SET;
        end;
        if (rectState = &SET) then
        begin
          nextIter();
          showImage();
        end;
      end;
    EVENT_MOUSEMOVE:
      begin
        if (rectState = IN_PROCESS) then
        begin
          r := Rect(Point(r.x, r.y), Point(x, y));
          Assert(bgdPxls.empty() and fgdPxls.empty() and prBgdPxls.empty() and prFgdPxls.empty());
          showImage();
        end
        else if (lblsState = IN_PROCESS) then
        begin
          setLblsInMask(flags, Point(x, y), false);
          showImage();
        end
        else if (prLblsState = IN_PROCESS) then
        begin
          setLblsInMask(flags, Point(x, y), true);
          showImage();
        end
      end;
  end;
end;

function TGCApplication.nextIter: int;
begin
  if (isInitialized) then
    grabCut(image, mask, r, bgdModel, fgdModel, 1)
  else
  begin
    if (rectState <> &SET) then
      Exit(iterCount);

    if (lblsState = &SET) or (prLblsState = &SET) then
      grabCut(image, mask, r, bgdModel, fgdModel, 1, GC_INIT_WITH_MASK)
    else
      grabCut(image, mask, r, bgdModel, fgdModel, 1, GC_INIT_WITH_RECT);

    isInitialized := true;
  end;
  Inc(iterCount);

  bgdPxls.clear();
  fgdPxls.clear();
  prBgdPxls.clear();
  prFgdPxls.clear();

  Result := iterCount;
end;

procedure TGCApplication.reset;
begin
  if (not mask.empty()) then
    mask.setTo(TScalar.all(int(GC_BGD)));
  bgdPxls.clear();
  fgdPxls.clear();
  prBgdPxls.clear();
  prFgdPxls.clear();

  isInitialized := false;
  rectState := NOT_SET;
  lblsState := NOT_SET;
  prLblsState := NOT_SET;
  iterCount := 0;
end;

procedure TGCApplication.setImageAndWinName(const _image: TMat; const _winName: string);
begin
  if (_image.empty()) or (_winName.Length = 0) then
    Exit;
  image := _image;
  winName := _winName;
  mask.create(image.size, CV_8UC1);
  reset();
end;

procedure TGCApplication.setLblsInMask(flags: int; p: TPoint; isPr: bool);
Type
  TVectorOfTPoint = vector<TPoint>;
  pVectorOfTPoint = ^TVectorOfTPoint;
begin
  Var
    bpxls, fpxls: pVectorOfTPoint;
  Var
    bvalue, fvalue: uchar;
  if (not isPr) then
  begin
    bpxls := @bgdPxls;
    fpxls := @fgdPxls;
    bvalue := uchar(GC_BGD);
    fvalue := uchar(GC_FGD);
  end
  else
  begin
    bpxls := @prBgdPxls;
    fpxls := @prFgdPxls;
    bvalue := uchar(GC_PR_BGD);
    fvalue := uchar(GC_PR_FGD);
  end;
  if (flags and int(BGD_KEY)) <> 0 then
  begin
    bpxls^.push_back(p);
    circle(mask, p, radius, bvalue, thickness);
  end;
  if (flags and int(FGD_KEY)) <> 0 then
  begin
    fpxls^.push_back(p);
    circle(mask, p, radius, fvalue, thickness);
  end;
end;

procedure TGCApplication.setRectInMask;
begin
  Assert(not mask.empty());
  mask.setTo(int(GC_BGD));
  r.x := cvMax(0, r.x);
  r.y := cvMax(0, r.y);
  r.width := cvMin(r.width, image.cols - r.x);
  r.height := cvMin(r.height, image.rows - r.y);
  mask.mat(r).setTo(Scalar(int(GC_PR_FGD)));
end;

procedure TGCApplication.showImage;
begin
  if (image.empty()) or (winName.Length = 0) then
    Exit;

  Var
    res: TMat;
  Var
    binMask: TMat;
  image.copyTo(res);
  if (isInitialized) then
  begin
    getBinMask(mask, binMask);

    Var
      black: TMat := TMat.mat(binMask.rows, binMask.cols, CV_8UC3, Scalar(0, 0, 0));
    black.setTo(TScalar.all(255), binMask);

    addWeighted(black, 0.5, res, 0.5, 0.0, res);
  end;

  // vector<Point>::const_iterator it;
  // for( it = bgdPxls.begin(); it != bgdPxls.end(); ++it )
  for Var i := 0 to bgdPxls.size - 1 do
    circle(res, bgdPxls[i], radius, BLUE, thickness);
  // for( it = fgdPxls.begin(); it != fgdPxls.end(); ++it )
  for Var i := 0 to fgdPxls.size - 1 do
    circle(res, fgdPxls[i], radius, RED, thickness);
  // for( it = prBgdPxls.begin(); it != prBgdPxls.end(); ++it )
  for Var i := 0 to prBgdPxls.size - 1 do
    circle(res, prBgdPxls[i], radius, LIGHTBLUE, thickness);
  // for( it = prFgdPxls.begin(); it != prFgdPxls.end(); ++it )
  for Var i := 0 to prFgdPxls.size - 1 do
    circle(res, prFgdPxls[i], radius, PINK, thickness);

  if (rectState = IN_PROCESS) or (rectState = &SET) then
    rectangle(res, Point(r.x, r.y), Point(r.x + r.width, r.y + r.height), GREEN, 2);

  imshow(winName, res);
end;

procedure on_mouse(event, x, y, flags: int; param: pointer);
begin
  gcapp.mouseClick(event, x, y, flags, param);
end;

begin
  try
    help;

    RED := Scalar(0, 0, 255);
    PINK := Scalar(230, 130, 255);
    BLUE := Scalar(255, 0, 0);
    LIGHTBLUE := Scalar(255, 255, 160);
    GREEN := Scalar(0, 255, 0);

    Var
      filename: string := OpenCVData + 'messi5.jpg';
    if (ParamCount > 0) and FileExists(ParamStr(1)) then
      filename := ParamStr(1);

    Var
      image: Tmat := imread(filename, IMREAD_COLOR);
    if (image.empty()) then
    begin
      cout + '\n Durn, couldn''t read image filename ' + filename + endl;
      Halt(1);
    end;

    var
      winName: string := 'image';
    namedWindow(winName, WINDOW_AUTOSIZE);
    setMouseCallback(winName, on_mouse, nil);

    gcapp.setImageAndWinName(image, winName);
    gcapp.showImage();

    While true do
    begin
      Var
        c: char := Chr(waitKey(0));
      case c of
        #27:
          begin
            cout + 'Exiting ...' + endl;
            break;
          end;
        'r':
          begin
            cout + endl;
            gcapp.reset();
            gcapp.showImage();
          end;
        'n':
          begin
            Var
              iterCount: int := gcapp.getIterCount();
            cout + '<' + iterCount + '... ';
            Var
              newIterCount: int := gcapp.nextIter();
            if (newIterCount > iterCount) then
            begin
              gcapp.showImage();
              cout + iterCount + '>' + endl;
            end
            else
              cout + 'rect must be determined>' + endl;
          end;
      end;
    end;

    destroyWindow(winName);

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
