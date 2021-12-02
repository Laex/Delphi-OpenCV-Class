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
program facedetect;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  CVResource,
  opencv_world;

procedure help;
begin
  WriteLn( //
    'This program demonstrates the use of cv::CascadeClassifier class to detect objects (Face + eyes). You can use Haar or LBP features.'#13#10 + //
    'This classifier can recognize many kinds of rigid objects, once the appropriate classifier is trained.'#13#10 + //
    'It''s most known use is for faces.'#13#10 + //
    'Usage:');
  WriteLn(ExtractFileName(ParamStr(0)));
  WriteLn( //
    '   [-cascade <cascade_path> this is the primary trained classifier such as frontal face]'#13#10 + //
    '   [-nested-cascade [nested_cascade_path this an optional secondary classifier such as eyes]]'#13#10 + //
    '   [-scale <image scale greater or equal to 1, try 1.3 for example>]'#13#10 + //
    '   [-try-flip]'#13#10 + //
    '   [filename|camera_index] - default camera with index 0'#13#10#13#10 + //
    'example:');
  WriteLn(ExtractFileName(ParamStr(0)));
  WriteLn( //
    '-cascade "data/haarcascades/haarcascade_frontalface_alt.xml" ' + //
    '-nested-cascade "data/haarcascades/haarcascade_eye_tree_eyeglasses.xml" ' + //
    '-scale 1.3'#13#10 + //
    'During execution:'#13#10 +      //
    #9'Hit any key to quit.'#13#10 + //
    #9'Using OpenCV version ', CV_VERSION);
end;

procedure detectAndDraw(const img: TMat; const cascade: TCascadeClassifier; const nestedCascade: TCascadeClassifier; scale: double; tryflip: bool);
begin
  Var
    t: double;
  Var
    gray, smallImg: TMat;
  Var
    faces, faces2: vector<TRect>;
  Var
    colors: TArray<TScalar> := [Scalar(255, 0, 0), Scalar(255, 128, 0), Scalar(255, 255, 0), Scalar(0, 255, 0), Scalar(0, 128, 255), Scalar(0, 255, 255), Scalar(0, 0, 255), Scalar(255, 0, 255)];

  cvtColor(img, gray, COLOR_BGR2GRAY);
  Var
    fx: double := 1 / scale;
  resize(gray, smallImg, TSize.Size(), fx, fx, INTER_LINEAR_EXACT);
  equalizeHist(smallImg, smallImg);

  t := getTickCount();
  cascade.detectMultiScale(smallImg, faces, 1.1, 2, 0
    // or CASCADE_FIND_BIGGEST_OBJECT //
    // or CASCADE_DO_ROUGH_SEARCH  //
    or CASCADE_SCALE_IMAGE, //
    Size(30, 30));

  if (tryflip) then
  begin
    flip(smallImg, smallImg, 1);
    cascade.detectMultiScale(smallImg, faces2, 1.1, 2, 0
      // or CASCADE_FIND_BIGGEST_OBJECT
      // or CASCADE_DO_ROUGH_SEARCH
      or CASCADE_SCALE_IMAGE, Size(30, 30));
    // for( vector<Rect>::const_iterator r = faces2.begin(); r != faces2.end(); ++r )
    for var i := 0 to faces2.Size - 1 do
      faces.push_back(Rect(smallImg.cols - faces2[i].x - faces2[i].width, faces2[i].y, faces2[i].width, faces2[i].height));
  end;
  t := getTickCount() - t;
  WriteLn('detection time = ', t * 1000 / getTickFrequency():1:8, ' ms');
  for Var i := 0 to faces.Size - 1 do
  begin
    Var
      r: TRect := faces[i];
    Var
      smallImgROI: TMat;
    Var
      nestedObjects: vector<TRect>;
    Var
      center: TPoint;
    Var
      color: TScalar := colors[i mod 8];
    Var
      radius: int;

    Var
      aspect_ratio: double := r.width / r.height;
    if (0.75 < aspect_ratio) and (aspect_ratio < 1.3) then
    begin
      center.x := cvRound((r.x + r.width * 0.5) * scale);
      center.y := cvRound((r.y + r.height * 0.5) * scale);
      radius   := cvRound((r.width + r.height) * 0.25 * scale);
      circle(img, center, radius, color, 3, LINE_8, 0);
    end
    else
      rectangle(img, Point(cvRound(r.x * scale), cvRound(r.y * scale)), Point(cvRound((r.x + r.width - 1) * scale), cvRound((r.y + r.height - 1) * scale)), color, 3, LINE_8, 0);
    if (nestedCascade.empty) then
      continue;
    smallImgROI := smallImg.Mat(r);
    nestedCascade.detectMultiScale(smallImgROI, nestedObjects, 1.1, 2, 0
      // or CASCADE_FIND_BIGGEST_OBJECT
      // or CASCADE_DO_ROUGH_SEARCH
      // or CASCADE_DO_CANNY_PRUNING
      or CASCADE_SCALE_IMAGE, Size(30, 30));
    for Var j := 0 to nestedObjects.Size - 1 do
    begin
      Var
        nr: TRect := nestedObjects[j];
      center.x    := cvRound((r.x + nr.x + nr.width * 0.5) * scale);
      center.y    := cvRound((r.y + nr.y + nr.height * 0.5) * scale);
      radius      := cvRound((nr.width + nr.height) * 0.25 * scale);
      circle(img, center, radius, color, 3, 8, 0);
    end;
  end;
  imshow('result', img);
end;

procedure perror(const ErrText: string);
begin
  WriteLn(ErrText);
  Halt(1);
end;

procedure DoImageFileName(const AFileName: String; const cascade: TCascadeClassifier; const nestedCascade: TCascadeClassifier; scale: double; tryflip: bool);
Var
  im: TMat;
begin
  WriteLn('Detecting face(s) in ', AFileName);
  im := imread(AFileName, IMREAD_COLOR);
  if im.empty then
    perror('ERROR: Could not load ' + AFileName);
  detectAndDraw(im, cascade, nestedCascade, scale, tryflip);
end;

procedure DoImageFileImageList(const AFileNameImageList: String; const cascade: TCascadeClassifier; const nestedCascade: TCascadeClassifier; scale: double; tryflip: bool);
Var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(AFileNameImageList);
    if S.Count = 0 then
      perror('ERROR: File ' + AFileNameImageList + ' is empty');
    for Var i := 0 to S.Count - 1 do
    begin
      DoImageFileName(S[i], cascade, nestedCascade, scale, tryflip);
      waitKey(0);
    end;
  finally
    S.Free;
  end;
end;

procedure DoCamera(const capture: TVideoCapture; const cascade: TCascadeClassifier; const nestedCascade: TCascadeClassifier; scale: double; tryflip: bool);
Var
  frame: TMat;
begin
  if (capture.isOpened()) then
    WriteLn('Video capturing has been started ...');

  While True do
  begin
    capture > frame;
    if (frame.empty()) then
      break;

    detectAndDraw(frame, cascade, nestedCascade, scale, tryflip);

    if waitKey(1) <> -1 then
      break;
  end;
end;

Var
  cascadeName: string       = OprnCVHaar + 'haarcascade_frontalface_alt.xml';
  nestedCascadeName: string = OprnCVHaar + 'haarcascade_eye_tree_eyeglasses.xml';

begin
  try
    help;

    Var
      capture: TVideoCapture;
    Var
      image: TMat;
    Var
      inputName: string := '0';
      // inputName: string := OpenCVData + 'Megamind.avi';
      // inputName: string := OpenCVData + 'lena.jpg';
    Var
      tryflip: bool := False;
    Var
      cascade, nestedCascade: TCascadeClassifier;
    Var
      scale: double := 1;

      // cv::CommandLineParser parser(argc, argv,
      // 'beginhelp h||end;'
      // 'begincascade|data/haarcascades/haarcascade_frontalface_alt.xml|end;'
      // 'beginnested-cascade|data/haarcascades/haarcascade_eye_tree_eyeglasses.xml|end;'
      // 'beginscale|1|end;begintry-flip||end;begin@filename||end;' );
    var
      i: Integer := 1;
    while i <= ParamCount do
    begin
      Var
        S: String := ParamStr(i);
      if SameText('h', S) or SameText('help', S) then
        Halt(1);
      if S[1] = '-' then
      begin
        if SameText('-cascade', S) then
        begin
          cascadeName := ParamStr(i + 1);
          if not FileExists(cascadeName) then
            Halt(1);
          inc(i);
        end
        else if SameText('-nested-cascade', S) then
        begin
          nestedCascadeName := ParamStr(i + 1);
          if not FileExists(nestedCascadeName) then
            Halt(1);
          inc(i);
        end
        else if SameText('-scale', S) then
        begin
          scale := ParamStr(i + 1).ToDouble;
          inc(i);
        end
        else if SameText('-try-flip', S) then
          tryflip := True
        else
          Halt(1);
      end
      else
        inputName := ParamStr(i);
      inc(i);
    end;

    if (cascadeName.Length = 0) then
      Halt(1);

    if not cascade.load(cascadeName) then
      perror('ERROR: Could not load classifier cascade');
    if (nestedCascadeName.Length > 0) and FileExists(nestedCascadeName) and (not nestedCascade.load(nestedCascadeName)) then
      WriteLn('WARNING: Could not load classifier cascade for nested objects');

    if FileExists(inputName) then
    begin
      if SameText('.txt', ExtractFileExt(inputName)) then // File images list
        DoImageFileImageList(inputName, cascade, nestedCascade, scale, tryflip)
      else if SameText('.jpg', ExtractFileExt(inputName)) then // JPG File
      begin
        DoImageFileName(inputName, cascade, nestedCascade, scale, tryflip);
        waitKey(0);
      end
      else if capture.open(inputName) then // VideoFileName
        DoCamera(capture, cascade, nestedCascade, scale, tryflip)
      else
        perror('ERROR: Could not open ' + inputName);
    end
    else
    begin
      Var
        CamIndex: Integer;
      if TryStrToInt(inputName, CamIndex) then
      begin
        if capture.open(inputName.ToInteger) then
          DoCamera(capture, cascade, nestedCascade, scale, tryflip)
        else
          perror('ERROR: Could not open camera #' + inputName);
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
