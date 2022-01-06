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
program facial_features;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  cpp.utils,
  cv.resource,
  cv.opencv;

procedure help;
begin
  Writeln( //
    'This file demonstrates facial feature points detection using Haarcascade classifiers.'#13#10 + //
    'The program detects a face and eyes, nose and mouth inside the face.'#13#10 + //
    'The code has been tested on the Japanese Female Facial Expression (JAFFE) database and found'#13#10 + //
    'to give reasonably accurate results.');

  Var
    ExeFileName: string := ExtractFileName(ParamStr(0));

  Writeln(   //
    #13#10 + //
    'USAGE: ', ExeFileName, ' [IMAGE] [FACE_CASCADE] [OPTIONS]'#13#10 + //
    'IMAGE'#13#10#9'Path to the image of a face taken as input.'#13#10 + //
    'FACE_CASCSDE'#13#10#9'Path to a haarcascade classifier for face detection.'#13#10 + //
    'OPTIONS: '#13#10'There are 3 options available which are described in detail. There must be a '#13#10 + //
    'space between the option and it''s argument (All three options accept arguments).'#13#10 + //
    #9'-eyes <eyes_cascade> : Specify the haarcascade classifier for eye detection.'#13#10 + //
    #9'-nose <nose_cascade> : Specify the haarcascade classifier for nose detection.'#13#10 + //
    #9'-mouth <mouth-cascade> : Specify the haarcascade classifier for mouth detection.');

  Writeln(             //
    'EXAMPLE:'#13#10 + //
    '(1) ', ExeFileName, ' image.jpg face.xml -eyes=eyes.xml -mouth=mouth.xml'#13#10 + //
    #9'This will detect the face, eyes and mouth in image.jpg.'#13#10 + //
    '(2) ', ExeFileName, ' image.jpg face.xml -nose=nose.xml'#13#10 + //
    #9'This will detect the face and nose in image.jpg.'#13#10 + //
    '(3) ', ExeFileName, ' image.jpg face.xml'#13#10 + //
    #9'This will detect only the face in image.jpg.');

  Writeln( //
    #13#10#13#10'The classifiers for face and eyes can be downloaded from : ' + //
    #13#10'https://github.com/opencv/opencv/tree/master/data/haarcascades');

  Writeln( //
    #13#10#13#10'The classifiers for nose and mouth can be downloaded from : ' + //
    #13#10'https://github.com/opencv/opencv_contrib/tree/master/modules/face/data/cascades');
end;

procedure detectFaces(const img: TMat; const faces: vector<TRect_<int>>; const cascade_path: string);
begin
  Var
    face_cascade: TCascadeClassifier;
  face_cascade.load(cascade_path);

  if (not face_cascade.empty()) then
    face_cascade.detectMultiScale(img, faces, 1.15, 3, 0 or CASCADE_SCALE_IMAGE, Size(30, 30));
end;

procedure detectEyes(const img: TMat; const eyes: vector<TRect_<int>>; cascade_path: string);
begin
  if not cascade_path.isempty then
  begin
    Var
      eyes_cascade: TCascadeClassifier;
    eyes_cascade.load(cascade_path);

    if (not eyes_cascade.empty()) then
      eyes_cascade.detectMultiScale(img, eyes, 1.20, 5, 0 or CASCADE_SCALE_IMAGE, Size(30, 30));
  end;
end;

procedure detectNose(const img: TMat; nose: vector<TRect_<int>>; cascade_path: string);
begin
  if not cascade_path.isempty then
  begin
    Var
      nose_cascade: TCascadeClassifier;
    nose_cascade.load(cascade_path);

    if (not nose_cascade.empty()) then
      nose_cascade.detectMultiScale(img, nose, 1.20, 5, 0 or CASCADE_SCALE_IMAGE, Size(30, 30));
  end;
end;

procedure detectMouth(const img: TMat; const mouth: vector<TRect_<int>>; cascade_path: string);
begin
  if not cascade_path.isempty then
  begin
    Var
      mouth_cascade: TCascadeClassifier;
    mouth_cascade.load(cascade_path);

    if (not mouth_cascade.empty()) then
      mouth_cascade.detectMultiScale(img, mouth, 1.20, 5, 0 or CASCADE_SCALE_IMAGE, Size(30, 30));
  end;
end;

procedure detectFacialFeaures(const img: TMat; const faces: vector<TRect_<int>>; eye_cascade: string; nose_cascade: string; mouth_cascade: string);
begin
  for Var i := 0 to faces.Size - 1 do
  begin
    // Mark the bounding box enclosing the face
    Var
      face: TRect := faces[i];
    rectangle(img, Point(face.x, face.y), Point(face.x + face.width, face.y + face.height), Scalar(255, 0, 0), 1, 4);

    // Eyes, nose and mouth will be detected inside the face (region of interest)
    Var
      ROI: TMat := img.Mat(cv.opencv.Rect(face.x, face.y, face.width, face.height));

      // Check if all features (eyes, nose and mouth) are being detected
    Var
      is_full_detection: bool := false;
    if ((not eye_cascade.isempty) and (not nose_cascade.isempty()) and (not mouth_cascade.isempty())) then
      is_full_detection := true;

    // Detect eyes if classifier provided by the user
    if (not eye_cascade.isempty()) then
    begin
      Var
        eyes: vector<TRect_<int>>;
      detectEyes(ROI, eyes, eye_cascade);

      // Mark points corresponding to the centre of the eyes
      for var j := 0 to eyes.Size() - 1 do
      begin
        Var
          e: TRect := eyes[j];
        circle(ROI, Point(e.x + e.width div 2, e.y + e.height div 2), 3, Scalar(0, 255, 0), -1, 8);
        (* rectangle(ROI, Point(e.x, e.y), Point(e.x+e.width, e.y+e.height),
          Scalar(0, 255, 0), 1, 4); *)
      end;
    end;

    // Detect nose if classifier provided by the user
    Var
      nose_center_height: double := 0.0;
    if (not nose_cascade.isempty()) then
    begin
      Var
        nose: vector<TRect_<int>>;
      detectNose(ROI, nose, nose_cascade);

      // Mark points corresponding to the centre (tip) of the nose
      for Var j := 0 to nose.Size() - 1 do
      begin
        Var
          n: TRect := nose[j];
        circle(ROI, Point(n.x + n.width div 2, n.y + n.height div 2), 3, Scalar(0, 255, 0), -1, 8);
        nose_center_height := (n.y + n.height / 2);
      end;
    end;

    // Detect mouth if classifier provided by the user
    Var
      mouth_center_height: double;
    if (not mouth_cascade.isempty()) then
    begin
      Var
        mouth: vector<TRect_<int>>;
      detectMouth(ROI, mouth, mouth_cascade);

      for Var j := 0 to mouth.Size() - 1 do
      begin
        Var
          m: TRect          := mouth[j];
        mouth_center_height := (m.y + m.height / 2);

        // The mouth should lie below the nose
        if ((is_full_detection) and (mouth_center_height > nose_center_height)) then
          rectangle(ROI, Point(m.x, m.y), Point(m.x + m.width, m.y + m.height), Scalar(0, 255, 0), 1, 4)
        else if ((is_full_detection) and (mouth_center_height <= nose_center_height)) then
          continue
        else
          rectangle(ROI, Point(m.x, m.y), Point(m.x + m.width, m.y + m.height), Scalar(0, 255, 0), 1, 4);
      end;
    end;

  end;

end;

Var
  input_image_path: string;
  face_cascade_path, eye_cascade_path, nose_cascade_path, mouth_cascade_path: string;

begin
  try
    if ParamCount = 0 then
    begin
      input_image_path  := OpenCVData + 'lena.jpg';
      face_cascade_path := OprnCVHaar + 'haarcascade_frontalface_alt.xml';
      eye_cascade_path  := OprnCVHaar + 'haarcascade_eye.xml';
      // nose_cascade_path:= OprnCVHaar+'';
      // mouth_cascade_path:= OprnCVHaar+'';
    end
    else if ParamCount < 2 then
    begin
      help;
      Halt(1);
    end
    else
    begin
      input_image_path  := ParamStr(1);
      face_cascade_path := ParamStr(2);
      Var
        i: integer := 3;
      While i <= ParamCount do
      begin
        var
          s: string := ParamStr(i);
        if SameText(s, '-eyes') then
        begin
          eye_cascade_path := ParamStr(i + 1);
          Inc(i);
        end
        else if SameText(s, '-nose') then
        begin
          nose_cascade_path := ParamStr(i + 1);
          Inc(i);
        end
        else if SameText(s, '-mouth') then
        begin
          mouth_cascade_path := ParamStr(i + 1);
          Inc(i);
        end
        else
        begin
          help;
          Halt(1);
        end;
      end;
    end;

    // Load image and cascade classifier files
    Var
      image: TMat;
    image := imread(input_image_path);

    // Detect faces and facial features
    Var
      faces: vector<TRect_<int>>;
    detectFaces(image, faces, face_cascade_path);
    detectFacialFeaures(image, faces, eye_cascade_path, nose_cascade_path, mouth_cascade_path);

    imshow('Result', image);

    waitKey(0);
  except
    on e: Exception do
    begin
      Writeln(e.ClassName, ': ', e.Message);
      Readln;
    end;
  end;

end.
