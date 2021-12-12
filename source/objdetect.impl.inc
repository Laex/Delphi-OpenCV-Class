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

{$ifdef OPENCV_OBJDETECT_HPP}

{ TCascadeClassifier }

procedure TCascadeClassifier.detectMultiScale(const image: TInputArray; const objects: vector<TRect>; scaleFactor: double; minNeighbors, flags: Int);
begin
  detectMultiScale(image, objects, scaleFactor, minNeighbors, flags, size(0, 0));
end;

procedure TCascadeClassifier.detectMultiScale(const image: TInputArray; const objects: vector<TRect>; scaleFactor: double; minNeighbors, flags: Int; const minSize: TSize);
begin
  detectMultiScale(image, objects, scaleFactor, minNeighbors, flags, minSize, size(0, 0));
end;

procedure TCascadeClassifier.detectMultiScale(const image: TInputArray; const objects: vector<TRect>; scaleFactor: double; minNeighbors, flags: Int; const minSize, maxSize: TSize);
begin
  detectMultiScale_CascadeClassifier(Self, image, objects, scaleFactor, minNeighbors, flags, minSize, maxSize);
end;

function TCascadeClassifier.empty: BOOL;
begin
  Result := empty_CascadeClassifier(Self);
end;

class operator TCascadeClassifier.Finalize(var Dest: TCascadeClassifier);
begin
  Destructor_CascadeClassifier(Dest);
end;

class operator TCascadeClassifier.Initialize(out Dest: TCascadeClassifier);
begin
  Constructor_CascadeClassifier(Dest);
end;

function TCascadeClassifier.load(const filename: CppString): BOOL;
begin
  Result := load_CascadeClassifier(Self, filename);
end;

function TCascadeClassifier.load(const filename: String): BOOL;
begin
  Result := load_CascadeClassifier(Self, CppString(filename));
end;

{$ENDIF}