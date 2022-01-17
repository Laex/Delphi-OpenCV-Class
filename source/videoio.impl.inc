(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the 'License');
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an 'AS IS' BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

{$IFDEF OPENCV_VIDEOIO_HPP}
{ TVideoCapture }

function TVideoCapture.&set(propId: VideoCaptureProperties; Value: double): BOOL;
begin
  Result := set_VideoCapture(Self, Int(propId), Value);
end;

class operator TVideoCapture.assign(var Dest: TVideoCapture; const [ref] Src: TVideoCapture);
begin
  Move(Src, Dest, SizeOf(Dest));
  if Src.isOpened then
    FillChar((@Src.Dummy)^, SizeOf(Dest.Dummy), 0);
end;

class operator TVideoCapture.Finalize(var Dest: TVideoCapture);
begin
  destructor_VideoCapture(Dest);
end;

function TVideoCapture.get(propId: VideoCaptureProperties): double;
begin
  Result := get_VideoCapture(Self, Int(propId));
end;

class operator TVideoCapture.Implicit(const filename: string): TVideoCapture;
begin
  Result.open(filename);
end;

class operator TVideoCapture.GreaterThan(const VideoCapture: TVideoCapture; var frame: TMat): BOOL;
begin
  Result := VideoCapture.read(frame);
end;

class operator TVideoCapture.Implicit(const index: Int): TVideoCapture;
begin
  Result.open(index);
end;

class function TVideoCapture.Capture(const index: Int; const apiPreference: VideoCaptureAPIs = CAP_ANY): TVideoCapture;
begin
  Result.open(index, apiPreference);
end;

class operator TVideoCapture.Initialize(out Dest: TVideoCapture);
begin
  constructor_VideoCapture(Dest);
end;

function TVideoCapture.isOpened: BOOL;
begin
  Result := isOpened_VideoCapture(Self);
end;

function TVideoCapture.open(const filename: String; const apiPreference: VideoCaptureAPIs; const params: Vector<Int>): BOOL;
begin
  try
    Result := open_VideoCapture(Self, filename, Int(apiPreference), params);
  except
    Result := False;
  end;
end;

function TVideoCapture.open(const filename: String; const apiPreference: VideoCaptureAPIs): BOOL;
begin
  try
    Result := open_VideoCapture(Self, filename, Int(apiPreference));
  except
    Result := False;
  end;
end;

function TVideoCapture.open(const index: Int; const apiPreference: VideoCaptureAPIs; const params:vector<int>): BOOL;
begin
  try
    Result := open_VideoCapture(Self, index, apiPreference, params);
  except
    Result := False;
  end;
end;

function TVideoCapture.open(const index: Int; const apiPreference: VideoCaptureAPIs): BOOL;
begin
  try
    Result := open_VideoCapture(Self, index, Int(apiPreference));
  except
    Result := False;
  end;
end;

function TVideoCapture.read(const image: TOutputArray): BOOL;
begin
  Result := read_VideoCapture(Self, image);
end;

function TVideoCapture.grab(): BOOL;
begin
  Result := grab_VideoCapture(Self);
end;

function TVideoCapture.retrieve(const image: TOutputArray; flag: Int = 0): BOOL;
begin
  Result := retrieve_VideoCapture(Self, image, flag);
end;

procedure TVideoCapture.release;
begin
  release_VideoCapture(Self);
end;

{ TVideoWriter }

class operator TVideoWriter.assign(var Dest: TVideoWriter; const [ref] Src: TVideoWriter);
begin
  Move(Src, Dest, SizeOf(Dest));
  if Src.isOpened then
    FillChar((@Src.Dummy)^, SizeOf(Dest.Dummy), 0);
end;

class operator TVideoWriter.Finalize(var Dest: TVideoWriter);
begin
  destructor_VideoWriter(Dest);
end;

class operator TVideoWriter.LessThan(const VideoWriter: TVideoWriter; const frame: TMat): BOOL;
begin
  VideoWriter.write(frame);
  Result := true;
end;

class operator TVideoWriter.Initialize(out Dest: TVideoWriter);
begin
  constructor_VideoWriter(Dest);
end;

function TVideoWriter.isOpened: BOOL;
begin
  Result := isOpened_VideoWriter(Self);
end;

function TVideoWriter.open(const filename: String; fourcc: Int; fps: double; const frameSize: TSize; isColor: BOOL): BOOL;
begin
  Result := open_VideoWriter(Self, filename, fourcc, fps, frameSize, isColor);
end;

procedure TVideoWriter.release;
begin
  release_VideoWriter(Self);
end;

procedure TVideoWriter.write(const image: TInputArray);
begin
  write_VideoWriter(Self, image);
end;

{$ENDIF}
