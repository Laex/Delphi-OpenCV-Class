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

{$IF defined(OPENCV_VIDEOIO_HPP) and (not defined(OPENCV_VIDEOIO_HPP_IMPL))}
{$DEFINE OPENCV_VIDEOIO_HPP_IMPL}
{ TVideoCapture }

function TVideoCapture.&set(propId: VideoCaptureProperties; Value: double): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOCAPTURE_set(Self, Int(propId), Value);
{$IFEND}
end;

class operator TVideoCapture.assign(var Dest: TVideoCapture; const [ref] Src: TVideoCapture);
begin
  Move(Src, Dest, SizeOf(Dest));
  if Src.isOpened then
    FillChar((@Src.Dummy)^, SizeOf(Dest.Dummy), 0);
end;

class operator TVideoCapture.Finalize(var Dest: TVideoCapture);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_VIDEOCAPTURE(Dest);
{$IFEND}
end;

function TVideoCapture.get(propId: VideoCaptureProperties): double;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOCAPTURE_get(Self, Int(propId));
{$IFEND}
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

class function TVideoCapture.Capture(const index: Int; const apiPreference: TVideoCaptureAPIs = CAP_ANY): TVideoCapture;
begin
  Result.open(index, apiPreference);
end;

class operator TVideoCapture.Initialize(out Dest: TVideoCapture);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_VIDEOCAPTURE_5(Dest);
{$IFEND}
end;

function TVideoCapture.isOpened: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOCAPTURE_isOpened(Self);
{$IFEND}
end;

function TVideoCapture.open(const filename: String; const apiPreference: TVideoCaptureAPIs; const params: Vector<Int>): BOOL;
begin
{$IF not defined(PACKAGE)}
  try
    Result := class_func_CV_VIDEOCAPTURE_open_1(Self, filename, Int(apiPreference), params);
  except
    Result := False;
  end;
{$IFEND}
end;

function TVideoCapture.open(const filename: String; const apiPreference: TVideoCaptureAPIs): BOOL;
begin
{$IF not defined(PACKAGE)}
  try
    Result := class_func_CV_VIDEOCAPTURE_open(Self, filename, Int(apiPreference));
  except
    Result := False;
  end;
{$IFEND}
end;

function TVideoCapture.open(const index: Int; const apiPreference: TVideoCaptureAPIs; const params: Vector<Int>): BOOL;
begin
{$IF not defined(PACKAGE)}
  try
    Result := class_func_CV_VIDEOCAPTURE_open_3(Self, index, Int(apiPreference), params);
  except
    Result := False;
  end;
{$IFEND}
end;

function TVideoCapture.open(const index: Int; const apiPreference: TVideoCaptureAPIs): BOOL;
begin
{$IF not defined(PACKAGE)}
  try
    Result := class_func_CV_VIDEOCAPTURE_open_2(Self, index, Int(apiPreference));
  except
    Result := False;
  end;
{$IFEND}
end;

function TVideoCapture.read(const image: TOutputArray): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOCAPTURE_read(Self, image);
{$IFEND}
end;

function TVideoCapture.grab(): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOCAPTURE_grab(Self);
{$IFEND}
end;

function TVideoCapture.retrieve(const image: TOutputArray; flag: Int = 0): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOCAPTURE_retrieve(Self, image, flag);
{$IFEND}
end;

procedure TVideoCapture.release;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_VIDEOCAPTURE_release(Self);
{$IFEND}
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
{$IF not defined(PACKAGE)}
  destructor_CV_VIDEOWRITER(Dest);
{$IFEND}
end;

class operator TVideoWriter.LessThan(const VideoWriter: TVideoWriter; const frame: TMat): BOOL;
begin
  VideoWriter.write(frame);
  Result := true;
end;

class operator TVideoWriter.Initialize(out Dest: TVideoWriter);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_VIDEOWRITER_5(Dest);
{$IFEND}
end;

function TVideoWriter.isOpened: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOWRITER_isOpened(Self);
{$IFEND}
end;

function TVideoWriter.open(const filename: String; fourcc: Int; fps: double; const frameSize: TSize; isColor: BOOL): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_VIDEOWRITER_open_3(Self, filename, fourcc, fps, frameSize, isColor);
{$IFEND}
end;

procedure TVideoWriter.release;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_VIDEOWRITER_release(Self);
{$IFEND}
end;

procedure TVideoWriter.write(const image: TInputArray);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_VIDEOWRITER_write(Self, image);
{$IFEND}
end;

{$IFEND}
