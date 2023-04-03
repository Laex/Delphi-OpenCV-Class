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

{$IF defined(OPENCV_OBJDETECT_HPP) and (not defined(OPENCV_OBJDETECT_HPP_IMPL))}
{$DEFINE OPENCV_OBJDETECT_HPP_IMPL}
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
{$IF not defined(PACKAGE)}
  class_proc_CV_CASCADECLASSIFIER_detectMultiScale_2(Self, image, objects, scaleFactor, minNeighbors, flags, minSize, maxSize);
{$IFEND}
end;

function TCascadeClassifier.empty: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_CASCADECLASSIFIER_empty(Self);
{$IFEND}
end;

class operator TCascadeClassifier.Finalize(var Dest: TCascadeClassifier);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_CASCADECLASSIFIER(Dest);
{$IFEND}
end;

class operator TCascadeClassifier.Initialize(out Dest: TCascadeClassifier);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_CASCADECLASSIFIER_2(Dest);
{$IFEND}
end;

function TCascadeClassifier.load(const filename: CppString): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_CASCADECLASSIFIER_load(Self, filename);
{$IFEND}
end;

function TCascadeClassifier.load(const filename: String): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_CASCADECLASSIFIER_load(Self, CppString(filename));
{$IFEND}
end;

{ THOGDescriptor }

class operator THOGDescriptor.Initialize(out Dest: THOGDescriptor);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_HOGDESCRIPTOR_3(Dest);
{$IFEND}
end;

class operator THOGDescriptor.Finalize(var Dest: THOGDescriptor);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_HOGDESCRIPTOR(Dest);
{$IFEND}
end;

class function THOGDescriptor.HOGDescriptor(const _winSize: TSize; const _blockSize: TSize; const _blockStride: TSize; const _cellSize: TSize; _nbins: Int; _derivAperture: Int = 1; _winSigma: double = -1; _histogramNormType: HistogramNormType = L2Hys; _L2HysThreshold: double = 0.2; _gammaCorrection: BOOL = false; _nlevels: Int = DEFAULT_NLEVELS; _signedGradient: BOOL = false): THOGDescriptor;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_HOGDESCRIPTOR_2(Result, _winSize, _blockSize, _blockStride, _cellSize, _nbins, _derivAperture, _winSigma, _histogramNormType, _L2HysThreshold, _gammaCorrection, _nlevels, _signedGradient);
{$IFEND}
end;

class procedure THOGDescriptor.HOGDescriptor(Var R: THOGDescriptor; const _winSize: TSize; const _blockSize: TSize; const _blockStride: TSize; const _cellSize: TSize; _nbins: Int; _derivAperture: Int = 1; _winSigma: double = -1; _histogramNormType: HistogramNormType = L2Hys; _L2HysThreshold: double = 0.2; _gammaCorrection: BOOL = false; _nlevels: Int = DEFAULT_NLEVELS; _signedGradient: BOOL = false);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_HOGDESCRIPTOR_2(R, _winSize, _blockSize, _blockStride, _cellSize, _nbins, _derivAperture, _winSigma, _histogramNormType, _L2HysThreshold, _gammaCorrection, _nlevels, _signedGradient);
{$IFEND}
end;

procedure THOGDescriptor.setSVMDetector(const svmdetector: TInputArray);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_HOGDESCRIPTOR_setSVMDetector(Self, svmdetector);
{$IFEND}
end;

class function THOGDescriptor.getDefaultPeopleDetector(): vector<float>;
begin
{$IF not defined(PACKAGE)}
  Result := class_proc_CV_HOGDESCRIPTOR_getDefaultPeopleDetector();
{$IFEND}
end;

class function THOGDescriptor.getDaimlerPeopleDetector(): vector<float>;
begin
{$IF not defined(PACKAGE)}
  Result := class_proc_CV_HOGDESCRIPTOR_getDaimlerPeopleDetector();
{$IFEND}
end;

procedure THOGDescriptor.detectMultiScale(const img: TInputArray; const foundLocations: vector<TRect>; const foundWeights: vector<double>; hitThreshold: double { = 0 }; const winStride: TSize { = Size() }; const padding: TSize { = Size() }; scale: double = 1.05; finalThreshold: double = 2.0; useMeanshiftGrouping: BOOL = false);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_HOGDESCRIPTOR_detectMultiScale(Self, img, foundLocations, foundWeights, hitThreshold, winStride, padding, scale, finalThreshold, useMeanshiftGrouping);
{$IFEND}
end;

procedure THOGDescriptor.detectMultiScale(const img: TInputArray; const foundLocations: vector<TRect>; const foundWeights: vector<double>; hitThreshold: double { = 0 }; const winStride: TSize { = Size() } );
begin
  detectMultiScale(img, foundLocations, foundWeights, hitThreshold, winStride, size());
end;

procedure THOGDescriptor.detectMultiScale(const img: TInputArray; const foundLocations: vector<TRect>; const foundWeights: vector<double>);
begin
  detectMultiScale(img, foundLocations, foundWeights, 0, TSize.size());
end;

procedure THOGDescriptor.detectMultiScale(const img: TInputArray; const foundLocations: vector<TRect>; hitThreshold: double { = 0 }; const winStride: TSize { = Size() }; const padding: TSize { = Size() }; scale: double = 1.05; finalThreshold: double = 2.0; useMeanshiftGrouping: BOOL = false);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_HOGDESCRIPTOR_detectMultiScale_1(Self, img, foundLocations, hitThreshold, winStride, padding, scale, finalThreshold, useMeanshiftGrouping);
{$IFEND}
end;

procedure THOGDescriptor.detectMultiScale(const img: TInputArray; const foundLocations: vector<TRect>; hitThreshold: double { = 0 }; const winStride: TSize);
begin
  detectMultiScale(img, foundLocations, hitThreshold, winStride, size());
end;

procedure THOGDescriptor.detectMultiScale(const img: TInputArray; const foundLocations: vector<TRect>);
begin
  detectMultiScale(img, foundLocations, 0, size(), size());
end;

{ TQRCodeDetector }

class operator TQRCodeDetector.Initialize(out Dest: TQRCodeDetector);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_QRCODEDETECTOR_1(Dest);
{$IFEND}
end;

class operator TQRCodeDetector.Finalize(var Dest: TQRCodeDetector);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_QRCODEDETECTOR(Dest);
{$IFEND}
end;

function TQRCodeDetector.detectAndDecode(const img: TInputArray; const points: TOutputArray { =noArray() }; const straight_qrcode: TOutputArray { = noArray() } ): CppString;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_QRCODEDETECTOR_detectAndDecode(Self, img, points, straight_qrcode);
{$IFEND}
end;

function TQRCodeDetector.detectAndDecode(const img: TInputArray; const points: TOutputArray { =noArray() } ): CppString;
begin
  Result := detectAndDecode(img, points, TOutputArray.noArray);
end;

function TQRCodeDetector.detectAndDecode(const img: TInputArray): CppString;
begin
  Result := detectAndDecode(img, TOutputArray.noArray);
end;

function TQRCodeDetector.detect(const img: TInputArray; const points: TOutputArray): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_QRCODEDETECTOR_detect(Self, img, points);
{$ifend}
end;

function TQRCodeDetector.detectAndDecodeMulti(const img: TInputArray; const decoded_info: vector<CppString>; const points: TOutputArray { = noArray() }; const straight_qrcode: TOutputArrayOfArrays { = noArray() }
  ): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_QRCODEDETECTOR_detectAndDecodeMulti(Self, img, decoded_info, points, straight_qrcode);
{$ifend}
end;

function TQRCodeDetector.detectAndDecodeMulti(const img: TInputArray; const decoded_info: vector<CppString>; const points: TOutputArray { = noArray() }
  ): BOOL;
begin
  Result := detectAndDecodeMulti(img, decoded_info, points, TOutputArray.noArray);
end;

function TQRCodeDetector.detectAndDecodeMulti(const img: TInputArray; const decoded_info: vector<CppString>): BOOL;
begin
  Result := detectAndDecodeMulti(img, decoded_info, TOutputArray.noArray);
end;

function TQRCodeDetector.detectMulti(const img: TInputArray; const points: TOutputArray): BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_QRCODEDETECTOR_detectMulti(Self, img, points);
{$ifend}
end;

{$IFEND}
