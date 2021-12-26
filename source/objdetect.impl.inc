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

{$IFDEF OPENCV_OBJDETECT_HPP}
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

{ THOGDescriptor }

class operator THOGDescriptor.Initialize(out Dest: THOGDescriptor);
begin
  constructor_HOGDescriptor(Dest);
end;

class operator THOGDescriptor.Finalize(var Dest: THOGDescriptor);
begin
  destructor_HOGDescriptor(Dest);
end;

class function THOGDescriptor.HOGDescriptor(
               const _winSize: TSize;
               const _blockSize: TSize;
               const _blockStride: TSize;
               const _cellSize: TSize;
               _nbins: Int;
               _derivAperture: Int = 1;
               _winSigma: double = -1;
               _histogramNormType: HistogramNormType = L2Hys;
               _L2HysThreshold: double = 0.2;
               _gammaCorrection: BOOL = false;
               _nlevels: Int = DEFAULT_NLEVELS;
               _signedGradient: BOOL = false): THOGDescriptor;
begin
  constructor_HOGDescriptor(Result, _winSize, _blockSize, _blockStride, _cellSize, _nbins, _derivAperture, _winSigma, _histogramNormType, _L2HysThreshold, _gammaCorrection, _nlevels, _signedGradient);
end;

class procedure THOGDescriptor.HOGDescriptor(Var R:THOGDescriptor;
                  const _winSize:TSize;const _blockSize:TSize;const _blockStride:TSize;
                  const _cellSize:TSize; _nbins:int; _derivAperture:int=1; _winSigma:double=-1;
                  _histogramNormType:HistogramNormType=L2Hys;
                  _L2HysThreshold:double=0.2; _gammaCorrection:bool=false;
                  _nlevels:int=DEFAULT_NLEVELS; _signedGradient:bool=false);
begin
  constructor_HOGDescriptor(R, _winSize, _blockSize, _blockStride, _cellSize, _nbins, _derivAperture, _winSigma, _histogramNormType, _L2HysThreshold, _gammaCorrection, _nlevels, _signedGradient);
end;

procedure THOGDescriptor.setSVMDetector(const svmdetector: TInputArray);
begin
  setSVMDetector_HOGDescriptor(Self, svmdetector);
end;

class function THOGDescriptor.getDefaultPeopleDetector(): vector<float>;
begin
  Result := getDefaultPeopleDetector_HOGDescriptor( { Self } );
end;

class function THOGDescriptor.getDaimlerPeopleDetector(): vector<float>;
begin
  Result := getDaimlerPeopleDetector_HOGDescriptor( { Self } );
end;

procedure THOGDescriptor.detectMultiScale( const img: TInputArray;
                                           const foundLocations: vector<TRect>;
                                           const foundWeights: vector<double>;
                                           hitThreshold: double {= 0};
                                           const winStride: TSize { = Size() };
                                           const padding: TSize { = Size() };
                                           scale: double = 1.05;
                                           finalThreshold: double = 2.0;
                                           useMeanshiftGrouping: BOOL = false);
begin
 detectMultiScale_HOGDescriptor(
             Self,
             img,
             foundLocations,
             foundWeights,
             hitThreshold,
             winStride,
             padding,
             scale,
             finalThreshold,
             useMeanshiftGrouping);
end;

procedure THOGDescriptor.detectMultiScale( const img: TInputArray;
                                           const foundLocations: vector<TRect>;
                                           const foundWeights: vector<double>;
                                           hitThreshold: double {= 0};
                                           const winStride: TSize { = Size() } );
begin
  detectMultiScale(img,foundLocations,foundWeights,hitThreshold,winStride,Size());
end;

procedure THOGDescriptor.detectMultiScale( const img: TInputArray;
                                           const foundLocations: vector<TRect>;
                                           const foundWeights: vector<double>);
begin
  detectMultiScale(img,foundLocations,foundWeights,0,TSize.Size());
end;

procedure THOGDescriptor.detectMultiScale(
                                const img:TInputArray;
                                const foundLocations:vector<TRect>;
                                hitThreshold :double {= 0};
                                const winStride :TSize {= Size()};
                                const padding :TSize {= Size()};
                                scale :double = 1.05;
                                finalThreshold :double = 2.0;
                                useMeanshiftGrouping :bool = false);
begin
  detectMultiScale_HOGDescriptor(
             Self,
             img,
             foundLocations,
             hitThreshold,
             winStride,
             padding,
             scale,
             finalThreshold,
             useMeanshiftGrouping);
end;

procedure THOGDescriptor.detectMultiScale(
                                const img:TInputArray;
                                const foundLocations:vector<TRect>;
                                hitThreshold :double {= 0};
                                const winStride :TSize);
begin
  detectMultiScale(img,foundLocations,hitThreshold,winStride,Size());
end;

procedure THOGDescriptor.detectMultiScale(
                                const img:TInputArray;
                                const foundLocations:vector<TRect>);
begin
  detectMultiScale(img,foundLocations,0,Size(),Size());
end;

{$ENDIF}
