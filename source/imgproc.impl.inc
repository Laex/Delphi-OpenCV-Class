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

{$ifdef OPENCV_IMGPROC_HPP}

procedure putText( const img: TInputOutputArray; const text: String; org: TPoint;
                   fontFace: HersheyFonts; fontScale: double; color: TScalar;
                   thickness: Int = 1; lineType: LineTypes = LINE_8;
                   bottomLeftOrigin: BOOL = false);
begin
  putText(img, CppString(text), org, Int(fontFace), fontScale, color, thickness, Int(lineType), bottomLeftOrigin);
end;

procedure blur(const Src: TInputArray; const dst: TOutputArray; const ksize: TSize);
begin
  blur(Src, dst, ksize, Point(-1, -1), BORDER_DEFAULT);
end;

procedure blur(const Src: TInputArray; const dst: TOutputArray; const ksize: TSize; const anchor: TPoint; borderType: BorderTypes);
begin
  blur(Src, dst, ksize, anchor, Int(borderType));
end;

procedure GaussianBlur(const Src: TInputArray; const dst: TOutputArray; const ksize: TSize; sigmaX: double; sigmaY: double; borderType: BorderTypes);
begin
  GaussianBlur(Src, dst, ksize, sigmaX, sigmaY, Int(borderType));
end;

procedure cvtColor(const Src: TInputArray; const dst: TOutputArray; code: ColorConversionCodes; dstCn: Int = 0);
begin
  cvtColor(Src, dst, Int(code), dstCn);
end;

procedure adaptiveThreshold(const Src: TInputArray; const dst: TOutputArray; maxValue: double; adaptiveMethod: AdaptiveThresholdTypes; thresholdType: ThresholdTypes; blockSize: Int; c: double);
begin
  adaptiveThreshold(Src, dst, maxValue, Int(adaptiveMethod), Int(thresholdType), blockSize, c);
end;

function getStructuringElement(shape: MorphShapes; const ksize: TSize): TMat;
begin
  Result := getStructuringElement(shape, ksize, Point(-1, -1));
end;

function getStructuringElement(shape: MorphShapes; const ksize: TSize; anchor: TPoint): TMat;
begin
  Result := getStructuringElement(Int(shape), UInt64(ksize), UInt64(anchor));
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint; const iterations: Int; const borderType: BorderTypes;
  const borderValue: TScalar { = morphologyDefaultBorderValue() } );
begin
  erode(Src, dst, kernel, UInt64(anchor), iterations, Int(borderType), borderValue);
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint; const iterations: Int; const borderType: BorderTypes { = BORDER_CONSTANT } );
begin
  erode(Src, dst, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint; const iterations: Int { = 1 } );
begin
  erode(Src, dst, kernel, anchor, iterations, BORDER_CONSTANT);
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) } );
begin
  erode(Src, dst, kernel, anchor, 1);
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray);
begin
  erode(Src, dst, kernel, Point(-1, -1));
end;

function morphologyDefaultBorderValue(): TScalar;
begin
  Result := TScalar.all(DBL_MAX);
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT }; const borderValue: TScalar { = morphologyDefaultBorderValue() } );
begin
  dilate(Src, dst, kernel, UInt64(anchor), iterations, Int(borderType), borderValue);
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT } );
begin
  dilate(Src, dst, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 } );
begin
  dilate(Src, dst, kernel, anchor, iterations, BORDER_CONSTANT);
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) } );
begin
  dilate(Src, dst, kernel, anchor, 1);
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray);
begin
  dilate(Src, dst, kernel, Point(-1, -1));
end;

procedure ellipse(const img: TInputOutputArray; const center: TPoint; const axes: TSize; angle, startAngle, endAngle: double; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
  shift: Int = 0);
begin
  ellipse(img, UInt64(center), UInt64(axes), angle, startAngle, endAngle, color, thickness, Int(lineType), shift);
end;

procedure ellipse(const img: TInputOutputArray; const box: TRotatedRect; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8);
begin
  ellipse(img, box, color, thickness, Int(lineType));
end;

procedure circle(const img: TInputOutputArray; const center: TPoint; radius: Int; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8; shift: Int = 0);
begin
  circle(img, center, radius, color, thickness, Int(lineType), shift);
end;

procedure line(const img: TInputOutputArray; const pt1: TPoint; const pt2: TPoint; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8; shift: Int = 0);
begin
  line(img, UInt64(pt1), UInt64(pt2), color, thickness, Int(lineType), shift);
end;

procedure arrowedLine(const img: TInputOutputArray; const pt1: TPoint; const pt2: TPoint; const color: TScalar; const thickness: Int = 1; const line_type: LineTypes = LineTypes(8);
  const shift: Int = 0; const tipLength: double = 0.1);
begin
  arrowedLine(img, UInt64(pt1), UInt64(pt2), color, thickness, Int(line_type), shift, tipLength);
end;

procedure rectangle(const img: TInputOutputArray; const pt1, pt2: TPoint; const color: TScalar; const thickness: Int = 1; const lineType: LineTypes = LINE_8; const shift: Int = 0);
begin
  rectangle(img, pt1, pt2, color, thickness, Int(lineType), shift);
end;

procedure drawMarker(const img: TInputOutputArray; const position: TPoint; const color: TScalar; const markerType: MarkerTypes = MARKER_CROSS; const markerSize: Int = 20; const thickness: Int = 1;
  const line_type: LineTypes = LineTypes(8));
begin
  drawMarker(img, UInt64(position), color, Int(markerType), markerSize, thickness, Int(line_type));
end;

procedure polylines(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const isClosed: BOOL; const color: TScalar; const thickness: Int = 1;
  const lineType: LineTypes = LINE_8; const shift: Int = 0);
begin
  polylines(img, pts, npts, ncontours, isClosed, color, thickness, Int(lineType), shift);
end;

procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const color: TScalar; const lineType: LineTypes { = LINE_8 }; const shift: Int { = 0 };
  const offset: TPoint); overload;
begin
  fillPoly(img, pts, npts, ncontours, color, Int(lineType), shift, UInt64(offset));
end;

procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const color: TScalar; const lineType: LineTypes = LINE_8; const shift: Int = 0);
begin
  fillPoly(img, pts, npts, ncontours, color, lineType, shift, Point(0, 0));
end;

function getTextSize(const text: String; fontFace: HersheyFonts; fontScale: double; thickness: Int; baseLine: pInt = nil): TSize;
begin
  getTextSize(Result, text, Int(fontFace), fontScale, thickness, baseLine);
end;

procedure Scharr(const Src: TInputArray; const dst: TOutputArray; depth: Int; dx, dy: Int; scale: double = 1; delta: double = 0; borderType: BorderTypes = BORDER_DEFAULT);
begin
  Scharr(Src, dst, depth, dx, dy, scale, delta, Int(borderType));
end;

procedure createHanningWindow(const dst: TOutputArray; const winSize: TSize; &type: Int);
begin
  createHanningWindow(dst, UInt64(winSize), &type);
end;

function phaseCorrelate(const src1: TInputArray; const src2: TInputArray; const window: TInputArray { = noArray() }; Var response: double { = 0 } ): TPoint2d;
begin
  Result := phaseCorrelate(src1, src2, window, @response);
end;

function phaseCorrelate(const src1: TInputArray; const src2: TInputArray; const window: TInputArray { = noArray() } ): TPoint2d;
begin
  Result := phaseCorrelate(src1, src2, window, nil);
end;

function phaseCorrelate(const src1: TInputArray; const src2: TInputArray): TPoint2d;
begin
  Result := phaseCorrelate(src1, src2, TInputArray.noArray);
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; const hierarchy: TOutputArray; mode: RetrievalModes; method: ContourApproximationModes; const offset: TPoint);
begin
  findContours(image, contours, hierarchy, Int(mode), Int(method), offset);
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; const hierarchy: TOutputArray; mode: RetrievalModes; method: ContourApproximationModes);
begin
  findContours(image, contours, hierarchy, mode, method, Point(0, 0));
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; mode: RetrievalModes; method: ContourApproximationModes);
begin
  findContours(image, contours, mode, method, Point(0, 0));
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; mode: RetrievalModes; method: ContourApproximationModes; const offset: TPoint);
begin
  findContours(image, contours, mode, method, offset);
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int { = 1 }; lineType: LineTypes { = LINE_8 };
  const hierarchy: TInputArray { = noArray() }; maxLevel: Int { = INT_MAX }; offset: TPoint { = Point() } );
begin
  drawContours(image, contours, contourIdx, color, thickness, Int(lineType), hierarchy, maxLevel, UInt64(offset));
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int { = 1 }; lineType: LineTypes;
  const hierarchy: TInputArray { = noArray() }; maxLevel: Int);
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType, hierarchy, maxLevel, Point(0, 0));
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int { = 1 }; lineType: LineTypes;
  const hierarchy: TInputArray { = noArray() } );
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType, hierarchy, INT_MAX);
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int { = 1 }; lineType: LineTypes);
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType, TInputArray.noArray);
end;

{$i 'imgproc/segmentation.impl.inc'}

{$ENDIF}
