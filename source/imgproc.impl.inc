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

{$IF defined(OPENCV_IMGPROC_HPP) and (not defined(OPENCV_IMGPROC_HPP_IMPL))}
{$DEFINE OPENCV_IMGPROC_HPP_IMPL}

procedure blur(const Src: TInputArray; const dst: TOutputArray; const ksize: TSize);
begin
  blur(Src, dst, ksize, Point(-1, -1), BORDER_DEFAULT);
end;

function morphologyDefaultBorderValue(): TScalar;
begin
  Result := TScalar.all(DBL_MAX);
end;

procedure polylines(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const isClosed: BOOL; const color: TScalar; const thickness: Int = 1; const lineType: LineTypes = LINE_8; const shift: Int = 0);
begin
  polylines(img, pts, npts, ncontours, isClosed, color, thickness, Int(lineType), shift);
end;

{ TSubdiv2D }

class operator TSubdiv2D.Initialize(out Dest: TSubdiv2D);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_SUBDIV2D_3(Dest);
{$IFEND}
end;

class operator TSubdiv2D.Finalize(var Dest: TSubdiv2D);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_SUBDIV2D(Dest);
{$IFEND}
end;

class operator TSubdiv2D.assign(var Dest: TSubdiv2D; const [ref] Src: TSubdiv2D);
begin
{$IF not defined(PACKAGE)}
  operator_CV_SUBDIV2D_assign_1(Dest, Src);
{$IFEND}
end;

procedure TSubdiv2D.getTriangleList(const triangleList: vector<TVec6f>);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_SUBDIV2D_getTriangleList(Self, triangleList);
{$IFEND}
end;

function TSubdiv2D.locate(const pt: TPoint2f; Var edge: Int; Var vertex: Int): Int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_SUBDIV2D_locate(Self, pt, edge, vertex);
{$IFEND}
end;

function TSubdiv2D.edgeOrg(edge: Int; var orgpt: TPoint2f): Int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_SUBDIV2D_edgeOrg(Self, edge, orgpt);
{$IFEND}
end;

function TSubdiv2D.edgeDst(edge: Int; var dstpt: TPoint2f): Int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_SUBDIV2D_edgeOrg(Self, edge, dstpt);
{$IFEND}
end;

function TSubdiv2D.getEdge(edge: Int; nextEdgeType: Int): Int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_SUBDIV2D_getEdge(Self, edge, nextEdgeType);
{$IFEND}
end;

procedure TSubdiv2D.getVoronoiFacetList(const idx: vector<Int>; const facetList: vector<vector<TPoint2f>>; const facetCenters: vector<TPoint2f>);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_SUBDIV2D_getVoronoiFacetList(Self, idx, facetList, facetCenters);
{$IFEND}
end;

class function TSubdiv2D.Subdiv2D(const rect: TRect): TSubdiv2D;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_SUBDIV2D_2(Result, rect);
{$IFEND}
end;

class operator TSubdiv2D.Implicit(const r: TRect): TSubdiv2D;
begin
  Result := TSubdiv2D.Subdiv2D(r);
end;

function TSubdiv2D.insert(const pt: TPoint2f): Int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_SUBDIV2D_insert(Self, pt);
{$IFEND}
end;

function getStructuringElement(shape: MorphShapes; const ksize: TSize): TMat;
begin
  Result := getStructuringElement(shape, ksize, Point(-1, -1));
end;

function getStructuringElement(shape: MorphShapes; ksize: TSize; anchor: TPoint): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := func_getStructuringElement(shape, ksize, anchor);
{$IFEND}
end;

procedure medianBlur(const Src: TInputArray; const dst: TOutputArray; ksize: Int);
begin
{$IF not defined(PACKAGE)}
  proc_medianBlur(Src, dst, ksize);
{$IFEND}
end;

procedure GaussianBlur(const Src: TInputArray; const dst: TOutputArray; ksize: TSize; sigmaX: double; sigmaY: double = 0; borderType: Int = BORDER_DEFAULT);
begin
{$IF not defined(PACKAGE)}
  proc_GaussianBlur(Src, dst, ksize, sigmaX, sigmaY, borderType);
{$IFEND}
end;

procedure bilateralFilter(const Src: TInputArray; const dst: TOutputArray; d: Int; sigmaColor, sigmaSpace: double; borderType: BorderTypes = BORDER_DEFAULT);
begin
{$IF not defined(PACKAGE)}
  proc_bilateralFilter(Src, dst, d, sigmaColor, sigmaSpace, borderType);
{$IFEND}
end;

procedure blur(const Src: TInputArray; const dst: TOutputArray; ksize: TSize; anchor: TPoint { = Point(-1, -1) }; borderType: Int = BORDER_DEFAULT);
begin
{$IF not defined(PACKAGE)}
  proc_blur(Src, dst, ksize, anchor, borderType);
{$IFEND}
end;

procedure Scharr(const Src: TInputArray; const dst: TOutputArray; depth: Int; dx, dy: Int; scale: double = 1; delta: double = 0; borderType: Int = BORDER_DEFAULT);
begin
{$IF not defined(PACKAGE)}
  proc_Scharr(Src, dst, depth, dx, dy, scale, delta, borderType);
{$IFEND}
end;

procedure Laplacian(const Src: TInputArray; const dst: TOutputArray; ddepth: Int; ksize: Int = 1; scale: double = 1; delta: double = 0; borderType: Int = BORDER_DEFAULT);
begin
{$IF not defined(PACKAGE)}
  proc_Laplacian(Src, dst, ddepth, ksize, scale, delta, borderType);
{$IFEND}
end;

procedure Canny(const image: TInputArray; const edges: TOutputArray; threshold1, threshold2: double; apertureSize: Int = 3; L2gradient: BOOL = false);
begin
{$IF not defined(PACKAGE)}
  proc_Canny_1(image, edges, threshold1, threshold2, apertureSize, L2gradient);
{$IFEND}
end;

procedure Canny(const dx: TInputArray; const dy: TInputArray; const edges: TOutputArray; threshold1, threshold2: double; L2gradient: BOOL = false);
begin
{$IF not defined(PACKAGE)}
  proc_Canny(dx, dy, edges, threshold1, threshold2, L2gradient);
{$IFEND}
end;

procedure cornerMinEigenVal(const Src: TInputArray; const dst: TOutputArray; blockSize: Int; ksize: Int = 3; borderType: BorderTypes = BORDER_DEFAULT);
begin
{$IF not defined(PACKAGE)}
  proc_cornerMinEigenVal(Src, dst, blockSize, ksize, borderType);
{$IFEND}
end;

procedure cornerEigenValsAndVecs(const Src: TInputArray; const dst: TOutputArray; blockSize, ksize: Int; borderType: BorderTypes = BORDER_DEFAULT);
begin
{$IF not defined(PACKAGE)}
  proc_cornerEigenValsAndVecs(Src, dst, blockSize, ksize, borderType);
{$IFEND}
end;

procedure cornerSubPix(const image: TInputArray; const corners: TInputOutputArray; winSize: TSize; zeroZone: TSize; const criteria: TTermCriteria);
begin
{$IF not defined(PACKAGE)}
  proc_cornerSubPix(image, corners, winSize, zeroZone, criteria);
{$IFEND}
end;

procedure goodFeaturesToTrack(const image: TInputArray; const corners: TOutputArray; maxCorners: Int; qualityLevel: double; minDistance: double; const mask: TInputArray { = noArray() }; blockSize: Int = 3; useHarrisDetector: BOOL = false; k: double = 0.04);
begin
{$IF not defined(PACKAGE)}
  proc_goodFeaturesToTrack_2(image, corners, maxCorners, qualityLevel, minDistance, mask, blockSize, useHarrisDetector, k);
{$IFEND}
end;

procedure goodFeaturesToTrack(const image: TInputArray; const corners: TOutputArray; maxCorners: Int; qualityLevel: double; minDistance: double);
begin
  goodFeaturesToTrack(image, corners, maxCorners, qualityLevel, minDistance, TInputArray.noArray());
end;

procedure goodFeaturesToTrack(const image: TInputArray; const corners: TOutputArray; maxCorners: Int; qualityLevel: double; minDistance: double; const mask: TInputArray; blockSize: Int; gradientSize: Int; useHarrisDetector: BOOL = false; k: double = 0.04);
begin
{$IF not defined(PACKAGE)}
  proc_goodFeaturesToTrack_1(image, corners, maxCorners, qualityLevel, minDistance, mask, blockSize, gradientSize, useHarrisDetector, k);
{$IFEND}
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; anchor: TPoint { = Point(-1,-1) }; iterations: Int { = 1 }; borderType: Int { = BORDER_CONSTANT }; const borderValue: TScalar { = morphologyDefaultBorderValue() }
  );
begin
{$IF not defined(PACKAGE)}
  proc_erode(Src, dst, kernel, anchor, iterations, borderType, borderValue);
{$IFEND}
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int = 1; const borderType: BorderTypes = BORDER_CONSTANT);
begin
  erode(Src, dst, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray);
begin
  erode(Src, dst, kernel, Point(-1, -1));
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; anchor: TPoint { = Point(-1,-1) }; //
  iterations: Int { = 1 }; borderType: Int { = BORDER_CONSTANT }; const borderValue: TScalar { = morphologyDefaultBorderValue() }
  );
begin
{$IF not defined(PACKAGE)}
  proc_dilate(Src, dst, kernel, anchor, iterations, borderType, borderValue);
{$IFEND}
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; anchor: TPoint { = Point(-1,-1) }; //
  iterations: Int = 1; borderType: Int = BORDER_CONSTANT);
begin
  dilate(Src, dst, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray);
begin
  dilate(Src, dst, kernel, Point(-1, -1));
end;

// procedure morphologyEx(const Src: TInputArray; const dst: TOutputArray; op: MorphTypes; const kernel: TInputArray; const anchor: TPoint; iterations: Int = 1; borderType: BorderTypes = BORDER_CONSTANT);
// begin
// morphologyEx(Src, dst, op, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
// end;
// procedure morphologyEx(const Src: TInputArray; const dst: TOutputArray; op: MorphTypes; const kernel: TInputArray);
// begin
// morphologyEx(Src, dst, op, kernel, Point(-1, -1));
// end;

procedure morphologyEx(const Src: TInputArray; const dst: TOutputArray; op: MorphTypes; const kernel: TInputArray; //
  const anchor: TPoint { = Point(-1,-1) }; iterations: Int { = 1 }; borderType: BorderTypes { = BORDER_CONSTANT }; //
  const borderValue: TScalar { = morphologyDefaultBorderValue() } );
begin
{$IF not defined(PACKAGE)}
  proc_morphologyEx(Src, dst, op, kernel, anchor, iterations, borderType, borderValue);
{$IFEND}
end;

procedure morphologyEx(const Src: TInputArray; const dst: TOutputArray; op: MorphTypes; const kernel: TInputArray; //
  const anchor: TPoint { = Point(-1,-1) }; iterations: Int = 1; borderType: BorderTypes = BORDER_CONSTANT);
begin
  morphologyEx(Src, dst, op, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
end;

procedure morphologyEx(const Src: TInputArray; const dst: TOutputArray; op: MorphTypes; const kernel: TInputArray);
begin
  morphologyEx(Src, dst, op, kernel, Point(-1, -1));
end;

procedure resize(const Src: TInputArray; const dst: TOutputArray; dsize: TSize; fx: double = 0; fy: double = 0; interpolation: InterpolationFlags = INTER_LINEAR);
begin
{$IF not defined(PACKAGE)}
  proc_resize(Src, dst, dsize, fx, fy, interpolation);
{$IFEND}
end;

function phaseCorrelate(const src1: TInputArray; const src2: TInputArray; const window: TInputArray { = noArray() }; response: pDouble = nil): TPoint2d;
begin
{$IF not defined(PACKAGE)}
  Result := func_phaseCorrelate(src1, src2, window, response);
{$IFEND}
end;

function phaseCorrelate(const src1: TInputArray; const src2: TInputArray): TPoint2d;
begin
  Result := phaseCorrelate(src1, src2, TInputArray.noArray());
end;

procedure createHanningWindow(const dst: TOutputArray; const winSize: TSize; &type: Int);
begin
{$IF not defined(PACKAGE)}
  proc_createHanningWindow(dst, winSize, &type);
{$IFEND}
end;

function threshold(const Src: TInputArray; const dst: TOutputArray; thresh, maxVal: double; &type: ThresholdTypes): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_threshold(Src, dst, thresh, maxVal, &type);
{$IFEND}
end;

procedure adaptiveThreshold(const Src: TInputArray; const dst: TOutputArray; maxValue: double; adaptiveMethod: AdaptiveThresholdTypes; thresholdType: ThresholdTypes; blockSize: Int; c: double);
begin
{$IF not defined(PACKAGE)}
  proc_adaptiveThreshold(Src, dst, maxValue, adaptiveMethod, thresholdType, blockSize, c);
{$IFEND}
end;

procedure calcHist(const images: TMat; nimages: Int; channels: pInt; const mask: TInputArray; const hist: TOutputArray; dims: Int; const histSize: pInt; const ranges: pFloat; UNIFORM: BOOL = true; accumulate: BOOL = false);
begin
{$IF not defined(PACKAGE)}
  proc_calcHist_2(images, nimages, channels, mask, hist, dims, histSize, ranges, UNIFORM, accumulate);
{$IFEND}
end;

procedure calcBackProject(const images: pMat; nimages: Int; const channels: pInt; const hist: TInputArray; const backProject: TOutputArray; const ranges: pFloat { float** }; scale: double = 1; UNIFORM: BOOL = true);
begin
{$IF not defined(PACKAGE)}
  proc_calcBackProject_2(images, nimages, channels, hist, backProject, ranges, scale, UNIFORM);
{$IFEND}
end;

procedure calcBackProject(const images: TInputArrayOfArrays; const channels: vector<Int>; const hist: TInputArray; const dst: TOutputArray; const ranges: vector<float>; scale: double);
begin
{$IF not defined(PACKAGE)}
  proc_calcBackProject(images, channels, hist, dst, ranges, scale);
{$IFEND}
end;

procedure equalizeHist(const Src: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_equalizeHist(Src, dst);
{$IFEND}
end;

procedure grabCut(const img: TInputArray; const mask: TInputOutputArray; const rect: TRect; const bgdModel: TInputOutputArray; const fgdModel: TInputOutputArray; iterCount: Int; mode: GrabCutModes = GC_EVAL);
begin
{$IF not defined(PACKAGE)}
  proc_grabCut(img, mask, rect, bgdModel, fgdModel, iterCount, mode);
{$IFEND}
end;

procedure distanceTransform(const Src: TInputArray; const dst: TOutputArray; const labels: TOutputArray; distanceType: DistanceTypes; maskSize: DistanceTransformMasks; labelType: DistanceTransformLabelTypes = DIST_LABEL_CCOMP);
begin
{$IF not defined(PACKAGE)}
  proc_distanceTransform(Src, dst, labels, distanceType, maskSize, labelType);
{$IFEND}
end;

procedure distanceTransform(const Src: TInputArray; const dst: TOutputArray; distanceType: DistanceTypes; maskSize: DistanceTransformMasks; dstType: Int = CV_32F);
begin
{$IF not defined(PACKAGE)}
  proc_distanceTransform_1(Src, dst, distanceType, maskSize, dstType);
{$IFEND}
end;

procedure cvtColor(const Src: TInputArray; const dst: TOutputArray; code: ColorConversionCodes; dstCn: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_cvtColor(Src, dst, code, dstCn);
{$IFEND}
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; const hierarchy: TOutputArray; mode: Int; method: Int; const offset: TPoint { = Point() }
  );
begin
{$IF not defined(PACKAGE)}
  proc_findContours(image, contours, hierarchy, mode, method, offset);
{$IFEND}
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; const hierarchy: TOutputArray; mode: Int; method: Int);
begin
  findContours(image, contours, hierarchy, mode, method, TPoint.Point());
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; mode: Int; method: Int; const offset: TPoint { = Point() } );
begin
{$IF not defined(PACKAGE)}
  proc_findContours_1(image, contours, mode, method, offset);
{$IFEND}
end;

procedure findContours(const image: TInputArray; const contours: TOutputArrayOfArrays; mode: Int; method: Int);
begin
  findContours(image, contours, mode, method, TPoint.Point());
end;

procedure approxPolyDP(const curve: TInputArray; const approxCurve: TOutputArray; epsilon: double; closed: BOOL);
begin
{$IF not defined(PACKAGE)}
  proc_approxPolyDP(curve, approxCurve, epsilon, closed);
{$IFEND}
end;

function contourArea(const contour: TInputArray; oriented: BOOL = false): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_contourArea(contour, oriented);
{$IFEND}
end;

procedure convexHull(const points: TInputArray; const hull: TOutputArray; clockwise: BOOL = false; returnPoints: BOOL = true);
begin
{$IF not defined(PACKAGE)}
  proc_convexHull(points, hull, clockwise, returnPoints);
{$IFEND}
end;

procedure applyColorMap(const Src: TInputArray; const dst: TOutputArray; colormap: ColormapTypes);
begin
{$IF not defined(PACKAGE)}
  proc_applyColorMap_1(Src, dst, colormap);
{$IFEND}
end;

procedure line(const img: TInputOutputArray; const pt1: TPoint; const pt2: TPoint; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_line(img, pt1, pt2, color, thickness, lineType, shift);
{$IFEND}
end;

procedure arrowedLine(const img: TInputOutputArray; pt1: TPoint; pt2: TPoint; const color: TScalar; thickness: Int = 1; line_type: Int = 8; shift: Int = 0; tipLength: double = 0.1);
begin
{$IF not defined(PACKAGE)}
  proc_arrowedLine(img, pt1, pt2, color, thickness, line_type, shift, tipLength);
{$IFEND}
end;

procedure rectangle(const img: TInputOutputArray; pt1, pt2: TPoint; const color: TScalar; thickness: Int = 1; lineType: Int = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_rectangle(img, pt1, pt2, color, thickness, lineType, shift);
{$IFEND}
end;

procedure circle(const img: TInputOutputArray; center: TPoint; radius: Int; const color: TScalar; thickness: Int = 1; lineType: Int = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_circle(img, center, radius, color, thickness, lineType, shift);
{$IFEND}
end;

procedure ellipse(const img: TInputOutputArray; center: TPoint; axes: TSize; angle, startAngle, endAngle: double; const color: TScalar; thickness: Int = 1; lineType: Int = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_ellipse_1(img, center, axes, angle, startAngle, endAngle, color, thickness, lineType, shift);
{$IFEND}
end;

procedure ellipse(const img: TInputOutputArray; const box: TRotatedRect; const color: TScalar; thickness: Int = 1; lineType: Int = LINE_8);
begin
{$IF not defined(PACKAGE)}
  proc_ellipse(img, box, color, thickness, lineType);
{$IFEND}
end;

procedure drawMarker(const img: TInputOutputArray; position: TPoint; const color: TScalar; markerType: Int = MARKER_CROSS; markerSize: Int = 20; thickness: Int = 1; line_type: Int = 8);
begin
{$IF not defined(PACKAGE)}
  proc_drawMarker(img, position, color, markerType, markerSize, thickness, line_type);
{$IFEND}
end;

procedure fillConvexPoly(const img: TInputOutputArray; const points: TInputArray; const color: TScalar; lineType: Int = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_fillConvexPoly(img, points, color, lineType, shift);
{$IFEND}
end;

procedure fillConvexPoly(const img: TInputOutputArray; const pts: pPoint; npts: Int; const color: TScalar; lineType: Int = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_fillConvexPoly_1(img, pts, npts, color, lineType, shift);
{$IFEND}
end;

procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; ncontours: Int; const color: TScalar; lineType: Int { = LINE_8 }; shift: Int { = 0 }; offset: TPoint { = Point() }
  );
begin
{$IF not defined(PACKAGE)}
  proc_fillPoly_1(img, pts, npts, ncontours, color, lineType, shift, offset);
{$IFEND}
end;

procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; ncontours: Int; const color: TScalar; lineType: Int = LINE_8; shift: Int = 0);
begin
  fillPoly(img, pts, npts, ncontours, color, lineType, shift, TPoint.Point());
end;

procedure polylines(const img: TInputOutputArray; const pts: TInputArrayOfArrays; isClosed: BOOL; const color: TScalar; thickness: Int = 1; lineType: Int = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_polylines(img, pts, isClosed, color, thickness, lineType, shift);
{$IFEND}
end;

procedure polylines(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; ncontours: Int; isClosed: BOOL; const color: TScalar; thickness: Int = 1; lineType: Int = LINE_8; shift: Int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_polylines_1(img, pts, npts, ncontours, isClosed, color, thickness, lineType, shift);
{$IFEND}
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int { = 1 }; lineType: Int { = LINE_8 }; const hierarchy: TInputArray { = noArray() }; maxLevel: Int { = INT_MAX }; offset: TPoint { = Point() }
  );
begin
{$IF not defined(PACKAGE)}
  proc_drawContours(image, contours, contourIdx, color, thickness, lineType, hierarchy, maxLevel, offset);
{$IFEND}
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int { = 1 }; lineType: Int { = LINE_8 }; const hierarchy: TInputArray { = noArray() }; maxLevel: Int = INT_MAX);
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType, hierarchy, maxLevel, TPoint.Point());
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int = 1; lineType: Int = LINE_8);
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType);
end;

procedure putText(const img: TInputOutputArray; const text: CppString; org: TPoint; fontFace: Int; fontScale: double; color: TScalar; thickness: Int = 1; lineType: Int = LINE_8; bottomLeftOrigin: BOOL = false);
begin
{$IF not defined(PACKAGE)}
  proc_putText(img, text, org, fontFace, fontScale, color, thickness, lineType, bottomLeftOrigin);
{$IFEND}
end;

function getTextSize(text: String; fontFace: Int; fontScale: double; thickness: Int; baseLine: pInt = nil): TSize;
begin
{$IF not defined(PACKAGE)}
  func_getTextSize(pCV_SIZE__OF_INT(@Result), CppString(text), fontFace, fontScale, thickness, baseLine);
{$IFEND}
end;

{$I 'imgproc/segmentation.impl.inc'}
{$IFEND}
