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

{$IF defined(OPENCV_IMGPROC_IMGPROC_C_H) and (not defined(OPENCV_IMGPROC_IMGPROC_C_H_IMPL))}

{$if not defined(OPENCV_IMGPROC_TYPES_C_H_IMPL)}{$i 'imgproc/types_c.impl.inc'}{$ifend}

{$DEFINE OPENCV_IMGPROC_IMGPROC_C_H_IMPL}

procedure cvPyrMeanShiftFiltering(const src: pCvArr; dst: pCvArr; sp: double; sr: double; max_level: int);
begin
  cvPyrMeanShiftFiltering(src, dst, sp, sr, max_level, cvTermCriteria(CV_TERMCRIT_ITER + CV_TERMCRIT_EPS, 5, 1));
end;

procedure cvPyrMeanShiftFiltering(const src: pCvArr; dst: pCvArr; sp: double; sr: double);
begin
  cvPyrMeanShiftFiltering(src, dst, sp, sr, 1);
end;

procedure cvWarpAffine(const src: pCvArr; dst: pCvArr; const map_matrix: pCvMat; flags: int);
begin
  cvWarpAffine(src, dst, map_matrix, flags, cvScalarAll(0));
end;

procedure cvWarpPerspective(const src: pCvArr; dst: pCvArr; const map_matrix: pCvMat; flags: int);
begin
  cvWarpPerspective(src, dst, map_matrix, flags, cvScalarAll(0));
end;

procedure cvRemap(const src: pCvArr; dst: pCvArr; const mapx: pCvArr; const mapy: pCvArr; flags: int);
begin
  cvRemap(src, dst, mapx, mapy, flags, cvScalarAll(0));
end;

function cvFindContours(image: pCvArr; storage: pCvMemStorage; first_contour: pCvSeq; header_size: int; mode: int; method: int): int;
begin
  Result := cvFindContours(image, storage, first_contour, header_size, mode, method, cvPoint(0, 0));
end;

function cvStartFindContours(image: pCvArr; storage: pCvMemStorage; header_size: int; mode: int; method: int): TCvContourScanner;
begin
  Result := cvStartFindContours(image, storage, header_size, mode, method, cvPoint(0, 0));
end;

function cvArcLength(const curve: pvoid; slice: TCvSlice): double;
begin
  Result := cvArcLength(curve, slice, -1);
end;

function cvArcLength(const curve: pvoid): double;
begin
  Result := cvArcLength(curve, CV_WHOLE_SEQ);
end;

function cvContourPerimeter(const contour: pvoid): double;
begin
  Result := cvArcLength(contour, CV_WHOLE_SEQ, 1);
end;

function cvContourArea(const contour: pCvArr; slice: TCvSlice): double;
begin
  Result := cvContourArea(contour, slice, 0);
end;

function cvContourArea(const contour: pCvArr): double;
begin
  Result := cvContourArea(contour, CV_WHOLE_SEQ);
end;

procedure cvCalcHist(image: pIplImage; hist: pCvHistogram; accumulate: int = 0; const mask: pCvArr = nil);
begin
  cvCalcArrHist(pCvArr(image), hist, accumulate, mask);
end;

procedure cvFloodFill(image: pCvArr; seed_point: TCvPoint; new_val: TCvScalar; lo_diff: TCvScalar);
begin
  cvFloodFill(image, seed_point, new_val, lo_diff, cvScalarAll(0));
end;

procedure cvFloodFill(image: pCvArr; seed_point: TCvPoint; new_val: TCvScalar);
begin
  cvFloodFill(image, seed_point, new_val, cvScalarAll(0));
end;

procedure cvEllipseBox(img: pCvArr; box: TCvBox2D; color: TCvScalar; thickness: int = 1; line_type: int = 8; shift: int = 0);
begin
  Var
    axes: TCvSize := cvSize(cvRound(box.size.width * 0.5), cvRound(box.size.height * 0.5));
  cvEllipse(img, cvPointFrom32f(box.center), axes, box.angle, 0, 360, color, thickness, line_type, shift);
end;

function cvFont(scale: double; thickness: int = 1): TCvFont;
begin
  cvInitFont(Result, CV_FONT_HERSHEY_PLAIN, scale, scale, 0, thickness, CV_AA)
end;

procedure cvDrawContours(img: pCvArr; contour: pCvSeq; external_color: TCvScalar; hole_color: TCvScalar; max_level: int; thickness: int; line_type: int);
begin
  cvDrawContours(img, contour, external_color, hole_color, max_level, thickness, line_type, cvPoint(0, 0));
end;

procedure cvAcc(const image: pCvArr; sum: pCvArr; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvAcc(image, sum, mask);
{$IFEND}
end;

procedure cvSquareAcc(const image: pCvArr; sqsum: pCvArr; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvSquareAcc(image, sqsum, mask);
{$IFEND}
end;

procedure cvMultiplyAcc(const image1: pCvArr; const image2: pCvArr; acc: pCvArr; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvMultiplyAcc(image1, image2, acc, mask);
{$IFEND}
end;

procedure cvRunningAvg(const image: pCvArr; acc: pCvArr; alpha: double; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvRunningAvg(image, acc, alpha, mask);
{$IFEND}
end;

procedure cvCopyMakeBorder(const src: pCvArr; dst: pCvArr; offset: TCvPoint; bordertype: int);
begin
  cvCopyMakeBorder(src, dst, offset, bordertype, cvScalarAll(0));
end;

procedure cvCopyMakeBorder(const src: pCvArr; dst: pCvArr; offset: TCvPoint; bordertype: int; value: TCvScalar); overload;
begin
{$IF not defined(PACKAGE)}
  _cvCopyMakeBorder(src, dst, offset, bordertype, value);
{$IFEND}
end;

procedure cvSmooth(const src: pCvArr; dst: pCvArr; smoothtype: int = CV_GAUSSIAN; size1: int = 3; size2: int = 0; sigma1: double = 0; sigma2: double = 0);
begin
{$IF not defined(PACKAGE)}
  _cvSmooth(src, dst, smoothtype, size1, size2, sigma1, sigma2);
{$IFEND}
end;

procedure cvFilter2D(const src: pCvArr; dst: pCvArr; const kernel: pCvMat);
begin
  cvFilter2D(src, dst, kernel, cvPoint(-1, -1));
end;

procedure cvFilter2D(const src: pCvArr; dst: pCvArr; const kernel: pCvMat; anchor: TCvPoint);
begin
{$IF not defined(PACKAGE)}
  _cvFilter2D(src, dst, kernel, anchor);
{$IFEND}
end;

procedure cvIntegral(const image: pCvArr; sum: pCvArr; sqsum: pCvArr = nil; tilted_sum: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvIntegral(image, sum, sqsum, tilted_sum);
{$IFEND}
end;

procedure cvPyrDown(const src: pCvArr; dst: pCvArr; filter: int = CV_GAUSSIAN_5x5);
begin
{$IF not defined(PACKAGE)}
  _cvPyrDown(src, dst, filter);
{$IFEND}
end;

procedure cvPyrUp(const src: pCvArr; dst: pCvArr; filter: int = CV_GAUSSIAN_5x5);
begin
{$IF not defined(PACKAGE)}
  _cvPyrUp(src, dst, filter);
{$IFEND}
end;

function cvCreatePyramid(const img: pCvArr; extra_layers: int; rate: double; const layer_sizes: pCvSize = nil; bufarr: pCvArr = nil; calc: int = 1; filter: int = CV_GAUSSIAN_5x5): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreatePyramid(img, extra_layers, rate, layer_sizes, bufarr, calc, filter);
{$IFEND}
end;

procedure cvReleasePyramid(pyramid: pCvMat; extra_layers: int);
begin
{$IF not defined(PACKAGE)}
  _cvReleasePyramid(pyramid, extra_layers);
{$IFEND}
end;

procedure cvPyrMeanShiftFiltering(const src: pCvArr; dst: pCvArr; sp: double; sr: double; max_level: int; termcrit: TCvTermCriteria);
begin
{$IF not defined(PACKAGE)}
  _cvPyrMeanShiftFiltering(src, dst, sp, sr, max_level, termcrit);
{$IFEND}
end;

procedure cvWatershed(const image: pCvArr; markers: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvWatershed(image, markers);
{$IFEND}
end;

procedure cvSobel(const src: pCvArr; dst: pCvArr; xorder: int; yorder: int; aperture_size: int = 3);
begin
{$IF not defined(PACKAGE)}
  _cvSobel(src, dst, xorder, yorder, aperture_size);
{$IFEND}
end;

procedure cvLaplace(const src: pCvArr; dst: pCvArr; aperture_size: int = 3);
begin
{$IF not defined(PACKAGE)}
  _cvLaplace(src, dst, aperture_size);
{$IFEND}
end;

procedure cvCvtColor(const src: pCvArr; dst: pCvArr; code: int);
begin
{$IF not defined(PACKAGE)}
  _cvCvtColor(src, dst, code);
{$IFEND}
end;

procedure cvResize(const src: pCvArr; dst: pCvArr; interpolation: int = CV_INTER_LINEAR);
begin
{$IF not defined(PACKAGE)}
  _cvResize(src, dst, interpolation);
{$IFEND}
end;

procedure cvWarpAffine(const src: pCvArr; dst: pCvArr; const map_matrix: pCvMat; flags: int; fillval: TCvScalar);
begin
{$IF not defined(PACKAGE)}
  _cvWarpAffine(src, dst, map_matrix, flags, fillval);
{$IFEND}
end;

function cvGetAffineTransform(const src: pCvPoint2D32f; const dst: pCvPoint2D32f; map_matrix: pCvMat): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetAffineTransform(src, dst, map_matrix);
{$IFEND}
end;

function cv2DRotationMatrix(center: TCvPoint2D32f; angle: double; scale: double; map_matrix: pCvMat): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cv2DRotationMatrix(center, angle, scale, map_matrix);
{$IFEND}
end;

procedure cvWarpPerspective(const src: pCvArr; dst: pCvArr; const map_matrix: pCvMat; flags: int; fillval: TCvScalar);
begin
{$IF not defined(PACKAGE)}
  _cvWarpPerspective(src, dst, map_matrix, flags, fillval);
{$IFEND}
end;

function cvGetPerspectiveTransform(const src: pCvPoint2D32f; const dst: pCvPoint2D32f; map_matrix: pCvMat): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetPerspectiveTransform(src, dst, map_matrix);
{$IFEND}
end;

procedure cvRemap(const src: pCvArr; dst: pCvArr; const mapx: pCvArr; const mapy: pCvArr; flags: int; fillval: TCvScalar);
begin
{$IF not defined(PACKAGE)}
  _cvRemap(src, dst, mapx, mapy, flags, fillval);
{$IFEND}
end;

procedure cvConvertMaps(const mapx: pCvArr; const mapy: pCvArr; mapxy: pCvArr; mapalpha: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvConvertMaps(mapx, mapy, mapxy, mapalpha);
{$IFEND}
end;

procedure cvLogPolar(const src: pCvArr; dst: pCvArr; center: TCvPoint2D32f; M: double; flags: int = CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS);
begin
{$IF not defined(PACKAGE)}
  _cvLogPolar(src, dst, center, M, flags);
{$IFEND}
end;

procedure cvLinearPolar(const src: pCvArr; dst: pCvArr; center: TCvPoint2D32f; maxRadius: double; flags: int = CV_INTER_LINEAR + CV_WARP_FILL_OUTLIERS);
begin
{$IF not defined(PACKAGE)}
  _cvLinearPolar(src, dst, center, maxRadius, flags);
{$IFEND}
end;

function cvCreateStructuringElementEx(cols: int; rows: int; anchor_x: int; anchor_y: int; shape: int; values: pint = nil): pIplConvKernel;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateStructuringElementEx(cols, rows, anchor_x, anchor_y, shape, values);
{$IFEND}
end;

procedure cvReleaseStructuringElement(element: pIplConvKernel);
begin
{$IF not defined(PACKAGE)}
  _cvReleaseStructuringElement(element);
{$IFEND}
end;

procedure cvErode(const src: pCvArr; dst: pCvArr; element: pIplConvKernel = nil; iterations: int = 1);
begin
{$IF not defined(PACKAGE)}
  _cvErode(src, dst, element, iterations);
{$IFEND}
end;

procedure cvDilate(const src: pCvArr; dst: pCvArr; element: pIplConvKernel = nil; iterations: int = 1);
begin
{$IF not defined(PACKAGE)}
  _cvDilate(src, dst, element, iterations);
{$IFEND}
end;

procedure cvMorphologyEx(const src: pCvArr; dst: pCvArr; temp: pCvArr; element: pIplConvKernel; operation: int; iterations: int = 1);
begin
{$IF not defined(PACKAGE)}
  _cvMorphologyEx(src, dst, temp, element, operation, iterations);
{$IFEND}
end;

procedure cvMoments(const arr: pCvArr; moments: pCvMoments; binary: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvMoments_1(arr, moments, binary);
{$IFEND}
end;

function cvGetSpatialMoment(moments: pCvMoments; x_order: int; y_order: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetSpatialMoment(moments, x_order, y_order);
{$IFEND}
end;

function cvGetCentralMoment(moments: pCvMoments; x_order: int; y_order: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetCentralMoment(moments, x_order, y_order);
{$IFEND}
end;

function cvGetNormalizedCentralMoment(moments: pCvMoments; x_order: int; y_order: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetNormalizedCentralMoment(moments, x_order, y_order);
{$IFEND}
end;

procedure cvGetHuMoments(moments: pCvMoments; hu_moments: pCvHuMoments);
begin
{$IF not defined(PACKAGE)}
  _cvGetHuMoments(moments, hu_moments);
{$IFEND}
end;

function cvSampleLine(const image: pCvArr; pt1: TCvPoint; pt2: TCvPoint; buffer: pvoid; connectivity: int = 8): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSampleLine(image, pt1, pt2, buffer, connectivity);
{$IFEND}
end;

procedure cvGetRectSubPix(const src: pCvArr; dst: pCvArr; center: TCvPoint2D32f);
begin
{$IF not defined(PACKAGE)}
  _cvGetRectSubPix(src, dst, center);
{$IFEND}
end;

procedure cvGetQuadrangleSubPix(const src: pCvArr; dst: pCvArr; const map_matrix: pCvMat);
begin
{$IF not defined(PACKAGE)}
  _cvGetQuadrangleSubPix(src, dst, map_matrix);
{$IFEND}
end;

procedure cvMatchTemplate(const image: pCvArr; const templ: pCvArr; Result: pCvArr; method: int);
begin
{$IF not defined(PACKAGE)}
  _cvMatchTemplate(image, templ, Result, method);
{$IFEND}
end;

function cvCalcEMD2(const signature1: pCvArr; const signature2: pCvArr; distance_type: int; distance_func: TCvDistanceFunction = nil; const cost_matrix: pCvArr = nil; flow: pCvArr = nil; lower_bound: pfloat = nil; userdata: pvoid = nil): float;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCalcEMD2(signature1, signature2, distance_type, distance_func, cost_matrix, flow, lower_bound, userdata);
{$IFEND}
end;

function cvFindContours(image: pCvArr; storage: pCvMemStorage; first_contour: pCvSeq; header_size: int; mode: int; method: int; offset: TCvPoint): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvFindContours(image, storage, first_contour, header_size, mode, method, offset);
{$IFEND}
end;

function cvStartFindContours(image: pCvArr; storage: pCvMemStorage; header_size: int; mode: int; method: int; offset: TCvPoint): TCvContourScanner;
begin
{$IF not defined(PACKAGE)}
  Result := _cvStartFindContours(image, storage, header_size, mode, method, offset);
{$IFEND}
end;

function cvFindNextContour(scanner: TCvContourScanner): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvFindNextContour(scanner);
{$IFEND}
end;

procedure cvSubstituteContour(scanner: TCvContourScanner; new_contour: pCvSeq);
begin
{$IF not defined(PACKAGE)}
  _cvSubstituteContour(scanner, new_contour);
{$IFEND}
end;

function cvEndFindContours(scanner: pCvContourScanner): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvEndFindContours(scanner);
{$IFEND}
end;

function cvApproxChains(src_seq: pCvSeq; storage: pCvMemStorage; method: int = CV_CHAIN_APPROX_SIMPLE; parameter: double = 0; minimal_perimeter: int = 0; recursive: int = 0): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvApproxChains(src_seq, storage, method, parameter, minimal_perimeter, recursive);
{$IFEND}
end;

procedure cvStartReadChainPoints(chain: pCvChain; reader: pCvChainPtReader);
begin
{$IF not defined(PACKAGE)}
  _cvStartReadChainPoints(chain, reader);
{$IFEND}
end;

function cvReadChainPoint(reader: pCvChainPtReader): TCvPoint;
begin
{$IF not defined(PACKAGE)}
  Result := _cvReadChainPoint(reader);
{$IFEND}
end;

function cvApproxPoly(const src_seq: pvoid; header_size: int; storage: pCvMemStorage; method: int; eps: double; recursive: int = 0): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvApproxPoly(src_seq, header_size, storage, method, eps, recursive);
{$IFEND}
end;

function cvArcLength(const curve: pvoid; slice: TCvSlice; is_closed: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvArcLength(curve, slice, is_closed);
{$IFEND}
end;

function cvBoundingRect(points: pCvArr; update: int = 0): TCvRect;
begin
{$IF not defined(PACKAGE)}
  Result := _cvBoundingRect(points, update);
{$IFEND}
end;

function cvContourArea(const contour: pCvArr; slice: TCvSlice; oriented: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvContourArea(contour, slice, oriented);
{$IFEND}
end;

function cvMinAreaRect2(const points: pCvArr; storage: pCvMemStorage = nil): TCvBox2D;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMinAreaRect2(points, storage);
{$IFEND}
end;

function cvMinEnclosingCircle(const points: pCvArr; center: pCvPoint2D32f; radius: pfloat): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMinEnclosingCircle(points, center, radius);
{$IFEND}
end;

function cvMatchShapes(const object1: pvoid; const object2: pvoid; method: int; parameter: double = 0): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMatchShapes(object1, object2, method, parameter);
{$IFEND}
end;

function cvConvexHull2(const input: pCvArr; hull_storage: pvoid = nil; orientation: int = CV_CLOCKWISE; return_points: int = 0): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvConvexHull2(input, hull_storage, orientation, return_points);
{$IFEND}
end;

function cvCheckContourConvexity(const contour: pCvArr): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCheckContourConvexity(contour);
{$IFEND}
end;

function cvConvexityDefects(const contour: pCvArr; const convexhull: pCvArr; storage: pCvMemStorage = nil): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvConvexityDefects(contour, convexhull, storage);
{$IFEND}
end;

function cvFitEllipse2(const points: pCvArr): TCvBox2D;
begin
{$IF not defined(PACKAGE)}
  Result := _cvFitEllipse2(points);
{$IFEND}
end;

function cvMaxRect(const rect1: pCvRect; const rect2: pCvRect): TCvRect;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMaxRect(rect1, rect2);
{$IFEND}
end;

procedure cvBoxPoints(box: TCvBox2D; pt: pt4);
begin
{$IF not defined(PACKAGE)}
  _cvBoxPoints(box, pt);
{$IFEND}
end;

function cvPointSeqFromMat(seq_kind: int; const mat: pCvArr; contour_header: pCvContour; block: pCvSeqBlock): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvPointSeqFromMat(seq_kind, mat, contour_header, block);
{$IFEND}
end;

function cvPointPolygonTest(const contour: pCvArr; pt: TCvPoint2D32f; measure_dist: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvPointPolygonTest(contour, pt, measure_dist);
{$IFEND}
end;

function cvCreateHist(dims: int; sizes: pint; &type: int; ranges: pfloat = nil; uniform: int = 1): pCvHistogram;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateHist(dims, sizes, &type, ranges, uniform);
{$IFEND}
end;

procedure cvSetHistBinRanges(hist: pCvHistogram; ranges: pfloat; uniform: int = 1);
begin
{$IF not defined(PACKAGE)}
  _cvSetHistBinRanges(hist, ranges, uniform);
{$IFEND}
end;

function cvMakeHistHeaderForArray(dims: int; sizes: pint; hist: pCvHistogram; data: pfloat; ranges: pfloat = nil; uniform: int = 1): pCvHistogram;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMakeHistHeaderForArray(dims, sizes, hist, data, ranges, uniform);
{$IFEND}
end;

procedure cvReleaseHist(hist: pCvHistogram);
begin
{$IF not defined(PACKAGE)}
  _cvReleaseHist(hist);
{$IFEND}
end;

procedure cvClearHist(hist: pCvHistogram);
begin
{$IF not defined(PACKAGE)}
  _cvClearHist(hist);
{$IFEND}
end;

procedure cvGetMinMaxHistValue(const hist: pCvHistogram; min_value: pfloat; max_value: pfloat; min_idx: pint = nil; max_idx: pint = nil);
begin
{$IF not defined(PACKAGE)}
  _cvGetMinMaxHistValue(hist, min_value, max_value, min_idx, max_idx);
{$IFEND}
end;

procedure cvNormalizeHist(hist: pCvHistogram; factor: double);
begin
{$IF not defined(PACKAGE)}
  _cvNormalizeHist(hist, factor);
{$IFEND}
end;

procedure cvThreshHist(hist: pCvHistogram; threshold: double);
begin
{$IF not defined(PACKAGE)}
  _cvThreshHist(hist, threshold);
{$IFEND}
end;

function cvCompareHist(const hist1: pCvHistogram; const hist2: pCvHistogram; method: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCompareHist(hist1, hist2, method);
{$IFEND}
end;

procedure cvCopyHist(const src: pCvHistogram; dst: pCvHistogram);
begin
{$IF not defined(PACKAGE)}
  _cvCopyHist(src, dst);
{$IFEND}
end;

procedure cvCalcBayesianProb(src: pCvHistogram; number: int; dst: pCvHistogram);
begin
{$IF not defined(PACKAGE)}
  _cvCalcBayesianProb(src, number, dst);
{$IFEND}
end;

procedure cvCalcArrHist(arr: pCvArr; hist: pCvHistogram; accumulate: int = 0; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvCalcArrHist(arr, hist, accumulate, mask);
{$IFEND}
end;

procedure cvCalcArrBackProject(image: pCvArr; dst: pCvArr; const hist: pCvHistogram);
begin
{$IF not defined(PACKAGE)}
  _cvCalcArrBackProject(image, dst, hist);
{$IFEND}
end;

procedure cvCalcArrBackProjectPatch(image: pCvArr; dst: pCvArr; range: TCvSize; hist: pCvHistogram; method: int; factor: double);
begin
{$IF not defined(PACKAGE)}
  _cvCalcArrBackProjectPatch(image, dst, range, hist, method, factor);
{$IFEND}
end;

procedure cvCalcProbDensity(const hist1: pCvHistogram; const hist2: pCvHistogram; dst_hist: pCvHistogram; scale: double = 255);
begin
{$IF not defined(PACKAGE)}
  _cvCalcProbDensity(hist1, hist2, dst_hist, scale);
{$IFEND}
end;

procedure cvEqualizeHist(const src: pCvArr; dst: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvEqualizeHist(src, dst);
{$IFEND}
end;

procedure cvDistTransform(const src: pCvArr; dst: pCvArr; distance_type: int = CV_DIST_L2; mask_size: int = 3; const mask: pfloat = nil; labels: pCvArr = nil; labelType: int = CV_DIST_LABEL_CCOMP);
begin
{$IF not defined(PACKAGE)}
  _cvDistTransform(src, dst, distance_type, mask_size, mask, labels, labelType);
{$IFEND}
end;

function cvThreshold(const src: pCvArr; dst: pCvArr; threshold: double; max_value: double; threshold_type: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvThreshold(src, dst, threshold, max_value, threshold_type);
{$IFEND}
end;

procedure cvAdaptiveThreshold(const src: pCvArr; dst: pCvArr; max_value: double; adaptive_method: int = CV_ADAPTIVE_THRESH_MEAN_C; threshold_type: int = CV_THRESH_BINARY; block_size: int = 3; param1: double = 5);
begin
{$IF not defined(PACKAGE)}
  _cvAdaptiveThreshold(src, dst, max_value, adaptive_method, threshold_type, block_size, param1);
{$IFEND}
end;

procedure cvFloodFill(image: pCvArr; seed_point: TCvPoint; new_val: TCvScalar; lo_diff: TCvScalar; up_diff: TCvScalar; comp: pCvConnectedComp = nil; flags: int = 4; mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvFloodFill(image, seed_point, new_val, lo_diff, up_diff, comp, flags, mask);
{$IFEND}
end;

procedure cvCanny(const image: pCvArr; edges: pCvArr; threshold1: double; threshold2: double; aperture_size: int = 3);
begin
{$IF not defined(PACKAGE)}
  _cvCanny(image, edges, threshold1, threshold2, aperture_size);
{$IFEND}
end;

procedure cvPreCornerDetect(const image: pCvArr; corners: pCvArr; aperture_size: int = 3);
begin
{$IF not defined(PACKAGE)}
  _cvPreCornerDetect(image, corners, aperture_size);
{$IFEND}
end;

procedure cvCornerEigenValsAndVecs(const image: pCvArr; eigenvv: pCvArr; block_size: int; aperture_size: int = 3);
begin
{$IF not defined(PACKAGE)}
  _cvCornerEigenValsAndVecs(image, eigenvv, block_size, aperture_size);
{$IFEND}
end;

procedure cvCornerMinEigenVal(const image: pCvArr; eigenval: pCvArr; block_size: int; aperture_size: int = 3);
begin
{$IF not defined(PACKAGE)}
  _cvCornerMinEigenVal(image, eigenval, block_size, aperture_size);
{$IFEND}
end;

procedure cvCornerHarris(const image: pCvArr; harris_response: pCvArr; block_size: int; aperture_size: int = 3; k: double = 0.04);
begin
{$IF not defined(PACKAGE)}
  _cvCornerHarris(image, harris_response, block_size, aperture_size, k);
{$IFEND}
end;

procedure cvFindCornerSubPix(const image: pCvArr; corners: pCvPoint2D32f; count: int; win: TCvSize; zero_zone: TCvSize; criteria: TCvTermCriteria);
begin
{$IF not defined(PACKAGE)}
  _cvFindCornerSubPix(image, corners, count, win, zero_zone, criteria);
{$IFEND}
end;

procedure cvGoodFeaturesToTrack(const image: pCvArr; eig_image: pCvArr; temp_image: pCvArr; corners: pCvPoint2D32f; corner_count: pint; quality_level: double; min_distance: double; const mask: pCvArr = nil; block_size: int = 3; use_harris: int = 0; k: double = 0.04);
begin
{$IF not defined(PACKAGE)}
  _cvGoodFeaturesToTrack(image, eig_image, temp_image, corners, corner_count, quality_level, min_distance, mask, block_size, use_harris, k);
{$IFEND}
end;

function cvHoughLines2(image: pCvArr; line_storage: pvoid; method: int; rho: double; theta: double; threshold: int; param1: double = 0; param2: double = 0; min_theta: double = 0; max_theta: double = CV_PI): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvHoughLines2(image, line_storage, method, rho, theta, threshold, param1, param2, min_theta, max_theta);
{$IFEND}
end;

function cvHoughCircles(image: pCvArr; circle_storage: pvoid; method: int; dp: double; min_dist: double; param1: double = 100; param2: double = 100; min_radius: int = 0; max_radius: int = 0): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvHoughCircles(image, circle_storage, method, dp, min_dist, param1, param2, min_radius, max_radius);
{$IFEND}
end;

procedure cvFitLine(const points: pCvArr; dist_type: int; param: double; reps: double; aeps: double; line: pfloat);
begin
{$IF not defined(PACKAGE)}
  _cvFitLine(points, dist_type, param, reps, aeps, line);
{$IFEND}
end;

procedure cvLine(img: pCvArr; pt1: TCvPoint; pt2: TCvPoint; color: TCvScalar; thickness: int = 1; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvLine(img, pt1, pt2, color, thickness, line_type, shift);
{$IFEND}
end;

procedure cvRectangle(img: pCvArr; pt1: TCvPoint; pt2: TCvPoint; color: TCvScalar; thickness: int = 1; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvRectangle(img, pt1, pt2, color, thickness, line_type, shift);
{$IFEND}
end;

procedure cvRectangleR(img: pCvArr; r: TCvRect; color: TCvScalar; thickness: int = 1; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvRectangleR(img, r, color, thickness, line_type, shift);
{$IFEND}
end;

procedure cvCircle(img: pCvArr; center: TCvPoint; radius: int; color: TCvScalar; thickness: int = 1; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvCircle(img, center, radius, color, thickness, line_type, shift);
{$IFEND}
end;

procedure cvEllipse(img: pCvArr; center: TCvPoint; axes: TCvSize; angle: double; start_angle: double; end_angle: double; color: TCvScalar; thickness: int = 1; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvEllipse(img, center, axes, angle, start_angle, end_angle, color, thickness, line_type, shift);
{$IFEND}
end;

procedure cvFillConvexPoly(img: pCvArr; const pts: pCvPoint; npts: int; color: TCvScalar; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvFillConvexPoly(img, pts, npts, color, line_type, shift);
{$IFEND}
end;

procedure cvFillPoly(img: pCvArr; pts: pCvPoint; const npts: pint; contours: int; color: TCvScalar; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvFillPoly(img, pts, npts, contours, color, line_type, shift);
{$IFEND}
end;

procedure cvPolyLine(img: pCvArr; pts: pCvPoint; const npts: pint; contours: int; is_closed: int; color: TCvScalar; thickness: int = 1; line_type: int = 8; shift: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvPolyLine(img, pts, npts, contours, is_closed, color, line_type, shift);
{$IFEND}
end;

function cvClipLine(img_size: TCvSize; pt1: pCvPoint; pt2: pCvPoint): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvClipLine(img_size, pt1, pt2);
{$IFEND}
end;

function cvInitLineIterator(const image: pCvArr; pt1: TCvPoint; pt2: TCvPoint; line_iterator: pCvLineIterator; connectivity: int = 8; left_to_right: int = 0): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInitLineIterator(image, pt1, pt2, line_iterator, connectivity, left_to_right);
{$IFEND}
end;

procedure cvInitFont(Var font: TCvFont; font_face: int; hscale: double; vscale: double; shear: double = 0; thickness: int = 1; line_type: int = 8);
begin
{$IF not defined(PACKAGE)}
  _cvInitFont(font, font_face, hscale, vscale, shear, thickness, line_type);
{$IFEND}
end;

procedure cvPutText(img: pCvArr; const text: pcvchar; org: TCvPoint; const font: pCvFont; color: TCvScalar);
begin
{$IF not defined(PACKAGE)}
  _cvPutText(img, text, org, font, color);
{$IFEND}
end;

procedure cvGetTextSize(const text_string: pcvchar; const font: pCvFont; text_size: pCvSize; baseline: pint);
begin
{$IF not defined(PACKAGE)}
  _cvGetTextSize(text_string, font, text_size, baseline);
{$IFEND}
end;

function cvColorToScalar(packed_color: double; arrtype: int): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvColorToScalar(packed_color, arrtype);
{$IFEND}
end;

function cvEllipse2Poly(center: TCvPoint; axes: TCvSize; angle: int; arc_start: int; arc_end: int; pts: pCvPoint; delta: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvEllipse2Poly(center, axes, angle, arc_start, arc_end, pts, delta);
{$IFEND}
end;

procedure cvDrawContours(img: pCvArr; contour: pCvSeq; external_color: TCvScalar; hole_color: TCvScalar; max_level: int; thickness: int; line_type: int; offset: TCvPoint);
begin
{$IF not defined(PACKAGE)}
  _cvDrawContours(img, contour, external_color, hole_color, max_level, thickness, line_type, offset);
{$IFEND}
end;

{$IFEND}
