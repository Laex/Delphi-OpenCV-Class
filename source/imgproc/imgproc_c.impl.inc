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
{$DEFINE OPENCV_IMGPROC_IMGPROC_C_H_IMPL}

procedure cvCopyMakeBorder(const src: pCvArr; dst: pCvArr; offset: TCvPoint; bordertype: int);
begin
  cvCopyMakeBorder(src, dst, offset, bordertype, cvScalarAll(0));
end;

procedure cvFilter2D(const src: pCvArr; dst: pCvArr; const kernel: pCvMat);
begin
  cvFilter2D(src, dst, kernel, cvPoint(-1, -1));
end;

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

procedure cvDrawContours(img: pCvArr; contour: pCvSeq; external_color: TCvScalar; hole_color: TCvScalar; max_level: int; thickness: int;
  line_type: int);
begin
  cvDrawContours(img, contour, external_color, hole_color, max_level, thickness, line_type, cvPoint(0, 0));
end;

{$IFEND}
