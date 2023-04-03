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

{$IF defined(OPENCV_CORE_TYPES_H) and (not defined(OPENCV_CORE_TYPES_H_IMPL))}
{$DEFINE OPENCV_CORE_TYPES_H_IMPL}
{$IF not defined(OPENCV_CORE_CVDEF_H_IMPL)}{$INCLUDE 'core/cvdef.impl.inc'}{$IFEND}
{$IF not defined(OPENCV_CORE_HPP_IMPL)}{$INCLUDE 'core.impl.inc'}{$IFEND}

function cvRNG(const seed: Int64 = -1): TCvRNG;
begin
  if seed <> 0 then
    Result := TCvRNG(seed)
  else
    Result := TCvRNG(-1);
end;

function cvRandInt(Var rng: TCvRNG): unsigned;
begin
  Var
    temp: uint64 := rng;
  temp := uint64(unsigned(temp) * CV_RNG_COEFF) + (temp shl 32);
  rng := temp;
  Result := unsigned(temp);
end;

function cvRandReal(Var rng: TCvRNG): double; inline;
begin
  Result := cvRandInt(rng) * 2.3283064365386962890625E-10; (* 2^-32 *)
end;

function cvIplImage(): TIplImage;
begin
  Result := default (TIplImage);
end;

function CV_IS_MAT_HDR(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MAT_HDR(mat) \
  // ((mat) != NULL && \
  // (((const CvMat*)(mat))->type & CV_MAGIC_MASK) == CV_MAT_MAGIC_VAL && \
  // ((const CvMat*)(mat))->cols > 0 && ((const CvMat*)(mat))->rows > 0)
  Result :=          //
    (mat <> nil) and //
    ((pCvMat(mat)^.&type and CV_MAGIC_MASK) = CV_MAT_MAGIC_VAL) and //
    ((pCvMat(mat)^.cols > 0) and (pCvMat(mat)^.rows > 0));
end;

function CV_IS_MAT(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MAT(mat) \
  // (CV_IS_MAT_HDR(mat) && ((const CvMat*)(mat))->data.ptr != NULL)
  Result := CV_IS_MAT_HDR(mat) and (pCvMat(mat)^.data.ptr <> nil);
end;

function CV_IS_MATND_HDR(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MATND_HDR(mat) \
  // ((mat) != NULL && (((const CvMatND*)(mat))->type & CV_MAGIC_MASK) == CV_MATND_MAGIC_VAL)
  Result :=          //
    (mat <> nil) and //
    ((pCvMatND(mat)^.&type and CV_MAGIC_MASK) = CV_MATND_MAGIC_VAL);
end;

function CV_IS_MATND(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MATND(mat) \
  // (CV_IS_MATND_HDR(mat) && ((const CvMatND*)(mat))->data.ptr != NULL)
  Result := CV_IS_MATND_HDR(mat) and (pCvMatND(mat)^.data.ptr <> nil);
end;

function cvRealScalar(val0: double): TCvScalar;
begin
  Result.val[0] := val0;
  Result.val[1] := 0;
  Result.val[2] := 0;
  Result.val[3] := 0;
end;

function CV_IS_SET_ELEM(const ptr: pCvArr): Boolean;
begin
  // #define CV_IS_SET_ELEM( ptr )  (((CvSetElem*)(ptr))->flags >= 0)
  Result := pCvSetElem(ptr)^.flags >= 0;
end;

(* ************************************ CvSlice ***************************************** *)

function cvSlice(start: int; &end: int): TCvSlice;
begin
  Result := _cvSlice.cvSlice(start, &end);
end;

class function _cvSlice.cvSlice(start: int = 0; &end: int = 0): _cvSlice;
begin
  Result.start_index := start;
  Result.end_index := &end;
end;

{ _IplImage }

class operator _IplImage.Initialize(out Dest: _IplImage);
begin
  // memset(this, 0, sizeof(*this));  // valid for POD structure
  Dest.nSize := sizeof(TIplImage);
end;

class operator _IplImage.Explicit(const m: TMat): _IplImage;
begin
  Result := cvIplImage(m);
end;

{ _CvScalar }

function cvScalar(val0: double; val1: double = 0; val2: double = 0; val3: double = 0): TCvScalar;
begin
  Result := _CvScalar.cvScalar(val0, val1, val2, val3);
end;

class function _CvScalar.cvScalar(d0: double; d1: double = 0; d2: double = 0; d3: double = 0): _CvScalar;
begin
  { val[0] = d0; val[1] = d1; val[2] = d2; val[3] = d3; }
  With Result do
  begin
    val[0] := d0;
    val[1] := d1;
    val[2] := d2;
    val[3] := d3;
  end;
end;

(* ***************************************************************************************\
  *                                  Matrix type (CvMat)                                   *
  \*************************************************************************************** *)

function cvMat(const m: TMat): TCvMat;
begin
  Assert(m.dims <= 2);
  Result := cvMat(m.rows, ifthen(m.dims = 1, 1, m.cols), m.&type(), m.data);
  Result.step := int(m.step[0]);
  Result.&type := (Result.&type and (not TMat.CONTINUOUS_FLAG)) or (m.flags and TMat.CONTINUOUS_FLAG);
end;

function cvMat(): TCvMat;
begin
  Result := default (TCvMat);
end;

function cvMat(const m: TCvMat): TCvMat;
begin
  Result := TCvMat(m);
end;

function cvmGet(const mat: pCvMat; row, col: int): double;
begin
  var
    &type: int;

  &type := CV_MAT_TYPE(mat^.&type);
  Assert((unsigned(row) < unsigned(mat^.rows)) and (unsigned(col) < unsigned(mat^.cols)));

  if (&type = CV_32FC1) then
    Result := pfloat(mat^.data.ptr + mat^.step * row)[col]
  else
  begin
    Assert(&type = CV_64FC1);
    Result := pdouble(mat^.data.ptr + mat^.step * row)[col];
  end;
end;

procedure cvmSet(Var mat: TCvMat; row, col: int; value: double);
begin
  var
    &type: int;
  &type := CV_MAT_TYPE(mat.&type);
  Assert((unsigned(row) < unsigned(mat.rows)) and (unsigned(col) < unsigned(mat.cols)));
  if (&type = CV_32FC1) then
    pfloat(mat.data.ptr + mat.step * row)[col] := float(value)
  else
  begin
    Assert(&type = CV_64FC1);
    pdouble(mat.data.ptr + mat.step * row)[col] := value;
  end
end;

function cvIplDepth(&type: int): int;
begin
  Var
    depth: int := CV_MAT_DEPTH(&type);
  Result :=                         //
    (CV_ELEM_SIZE1(depth) * 8) or   //
    (ifthen(depth = CV_8S, 1, 0) or //
    ifthen(depth = CV_16S, 1, 0) or //
    ifthen(depth = CV_32S, IPL_DEPTH_SIGN, 0));
end;

class operator _CvMat.Explicit(const m: TMat): _CvMat;
begin
  Result := cvMat(m);
end;

function TPointerHelper.get_puchar: puchar;
begin
  Result := puchar(self);
end;

procedure TPointerHelper.set_puchar(const value: puchar);
begin
  self := value;
end;

function TPointerHelper.get_pshort: pshort;
begin
  Result := pshort(self);
end;

procedure TPointerHelper.set_pshort(const value: pshort);
begin
  self := value;
end;

function TPointerHelper.get_pInt: pInt;
begin
  Result := pInt(self);
end;

procedure TPointerHelper.set_pInt(const value: pInt);
begin
  self := value;
end;

function TPointerHelper.get_pfloat: pfloat;
begin
  Result := pfloat(self);
end;

procedure TPointerHelper.set_pfloat(const value: pfloat);
begin
  self := value;
end;

function TPointerHelper.get_pdouble: pdouble;
begin
  Result := pdouble(self);
end;

procedure TPointerHelper.set_pdouble(const value: pdouble);
begin
  self := value;
end;

(* ***************************************************************************************\
  *                       Multi-dimensional dense array (CvMatND)                          *
  \*************************************************************************************** *)

function cvMatND(const m: TMat): TCvMatND;
begin
{$IF not defined(PACKAGE)}
  Result := func_cvMatND(m);
{$IFEND}
end;

function cvMatND(): TCvMatND;
begin
  Result := default (TCvMatND);
end;

class operator _CvMatND.Explicit(const m: TMat): _CvMatND;
begin
  Result := cvMatND(m);
end;

(* ***************************************************************************************\
  *                                  Image type (IplImage)                                 *
  \*************************************************************************************** *)

function cvIplImage(const m: TMat): TIplImage;
begin
{$IF not defined(PACKAGE)}
  Result := func_cvIplImage(m);
{$IFEND}
end;

function CV_IS_IMAGE_HDR(const img: pCvArr): Boolean;
begin
  // ((img) != NULL && ((const IplImage*)(img))->nSize == sizeof(IplImage))
  Result := Assigned(img) and (pIplImage(img)^.nSize = sizeof(TIplImage));
end;

function CV_IS_IMAGE(const img: pCvArr): Boolean;
begin
  // (CV_IS_IMAGE_HDR(img) && ((IplImage*)img)->imageData != NULL)
  Result := CV_IS_IMAGE_HDR(img) and (pIplImage(img)^.imageData <> nil);
end;

function cvMat(rows, cols, &type: int; const data: pointer = nil): TCvMat;
begin
  Assert(unsigned(CV_MAT_DEPTH(&type)) <= CV_64F);
  &type := CV_MAT_TYPE(&type);
  Result.&type := CV_MAT_MAGIC_VAL or CV_MAT_CONT_FLAG or &type;
  Result.cols := cols;
  Result.rows := rows;
  Result.step := Result.cols * CV_ELEM_SIZE(&type);
  Result.data.ptr := puchar(data);
  Result.refcount := nil;
  Result.hdr_refcount := 0;
end;

function cvScalarAll(val0123: double): TCvScalar;
begin
  Result.val[0] := val0123;
  Result.val[1] := val0123;
  Result.val[2] := val0123;
  Result.val[3] := val0123;
end;

(* ****************************** CvPoint and variants ********************************** *)

class function _CvPoint.CvPoint(const _x: int = 0; const _y: int = 0): _CvPoint;
begin
  Result.x := _x;
  Result.y := _y;
end;

function CvPoint(x, y: int): TCvPoint;
begin
  Result := _CvPoint.CvPoint(x, y);
end;

function CvPoint(const pt: TPoint): TCvPoint;
begin
  Result := CvPoint(pt.x, pt.y);
end;

(* ********************************** CvTermCriteria ************************************ *)
class function _CvTermCriteria.CvTermCriteria(_type: int = 0; _iter: int = 0; _eps: double = 0): _CvTermCriteria;
begin
  With Result do
  begin
    &type := _type;
    max_iter := _iter;
    epsilon := _eps;
  end;
end;

function CvTermCriteria(&type: int; max_iter: int; epsilon: double): TcvTermCriteria;
begin
  Result := _CvTermCriteria.CvTermCriteria(&type, max_iter, epsilon);
end;

function CvTermCriteria(const t: TTermCriteria): TcvTermCriteria;
begin
  Result := CvTermCriteria(t.&type, t.maxCount, t.epsilon);
end;

(* ******************************* CvSize's & CvBox ************************************* *)

function cvSize(const w: int = 0; const h: int = 0): _CvSize;
begin
  Result := _CvSize.cvSize(w, h);
end;

class function _CvSize.cvSize(const w: int = 0; const h: int = 0): _CvSize;
begin
  Result.width := w;
  Result.height := h;
end;

function cvPointFrom32f(const point: TCvPoint2D32f): TCvPoint;
begin
  Result := CvPoint(cvRound(point.x), cvRound(point.y));
end;

{$IFEND}
