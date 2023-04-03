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

{$IF defined(OPENCV_CORE_MAT_HPP) and (not defined(OPENCV_CORE_MAT_HPP_IMPL))}
{$DEFINE OPENCV_CORE_MAT_HPP_IMPL}
{ TMat }

class operator TMat.Initialize(out Dest: TMat);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_19(Dest);
{$IFEND}
end;

class operator TMat.Finalize(var Dest: TMat);
begin
{$IF not defined(PACKAGE)}
  Destructor_cv_Mat(Dest);
{$IFEND}
end;

class operator TMat.assign(var Dest: TMat; const [ref] Src: TMat);
begin
{$IF not defined(PACKAGE)}
  if @Dest <> @Src then
    operator_CV_MAT_assign_1(Dest, Src);
{$IFEND}
end;

function TMatHelper.setTo(const value: TInputArray; const mask: TInputArray { =noArray() } ): TMat;
begin
{$IF not defined(PACKAGE)}
  // Result := class_func_CV_MAT_setTo(Self, value, mask)^;
  Result := class_func_CV_MAT_setTo(Self, value, mask)^;
{$IFEND}
end;

function TMatHelper.setTo(const value: TInputArray): TMat;
begin
  Result := setTo(value, TInputArray.noArray);
end;

function TMatHelper.reshape(cn: int; newndims: int; const newsz: pInt): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_reshape_2(Self, cn, newndims, newsz);
{$IFEND}
end;

class operator TMatHelper.Implicit(const m: TMatExpr): TMat;
begin
{$IF not defined(PACKAGE)}
  operator_CV_MAT_assign_3(Result, m);
{$IFEND}
end;

function TMatHelper.clone: TMat;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_clone(Self)^;
{$IFEND}
end;

procedure TMatHelper.convertTo(const m: TOutputArray; rtype: int; alpha: double = 1; beta: double = 0);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_convertTo(Self, m, rtype, alpha, beta);
{$IFEND}
end;

procedure TMatHelper.Create(const size: TSize; &type: int);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_create_3(Self, size, &type);
{$IFEND}
end;

function TMatHelper.diag(d: int): TMat;
begin
{$IF not defined(PACKAGE)}
  // diag_Mat(Self, Result, d);
  Result := class_func_CV_MAT_diag(Self, d);
{$IFEND}
end;

function TMatHelper.mat(const roi: TRect): TMat;
begin
  Result := TMat.mat(Self, roi);
end;

function TMatHelper.isContinuous: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_isContinuous(Self);
{$IFEND}
end;

function TMatHelper.isSubmatrix: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_isSubmatrix(Self);
{$IFEND}
end;

class operator TMatHelper.BitwiseAnd(const a, b: TMat): TMat;
begin
{$IF not defined(PACKAGE)}
  bitwise_and(a, b, Result);
{$IFEND}
end;

class operator TMatHelper.BitwiseAnd(const a: TMat; const b: TScalar): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // operator_MatExpr_BitwiseAnd_Mat_Scalar(Result, a, b);
  Result := operator_CV_and_5(a, b);
{$IFEND}
end;

class operator TMatHelper.LogicalNot(const m: TMat): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // operator_MatExpr_LogicalNot_Mat(Result, m);
  Result := operator_CV_not_1(m);
{$IFEND}
end;

class operator TMatHelper.GreaterThanOrEqual(const a: TMat; const b: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // operator_MatExpr_GreaterThanOrEqual_double(Result, a, b);
  Result := operator_CV_great_eq_4(a, b);
{$IFEND}
end;

class operator TMatHelper.Multiply(const a: TMat; const b: double): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // operator_MatExpr_Mul_Mat_double(Result, a, b);
  Result := operator_CV_mul_9(a, b);
{$IFEND}
end;

class operator TMatHelper.Multiply(const a: TMat; const b: TMat): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // operator_MatExpr_Mul_Mat_Mat(Result, a, b);
  Result := operator_CV_mul_7(a, b);
{$IFEND}
end;

class operator TMatHelper.Add(const a: TMat; const s: TScalar): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // operator_MatExpr_Add_Mat_Scalar(Result, a, s);
  Result := operator_CV_plus_10(a, s);
{$IFEND}
end;

class operator TMatHelper.Add(const a: TMat; const s: TMatExpr): TMat;
// Var
// r: TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // operator_MatExpr_Add_Mat_MatExpr(r, a, s);
  // Result := r;
  Result := operator_CV_plus_9(a, s);
{$IFEND}
end;

class function TMatHelper.mat(const size: TSize; &type: int): TMat;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_16(Result, size, &type);
{$IFEND}
end;

class function TMatHelper.mat(const size: TSize; &type: int; const s: TScalar): TMat;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_17(Result, size, &type, s);
{$IFEND}
end;

class function TMatHelper.mat(rows, cols, &type: int): TMat;
begin
  Result.Create(rows, cols, &type);
end;

class function TMatHelper.mat<T>(rows, cols: int; Data: TArray<TArray<T>>; step: size_t): TMat;
Type
  pType = ^T;
Var
  d: TDataType<T>;
  p: pType;
begin
  p := AllocMem(rows * cols * SizeOf(T));
  try
    for Var i := 0 to cols - 1 do
      for Var j := 0 to rows - 1 do
        p[j * cols + i] := Data[j, i];
    Result := TMat.mat(rows, cols, d.&type, p, step);
  finally
    FreeMem(p);
  end;
end;

class function TMatHelper.mat<T>(rows, cols: int; Data: TArray<T>; step: size_t): TMat;
Var
  d: TDataType<T>;
begin
  Result := TMat.mat(rows, cols, d.&type, @Data[0], step);
end;

class function TMatHelper.mat(rows, cols, &type: int; Data: Pointer; step: size_t): TMat;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_12(Result, rows, cols, &type, Data, step);
{$IFEND}
end;

class function TMatHelper.mat: TMat;
begin
{$IF not defined(PACKAGE)}
  // Result := Constructor_Mat(Result)^;
  constructor_CV_MAT_19(Result);
{$IFEND}
end;

class function TMatHelper.mat(const m: TMat; const roi: TRect): TMat;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_2(Result, m, roi);
{$IFEND}
end;

function TMatHelper.pT<T>(const i0: int): Pointer;
Type
  pType = ^T;
begin
  // CV_DbgAssert( y == 0 || (data && dims >= 1 && (unsigned)y < (unsigned)size.p[0]) );
{$WARNINGS OFF}
  Result := (Data + step.p[0] * i0);
{$WARNINGS ON}
end;

function TMatHelper.pT<T>(const i0, i1, i2: int): Pointer;
Type
  pType = ^T;
begin
{$WARNINGS OFF}
  Result := Pointer(Data + i0 * step.p[0] + i1 * step.p[1] + i2 * step.p[2]);
{$WARNINGS ON}
end;

function TMatHelper.pT<T>(const i0, i1: int): Pointer;
begin
{$WARNINGS OFF}
  Result := Pointer(Data + step.p[0] * i0 + i1 * SizeOf(T));
{$WARNINGS ON}
end;

procedure TMatHelper.release;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_release(Self);
{$IFEND}
end;

procedure TMatHelper.st<T>(const i0, i1: int; const v: T);
Type
  pType = ^T;
var
  p: pType;
begin
{$WARNINGS OFF}
  p := pType(Data + step.p[0] * i0);
  p[i1] := v;
{$WARNINGS ON}
end;

procedure TMatHelper.st<T>(const i0: int; const v: T);
Type
  pType = ^T;
begin
  // {$WARNINGS OFF}
  // pType(Data + step.p[0] * i0)^ := v;
  // {$WARNINGS ON}
{$WARNINGS OFF}
  if (isContinuous() or (size.p[0] = 1)) then
  begin
    pType(Data)[i0] := v;
    Exit;
  end;
  if (size.p[1] = 1) then
  begin
    pType(Data + step.p[0] * i0)^ := v; // *(_Tp*)(data + step.p[0] * i0);
    Exit;
  end;
  Var
    i: int := i0 div cols;
  Var
    j: int := i0 - i * cols;
  pType(Data + step.p[0] * i0 + j * SizeOf(T))^ := v;
{$WARNINGS ON}
end;

function TMatHelper.step1(i: int): size_t;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_step1(Self, i);
{$IFEND}
end;

procedure TMatHelper.addref;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_addref(Self);
{$IFEND}
end;

procedure TMatHelper.assign(const s: TScalar);
begin
{$IF not defined(PACKAGE)}
  operator_CV_MAT_assign_2(Self, s);
{$IFEND}
end;

function TMatHelper.at<T>(const i0, i1: int): T;
Type
  pType = ^T;
begin
  // CV_DbgAssert(dims <= 2);
  // CV_DbgAssert(data);
  // CV_DbgAssert((unsigned)i0 < (unsigned)size.p[0]);
  // CV_DbgAssert((unsigned)(i1 * DataType<_Tp>::channels) < (unsigned)(size.p[1] * channels()));
  // CV_DbgAssert(CV_ELEM_SIZE1(traits::Depth<_Tp>::value) == elemSize1());
{$WARNINGS OFF}
  Result := pType(Data + step.p[0] * i0)[i1];
{$WARNINGS ON}
end;

function TMatHelper.at<T>(const i0: int): T;
Type
  pType = ^T;
begin
  // CV_DbgAssert(dims <= 2);
  // CV_DbgAssert(data);
  // CV_DbgAssert((unsigned)i0 < (unsigned)(size.p[0] * size.p[1]));
  // CV_DbgAssert(elemSize() == sizeof(_Tp));
{$WARNINGS OFF}
  if (isContinuous() or (size.p[0] = 1)) then
    Exit(pType(Data)[i0]);
  if (size.p[1] = 1) then
    Exit((pType(Data + step.p[0] * i0))[0]); // *(_Tp*)(data + step.p[0] * i0);
  Var
    i: int := i0 div cols;
  Var
    j: int := i0 - i * cols;
  Exit((pType(Data + step.p[0] * i0))[j]);
{$WARNINGS ON}
end;

function TMatHelper.channels: int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_channels(Self);
{$IFEND}
end;

function TMatHelper.checkVector(elemChannels, depth: int; requireContinuous: BOOL): int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_checkVector(Self, elemChannels, depth, requireContinuous);
{$IFEND}
end;

procedure TMatHelper.Create(rows, cols, &type: int);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_10(Self, rows, cols, &type);
{$IFEND}
end;

function TMatHelper.depth: int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_depth(Self);
{$IFEND}
end;

function TMatHelper.elemSize: size_t;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_elemSize(Self);
{$IFEND}
end;

function TMatHelper.elemSize1: size_t;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_elemSize1(Self);
{$IFEND}
end;

function TMatHelper.empty: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_empty(Self);
{$IFEND}
end;

function TMatHelper.total(startDim, endDim: int): size_t;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_total(Self, startDim, endDim);
{$IFEND}
end;

function TMatHelper.total: size_t;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_total_1(Self);
{$IFEND}
end;

function TMatHelper.&type: int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_MAT_type(Self);
{$IFEND}
end;

class function TMatHelper.zeros(const rows, cols: int; &type: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // zeros_Mat(Result, rows, cols, &type);
  class_proc_CV_MAT_zeros(Result, rows, cols, &type);
{$IFEND}
end;

class function TMatHelper.zeros(const size: TSize; &type: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  // zeros_Mat(Result, size, &type);
  class_proc_CV_MAT_zeros_2(Result, size, &type);
{$IFEND}
end;

class function TMatHelper.mat<T>(const vec: Vector<T>; copyData: BOOL): TMat;
begin
  Result.Create(vec.size, 1, TTraits.&type<T>.value);
  if (vec.empty()) then
    Exit(TMat.mat);
  if (not copyData) then
  begin
    Result.step[0] := SizeOf(T);
    Result.step[1] := SizeOf(T);
    Result.datastart := vec.pT(0);
    Result.Data := vec.pT(0);
    Result.datalimit := Result.datastart + Result.rows * Result.step[0];
    Result.dataend := Result.datastart + Result.rows * Result.step[0];
  end
  else
    TMat.mat(vec.size, 1, TTraits.&type<T>.value, vec.pT(0)).copyTo(Result);
end;

procedure TMatHelper.copyTo(m: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_copyTo(Self, m);
{$IFEND}
end;

procedure TMatHelper.copyTo(m: TOutputArray; mask: TInputArray);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_copyTo_1(Self, m, mask);
{$IFEND}
end;

class function TMatHelper.eye(rows, cols, &type: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_eye(Result, rows, cols, &type);
{$IFEND}
end;

class function TMatHelper.eye(size: TSize; &type: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_eye_1(Result, size, &type);
{$IFEND}
end;

class operator TMatHelper.Implicit(const v: Vector<TPoint>): TMat;
begin
  Result := TMat.mat<TPoint>(v);
end;

class function TMatHelper.ones(rows, cols, &type: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_ones(Result, rows, cols, &type);
{$IFEND}
end;

class function TMatHelper.mat(rows, cols, &type: int; const s: TScalar): TMat;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_11(Result, rows, cols, &type, s);
{$IFEND}
end;

class function TMatHelper.ones(ndims: int; const sz: pInt; &type: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_ones_1(Result, ndims, sz, &type);
{$IFEND}
end;

class function TMatHelper.ones(size: TSize; &type: int): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_MAT_ones_2(Result, size, &type);
{$IFEND}
end;

class operator TMatHelper.Subtract(const m: TMat; const s: TScalar): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  operator_CV_minus_12(Result, m, s);
{$IFEND}
end;

function TMatHelper.ptr(const i0: int): puchar;
begin
  Result := pT<uchar>(i0);
end;

function TMatHelper.ptr(const row, col: int): puchar;
begin
  Result := pT<uchar>(row, col);
end;

function TMatHelper.ptr(const i0, i1, i2: int): puchar;
begin
  Result := pT<uchar>(i0, i1, i2);
end;

function TMatHelper.ptr(const idx: pInt): puchar;
begin
  Var
    i: int;
  Var
    d: int := dims;
  Result := Data;
  // CV_DbgAssert( d >= 1 && p );
  for i := 0 to d - 1 do
  begin
    // CV_DbgAssert( (unsigned)idx[i] < (unsigned)size.p[i] );
    Result := Result + UInt64(idx[i]) * UInt64(step.p[i]);
  end;
end;

class function TMatHelper.mat(const m: TMat; const rowRange: TRange; const colRange: TRange { =Range::all() } ): TMat;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MAT_4(Result, m, rowRange, colRange);
{$IFEND}
end;

class function TMatHelper.mat(const m: TMat; const rowRange: TRange): TMat;
begin
  Result := TMat.mat(m, rowRange, TRange.All);
end;

class function TMatHelper.mat(const m: TMat): TMat;
begin
  Result := m;
end;

class operator TMatHelper.Add(const a: TMatExpr; const s: TMat): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  operator_CV_plus_5(Result, a, s);
{$IFEND}
end;

class operator TMatHelper.Add(const a, b: TMat): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  operator_CV_plus_8(Result, a, b);
{$IFEND}
end;

{ TMatExpr }

class operator TMatExpr.assign(var Dest: TMatExpr; const [ref] Src: TMatExpr);
begin
{$IF not defined(PACKAGE)}
  operator_CV_MATEXPR_assign_1(Dest, Src);
{$IFEND}
end;

class operator TMatExpr.Finalize(var Dest: TMatExpr);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_MATEXPR(Dest);
{$IFEND}
end;

class operator TMatExpr.Initialize(out Dest: TMatExpr);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_MATEXPR_4(Dest);
{$IFEND}
end;

class operator TMatExpr.Multiply(const a: TMatExpr; const b: double): TMatExpr;
begin
{$IF not defined(PACKAGE)}
  operator_CV_mul_6(Result, a, b);
{$IFEND}
end;

function TMatExprHelper.size: TSize;
begin
{$IF not defined(PACKAGE)}
  Var
    T: UInt64;
  class_func_CV_MATEXPR_size(Self, T);
  Result := T;
{$IFEND}
end;

{ TInputArray }

class function TInputArray.InputArray(const m: TMat): TInputArray;
begin
{$IF not defined(PACKAGE)}
  constructor_CV__INPUTARRAY_8(Result, m);
{$IFEND}
end;

class operator TInputArray.Implicit(const m: TMat): TInputArray;
begin
  Result := TInputArray.InputArray(m);
end;

class function TInputArray.InputArray(const m: TMatExpr): TInputArray;
begin
{$IF not defined(PACKAGE)}
  constructor_CV__INPUTARRAY_9(Result, m);
{$IFEND}
end;

class operator TInputArray.Implicit(const m: TMatExpr): TInputArray;
begin
  Result := TInputArray.InputArray(m);
end;

function TInputArray.getMat(idx: int): TMat;
begin
{$IF not defined(PACKAGE)}
  class_func_CV__INPUTARRAY_getMat(Self, Result, idx);
{$IFEND}
end;

function TInputArray.getObj: Pointer;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV__INPUTARRAY_getObj(Self, Result);
{$IFEND}
end;

class operator TInputArray.Initialize(out Dest: TInputArray);
begin
{$IF not defined(PACKAGE)}
  constructor_CV__INPUTARRAY_12(Dest);
{$IFEND}
end;

class operator TInputArray.Finalize(var Dest: TInputArray);
begin
{$IF not defined(PACKAGE)}
  destructor_CV__INPUTARRAY(Dest);
{$IFEND}
end;

function TInputArray.isMat: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV__INPUTARRAY_isMat(Self);
{$IFEND}
end;

class function TInputArray.noArray: TInputArray;
begin
{$IF not defined(PACKAGE)}
  // noArray_InputOutputArray(TInputOutputArray(Result));
  Result := TInputOutputArray(func_noArray()^);
{$IFEND}
end;

class operator TInputArray.Implicit(const v: Vector < Vector < TPoint >> ): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR_VECTOR) + TTraits.&type<TPoint>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: Vector<TPoint2f>): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TPoint2f>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: Vector<TPoint>): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TPoint>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: Vector<uchar>): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<uchar>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: double): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(FIXED_SIZE) + int(MATX) + int(CV_64F) + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(1, 1);
end;

class operator TInputArray.Implicit(const v: Vector<TVec4i>): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TVec4i>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: Vector<TMat>): TInputArray;
begin
  Result.flags := int(STD_VECTOR_MAT) + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: TScalar): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(FIXED_SIZE) + int(MATX) + TTraits.&type<double>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(1, 4);
end;

class operator TInputArray.Implicit(const v: Vector<float>): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<float>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(1, 4);
end;

class operator TInputArray.Implicit(const v: TVec3b): TInputArray;
begin
  // { init(FIXED_TYPE + FIXED_SIZE + MATX + traits::Type<_Tp>::value + ACCESS_READ, &mtx, Size(n, m)); }
  Result.flags := int(FIXED_TYPE) + int(MATX) + TTraits.&type<uchar>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(1, 3);
end;

class operator TInputArrayHelper.Implicit(const v: TMat1i): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(mat) + TTraits.&type<int>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArrayHelper.Implicit(const v: TMat1f): TInputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(mat) + TTraits.&type<float>.value + int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

{ TOutputArray }

class function TOutputArrayHelper.noArray: TOutputArray;
begin
{$IF not defined(PACKAGE)}
  // noArray_InputOutputArray(TInputOutputArray(Result));
  Result := TOutputArray(func_noArray()^);
{$IFEND}
end;

class function TOutputArrayHelper.OutputArray(const vec: Vector<TMat>): TOutputArray;
begin
{$IF not defined(PACKAGE)}
  constructor_CV__OUTPUTARRAY_1(Result, vec);
{$IFEND}
end;

class function TOutputArrayHelper.OutputArray(const m: TMat): TOutputArray;
begin
{$IF not defined(PACKAGE)}
  constructor_CV__OUTPUTARRAY_6(Result, m);
{$IFEND}
end;

class operator TOutputArrayHelper.Implicit(const m: TMat): TOutputArray;
begin
  Result := TOutputArray.OutputArray(m);
end;

class operator TOutputArrayHelper.Implicit(const OA: TOutputArray): TMat;
begin
  Assert(TInputArray(OA).isMat);
  Result := pMat(TInputArray(OA).getObj)^;
end;

class operator TOutputArrayHelper.Implicit(const m: Vector<TMat>): TOutputArray;
begin
  Result := TOutputArray.OutputArray(m);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<TPoint>): TOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TPoint>.value + int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<TPoint2f>): TOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TPoint2f>.value + int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<uchar>): TOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<uchar>.value + int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<float>): TOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<float>.value + int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<TVec4i>): TOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TVec4i>.value + int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector < Vector < TPoint >> ): TOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR_VECTOR) + TTraits.&type<TPoint>.value + int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

{ TInputOutputArray }

class operator TInputOutputArrayHelper.Implicit(const IOA: TInputOutputArray): TMat;
Type
  pMat = ^TMat;
begin
  Assert(IOA.isMat);
  Result := pMat(IOA.getObj)^;
end;

class operator TInputOutputArrayHelper.Implicit(const m: TMat): TInputOutputArray;
begin
  Result.InputOutputArray(m);
end;

class operator TInputOutputArrayHelper.Implicit(const m: TMat1f): TInputOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(mat) + TTraits.&type<float>.value + int(ACCESS_RW);
  Result.Obj := @m;
  Result.sz := size(0, 0);
end;

procedure TInputOutputArrayHelper.InputOutputArray(const m: TMat);
begin
{$IF not defined(PACKAGE)}
  constructor_CV__INPUTOUTPUTARRAY_5(Self, m);
{$IFEND}
end;

class function TInputOutputArrayHelper.noArray: TInputOutputArray;
begin
{$IF not defined(PACKAGE)}
  // noArray_InputOutputArray(Result);
  Result := TInputOutputArray(func_noArray()^);
{$IFEND}
end;

class operator TInputOutputArrayHelper.Implicit(const IOA: TInputOutputArray): TInputArray;
begin
  Move(IOA, Result, SizeOf(IOA));
end;

class operator TInputOutputArrayHelper.Implicit(const v: Vector<TPoint2f>): TInputOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TPoint2f>.value + int(ACCESS_RW);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputOutputArrayHelper.Implicit(const v: Vector<TPoint>): TInputOutputArray;
begin
  Result.flags := int(FIXED_TYPE) + int(STD_VECTOR) + TTraits.&type<TPoint>.value + int(ACCESS_RW);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

{ TInputArrayOfArraysHelper }

class operator TInputArrayOfArraysHelper.Implicit(const v: Vector < Vector < TPoint >> ): TInputArrayOfArrays;
begin
  Result := TInputArrayOfArrays(TInputArray(v));
end;

class operator TInputArrayOfArraysHelper.Implicit(const v: Vector<TMat>): TInputArrayOfArrays;
begin
  Result := TInputArrayOfArrays(TInputArray(v));
end;

class operator TInputArrayOfArraysHelper.Implicit(const v: Vector<TPoint>): TInputArrayOfArrays;
begin
  Result := TInputArrayOfArrays(TInputArray(v));
end;

{ TMatSize }

class operator TMatSizeHelper.Implicit(const m: TMatSize): TSize;
begin
{$IF not defined(PACKAGE)}
  Var
    T: UInt64;
  operator_CV_MATSIZE_explicit(m, T);
  Result := T;
{$IFEND}
end;

{ TMatStep }

function TMatStepHelper.GetItems(const index: int): int;
begin
  Result := p[index];
end;

procedure TMatStepHelper.setItems(const index, value: int);
begin
  p[index] := value;
end;

{ TMat_<T> }

class operator TMat_<T>.assign(var Dest: TMat_<T>; const [ref] Src: TMat_<T>);
begin
  Dest.FMat := Src.FMat;
end;

procedure TMat_<T>.assign(const m: TMat);
begin
  if (m.empty()) then
    FMat.release()
  else if (TTraits.&type<T>.value = m.&type()) then
    FMat := m
  else if (TTraits.depth<T>.value = m.depth()) then
    FMat := m.reshape(TDataType<T>.channels, m.dims, nil)
  else
  begin
    Assert((TDataType<T>.channels = m.channels()) or m.empty());
    m.convertTo(&FMat, FMat.&type);
  end;
end;

class operator TMat_<T>.Finalize(var Dest: TMat_<T>);
begin
  Finalize(Dest.FMat);
end;

class operator TMat_<T>.Initialize(out Dest: TMat_<T>);
begin
  // FMat := TMat.Mat(_rows, _cols, traits::Type<_Tp>::value)
  With Dest.FMat do
    flags := (flags and (not CV_MAT_TYPE_MASK)) + TTraits.&type<T>.value;
end;

class function TMat_<T>.mat(const m: TMat): TMat_<T>;
begin
  With Result.FMat do
    flags := (flags and (not CV_MAT_TYPE_MASK)) + TTraits.&type<T>.value;
  Result.assign(m);
end;

class function TMat_<T>.mat(_rows: int; _cols: int): TMat_<T>;
begin
  Result.FMat := TMat.mat(_rows, _cols, TTraits.&type<T>.value);
end;

function TMat_<T>.&type(): int;
begin
  Assert(FMat.&type() = TTraits.&type<T>.value);
  Result := TTraits.&type<T>.value;
end;

function TMat_<T>.pT(row: int; col: int): pType;
begin
  With FMat do
  begin
    Assert(dims <= 2);
    Assert(Assigned(Data));
    Assert(unsigned(row) < unsigned(size.p[0]));
    Assert(unsigned(col) < unsigned(size.p[1]));
    Assert(&type() = TTraits.&type<T>.value);
{$WARNINGS OFF}
    // Result := pType(@(data + step.p[0] * row)[col]);
    Result := pType(FMat.Data + step.p[0] * row + col * SizeOf(T));
{$WARNINGS ON}
  end;
end;

function TMat_<T>.row(y: int): TMat_<T>;
begin
  Result := TMat_<T>.mat(Self, Range(y, y + 1), TRange.All);
end;

class function TMat_<T>.mat(const m: TMat_<T>; const rowRange: TRange; const colRange: TRange { = Range::all() } ): TMat_<T>;
begin
  Result.FMat := TMat.mat(m.FMat, rowRange, colRange);
end;

class function TMat_<T>.mat(const m: TMat_<T>; const rowRange: TRange): TMat_<T>;
begin
  Result := TMat_<T>.mat(m, rowRange, TRange.All);
end;

class operator TMat_<T>.LeftShift(const a: TMat_<T>; const b: TArray<T>): TMat_<T>;
begin
  Result := a;
  for var i := 0 to System.Math.min(Result.cols * Result.rows - 1, High(b)) do
    Result.FMat.st<T>(i, b[i]);
end;

class operator TMat_<T>.Implicit(const a: TMat_<T>): TMat;
begin
  Result := a.FMat;
end;

{$IFEND}
