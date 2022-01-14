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

{$IFDEF OPENCV_CORE_MAT_HPP}
{ TMat }

class operator TMat.Initialize(out Dest: TMat);
begin
  Constructor_Mat(Dest);
end;

class operator TMat.Finalize(var Dest: TMat);
begin
  Destructor_Mat(Dest);
end;

class operator TMat.assign(var Dest: TMat; const [ref] Src: TMat);
begin
  if @Dest <> @Src then
    Operator_Mat_Assign_Const_Mat(Dest, Src);
end;

function TMatHelper.setTo(const value: TInputArray; const mask: TInputArray { =noArray() } ): TMat;
begin
  Result := setTo_Mat(Self, value, mask)^;
end;

function TMatHelper.setTo(const value: TInputArray): TMat;
begin
  Result := setTo(value, TInputArray.noArray);
end;

function TMatHelper.reshape(cn: int; newndims: int; const newsz: pInt): TMat;
begin
  Result := reshape_Mat(Self, cn, newndims, newsz);
end;

class operator TMatHelper.Implicit(const m: TMatExpr): TMat;
begin
  Operator_Mat_Assign_MatExpr(Result, m);
end;

function TMatHelper.clone: TMat;
begin
  clone_Mat(Self, Result);
end;

procedure TMatHelper.convertTo(const m: TOutputArray; rtype: int; alpha: double = 1; beta: double = 0);
begin
  convertTo_Mat(Self, m, rtype, alpha, beta);
end;

procedure TMatHelper.Create(const size: TSize; &type: int);
begin
  create_Mat(Self, size, &type);
end;

function TMatHelper.diag(d: int): TMat;
begin
  diag_Mat(Self, Result, d);
end;

function TMatHelper.mat(const roi: TRect): TMat;
begin
  Result := TMat.mat(Self, roi);
end;

function TMatHelper.isContinuous: BOOL;
begin
  Result := isContinuous_Mat(Self);
end;

function TMatHelper.isSubmatrix: BOOL;
begin
  Result := isSubmatrix_Mat(Self);
end;

class operator TMatHelper.BitwiseAnd(const a, b: TMat): TMat;
begin
  bitwise_and(a, b, Result);
end;

class operator TMatHelper.BitwiseAnd(const a: TMat; const b: TScalar): TMatExpr;
begin
  operator_MatExpr_BitwiseAnd_Mat_Scalar(Result, a, b);
end;

class operator TMatHelper.LogicalNot(const m: TMat): TMatExpr;
begin
  operator_MatExpr_LogicalNot_Mat(Result, m);
end;

class operator TMatHelper.GreaterThanOrEqual(const a: TMat; const b: int): TMatExpr;
begin
  operator_MatExpr_GreaterThanOrEqual_double(Result, a, b);
end;

class operator TMatHelper.Multiply(const a: TMat; const b: double): TMatExpr;
begin
  operator_MatExpr_Mul_Mat_double(Result, a, b);
end;

class operator TMatHelper.Add(const a: TMat; const s: TScalar): TMatExpr;
begin
  operator_MatExpr_Add_Mat_Scalar(Result, a, s);
end;

class function TMatHelper.mat(const size: TSize; &type: int): TMat;
begin
  Result := Constructor_Mat(Result, size, &type)^;
end;

class function TMatHelper.mat(const size: TSize; &type: int; const s: TScalar): TMat;
begin
  Constructor_Mat(Result, size, &type, s);
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
  Result := Constructor_Mat(Result, rows, cols, &type, Data, step)^;
end;

class function TMatHelper.mat: TMat;
begin
  // Result := Constructor_Mat(Result)^;
  Constructor_Mat(Result);
end;

class function TMatHelper.mat(const m: TMat; const roi: TRect): TMat;
begin
  Result := Constructor_Mat(Result, m, roi)^;
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
  release_Mat(Self);
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
  Result := step1_Mat(Self, i);
end;

procedure TMatHelper.addref;
begin
  addref_Mat(Self);
end;

procedure TMatHelper.assign(const s: TScalar);
begin
  Operator_Mat_Assign_Scalar(Self, s);
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
  Result := channels_Mat(Self);
end;

function TMatHelper.checkVector(elemChannels, depth: int; requireContinuous: BOOL): int;
begin
  Result := checkVector_Mat(Self, elemChannels, depth, requireContinuous);
end;

procedure TMatHelper.Create(rows, cols, &type: int);
begin
  Constructor_Mat(Self, rows, cols, &type);
end;

function TMatHelper.depth: int;
begin
  Result := depth_Mat(Self);
end;

function TMatHelper.elemSize: size_t;
begin
  Result := elemSize_Mat(Self);
end;

function TMatHelper.elemSize1: size_t;
begin
  Result := elemSize1_Mat(Self);
end;

function TMatHelper.empty: BOOL;
begin
  Result := empty_Mat(Self);
end;

function TMatHelper.total(startDim, endDim: int): size_t;
begin
  Result := total_Mat(Self, startDim, endDim);
end;

function TMatHelper.total: size_t;
begin
  Result := total_Mat(Self);
end;

function TMatHelper.&type: int;
begin
  Result := type_Mat(Self);
end;

class function TMatHelper.zeros(const rows, cols: int; &type: int): TMatExpr;
begin
  zeros_Mat(Result, rows, cols, &type);
end;

class function TMatHelper.zeros(const size: TSize; &type: int): TMatExpr;
begin
  zeros_Mat(Result, size, &type);
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
  copyTo_Mat(Self, m);
end;

procedure TMatHelper.copyTo(m: TOutputArray; mask: TInputArray);
begin
  copyTo_Mat(Self, m, mask);
end;

class function TMatHelper.eye(rows, cols, &type: int): TMatExpr;
begin
  eye_Mat(Result, rows, cols, &type);
end;

class function TMatHelper.eye(size: TSize; &type: int): TMatExpr;
begin
  eye_Mat(Result, size, &type);
end;

class operator TMatHelper.Implicit(const v: Vector<TPoint>): TMat;
begin
  Result := TMat.mat<TPoint>(v);
end;

class function TMatHelper.ones(rows, cols, &type: int): TMatExpr;
begin
  Result := ones_Mat(Result, rows, cols, &type)^;
end;

class function TMatHelper.mat(rows, cols, &type: int; const s: TScalar): TMat;
begin
  Result := Constructor_Mat(Result, rows, cols, &type, s)^;
end;

class function TMatHelper.ones(ndims: int; const sz: pInt; &type: int): TMatExpr;
begin
  Result := ones_Mat(Result, ndims, sz, &type)^;
end;

class function TMatHelper.ones(size: TSize; &type: int): TMatExpr;
begin
  Result := ones_Mat(Result, size, &type)^;
end;

class operator TMatHelper.Subtract(const m: TMat; const s: TScalar): TMatExpr;
begin
  operator_MatExpr_Subtract_Mat_MatExpr(Result, m, s);
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

{ TMatExpr }

class operator TMatExpr.assign(var Dest: TMatExpr; const [ref] Src: TMatExpr);
begin
  Operator_MatExpr_Assign_Const_MatExpr(Dest, Src);
end;

class operator TMatExpr.Finalize(var Dest: TMatExpr);
begin
  Destructor_MatExpr(Dest);
end;

class operator TMatExpr.Initialize(out Dest: TMatExpr);
begin
  constructor_MatExpr(Dest);
end;

function TMatExprHelper.size: TSize;
begin
  MatExpr_size(Self, Result);
end;

{ TInputArray }

class function TInputArray.InputArray(const m: TMat): TInputArray;
begin
  Constructor_InputArray(Result, m);
end;

class operator TInputArray.Implicit(const m: TMat): TInputArray;
begin
  Result := TInputArray.InputArray(m);
end;

class function TInputArray.InputArray(const m: TMatExpr): TInputArray;
begin
  Constructor_InputArray(Result, m);
end;

class operator TInputArray.Implicit(const m: TMatExpr): TInputArray;
begin
  Result := TInputArray.InputArray(m);
end;

function TInputArray.getMat(idx: int): TMat;
begin
  getMat_InputArray(Self, Result, idx);
end;

function TInputArray.getObj: Pointer;
begin
  Result := getObj_InputArray(Self);
end;

class operator TInputArray.Initialize(out Dest: TInputArray);
begin
  Constructor_InputArray(Dest);
end;

class operator TInputArray.Finalize(var Dest: TInputArray);
begin
  Destructor_InputArray(Dest);
end;

function TInputArray.isMat: BOOL;
begin
  Result := isMat_InputArray(Self);
end;

class function TInputArray.noArray: TInputArray;
begin
  noArray_InputOutputArray(TInputOutputArray(Result));
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
{ TOutputArray }

class function TOutputArrayHelper.noArray: TOutputArray;
begin
  noArray_InputOutputArray(TInputOutputArray(Result));
end;

class function TOutputArrayHelper.OutputArray(const vec: Vector<TMat>): TOutputArray;
begin
  Constructor_OutputArray(Result, vec);
end;

class function TOutputArrayHelper.OutputArray(const m: TMat): TOutputArray;
begin
  Constructor_OutputArray(Result, m);
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

procedure TInputOutputArrayHelper.InputOutputArray(const m: TMat);
begin
  Constructor_InputOutputArray(Self, m);
end;

class function TInputOutputArrayHelper.noArray: TInputOutputArray;
begin
  noArray_InputOutputArray(Result);
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
  operator_MatSize_MatSizeToSize(m, Result);
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

class operator TMat_<T>.Assign(var Dest: TMat_<T>; const [ref] Src: TMat_<T>);
begin
  Dest.FMat := Src.FMat;
end;

procedure TMat_<T>.Assign(const m: TMat);
begin
  if (m.empty()) then
    FMat.release()
  else if (ttraits.&Type<T>.value = m.&Type()) then
    FMat := m
  else if (ttraits.Depth<T>.value = m.Depth()) then
    FMat := m.reshape(TDataType<T>.channels, m.dims, nil)
  else
  begin
    Assert((TDataType<T>.channels = m.channels()) or m.empty());
    m.convertTo(&FMat, FMat.&Type);
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
    flags := (flags and (not CV_MAT_TYPE_MASK)) + ttraits.&Type<T>.value;
end;

class function TMat_<T>.Mat(const m: TMat): TMat_<T>;
begin
  With Result.FMat do
    flags := (flags and (not CV_MAT_TYPE_MASK)) + ttraits.&Type<T>.value;
  Result.Assign(m);
end;

{$ENDIF}
