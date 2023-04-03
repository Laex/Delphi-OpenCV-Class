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

{$IF defined(OPENCV_CORE_TYPES_HPP) and (not defined(OPENCV_CORE_TYPES_HPP_IMPL))}
{$DEFINE OPENCV_CORE_TYPES_HPP_IMPL}
{ TPoint_<T> }

class operator TPoint_<T>.Implicit(const m: TPoint_<T>): UInt64;
begin
  Assert(SizeOf(m) = 8);
  Move(m, Result, 8);
end;

class operator TPoint_<T>.Implicit(const p: TPoint_<T>): TPoint_<Int>;
begin
  // Result.x := TValue.From<T>(p.x).AsType<Int>;
  // Result.y := TValue.From<T>(p.y).AsType<Int>;

  if TypeInfo(T) = TypeInfo(float) then
  begin
    Result.x := Trunc(pFloat(@p.x)^); // TValue.From<T>(p.x).AsType<Int>;
    Result.y := Trunc(pFloat(@p.y)^); // TValue.From<T>(p.y).AsType<Int>
  end
  else if TypeInfo(T) = TypeInfo(double) then
  begin
    Result.x := Trunc(System.pDouble(@p.x)^);
    Result.y := Trunc(System.pDouble(@p.y)^);
  end
  else
    Assert(false, 'Point type must be Single(Float) or Double');
end;

class operator TPoint_<T>.Subtract(const a: TPoint_<T>; const b: TPoint_<T>): TPoint_<T>;
begin
  // Result.x := TValue(TValue.From<T>(a.x).AsType<double> - TValue.From<T>(b.x).AsType<double>).AsType<T>;
  // Result.y := TValue(TValue.From<T>(a.y).AsType<double> - TValue.From<T>(b.y).AsType<double>).AsType<T>;
  if TypeInfo(T) = TypeInfo(float) then
  begin
    pFloat(@Result.x)^ := pFloat(@a.x)^ - pFloat(@b.x)^;
    pFloat(@Result.y)^ := pFloat(@a.y)^ - pFloat(@b.y)^;
  end
  else if TypeInfo(T) = TypeInfo(double) then
  begin
    System.pDouble(@Result.x)^ := System.pDouble(@a.x)^ - System.pDouble(@b.x)^;
    System.pDouble(@Result.y)^ := System.pDouble(@a.y)^ - System.pDouble(@b.y)^;
  end
  else if TypeInfo(T) = TypeInfo(Int) then
  begin
    pInt(@Result.x)^ := saturate_cast<Int>.cast(pInt(@a.x)^ - pInt(@b.x)^);
    pInt(@Result.y)^ := saturate_cast<Int>.cast(pInt(@a.y)^ - pInt(@b.y)^);
  end
  else
    Assert(false, 'Point type must be Integer, Single(Float) or Double');
end;

class operator TPoint_<T>.Add(const a: TPoint_<T>; const b: TPoint_<T>): TPoint_<T>;
begin
  if TypeInfo(T) = TypeInfo(float) then
  begin
    pFloat(@Result.x)^ := pFloat(@a.x)^ + pFloat(@b.x)^;
    pFloat(@Result.y)^ := pFloat(@a.y)^ + pFloat(@b.y)^;
  end
  else if TypeInfo(T) = TypeInfo(double) then
  begin
    pDouble(@Result.x)^ := pDouble(@a.x)^ + pDouble(@b.x)^;
    pDouble(@Result.y)^ := pDouble(@a.y)^ + pDouble(@b.y)^;
  end
  else if TypeInfo(T) = TypeInfo(Int) then
  begin
    pInt(@Result.x)^ := saturate_cast<Int>.cast(pInt(@a.x)^ + pInt(@b.x)^);
    pInt(@Result.y)^ := saturate_cast<Int>.cast(pInt(@a.y)^ + pInt(@b.y)^);
  end
  else
    Assert(false, 'Point type must be Integer, Single(Float) or Double');
end;

class operator TPoint_<T>.Multiply(const a: TPoint_<T>; const b: float): TPoint_<T>;
begin
  if TypeInfo(T) = TypeInfo(float) then
  begin
    pFloat(@Result.x)^ := pFloat(@a.x)^ * b;
    pFloat(@Result.y)^ := pFloat(@a.y)^ * b;
  end
  else if TypeInfo(T) = TypeInfo(double) then
  begin
    pDouble(@Result.x)^ := pDouble(@a.x)^ * b;
    pDouble(@Result.y)^ := pDouble(@a.y)^ * b;
  end
  else if TypeInfo(T) = TypeInfo(Int) then
  begin
    pInt(@Result.x)^ := saturate_cast<Int>.cast(pInt(@a.x)^ * b);
    pInt(@Result.y)^ := saturate_cast<Int>.cast(pInt(@a.y)^ * b);
  end
  else
    Assert(false, 'Point type must be Integer, Single(Float) or Double');
end;

class operator TPoint_<T>.Multiply(const a: TPoint_<T>; const b: double): TPoint_<T>;
begin
  Result := a * float(b);
end;

class function TPoint_<T>.Point(_x, _y: T): TPoint_<T>;
begin
  Result.x := _x;
  Result.y := _y;
end;

class function TPoint_<T>.Point(): TPoint_<T>;
begin
  Result.x := default(T);
  Result.y := default(T);
end;

function Point(const _x, _y: Int): TPoint;
begin
  Result := TPoint.Point(_x, _y);
end;

function Point2f(const _x, _y: float): TPoint2f;
begin
  Result := TPoint_<float>.Point(_x, _y);
end;

{ TSize_<T> }

class operator TSize_<T>.Implicit(const m: TSize_<T>): UInt64;
begin
  Assert(SizeOf(m) = 8);
  Move(m, Result, 8);
end;

class function TSize_<T>.size(const _width, _height: T): TSize_<T>;
begin
  Result.width := _width;
  Result.height := _height;
end;

function size(const _width, _height: Int): TSize;
begin
  Result := TSize.size(_width, _height);
end;

class operator TSize_<T>.Implicit(const m: UInt64): TSize_<T>;
begin
  Assert(SizeOf(Result) = 8);
  Move(m, Result, 8);
end;

class function TSize_<T>.size: TSize_<T>;
begin
  Result.width := default (T);
  Result.height := default (T);
end;

{ TRect_<T> }

function TRect_<T>.area: T;
begin
  (*
    Var
    p: pTypeInfo := TypeInfo(T);
    if Assigned(p) then
    begin
    Var
    vH := TValue.From<T>(height);
    Var
    vW := TValue.From<T>(width);
    Case p^.Kind of
    tkInteger, tkChar, tkInt64:
    Result := vW.AsOrdinal * vH.AsOrdinal;
    tkFloat:
    Result := vW.AsExtended * vH.AsExtended;
    else
    Assert(false, 'Point type must be numeric');
    end
    end
    else
    Assert(false, 'Unknown type T');
  *)

  if TypeInfo(T) = TypeInfo(float) then
    pFloat(@Result)^ := (pFloat(@Self.width)^) * (pFloat(@Self.height)^)
  else if TypeInfo(T) = TypeInfo(double) then
    pDouble(@Result)^ := (pDouble(@Self.width)^) * (pDouble(@Self.height)^)
  else if TypeInfo(T) = TypeInfo(Int) then
    pInt(@Result)^ := (pInt(@Self.width)^) * (pInt(@Self.height)^)
  else
    Assert(false, 'Point type must be numeric');

  // Result := TValue(TValue.From<T>(width).AsType<Double> * TValue.From<T>(height).AsType<Double>).AsType<T>;
end;

class operator TRect_<T>.BitwiseAnd(const a, b: TRect_<T>): TRect_<T>;
begin
Assert(False);
(*
  Result := default (TRect_<T>);
  if TypeInfo(T) = TypeInfo(Int) then
  begin
    Var
      x1, y1, a_x, a_y, b_x, b_y, a_width, a_height, b_width, b_height: Int;

    a_x := TValue.From<T>(a.x).AsType<Int>; // a_x := pInt(@a.x)^;
    a_y := TValue.From<T>(a.y).AsType<Int>; // a_y := pInt(@a.y)^;
    b_x := TValue.From<T>(b.x).AsType<Int>; // b_x := pInt(@b.x)^;
    b_y := TValue.From<T>(b.y).AsType<Int>; // b_y := pInt(@b.y)^;

    a_width := TValue.From<T>(a.width).AsType<Int>;   // a_width := pInt(@a.width)^;
    a_height := TValue.From<T>(a.height).AsType<Int>; // a_height := pInt(@a.height)^;
    b_width := TValue.From<T>(b.width).AsType<Int>;   // b_width := pInt(@b.width)^;
    b_height := TValue.From<T>(b.height).AsType<Int>; // b_height := pInt(@b.height)^;

    x1 := System.Math.MAX(a_x, b_x);
    y1 := System.Math.MAX(a_y, b_y);
    a_width := System.Math.MIN(a_x + a_width, b_x + b_width) - x1;
    a_height := System.Math.MIN(a_y + a_height, b_y + b_height) - y1;
    if (a_width > 0) and (a_height > 0) then
    begin
      Result.x := TValue(x1).AsType<T>;            // pInt(@Result.x)^ := x1;
      Result.y := TValue(y1).AsType<T>;            // pInt(@Result.y)^ := y1;
      Result.width := TValue(a_width).AsType<T>;   // pInt(@Result.width)^ := a_width;
      Result.height := TValue(a_height).AsType<T>; // pInt(@Result.height)^ := a_height;
    end;
  end
  else
    Assert(false, 'Point type must be numeric');
*)
end;

class function TRect_<T>.Rect(const _x: T; _y, _width, _height: T): TRect_<T>;
begin
  Result.x := _x;
  Result.y := _y;
  Result.width := _width;
  Result.height := _height;
end;

class operator TRect_<T>.Implicit(const a: TArray<T>): TRect_<T>;
begin
  Assert(Length(a) = 4);
  Result.x := a[0];
  Result.y := a[1];
  Result.width := a[2];
  Result.height := a[3];
end;

function TRect_<T>.size: TSize_<T>;
begin
  Result.width := width;
  Result.height := height;
end;

function TRect_<T>.tl: TPoint_<T>;
begin
  Result.x := x;
  Result.y := y;
end;

function TRect_<T>.br: TPoint_<T>;
begin
  // Result.x := TValue(TValue.From<T>(x).AsType<Double> + TValue.From<T>(width).AsType<Double>).AsType<T>;
  // Result.y := TValue(TValue.From<T>(y).AsType<Double> + TValue.From<T>(height).AsType<Double>).AsType<T>;
  if TypeInfo(T) = TypeInfo(float) then
  begin
    pFloat(@Result.x)^ := pFloat(@x)^ + pFloat(@width)^;
    pFloat(@Result.y)^ := pFloat(@y)^ + pFloat(@height)^;
  end
  else if TypeInfo(T) = TypeInfo(double) then
  begin
    pDouble(@Result.x)^ := pDouble(@x)^ + pDouble(@width)^;
    pDouble(@Result.y)^ := pDouble(@y)^ + pDouble(@height)^;
  end
  else if TypeInfo(T) = TypeInfo(Int) then
  begin
    pInt(@Result.x)^ := pInt(@x)^ + pInt(@width)^;
    pInt(@Result.y)^ := pInt(@y)^ + pInt(@height)^;
  end
  else
    Assert(false, 'Point type must be numeric');
end;

function Rect(const _x: Int; _y: Int; _width: Int; _height: Int): TRect;
begin
  Result := TRect_<Int>.Rect(_x, _y, _width, _height);
end;

function Rect(const pt1, pt2: TPoint): TRect;
begin
  Result.x := System.Math.MIN(pt1.x, pt2.x);
  Result.y := System.Math.MIN(pt1.y, pt2.y);
  Result.width := System.Math.MAX(pt1.x, pt2.x) - Result.x;
  Result.height := System.Math.MAX(pt1.y, pt2.y) - Result.y;
end;

{ TScalar }

class function TScalar.Create(const v0, v1, v2, v3: double): TScalar;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_GAPI_OWN_SCALAR_1(Result, v0, v1, v2, v3);
{$IFEND}
end;

class function TScalar.all(const v0: double): TScalar;
begin
  Result := TScalar.Create(v0, v0, v0, v0);
end;

class function TScalar.Create(const v0: double): TScalar;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_GAPI_OWN_SCALAR(Result, v0);
{$IFEND}
end;

function TScalar.getval(const index: integer): double;
begin
  Assert((index >= 0) and (index < 4));
  Result := v[index];
end;

class operator TScalar.Implicit(const v0: double): TScalar;
begin
  Result := TScalar.Create(v0);
end;

class operator TScalar.Initialize(out Dest: TScalar);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_GAPI_OWN_SCALAR_2(Dest);
{$IFEND}
end;

procedure TScalar.setval(const index: integer; const Value: double);
begin
  Assert((index >= 0) and (index < 4));
  v[index] := Value;
end;

class function TScalar.Scalar: TScalar;
begin
  Initialize(Result);
end;

class operator TScalar.Implicit(const a: TArray<double>): TScalar;
begin
  for var i := 0 to System.Math.MIN(High(a), High(Result.v)) do
    Result.v[i] := a[i];
end;

function Scalar(const v0, v1, v2, v3: double): TScalar;
begin
  Result := TScalar.Create(v0, v1, v2, v3);
end;

function Scalar(const v: TVec3b): TScalar;
begin
  Result := Scalar(v[0], v[1], v[2]);
end;

function Scalar: TScalar;
begin
  Result := TScalar.Scalar();
end;

{ TTermCriteria }

function TTermCriteria.isValid: BOOL;
begin
  Var
    isCount: BOOL := ((&type and COUNT) <> 0) and (maxCount > 0);
  var
    isEps: BOOL := ((&type and EPS) <> 0) and (not IsNaN(epsilon));
  Result := isCount or isEps;
end;

class function TTermCriteria.TermCriteria(const &type, maxCount: Int; const epsilon: double): TTermCriteria;
begin
  Result.&type := &type;
  Result.maxCount := maxCount;
  Result.epsilon := epsilon;
end;

// function norm(const pt: TPoint_<T>): Double;
// begin
// Result := Sqrt(Sqr(TValue.From<T>(a.x).AsType<Double> - TValue.From<T>(b.x).AsType<Double>) + //
// Sqr(TValue.From<T>(a.y).AsType<Double> - TValue.From<T>(b.y).AsType<Double>));

// if TypeInfo(T) = TypeInfo(float) then
// Result := Sqrt(Sqr(pFloat(@a.x)^ - pFloat(@b.x)^) + Sqr(pFloat(@a.y)^ - pFloat(@b.y)^))
// else if TypeInfo(T) = TypeInfo(double) then
// Result := Sqrt(Sqr(pDouble(@a.x)^ - pDouble(@b.x)^) + Sqr(pDouble(@a.y)^ - pDouble(@b.y)^))
// else if TypeInfo(T) = TypeInfo(Int) then
// Result := Sqrt(Sqr(pInt(@a.x)^ - pInt(@b.x)^) + Sqr(pInt(@a.y)^ - pInt(@b.y)^))
// else
// Assert(false, 'Point type must be Integer, Single(Float) or Double');
// end;

function norm(const pt: TPoint): double;
begin
  Result := System.Sqrt(System.Sqr(pt.x) + System.Sqr(pt.y));
end;

{ TRange }

function Range(_start: Int; _end: Int): TRange;
begin
  Result.start := _start;
  Result.&end := _end;
end;

class function TRange.all: TRange;
begin
  Result := Range(INT_MIN, INT_MAX);
end;

{$IFEND}
