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

{$IFDEF OPENCV_CORE_TYPES_HPP}
{ TPoint_<T> }

class operator TPoint_<T>.Implicit(const m: TPoint_<T>): UInt64;
begin
  Assert(SizeOf(m) = 8);
  Move(m, Result, 8);
end;

class operator TPoint_<T>.Implicit(const p: TPoint_<T>): TPoint_<Int>;
begin
  if TypeInfo(T) = TypeInfo(float) then
  begin
    Result.x := Trunc(pFloat(@p.x)^);
    Result.y := Trunc(pFloat(@p.y)^);
  end
  else if TypeInfo(T) = TypeInfo(double) then
  begin
    Result.x := Trunc(pDouble(@p.x)^);
    Result.y := Trunc(pDouble(@p.y)^);
  end
  else
    Assert(false, 'Point type must be Single(Float) or Double');
end;

class function TPoint_<T>.Point(_x, _y: T): TPoint_<T>;
begin
  Result.x := _x;
  Result.y := _y;
end;

function Point(const _x, _y: Int): TPoint;
begin
  Result := TPoint.Point(_x, _y);
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
end;

class operator TRect_<T>.BitwiseAnd(const a, b: TRect_<T>): TRect_<T>;
begin
  Result := default (TRect_<T>);
  if TypeInfo(T) = TypeInfo(Int) then
  begin
    Var
      x1, y1, a_x, a_y, b_x, b_y, a_width, a_height, b_width, b_height: Int;

    a_x := pInt(@a.x)^;
    a_y := pInt(@a.y)^;
    b_x := pInt(@b.x)^;
    b_y := pInt(@b.y)^;
    a_width := pInt(@a.width)^;
    a_height := pInt(@a.height)^;
    b_width := pInt(@b.width)^;
    b_height := pInt(@b.height)^;

    x1 := MAX(a_x, b_x);
    y1 := MAX(a_y, b_y);
    a_width := MIN(a_x + a_width, b_x + b_width) - x1;
    a_height := MIN(a_y + a_height, b_y + b_height) - y1;
    if (a_width > 0) and (a_height > 0) then
    begin
      pInt(@Result.x)^ := x1;
      pInt(@Result.y)^ := y1;
      pInt(@Result.width)^ := a_width;
      pInt(@Result.height)^ := a_height;
    end;
  end
  else
    Assert(false, 'Point type must be numeric');
end;

class function TRect_<T>.Rect(const _x: T; _y, _width, _height: T): TRect_<T>;
begin
  Result.x := _x;
  Result.y := _y;
  Result.width := _width;
  Result.height := _height;
end;

function Rect(const _x: Int; _y: Int; _width: Int; _height: Int): TRect;
begin
  Result := TRect_<Int>.Rect(_x, _y, _width, _height);
end;

{ TScalar }

class function TScalar.Create(const v0, v1, v2, v3: double): TScalar;
begin
  constructor_Scalar(Result, v0, v1, v2, v3);
end;

class function TScalar.all(const v0: double): TScalar;
begin
  Result := TScalar.Create(v0, v0, v0, v0);
end;

class function TScalar.Create(const v0: double): TScalar;
begin
  constructor_Scalar(Result, v0);
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
  constructor_Scalar(Dest);
end;

procedure TScalar.setval(const index: integer; const Value: double);
begin
  Assert((index >= 0) and (index < 4));
  v[index] := Value;
end;

function Scalar(const v0, v1, v2, v3: double): TScalar;
begin
  Result := TScalar.Create(v0, v1, v2, v3);
end;

function Scalar(const v: TVec3b): TScalar;
begin
  Result := Scalar(v[0], v[1], v[2]);
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

class function TTermCriteria.TermCriteria(const &type, maxCount: Int;

  const epsilon: double): TTermCriteria;
begin
  Result.&type := &type;
  Result.maxCount := maxCount;
  Result.epsilon := epsilon;
end;

{$ENDIF}
