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

{$IFDEF OPENCV_CORE_HPP}

{$I 'core/cvdef.impl.inc'}
{$I 'core/base.impl.inc'}
{$I 'core/cvstd.impl.inc'}
{$I 'core/traits.impl.inc'}
{$I 'core/matx.impl.inc' }
{$I 'core/types.impl.inc'}
{$I 'core/mat.impl.inc'}
{ .$I 'core/persistence.impl.inc' }

procedure bitwise_and(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  bitwise_and(src1, src2, dst, TInputArray.noArray);
end;

procedure bitwise_not(const Src: TInputArray; const dst: TOutputArray);
begin
  bitwise_not(Src, dst, TInputArray.noArray());
end;

{ TRNG }

class operator TRNG.Implicit(const p: TRNG): unsigned;
begin
  Result := operator_RNG_ToUnsignedInt(p);
end;

class operator TRNG.Implicit(const u: UInt64): TRNG;
begin
  Result := TRNG.RNG(u);
end;

class operator TRNG.Initialize(out Dest: TRNG);
begin
  constructor_RNG(Dest);
end;

class function TRNG.RNG(state: UInt64): TRNG;
begin
  constructor_RNG(Result, state);
end;

function TRNG.UNIFORM(a, b: Int): Int;
begin
  Result := uniform_RNG(Self, a, b);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil }; var maxLoc: TPoint { = nil };
  const mask: TInputArray { = noArray() } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, @minLoc, @maxLoc, mask);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil }; var maxLoc: TPoint { = nil } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, @minLoc, @maxLoc, TInputArray.noArray);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, @minLoc, nil, TInputArray.noArray);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, nil, nil, TInputArray.noArray);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double); overload;
begin
  minMaxLoc(Src, minVal, nil, nil, nil, TInputArray.noArray);
end;

procedure Add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  Add(src1, src2, dst, TInputArray.noArray);
end;

procedure Add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray);
begin
  Add(src1, src2, dst, mask, -1);
end;

procedure split(const m: TMat; const mv: TArray<TMat>);
begin
  split(m, @mv[0]);
end;

procedure merge(const mv: TArray<TMat>; dst: TOutputArray);
begin
  merge(@mv[0], length(mv), dst);
end;

procedure copyMakeBorder(const src: TInputArray; const dst: TOutputArray; top, bottom, left, right, borderType: int);
begin
  copyMakeBorder(src,dst,top, bottom, left, right, borderType, TScalar.Scalar());
end;

procedure subtract(const src1:TInputArray; const src2:TInputArray; const dst:TOutputArray);
begin
  subtract(src1,src2,dst,TInputArray.noArray());
end;

function mean(const src:TInputArray):TScalar;
begin
  Result := mean(src, TInputArray.noArray());
end;

procedure meanStdDev(const src:TInputArray;const mean:TOutputArray;const stddev:TOutputArray);
begin
  meanStdDev(src,mean,stddev,TInputArray.noArray());
end;

function norm(const src1:TInputArray; normType :NormTypes= NORM_L2):double;
begin
  norm(src1,Int(normType),TInputArray.noArray());
end;

function norm(const src1:TInputArray;const src2:TInputArray; normType: NormTypes= NORM_L2):double;
begin
  norm(src1,src2,Int(normType),TInputArray.noArray());
end;

procedure batchDistance(const src1:TInputArray;const src2:TInputArray;
          const dist:TOutputArray; dtype:int; const nidx:TOutputArray;
          normType :normTypes= NORM_L2; K :int= 0);
begin
  batchDistance(src1,src2,dist,dtype,nidx,Int(normType),K,TInputArray.noArray());
end;

{$ENDIF OPENCV_CORE_HPP}

