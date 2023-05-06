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

{$IF defined(OPENCV_CORE_HPP) and (not defined(OPENCV_CORE_HPP_IMPL))}
{$DEFINE OPENCV_CORE_HPP_IMPL}
{$I 'core/cvdef.impl.inc'}
{$I 'core/base.impl.inc'}
{$I 'core/cvstd.impl.inc'}
{$I 'core/traits.impl.inc'}
{$I 'core/matx.impl.inc' }
{$I 'core/types.impl.inc'}
{$I 'core/mat.impl.inc'}
{$I 'core/persistence.impl.inc'}
{$I 'core/utility.impl.inc'}

procedure bitwise_and(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  bitwise_and(src1, src2, dst, TInputArray.noArray());
end;

procedure bitwise_not(const Src: TInputArray; const dst: TOutputArray);
begin
  bitwise_not(Src, dst, TInputArray.noArray());
end;

{ TRNG }

class operator TRNGHelper.Implicit(const rng: TRNG): unsigned;
begin
{$IF not defined(PACKAGE)}
  Result := operator_CV_RNG_implicit_5(rng);
{$IFEND}
end;

class operator TRNGHelper.Implicit(const rng: TRNG): int;
begin
{$IF not defined(PACKAGE)}
  Result := operator_CV_RNG_implicit_4(rng);
{$IFEND}
end;

class operator TRNGHelper.Implicit(const u: UInt64): TRNG;
begin
  Result := TRNG.rng(u);
end;

class operator TRNG.Initialize(out Dest: TRNG);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_RNG(Dest);
{$IFEND}
end;

class function TRNGHelper.rng(state: UInt64): TRNG;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_RNG_1(Result, state);
{$IFEND}
end;

function TRNGHelper.UNIFORM(a, b: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_RNG_uniform(Self, a, b);
{$IFEND}
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; const maxVal: pDouble { = nil }; const minLoc: pPoint { = nil }; const maxLoc: pPoint { = nil }; const mask: TInputArray { = noArray() }
  );
begin
{$IF not defined(PACKAGE)}
  proc_minMaxLoc_1(Src, @minVal, maxVal, minLoc, maxLoc, mask);
{$IFEND}
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil }; var maxLoc: TPoint { = nil }; const mask: TInputArray { = noArray() } ); overload;
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

procedure Add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray { = noArray() }; dtype: int { = -1 } );
begin
{$IF not defined(PACKAGE)}
  proc_add(src1, src2, dst, mask, dtype);
{$IFEND}
end;

procedure Add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  Add(src1, src2, dst, TInputArray.noArray);
end;

procedure Add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray);
begin
  Add(src1, src2, dst, mask, -1);
end;

procedure split(const m: TMat; const mv: pMat);
begin
{$IF not defined(PACKAGE)}
  proc_split(m, mv);
{$IFEND}
end;

procedure split(const m: TInputArray; const mv: TOutputArrayOfArrays);
begin
{$IF not defined(PACKAGE)}
  proc_split_1(m, mv);
{$IFEND}
end;

procedure split(const m: TMat; const mv: TArray<TMat>);
begin
  split(m, @mv[0]);
end;

procedure merge(const mv: TInputArrayOfArrays; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_merge(mv, dst);
{$IFEND}
end;

procedure merge(const mv: pMat; COUNT: size_t; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_merge_1(mv, COUNT, dst);
{$IFEND}
end;

procedure merge(const mv: TArray<TMat>; dst: TOutputArray);
begin
  merge(@mv[0], length(mv), dst);
end;

procedure copyMakeBorder(const Src: TInputArray; const dst: TOutputArray; top, bottom, left, right: int; borderType: borderTypes; const value: TScalar { = Scalar() } );
begin
{$IF not defined(PACKAGE)}
  proc_copyMakeBorder(Src, dst, top, bottom, left, right, borderType, value);
{$IFEND}
end;

procedure copyMakeBorder(const Src: TInputArray; const dst: TOutputArray; top, bottom, left, right: int; borderType: borderTypes);
begin
  copyMakeBorder(Src, dst, top, bottom, left, right, borderType, TScalar.Scalar());
end;

procedure subtract(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray { = noArray() }; dtype: int = -1);
begin
{$IF not defined(PACKAGE)}
  proc_subtract(src1, src2, dst, mask, dtype);
{$IFEND}
end;

procedure subtract(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  subtract(src1, src2, dst, TInputArray.noArray());
end;

procedure Multiply(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; scale: double = 1; dtype: int = -1);
begin
{$IF not defined(PACKAGE)}
  proc_multiply(src1, src2, dst, scale, dtype);
{$IFEND}
end;

procedure divide(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; scale: double = 1; dtype: int = -1);
begin
{$IF not defined(PACKAGE)}
  proc_divide(src1, src2, dst, scale, dtype);
{$IFEND}
end;

procedure divide(scale: double; const src2: TInputArray; const dst: TOutputArray; dtype: int = -1);
begin
{$IF not defined(PACKAGE)}
  proc_divide_1(scale, src2, dst, dtype);
{$IFEND}
end;

procedure scaleAdd(const src1: TInputArray; alpha: double; const src2: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_scaleAdd(src1, alpha, src2, dst);
{$IFEND}
end;

procedure addWeighted(const src1: TInputArray; alpha: double; const src2: TInputArray; beta, gamma: double; const dst: TOutputArray; dtype: int = -1);
begin
{$IF not defined(PACKAGE)}
  proc_addWeighted(src1, alpha, src2, beta, gamma, dst, dtype);
{$IFEND}
end;

procedure convertScaleAbs(const Src: TInputArray; const dst: TOutputArray; alpha: double = 1; beta: double = 0);
begin
{$IF not defined(PACKAGE)}
  proc_convertScaleAbs(Src, dst, alpha, beta);
{$IFEND}
end;

procedure convertFp16(const Src: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_convertFp16(Src, dst);
{$IFEND}
end;

procedure LUT(const Src: TInputArray; const LUT: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_LUT(Src, LUT, dst);
{$IFEND}
end;

function sum(const Src: TInputArray): TScalar;
begin
{$IF not defined(PACKAGE)}
  Result := func_sum(Src);
{$IFEND}
end;

function countNonZero(const Src: TInputArray): int;
begin
{$IF not defined(PACKAGE)}
  Result := func_countNonZero(Src);
{$IFEND}
end;

procedure findNonZero(const Src: TInputArray; const idx: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_findNonZero(Src, idx);
{$IFEND}
end;

function mean(const Src: TInputArray; const mask: TInputArray { = noArray() } ): TScalar;
begin
{$IF not defined(PACKAGE)}
  Result := func_mean(Src, mask);
{$IFEND}
end;

function mean(const Src: TInputArray): TScalar;
begin
  Result := mean(Src, TInputArray.noArray());
end;

procedure meanStdDev(const Src: TInputArray; const mean: TOutputArray; const stddev: TOutputArray; const mask: TInputArray { =noArray() } );
begin
{$IF not defined(PACKAGE)}
  proc_meanStdDev(Src, mean, stddev, mask);
{$IFEND}
end;

procedure meanStdDev(const Src: TInputArray; const mean: TOutputArray; const stddev: TOutputArray);
begin
  meanStdDev(Src, mean, stddev, TInputArray.noArray());
end;

function norm(const src1: TInputArray; normType: int { = int(NORM_L2) }; const mask: TInputArray { = noArray() } ): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_norm_2(src1, normType, mask);
{$IFEND}
end;

function norm(const src1: TInputArray; normType: NormTypes = NORM_L2): double;
begin
  Result := norm(src1, int(normType), TInputArray.noArray());
end;

function norm(const src1: TInputArray; const src2: TInputArray; normType: int { = int(NORM_L2) }; const mask: TInputArray { = noArray() } ): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_norm_1(src1, src2, normType, mask);
{$IFEND}
end;

function norm(const src1: TInputArray; const src2: TInputArray; normType: NormTypes = NORM_L2): double;
begin
  Result := norm(src1, src2, int(normType), TInputArray.noArray());
end;

function PSNR(const src1: TInputArray; const src2: TInputArray; R: double = 255): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_PSNR(src1, src2, R);
{$IFEND}
end;

procedure batchDistance(const src1: TInputArray; const src2: TInputArray; const dist: TOutputArray; dtype: int; const nidx: TOutputArray; normType: int { = int(NORM_L2) }; K: int { = 0 }; const mask: TInputArray { = noArray() }; update: int = 0; crosscheck: bool = false);
begin
{$IF not defined(PACKAGE)}
  proc_batchDistance(src1, src2, dist, dtype, nidx, normType, K, mask, update, crosscheck);
{$IFEND}
end;

procedure batchDistance(const src1: TInputArray; const src2: TInputArray; const dist: TOutputArray; dtype: int; const nidx: TOutputArray; normType: NormTypes = NORM_L2; K: int = 0);
begin
  batchDistance(src1, src2, dist, dtype, nidx, int(normType), K, TInputArray.noArray());
end;

procedure minMaxIdx(const Src: TInputArray; minVal: pDouble; maxVal: pDouble = nil; minIdx: Pointer = nil; maxIdx: pInt = nil);
begin
  minMaxIdx(Src, minVal, maxVal, minIdx, maxIdx, TInputArray.noArray());
end;

procedure bitwise_or(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  bitwise_or(src1, src2, dst, TInputArray.noArray());
end;

procedure bitwise_xor(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  bitwise_xor(src1, src2, dst, TInputArray.noArray());
end;

procedure mulTransposed(const Src: TInputArray; const dst: TOutputArray; aTa: bool);
begin
  mulTransposed(Src, dst, aTa, TInputArray.noArray());
end;

procedure setIdentity(const mtx: TInputOutputArray);
begin
  setIdentity(mtx, Scalar(1));
end;

function eigen(const Src: TInputArray; const eigenvalues: TOutputArray): bool;
begin
  Result := eigen(Src, eigenvalues, TOutputArray.noArray());
end;

function kmeans(const data: TInputArray; K: int; const bestLabels: TInputOutputArray; const criteria: TTermCriteria; attempts: int; flags: int): double;
begin
  Result := kmeans(data, K, bestLabels, criteria, attempts, flags, TOutputArray.noArray());
end;

procedure normalize( const Src: TInputArray; const dst: TInputOutputArray; alpha: double { = 1 };
                     beta: double { = 0 }; norm_type: int { = int(NORM_L2) }; dtype: int { = -1 };
                     const mask: TInputArray { = noArray() } );
begin
{$IF not defined(PACKAGE)}
  proc_normalize_1(Src, dst, alpha, beta, norm_type, dtype, mask);
{$IFEND}
end;

procedure normalize( const Src: TInputArray; const dst: TInputOutputArray; const alpha: double = 1;
                     beta: double = 0; norm_type: NormTypes = NORM_L2; dtype: int = -1);
begin
  normalize(Src, dst, alpha, beta, norm_type, dtype, TInputArray.noArray);
end;

procedure compare(const src1, src2: TInputArray; const dst: TOutputArray; cmpop: CmpTypes);
begin
{$IF not defined(PACKAGE)}
  proc_compare(src1, src2, dst, int(cmpop));
{$IFEND}
end;

procedure swap(var a, b: TMat);
begin
{$IF not defined(PACKAGE)}
  proc_swap(a, b);
{$IFEND}
end;

(*
  procedure swap(var a, b: UMat);
  begin
  {$IF not defined(PACKAGE)}
  proc_swap_1(a, b);
  {$IFEND}
  end;
*)

function borderInterpolate(p: int; len: int; borderType: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := func_borderInterpolate(p, len, borderType);
{$IFEND}
end;

procedure minMaxIdx(const Src: TInputArray; minVal: pDouble; maxVal: pDouble { = nil }; minIdx: Pointer { = nil }; maxIdx: pInt { = nil }; const mask: TInputArray { = noArray() } );
begin
{$IF not defined(PACKAGE)}
  proc_minMaxIdx(Src, minVal, maxVal, minIdx, maxIdx, mask);
{$IFEND}
end;

procedure reduce(const Src: TInputArray; const dst: TOutputArray; dim, rtype: int; dtype: int = -1);
begin
{$IF not defined(PACKAGE)}
  proc_reduce(Src, dst, dim, rtype, dtype);
{$IFEND}
end;

procedure mixChannels(Var Src: TMat; nsrcs: size_t; Var dst: TMat; ndsts: size_t; const fromTo: pInt; npairs: size_t);
begin
{$IF not defined(PACKAGE)}
  proc_mixChannels_2(@Src, nsrcs, @dst, ndsts, fromTo, npairs);
{$IFEND}
end;

procedure mixChannels(const Src: TInputArrayOfArrays; const dst: TInputOutputArrayOfArrays; const fromTo: pInt; npairs: size_t);
begin
{$IF not defined(PACKAGE)}
  proc_mixChannels_1(Src, dst, fromTo, npairs);
{$IFEND}
end;

procedure mixChannels(const Src: TInputArrayOfArrays; const dst: TInputOutputArrayOfArrays; const fromTo: vector<int>);
begin
{$IF not defined(PACKAGE)}
  proc_mixChannels(Src, dst, fromTo);
{$IFEND}
end;

procedure extractChannel(const Src: TInputArray; const dst: TOutputArray; coi: int);
begin
{$IF not defined(PACKAGE)}
  proc_extractChannel(Src, dst, coi);
{$IFEND}
end;

procedure insertChannel(const Src: TInputArray; const dst: TInputOutputArray; coi: int);
begin
{$IF not defined(PACKAGE)}
  proc_insertChannel(Src, dst, coi);
{$IFEND}
end;

procedure flip(const Src: TInputArray; const dst: TOutputArray; flipCode: int);
begin
{$IF not defined(PACKAGE)}
  proc_flip(Src, dst, flipCode);
{$IFEND}
end;

procedure rotate(const Src: TInputArray; const dst: TOutputArray; rotateCode: int);
begin
{$IF not defined(PACKAGE)}
  proc_rotate(Src, dst, rotateCode);
{$IFEND}
end;

procedure &repeat(const Src: TInputArray; ny, nx: int; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_repeat(Src, ny, nx, dst);
{$IFEND}
end;

function &repeat(const Src: TMat; ny, nx: int): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := func_repeat(Src, ny, nx);
{$IFEND}
end;

procedure hconcat(const Src: pMat; nsrc: size_t; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_hconcat_2(Src, nsrc, dst);
{$IFEND}
end;

procedure hconcat(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_hconcat(src1, src2, dst);
{$IFEND}
end;

procedure hconcat(const Src: TInputArrayOfArrays; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_hconcat_1(Src, dst);
{$IFEND}
end;

procedure vconcat(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_vconcat(src1, src2, dst);
{$IFEND}
end;

procedure vconcat(const Src: TInputArrayOfArrays; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_vconcat_1(Src, dst);
{$IFEND}
end;

procedure bitwise_and(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray { = noArray() } );
begin
{$IF not defined(PACKAGE)}
  proc_bitwise_and(src1, src2, dst, mask);
{$IFEND}
end;

procedure bitwise_or(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray { = noArray() } );
begin
{$IF not defined(PACKAGE)}
  proc_bitwise_or(src1, src2, dst, mask);
{$IFEND}
end;

procedure bitwise_xor(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray { = noArray() } );
begin
{$IF not defined(PACKAGE)}
  proc_bitwise_xor(src1, src2, dst, mask);
{$IFEND}
end;

procedure bitwise_not(const Src: TInputArray; const dst: TOutputArray; mask: TInputArray { = noArray() } );
begin
{$IF not defined(PACKAGE)}
  proc_bitwise_not(Src, dst, mask);
{$IFEND}
end;

procedure absdiff(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_absdiff(src1, src2, dst);
{$IFEND}
end;

procedure copyTo(const Src: TInputArray; const dst: TOutputArray; const mask: TInputArray);
begin
{$IF not defined(PACKAGE)}
  proc_copyTo(Src, dst, mask);
{$IFEND}
end;

procedure inRange(const Src: TInputArray; const lowerb: TInputArray; const upperb: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_inRange(Src, lowerb, upperb, dst);
{$IFEND}
end;

procedure min(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_min_2(src1, src2, dst);
{$IFEND}
end;

procedure min(const src1: TMat; const src2: TMat; Var dst: TMat);
begin
{$IF not defined(PACKAGE)}
  proc_min(src1, src2, dst);
{$IFEND}
end;

procedure max(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_max_2(src1, src2, dst);
{$IFEND}
end;

procedure max(const src1: TMat; const src2: TMat; Var dst: TMat);
begin
{$IF not defined(PACKAGE)}
  proc_max(src1, src2, dst);
{$IFEND}
end;

procedure sqrt(const Src: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_sqrt(Src, dst);
{$IFEND}
end;

procedure pow(const Src: TInputArray; power: double; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_pow(Src, power, dst);
{$IFEND}
end;

procedure exp(const Src: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_exp(Src, dst);
{$IFEND}
end;

procedure log(const Src: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_log_2(Src, dst);
{$IFEND}
end;

procedure polarToCart(const magnitude: TInputArray; const angle: TInputArray; const x: TOutputArray; const y: TOutputArray; angleInDegrees: bool = false);
begin
{$IF not defined(PACKAGE)}
  proc_polarToCart(magnitude, angle, x, y, angleInDegrees);
{$IFEND}
end;

procedure cartToPolar(const x: TInputArray; const y: TInputArray; const magnitude: TOutputArray; const angle: TOutputArray; angleInDegrees: bool = false);
begin
{$IF not defined(PACKAGE)}
  proc_cartToPolar(x, y, magnitude, angle, angleInDegrees);
{$IFEND}
end;

procedure phase(const x: TInputArray; const y: TInputArray; const angle: TOutputArray; angleInDegrees: bool = false);
begin
{$IF not defined(PACKAGE)}
  proc_phase(x, y, angle, angleInDegrees);
{$IFEND}
end;

procedure magnitude(const x: TInputArray; const y: TInputArray; const magnitude: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_magnitude(x, y, magnitude);
{$IFEND}
end;

function checkRange(const a: TInputArray; quiet: bool = true; pos: pPoint = nil; minVal: double = -DBL_MAX; maxVal: double = DBL_MAX): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_checkRange(a, quiet, pos, minVal, maxVal);
{$IFEND}
end;

procedure patchNaNs(const a: TInputOutputArray; val: double = 0);
begin
{$IF not defined(PACKAGE)}
  proc_patchNaNs(a, val);
{$IFEND}
end;

procedure gemm(const src1: TInputArray; const src2: TInputArray; alpha: double; const src3: TInputArray; beta: double; const dst: TOutputArray; flags: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_gemm(src1, src2, alpha, src3, beta, dst, flags);
{$IFEND}
end;

procedure mulTransposed(const Src: TInputArray; const dst: TOutputArray; aTa: bool; const delta: TInputArray { = noArray() }; scale: double = 1; dtype: int = -1);
begin
{$IF not defined(PACKAGE)}
  proc_mulTransposed(Src, dst, aTa, delta, scale, dtype);
{$IFEND}
end;

procedure transpose(const Src: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_transpose(Src, dst);
{$IFEND}
end;

procedure transform(const Src: TInputArray; const dst: TOutputArray; const m: TInputArray);
begin
{$IF not defined(PACKAGE)}
  proc_transform(Src, dst, m);
{$IFEND}
end;

procedure perspectiveTransform(const Src: TInputArray; const dst: TOutputArray; const m: TInputArray);
begin
{$IF not defined(PACKAGE)}
  proc_perspectiveTransform(Src, dst, m);
{$IFEND}
end;

procedure completeSymm(const m: TInputOutputArray; lowerToUpper: bool = false);
begin
{$IF not defined(PACKAGE)}
  proc_completeSymm(m, lowerToUpper);
{$IFEND}
end;

procedure setIdentity(const mtx: TInputOutputArray; const s: TScalar { = Scalar(1) } );
begin
{$IF not defined(PACKAGE)}
  proc_setIdentity(mtx, s);
{$IFEND}
end;

function determinant(const mtx: TInputArray): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_determinant(mtx);
{$IFEND}
end;

function trace(const mtx: TInputArray): TScalar;
begin
{$IF not defined(PACKAGE)}
  Result := func_trace(mtx);
{$IFEND}
end;

function invert(const Src: TInputArray; const dst: TOutputArray; flags: int = int(DECOMP_LU)): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_invert(Src, dst, flags);
{$IFEND}
end;

function solve(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; flags: int = int(DECOMP_LU)): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_solve(src1, src2, dst, flags);
{$IFEND}
end;

procedure sort(const Src: TInputArray; const dst: TOutputArray; flags: int);
begin
{$IF not defined(PACKAGE)}
  proc_sort(Src, dst, flags);
{$IFEND}
end;

procedure sortIdx(const Src: TInputArray; const dst: TOutputArray; flags: int);
begin
{$IF not defined(PACKAGE)}
  proc_sortIdx(Src, dst, flags);
{$IFEND}
end;

function solveCubic(const coeffs: TInputArray; const roots: TOutputArray): int;
begin
{$IF not defined(PACKAGE)}
  Result := func_solveCubic(coeffs, roots);
{$IFEND}
end;

function solvePoly(const coeffs: TInputArray; const roots: TOutputArray; maxIters: int = 300): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_solvePoly(coeffs, roots, maxIters);
{$IFEND}
end;

function eigen(const Src: TInputArray; const eigenvalues: TOutputArray; const eigenvectors: TOutputArray { = noArray() } ): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_eigen(Src, eigenvalues, eigenvectors);
{$IFEND}
end;

procedure eigenNonSymmetric(const Src: TInputArray; const eigenvalues: TOutputArray; const eigenvectors: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_eigenNonSymmetric(Src, eigenvalues, eigenvectors);
{$IFEND}
end;

procedure calcCovarMatrix(const samples: pMat; nsamples: int; Var covar: TMat; Var mean: TMat; flags: int; ctype: int = CV_64F);
begin
{$IF not defined(PACKAGE)}
  proc_calcCovarMatrix_1(samples, nsamples, covar, mean, flags, ctype);
{$IFEND}
end;

procedure calcCovarMatrix(const samples: TInputArray; const covar: TOutputArray; const mean: TInputOutputArray; flags: int; ctype: int = CV_64F);
begin
{$IF not defined(PACKAGE)}
  proc_calcCovarMatrix(samples, covar, mean, flags, ctype);
{$IFEND}
end;

procedure PCACompute(const data: TInputArray; const mean: TInputOutputArray; const eigenvectors: TOutputArray; maxComponents: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_PCACompute_2(data, mean, eigenvectors, maxComponents);
{$IFEND}
end;

procedure PCACompute(const data: TInputArray; const mean: TInputOutputArray; const eigenvectors: TOutputArray; const eigenvalues: TOutputArray; maxComponents: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_PCACompute(data, mean, eigenvectors, eigenvalues, maxComponents);
{$IFEND}
end;

procedure PCACompute(const data: TInputArray; const mean: TInputOutputArray; const eigenvectors: TOutputArray; retainedVariance: double);
begin
{$IF not defined(PACKAGE)}
  proc_PCACompute_3(data, mean, eigenvectors, retainedVariance);
{$IFEND}
end;

procedure PCACompute(const data: TInputArray; const mean: TInputOutputArray; const eigenvectors: TOutputArray; const eigenvalues: TOutputArray; retainedVariance: double);
begin
{$IF not defined(PACKAGE)}
  proc_PCACompute_1(data, mean, eigenvectors, eigenvalues, retainedVariance);
{$IFEND}
end;

procedure PCAProject(const data: TInputArray; const mean: TInputArray; const eigenvectors: TInputArray; const res: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_PCAProject(data, mean, eigenvectors, res);
{$IFEND}
end;

procedure PCABackProject(const data: TInputArray; const mean: TInputArray; const eigenvectors: TInputArray; const res: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_PCABackProject(data, mean, eigenvectors, res);
{$IFEND}
end;

procedure SVDecomp(const Src: TInputArray; const w: TOutputArray; const u: TOutputArray; const vt: TOutputArray; flags: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_SVDecomp(Src, w, u, vt, flags);
{$IFEND}
end;

procedure SVBackSubst(const w: TInputArray; const u: TInputArray; const vt: TInputArray; const rhs: TInputArray; const dst: TOutputArray);
begin
{$IF not defined(PACKAGE)}
  proc_SVBackSubst(w, u, vt, rhs, dst);
{$IFEND}
end;

function Mahalanobis(const v1: TInputArray; const v2: TInputArray; const icovar: TInputArray): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_Mahalanobis(v1, v2, icovar);
{$IFEND}
end;

procedure dft(const Src: TInputArray; const dst: TOutputArray; flags: int = 0; nonzeroRows: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_dft(Src, dst, flags, nonzeroRows);
{$IFEND}
end;

procedure idft(const Src: TInputArray; const dst: TOutputArray; flags: int = 0; nonzeroRows: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_idft(Src, dst, flags, nonzeroRows);
{$IFEND}
end;

procedure dct(const Src: TInputArray; const dst: TOutputArray; flags: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_dct(Src, dst, flags);
{$IFEND}
end;

procedure idct(const Src: TInputArray; const dst: TOutputArray; flags: int = 0);
begin
{$IF not defined(PACKAGE)}
  proc_idct(Src, dst, flags);
{$IFEND}
end;

procedure mulSpectrums(const a: TInputArray; const b: TInputArray; const c: TOutputArray; flags: int; conjB: bool = false);
begin
{$IF not defined(PACKAGE)}
  proc_mulSpectrums(a, b, c, flags, conjB);
{$IFEND}
end;

function getOptimalDFTSize(vecsize: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getOptimalDFTSize(vecsize);
{$IFEND}
end;

function theRNG: TRNG;
begin
{$IF not defined(PACKAGE)}
  Result := func_theRNG;
{$IFEND}
end;

procedure setRNGSeed(seed: int);
begin
{$IF not defined(PACKAGE)}
  proc_setRNGSeed(seed);
{$IFEND}
end;

procedure randu(const dst: TInputOutputArray; const low: TInputArray; const high: TInputArray);
begin
{$IF not defined(PACKAGE)}
  proc_randu(dst, low, high);
{$IFEND}
end;

procedure randn(const dst: TInputOutputArray; const mean: TInputArray; const stddev: TInputArray);
begin
{$IF not defined(PACKAGE)}
  proc_randn(dst, mean, stddev);
{$IFEND}
end;

procedure randShuffle(const dst: TInputOutputArray; iterFactor: double = 1; rng: pRNG = nil);
begin
{$IF not defined(PACKAGE)}
  proc_randShuffle(dst, iterFactor, rng);
{$IFEND}
end;

function kmeans(const data: TInputArray; K: int; const bestLabels: TInputOutputArray; const criteria: TTermCriteria; attempts: int; flags: int; const centers: TOutputArray { = noArray() } ): double;
begin
{$IF not defined(PACKAGE)}
  Result := func_kmeans(data, K, bestLabels, criteria, attempts, flags, centers);
{$IFEND}
end;

{$IFEND OPENCV_CORE_HPP}
