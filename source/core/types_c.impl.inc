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

{$include 'core/cvdef.impl.inc'}
{$include 'core.impl.inc'}

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

{ _IplImage }

class operator _IplImage.Initialize(out Dest: _IplImage);
begin
  // memset(this, 0, sizeof(*this));  // valid for POD structure
  Dest.nSize := sizeof(TIplImage);
end;

class operator _IplImage.Explicit(const m: TMat): _IplImage;
begin
  ! ! ! !
end;

{$IFEND}
