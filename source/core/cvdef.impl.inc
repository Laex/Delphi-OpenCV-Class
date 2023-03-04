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

{$IF defined(OPENCV_CORE_CVDEF_H) and (not defined(OPENCV_CORE_CVDEF_H_IMPL))}
{$DEFINE  OPENCV_CORE_CVDEF_H_IMPL}

function MIN(a, b: Int): Int;
begin
  Result := System.Math.MIN(a, b);
end;

function MAX(a, b: Int): Int;
begin
  Result := System.Math.MAX(a, b);
end;

function CV_MAT_TYPE(flags:int):int;
begin
  Result:= ((flags) and CV_MAT_TYPE_MASK);
end;

function CV_ELEM_SIZE1(&type:int):int;
begin
  // CV_ELEM_SIZE1(type) = ((0x28442211 >> CV_MAT_DEPTH(type)*4) & 15);
  Result := (($28442211 shr CV_MAT_DEPTH(&type)*4) and 15);
end;

function CV_MAT_CN(flags:int):int;
begin
  // CV_MAT_CN(flags)       = ((((flags) & CV_MAT_CN_MASK) >> CV_CN_SHIFT) + 1);
  Result := ((((flags) and CV_MAT_CN_MASK) shr CV_CN_SHIFT) + 1);
end;

function CV_ELEM_SIZE(&type:int):int;
begin
    // CV_ELEM_SIZE(type) =(CV_MAT_CN(type)*CV_ELEM_SIZE1(type));
  Result := CV_MAT_CN(&type)*CV_ELEM_SIZE1(&type);
end;

{$i core/hal/interface.impl.inc}
{$i core/fast_math.impl.inc}

{$IFEND OPENCV_CORE_CVDEF_H_IMPL}
