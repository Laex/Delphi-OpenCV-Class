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

{$IFDEF OPENCV_CORE_HAL_INTERFACE_H}

function CV_MAT_DEPTH(flags: Int): Int;
begin
  Result := flags and CV_MAT_DEPTH_MASK;
end;

function CV_MAKETYPE(depth, cn: Int): Int;
begin
  Result := (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
end;

function CV_MAKE_TYPE(depth, cn: Int): Int;
begin
  Result := CV_MAKETYPE(depth, cn);
end;

function CV_8UC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_8U, n);
end;

function CV_8SC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_8S, n);
end;

function CV_16UC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_16U, n);
end;

function CV_16SC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_16S, n);
end;

function CV_32SC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_32S, n);
end;

function CV_32FC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_32F, n);
end;

function CV_64FC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_64F, n);
end;

function CV_16FC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_16F, n);
end;
{$ENDIF OPENCV_CORE_HAL_INTERFACE_H}