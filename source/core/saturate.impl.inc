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

{$IF defined(OPENCV_CORE_SATURATE_HPP) and (not defined(OPENCV_CORE_SATURATE_HPP_IMPL))}
{$DEFINE OPENCV_CORE_SATURATE_HPP_IMPL}
//
{$MESSAGE WARN 'TODO: saturate_cast<T>'}

class function saturate_cast<T>.cast(const v: Int): T;
begin
   Result := TValue(v).AsType<T>;
   if TypeInfo(T)=TypeInfo(Int) then
   pInt(@Result)^ := v else
   if TypeInfo(T)=TypeInfo(uchar) then
   pUchar(@Result)^ := uchar(iif.iif<Int>(uint(v) <= UCHAR_MAX, v, iif.iif<Int>(v > 0, UCHAR_MAX, 0)));
end;

class function saturate_cast<T>.cast(const v: float): T;
begin
   Result := TValue(v).AsType<T>;
end;

class function saturate_cast<T>.cast(const v: double): T;
begin
   Result := TValue(v).AsType<T>;
end;

{$IFEND}
