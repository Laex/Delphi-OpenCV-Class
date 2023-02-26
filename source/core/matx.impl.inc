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

{$if defined(OPENCV_CORE_MATX_HPP) and (not defined(OPENCV_CORE_MATX_HPP_IMPL))}
{$DEFINE OPENCV_CORE_MATX_HPP_IMPL}

function TVec3b.getItem(const index: integer): uchar;
begin
  Assert((index >= 0) and (index < 3));
  Result := _Data[index];
end;

class operator TVec3b.Implicit(const a: TArray<uchar>): TVec3b;
begin
  Assert(length(a) > 2);
  // slow
  for Var i := 0 to High(Result._Data) do
    Result._Data[i] := a[i];
end;

procedure TVec3b.setItem(const index: integer; const Value: uchar);
begin
  Assert((index >= 0) and (index < 3));
  _Data[index] := Value;
end;

function Vec3b(const a,b,c:uchar):TVec3b;
begin
  Result := [a,b,c];
end;

{ TMatSize }

function TMatSize.getValues(const Index:Integer):Int;
begin
 Result := p[Index];
end;

{$I 'core/saturate.impl.inc'}

{$ifend}