(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

{$IF defined(OPENCV_IMGCODECS_HPP) and (not defined(OPENCV_IMGCODECS_HPP_IMPL))}
{$DEFINE OPENCV_IMGCODECS_HPP_IMPL}

function imread(const filename: String; flag: ImreadModes = IMREAD_COLOR): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := func_imread(CppString(filename), flag);
{$IFEND}
end;

function imreadmulti(const filename: String; Var mats: vector<TMat>; flags: int = IMREAD_ANYCOLOR): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_imreadmulti(CppString(filename), mats, flags);
{$IFEND}
end;

function imreadmulti(const filename: String; Var mats: vector<TMat>; start, count: int; flags: int = IMREAD_ANYCOLOR): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_imreadmulti_1(CppString(filename), mats, start, count, flags);
{$IFEND}
end;

function imcount(const filename: String; flags: int = IMREAD_ANYCOLOR): size_t;
begin
{$IF not defined(PACKAGE)}
  Result := func_imcount(CppString(filename), flags);
{$IFEND}
end;

function imwrite(const filename: String; const img: TInputArray; const params: vector<int>): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_imwrite(CppString(filename), img, params);
{$IFEND}
end;

function imwrite(const filename: String; const img: TInputArray): bool;
begin
  Var
    a: vector<int>;
  Result := imwrite(filename, img, a);
end;

function imwritemulti(const filename: String; const img: TInputArrayOfArrays; const params: vector<int>): bool;
begin
  Result := imwrite(filename, img, params);
end;

function imwritemulti(const filename: String; const img: TInputArrayOfArrays): bool;
begin
  Var
    params: vector<int>;
  Result := imwrite(filename, img, params);
end;

function imdecode(const buf: TInputArray; const flags: Integer): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := func_imdecode(buf, flags);
{$IFEND}
end;

function imdecode(const buf: TInputArray; const flags: int; const dst: pMat): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := func_imdecode_1(buf, flags, dst);
{$IFEND}
end;

function imencode(const ext: string; const img: TInputArray; Var buf: vector<uchar>; const params: vector<int>): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_imencode(CppString(ext), img, buf, params);
{$IFEND}
end;

function imencode(const ext: string; const img: TInputArray; Var buf: vector<uchar>): bool;
begin
{$IF not defined(PACKAGE)}
  Var
    V: vector<int>;
  Result := func_imencode(CppString(ext), img, buf, V);
{$IFEND}
end;

function haveImageReader(const filename: string): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_haveImageReader(CppString(filename));
{$IFEND}
end;

function haveImageWriter(const filename: string): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_haveImageWriter(CppString(filename));
{$IFEND}
end;

{$IFEND}
