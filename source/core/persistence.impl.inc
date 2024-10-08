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

{$IF defined(OPENCV_CORE_PERSISTENCE_HPP) and (not defined(OPENCV_CORE_PERSISTENCE_IMPL_HPP))}
{$DEFINE OPENCV_CORE_PERSISTENCE_IMPL_HPP}

{ TFileNode }
class operator TFileNode.Initialize(out Dest: TFileNode);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_FILENODE_3(Dest);
{$IFEND}
end;

{ TFileNodeIterator }
class operator TFileNodeIterator.Initialize(out Dest: TFileNodeIterator);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_FILENODEITERATOR_2(Dest);
{$IFEND}
end;

{ TFileStorage }

class operator TFileStorage.Initialize(out Dest: TFileStorage);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_FILESTORAGE_2(Dest);
{$IFEND}
end;

class operator TFileStorage.Finalize(var Dest: TFileStorage);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_FILESTORAGE(Dest);
{$IFEND}
end;

class function TFileStorage.FileStorage(const filename: String; flags: TMode; const encoding: String = ''): TFileStorage;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_FILESTORAGE_1(Result, filename, Int(flags), encoding);
{$IFEND}
end;

class function TFileStorage.FileStorage(): TFileStorage;
begin
{$IF not defined(PACKAGE)}
  // called Initialize for Result
{$IFEND}
end;

function TFileStorage.isOpened(): bool;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_FILESTORAGE_isOpened(Self);
{$IFEND}
end;

{$IFEND} // OPENCV_CORE_PERSISTENCE_IMPL_HPP
