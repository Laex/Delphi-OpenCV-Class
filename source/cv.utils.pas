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

unit cv.utils;

{$I opt.inc}

interface

Uses
  System.Math,
  System.Generics.Collections;

const
  INT_MAX = MaxInt;
  DBL_MAX = MaxDouble;

Type
  TSet<T> = record
  private
    FDict: TDictionary<T, Integer>;
  public
    class operator Initialize(out Dest: TSet<T>);
    class operator Finalize(var Dest: TSet<T>);
    class operator Assign(var Dest: TSet<T>; const [ref] Src: TSet<T>);
    class operator In (const a: T; const b: TSet<T>): Boolean;
    class operator Implicit(const a: TArray<T>): TSet<T>;
    class operator Implicit(const a: TSet<T>): TArray<T>;
    function Contains(const Value: T): Boolean; inline;
    procedure Include(const Value: T); inline;
    procedure Exclude(const Value: T); inline;
  end;

  vftable_func = type Pointer;
  pvftable     = ^vftable_func;

function vftable(const vft: vftable_func; const index: integer): Pointer; {$IFDEF USE_INLINE}inline; {$ENDIF}

implementation

{ TSet<T> }

class operator TSet<T>.Assign(var Dest: TSet<T>; const [ref] Src: TSet<T>);
begin
  // Dest.FDict.ToArray
  // Src.FDict.F
  // .Assign(Src.FDict);
end;

function TSet<T>.Contains(const Value: T): Boolean;
begin
  Result := FDict.ContainsKey(Value);
end;

procedure TSet<T>.Exclude(const Value: T);
begin
  FDict.Remove(Value);
end;

class operator TSet<T>.Finalize(var Dest: TSet<T>);
begin
  Dest.FDict.Free;
end;

class operator TSet<T>.Implicit(const a: TArray<T>): TSet<T>;
begin
  for Var V: T in a do
    Result.Include(V);
end;

class operator TSet<T>.Implicit(const a: TSet<T>): TArray<T>;
begin
 Result := a.FDict.Keys.ToArray;
end;

class operator TSet<T>.In(const a: T; const b: TSet<T>): Boolean;
begin
  Result := b.Contains(a);
end;

procedure TSet<T>.Include(const Value: T);
begin
  FDict.AddOrSetValue(Value, 0);
end;

class operator TSet<T>.Initialize(out Dest: TSet<T>);
begin
  Dest.FDict := TDictionary<T, Integer>.Create;
end;

function vftable(const vft: vftable_func; const index: integer): Pointer;
begin
  Result := pvftable(vft)[index];
end;

end.
