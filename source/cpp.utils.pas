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

unit cpp.utils;

{$I opt.inc}

interface

Uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections
{$IFDEF USE_TYPEINFO}
    , System.TypInfo
{$ENDIF}
    ;
{$I core/version.inc}

const
  INT_MAX = MaxInt;
  DBL_MAX = MaxDouble;

Type
  BOOL = ByteBool;

  TVectorType = //
    (           //
{$I vectortype.inc}
  );

  pVector = type Pointer;

  Vector<T> = record
  private
{$HINTS OFF}
    Data: array [0 .. 31] of Byte;
{$HINTS ON}
    class function vt: TVectorType; static;
    function GetItems(const index: UInt64): T;
    procedure setItems(const index: UInt64; const Value: T);
  public
    class operator Initialize(out Dest: Vector<T>);
    class operator Finalize(var Dest: Vector<T>);
    class operator assign(var Dest: Vector<T>; const [ref] Src: Vector<T>);

    function size: { UInt64 } Int64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function empty: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure push_back(const Value: T); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure resize(const NewSize: UInt64); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure clear(); {$IFDEF USE_INLINE}inline; {$ENDIF}
      //
    function pT(const index: UInt64): Pointer; {$IFDEF USE_INLINE}inline; {$ENDIF}
    property v[const index: UInt64]: T read GetItems write setItems; default;
    class operator Implicit(const a: TArray<T>): Vector<T>; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class function noVector: Vector<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  pCppString = ^CppString;

  CppString = record
  private
{$HINTS OFF}
    Dummy: array [0 .. 39] of Byte;
{$HINTS ON}
  public
    class operator Initialize(out Dest: CppString);
    class operator Finalize(var Dest: CppString);

    function length: UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function size: UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure erase(const _Off: UInt64 = 0); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure assign(const p: pAnsiChar); {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator assign(var Dest: CppString; const [ref] Src: CppString);
    class operator Implicit(const p: pAnsiChar): CppString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: string): CppString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: CppString): string; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TPtr<T: record > = record
  public type
    pT = ^T;
  public
    _Ptr: pT;
    _Rep: Pointer;
    _Ref: integer;
    function v: pT; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator assign(var Dest: TPtr<T>; const [ref] Src: TPtr<T>);
    class operator Finalize(var Dest: TPtr<T>);
  end;

  makePtr<T: record > = record
  public type
    pT = ^T;
  public
    class function Create(const v: T): TPtr<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TSet<T> = record
  private
    FDict: TDictionary<T, integer>;
  public
    class operator Initialize(out Dest: TSet<T>);
    class operator Finalize(var Dest: TSet<T>);
    class operator assign(var Dest: TSet<T>; const [ref] Src: TSet<T>);
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

const
  endl: String = #13#10;

type
  Tcout = record
    class operator Add(const c: Tcout; const b: String): Tcout; inline;
    class operator Add(const c: Tcout; const b: double): Tcout; inline;
  end;

function CppReplace(const text: String): String;

{$I 'external/cpp.std.import.inc'}

Var
  cout: Tcout;
  cerr: Tcout;
  argv: TArray<string>;

function isIntNumber(const v: String): Boolean;
function isIntNumberWithDefault(const v: String; const D: integer = 0): integer;

type
  TSwap = record
    class procedure swap<T>(var a, b: Vector<T>); static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

implementation

Uses
  cv.opencv;

  { vector<T> }

class operator Vector<T>.assign(var Dest: Vector<T>; const [ref] Src: Vector<T>);
begin
  CopyStdVector(@Dest, @Src, vt);
end;

procedure Vector<T>.clear;
begin
  clearStdVector(@Self, vt);
end;

function Vector<T>.empty: BOOL;
begin
  Result := StdEmpty(@Self, vt);
end;

class operator Vector<T>.Finalize(var Dest: Vector<T>);
begin
  DestroyStdVector(@Dest, vt);
end;

function Vector<T>.GetItems(const index: UInt64): T;
begin
  StdGetItem(@Self, vt, index, @Result);
end;

class operator Vector<T>.Implicit(const a: TArray<T>): Vector<T>;
begin
  for Var i := 0 to High(a) do
    StdPushBack(@Result, @a[i], vt);
end;

class operator Vector<T>.Initialize(out Dest: Vector<T>);
begin
  FillChar(Dest, SizeOf(Dest), 0);
  CreateStdVector(@Dest, vt);
end;

class function Vector<T>.noVector: Vector<T>;
begin
  Initialize(Result);
end;

function Vector<T>.pT(const index: UInt64): Pointer;
begin
  StdGetPItem(@Self, vt, index, pVector(Result));
end;

procedure Vector<T>.push_back(const Value: T);
begin
  StdPushBack(@Self, @Value, vt);
end;

procedure Vector<T>.resize(const NewSize: UInt64);
begin
  resizeStdVector(@Self, NewSize, vt);
end;

procedure Vector<T>.setItems(const index: UInt64;const Value: T);
begin
  StdSetItem(@Self, vt, index, @Value);
end;

function Vector<T>.size: { UInt64 } Int64;
begin
  Result := StdSize(@Self, vt);
end;

class function Vector<T>.vt: TVectorType;
begin
  if TypeInfo(T) = TypeInfo(TMat) then
    vt := vtMat
  else if TypeInfo(T) = TypeInfo(TRect) then
    vt := vtRect
  else if TypeInfo(T) = TypeInfo(TPoint) then
    vt := vtPoint
  else if TypeInfo(T) = TypeInfo(Vector<TPoint>) then
    vt := vtVectorPoint
  else if TypeInfo(T) = TypeInfo(TPoint2f) then
    vt := vtPoint2f
  else if TypeInfo(T) = TypeInfo(TScalar) then
    vt := vtScalar
  else if TypeInfo(T) = TypeInfo(uchar) then // vector<uchar>
    vt := vtUchar
  else if TypeInfo(T) = TypeInfo(float) then // vector<float>
    vt := vtFloat
  else if TypeInfo(T) = TypeInfo(Int) then // vector<float>
    vt := vtInt
  else if TypeInfo(T) = TypeInfo(TVec4i) then // vector<float>
    vt := vtVec4i
      // else if TypeInfo(T) = TypeInfo(TGMat) then // vector<GMat>
      // vt := vtGMat
      // else if TypeInfo(T) = TypeInfo(TGCompileArg) then // vector<GCompileArg>
      // vt := vtGCompileArg
      // else if TypeInfo(T) = TypeInfo(pMat) then // vector<GCompileArg>
      // vt := vtpVoid
  else
  begin
    Var
    AssertMsg := 'VectorType - not defined type'
{$IFDEF USE_TYPEINFO}
      + ' "' + GetTypeName(TypeInfo(T)) + '"'
{$ENDIF}
      ;
    Assert(false, AssertMsg);
  end;
end;

  { CppString }

procedure CppString.assign(const p: pAnsiChar);
begin
  assign_CppString(@Self, p);
end;

class operator CppString.assign(var Dest: CppString; const [ref] Src: CppString);
begin
  assign_CppString(pCppString(@Dest), pCppString(@Src));
end;

procedure CppString.erase(const _Off: UInt64);
begin
  erase_CppString(@Self, _Off);
end;

class operator CppString.Finalize(var Dest: CppString);
begin
  Destructor_CppString(@Dest);
end;

class operator CppString.Implicit(const s: string): CppString;
begin
  Result.assign(pAnsiChar(AnsiString(s)));
end;

class operator CppString.Implicit(const p: pAnsiChar): CppString;
begin
  Result.assign(p);
end;

class operator CppString.Implicit(const s: CppString): string;
begin
  Result := string(c_str_CppString(@s));
end;

class operator CppString.Initialize(out Dest: CppString);
begin
  Constructor_CppString(@Dest);
end;

function CppString.length: UInt64;
begin
  Result := length_CppString(@Self);
end;

function CppString.size: UInt64;
begin
  Result := size_CppString(@Self);
end;

  { TPtr<T> }

class operator TPtr<T>.assign(var Dest: TPtr<T>; const [ref] Src: TPtr<T>);
Var
  p: ^TPtr<T>;
begin
  Move(Src, Dest, SizeOf(Dest));
  p := @Src;
  Inc(p^._Ref);
end;

class operator TPtr<T>.Finalize(var Dest: TPtr<T>);
begin
  if Dest._Ref = 0 then
    Finalize(pT(Dest._Ptr)^)
  else
    Dec(Dest._Ref);
end;

function TPtr<T>.v: pT;
begin
  Result := _Ptr;
end;

function CppReplace(const text: String): String;
begin
  Result := text.Replace('\n', #13#10).Replace('\t', #9);
end;

  { Tcout }

class operator Tcout.Add(const c: Tcout; const b: String): Tcout;
begin
  write(CppReplace(b));
  Result := c;
end;

class operator Tcout.Add(const c: Tcout; const b: double): Tcout;
begin
  write(b.ToString);
  Result := c;
end;

  { TSet<T> }

class operator TSet<T>.assign(var Dest: TSet<T>; const [ref] Src: TSet<T>);
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
  for Var v: T in a do
    Result.Include(v);
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
  Dest.FDict := TDictionary<T, integer>.Create;
end;

function vftable(const vft: vftable_func; const index: integer): Pointer;
begin
  Result := pvftable(vft)[index];
end;

  { makePtr<T> }

class function makePtr<T>.Create(const v: T): TPtr<T>;
begin
  Result._Ptr := @v;
end;

function isIntNumber(const v: String): Boolean;
Var
  R: integer;
begin
  Result := TryStrToInt(v, R);
end;

function isIntNumberWithDefault(const v: String; const D: integer = 0): integer;
begin
  if not TryStrToInt(v, Result) then
    Result := D;
end;

  { TSwap }

class procedure TSwap.swap<T>(var a, b: Vector<T>);
Var
  c: Pointer;
  cs: size_t;
begin
  cs := SizeOf(a);
  c := AllocMem(cs);
  try
    Move(a, c^, cs);
    Move(b, a, cs);
    Move(c^, b, cs);
  finally
    FreeMem(c);
  end;
end;

initialization

argv := [ExtractFileName(ParamStr(0))];
for Var i := 1 to ParamCount do
  argv := argv + [ParamStr(i)];

end.
