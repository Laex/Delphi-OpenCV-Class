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
  INT_MIN   = Pred(-MaxInt);
  INT_MAX   = MaxInt;
  DBL_MAX   = MaxDouble;
  CHAR_BIT  = 8;
  SCHAR_MIN = (-128);
  SCHAR_MAX = 127;
  UCHAR_MAX = $FF;

Type
  BOOL             = bytebool;
  __INT64          = int64;
  pVOID            = pointer;
  UNSIGNED_CHAR    = byte;
  FLOAT            = single;
  SIGNED           = Integer;
  SHORT            = int16;
  INT              = Integer;
  UNSIGNED___INT64 = uint64;
  CVCHAR           = AnsiChar;
  unsigned         = UInt32;
  UNSIGNED_CVCHAR  = UNSIGNED_CHAR;
  UNSIGNED_INT     = unsigned;
  UNSIGNED_SHORT   = UInt16;
  pSHORT           = ^SHORT;
  pFLOAT           = ^FLOAT;
  pINT             = ^INT;
  pBOOL            = ^BOOL;
  p__INT64         = ^__INT64;
  pCVCHAR          = pAnsiChar;
  schar = int8;
  pschar = ^schar;
  punsigned = ^unsigned;

  TVectorType = //
    (           //
{$I vectortype.inc}
  );

  pVector = type pointer;

  Vector<T> = record
  private
{$HINTS OFF}
    // release 24
    // Data: array [0 .. 24 - 1] of Byte;
    A: uint64;
    B: uint64;
    C: uint64;
{$IFDEF DEBUG}
    // debug 32
    // Data: array [0 .. 32 - 1] of Byte;
    D: uint64;
{$ENDIF}
{$HINTS ON}
    class function vt: TVectorType; static;
    function GetItems(const index: uint64): T;
    procedure setItems(const index: uint64; const Value: T);
  public
    class operator Initialize(out Dest: Vector<T>);
    class operator Finalize(var Dest: Vector<T>);
    class operator assign(var Dest: Vector<T>; const [ref] Src: Vector<T>);
    class function Vector: Vector<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function size: { UInt64 } int64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function empty: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure push_back(const Value: T); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure resize(const NewSize: uint64); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure clear(); {$IFDEF USE_INLINE}inline; {$ENDIF}
    //
    function pT(const index: uint64): pVector; {$IFDEF USE_INLINE}inline; {$ENDIF}
    property v[const index: uint64]: T read GetItems write setItems; default;
    class operator Implicit(const A: TArray<T>): Vector<T>; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const size: Integer): Vector<T>; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class function noVector: Vector<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  pCppString = ^CppString;

  CppString = record
  private
{$HINTS OFF}
    Dummy: array [0 .. 39] of byte;
{$HINTS ON}
  public
    class operator Initialize(out Dest: CppString);
    class operator Finalize(var Dest: CppString);

    function length: uint64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function size: uint64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure erase(const _Off: uint64 = 0); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure assign(const p: pAnsiChar); {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator assign(var Dest: CppString; const [ref] Src: CppString);
    class operator Implicit(const p: pAnsiChar): CppString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: string): CppString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: CppString): string; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TPtr<T { : record } > = record
  public type
    pT = ^T;
  public
    _Ptr: pT;
    _Rep: pointer;
    _Ref: Integer;
    function v: pT; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator assign(var Dest: TPtr<T>; const [ref] Src: TPtr<T>);
    class operator Finalize(var Dest: TPtr<T>);
    class operator Equal(const A: TPtr<T>; const B: boolean): boolean; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  makePtr<T: record > = record
  public type
    pT = ^T;
  public
    class function Create(const v: T): TPtr<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TSet<T> = record
  private
    FDict: TDictionary<T, Integer>;
  public
    class operator Initialize(out Dest: TSet<T>);
    class operator Finalize(var Dest: TSet<T>);
    class operator assign(var Dest: TSet<T>; const [ref] Src: TSet<T>);
    class operator In (const A: T; const B: TSet<T>): boolean;
    class operator Implicit(const A: TArray<T>): TSet<T>;
    class operator Implicit(const A: TSet<T>): TArray<T>;
    function Contains(const Value: T): boolean; inline;
    procedure Include(const Value: T); inline;
    procedure Exclude(const Value: T); inline;
  end;

  vftable_func = type pointer;
  pvftable     = ^vftable_func;

function vftable(const vft: vftable_func; const index: Integer): pointer; {$IFDEF USE_INLINE}inline; {$ENDIF}

const
  endl: String = #13#10;

type
  Tcout = record
    class operator Add(const C: Tcout; const B: String): Tcout; inline;
    class operator Add(const C: Tcout; const B: DOUBLE): Tcout; inline;
  end;

function CppReplace(const text: String): String;

Var
  cout: Tcout;
  cerr: Tcout;
  argv: TArray<string>;
  argc: Integer;

function isIntNumber(const v: String): boolean;
function isIntNumberWithDefault(const v: String; const D: Integer = 0): Integer;

type
  TSwap = record
    class procedure swap<T>(var A, B: Vector<T>); static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

procedure printf(const text: string); {$IFDEF USE_INLINE}inline; {$ENDIF}

Type
  iif = record
    class function iif<T>(const Cond: boolean; const ifTrue, ifFalse: T): T; static; inline;
  end;

  TEmptyRec = record
  end;

  CLEARSTDVECTOR                                 = type TEmptyRec;
  RESIZESTDVECTOR                                = type TEmptyRec;
  COPYSTDVECTOR                                  = type TEmptyRec;
  STD__STRING_CONSTRUCTOR_CONCAT_TAG             = type TEmptyRec;
  STDITEM                                        = type TEmptyRec;
  STDPUSHBACK                                    = type TEmptyRec;
  STDSIZE                                        = type TEmptyRec;
  EXPORTSTRING                                   = type TEmptyRec;
  DESTROYSTDVECTOR                               = type TEmptyRec;
  STD_INITIALIZER_LIST_OF_CVCHAR                 = type TEmptyRec;
  STDEMPTY                                       = type TEmptyRec;
  STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TEmptyRec;
  STD_ALLOCATOR_OF_CVCHAR                        = type TEmptyRec;
  STD_BASIC_STRING_OF_CVCHAR                     = CppString; // type TEmptyRec;
  STDSETITEM                                     = type TEmptyRec;
  CREATESTDVECTOR                                = type TEmptyRec;
  STDPITEM                                       = type TEmptyRec;
  VOID                                           = type TEmptyRec;
  STD_REVERSE_ITERATOR_OF_STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TEmptyRec;
  STD_REVERSE_ITERATOR_OF_STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TEmptyRec;
  STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TEmptyRec;
  STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TEmptyRec;

{$I 'std.external.inc'}

implementation

Uses
  cv.opencv;

{ vector<T> }

class operator Vector<T>.assign(var Dest: Vector<T>; const [ref] Src: Vector<T>);
begin
  proc_CopyStdVector(@Dest, @Src, vt);
end;

procedure Vector<T>.clear;
begin
  proc_clearStdVector(@Self, vt);
end;

function Vector<T>.empty: BOOL;
begin
  Result := func_StdEmpty(@Self, vt);
end;

class operator Vector<T>.Finalize(var Dest: Vector<T>);
begin
  proc_DestroyStdVector(@Dest, vt);
end;

function Vector<T>.GetItems(const index: uint64): T;
begin
  proc_StdItem(@Self, vt, index, @Result);
end;

class operator Vector<T>.Implicit(const A: TArray<T>): Vector<T>;
begin
  for Var i := 0 to High(A) do
    proc_StdPushBack(@Result, @A[i], vt);
end;

class operator Vector<T>.Implicit(const size: Integer): Vector<T>;
begin
  Result.resize(size);
end;

class operator Vector<T>.Initialize(out Dest: Vector<T>);
begin
  FillChar(Dest, SizeOf(Dest), 0);
  proc_CreateStdVector(@Dest, vt);
end;

class function Vector<T>.noVector: Vector<T>;
begin
  Initialize(Result);
end;

function Vector<T>.pT(const index: uint64): pVector;
begin
  proc_StdPItem(@Self, vt, index, Result);
end;

procedure Vector<T>.push_back(const Value: T);
begin
  proc_StdPushBack(@Self, @Value, vt);
end;

procedure Vector<T>.resize(const NewSize: uint64);
begin
  proc_resizeStdVector(@Self, NewSize, vt);
end;

procedure Vector<T>.setItems(const index: uint64; const Value: T);
begin
  proc_StdSetItem(@Self, vt, index, @Value);
end;

function Vector<T>.size: { UInt64 } int64;
begin
  Result := func_StdSize(@Self, vt);
end;

class function Vector<T>.Vector: Vector<T>;
begin
  Initialize(Result);
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
  else if TypeInfo(T) = TypeInfo(FLOAT) then // vector<float>
    vt := vtFloat
  else if TypeInfo(T) = TypeInfo(INT) then // vector<float>
    vt := vtInt
  else if TypeInfo(T) = TypeInfo(TVec4i) then // vector<float>
    vt := vtVec4i
  else if TypeInfo(T) = TypeInfo(TVec6f) then // vector<float>
    vt := vtVec6f
  else if TypeInfo(T) = TypeInfo(Vector<TPoint2f>) then // vector<float>
    vt := vtVectorPoint2f
  else if TypeInfo(T) = TypeInfo(Vector<TMat>) then // vector<float>
    vt := vtVectorMat
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
  class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_3(Self, p);
end;

class operator CppString.assign(var Dest: CppString; const [ref] Src: CppString);
begin
  class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_1(Dest, Src);
end;

procedure CppString.erase(const _Off: uint64);
begin
  class_virt_func_STD_BASIC_STRING_OF_CVCHAR_erase_3(Self, _Off);
end;

class operator CppString.Finalize(var Dest: CppString);
begin
  destructor_STD_BASIC_STRING_OF_CVCHAR(Dest);
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
  Result :=
  string(class_virt_func_STD_BASIC_STRING_OF_CVCHAR_c_str(s));
end;

class operator CppString.Initialize(out Dest: CppString);
begin
  constructor_STD_BASIC_STRING_OF_CVCHAR_14(Dest);
end;

function CppString.length: uint64;
begin
  Result := class_virt_func_STD_BASIC_STRING_OF_CVCHAR_length(Self);
end;

function CppString.size: uint64;
begin
  Result := class_virt_func_STD_BASIC_STRING_OF_CVCHAR_size(Self);
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

class operator TPtr<T>.Equal(const A: TPtr<T>; const B: boolean): boolean;
begin
  Result := Assigned(A._Ptr) = B;
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

class operator Tcout.Add(const C: Tcout; const B: String): Tcout;
begin
  write(CppReplace(B));
  Result := C;
end;

class operator Tcout.Add(const C: Tcout; const B: DOUBLE): Tcout;
begin
  write(B.ToString);
  Result := C;
end;

{ TSet<T> }

class operator TSet<T>.assign(var Dest: TSet<T>; const [ref] Src: TSet<T>);
begin
  // Dest.FDict.ToArray
  // Src.FDict.F
  // .Assign(Src.FDict);
end;

function TSet<T>.Contains(const Value: T): boolean;
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

class operator TSet<T>.Implicit(const A: TArray<T>): TSet<T>;
begin
  for Var v: T in A do
    Result.Include(v);
end;

class operator TSet<T>.Implicit(const A: TSet<T>): TArray<T>;
begin
  Result := A.FDict.Keys.ToArray;
end;

class operator TSet<T>.In(const A: T; const B: TSet<T>): boolean;
begin
  Result := B.Contains(A);
end;

procedure TSet<T>.Include(const Value: T);
begin
  FDict.AddOrSetValue(Value, 0);
end;

class operator TSet<T>.Initialize(out Dest: TSet<T>);
begin
  Dest.FDict := TDictionary<T, Integer>.Create;
end;

function vftable(const vft: vftable_func; const index: Integer): pointer;
begin
  Result := pvftable(vft)[index];
end;

{ makePtr<T> }

class function makePtr<T>.Create(const v: T): TPtr<T>;
begin
  Result._Ptr := @v;
end;

function isIntNumber(const v: String): boolean;
Var
  R: Integer;
begin
  Result := TryStrToInt(v, R);
end;

function isIntNumberWithDefault(const v: String; const D: Integer = 0): Integer;
begin
  if not TryStrToInt(v, Result) then
    Result := D;
end;

{ TSwap }

class procedure TSwap.swap<T>(var A, B: Vector<T>);
Var
  C: pointer;
  cs: size_t;
begin
  cs := SizeOf(A);
  C := AllocMem(cs);
  try
    Move(A, C^, cs);
    Move(B, A, cs);
    Move(C^, B, cs);
  finally
    FreeMem(C);
  end;
end;

procedure printf(const text: string);
begin
  cout + text;
end;

{ iif }

class function iif.iif<T>(const Cond: boolean; const ifTrue, ifFalse: T): T;
begin
  if Cond then
    Result := ifTrue
  else
    Result := ifFalse;
end;

initialization

argv := [ExtractFileName(ParamStr(0))];
for Var i := 1 to ParamCount do
  argv := argv + [ParamStr(i)];

argc := 1 + ParamCount;

end.
