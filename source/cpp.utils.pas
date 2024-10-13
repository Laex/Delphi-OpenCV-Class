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
  INT_MIN     = Pred(-MaxInt);
  INT_MAX     = MaxInt;
  DBL_MAX     = MaxDouble;
  CHAR_BIT    = 8;
  SCHAR_MIN   = (-128);
  SCHAR_MAX   = 127;
  UCHAR_MAX   = $FF;
  DBL_EPSILON = 2.2204460492503131E-16;

Type
  BOOL = bytebool;
  __INT64 = int64;
  pVOID = pointer;
  UNSIGNED_CHAR = byte;
  FLOAT = single;
  SIGNED = Integer;
  SHORT = int16;
  Int = Integer;
  UNSIGNED___INT64 = uint64;
  CVCHAR = AnsiChar;
  unsigned = UInt32;
  UNSIGNED_CVCHAR = UNSIGNED_CHAR;
  UNSIGNED_INT = unsigned;
  UNSIGNED_SHORT = UInt16;
  pSHORT = ^SHORT;
  pFLOAT = ^FLOAT;
  pINT = ^Int;
  pBool = ^BOOL;
  p__INT64 = ^__INT64;
  pCVChar = ^CVCHAR;
  SChar = int8;
  pSChar = ^SChar;
  pUnsigned = ^unsigned;
  size_t = NativeUInt;
  psize_t = ^size_t;
  uInt = Cardinal;
  UShort = UInt16;
  ppAnsiChar = ^pAnsiChar;
  UChar = byte;
  pUChar = type pByte;
  pMatOp = type pointer;
  pUCharConst = pUChar;
  PointerConst = type pointer;

  TVectorType = //
    (           //
{$I vectortype.inc}
  );

  pVector = type pointer;

  TVector_Enumerator<T> = class;

  Vector<T> = record
  public type
    pType = ^T;
  private
{$HINTS OFF}
    // release - size 24
    // Data: array [0 .. 24 - 1] of Byte;
    A: uint64;
    B: uint64;
    C: uint64;
{$IFDEF DEBUG}
    // debug - size 32
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
    function data: pType; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const A: TArray<T>): Vector<T>; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const size: Integer): Vector<T>; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class function noVector: Vector<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  public
    function GetEnumerator: TVector_Enumerator<T>;
  end;

  TVector_Enumerator<T> = class
  protected
    Parent: Vector<T>;
    Position: Integer;
  public
    constructor Create(AParent: Vector<T>);
    function MoveNext: boolean;
    function GetCurrent: T;
    property Current: T read GetCurrent;
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
    procedure assign(const p: pCVChar); {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator assign(var Dest: CppString; const [ref] Src: CppString);
    class operator Implicit(const p: pCVChar): CppString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: string): CppString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: CppString): string; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // class operator Explicit(const s: CppString): string; {$IFDEF USE_INLINE}inline; {$ENDIF}
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
  pvftable = ^vftable_func;

function vftable(const vft: vftable_func; const index: Integer): pointer; {$IFDEF USE_INLINE}inline; {$ENDIF}

const
  endl: String = #13#10;

type
  Tcout = record
    class operator Add(const C: Tcout; const B: pAnsiChar): Tcout; inline;
    class operator Add(const C: Tcout; const B: pChar): Tcout; inline;
    class operator Add(const C: Tcout; const B: pCVChar): Tcout; inline;
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

  TCppEmptyRec = record
  end;

  CLEARSTDVECTOR = type TCppEmptyRec;
  RESIZESTDVECTOR = type TCppEmptyRec;
  COPYSTDVECTOR = type TCppEmptyRec;
  STD__STRING_CONSTRUCTOR_CONCAT_TAG = type TCppEmptyRec;
  STDITEM = type TCppEmptyRec;
  STDPUSHBACK = type TCppEmptyRec;
  STDSIZE = type TCppEmptyRec;
  EXPORTSTRING = type TCppEmptyRec;
  DESTROYSTDVECTOR = type TCppEmptyRec;
  STD_INITIALIZER_LIST_OF_CVCHAR = type TCppEmptyRec;
  STDEMPTY = type TCppEmptyRec;
  STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TCppEmptyRec;
  STD_ALLOCATOR_OF_CVCHAR = type TCppEmptyRec;
  STD_BASIC_STRING_OF_CVCHAR = CppString; // type TEmptyRec;
  STDSETITEM = type TCppEmptyRec;
  CREATESTDVECTOR = type TCppEmptyRec;
  STDPITEM = type TCppEmptyRec;
  VOID = type TCppEmptyRec;
  STD_REVERSE_ITERATOR_OF_STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TCppEmptyRec;
  STD_REVERSE_ITERATOR_OF_STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TCppEmptyRec;
  STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TCppEmptyRec;
  STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR = type TCppEmptyRec;

{$IF not defined(PACKAGE)}
{$IF not defined(EXTERNAL_TYPES)}{$I 'external/std.external.inc'}{$IFEND}
{$IFEND}

Type
  TVectorTypes = array [Low(TVectorType) .. High(TVectorType)] of pointer;

Var
  VectorTypes: TVectorTypes;

implementation

Uses
  cv.opencv;

{ vector<T> }

class operator Vector<T>.assign(var Dest: Vector<T>; const [ref] Src: Vector<T>);
begin
{$IF not defined(PACKAGE)}
  proc_CopyStdVector(@Dest, @Src, vt);
{$IFEND}
end;

procedure Vector<T>.clear;
begin
{$IF not defined(PACKAGE)}
  proc_clearStdVector(@Self, vt);
{$IFEND}
end;

function Vector<T>.data: pType;
begin
{$IF not defined(PACKAGE)}
  Result := dataStdVector(@Self, vt);
{$IFEND}
end;

function Vector<T>.empty: BOOL;
begin
{$IF not defined(PACKAGE)}
  Result := func_StdEmpty(@Self, vt);
{$IFEND}
end;

class operator Vector<T>.Finalize(var Dest: Vector<T>);
begin
{$IF not defined(PACKAGE)}
  proc_DestroyStdVector(@Dest, vt);
{$IFEND}
end;

function Vector<T>.GetEnumerator: TVector_Enumerator<T>;
begin
  Result := TVector_Enumerator<T>.Create(Self);
end;

function Vector<T>.GetItems(const index: uint64): T;
begin
{$IF not defined(PACKAGE)}
  proc_StdItem(@Self, vt, index, @Result);
{$IFEND}
end;

class operator Vector<T>.Implicit(const A: TArray<T>): Vector<T>;
begin
{$IF not defined(PACKAGE)}
  for Var i := 0 to High(A) do
    proc_StdPushBack(@Result, @A[i], vt);
{$IFEND}
end;

class operator Vector<T>.Implicit(const size: Integer): Vector<T>;
begin
  Result.resize(size);
end;

class operator Vector<T>.Initialize(out Dest: Vector<T>);
begin
{$IF not defined(PACKAGE)}
  FillChar(Dest, SizeOf(Dest), 0);
  proc_CreateStdVector(@Dest, vt);
{$IFEND}
end;

class function Vector<T>.noVector: Vector<T>;
begin
  Initialize(Result);
end;

function Vector<T>.pT(const index: uint64): pVector;
begin
{$IF not defined(PACKAGE)}
  proc_StdPItem(@Self, vt, index, Result);
{$IFEND}
end;

procedure Vector<T>.push_back(const Value: T);
begin
{$IF not defined(PACKAGE)}
  proc_StdPushBack(@Self, @Value, vt);
{$IFEND}
end;

procedure Vector<T>.resize(const NewSize: uint64);
begin
{$IF not defined(PACKAGE)}
  proc_resizeStdVector(@Self, NewSize, vt);
{$IFEND}
end;

procedure Vector<T>.setItems(const index: uint64; const Value: T);
begin
{$IF not defined(PACKAGE)}
  proc_StdSetItem(@Self, vt, index, @Value);
{$IFEND}
end;

function Vector<T>.size: { UInt64 } int64;
begin
{$IF not defined(PACKAGE)}
  Result := func_StdSize(@Self, vt);
{$IFEND}
end;

class function Vector<T>.Vector: Vector<T>;
begin
  Initialize(Result);
end;

class function Vector<T>.vt: TVectorType;
begin
  for Var i: TVectorType := Low(TVectorType) to High(TVectorType) do
    if TypeInfo(T) = VectorTypes[i] then
      Exit(i);
  Raise Exception.Create('VectorType - not defined type' {$IFDEF USE_TYPEINFO} + ' "' + GetTypeName(TypeInfo(T)) + '"' {$ENDIF});

  (*
    vt := TVectorType.vtNone;
    if TypeInfo(T) = TypeInfo(TMat) then
    vt := TVectorType.vtMat
    else if TypeInfo(T) = TypeInfo(TRect) then
    vt := TVectorType.vtRect
    else if TypeInfo(T) = TypeInfo(TPoint) then
    vt := TVectorType.vtPoint
    else if TypeInfo(T) = TypeInfo(Vector<TPoint>) then
    vt := TVectorType.vtVectorPoint
    else if TypeInfo(T) = TypeInfo(TPoint2f) then
    vt := TVectorType.vtPoint2f
    else if TypeInfo(T) = TypeInfo(TScalar) then
    vt := TVectorType.vtScalar
    else if TypeInfo(T) = TypeInfo(uchar) then // vector<uchar>
    vt := TVectorType.vtUchar
    else if TypeInfo(T) = TypeInfo(FLOAT) then // vector<float>
    vt := TVectorType.vtFloat
    else if TypeInfo(T) = TypeInfo(INT) then // vector<float>
    vt := TVectorType.vtInt
    else if TypeInfo(T) = TypeInfo(TVec4i) then // vector<float>
    vt := TVectorType.vtVec4i
    else if TypeInfo(T) = TypeInfo(TVec6f) then // vector<float>
    vt := TVectorType.vtVec6f
    else if TypeInfo(T) = TypeInfo(Vector<TPoint2f>) then // vector<float>
    vt := TVectorType.vtVectorPoint2f
    else if TypeInfo(T) = TypeInfo(Vector<TMat>) then // vector<float>
    vt := TVectorType.vtVectorMat
    else if TypeInfo(T) = TypeInfo(CppString) then // vector<CppString>
    vt := TVectorType.vtString
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
  *)
end;

{ CppString }

procedure CppString.assign(const p: pCVChar);
begin
{$IF not defined(PACKAGE)}
  if Assigned(p) then
    class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_3(Self, p);
{$IFEND}
end;

class operator CppString.assign(var Dest: CppString; const [ref] Src: CppString);
begin
{$IF not defined(PACKAGE)}
  class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_1(Dest, Src);
{$IFEND}
end;

procedure CppString.erase(const _Off: uint64);
begin
{$IF not defined(PACKAGE)}
  class_virt_func_STD_BASIC_STRING_OF_CVCHAR_erase_3(Self, _Off);
{$IFEND}
end;

(*
  class operator CppString.Explicit(const s: CppString): string;
  begin
  {$IF not defined(PACKAGE)}
  Result := string(class_virt_func_STD_BASIC_STRING_OF_CVCHAR_c_str(s));
  {$ifend}
  end;
*)

class operator CppString.Finalize(var Dest: CppString);
begin
{$IF not defined(PACKAGE)}
  destructor_STD_BASIC_STRING_OF_CVCHAR(Dest);
{$IFEND}
end;

class operator CppString.Implicit(const s: string): CppString;
begin
  Result.assign(pCVChar(AnsiString(s)));
end;

class operator CppString.Implicit(const p: pCVChar): CppString;
begin
  Result.assign(p);
end;

class operator CppString.Implicit(const s: CppString): string;
begin
{$IF not defined(PACKAGE)}
  Result := string(class_virt_func_STD_BASIC_STRING_OF_CVCHAR_c_str(s));
{$IFEND}
end;

class operator CppString.Initialize(out Dest: CppString);
begin
{$IF not defined(PACKAGE)}
  constructor_STD_BASIC_STRING_OF_CVCHAR_14(Dest);
{$IFEND}
end;

function CppString.length: uint64;
begin
{$IF not defined(PACKAGE)}
  Result := class_virt_func_STD_BASIC_STRING_OF_CVCHAR_length(Self);
{$IFEND}
end;

function CppString.size: uint64;
begin
{$IF not defined(PACKAGE)}
  Result := class_virt_func_STD_BASIC_STRING_OF_CVCHAR_size(Self);
{$IFEND}
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

class operator Tcout.Add(const C: Tcout; const B: pChar): Tcout;
begin
  write(CppReplace(String(B)));
  Result := C;
end;

class operator Tcout.Add(const C: Tcout; const B: pAnsiChar): Tcout;
begin
  write(CppReplace(String(B)));
  Result := C;
end;

class operator Tcout.Add(const C: Tcout; const B: pCVChar): Tcout;
begin
  write(CppReplace(String(B)));
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
  C  := AllocMem(cs);
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

{ TVector_Enumerator<T> }

constructor TVector_Enumerator<T>.Create(AParent: Vector<T>);
begin
  inherited Create;
  Position := -1;
  Parent   := AParent;
end;

function TVector_Enumerator<T>.GetCurrent: T;
begin
  Result := Parent[Position];
end;

function TVector_Enumerator<T>.MoveNext: boolean;
begin
  Result := Position < (Parent.size - 1);
  if Result then
    Inc(Position);
end;

procedure InitVectorTypesArray;
begin
  VectorTypes[vtNone]          := nil;
  VectorTypes[vtMat]           := TypeInfo(TMat);          // = 1,   // vector<Mat>
  VectorTypes[vtRect]          := TypeInfo(TRect);         // = 2,   // vector<Rect>
  VectorTypes[vtPoint]         := TypeInfo(TPoint);        // = 3,   // vector<Point>
  VectorTypes[vtVectorMat]     := TypeInfo(Vector<TMat>);  // = 4,   // vector<vector<Mat>>
  VectorTypes[vtVectorRect]    := TypeInfo(Vector<TRect>); // = 5,   // vector<vector<Rect>>
  VectorTypes[vtVectorPoint]   := TypeInfo(Vector<TPoint>); // = 6,   // vector<vector<Point>>
  VectorTypes[vtPoint2f]       := TypeInfo(TPoint2f); // = 7,   // vector<Point2f>
  VectorTypes[vtScalar]        := TypeInfo(TScalar);  // = 8,   // vector<Scalar>
  VectorTypes[vtUchar]         := TypeInfo(UChar);    // = 9,   // vector<uchar>
  VectorTypes[vtFloat]         := TypeInfo(FLOAT);    // = 10,  // vector<float>
  VectorTypes[vtInt]           := TypeInfo(Int);      // = 11,  // vector<int>
  VectorTypes[vtVec4i]         := TypeInfo(TVec4i);   // = 12,  // vector<Vec4i>
  VectorTypes[vtGMat]          := nil;                // TypeInfo(TGMat); // = 13,  // vector<GMat>
  VectorTypes[vtGCompileArg]   := nil;                // TypeInfo(TGCompileArg); // = 14,  // vector<GCompileArg>
  VectorTypes[vtVec6f]         := TypeInfo(TVec6f);   // = 15,  // vector<Vec6f>
  VectorTypes[vtVectorPoint2f] := TypeInfo(Vector<TPoint2f>); // = 16, // vector<vector<Point2f>>
  VectorTypes[vtString]        := TypeInfo(CppString); // = 17   // vector<std::String>
end;

initialization

InitVectorTypesArray;

argv      := [ExtractFileName(ParamStr(0))];
for Var i := 1 to ParamCount do
  argv    := argv + [ParamStr(i)];

argc := 1 + ParamCount;

end.
