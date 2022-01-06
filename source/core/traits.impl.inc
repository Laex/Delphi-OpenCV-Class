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

{$IFDEF OPENCV_CORE_TRAITS_HPP}
{ TDataType<T> }

class function TDataType<T>.generic_type: Int;
Var
  a:TDataType_<T>;
begin
  result := a.generic_type;
end;
class function TDataType<T>.depth: Int;
Var
  a:TDataType_<T>;
begin
  result := a.depth;
end;
class function TDataType<T>.channels: Int;
Var
  a:TDataType_<T>;
begin
  result := a.channels;
end;
class function TDataType<T>.fmt: Int;
Var
  a:TDataType_<T>;
begin
  result := a.fmt;
end;
class function TDataType<T>.&type: Int;
Var
  a:TDataType_<T>;
begin
  result := a.&type;
end;

{ TDataType_<T> }

class operator TDataType_<T>.Initialize(out Dest: TDataType_<T>);
begin
  if TypeInfo(T) = TypeInfo(BOOL) then
    with Dest do
    begin
      generic_type := 0;
      depth := CV_8U;
      channels := 1;
      fmt := Int('u');
      &type := CV_MAKETYPE(depth, channels)
    end
  else if TypeInfo(T) = TypeInfo(uchar) then
    with Dest do
    begin
      generic_type := 0;
      depth := CV_8U;
      channels := 1;
      fmt := Int('u');
      &type := CV_MAKETYPE(depth, channels)
    end
  else
    // template<> class DataType<schar>
    // {
    // public:
    // typedef schar       value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_8S,
    // channels     = 1,
    // fmt          = (int)'c',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    // template<> class DataType<char>
    // {
    // public:
    // typedef schar       value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_8S,
    // channels     = 1,
    // fmt          = (int)'c',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    //
    // template<> class DataType<ushort>
    // {
    // public:
    // typedef ushort      value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_16U,
    // channels     = 1,
    // fmt          = (int)'w',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    //
    // template<> class DataType<short>
    // {
    // public:
    // typedef short       value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_16S,
    // channels     = 1,
    // fmt          = (int)'s',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    if TypeInfo(T) = TypeInfo(Int) then
      with Dest do
      begin
        generic_type := 0;
        depth := CV_32S;
        channels := 1;
        fmt := Int('i');
        &type := CV_MAKETYPE(depth, channels)
      end
    else if TypeInfo(T) = TypeInfo(float) then
      with Dest do
      begin
        generic_type := 0;
        depth := CV_32F;
        channels := 1;
        fmt := Int('f');
        &type := CV_MAKETYPE(depth, channels)
      end
    else if TypeInfo(T) = TypeInfo(double) then
      with Dest do
      begin
        generic_type := 0;
        depth := CV_64F;
        channels := 1;
        fmt := Int('d');
        &type := CV_MAKETYPE(depth, channels)
      end
    else
      // template<> class DataType<float16_t>
      // {
      // public:
      // typedef float16_t   value_type;
      // typedef float       work_type;
      // typedef value_type  channel_type;
      // typedef value_type  vec_type;
      // enum { generic_type = 0,
      // depth        = CV_16F,
      // channels     = 1,
      // fmt          = (int)'h',
      // type         = CV_MAKETYPE(depth, channels)
      // };
      // };
      Assert(false, 'Define more types');
end;

{ TDepth<T> }

class function TDepth<T>.Value: Int;
begin
  Result := TDataType<T>.depth;
end;

{ TTraits }
class function TTraits.&type<T>: TTraitsValue<T>;
begin
  if TypeInfo(T) = TypeInfo(TScalar) then
    Result.Value := CV_MAKETYPE(TDepth<double>.Value, 4)
  else if TypeInfo(T) = TypeInfo(TVec4i) then
    Result.Value := CV_MAKETYPE(TDepth<Int>.Value, 4)
  else if TypeInfo(T) = TypeInfo(TPoint) then
    Result.Value := CV_MAKETYPE(TDepth<Int>.Value, 2)
  else if TypeInfo(T) = TypeInfo(TPoint2f) then
    Result.Value := CV_MAKETYPE(TDepth<float>.Value, 2)
  else
    Result.Value := TDataType<T>.&type;
end;

class function TTraits.depth<T>: TTraitsValue<T>;
begin
  Result.Value := TDataType<T>.depth;
end;

{ TTraitsType<T> }

//class function TTraitsType<T>.Value: Int;
//begin
//  if TypeInfo(T) = TypeInfo(TScalar) then
//    Result := CV_MAKETYPE(TDepth<double>.Value, 4)
//  else if TypeInfo(T) = TypeInfo(TVec4i) then
//    Result := CV_MAKETYPE(TDepth<Int>.Value, 4)
//  else if TypeInfo(T) = TypeInfo(TPoint) then
//    Result := CV_MAKETYPE(TDepth<Int>.Value, 2)
//  else if TypeInfo(T) = TypeInfo(TPoint2f) then
//    Result := CV_MAKETYPE(TDepth<float>.Value, 2)
//  else
//  begin
//    Var
//      a: TDataType<T>;
//    Result := a.&type;
//  end;
//  (*
//    if TypeInfo(T) = TypeInfo(uchar) then
//    begin
//    Var
//    a: TDataType<uchar>;
//    Result := a.&type;
//    end
//    else if TypeInfo(T) = TypeInfo(float) then
//    begin
//    Var
//    a: TDataType<float>;
//    Result := a.&type;
//    end
//    else if TypeInfo(T) = TypeInfo(float) then
//    begin
//    Var
//    a: TDataType<float>;
//    Result := a.&type;
//    end
//    else
//    begin
//    Var
//    AssertMsg := 'TTraitsType - not defined type'
//    {$IFDEF USE_TYPEINFO}
//    + ' "' + GetTypeName(TypeInfo(T)) + '"'
//    {$ENDIF}
//    ;
//    Assert(false, AssertMsg);
//    end;
//  *)
//end;

{$ENDIF OPENCV_CORE_TRAITS_HPP}
