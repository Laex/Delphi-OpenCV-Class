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

{$IF not defined(OPENCV_CORE_TRAITS_HPP)}
{$DEFINE OPENCV_CORE_TRAITS_HPP}

{$IF not defined(OPENCV_CORE_CVDEF_H)}{$I 'core/cvdef.inc'}{$ifend}

// namespace cv
// {

// #define OPENCV_TRAITS_ENABLE_DEPRECATED

// ! @addtogroup core_basic
// ! @{
Type
  (* * @brief Template 'trait' class for OpenCV primitive data types.

    @note Deprecated. This is replaced by 'single purpose' traits: traits::Type and traits::Depth

    A primitive OpenCV data type is one of unsigned char, bool, signed char, unsigned short, signed
    short, int, float, double, or a tuple of values of one of these types, where all the values in the
    tuple have the same type. Any primitive type from the list can be defined by an identifier in the
    form CV_\<bit-depth\>{U|S|F}C(\<number_of_channels\>), for example: uchar \~ CV_8UC1, 3-element
    floating-point tuple \~ CV_32FC3, and so on. A universal OpenCV structure that is able to store a
    single instance of such a primitive data type is Vec. Multiple instances of such a type can be
    stored in a std::vector, Mat, Mat_, SparseMat, SparseMat_, or any other container that is able to
    store Vec instances.

    The DataType class is basically used to provide a description of such primitive data types without
    adding any fields or methods to the corresponding classes (and it is actually impossible to add
    anything to primitive C/C++ data types). This technique is known in C++ as class traits. It is not
    DataType itself that is used but its specialized versions, such as:
    @code
    template<> class DataType<uchar>
    {
    typedef uchar value_type;
    typedef int work_type;
    typedef uchar channel_type;
    enum { channel_type = CV_8U, channels = 1, fmt='u', type = CV_8U };
    };
    ...
    template<typename _Tp> DataType<std::complex<_Tp> >
    {
    typedef std::complex<_Tp> value_type;
    typedef std::complex<_Tp> work_type;
    typedef _Tp channel_type;
    // DataDepth is another helper trait class
    enum { depth = DataDepth<_Tp>::value, channels=2,
    fmt=(channels-1)*256+DataDepth<_Tp>::fmt,
    type=CV_MAKETYPE(depth, channels) };
    };
    ...
    @endcode
    The main purpose of this class is to convert compilation-time type information to an
    OpenCV-compatible data type identifier, for example:
    @code
    // allocates a 30x40 floating-point matrix
    Mat A(30, 40, DataType<float>::type);

    Mat B = Mat_<std::complex<double> >(3, 3);
    // the statement below will print 6, 2 , that is depth == CV_64F, channels == 2
    cout << B.depth() << ', ' << B.channels() << endl;
    @endcode
    So, such traits are used to tell OpenCV which data type you are working with, even if such a type is
    not native to OpenCV. For example, the matrix B initialization above is compiled because OpenCV
    defines the proper specialized template class DataType\<complex\<_Tp\> \> . This mechanism is also
    useful (and used in OpenCV this way) for generic algorithms implementations.

    @note Default values were dropped to stop confusing developers about using of unsupported types (see #7599)
  *)
  TDataType<T> = record
    class function generic_type: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF} // = 1,
    class function depth: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}   // = -1,
    class function channels: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}   // = 1,
    class function fmt: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}   // = 0,
    class function &type: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}    // = CV_MAKETYPE(depth, channels)
  end;

  TDataType_<T> = record
  public
    class operator Initialize(out Dest: TDataType_<T>);
  public
    generic_type: Int;
    depth: Int;
    channels: Int;
    fmt: Int;
    &type: Int;
  end;

  TDepth<T> = record // Need refactoring as TTraitsType<T>
    class function Value: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TTraitsValue<T> = record
    Value: Int;
  end;

  TTraits = record
    class function &type<T>: TTraitsValue<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class function depth<T>: TTraitsValue<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  // TTraitsType<T> = record
  // class function Value: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  // end;

  (* * @brief A helper class for cv::DataType

    The class is specialized for each fundamental numerical data type supported by OpenCV. It provides
    DataDepth<T>::value constant.
  *)
  (*
    template<typename _Tp> class DataDepth
    {
    public:
    enum
    {
    value = DataType<_Tp>::depth,
    fmt   = DataType<_Tp>::fmt
    };
    };


    #ifdef OPENCV_TRAITS_ENABLE_DEPRECATED

    template<int _depth> class TypeDepth
    {
    #ifdef OPENCV_TRAITS_ENABLE_LEGACY_DEFAULTS
    enum { depth = CV_USRTYPE1 };
    typedef void value_type;
    #endif
    };

    template<> class TypeDepth<CV_8U>
    {
    enum { depth = CV_8U };
    typedef uchar value_type;
    };

    template<> class TypeDepth<CV_8S>
    {
    enum { depth = CV_8S };
    typedef schar value_type;
    };

    template<> class TypeDepth<CV_16U>
    {
    enum { depth = CV_16U };
    typedef ushort value_type;
    };

    template<> class TypeDepth<CV_16S>
    {
    enum { depth = CV_16S };
    typedef short value_type;
    };

    template<> class TypeDepth<CV_32S>
    {
    enum { depth = CV_32S };
    typedef int value_type;
    };

    template<> class TypeDepth<CV_32F>
    {
    enum { depth = CV_32F };
    typedef float value_type;
    };

    template<> class TypeDepth<CV_64F>
    {
    enum { depth = CV_64F };
    typedef double value_type;
    };

    template<> class TypeDepth<CV_16F>
    {
    enum { depth = CV_16F };
    typedef float16_t value_type;
    };

    #endif

    //! @}
  *)
  (*
    namespace traits {

    namespace internal {
    #define CV_CREATE_MEMBER_CHECK(X) \
    template<typename T> class CheckMember_##X { \
    struct Fallback { int X; }; \
    struct Derived : T, Fallback { }; \
    template<typename U, U> struct Check; \
    typedef char CV_NO[1]; \
    typedef char CV_YES[2]; \
    template<typename U> static CV_NO & func(Check<int Fallback::*, &U::X> * ); \
    template<typename U> static CV_YES & func(...); \
    public: \
    typedef CheckMember_##X type; \
    enum { value = sizeof(func<Derived>(0)) == sizeof(CV_YES) }; \
    };

    CV_CREATE_MEMBER_CHECK(fmt)
    CV_CREATE_MEMBER_CHECK(type)

    } // namespace internal


    template<typename T>
    struct Depth
    { enum { value = DataType<T>::depth }; };

    template<typename T>
    struct Type
    { enum { value = DataType<T>::type }; };

    /** Similar to traits::Type<T> but has value = -1 in case of unknown type (instead of compiler error) */
    template<typename T, bool available = internal::CheckMember_type< DataType<T> >::value >
    struct SafeType {};

    template<typename T>
    struct SafeType<T, false>
    { enum { value = -1 }; };

    template<typename T>
    struct SafeType<T, true>
    { enum { value = Type<T>::value }; };


    template<typename T, bool available = internal::CheckMember_fmt< DataType<T> >::value >
    struct SafeFmt {};

    template<typename T>
    struct SafeFmt<T, false>
    { enum { fmt = 0 }; };

    template<typename T>
    struct SafeFmt<T, true>
    { enum { fmt = DataType<T>::fmt }; };


    } // namespace

    } // cv
  *)
{$IFEND OPENCV_CORE_TRAITS_HPP}
