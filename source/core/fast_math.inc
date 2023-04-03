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

{$if not defined(OPENCV_CORE_FAST_MATH_HPP)}
{$define OPENCV_CORE_FAST_MATH_HPP}

{$IF not defined(OPENCV_CORE_CVDEF_H)}{$I 'core/cvdef.inc'}{$ifend}

//! @addtogroup core_utils
//! @{

(****************************************************************************************\
*                                      fast math                                         *
\****************************************************************************************)

//#ifdef __cplusplus
//#  include <cmath>
//#else
//#  ifdef __BORLANDC__
//#    include <fastmath.h>
//#  else
//#    include <math.h>
//#  endif
//#endif

(*
#if defined(__CUDACC__)
  // nothing, intrinsics/asm code is not supported
#else
  #if ((defined _MSC_VER && defined _M_X64) \
      || (defined __GNUC__ && defined __x86_64__ && defined __SSE2__)) \
      && !defined(OPENCV_SKIP_INCLUDE_EMMINTRIN_H)
    #include <emmintrin.h>
  #endif

  #if defined __PPC64__ && defined __GNUC__ && defined _ARCH_PWR8 \
      && !defined(OPENCV_SKIP_INCLUDE_ALTIVEC_H)
    #include <altivec.h>
    #undef vector
    #undef bool
    #undef pixel
  #endif

  #if defined(CV_INLINE_ROUND_FLT)
    // user-specified version
    // CV_INLINE_ROUND_DBL should be defined too
  #elif defined __GNUC__ && defined __arm__ && (defined __ARM_PCS_VFP || defined __ARM_VFPV3__ || defined __ARM_NEON__) && !defined __SOFTFP__
    // 1. general scheme
    #define ARM_ROUND(_value, _asm_string) \
        int res; \
        float temp; \
        CV_UNUSED(temp); \
        __asm__(_asm_string : [res] "=r" (res), [temp] "=w" (temp) : [value] "w" (_value)); \
        return res
    // 2. version for double
    #ifdef __clang__
        #define CV_INLINE_ROUND_DBL(value) ARM_ROUND(value, "vcvtr.s32.f64 %[temp], %[value] \n vmov %[res], %[temp]")
    #else
        #define CV_INLINE_ROUND_DBL(value) ARM_ROUND(value, "vcvtr.s32.f64 %[temp], %P[value] \n vmov %[res], %[temp]")
    #endif
    // 3. version for float
    #define CV_INLINE_ROUND_FLT(value) ARM_ROUND(value, "vcvtr.s32.f32 %[temp], %[value]\n vmov %[res], %[temp]")
  #elif defined __PPC64__ && defined __GNUC__ && defined _ARCH_PWR8
    // P8 and newer machines can convert fp32/64 to int quickly.
    #define CV_INLINE_ROUND_DBL(value) \
        int out; \
        double temp; \
        __asm__( "fctiw %[temp],%[in]\n\tmfvsrwz %[out],%[temp]\n\t" : [out] "=r" (out), [temp] "=d" (temp) : [in] "d" ((double)(value)) : ); \
        return out;

    // FP32 also works with FP64 routine above
    #define CV_INLINE_ROUND_FLT(value) CV_INLINE_ROUND_DBL(value)
  #endif

  #ifdef CV_INLINE_ISINF_FLT
    // user-specified version
    // CV_INLINE_ISINF_DBL should be defined too
  #elif defined __PPC64__ && defined _ARCH_PWR9 && defined(scalar_test_data_class)
    #define CV_INLINE_ISINF_DBL(value) return scalar_test_data_class(value, 0x30);
    #define CV_INLINE_ISINF_FLT(value) CV_INLINE_ISINF_DBL(value)
  #endif

  #ifdef CV_INLINE_ISNAN_FLT
    // user-specified version
    // CV_INLINE_ISNAN_DBL should be defined too
  #elif defined __PPC64__ && defined _ARCH_PWR9 && defined(scalar_test_data_class)
    #define CV_INLINE_ISNAN_DBL(value) return scalar_test_data_class(value, 0x40);
    #define CV_INLINE_ISNAN_FLT(value) CV_INLINE_ISNAN_DBL(value)
  #endif

  #if !defined(OPENCV_USE_FASTMATH_BUILTINS) \
    && ( \
        defined(__x86_64__) || defined(__i686__) \
        || defined(__arm__) \
        || defined(__PPC64__) \
    )
    /* Let builtin C math functions when available. Dedicated hardware is available to
       round and convert FP values. */
    #define OPENCV_USE_FASTMATH_BUILTINS 1
  #endif

  /* Enable builtin math functions if possible, desired, and available.
     Note, not all math functions inline equally. E.g lrint will not inline
     without the -fno-math-errno option. */
  #if defined(CV_ICC)
    // nothing
  #elif defined(OPENCV_USE_FASTMATH_BUILTINS) && OPENCV_USE_FASTMATH_BUILTINS
    #if defined(__clang__)
      #define CV__FASTMATH_ENABLE_CLANG_MATH_BUILTINS
      #if !defined(CV_INLINE_ISNAN_DBL) && __has_builtin(__builtin_isnan)
        #define CV_INLINE_ISNAN_DBL(value) return __builtin_isnan(value);
      #endif
      #if !defined(CV_INLINE_ISNAN_FLT) && __has_builtin(__builtin_isnan)
        #define CV_INLINE_ISNAN_FLT(value) return __builtin_isnan(value);
      #endif
      #if !defined(CV_INLINE_ISINF_DBL) && __has_builtin(__builtin_isinf)
        #define CV_INLINE_ISINF_DBL(value) return __builtin_isinf(value);
      #endif
      #if !defined(CV_INLINE_ISINF_FLT) && __has_builtin(__builtin_isinf)
        #define CV_INLINE_ISINF_FLT(value) return __builtin_isinf(value);
      #endif
    #elif defined(__GNUC__)
      #define CV__FASTMATH_ENABLE_GCC_MATH_BUILTINS
      #if !defined(CV_INLINE_ISNAN_DBL)
        #define CV_INLINE_ISNAN_DBL(value) return __builtin_isnan(value);
      #endif
      #if !defined(CV_INLINE_ISNAN_FLT)
        #define CV_INLINE_ISNAN_FLT(value) return __builtin_isnanf(value);
      #endif
      #if !defined(CV_INLINE_ISINF_DBL)
        #define CV_INLINE_ISINF_DBL(value) return __builtin_isinf(value);
      #endif
      #if !defined(CV_INLINE_ISINF_FLT)
        #define CV_INLINE_ISINF_FLT(value) return __builtin_isinff(value);
      #endif
    #elif defined(_MSC_VER)
      #if !defined(CV_INLINE_ISNAN_DBL)
        #define CV_INLINE_ISNAN_DBL(value) return isnan(value);
      #endif
      #if !defined(CV_INLINE_ISNAN_FLT)
        #define CV_INLINE_ISNAN_FLT(value) return isnan(value);
      #endif
      #if !defined(CV_INLINE_ISINF_DBL)
        #define CV_INLINE_ISINF_DBL(value) return isinf(value);
      #endif
      #if !defined(CV_INLINE_ISINF_FLT)
        #define CV_INLINE_ISINF_FLT(value) return isinf(value);
      #endif
    #endif
  #endif

#endif // defined(__CUDACC__)
*)
(** @brief Rounds floating-point number to the nearest integer

 @param value floating-point number. If the value is outside of INT_MIN ... INT_MAX range, the
 result is not defined.
 *)
(*
CV_INLINE int cvRound( double value )
{
#if defined CV_INLINE_ROUND_DBL
    CV_INLINE_ROUND_DBL(value);
#elif ((defined _MSC_VER && defined _M_X64) || (defined __GNUC__ && defined __x86_64__ \
    && defined __SSE2__ && !defined __APPLE__) || CV_SSE2) \
    && !defined(__CUDACC__)
    __m128d t = _mm_set_sd( value );
    return _mm_cvtsd_si32(t);
#elif defined _MSC_VER && defined _M_IX86
    int t;
    __asm
    {
        fld value;
        fistp t;
    }
    return t;
#elif defined CV_ICC || defined __GNUC__
    return (int)(lrint(value));
#else
    /* it's ok if round does not comply with IEEE754 standard;
       the tests should allow +/-1 difference when the tested functions use round */
    return (int)(value + (value >= 0 ? 0.5 : -0.5));
#endif
}
*)
function cvRound(value:double ):int; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Rounds floating-point number to the nearest integer not larger than the original.

 The function computes an integer i such that:
 \f[i \le \texttt{value} < i+1\f]
 @param value floating-point number. If the value is outside of INT_MIN ... INT_MAX range, the
 result is not defined.
 *)
(*
CV_INLINE int cvFloor( double value )
{
#if (defined CV__FASTMATH_ENABLE_GCC_MATH_BUILTINS || defined CV__FASTMATH_ENABLE_CLANG_MATH_BUILTINS) \
    && ( \
        defined(__PPC64__) \
    )
    return __builtin_floor(value);
#else
    int i = (int)value;
    return i - (i > value);
#endif
}
*)
function cvFloor(value:double ):int; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Rounds floating-point number to the nearest integer not smaller than the original.

 The function computes an integer i such that:
 \f[i \le \texttt{value} < i+1\f]
 @param value floating-point number. If the value is outside of INT_MIN ... INT_MAX range, the
 result is not defined.
 *)
(*
CV_INLINE int cvCeil( double value )
{
#if (defined CV__FASTMATH_ENABLE_GCC_MATH_BUILTINS || defined CV__FASTMATH_ENABLE_CLANG_MATH_BUILTINS) \
    && ( \
        defined(__PPC64__) \
    )
    return __builtin_ceil(value);
#else
    int i = (int)value;
    return i + (i < value);
#endif
}
*)
function cvCeil(value:double ):int; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Determines if the argument is Not A Number.

 @param value The input floating-point value

 The function returns 1 if the argument is Not A Number (as defined by IEEE754 standard), 0
 otherwise. *)
(*
CV_INLINE int cvIsNaN( double value )
{
#if defined CV_INLINE_ISNAN_DBL
    CV_INLINE_ISNAN_DBL(value);
#else
    Cv64suf ieee754;
    ieee754.f = value;
    return ((unsigned)(ieee754.u >> 32) & 0x7fffffff) +
           ((unsigned)ieee754.u != 0) > 0x7ff00000;
#endif
}
*)
function cvIsNaN(value:double ):bool; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Determines if the argument is Infinity.

 @param value The input floating-point value

 The function returns 1 if the argument is a plus or minus infinity (as defined by IEEE754 standard)
 and 0 otherwise. *)
(*
CV_INLINE int cvIsInf( double value )
{
#if defined CV_INLINE_ISINF_DBL
    CV_INLINE_ISINF_DBL(value);
#elif defined(__x86_64__) || defined(_M_X64) || defined(__aarch64__) || defined(_M_ARM64) || defined(__PPC64__)
    Cv64suf ieee754;
    ieee754.f = value;
    return (ieee754.u & 0x7fffffff00000000) ==
                        0x7ff0000000000000;
#else
    Cv64suf ieee754;
    ieee754.f = value;
    return ((unsigned)(ieee754.u >> 32) & 0x7fffffff) == 0x7ff00000 &&
            (unsigned)ieee754.u == 0;
#endif
}
*)
function cvIsInf(value:double ):bool; {$IFDEF USE_INLINE}inline; {$ENDIF}
(*
#ifdef __cplusplus

/** @overload */
CV_INLINE int cvRound(float value)
{
#if defined CV_INLINE_ROUND_FLT
    CV_INLINE_ROUND_FLT(value);
#elif ((defined _MSC_VER && defined _M_X64) || (defined __GNUC__ && defined __x86_64__ \
    && defined __SSE2__ && !defined __APPLE__) || CV_SSE2) \
    && !defined(__CUDACC__)
    __m128 t = _mm_set_ss( value );
    return _mm_cvtss_si32(t);
#elif defined _MSC_VER && defined _M_IX86
    int t;
    __asm
    {
        fld value;
        fistp t;
    }
    return t;
#elif defined CV_ICC || defined __GNUC__
    return (int)(lrintf(value));
#else
    /* it's ok if round does not comply with IEEE754 standard;
     the tests should allow +/-1 difference when the tested functions use round */
    return (int)(value + (value >= 0 ? 0.5f : -0.5f));
#endif
}

/** @overload */
CV_INLINE int cvRound( int value )
{
    return value;
}

/** @overload */
CV_INLINE int cvFloor( float value )
{
#if (defined CV__FASTMATH_ENABLE_GCC_MATH_BUILTINS || defined CV__FASTMATH_ENABLE_CLANG_MATH_BUILTINS) \
    && ( \
        defined(__PPC64__) \
    )
    return __builtin_floorf(value);
#else
    int i = (int)value;
    return i - (i > value);
#endif
}

/** @overload */
CV_INLINE int cvFloor( int value )
{
    return value;
}

/** @overload */
CV_INLINE int cvCeil( float value )
{
#if (defined CV__FASTMATH_ENABLE_GCC_MATH_BUILTINS || defined CV__FASTMATH_ENABLE_CLANG_MATH_BUILTINS) \
    && ( \
        defined(__PPC64__) \
    )
    return __builtin_ceilf(value);
#else
    int i = (int)value;
    return i + (i < value);
#endif
}

/** @overload */
CV_INLINE int cvCeil( int value )
{
    return value;
}

/** @overload */
CV_INLINE int cvIsNaN( float value )
{
#if defined CV_INLINE_ISNAN_FLT
    CV_INLINE_ISNAN_FLT(value);
#else
    Cv32suf ieee754;
    ieee754.f = value;
    return (ieee754.u & 0x7fffffff) > 0x7f800000;
#endif
}

/** @overload */
CV_INLINE int cvIsInf( float value )
{
#if defined CV_INLINE_ISINF_FLT
    CV_INLINE_ISINF_FLT(value);
#else
    Cv32suf ieee754;
    ieee754.f = value;
    return (ieee754.u & 0x7fffffff) == 0x7f800000;
#endif
}

#endif // __cplusplus
*)
//! @} core_utils

{$ELSE}
{$MESSAGE FATAL 'Duplicate included file - core/fast_math.inc'}

{$ifend}
