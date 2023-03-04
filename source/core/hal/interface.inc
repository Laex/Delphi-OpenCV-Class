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

{$IF not defined(OPENCV_CORE_HAL_INTERFACE_H)}
{$define OPENCV_CORE_HAL_INTERFACE_H}

//! @addtogroup core_hal_interface
//! @{

const
//! @name Return codes
//! @{
 CV_HAL_ERROR_OK =0;
 CV_HAL_ERROR_NOT_IMPLEMENTED =1;
 CV_HAL_ERROR_UNKNOWN =-1;
//! @}
(*
#ifdef __cplusplus
#include <cstddef>
#else
#include <stddef.h>
#include <stdbool.h>
#endif
*)
//! @name Data types
//! primitive types
//! - schar  - signed 1 byte integer
//! - uchar  - unsigned 1 byte integer
//! - short  - signed 2 byte integer
//! - ushort - unsigned 2 byte integer
//! - int    - signed 4 byte integer
//! - uint   - unsigned 4 byte integer
//! - int64  - signed 8 byte integer
//! - uint64 - unsigned 8 byte integer
//! @

Type
  BOOL          = ByteBool;
  size_t        = NativeUInt;
  psize_t       = ^size_t;
  Int           = integer;
  uint          = Cardinal;
  pInt          = ^Int;
  short 	= Int16;
  ushort	= UInt16;
  unsigned      = UInt32;
  float         = Single;
  pFloat        = ^float;
  ppAnsiChar    = ^pAnsiChar;
  uchar         = Byte;
  pUChar        = type pByte;
  pMatOp        = type Pointer;
  pUCharConst   = pUChar;
  PointerConst  = type Pointer;
  //
  vftable_func = type Pointer;
  pvftable     = ^vftable_func;

{
#if !defined _MSC_VER && !defined __BORLANDC__
#  if defined __cplusplus && __cplusplus >= 201103L && !defined __APPLE__
#    include <cstdint>
#    ifdef __NEWLIB__
        typedef unsigned int uint;
#    else
        typedef std::uint32_t uint;
#    endif
#  else
#    include <stdint.h>
     typedef uint32_t uint;
#  endif
#else
   typedef unsigned uint;
#endif

typedef signed char schar;


#ifndef __IPL_H__
   typedef unsigned char uchar;
   typedef unsigned short ushort;
#endif

#if defined _MSC_VER || defined __BORLANDC__
   typedef __int64 int64;
   typedef unsigned __int64 uint64;
#  define CV_BIG_INT(n)   n##I64
#  define CV_BIG_UINT(n)  n##UI64
#else
   typedef int64_t int64;
   typedef uint64_t uint64;
#  define CV_BIG_INT(n)   n##LL
#  define CV_BIG_UINT(n)  n##ULL
#endif

#define CV_USRTYPE1 (void)"CV_USRTYPE1 support has been dropped in OpenCV 4.0"
}
const
 CV_CN_MAX     =512;
 CV_CN_SHIFT   =3;
 CV_DEPTH_MAX  =(1 shl CV_CN_SHIFT);

 CV_MAT_DEPTH_MASK       =(CV_DEPTH_MAX - 1);
  CV_8U  = 0; // UInt8
  CV_8S  = 1; // Int8
  CV_16U = 2; // UInt16
  CV_16S = 3; // Int16
  CV_32S = 4; // Int32
  CV_32F = 5; // float(single)
  CV_64F = 6; // double
  CV_16F = 7; // unsigned short - type float16_t

function CV_MAT_DEPTH(flags: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}     // #define CV_MAT_DEPTH(flags)     ((flags) & CV_MAT_DEPTH_MASK)
function CV_MAKETYPE(depth, cn: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}  // #define CV_MAKETYPE(depth,cn) (CV_MAT_DEPTH(depth) + (((cn)-1) << CV_CN_SHIFT))
function CV_MAKE_TYPE(depth, cn: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // #define CV_MAKE_TYPE CV_MAKETYPE

Var
  CV_8UC1: Int;
  CV_8UC2: Int;
  CV_8UC3: Int;
  CV_8UC4: Int;
function CV_8UC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_8UC(n) CV_MAKETYPE(CV_8U,(n))

Var
  CV_8SC1: Int;
  CV_8SC2: Int;
  CV_8SC3: Int;
  CV_8SC4: Int;
function CV_8SC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_MAKETYPE(CV_8S,(n))

Var
  CV_16UC1: Int;
  CV_16UC2: Int;
  CV_16UC3: Int;
  CV_16UC4: Int;
function CV_16UC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_16U,(n))

Var
  CV_16SC1: Int;
  CV_16SC2: Int;
  CV_16SC3: Int;
  CV_16SC4: Int;
function CV_16SC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_16S,(n))

Var
  CV_32SC1: Int;
  CV_32SC2: Int;
  CV_32SC3: Int;
  CV_32SC4: Int;
function CV_32SC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_32S,(n))

Var
  CV_32FC1: Int;
  CV_32FC2: Int;
  CV_32FC3: Int;
  CV_32FC4: Int;
function CV_32FC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_32F,(n))

Var
  CV_64FC1: Int;
  CV_64FC2: Int;
  CV_64FC3: Int;
  CV_64FC4: Int;
function CV_64FC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_64F,(n))

Var
  CV_16FC1: Int;
  CV_16FC2: Int;
  CV_16FC3: Int;
  CV_16FC4: Int;
function CV_16FC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_16F,(n))


const
//! @name Comparison operation
//! @sa cv::CmpTypes
//! @{
 CV_HAL_CMP_EQ =0;
 CV_HAL_CMP_GT =1;
 CV_HAL_CMP_GE =2;
 CV_HAL_CMP_LT =3;
 CV_HAL_CMP_LE =4;
 CV_HAL_CMP_NE =5;
//! @}

//! @name Border processing modes
//! @sa cv::BorderTypes
//! @{
 CV_HAL_BORDER_CONSTANT =0;
 CV_HAL_BORDER_REPLICATE =1;
 CV_HAL_BORDER_REFLECT =2;
 CV_HAL_BORDER_WRAP =3;
 CV_HAL_BORDER_REFLECT_101 =4;
 CV_HAL_BORDER_TRANSPARENT =5;
 CV_HAL_BORDER_ISOLATED =16;
//! @}

//! @name DFT flags
//! @{
 CV_HAL_DFT_INVERSE        =1;
 CV_HAL_DFT_SCALE          =2;
 CV_HAL_DFT_ROWS           =4;
 CV_HAL_DFT_COMPLEX_OUTPUT =16;
 CV_HAL_DFT_REAL_OUTPUT    =32;
 CV_HAL_DFT_TWO_STAGE      =64;
 CV_HAL_DFT_STAGE_COLS    =128;
 CV_HAL_DFT_IS_CONTINUOUS =512;
 CV_HAL_DFT_IS_INPLACE =1024;
//! @}

//! @name SVD flags
//! @{
 CV_HAL_SVD_NO_UV    =1;
 CV_HAL_SVD_SHORT_UV =2;
 CV_HAL_SVD_MODIFY_A =4;
 CV_HAL_SVD_FULL_UV  =8;
//! @}

//! @name Gemm flags
//! @{
 CV_HAL_GEMM_1_T =1;
 CV_HAL_GEMM_2_T =2;
 CV_HAL_GEMM_3_T =4;
//! @}

//! @}
{$IFEND}// OPENCV_CORE_HAL_INTERFACE_H