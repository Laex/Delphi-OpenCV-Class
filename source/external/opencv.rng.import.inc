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

// ??0RNG@cv@@QEAA@XZ	public: __cdecl cv::RNG::RNG(void) __ptr64
procedure constructor_RNG(const Obj: TRNG); overload; external opencv_world_dll name '??0RNG@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0RNG@cv@@QEAA@_K@Z	public: __cdecl cv::RNG::RNG(unsigned __int64) __ptr64
procedure constructor_RNG(const Obj: TRNG; u: UInt64); overload; external opencv_world_dll name '??0RNG@cv@@QEAA@_K@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??4RNG@cv@@QEAAAEAV01@$$QEAV01@@Z	public: class cv::RNG & __ptr64 __cdecl cv::RNG::operator=(class cv::RNG && __ptr64) __ptr64
// ??4RNG@cv@@QEAAAEAV01@AEBV01@@Z	public: class cv::RNG & __ptr64 __cdecl cv::RNG::operator=(class cv::RNG const & __ptr64) __ptr64
// ??8RNG@cv@@QEBA_NAEBV01@@Z	public: bool __cdecl cv::RNG::operator==(class cv::RNG const & __ptr64)const __ptr64
// ??BRNG@cv@@QEAACXZ	public: __cdecl cv::RNG::operator signed char(void) __ptr64
// ??BRNG@cv@@QEAAEXZ	public: __cdecl cv::RNG::operator unsigned char(void) __ptr64
// ??BRNG@cv@@QEAAFXZ	public: __cdecl cv::RNG::operator short(void) __ptr64
// ??BRNG@cv@@QEAAGXZ	public: __cdecl cv::RNG::operator unsigned short(void) __ptr64
// ??BRNG@cv@@QEAAHXZ	public: __cdecl cv::RNG::operator int(void) __ptr64
// ??BRNG@cv@@QEAAIXZ	public: __cdecl cv::RNG::operator unsigned int(void) __ptr64
function operator_RNG_ToUnsignedInt(const Obj: TRNG): Unsigned; overload; external opencv_world_dll name '??BRNG@cv@@QEAAIXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??BRNG@cv@@QEAAMXZ	public: __cdecl cv::RNG::operator float(void) __ptr64
// ??BRNG@cv@@QEAANXZ	public: __cdecl cv::RNG::operator double(void) __ptr64
// ??RRNG@cv@@QEAAII@Z	public: unsigned int __cdecl cv::RNG::operator()(unsigned int) __ptr64
// ??RRNG@cv@@QEAAIXZ	public: unsigned int __cdecl cv::RNG::operator()(void) __ptr64
// ?fill@RNG@cv@@QEAAXAEBV_InputOutputArray@2@HAEBV_InputArray@2@1_N@Z	public: void __cdecl cv::RNG::fill(class cv::_InputOutputArray const & __ptr64,int,class cv::_InputArray const & __ptr64,class cv::_InputArray const & __ptr64,bool) __ptr64
// ?gaussian@RNG@cv@@QEAANN@Z	public: double __cdecl cv::RNG::gaussian(double) __ptr64
// ?next@RNG@cv@@QEAAIXZ	public: unsigned int __cdecl cv::RNG::next(void) __ptr64
// ?randShuffle@cv@@YAXAEBV_InputOutputArray@1@NPEAVRNG@1@@Z	void __cdecl cv::randShuffle(class cv::_InputOutputArray const & __ptr64,double,class cv::RNG * __ptr64)
// ?theRNG@cv@@YAAEAVRNG@1@XZ	class cv::RNG & __ptr64 __cdecl cv::theRNG(void)
// ?uniform@RNG@cv@@QEAAHHH@Z	public: int __cdecl cv::RNG::uniform(int,int) __ptr64
function uniform_RNG(const Obj: TRNG; a, b: Int): Int; external opencv_world_dll name '?uniform@RNG@cv@@QEAAHHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?uniform@RNG@cv@@QEAAMMM@Z	public: float __cdecl cv::RNG::uniform(float,float) __ptr64
// ?uniform@RNG@cv@@QEAANNN@Z	public: double __cdecl cv::RNG::uniform(double,double) __ptr64
