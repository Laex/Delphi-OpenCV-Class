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

// ??0QRCodeDetector@cv@@QEAA@AEBV01@@Z	public: __cdecl cv::QRCodeDetector::QRCodeDetector(class cv::QRCodeDetector const & __ptr64) __ptr64
// ??0QRCodeDetector@cv@@QEAA@XZ	public: __cdecl cv::QRCodeDetector::QRCodeDetector(void) __ptr64
procedure Constructor_QRCodeDetector(obj: pQRCodeDetector); external opencv_world_dll name '??0QRCodeDetector@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??1QRCodeDetector@cv@@QEAA@XZ	public: __cdecl cv::QRCodeDetector::~QRCodeDetector(void) __ptr64
procedure Destructor_QRCodeDetector(obj: pQRCodeDetector); external opencv_world_dll name '??1QRCodeDetector@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??4QRCodeDetector@cv@@QEAAAEAV01@AEBV01@@Z	public: class cv::QRCodeDetector & __ptr64 __cdecl cv::QRCodeDetector::operator=(class cv::QRCodeDetector const & __ptr64) __ptr64
// ?decode@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@0AEBV_OutputArray@62@@Z	public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > __cdecl cv::QRCodeDetector::decode(class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64) __ptr64
// ?decodeCurved@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@0AEBV_OutputArray@62@@Z	public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > __cdecl cv::QRCodeDetector::decodeCurved(class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64) __ptr64
// ?decodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@0AEAV?$vector@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@AEBV_OutputArray@42@@Z	public: bool __cdecl cv::QRCodeDetector::decodeMulti(class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_InputArray const & __ptr64,class std::vector<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > > & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64)const __ptr64
function decodeMulti_QRCodeDetector(obj: pQRCodeDetector; img: pInputArray; points: pInputArray; const decoded_info: pstdvectorcppstring; straight_qrcode: pOutputArrayOfArrays): bool;
  external opencv_world_dll name '?decodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@0AEAV?$vector@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?'
  + '$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@AEBV_OutputArray@42@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?detect@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z	public: bool __cdecl cv::QRCodeDetector::detect(class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64)const __ptr64
function detect_QRCodeDetector(obj: pQRCodeDetector; img: pInputArray; points: pOutputArray): bool;
  external opencv_world_dll name '?detect@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?detectAndDecode@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@62@1@Z	public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > __cdecl cv::QRCodeDetector::detectAndDecode(class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64) __ptr64
function detectAndDecode_QRCodeDetector(obj: pQRCodeDetector; img: pInputArray; points: pOutputArray; straight_qrcode: pOutputArray): CppString;
  external opencv_world_dll name '??detectAndDecode@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@62@1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?detectAndDecodeCurved@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@62@1@Z	public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > __cdecl cv::QRCodeDetector::detectAndDecodeCurved(class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64) __ptr64
// ?detectAndDecodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@AEBV_OutputArray@42@2@Z	public: bool __cdecl cv::QRCodeDetector::detectAndDecodeMulti(class cv::debug_build_guard::_InputArray const & __ptr64,class std::vector<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > > & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64)const __ptr64
function DetectAndDecodeMulti_QRCodeDetector(obj: pQRCodeDetector; img: pInputArray; decoded_info: pstdvectorcppstring; points: pOutputArray; straight_qrcode: pOutputArrayOfArrays): bool;
  external opencv_world_dll name '?detectAndDecodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$basic_string@DU' +
  '?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@AEBV_OutputArray@42@2@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?detectMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z	public: bool __cdecl cv::QRCodeDetector::detectMulti(class cv::debug_build_guard::_InputArray const & __ptr64,class cv::debug_build_guard::_OutputArray const & __ptr64)const __ptr64
function detectMulti_QRCodeDetector(obj: pQRCodeDetector; img: pInputArray; points: pOutputArray): bool;
  external opencv_world_dll name '?detectMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?setEpsX@QRCodeDetector@cv@@QEAAXN@Z	public: void __cdecl cv::QRCodeDetector::setEpsX(double) __ptr64
// ?setEpsY@QRCodeDetector@cv@@QEAAXN@Z	public: void __cdecl cv::QRCodeDetector::setEpsY(double) __ptr64
