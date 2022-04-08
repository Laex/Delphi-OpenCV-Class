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

{
  996
  ??0QRCodeDetector@cv@@QEAA@AEBV01@@Z
  ??0QRCodeDetector@cv@@QEAA@AEBV01@@Z
  cv::QRCodeDetector::QRCodeDetector(class cv::QRCodeDetector const &)
}
{
  997
  ??0QRCodeDetector@cv@@QEAA@XZ
  ??0QRCodeDetector@cv@@QEAA@XZ
  cv::QRCodeDetector::QRCodeDetector(void)
}
procedure Constructor_QRCodeDetector(Var obj: TQRCodeDetector); external opencv_world_dll name '??0QRCodeDetector@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
  1708
  ??1QRCodeDetector@cv@@QEAA@XZ
  ??1QRCodeDetector@cv@@QEAA@XZ
  cv::QRCodeDetector::~QRCodeDetector(void)
}
procedure Destructor_QRCodeDetector(Var obj: TQRCodeDetector); external opencv_world_dll name '??1QRCodeDetector@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
  4426
  ?detectAndDecode@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@62@1@Z
  ?detectAndDecode@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@2@AEBV_OutputArray@2@1@Z
  class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > cv::QRCodeDetector::detectAndDecode(
  class cv::_InputArray const &,
  class cv::_OutputArray const &,
  class cv::_OutputArray const &
  )
}
function detectAndDecode_QRCodeDetector(const obj: TQRCodeDetector; const img: TInputArray; const points: TOutputArray; const straight_qrcode: TOutputArray): CppString;
  external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
  name '?detectAndDecode@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@62@1@Z'
{$ELSE}
  name '?detectAndDecode@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@2@AEBV_OutputArray@2@1@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  4427
  ?detectAndDecodeCurved@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@62@1@Z
  ?detectAndDecodeCurved@QRCodeDetector@cv@@QEAA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@2@AEBV_OutputArray@2@1@Z
  class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > cv::QRCodeDetector::detectAndDecodeCurved(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::_OutputArray const &)
}

{
  4421
  ?detect@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z
  ?detect@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@2@AEBV_OutputArray@2@@Z
  bool cv::QRCodeDetector::detect(class cv::_InputArray const &,class cv::_OutputArray const &)
}
function detect_QRCodeDetector(const obj: TQRCodeDetector; const img: TInputArray; const points: TOutputArray): bool;
external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
  name '?detect@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z'
{$ELSE}
  name '?detect@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@2@AEBV_OutputArray@2@@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
4428
?detectAndDecodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@AEBV_OutputArray@42@2@Z
?detectAndDecodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@2@AEAV?$vector@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@AEBV_OutputArray@2@2@Z
bool cv::QRCodeDetector::detectAndDecodeMulti(class cv::_InputArray const &,class std::vector<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > > &,class cv::_OutputArray const &,class cv::_OutputArray const &)
}
function detectAndDecodeMulti_QRCodeDetector(const obj: TQRCodeDetector;
const img:TInputArray;const decoded_info:vector<cppstring>;
     const points :TOutputArray {= noArray()};
     const straight_qrcode :TOutputArrayOfArrays):bool;
external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
  name '?detectAndDecodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$basic_string@DU?$'+
       'char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@'+
       '2@@std@@AEBV_OutputArray@42@2@Z'
{$ELSE}
  name '?detectAndDecodeMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@2@AEAV?$vector@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$allocator@V?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@2@@std@@AEBV_OutputArray@2@2@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
4430
?detectMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z
?detectMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@2@AEBV_OutputArray@2@@Z
bool cv::QRCodeDetector::detectMulti(class cv::_InputArray const &,class cv::_OutputArray const &)
}
function detectMulti_QRCodeDetector(const obj: TQRCodeDetector; const img:TInputArray;const points:TOutputArray):Bool;
external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
  name '?detectMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@42@@Z'
{$ELSE}
  name '?detectMulti@QRCodeDetector@cv@@QEBA_NAEBV_InputArray@2@AEBV_OutputArray@2@@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
