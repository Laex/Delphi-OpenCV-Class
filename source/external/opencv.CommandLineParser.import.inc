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

// ??0CommandLineParser@cv@@QEAA@AEBV01@@Z	public: __cdecl cv::CommandLineParser::CommandLineParser(class cv::CommandLineParser const & __ptr64) __ptr64
// ??0CommandLineParser@cv@@QEAA@HQEBQEBDAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	public: __cdecl cv::CommandLineParser::CommandLineParser(int,char const * __ptr64 const * __ptr64 const,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64) __ptr64
procedure Constructor_CommandLineParser(Obj: pCommandLineParser; argc: int; const argv: ppAnsiChar; const keys: pCvStdString); overload;
  external opencv_world_dll name '??0CommandLineParser@cv@@QEAA@HQEBQEBDAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??1CommandLineParser@cv@@QEAA@XZ	public: __cdecl cv::CommandLineParser::~CommandLineParser(void) __ptr64
procedure Destructor_CommandLineParser(Obj: pCommandLineParser); external opencv_world_dll name '??1CommandLineParser@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??4CommandLineParser@cv@@QEAAAEAV01@AEBV01@@Z	public: class cv::CommandLineParser & __ptr64 __cdecl cv::CommandLineParser::operator=(class cv::CommandLineParser const & __ptr64) __ptr64
function operator_CommandLineParser_Assign_CommandLineParser(Obj: pCommandLineParser; c: pCommandLineParser): pCommandLineParser;
  external opencv_world_dll name '??4CommandLineParser@cv@@QEAAAEAV01@AEBV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?about@CommandLineParser@cv@@QEAAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	public: void __cdecl cv::CommandLineParser::about(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64) __ptr64
procedure About_CommandLineParser(Obj: pCommandLineParser; m: pCvStdString);
  external opencv_world_dll name '?about@CommandLineParser@cv@@QEAAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?check@CommandLineParser@cv@@QEBA_NXZ	public: bool __cdecl cv::CommandLineParser::check(void)const __ptr64
function check_CommandLineParser(Obj: pCommandLineParser): bool; external opencv_world_dll name '?check@CommandLineParser@cv@@QEBA_NXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?getByIndex@CommandLineParser@cv@@IEBAXH_NW4Param@2@PEAX@Z	protected: void __cdecl cv::CommandLineParser::getByIndex(int,bool,enum cv::Param,void * __ptr64)const __ptr64
// ?getByName@CommandLineParser@cv@@IEBAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@_NW4Param@2@PEAX@Z	protected: void __cdecl cv::CommandLineParser::getByName(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64,bool,enum cv::Param,void * __ptr64)const __ptr64
procedure getByName_CommandLineParser(Obj: pCommandLineParser; const name: pCvStdString; space_delete: bool; &type: int; dst: Pointer);
  external opencv_world_dll name '?getByName@CommandLineParser@cv@@IEBAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@_NW4Param@2@PEAX@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?getPathToApplication@CommandLineParser@cv@@QEBA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@XZ	public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > __cdecl cv::CommandLineParser::getPathToApplication(void)const __ptr64
// ?has@CommandLineParser@cv@@QEBA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	public: bool __cdecl cv::CommandLineParser::has(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64)const __ptr64
function Has_CommandLineParser(Obj: pCommandLineParser; const name: pCvStdString): bool;
  external opencv_world_dll name '?has@CommandLineParser@cv@@QEBA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?printErrors@CommandLineParser@cv@@QEBAXXZ	public: void __cdecl cv::CommandLineParser::printErrors(void)const __ptr64
procedure printErrors_CommandLineParser(Obj: pCommandLineParser); external opencv_world_dll name '?printErrors@CommandLineParser@cv@@QEBAXXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?printMessage@CommandLineParser@cv@@QEBAXXZ	public: void __cdecl cv::CommandLineParser::printMessage(void)const __ptr64
procedure printMessage_CommandLineParser(Obj: pCommandLineParser); external opencv_world_dll name '?printMessage@CommandLineParser@cv@@QEBAXXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
