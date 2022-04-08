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

// ??0VideoWriter@cv@@QEAA@AEBV01@@Z	??0VideoWriter@cv@@QEAA@AEBV01@@Z	cv::VideoWriter::VideoWriter(class cv::VideoWriter const &)
// ??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNAEBV?$Size_@H@1@AEBV?$vector@HV?$allocator@H@std@@@3@@Z	??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNAEBV?$Size_@H@1@AEBV?$vector@HV?$allocator@H@std@@@3@@Z	cv::VideoWriter::VideoWriter(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,int,double,class cv::Size_<int> const &,class std::vector<int,class std::allocator<int> > const &)
// ??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNV?$Size_@H@1@_N@Z	??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNV?$Size_@H@1@_N@Z	cv::VideoWriter::VideoWriter(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,int,double,class cv::Size_<int>,bool)
// ??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNAEBV?$Size_@H@1@AEBV?$vector@HV?$allocator@H@std@@@3@@Z	??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNAEBV?$Size_@H@1@AEBV?$vector@HV?$allocator@H@std@@@3@@Z	cv::VideoWriter::VideoWriter(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,double,class cv::Size_<int> const &,class std::vector<int,class std::allocator<int> > const &)
// ??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNV?$Size_@H@1@_N@Z	??0VideoWriter@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNV?$Size_@H@1@_N@Z	cv::VideoWriter::VideoWriter(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,double,class cv::Size_<int>,bool)
{
  ??0VideoWriter@cv@@QEAA@XZ
  ??0VideoWriter@cv@@QEAA@XZ
  cv::VideoWriter::VideoWriter(void)
}
procedure constructor_VideoWriter(const Obj: TVideoWriter); external opencv_world_dll name '??0VideoWriter@cv@@QEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  ??1VideoWriter@cv@@UEAA@XZ
  ??1VideoWriter@cv@@UEAA@XZ
  cv::VideoWriter::~VideoWriter(void)
}
procedure destructor_VideoWriter(const Obj: TVideoWriter); external opencv_world_dll name '??1VideoWriter@cv@@UEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??4VideoWriter@cv@@QEAAAEAV01@AEBV01@@Z	??4VideoWriter@cv@@QEAAAEAV01@AEBV01@@Z	class cv::VideoWriter & cv::VideoWriter::operator=(class cv::VideoWriter const &)
// ??6VideoWriter@cv@@UEAAAEAV01@AEBVMat@1@@Z	??6VideoWriter@cv@@UEAAAEAV01@AEBVMat@1@@Z	class cv::VideoWriter & cv::VideoWriter::operator<<(class cv::Mat const &)
// ??6VideoWriter@cv@@UEAAAEAV01@AEBVUMat@1@@Z	??6VideoWriter@cv@@UEAAAEAV01@AEBVUMat@1@@Z	class cv::VideoWriter & cv::VideoWriter::operator<<(class cv::UMat const &)
// ??_7VideoWriter@cv@@6B@	??_7VideoWriter@cv@@6B@	const cv::VideoWriter::`vftable'
// ?fourcc@VideoWriter@cv@@SAHDDDD@Z	?fourcc@VideoWriter@cv@@SAHDDDD@Z	int cv::VideoWriter::fourcc(char,char,char,char)
{
  ?get@VideoWriter@cv@@UEBANH@Z
  ?get@VideoWriter@cv@@UEBANH@Z
  double cv::VideoWriter::get(int)
}
function get_VideoWriter(const Obj: TVideoWriter; propId: Int): double; external opencv_world_dll name '?get@VideoWriter@cv@@UEBANH@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?getBackendName@VideoWriter@cv@@QEBA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@XZ	?getBackendName@VideoWriter@cv@@QEBA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@XZ	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > cv::VideoWriter::getBackendName(void)
{
  ?isOpened@VideoWriter@cv@@UEBA_NXZ
  ?isOpened@VideoWriter@cv@@UEBA_NXZ
  bool cv::VideoWriter::isOpened(void)
}
function isOpened_VideoWriter(const Obj: TVideoWriter): Bool; external opencv_world_dll name '?isOpened@VideoWriter@cv@@UEBA_NXZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  ?open@VideoWriter@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNAEBV?$Size_@H@2@AEBV?$vector@HV?$allocator@H@std@@@4@@Z
  ?open@VideoWriter@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNAEBV?$Size_@H@2@AEBV?$vector@HV?$allocator@H@std@@@4@@Z
  bool cv::VideoWriter::open(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,int,double,class cv::Size_<int> const &,class std::vector<int,class std::allocator<int> > const &)
}
{
  ?open@VideoWriter@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNV?$Size_@H@2@_N@Z
  ?open@VideoWriter@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNV?$Size_@H@2@_N@Z
  bool cv::VideoWriter::open(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,int,double,class cv::Size_<int>,bool)
  CV_WRAP BOOL open(const String & filename, Int apiPreference, Int fourcc, double fps, size frameSize, BOOL isColor = true);
}
function open_VideoWriter(const Obj: TVideoWriter; const filename: CppString; apiPreference: Int; fourcc: Int;
fps: double; frameSize: UInt64 { TSize }; isColor: Bool = true): Bool; overload;
  external opencv_world_dll name '?open@VideoWriter@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HHNV?$Size_@H@2@_N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  ?open@VideoWriter@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNAEBV?$Size_@H@2@AEBV?$vector@HV?$allocator@H@std@@@4@@Z
  ?open@VideoWriter@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNAEBV?$Size_@H@2@AEBV?$vector@HV?$allocator@H@std@@@4@@Z
  bool cv::VideoWriter::open(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,double,class cv::Size_<int> const &,class std::vector<int,class std::allocator<int> > const &)
}
{
  ?open@VideoWriter@cv@@UEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNV?$Size_@H@2@_N@Z
  ?open@VideoWriter@cv@@UEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNV?$Size_@H@2@_N@Z
  bool cv::VideoWriter::open(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,double,class cv::Size_<int>,bool)
  CV_WRAP virtual BOOL open(const String & filename, Int fourcc, double fps, size frameSize, BOOL isColor = true);
}
function open_VideoWriter(const Obj: TVideoWriter; const filename: CppString; fourcc: Int; fps: double; frameSize: UInt64 { TSize }; isColor: Bool = true): Bool; overload;
  external opencv_world_dll
  name '?open@VideoWriter@cv@@UEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNV?$Size_@H@2@_N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  ?release@VideoWriter@cv@@UEAAXXZ
  ?release@VideoWriter@cv@@UEAAXXZ
  void cv::VideoWriter::release(void)
}
procedure release_VideoWriter(const Obj: TVideoWriter); external opencv_world_dll name '?release@VideoWriter@cv@@UEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  ?set@VideoWriter@cv@@UEAA_NHN@Z
  ?set@VideoWriter@cv@@UEAA_NHN@Z
  bool cv::VideoWriter::set(int,double)
}
function set_VideoWriter(const Obj: TVideoWriter; propId: Int; value: double): Bool; external opencv_world_dll name '?set@VideoWriter@cv@@UEAA_NHN@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  ?write@VideoWriter@cv@@UEAAXAEBV_InputArray@debug_build_guard@2@@Z
  ?write@VideoWriter@cv@@UEAAXAEBV_InputArray@2@@Z
  void cv::VideoWriter::write(class cv::_InputArray const &)
  CV_WRAP virtual void write(InputArray image);
}
procedure write_VideoWriter(const Obj: TVideoWriter; const image: TInputArray); external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
  name '?write@VideoWriter@cv@@UEAAXAEBV_InputArray@debug_build_guard@2@@Z'
{$ELSE}
  name '?write@VideoWriter@cv@@UEAAXAEBV_InputArray@2@@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
