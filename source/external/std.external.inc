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

{$IF not defined(EXTERNAL_TYPES)}
{$DEFINE EXTERNAL_TYPES}

{$IF not defined(OPENCV_VERSION_HPP)}{$I 'core/version.inc'}{$ifend}
{$IF not defined(CVOPT)}{$I 'opt.inc'}{$ifend}

{
  opencv_delphi480 4
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &&)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::basic_string<ch
  ar,struct std::char_traits<char>,class std::allocator<char>> && __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_Assign(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_BASIC_STRING_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 5
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@AEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@AEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &&,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::basic_string<ch
  ar,struct std::char_traits<char>,class std::allocator<char>> && __ptr64,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_1_Assign(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_BASIC_STRING_OF_CVCHAR;
  const b: STD_ALLOCATOR_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@$$QEAV01@AEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 6
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::basic_string<ch
  ar,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 7
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@AEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@AEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::basic_string<ch
  ar,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  const b: STD_ALLOCATOR_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@AEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 8
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_K1AEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_K1AEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::basic_string<char,std::char_traits<char>,std::allocator<char>>
  const &,unsigned __int64,unsigned __int64,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::basic_string<ch
  ar,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64,unsigned __int64,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_4(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64; const d: STD_ALLOCATOR_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_K1AEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 9
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_KAEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_KAEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::basic_string<ch
  ar,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_5(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR; b: UNSIGNED___INT64;
  const c: STD_ALLOCATOR_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV01@_KAEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 10
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_6(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_ALLOCATOR_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@AEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 11
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(char const * const)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(char const * __ptr64 const) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_7(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 12
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBDAEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBDAEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(char const * const,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(char const * __ptr64 const,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_8(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; const b: STD_ALLOCATOR_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBDAEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 13
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_K@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(char const * const,unsigned __int64)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(char const * __ptr64 const,unsigned __int64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_9(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64); overload;
  external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 14
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_KAEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_KAEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(char const * const,unsigned __int64,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(char const * __ptr64 const
  ,unsigned __int64,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_10(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  const c: STD_ALLOCATOR_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@QEBD_KAEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 15
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@U_String_constructor_concat_tag@1@AEAV01@1@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@U_String_constructor_concat_tag@1@AEAV01@1@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::_String_constructor_concat_tag,std::basic_string<char,std::char
  _traits<char>,std::allocator<char>> &,std::basic_string<char,std::char_traits<char>,std::allocator<char>> &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(struct std::_String_constr
  uctor_concat_tag,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_11(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD__STRING_CONSTRUCTOR_CONCAT_TAG;
  var b: STD_BASIC_STRING_OF_CVCHAR; var c: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@U_String_constructor_concat_tag@1@AEAV01@1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 16
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@U_String_constructor_concat_tag@1@AEBV01@QEBD_K23@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@U_String_constructor_concat_tag@1@AEBV01@QEBD_K23@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::_String_constructor_concat_tag,std::basic_string<char,std::char
  _traits<char>,std::allocator<char>> const &,char const * const,unsigned __int64,char const * const,unsigned __int64)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(struct std::_String_constr
  uctor_concat_tag,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,char const * __ptr64 const,unsigned __int64,char const * __ptr64 const,unsigned __int64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_12(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD__STRING_CONSTRUCTOR_CONCAT_TAG;
  const b: STD_BASIC_STRING_OF_CVCHAR; const c: pCVCHAR; d: UNSIGNED___INT64; const e: pCVCHAR; f: UNSIGNED___INT64); overload;
  external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@U_String_constructor_concat_tag@1@AEBV01@QEBD_K23@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 17
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@V?$initializer_list@D@1@AEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@V?$initializer_list@D@1@AEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(std::initializer_list<char>,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(class std::initializer_lis
  t<char>,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_13(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_INITIALIZER_LIST_OF_CVCHAR;
  const b: STD_ALLOCATOR_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@V?$initializer_list@D@1@AEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 18
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(void)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(void) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_14(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 19
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KD@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(unsigned __int64,char)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(unsigned __int64,char) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_15(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: CVCHAR); overload;
  external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 20
  Type: Constructor
  Release: ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KDAEBV?$allocator@D@1@@Z
  Debug:   ??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KDAEBV?$allocator@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::basic_string<char,std::char_traits<char>,std::allocator<char>>(unsigned __int64,char,std::allocator<char> const &)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(unsigned __int64,char,class std::allocator<char> const & __ptr64) __ptr64
}
procedure constructor_STD_BASIC_STRING_OF_CVCHAR_16(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: CVCHAR;
  const c: STD_ALLOCATOR_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??0?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@_KDAEBV?$allocator@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 21
  Type: Constructor
  Release: ??0ExportString@@QEAA@$$QEAV0@@Z
  Debug:   ??0ExportString@@QEAA@$$QEAV0@@Z
  ExportString::ExportString(ExportString &&)
  public: __cdecl ExportString::ExportString(class ExportString && __ptr64) __ptr64
}
procedure constructor_EXPORTSTRING_Assign(Var Obj: EXPORTSTRING; a: EXPORTSTRING); overload; external opencv_delphi_dll
  name '??0ExportString@@QEAA@$$QEAV0@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 22
  Type: Constructor
  Release: ??0ExportString@@QEAA@AEBV0@@Z
  Debug:   ??0ExportString@@QEAA@AEBV0@@Z
  ExportString::ExportString(ExportString const &)
  public: __cdecl ExportString::ExportString(class ExportString const & __ptr64) __ptr64
}
procedure constructor_EXPORTSTRING_1(Var Obj: EXPORTSTRING; const a: EXPORTSTRING); overload; external opencv_delphi_dll
  name '??0ExportString@@QEAA@AEBV0@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 23
  Type: Constructor
  Release: ??0ExportString@@QEAA@XZ
  Debug:   ??0ExportString@@QEAA@XZ
  ExportString::ExportString(void)
  public: __cdecl ExportString::ExportString(void) __ptr64
}
procedure constructor_EXPORTSTRING_2(Var Obj: EXPORTSTRING); overload; external opencv_delphi_dll
  name '??0ExportString@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 24
  Type: Destructor
  Release: ??1?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ
  Debug:   ??1?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ
  std::basic_string<char,std::char_traits<char>,std::allocator<char>>::~basic_string<char,std::char_traits<char>,std::allocator<char>>(void)
  public: __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::~basic_string<char,struct std::char_traits<char>,class std::allocator<char>>(void) __ptr64
}
procedure destructor_STD_BASIC_STRING_OF_CVCHAR(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '??1?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 25
  Type: Destructor
  Release: ??1ExportString@@QEAA@XZ
  Debug:   ??1ExportString@@QEAA@XZ
  ExportString::~ExportString(void)
  public: __cdecl ExportString::~ExportString(void) __ptr64
}
procedure destructor_EXPORTSTRING(Var Obj: EXPORTSTRING); overload; external opencv_delphi_dll
  name '??1ExportString@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 26
  Type: Operator '='
  Release: ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@$$QEAV01@@Z
  Debug:   ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@$$QEAV01@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator=(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &&)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::opera
  tor=(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> && __ptr64) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_assign(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_BASIC_STRING_OF_CVCHAR): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@$$QEAV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 27
  Type: Operator '='
  Release: ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z
  Debug:   ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator=(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::opera
  tor=(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_assign_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 28
  Type: Operator '='
  Release: ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z
  Debug:   ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator=(char)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator=(char) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_assign_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload;
  external opencv_delphi_dll
  name '??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 29
  Type: Operator '='
  Release: ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z
  Debug:   ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator=(char const * const)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator=(char const * __ptr64 const) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_assign_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload;
  external opencv_delphi_dll
  name '??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 30
  Type: Operator '='
  Release: ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z
  Debug:   ??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator=(std::initializer_list<char>)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator=(class std::initializer_list<char>) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_assign_4(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_INITIALIZER_LIST_OF_CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '??4?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 31
  Type: Operator '='
  Release: ??4ExportString@@QEAAAEAV0@$$QEAV0@@Z
  Debug:   ??4ExportString@@QEAAAEAV0@$$QEAV0@@Z
  ExportString & ExportString::operator=(ExportString &&)
  public: class ExportString & __ptr64 __cdecl ExportString::operator=(class ExportString && __ptr64) __ptr64
}
function operator_EXPORTSTRING_assign(Var Obj: EXPORTSTRING; a: EXPORTSTRING): EXPORTSTRING; overload; external opencv_delphi_dll
  name '??4ExportString@@QEAAAEAV0@$$QEAV0@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 32
  Type: Operator '='
  Release: ??4ExportString@@QEAAAEAV0@AEBV0@@Z
  Debug:   ??4ExportString@@QEAAAEAV0@AEBV0@@Z
  ExportString & ExportString::operator=(ExportString const &)
  public: class ExportString & __ptr64 __cdecl ExportString::operator=(class ExportString const & __ptr64) __ptr64
}
function operator_EXPORTSTRING_assign_1(Var Obj: EXPORTSTRING; const a: EXPORTSTRING): EXPORTSTRING; overload; external opencv_delphi_dll
  name '??4ExportString@@QEAAAEAV0@AEBV0@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 33
  Type: Operator '[]'
  Release: ??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z
  Debug:   ??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z
  char & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator[](unsigned __int64)
  public: char & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator[](unsigned __int64) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_array(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64): CVCHAR; overload;
  external opencv_delphi_dll
  name '??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 34
  Type: Operator '[]'
  Release: ??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z
  Debug:   ??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z
  char const & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator[](unsigned __int64)
  public: char const & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator[](unsigned __int64)const __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_array_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64): CVCHAR; overload;
  external opencv_delphi_dll
  name '??A?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 35
  Type: Operator '+='
  Release: ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z
  Debug:   ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator+=(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::opera
  tor+=(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_add_assign(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@AEBV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 36
  Type: Operator '+='
  Release: ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z
  Debug:   ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator+=(char)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator+=(char) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_add_assign_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload;
  external opencv_delphi_dll
  name '??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@D@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 37
  Type: Operator '+='
  Release: ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z
  Debug:   ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator+=(char const * const)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator+=(char const * __ptr64 const) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_add_assign_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 38
  Type: Operator '+='
  Release: ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z
  Debug:   ??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::operator+=(std::initializer_list<char>)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::operator+=(class std::initializer_list<char>) __ptr64
}
function operator_STD_BASIC_STRING_OF_CVCHAR_add_assign_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_INITIALIZER_LIST_OF_CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '??Y?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV01@V?$initializer_list@D@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 39
  Type: non class C++ procedure
  Release: ?CopyStdVector@@YAXPEAX0H@Z
  Debug:   ?CopyStdVector@@YAXPEAX0H@Z
  void CopyStdVector(void *,void *,int)
  void __cdecl CopyStdVector(void * __ptr64,void * __ptr64,int)
}
procedure proc_CopyStdVector(const Obj, Src: pVector; vt: TVectorType); overload; external opencv_delphi_dll
  name '?CopyStdVector@@YAXPEAX0H@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 40
  Type: non class C++ procedure
  Release: ?CreateStdVector@@YAXPEAXH@Z
  Debug:   ?CreateStdVector@@YAXPEAXH@Z
  void CreateStdVector(void *,int)
  void __cdecl CreateStdVector(void * __ptr64,int)
}
procedure proc_CreateStdVector(Obj: pVector; vt: TVectorType); overload; external opencv_delphi_dll
  name '?CreateStdVector@@YAXPEAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 41
  Type: non class C++ procedure
  Release: ?DestroyStdVector@@YAXPEAXH@Z
  Debug:   ?DestroyStdVector@@YAXPEAXH@Z
  void DestroyStdVector(void *,int)
  void __cdecl DestroyStdVector(void * __ptr64,int)
}
procedure proc_DestroyStdVector(Obj: pVector; vt: TVectorType); overload; external opencv_delphi_dll
  name '?DestroyStdVector@@YAXPEAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 42
  Type: non class C++ function
  Release: ?StdEmpty@@YA_NPEAXH@Z
  Debug:   ?StdEmpty@@YA_NPEAXH@Z
  bool StdEmpty(void *,int)
  bool __cdecl StdEmpty(void * __ptr64,int)
}
function func_StdEmpty(Obj: pVector; vt: TVectorType): BOOL; overload; external opencv_delphi_dll
  name '?StdEmpty@@YA_NPEAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


{
  opencv_delphi480 43
  Type: non class C++ procedure
  Release: ?StdItem@@YAXPEAXH_K0@Z
  Debug:   ?StdItem@@YAXPEAXH_K0@Z
  void StdItem(void *,int,unsigned __int64,void *)
  void __cdecl StdItem(void * __ptr64,int,unsigned __int64,void * __ptr64)
}
procedure proc_StdItem(const Obj: pVector; vt: TVectorType; index: UInt64; const pGetValue: Pointer); overload; external opencv_delphi_dll
  name '?StdItem@@YAXPEAXH_K0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 44
  Type: non class C++ procedure
  Release: ?StdPItem@@YAXPEAXH_KPEAPEAX@Z
  Debug:   ?StdPItem@@YAXPEAXH_KPEAPEAX@Z
  void StdPItem(void *,int,unsigned __int64,void *)
  void __cdecl StdPItem(void * __ptr64,int,unsigned __int64,void * __ptr64 * __ptr64)
}
procedure proc_StdPItem(const Obj: pVector; vt: TVectorType; index: UInt64; Var p: pVector); overload; external opencv_delphi_dll
  name '?StdPItem@@YAXPEAXH_KPEAPEAX@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 45
  Type: non class C++ procedure
  Release: ?StdPushBack@@YAXPEAX0H@Z
  Debug:   ?StdPushBack@@YAXPEAX0H@Z
  void StdPushBack(void *,void *,int)
  void __cdecl StdPushBack(void * __ptr64,void * __ptr64,int)
}
procedure proc_StdPushBack(Obj: pVector; p: pVector; vt: TVectorType); overload; external opencv_delphi_dll
  name '?StdPushBack@@YAXPEAX0H@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 46
  Type: non class C++ procedure
  Release: ?StdSetItem@@YAXPEAXH_K0@Z
  Debug:   ?StdSetItem@@YAXPEAXH_K0@Z
  void StdSetItem(void *,int,unsigned __int64,void *)
  void __cdecl StdSetItem(void * __ptr64,int,unsigned __int64,void * __ptr64)
}
procedure proc_StdSetItem(const Obj: pVector; vt: TVectorType; index: UInt64; const pSetValue: Pointer); overload; external opencv_delphi_dll
  name '?StdSetItem@@YAXPEAXH_K0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 47
  Type: non class C++ function
  Release: ?StdSize@@YA_KPEAXH@Z
  Debug:   ?StdSize@@YA_KPEAXH@Z
  unsigned __int64 StdSize(void *,int)
  unsigned __int64 __cdecl StdSize(void * __ptr64,int)
}
function func_StdSize(Obj: pVector; vt: TVectorType): UNSIGNED___INT64;
overload; external opencv_delphi_dll
  name '?StdSize@@YA_KPEAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 48
  Type: class virual procedure
  Release: ?_Become_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ
  Debug:   ?_Become_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Become_small(void)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Become_small(void) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Become_small(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?_Become_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 49
  Type: class virual function
  Release: ?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBA_K_K@Z
  Debug:   ?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBA_K_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Calculate_growth(unsigned __int64)
  private: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Calculate_growth(unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Calculate_growth(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64): UNSIGNED___INT64;
  overload; external opencv_delphi_dll
  name '?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBA_K_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 50
  Type: class virual function
  Release: ?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@CA_K_K00@Z
  Debug:   ?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@CA_K_K00@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Calculate_growth(unsigned __int64,unsigned __int64,unsigned __int64)
  private: static unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Calculate_growth(unsigned __int64,unsigned __int64,unsigned __int64)
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Calculate_growth_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?_Calculate_growth@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@CA_K_K00@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 51
  Type: class virual procedure
  Release: ?_Copy_assign_val_from_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z
  Debug:   ?_Copy_assign_val_from_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Copy_assign_val_from_small(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Copy_assign_val_from_small(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Copy_assign_val_from_small(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  const a: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?_Copy_assign_val_from_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 52
  Type: class virual function
  Release: ?_Copy_s@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K1@Z
  Debug:   ?_Copy_s@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K1@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Copy_s(char * const,unsigned __int64,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Copy_s(char * __ptr64 const,unsigned __int64,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Copy_s(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64; d: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?_Copy_s@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 53
  Type: class virual procedure
  Release: ?_Eos@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAX_K@Z
  Debug:   ?_Eos@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAX_K@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Eos(unsigned __int64)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Eos(unsigned __int64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Eos(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64); overload;
  external opencv_delphi_dll
  name '?_Eos@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAX_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 54
  Type: class virual function
  Release: ?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NAEBV12@@Z
  Debug:   ?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NAEBV12@@Z
  bool std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Equal(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: bool __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Equal(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Equal(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR): BOOL; overload;
  external opencv_delphi_dll
  name '?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NAEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 55
  Type: class virual function
  Release: ?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NQEBD@Z
  Debug:   ?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NQEBD@Z
  bool std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Equal(char const * const)
  public: bool __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Equal(char const * __ptr64 const)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Equal_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR): BOOL; overload;
  external opencv_delphi_dll
  name '?_Equal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NQEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 56
  Type: class virual function
  Release: ?_Erase_noexcept@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAAEAV12@_K_K@Z
  Debug:   ?_Erase_noexcept@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAAEAV12@_K_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Erase_noexcept(unsigned __int64,unsigned __int64)
  private: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Erase_noexcept(unsigned __int64,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Erase_noexcept(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?_Erase_noexcept@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAAEAV12@_K_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 57
  Type: class virual function
  Release: ?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAAEAV?$allocator@D@2@XZ
  Debug:   ?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAAEAV?$allocator@D@2@XZ
  std::allocator<char> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Getal(void)
  private: class std::allocator<char> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Getal(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Getal(Var Obj: STD_BASIC_STRING_OF_CVCHAR): STD_ALLOCATOR_OF_CVCHAR; overload;
  external opencv_delphi_dll
  name '?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAAEAV?$allocator@D@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 58
  Type: class virual function
  Release: ?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBAAEBV?$allocator@D@2@XZ
  Debug:   ?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBAAEBV?$allocator@D@2@XZ
  std::allocator<char> const & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Getal(void)
  private: class std::allocator<char> const & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Getal(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Getal_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR): STD_ALLOCATOR_OF_CVCHAR; overload;
  external opencv_delphi_dll
  name '?_Getal@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBAAEBV?$allocator@D@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 59
  Type: class virual procedure
  Release: ?_Memcpy_val_from@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z
  Debug:   ?_Memcpy_val_from@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Memcpy_val_from(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Memcpy_val_from(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Memcpy_val_from(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR);
  overload; external opencv_delphi_dll
  name '?_Memcpy_val_from@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 60
  Type: class virual procedure
  Release: ?_Orphan_all@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  Debug:   ?_Orphan_all@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Orphan_all(void)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Orphan_all(void) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Orphan_all(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?_Orphan_all@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 61
  Type: class virual procedure
  Release: ?_Swap_bx_large_with_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@SAXAEAV?$_String_val@U?$_Simple_types@D@std@@@2@0@Z
  Debug:   ?_Swap_bx_large_with_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@SAXAEAV?$_String_val@U?$_Simple_types@D@std@@@2@0@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Swap_bx_large_with_small(std::_String_val<std::_Simple_types<char>> &,std::_String_val<std::_Simple_types<char>> &)
  public: static void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Swap_bx_large_with_small(class std::_String_val<struct std::_Simple_types<char>> & __ptr6
  4,class std::_String_val<struct std::_Simple_types<char>> & __ptr64)
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Swap_bx_large_with_small(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  var a: STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; var b: STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?_Swap_bx_large_with_small@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@SAXAEAV?$_String_val@U?$_Simple_types@D@std@@@2@0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 62
  Type: class virual procedure
  Release: ?_Swap_data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@@Z
  Debug:   ?_Swap_data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Swap_data(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Swap_data(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Swap_data(Var Obj: STD_BASIC_STRING_OF_CVCHAR; var a: STD_BASIC_STRING_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '?_Swap_data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 63
  Type: class virual procedure
  Release: ?_Swap_proxy_and_iterators@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@@Z
  Debug:   ?_Swap_proxy_and_iterators@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Swap_proxy_and_iterators(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Swap_proxy_and_iterators(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Swap_proxy_and_iterators(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  var a: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?_Swap_proxy_and_iterators@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 64
  Type: class virual procedure
  Release: ?_Take_contents@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@@Z
  Debug:   ?_Take_contents@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Take_contents(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Take_contents(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Take_contents(Var Obj: STD_BASIC_STRING_OF_CVCHAR; var a: STD_BASIC_STRING_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '?_Take_contents@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXAEAV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 65
  Type: class virual procedure
  Release: ?_Tidy_deallocate@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ
  Debug:   ?_Tidy_deallocate@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Tidy_deallocate(void)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Tidy_deallocate(void) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Tidy_deallocate(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?_Tidy_deallocate@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 66
  Type: class virual procedure
  Release: ?_Tidy_init@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ
  Debug:   ?_Tidy_init@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Tidy_init(void)
  private: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Tidy_init(void) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR__Tidy_init(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?_Tidy_init@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 67
  Type: class virual function
  Release: ?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ
  Debug:   ?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ
  char * std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Unchecked_begin(void)
  public: char * __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Unchecked_begin(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Unchecked_begin(Var Obj: STD_BASIC_STRING_OF_CVCHAR): pCVCHAR; overload;
  external opencv_delphi_dll
  name '?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 68
  Type: class virual function
  Release: ?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  Debug:   ?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  char const * std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Unchecked_begin(void)
  public: char const * __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Unchecked_begin(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Unchecked_begin_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR): pCVCHAR; overload;
  external opencv_delphi_dll
  name '?_Unchecked_begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 69
  Type: class virual function
  Release: ?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ
  Debug:   ?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ
  char * std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Unchecked_end(void)
  public: char * __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Unchecked_end(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Unchecked_end(Var Obj: STD_BASIC_STRING_OF_CVCHAR): pCVCHAR; overload; external opencv_delphi_dll
  name '?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAPEADXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 70
  Type: class virual function
  Release: ?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  Debug:   ?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  char const * std::basic_string<char,std::char_traits<char>,std::allocator<char>>::_Unchecked_end(void)
  public: char const * __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::_Unchecked_end(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR__Unchecked_end_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR): pCVCHAR; overload;
  external opencv_delphi_dll
  name '?_Unchecked_end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 71
  Type: class virual function
  Release: ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z
  Debug:   ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::append(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::appen
  d(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_append(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 72
  Type: class virual function
  Release: ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z
  Debug:   ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::append(std::basic_string<char,std::char_traits<char>,std::all
  ocator<char>> const &,unsigned __int64,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::appen
  d(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_append_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64; c: UNSIGNED___INT64): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 73
  Type: class virual function
  Release: ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z
  Debug:   ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::append(char const * const)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::append(char const * __ptr64 const) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_append_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 74
  Type: class virual function
  Release: ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z
  Debug:   ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::append(char const * const,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::append(char const * __ptr64 const,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_append_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 75
  Type: class virual function
  Release: ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z
  Debug:   ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::append(std::initializer_list<char>)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::append(class std::initializer_list<char>) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_append_4(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_INITIALIZER_LIST_OF_CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 76
  Type: class virual function
  Release: ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z
  Debug:   ?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::append(unsigned __int64,char)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::append(unsigned __int64,char) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_append_5(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?append@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 77
  Type: class virual function
  Release: ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@$$QEAV12@@Z
  Debug:   ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@$$QEAV12@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::assign(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &&)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::assig
  n(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> && __ptr64) __ptr64
}
procedure class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_Assign(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_BASIC_STRING_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@$$QEAV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 78
  Type: class virual function
  Release: ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z
  Debug:   ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::assign(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::assig
  n(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
procedure class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 79
  Type: class virual function
  Release: ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z
  Debug:   ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::assign(std::basic_string<char,std::char_traits<char>,std::all
  ocator<char>> const &,unsigned __int64,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::assig
  n(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64,unsigned __int64) __ptr64
}
procedure class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64; c: UNSIGNED___INT64); overload; external opencv_delphi_dll
  name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@AEBV12@_K_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 80
  Type: class virual function
  Release: ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z
  Debug:   ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::assign(char const * const)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::assign(char const * __ptr64 const) __ptr64
}
procedure class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR); overload;
  external opencv_delphi_dll
  name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 81
  Type: class virual function
  Release: ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z
  Debug:   ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::assign(char const * const,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::assign(char const * __ptr64 const,unsigned __int64) __ptr64
}
procedure class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_4(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64); overload;
  external opencv_delphi_dll
  name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@QEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 82
  Type: class virual function
  Release: ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z
  Debug:   ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::assign(std::initializer_list<char>)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::assign(class std::initializer_list<char>) __ptr64
}
procedure class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_5(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: STD_INITIALIZER_LIST_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$initializer_list@D@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 83
  Type: class virual function
  Release: ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z
  Debug:   ?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::assign(unsigned __int64,char)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::assign(unsigned __int64,char) __ptr64
}
procedure class_virt_func_STD_BASIC_STRING_OF_CVCHAR_assign_6(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: CVCHAR); overload;
  external opencv_delphi_dll
  name '?assign@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 84
  Type: class virual function
  Release: ?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z
  Debug:   ?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z
  char & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::at(unsigned __int64)
  public: char & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::at(unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_at(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64): CVCHAR; overload;
  external opencv_delphi_dll
  name '?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 85
  Type: class virual function
  Release: ?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z
  Debug:   ?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z
  char const & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::at(unsigned __int64)
  public: char const & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::at(unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_at_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64): CVCHAR; overload;
  external opencv_delphi_dll
  name '?at@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 86
  Type: class virual function
  Release: ?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ
  Debug:   ?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ
  char & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::back(void)
  public: char & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::back(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_back(Var Obj: STD_BASIC_STRING_OF_CVCHAR): CVCHAR; overload; external opencv_delphi_dll
  name '?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 87
  Type: class virual function
  Release: ?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ
  Debug:   ?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ
  char const & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::back(void)
  public: char const & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::back(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_back_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR): CVCHAR; overload; external opencv_delphi_dll
  name '?back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 88
  Type: class virual function
  Release: ?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  Debug:   ?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  std::_String_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::begin(void)
  public: class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::begin(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_begin(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 89
  Type: class virual function
  Release: ?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  Debug:   ?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::begin(void)
  public: class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::begin(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_begin_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?begin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 90
  Type: class virual function
  Release: ?c_str@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  Debug:   ?c_str@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  char const * std::basic_string<char,std::char_traits<char>,std::allocator<char>>::c_str(void)
  public: char const * __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::c_str(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_c_str(const Obj: STD_BASIC_STRING_OF_CVCHAR): pCVCHAR; overload; external opencv_delphi_dll
  name '?c_str@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 91
  Type: class virual function
  Release: ?capacity@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  Debug:   ?capacity@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::capacity(void)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::capacity(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_capacity(Var Obj: STD_BASIC_STRING_OF_CVCHAR): UNSIGNED___INT64; overload;
  external opencv_delphi_dll
  name '?capacity@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 92
  Type: class virual function
  Release: ?cbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  Debug:   ?cbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::cbegin(void)
  public: class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::cbegin(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_cbegin(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?cbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 93
  Type: class virual function
  Release: ?cend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  Debug:   ?cend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::cend(void)
  public: class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::cend(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_cend(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?cend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 94
  Type: class virual procedure
  Release: ?clear@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  Debug:   ?clear@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::clear(void)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::clear(void) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR_clear(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?clear@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 95
  Type: non class C++ procedure
  Release: ?clearStdVector@@YAXPEAXH@Z
  Debug:   ?clearStdVector@@YAXPEAXH@Z
  void clearStdVector(void *,int)
  void __cdecl clearStdVector(void * __ptr64,int)
}
procedure proc_clearStdVector(Obj: pVector; vt: TVectorType); overload; external opencv_delphi_dll
  name '?clearStdVector@@YAXPEAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 96
  Type: class virual function
  Release: ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHAEBV12@@Z
  Debug:   ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHAEBV12@@Z
  int std::basic_string<char,std::char_traits<char>,std::allocator<char>>::compare(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: int __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::compare(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_compare(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR): INT; overload;
  external opencv_delphi_dll
  name '?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHAEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 97
  Type: class virual function
  Release: ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHQEBD@Z
  Debug:   ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHQEBD@Z
  int std::basic_string<char,std::char_traits<char>,std::allocator<char>>::compare(char const * const)
  public: int __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::compare(char const * __ptr64 const)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_compare_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR): INT; overload;
  external opencv_delphi_dll
  name '?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAHQEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 98
  Type: class virual function
  Release: ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@00@Z
  Debug:   ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@00@Z
  int std::basic_string<char,std::char_traits<char>,std::allocator<char>>::compare(unsigned __int64,unsigned __int64,std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64,unsigned __int64)
  public: int __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::compare(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<cha
  r>,class std::allocator<char>> const & __ptr64,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_compare_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: STD_BASIC_STRING_OF_CVCHAR; d: UNSIGNED___INT64; e: UNSIGNED___INT64): INT; overload; external opencv_delphi_dll
  name '?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@00@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 99
  Type: class virual function
  Release: ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@@Z
  Debug:   ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@@Z
  int std::basic_string<char,std::char_traits<char>,std::allocator<char>>::compare(unsigned __int64,unsigned __int64,std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: int __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::compare(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<cha
  r>,class std::allocator<char>> const & __ptr64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_compare_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: STD_BASIC_STRING_OF_CVCHAR): INT; overload; external opencv_delphi_dll
  name '?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0AEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 100
  Type: class virual function
  Release: ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD0@Z
  Debug:   ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD0@Z
  int std::basic_string<char,std::char_traits<char>,std::allocator<char>>::compare(unsigned __int64,unsigned __int64,char const * const,unsigned __int64)
  public: int __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::compare(unsigned __int64,unsigned __int64,char const * __ptr64 const,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_compare_4(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: pCVCHAR; d: UNSIGNED___INT64): INT; overload; external opencv_delphi_dll
  name '?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 101
  Type: class virual function
  Release: ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD@Z
  Debug:   ?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD@Z
  int std::basic_string<char,std::char_traits<char>,std::allocator<char>>::compare(unsigned __int64,unsigned __int64,char const * const)
  public: int __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::compare(unsigned __int64,unsigned __int64,char const * __ptr64 const)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_compare_5(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: pCVCHAR): INT; overload; external opencv_delphi_dll
  name '?compare@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAH_K0QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 102
  Type: class virual function
  Release: ?copy@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K@Z
  Debug:   ?copy@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::copy(char * const,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::copy(char * __ptr64 const,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_copy(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?copy@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEAD_K_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 103
  Type: class virual function
  Release: ?crbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  Debug:   ?crbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  std::reverse_iterator<std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::crbegin(void)
  public: class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::crbegin(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_crbegin(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD_REVERSE_ITERATOR_OF_STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?crbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 104
  Type: class virual function
  Release: ?crend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  Debug:   ?crend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  std::reverse_iterator<std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::crend(void)
  public: class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::crend(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_crend(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD_REVERSE_ITERATOR_OF_STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?crend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 105
  Type: class virual function
  Release: ?data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  Debug:   ?data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ
  char const * std::basic_string<char,std::char_traits<char>,std::allocator<char>>::data(void)
  public: char const * __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::data(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_data(Var Obj: STD_BASIC_STRING_OF_CVCHAR): pCVCHAR; overload; external opencv_delphi_dll
  name '?data@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAPEBDXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 106
  Type: class virual function
  Release: ?empty@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NXZ
  Debug:   ?empty@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NXZ
  bool std::basic_string<char,std::char_traits<char>,std::allocator<char>>::empty(void)
  public: bool __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::empty(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_empty(Var Obj: STD_BASIC_STRING_OF_CVCHAR): BOOL; overload; external opencv_delphi_dll
  name '?empty@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_NXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 107
  Type: class virual function
  Release: ?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  Debug:   ?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  std::_String_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::end(void)
  public: class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::end(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_end(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 108
  Type: class virual function
  Release: ?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  Debug:   ?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ
  std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::end(void)
  public: class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::end(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_end_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?end@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 109
  Type: class virual function
  Release: ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0@Z
  Debug:   ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0@Z
  std::_String_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::erase(std::_String_const_iterator<std::_String_val<std::_Simple_t
  ypes<char>>>,std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>)
  public: class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::erase(class std::
  _String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_erase(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR;
  b: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR)
  : STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 110
  Type: class virual function
  Release: ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@@Z
  Debug:   ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@@Z
  std::_String_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::erase(std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>)
  public: class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::erase(class std::
  _String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_erase_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR)
  : STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 111
  Type: class virual function
  Release: ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0@Z
  Debug:   ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::erase(unsigned __int64,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::erase(unsigned __int64,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_erase_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 112
  Type: class virual function
  Release: ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K@Z
  Debug:   ?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::erase(unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::erase(unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_erase_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '?erase@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 113
  Type: class virual function
  Release: ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  Debug:   ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 114
  Type: class virual function
  Release: ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  Debug:   ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find(char,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find(char,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR; b: UNSIGNED___INT64): UNSIGNED___INT64;
  overload; external opencv_delphi_dll
  name '?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 115
  Type: class virual function
  Release: ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  Debug:   ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find(char const * const,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find(char const * __ptr64 const,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 116
  Type: class virual function
  Release: ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  Debug:   ?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find(char const * const,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find(char const * __ptr64 const,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 117
  Type: class virual function
  Release: ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  Debug:   ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_not_of(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_not_of(class std::basic_string<char,struct std::char_traits<char>,class st
  d::allocator<char>> const & __ptr64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_not_of(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 118
  Type: class virual function
  Release: ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  Debug:   ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_not_of(char,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_not_of(char,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_not_of_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 119
  Type: class virual function
  Release: ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  Debug:   ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_not_of(char const * const,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_not_of(char const * __ptr64 const,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_not_of_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 120
  Type: class virual function
  Release: ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  Debug:   ?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_not_of(char const * const,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_not_of(char const * __ptr64 const,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_not_of_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 121
  Type: class virual function
  Release: ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  Debug:   ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_of(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_of(class std::basic_string<char,struct std::char_traits<char>,class std::a
  llocator<char>> const & __ptr64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_of(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 122
  Type: class virual function
  Release: ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  Debug:   ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_of(char,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_of(char,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_of_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 123
  Type: class virual function
  Release: ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  Debug:   ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_of(char const * const,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_of(char const * __ptr64 const,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_of_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 124
  Type: class virual function
  Release: ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  Debug:   ?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_first_of(char const * const,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_first_of(char const * __ptr64 const,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_first_of_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_first_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 125
  Type: class virual function
  Release: ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  Debug:   ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_not_of(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_not_of(class std::basic_string<char,struct std::char_traits<char>,class std
  ::allocator<char>> const & __ptr64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_not_of(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 126
  Type: class virual function
  Release: ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  Debug:   ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_not_of(char,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_not_of(char,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_not_of_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 127
  Type: class virual function
  Release: ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  Debug:   ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_not_of(char const * const,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_not_of(char const * __ptr64 const,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_not_of_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 128
  Type: class virual function
  Release: ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  Debug:   ?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_not_of(char const * const,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_not_of(char const * __ptr64 const,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_not_of_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_not_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 129
  Type: class virual function
  Release: ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  Debug:   ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_of(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_of(class std::basic_string<char,struct std::char_traits<char>,class std::al
  locator<char>> const & __ptr64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_of(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 130
  Type: class virual function
  Release: ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  Debug:   ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_of(char,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_of(char,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_of_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 131
  Type: class virual function
  Release: ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  Debug:   ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_of(char const * const,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_of(char const * __ptr64 const,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_of_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 132
  Type: class virual function
  Release: ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  Debug:   ?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::find_last_of(char const * const,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::find_last_of(char const * __ptr64 const,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_find_last_of_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?find_last_of@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 133
  Type: class virual function
  Release: ?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ
  Debug:   ?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ
  char & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::front(void)
  public: char & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::front(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_front(Var Obj: STD_BASIC_STRING_OF_CVCHAR): CVCHAR; overload; external opencv_delphi_dll
  name '?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEADXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 134
  Type: class virual function
  Release: ?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ
  Debug:   ?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ
  char const & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::front(void)
  public: char const & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::front(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_front_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR): CVCHAR; overload; external opencv_delphi_dll
  name '?front@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBAAEBDXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 135
  Type: class virual function
  Release: ?get_allocator@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$allocator@D@2@XZ
  Debug:   ?get_allocator@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$allocator@D@2@XZ
  std::allocator<char> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::get_allocator(void)
  public: class std::allocator<char> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::get_allocator(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_get_allocator(Var Obj: STD_BASIC_STRING_OF_CVCHAR): STD_ALLOCATOR_OF_CVCHAR; overload;
  external opencv_delphi_dll
  name '?get_allocator@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$allocator@D@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 136
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@D@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@D@Z
  std::_String_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,char)
  public: class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::insert(class std:
  :_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,char) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; b: CVCHAR)
  : STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@D@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 137
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$initializer_list@D@2@@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$initializer_list@D@2@@Z
  std::_String_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,std::initializer_list<char>)
  public: class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::insert(class std:
  :_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::initializer_list<char>) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; b: STD_INITIALIZER_LIST_OF_CVCHAR)
  : STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$initializer_list@D@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 138
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@_KD@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@_KD@Z
  std::_String_iterator<std::_String_val<std::_Simple_types<char>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,unsigned __int64,char)
  public: class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::insert(class std:
  :_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,unsigned __int64,char) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; b: UNSIGNED___INT64; c: CVCHAR)
  : STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@_KD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 139
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0D@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0D@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(unsigned __int64,unsigned __int64,char)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::insert(unsigned __int64,unsigned __int64,char) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64; c: CVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0D@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 140
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@0_K@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@0_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(unsigned __int64,std::basic_string<char,std::char_trai
  ts<char>,std::allocator<char>> const &,unsigned __int64,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::inser
  t(unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert_4(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64;
  const b: STD_BASIC_STRING_OF_CVCHAR; c: UNSIGNED___INT64; d: UNSIGNED___INT64): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@0_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 141
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(unsigned __int64,std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::inser
  t(unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert_5(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64;
  const b: STD_BASIC_STRING_OF_CVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KAEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 142
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD0@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD0@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(unsigned __int64,char const * const,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::inser
  t(unsigned __int64,char const * __ptr64 const,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert_6(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; const b: pCVCHAR;
  c: UNSIGNED___INT64): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 143
  Type: class virual function
  Release: ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD@Z
  Debug:   ?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::insert(unsigned __int64,char const * const)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::insert(unsigned __int64,char const * __ptr64 const) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_insert_7(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; const b: pCVCHAR)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?insert@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_KQEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 144
  Type: class virual function
  Release: ?length@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  Debug:   ?length@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::length(void)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::length(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_length(Var Obj: STD_BASIC_STRING_OF_CVCHAR): UNSIGNED___INT64; overload;
  external opencv_delphi_dll
  name '?length@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 145
  Type: class virual function
  Release: ?max_size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  Debug:   ?max_size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::max_size(void)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::max_size(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_max_size(Var Obj: STD_BASIC_STRING_OF_CVCHAR): UNSIGNED___INT64; overload;
  external opencv_delphi_dll
  name '?max_size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 146
  Type: class virual procedure
  Release: ?pop_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  Debug:   ?pop_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::pop_back(void)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::pop_back(void) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR_pop_back(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?pop_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 147
  Type: class virual procedure
  Release: ?push_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXD@Z
  Debug:   ?push_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXD@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::push_back(char)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::push_back(char) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR_push_back(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR); overload; external opencv_delphi_dll
  name '?push_back@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 148
  Type: class virual function
  Release: ?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  Debug:   ?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  std::reverse_iterator<std::_String_iterator<std::_String_val<std::_Simple_types<char>>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rbegin(void)
  public: class std::reverse_iterator<class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rbegin(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rbegin(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD_REVERSE_ITERATOR_OF_STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 149
  Type: class virual function
  Release: ?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  Debug:   ?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  std::reverse_iterator<std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rbegin(void)
  public: class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rbegin(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rbegin_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD_REVERSE_ITERATOR_OF_STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?rbegin@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 150
  Type: class virual function
  Release: ?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  Debug:   ?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  std::reverse_iterator<std::_String_iterator<std::_String_val<std::_Simple_types<char>>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rend(void)
  public: class std::reverse_iterator<class std::_String_iterator<class std::_String_val<struct std::_Simple_types<char>>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rend(void) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rend(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD_REVERSE_ITERATOR_OF_STD__STRING_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAA?AV?$reverse_iterator@V?$_String_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 151
  Type: class virual function
  Release: ?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  Debug:   ?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ
  std::reverse_iterator<std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rend(void)
  public: class std::reverse_iterator<class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rend(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rend_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR)
  : STD_REVERSE_ITERATOR_OF_STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?rend@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV?$reverse_iterator@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@std@@@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 152
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0AEBV12@@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0AEBV12@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(std::_String_const_iterator<std::_String_val<std::_Si
  mple_types<char>>>,std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::basi
  c_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR;
  b: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; const c: STD_BASIC_STRING_OF_CVCHAR): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0AEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 153
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(std::_String_const_iterator<std::_String_val<std::_Si
  mple_types<char>>>,std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,char const * const)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,char const * __ptr64 const) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR;
  b: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; const c: pCVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload;
  external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 154
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD_K@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD_K@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(std::_String_const_iterator<std::_String_val<std::_Si
  mple_types<char>>>,std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,char const * const,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,char const * __ptr64 const,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR;
  b: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; const c: pCVCHAR; d: UNSIGNED___INT64): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0QEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 155
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0V?$initializer_list@D@2@@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0V?$initializer_list@D@2@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(std::_String_const_iterator<std::_String_val<std::_Si
  mple_types<char>>>,std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,std::initializer_list<char>)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::initializer_list<char>) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR;
  b: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; c: STD_INITIALIZER_LIST_OF_CVCHAR): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0V?$initializer_list@D@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 156
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0_KD@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0_KD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(std::_String_const_iterator<std::_String_val<std::_Si
  mple_types<char>>>,std::_String_const_iterator<std::_String_val<std::_Simple_types<char>>>,unsigned __int64,char)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,class std::_String_const_iterator<class std::_String_val<struct std::_Simple_types<char>>>,unsigned __int64,char) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_4(Var Obj: STD_BASIC_STRING_OF_CVCHAR;
  a: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR;
  b: STD__STRING_CONST_ITERATOR_OF_STD__STRING_VAL_OF_STD__SIMPLE_TYPES_OF_CVCHAR; c: UNSIGNED___INT64; d: CVCHAR): STD_BASIC_STRING_OF_CVCHAR;
  overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@V?$_String_const_iterator@V?$_String_val@U?$_Simple_types@D@std@@@std@@@2@0_KD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 157
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0AEBV12@@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0AEBV12@@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(unsigned __int64,unsigned __int64,std::basic_string<c
  har,std::char_traits<char>,std::allocator<char>> const &)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_5(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: STD_BASIC_STRING_OF_CVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0AEBV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 158
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0QEBD@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0QEBD@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(unsigned __int64,unsigned __int64,char const * const)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(unsigned __int64,unsigned __int64,char const * __ptr64 const) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_6(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: pCVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K0QEBD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 159
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_K0D@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_K0D@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(unsigned __int64,unsigned __int64,unsigned __int64,char)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(unsigned __int64,unsigned __int64,unsigned __int64,char) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_7(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64; d: CVCHAR): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_K0D@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 160
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KAEBV12@01@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KAEBV12@01@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(unsigned __int64,unsigned __int64,std::basic_string<c
  har,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(unsigned __int64,unsigned __int64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_8(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: STD_BASIC_STRING_OF_CVCHAR; d: UNSIGNED___INT64; e: UNSIGNED___INT64): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KAEBV12@01@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 161
  Type: class virual function
  Release: ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KQEBD0@Z
  Debug:   ?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KQEBD0@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> & std::basic_string<char,std::char_traits<char>,std::allocator<char>>::replace(unsigned __int64,unsigned __int64,char const * const,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::repla
  ce(unsigned __int64,unsigned __int64,char const * __ptr64 const,unsigned __int64) __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_replace_9(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64;
  const c: pCVCHAR; d: UNSIGNED___INT64): STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?replace@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAAEAV12@_K_KQEBD0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 162
  Type: class virual procedure
  Release: ?reserve@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_K@Z
  Debug:   ?reserve@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_K@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::reserve(unsigned __int64)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::reserve(unsigned __int64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR_reserve(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64); overload;
  external opencv_delphi_dll
  name '?reserve@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 163
  Type: class virual procedure
  Release: ?resize@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_KD@Z
  Debug:   ?resize@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_KD@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::resize(unsigned __int64,char)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::resize(unsigned __int64,char) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR_resize(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: CVCHAR); overload;
  external opencv_delphi_dll
  name '?resize@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAX_KD@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 164
  Type: non class C++ procedure
  Release: ?resizeStdVector@@YAXPEAX_KH@Z
  Debug:   ?resizeStdVector@@YAXPEAX_KH@Z
  void resizeStdVector(void *,unsigned __int64,int)
  void __cdecl resizeStdVector(void * __ptr64,unsigned __int64,int)
}
procedure proc_resizeStdVector(Obj: pVector; NewSize: UInt64; vt: TVectorType); overload; external opencv_delphi_dll
  name '?resizeStdVector@@YAXPEAX_KH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 165
  Type: class virual function
  Release: ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  Debug:   ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rfind(std::basic_string<char,std::char_traits<char>,std::allocator<char>> const &,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rfind(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> const & __ptr64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rfind(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: STD_BASIC_STRING_OF_CVCHAR;
  b: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KAEBV12@_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 166
  Type: class virual function
  Release: ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  Debug:   ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rfind(char,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rfind(char,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rfind_1(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: CVCHAR; b: UNSIGNED___INT64): UNSIGNED___INT64;
  overload; external opencv_delphi_dll
  name '?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 167
  Type: class virual function
  Release: ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  Debug:   ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rfind(char const * const,unsigned __int64,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rfind(char const * __ptr64 const,unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rfind_2(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64;
  c: UNSIGNED___INT64): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K1@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 168
  Type: class virual function
  Release: ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  Debug:   ?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::rfind(char const * const,unsigned __int64)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::rfind(char const * __ptr64 const,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_rfind_3(Var Obj: STD_BASIC_STRING_OF_CVCHAR; const a: pCVCHAR; b: UNSIGNED___INT64)
  : UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?rfind@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KQEBD_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 169
  Type: class virual procedure
  Release: ?shrink_to_fit@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  Debug:   ?shrink_to_fit@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::shrink_to_fit(void)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::shrink_to_fit(void) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR_shrink_to_fit(Var Obj: STD_BASIC_STRING_OF_CVCHAR); overload; external opencv_delphi_dll
  name '?shrink_to_fit@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 170
  Type: class virual function
  Release: ?size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  Debug:   ?size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ
  unsigned __int64 std::basic_string<char,std::char_traits<char>,std::allocator<char>>::size(void)
  public: unsigned __int64 __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::size(void)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_size(Var Obj: STD_BASIC_STRING_OF_CVCHAR): UNSIGNED___INT64; overload; external opencv_delphi_dll
  name '?size@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA_KXZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 171
  Type: class virual function
  Release: ?substr@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV12@_K0@Z
  Debug:   ?substr@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV12@_K0@Z
  std::basic_string<char,std::char_traits<char>,std::allocator<char>> std::basic_string<char,std::char_traits<char>,std::allocator<char>>::substr(unsigned __int64,unsigned __int64)
  public: class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::substr(unsigned __int64,unsigned __int64)const __ptr64
}
function class_virt_func_STD_BASIC_STRING_OF_CVCHAR_substr(Var Obj: STD_BASIC_STRING_OF_CVCHAR; a: UNSIGNED___INT64; b: UNSIGNED___INT64)
  : STD_BASIC_STRING_OF_CVCHAR; overload; external opencv_delphi_dll
  name '?substr@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEBA?AV12@_K0@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  opencv_delphi480 172
  Type: class virual procedure
  Release: ?swap@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@@Z
  Debug:   ?swap@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@@Z
  void std::basic_string<char,std::char_traits<char>,std::allocator<char>>::swap(std::basic_string<char,std::char_traits<char>,std::allocator<char>> &)
  public: void __cdecl std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>>::swap(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char>> & __ptr64) __ptr64
}
procedure class_virt_proc_STD_BASIC_STRING_OF_CVCHAR_swap(Var Obj: STD_BASIC_STRING_OF_CVCHAR; var a: STD_BASIC_STRING_OF_CVCHAR); overload;
  external opencv_delphi_dll
  name '?swap@?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@QEAAXAEAV12@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(*
  opencv_delphi480 10400 {!!!!}
  Type: non class C++ procedure
  Release: ?dataStdVector@@YAPEAXPEAXH@Z
  Debug:   ?dataStdVector@@YAPEAXPEAXH@Z
  void * __ptr64 __cdecl dataStdVector(void * __ptr64,int)
*)

function dataStdVector(Obj: pVector; vt: TVectorType): pointer; external opencv_delphi_dll
  name '?dataStdVector@@YAPEAXPEAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


{$IFEND EXTERNAL_TYPES}
