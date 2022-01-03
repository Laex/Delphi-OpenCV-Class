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

// ??0MatExpr@cv@@QEAA@$$QEAV01@@Z public: __cdecl cv::MatExpr::MatExpr(class cv::MatExpr && __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@AEBV01@@Z   public: __cdecl cv::MatExpr::MatExpr(class cv::MatExpr const & __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@AEBVMat@1@@Z        public: __cdecl cv::MatExpr::MatExpr(class cv::Mat const & __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@PEBVMatOp@1@HAEBVMat@1@11NNAEBV?$Scalar_@N@1@@Z     public: __cdecl cv::MatExpr::MatExpr(class cv::MatOp const * __ptr64,int,class cv::Mat const & __ptr64,class cv::Mat const & __ptr64,class cv::Mat const & __ptr64,double,double,class cv::Scalar_<double> const & __ptr64) __ptr64

// ??0MatExpr@cv@@QEAA@XZ
// public: __cdecl cv::MatExpr::MatExpr(void) __ptr64
procedure constructor_MatExpr(Var Obj: TMatExpr); overload; external opencv_world_dll name '??0MatExpr@cv@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??1MatExpr@cv@@QEAA@XZ
// public: __cdecl cv::MatExpr::~MatExpr(void) __ptr64
procedure Destructor_MatExpr(Var Obj: TMatExpr); overload; external opencv_world_dll name '??1MatExpr@cv@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??4MatExpr@cv@@QEAAAEAV01@$$QEAV01@@Z   public: class cv::MatExpr & __ptr64 __cdecl cv::MatExpr::operator=(class cv::MatExpr && __ptr64) __ptr64
{
  2270
  ??4MatExpr@cv@@QEAAAEAV01@AEBV01@@Z
  public: class cv::MatExpr & __ptr64 __cdecl cv::MatExpr::operator=(class cv::MatExpr const & __ptr64) __ptr64
}
procedure Operator_MatExpr_Assign_Const_MatExpr(Var Obj: TMatExpr; const m: TMatExpr); overload; external opencv_world_dll name '??4MatExpr@cv@@QEAAAEAV01@AEBV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??RMatExpr@cv@@QEBA?AV01@AEBV?$Rect_@H@1@@Z     public: class cv::MatExpr __cdecl cv::MatExpr::operator()(class cv::Rect_<int> const & __ptr64)const __ptr64
// ??RMatExpr@cv@@QEBA?AV01@AEBVRange@1@0@Z        public: class cv::MatExpr __cdecl cv::MatExpr::operator()(class cv::Range const & __ptr64,class cv::Range const & __ptr64)const __ptr64
// ?col@MatExpr@cv@@QEBA?AV12@H@Z  public: class cv::MatExpr __cdecl cv::MatExpr::col(int)const __ptr64
// ?cross@MatExpr@cv@@QEBA?AVMat@2@AEBV32@@Z       public: class cv::Mat __cdecl cv::MatExpr::cross(class cv::Mat const & __ptr64)const __ptr64
// ?diag@MatExpr@cv@@QEBA?AV12@H@Z public: class cv::MatExpr __cdecl cv::MatExpr::diag(int)const __ptr64
// ?dot@MatExpr@cv@@QEBANAEBVMat@2@@Z      public: double __cdecl cv::MatExpr::dot(class cv::Mat const & __ptr64)const __ptr64
// ?inv@MatExpr@cv@@QEBA?AV12@H@Z  public: class cv::MatExpr __cdecl cv::MatExpr::inv(int)const __ptr64
// ?mul@MatExpr@cv@@QEBA?AV12@AEBV12@N@Z   public: class cv::MatExpr __cdecl cv::MatExpr::mul(class cv::MatExpr const & __ptr64,double)const __ptr64
// ?mul@MatExpr@cv@@QEBA?AV12@AEBVMat@2@N@Z        public: class cv::MatExpr __cdecl cv::MatExpr::mul(class cv::Mat const & __ptr64,double)const __ptr64
// ?row@MatExpr@cv@@QEBA?AV12@H@Z  public: class cv::MatExpr __cdecl cv::MatExpr::row(int)const __ptr64

// 6473
// ?size@MatExpr@cv@@QEBA?AV?$Size_@H@2@XZ
// public: class cv::Size_<int> __cdecl cv::MatExpr::size(void)const __ptr64
procedure MatExpr_size(const Obj: TMatExpr; const p: TSize); external opencv_world_dll
// index 6473
  name '?size@MatExpr@cv@@QEBA?AV?$Size_@H@2@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?swap@MatExpr@cv@@QEAAXAEAV12@@Z        public: void __cdecl cv::MatExpr::swap(class cv::MatExpr & __ptr64) __ptr64
// ?t@MatExpr@cv@@QEBA?AV12@XZ     public: class cv::MatExpr __cdecl cv::MatExpr::t(void)const __ptr64
// ?type@MatExpr@cv@@QEBAHXZ       public: int __cdecl cv::MatExpr::type(void)const __ptr64

{
  2979
  ??Scv@@YA?AVMatExpr@0@AEBVMat@0@@Z
  class cv::MatExpr cv::operator~(class cv::Mat const &)
}
 procedure operator_MatExpr_LogicalNot_Mat(Var Obj: TMatExpr; const m: TMat); //
  external opencv_world_dll                               //
  name '??Scv@@YA?AVMatExpr@0@AEBVMat@0@@Z'               //
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  2860
  ??Gcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z
  class cv::MatExpr cv::operator-(class cv::Mat const &,class cv::Scalar_<double> const &)
}
procedure operator_MatExpr_Subtract_Mat_MatExpr(Var Obj: TMatExpr; const m: TMat; const s: TScalar); external opencv_world_dll //
  name '??Gcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
2932
??Pcv@@YA?AVMatExpr@0@AEBVMat@0@N@Z
??Pcv@@YA?AVMatExpr@0@AEBVMat@0@N@Z
class cv::MatExpr cv::operator>=(class cv::Mat const &,double)
}
procedure operator_MatExpr_GreaterThanOrEqual_double(Var Obj: TMatExpr; const m: TMat; const d: double);
external opencv_world_dll name '??Pcv@@YA?AVMatExpr@0@AEBVMat@0@N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
2826
??Dcv@@YA?AVMatExpr@0@AEBV10@N@Z
??Dcv@@YA?AVMatExpr@0@AEBV10@N@Z
class cv::MatExpr cv::operator*(class cv::MatExpr const &,double)
}
procedure operator_MatExpr_Mul_MatExpr_double(Var Obj: TMatExpr; const m: TMatExpr; const d: double);
external opencv_world_dll name '??Dcv@@YA?AVMatExpr@0@AEBV10@N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//2827	??Dcv@@YA?AVMatExpr@0@AEBVMat@0@0@Z	??Dcv@@YA?AVMatExpr@0@AEBVMat@0@0@Z	class cv::MatExpr cv::operator*(class cv::Mat const &,class cv::Mat const &)
//2828	??Dcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV10@@Z	??Dcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV10@@Z	class cv::MatExpr cv::operator*(class cv::Mat const &,class cv::MatExpr const &)
{
2829
??Dcv@@YA?AVMatExpr@0@AEBVMat@0@N@Z
??Dcv@@YA?AVMatExpr@0@AEBVMat@0@N@Z
class cv::MatExpr cv::operator*(class cv::Mat const &,double)
}
procedure operator_MatExpr_Mul_Mat_double(Var Obj: TMatExpr; const m: TMat; const d: double);
external opencv_world_dll name '??Dcv@@YA?AVMatExpr@0@AEBVMat@0@N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//2830	??Dcv@@YA?AVMatExpr@0@NAEBV10@@Z	??Dcv@@YA?AVMatExpr@0@NAEBV10@@Z	class cv::MatExpr cv::operator*(double,class cv::MatExpr const &)
//2831	??Dcv@@YA?AVMatExpr@0@NAEBVMat@0@@Z	??Dcv@@YA?AVMatExpr@0@NAEBVMat@0@@Z	class cv::MatExpr cv::operator*(double,class cv::Mat const &)

//2869	??Hcv@@YA?AVMatExpr@0@AEBV10@0@Z	??Hcv@@YA?AVMatExpr@0@AEBV10@0@Z	class cv::MatExpr cv::operator+(class cv::MatExpr const &,class cv::MatExpr const &)
//2870	??Hcv@@YA?AVMatExpr@0@AEBV10@AEBV?$Scalar_@N@0@@Z	??Hcv@@YA?AVMatExpr@0@AEBV10@AEBV?$Scalar_@N@0@@Z	class cv::MatExpr cv::operator+(class cv::MatExpr const &,class cv::Scalar_<double> const &)
//2871	??Hcv@@YA?AVMatExpr@0@AEBV10@AEBVMat@0@@Z	??Hcv@@YA?AVMatExpr@0@AEBV10@AEBVMat@0@@Z	class cv::MatExpr cv::operator+(class cv::MatExpr const &,class cv::Mat const &)
//2872	??Hcv@@YA?AVMatExpr@0@AEBV?$Scalar_@N@0@AEBV10@@Z	??Hcv@@YA?AVMatExpr@0@AEBV?$Scalar_@N@0@AEBV10@@Z	class cv::MatExpr cv::operator+(class cv::Scalar_<double> const &,class cv::MatExpr const &)
//2873	??Hcv@@YA?AVMatExpr@0@AEBV?$Scalar_@N@0@AEBVMat@0@@Z	??Hcv@@YA?AVMatExpr@0@AEBV?$Scalar_@N@0@AEBVMat@0@@Z	class cv::MatExpr cv::operator+(class cv::Scalar_<double> const &,class cv::Mat const &)
//2874	??Hcv@@YA?AVMatExpr@0@AEBVMat@0@0@Z	??Hcv@@YA?AVMatExpr@0@AEBVMat@0@0@Z	class cv::MatExpr cv::operator+(class cv::Mat const &,class cv::Mat const &)
//2875	??Hcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV10@@Z	??Hcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV10@@Z	class cv::MatExpr cv::operator+(class cv::Mat const &,class cv::MatExpr const &)
{
2876
??Hcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z
??Hcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z
class cv::MatExpr cv::operator+(class cv::Mat const &,class cv::Scalar_<double> const &)
}
procedure operator_MatExpr_Add_Mat_Scalar(Var Obj: TMatExpr; const m: TMat; const d: TScalar);
external opencv_world_dll name '??Hcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//2883	??Icv@@YA?AVMatExpr@0@AEBVMat@0@0@Z	??Icv@@YA?AVMatExpr@0@AEBVMat@0@0@Z	class cv::MatExpr cv::operator&(class cv::Mat const &,class cv::Mat const &)
{
2884
??Icv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z
??Icv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z
class cv::MatExpr cv::operator&(class cv::Mat const &,class cv::Scalar_<double> const &)
}
procedure operator_MatExpr_BitwiseAnd_Mat_Scalar(Var Obj: TMatExpr; const m: TMat; const d: TScalar);
external opencv_world_dll name '??Icv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

