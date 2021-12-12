// ??0GMat@cv@@QEAA@$$QEAV01@@Z	??0GMat@cv@@QEAA@$$QEAV01@@Z	cv::GMat::GMat(class cv::GMat &&)
// ??0GMat@cv@@QEAA@AEBV01@@Z	??0GMat@cv@@QEAA@AEBV01@@Z	cv::GMat::GMat(class cv::GMat const &)
// ??0GMat@cv@@QEAA@AEBVGNode@1@_K@Z	??0GMat@cv@@QEAA@AEBVGNode@1@_K@Z	cv::GMat::GMat(class cv::GNode const &,unsigned __int64)
{
  ??0GMat@cv@@QEAA@XZ
  ??0GMat@cv@@QEAA@XZ
  cv::GMat::GMat(void)
}
procedure constructor_GMat(Obj: pGMat); external opencv_world_dll name '??0GMat@cv@@QEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??4GMat@cv@@QEAAAEAV01@$$QEAV01@@Z	??4GMat@cv@@QEAAAEAV01@$$QEAV01@@Z	class cv::GMat & cv::GMat::operator=(class cv::GMat &&)
// ??4GMat@cv@@QEAAAEAV01@AEBV01@@Z	??4GMat@cv@@QEAAAEAV01@AEBV01@@Z	class cv::GMat & cv::GMat::operator=(class cv::GMat const &)
// ??8cv@@YA?AVGMat@0@AEBV10@0@Z	??8cv@@YA?AVGMat@0@AEBV10@0@Z	class cv::GMat cv::operator==(class cv::GMat const &,class cv::GMat const &)
// ??8cv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??8cv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator==(class cv::GMat const &,class cv::GScalar const &)
// ??8cv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??8cv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator==(class cv::GScalar const &,class cv::GMat const &)
// ??9cv@@YA?AVGMat@0@AEBV10@0@Z	??9cv@@YA?AVGMat@0@AEBV10@0@Z	class cv::GMat cv::operator!=(class cv::GMat const &,class cv::GMat const &)
// ??9cv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??9cv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator!=(class cv::GMat const &,class cv::GScalar const &)
// ??9cv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??9cv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator!=(class cv::GScalar const &,class cv::GMat const &)
// ??Dcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Dcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator*(class cv::GMat const &,class cv::GScalar const &)
{
  ??Dcv@@YA?AVGMat@0@AEBV10@M@Z
  ??Dcv@@YA?AVGMat@0@AEBV10@M@Z
  class cv::GMat cv::operator*(class cv::GMat const &,float)
}
procedure operator_mul_float_GMat(const a: TGMat; b: float; r: pGMat); overload; external opencv_world_dll name '??Dcv@@YA?AVGMat@0@AEBV10@M@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??Dcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Dcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator*(class cv::GScalar const &,class cv::GMat const &)
{
  2823
  ??Dcv@@YA?AVGMat@0@MAEBV10@@Z
  ??Dcv@@YA?AVGMat@0@MAEBV10@@Z
  class cv::GMat cv::operator*(float,class cv::GMat const &)
}
procedure operator_mul_float_GMat(a: float; const b: TGMat; r: pGMat); overload; external opencv_world_dll name '??Dcv@@YA?AVGMat@0@MAEBV10@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  2848
  ??Gcv@@YA?AVGMat@0@AEBV10@0@Z
  ??Gcv@@YA?AVGMat@0@AEBV10@0@Z
  class cv::GMat cv::operator-(class cv::GMat const &,class cv::GMat const &)
}
procedure operator_sub_GMat(const a, b: TGMat; r: pGMat); external opencv_world_dll name '??Gcv@@YA?AVGMat@0@AEBV10@0@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??Gcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Gcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator-(class cv::GMat const &,class cv::GScalar const &)
// ??Gcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Gcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator-(class cv::GScalar const &,class cv::GMat const &)
{
  2866
  ??Hcv@@YA?AVGMat@0@AEBV10@0@Z
  ??Hcv@@YA?AVGMat@0@AEBV10@0@Z
  class cv::GMat cv::operator+(class cv::GMat const &,class cv::GMat const &)
}
procedure operator_add_GMat(const a, b: TGMat; r: pGMat); external opencv_world_dll name '??Hcv@@YA?AVGMat@0@AEBV10@0@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??Hcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Hcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator+(class cv::GMat const &,class cv::GScalar const &)
// ??Hcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Hcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator+(class cv::GScalar const &,class cv::GMat const &)
{
  ??Icv@@YA?AVGMat@0@AEBV10@0@Z
  ??Icv@@YA?AVGMat@0@AEBV10@0@Z
  class cv::GMat cv::operator&(class cv::GMat const &,class cv::GMat const &)
}
procedure operator_and_GMat(const a, b: TGMat; r: pGMat); external opencv_world_dll name '??Icv@@YA?AVGMat@0@AEBV10@0@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??Icv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Icv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator&(class cv::GMat const &,class cv::GScalar const &)
// ??Icv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Icv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator&(class cv::GScalar const &,class cv::GMat const &)
{
  ??Kcv@@YA?AVGMat@0@AEBV10@0@Z
  ??Kcv@@YA?AVGMat@0@AEBV10@0@Z
  class cv::GMat cv::operator/(class cv::GMat const &,class cv::GMat const &)
}
procedure operator_div_GMat(const a, b: TGMat; r: pGMat); external opencv_world_dll name '??Kcv@@YA?AVGMat@0@AEBV10@0@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??Kcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Kcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator/(class cv::GMat const &,class cv::GScalar const &)
// ??Kcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Kcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator/(class cv::GScalar const &,class cv::GMat const &)
// ??Mcv@@YA?AVGMat@0@AEBV10@0@Z	??Mcv@@YA?AVGMat@0@AEBV10@0@Z	class cv::GMat cv::operator<(class cv::GMat const &,class cv::GMat const &)
// ??Mcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Mcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator<(class cv::GMat const &,class cv::GScalar const &)
// ??Mcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Mcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator<(class cv::GScalar const &,class cv::GMat const &)
// ??Ncv@@YA?AVGMat@0@AEBV10@0@Z	??Ncv@@YA?AVGMat@0@AEBV10@0@Z	class cv::GMat cv::operator<=(class cv::GMat const &,class cv::GMat const &)
// ??Ncv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Ncv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator<=(class cv::GMat const &,class cv::GScalar const &)
// ??Ncv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Ncv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator<=(class cv::GScalar const &,class cv::GMat const &)
{
  ??Ocv@@YA?AVGMat@0@AEBV10@0@Z
  ??Ocv@@YA?AVGMat@0@AEBV10@0@Z
  class cv::GMat cv::operator>(class cv::GMat const &,class cv::GMat const &)
}
procedure operator_great_GMat(const a, b: TGMat; r: pGMat); overload; external opencv_world_dll name '??Ocv@@YA?AVGMat@0@AEBV10@0@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
  ??Ocv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z
  ??Ocv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z
  class cv::GMat cv::operator>(class cv::GMat const &,class cv::GScalar const &)
}
procedure operator_great_GMat(const a: TGMat; const b: TGScalar; r: pGMat); overload; external opencv_world_dll name '??Ocv@@YA?AVGMat@0@AEBV10@0@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??Ocv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Ocv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator>(class cv::GScalar const &,class cv::GMat const &)
// ??Pcv@@YA?AVGMat@0@AEBV10@0@Z	??Pcv@@YA?AVGMat@0@AEBV10@0@Z	class cv::GMat cv::operator>=(class cv::GMat const &,class cv::GMat const &)
// ??Pcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Pcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator>=(class cv::GMat const &,class cv::GScalar const &)
// ??Pcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Pcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator>=(class cv::GScalar const &,class cv::GMat const &)
// ??Scv@@YA?AVGMat@0@AEBV10@@Z	??Scv@@YA?AVGMat@0@AEBV10@@Z	class cv::GMat cv::operator~(class cv::GMat const &)
// ??Tcv@@YA?AVGMat@0@AEBV10@0@Z	??Tcv@@YA?AVGMat@0@AEBV10@0@Z	class cv::GMat cv::operator^(class cv::GMat const &,class cv::GMat const &)
// ??Tcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Tcv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator^(class cv::GMat const &,class cv::GScalar const &)
// ??Tcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Tcv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator^(class cv::GScalar const &,class cv::GMat const &)
// ??Ucv@@YA?AVGMat@0@AEBV10@0@Z	??Ucv@@YA?AVGMat@0@AEBV10@0@Z	class cv::GMat cv::operator|(class cv::GMat const &,class cv::GMat const &)
// ??Ucv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	??Ucv@@YA?AVGMat@0@AEBV10@AEBVGScalar@0@@Z	class cv::GMat cv::operator|(class cv::GMat const &,class cv::GScalar const &)
// ??Ucv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	??Ucv@@YA?AVGMat@0@AEBVGScalar@0@AEBV10@@Z	class cv::GMat cv::operator|(class cv::GScalar const &,class cv::GMat const &)
