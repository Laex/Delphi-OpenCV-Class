// 2979
// ??Scv@@YA?AVMatExpr@0@AEBVMat@0@@Z
// class cv::MatExpr cv::operator~(class cv::Mat const &)
procedure MatExpr_LogicalNot_Mat(Obj: pMatExpr; m: pMat); external opencv_world_dll index 2979 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// 2860
// ??Gcv@@YA?AVMatExpr@0@AEBVMat@0@AEBV?$Scalar_@N@0@@Z
// class cv::MatExpr cv::operator-(class cv::Mat const &,class cv::Scalar_<double> const &)
procedure MatExpr_Subtract_Mat_MatExpr(Obj: pMatExpr; m: pMat; s: pScalar); external opencv_world_dll index 2860 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};