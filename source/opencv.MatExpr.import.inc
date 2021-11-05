// ??0MatExpr@cv@@QEAA@$$QEAV01@@Z public: __cdecl cv::MatExpr::MatExpr(class cv::MatExpr && __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@AEBV01@@Z   public: __cdecl cv::MatExpr::MatExpr(class cv::MatExpr const & __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@AEBVMat@1@@Z        public: __cdecl cv::MatExpr::MatExpr(class cv::Mat const & __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@PEBVMatOp@1@HAEBVMat@1@11NNAEBV?$Scalar_@N@1@@Z     public: __cdecl cv::MatExpr::MatExpr(class cv::MatOp const * __ptr64,int,class cv::Mat const & __ptr64,class cv::Mat const & __ptr64,class cv::Mat const & __ptr64,double,double,class cv::Scalar_<double> const & __ptr64) __ptr64

// ??0MatExpr@cv@@QEAA@XZ
// public: __cdecl cv::MatExpr::MatExpr(void) __ptr64
procedure constructor_MatExpr(Obj: pMatExpr); overload; external opencv_world_dll index 787 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??1MatExpr@cv@@QEAA@XZ
// public: __cdecl cv::MatExpr::~MatExpr(void) __ptr64
procedure Destructor_MatExpr(Obj: pMatExpr); overload; external opencv_world_dll index 1649 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??4MatExpr@cv@@QEAAAEAV01@$$QEAV01@@Z   public: class cv::MatExpr & __ptr64 __cdecl cv::MatExpr::operator=(class cv::MatExpr && __ptr64) __ptr64
// ??4MatExpr@cv@@QEAAAEAV01@AEBV01@@Z     public: class cv::MatExpr & __ptr64 __cdecl cv::MatExpr::operator=(class cv::MatExpr const & __ptr64) __ptr64
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
function MatExpr_size(Obj: pMatExpr; p: pSize): pSize; external opencv_world_dll index 6473 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?swap@MatExpr@cv@@QEAAXAEAV12@@Z        public: void __cdecl cv::MatExpr::swap(class cv::MatExpr & __ptr64) __ptr64
// ?t@MatExpr@cv@@QEBA?AV12@XZ     public: class cv::MatExpr __cdecl cv::MatExpr::t(void)const __ptr64
// ?type@MatExpr@cv@@QEBAHXZ       public: int __cdecl cv::MatExpr::type(void)const __ptr64
