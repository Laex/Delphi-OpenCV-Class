// ??0Mat@cv@@QEAA@AEBV01@@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pMat; a: pMat); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@AEBV?$Rect_@H@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class cv::Rect_<int> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pMat; a: pMat; b: pRect); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@AEBV?$Rect_@H@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@AEBV?$vector@VRange@cv@@V?$allocator@VRange@cv@@@std@@@std@@@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class std::vector<class cv::Range,class std::allocator<class cv::Range> > const & __ptr64) __ptr64
// procedure Constructor_Mat(Obj: pMat; a: pMat; b: TCVectorPointer); overload;
// external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@AEBV?$vector@VRange@cv@@V?$allocator@VRange@cv@@@std@@@std@@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@AEBVRange@1@1@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class cv::Range const & __ptr64,class cv::Range const & __ptr64) __ptr64
// procedure Constructor_Mat(Obj: pMat; a: pMat; b: TCVRangePointer; c: TCVRangePointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@AEBVRange@1@1@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@PEBVRange@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class cv::Range const * __ptr64) __ptr64
// procedure Constructor_Mat(Obj: pMat; a: pMat; b: TCVRangePointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@PEBVRange@1@@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@H@Z
// public: __cdecl cv::Mat::Mat(class std::vector<int,class std::allocator<int> > const & __ptr64,int) __ptr64
// procedure Constructor_Mat(Obj: pMat; a: TCVectorPointer; b: int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@H@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(class std::vector<int,class std::allocator<int> > const & __ptr64,int,class cv::Scalar_<double> const & __ptr64) __ptr64
// procedure Constructor_Mat(Obj: pMat; a: TCVectorPointer; b: int; c: TCVScalarPointer); overload;
// external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HAEBV?$Scalar_@N@1@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HPEAXPEB_K@Z
// public: __cdecl cv::Mat::Mat(class std::vector<int,class std::allocator<int> > const & __ptr64,int,void * __ptr64,unsigned __int64 const * __ptr64) __ptr64
// procedure Constructor_Mat(Obj: pMat; a: TCVectorPointer; b: int; c: Pointer; d: UInt64); overload;
// external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HPEAXPEB_K@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBVGpuMat@cuda@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::cuda::GpuMat const & __ptr64) __ptr64
// procedure Constructor_Mat(Obj: pMat; a: TCVGpuMatPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBVGpuMat@cuda@1@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HHH@Z
// public: __cdecl cv::Mat::Mat(int,int,int) __ptr64
procedure Constructor_Mat(Obj: pMat; a: int; b: int; c: int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HHHAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(int,int,int,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pMat; a: int; b: int; c: int; d: pScalar); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HHHAEBV?$Scalar_@N@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HHHPEAX_K@Z
// public: __cdecl cv::Mat::Mat(int,int,int,void * __ptr64,unsigned __int64) __ptr64
// procedure
function Constructor_Mat(Obj: pMat; a: int; b: int; c: int; d: Pointer; e: UInt64): pMat; overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HHHPEAX_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HPEBHH@Z
// public: __cdecl cv::Mat::Mat(int,int const * __ptr64,int) __ptr64
procedure Constructor_Mat(Obj: pMat; a: int; b: pInt; c: int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HPEBHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HPEBHHAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(int,int const * __ptr64,int,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pMat; a: int; b: pInt; c: int; d: pScalar); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HPEBHHAEBV?$Scalar_@N@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??0Mat@cv@@QEAA@HPEBHHPEAXPEB_K@Z
// public: __cdecl cv::Mat::Mat(int,int const * __ptr64,int,void * __ptr64,unsigned __int64 const * __ptr64) __ptr64
procedure Constructor_Mat_Pointer(Obj: pMat; a: int; b: pInt; c: int; d: Pointer; e: UInt64); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HPEBHHPEAXPEB_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@V?$Size_@H@1@H@Z
// public: __cdecl cv::Mat::Mat(class cv::Size_<int>,int) __ptr64
procedure Constructor_Mat(Obj: pMat; a: rSize; b: int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@V?$Size_@H@1@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@V?$Size_@H@1@HAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::Size_<int>,int,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pMat; a: rSize; b: int; c: pScalar); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@V?$Size_@H@1@HAEBV?$Scalar_@N@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@V?$Size_@H@1@HPEAX_K@Z
// public: __cdecl cv::Mat::Mat(class cv::Size_<int>,int,void * __ptr64,unsigned __int64) __ptr64
procedure Constructor_Mat(Obj: pMat; a: rSize; b: int; c: Pointer; d: UInt64); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@V?$Size_@H@1@HPEAX_K@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??0Mat@cv@@QEAA@XZ
// public: __cdecl cv::Mat::Mat(void) __ptr64
procedure Constructor_Mat(Obj: pMat); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??1Mat@cv@@QEAA@XZ
// public: __cdecl cv::Mat::~Mat(void) __ptr64
procedure Destructor_Mat(Obj: pMat); overload; external opencv_world_dll name '??1Mat@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?addref@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::addref(void) __ptr64
procedure addref_Mat(Obj: pMat); overload; external opencv_world_dll name '?addref@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?adjustROI@Mat@cv@@QEAAAEAV12@HHHH@Z
// public: class cv::Mat & __ptr64 __cdecl cv::Mat::adjustROI(int,int,int,int) __ptr64
procedure adjustROI(Obj: pMat; a: int; b: int; c: int; d: int); overload; external opencv_world_dll name '?adjustROI@Mat@cv@@QEAAAEAV12@HHHH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?assignTo@Mat@cv@@QEBAXAEAV12@H@Z
// public: void __cdecl cv::Mat::assignTo(class cv::Mat & __ptr64,int)const __ptr64
procedure assignTo_Mat(Obj: pMat; a: pMat; b: int); overload; external opencv_world_dll name '?assignTo@Mat@cv@@QEBAXAEAV12@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?channels@Mat@cv@@QEBAHXZ
// public: int __cdecl cv::Mat::channels(void)const __ptr64
function channels_Mat(Obj: pMat): int; overload; external opencv_world_dll name '?channels@Mat@cv@@QEBAHXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?checkVector@Mat@cv@@QEBAHHH_N@Z
// public: int __cdecl cv::Mat::checkVector(int,int,bool)const __ptr64
function checkVector_Mat(Obj: pMat; a: int; b: int; c: Bool): int; overload; external opencv_world_dll name '?checkVector@Mat@cv@@QEBAHHH_N@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?clone@Mat@cv@@QEBA?AV12@XZ
// public: class cv::Mat __cdecl cv::Mat::clone(void)const __ptr64
// function clone(const Obj: pCVMatPointer): pCVMatPointer; overload; external opencv_world name '?clone@Mat@cv@@QEBA?AV12@XZ' {{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF}};
procedure clone_Mat(const Obj: pMat; r: pMat);
// function clone_Mat(const Obj: pMat): TMat; //
external opencv_world_dll name '?clone@Mat@cv@@QEBA?AV12@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?col@Mat@cv@@QEBA?AV12@H@Z
// public: class cv::Mat __cdecl cv::Mat::col(int)const __ptr64
function col_Mat(Obj: pMat; a: int): pMat; overload; external opencv_world_dll name '?col@Mat@cv@@QEBA?AV12@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?colRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z
// public: class cv::Mat __cdecl cv::Mat::colRange(class cv::Range const & __ptr64)const __ptr64
// function colRange_Mat(Obj: pMat; a: TCVRangePointer): pMat; overload; external opencv_world_dll name '?colRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?colRange@Mat@cv@@QEBA?AV12@HH@Z
// public: class cv::Mat __cdecl cv::Mat::colRange(int,int)const __ptr64
function colRange_Mat(Obj: pMat; a: int; b: int): pMat; overload; external opencv_world_dll name '?colRange@Mat@cv@@QEBA?AV12@HH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// 4046
// ?convertTo@Mat@cv@@QEBAXAEBV_OutputArray@2@HNN@Z
// public: void __cdecl cv::Mat::convertTo(class cv::_OutputArray const & __ptr64,int,double,double)const __ptr64
procedure convertTo_Mat(Obj: pMat; a: pOutputArray; b: int; c: double; d: double); overload; external opencv_world_dll index 4046 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?copySize@Mat@cv@@QEAAXAEBV12@@Z
// public: void __cdecl cv::Mat::copySize(class cv::Mat const & __ptr64) __ptr64
procedure copySize_Mat(Obj: pMat; a: pMat); overload; external opencv_world_dll name '?copySize@Mat@cv@@QEAAXAEBV12@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@@Z
// public: void __cdecl cv::Mat::copyTo(class cv::_OutputArray const & __ptr64)const __ptr64
// 4077
// ?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@@Z
// void cv::Mat::copyTo(class cv::_OutputArray const &)
procedure copyTo_Mat(Obj: pMat; a: pOutputArray); overload; external opencv_world_dll index 4077 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@AEBV_InputArray@2@@Z
// public: void __cdecl cv::Mat::copyTo(class cv::_OutputArray const & __ptr64,class cv::_InputArray const & __ptr64)const __ptr64
// 4078
// ?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@AEBV_InputArray@2@@Z
// void cv::Mat::copyTo(class cv::_OutputArray const &,class cv::_InputArray const &)
procedure copyTo_Mat(Obj: pMat; a: pOutputArray; b: pInputArray); overload; external opencv_world_dll index 4078 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXAEBV?$vector@HV?$allocator@H@std@@@std@@H@Z
// public: void __cdecl cv::Mat::create(class std::vector<int,class std::allocator<int> > const & __ptr64,int) __ptr64
// procedure create_Mat(Obj: pMat; a: TCVectorPointer; b: int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXAEBV?$vector@HV?$allocator@H@std@@@std@@H@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXHHH@Z
// public: void __cdecl cv::Mat::create(int,int,int) __ptr64
procedure create_Mat(Obj: pMat; a: int; b: int; c: int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXHHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXHPEBHH@Z
// public: void __cdecl cv::Mat::create(int,int const * __ptr64,int) __ptr64
procedure create_Mat(Obj: pMat; a: int; b: pInt; c: int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXHPEBHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXV?$Size_@H@2@H@Z
// public: void __cdecl cv::Mat::create(class cv::Size_<int>,int) __ptr64
procedure create_Mat(Obj: pMat; a: pSize; b: int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXV?$Size_@H@2@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?cross@Mat@cv@@QEBA?AV12@AEBV_InputArray@2@@Z
// public: class cv::Mat __cdecl cv::Mat::cross(class cv::_InputArray const & __ptr64)const __ptr64
function cross_Mat(Obj: pMat; a: pInputArray): pMat; overload; external opencv_world_dll name '?cross@Mat@cv@@QEBA?AV12@AEBV_InputArray@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?deallocate@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::deallocate(void) __ptr64
procedure deallocate_Mat(Obj: pMat); overload; external opencv_world_dll name '?deallocate@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?depth@Mat@cv@@QEBAHXZ
// public: int __cdecl cv::Mat::depth(void)const __ptr64
function depth_Mat(Obj: pMat): int; overload; external opencv_world_dll name '?depth@Mat@cv@@QEBAHXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?diag@Mat@cv@@QEBA?AV12@H@Z
// public: class cv::Mat __cdecl cv::Mat::diag(int)const __ptr64
// function diag(Obj: pCVMatPointer; a: int): pCVMatPointer; overload; external opencv_world name '?diag@Mat@cv@@QEBA?AV12@H@Z'{ {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF}};
procedure diag_Mat(Obj: pMat; a: pMat; d: int); overload; external opencv_world_dll name '?diag@Mat@cv@@QEBA?AV12@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?diag@Mat@cv@@SA?AV12@AEBV12@@Z
// public: static class cv::Mat __cdecl cv::Mat::diag(class cv::Mat const & __ptr64)
procedure diag_Mat(Obj: pMat; a: pMat); overload; external opencv_world_dll name '?diag@Mat@cv@@SA?AV12@AEBV12@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?dot@Mat@cv@@QEBANAEBV_InputArray@2@@Z
// public: double __cdecl cv::Mat::dot(class cv::_InputArray const & __ptr64)const __ptr64
function dot_Mat(Obj: pMat; a: pInputArray): double; overload; external opencv_world_dll name '?dot@Mat@cv@@QEBANAEBV_InputArray@2@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?elemSize1@Mat@cv@@QEBA_KXZ
// public: unsigned __int64 __cdecl cv::Mat::elemSize1(void)const __ptr64
function elemSize1_Mat(Obj: pMat): UInt64; overload; external opencv_world_dll name '?elemSize1@Mat@cv@@QEBA_KXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?elemSize@Mat@cv@@QEBA_KXZ
// public: unsigned __int64 __cdecl cv::Mat::elemSize(void)const __ptr64
function elemSize_Mat(Obj: pMat): UInt64; overload; external opencv_world_dll name '?elemSize@Mat@cv@@QEBA_KXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?empty@Mat@cv@@QEBA_NXZ
// public: bool __cdecl cv::Mat::empty(void)const __ptr64
function empty_Mat(Obj: pMat): Bool; overload; external opencv_world_dll name '?empty@Mat@cv@@QEBA_NXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?eye@Mat@cv@@SA?AVMatExpr@2@HHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::eye(int,int,int)
function eye_Mat(Obj: pMat; a: int; b: int; c: int): pMatExpr; overload; external opencv_world_dll name '?eye@Mat@cv@@SA?AVMatExpr@2@HHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?eye@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z
// public: static class cv::MatExpr __cdecl cv::Mat::eye(class cv::Size_<int>,int)
function eye_Mat(Obj: pMat; a: pSize; b: int): pMatExpr; overload; external opencv_world_dll name '?eye@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?getDefaultAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ
// public: static class cv::MatAllocator * __ptr64 __cdecl cv::Mat::getDefaultAllocator(void)
// function getDefaultAllocator_Mat(Obj: pMat): pCVMatAllocator; overload; external opencv_world_dll name '?getDefaultAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?getStdAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ
// public: static class cv::MatAllocator * __ptr64 __cdecl cv::Mat::getStdAllocator(void)
// function getStdAllocator_Mat(Obj: pMat): pCVMatAllocator; overload; external opencv_world_dll name '?getStdAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?getUMat@Mat@cv@@QEBA?AVUMat@2@W4AccessFlag@2@W4UMatUsageFlags@2@@Z
// public: class cv::UMat __cdecl cv::Mat::getUMat(enum cv::AccessFlag,enum cv::UMatUsageFlags)const __ptr64
// function getUMat_Mat(Obj: pMat; a: TCVAccessFlagEnum; b: TCVUMatUsageFlagsEnum): TCVUMatPointer; overload;
// external opencv_world_dll name '?getUMat@Mat@cv@@QEBA?AVUMat@2@W4AccessFlag@2@W4UMatUsageFlags@2@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?inv@Mat@cv@@QEBA?AVMatExpr@2@H@Z public: class cv::MatExpr __cdecl cv::Mat::inv(int)const __ptr64
function inv_Mat(Obj: pMat; a: int): pMatExpr; overload; external opencv_world_dll name '?inv@Mat@cv@@QEBA?AVMatExpr@2@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?isContinuous@Mat@cv@@QEBA_NXZ  public: bool __cdecl cv::Mat::isContinuous(void)const __ptr64
function isContinuous_Mat(Obj: pMat): Bool; overload; external opencv_world_dll name '?isContinuous@Mat@cv@@QEBA_NXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?isSubmatrix@Mat@cv@@QEBA_NXZ  public: bool __cdecl cv::Mat::isSubmatrix(void)const __ptr64
function isSubmatrix_Mat(Obj: pMat): Bool; overload; external opencv_world_dll name '?isSubmatrix@Mat@cv@@QEBA_NXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?locateROI@Mat@cv@@QEBAXAEAV?$Size_@H@2@AEAV?$Point_@H@2@@Z   public: void __cdecl cv::Mat::locateROI(class cv::Size_<int> & __ptr64,class cv::Point_<int> & __ptr64)const __ptr64
procedure locateROI_Mat(Obj: pMat; a: pSize; b: pPoint); overload; external opencv_world_dll name '?locateROI@Mat@cv@@QEBAXAEAV?$Size_@H@2@AEAV?$Point_@H@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?mul@Mat@cv@@QEBA?AVMatExpr@2@AEBV_InputArray@2@N@Z   public: class cv::MatExpr __cdecl cv::Mat::mul(class cv::_InputArray const & __ptr64,double)const __ptr64
function mul_Mat(Obj: pMat; a: pInputArray; b: double): pMatExpr; overload; external opencv_world_dll name '?mul@Mat@cv@@QEBA?AVMatExpr@2@AEBV_InputArray@2@N@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ones@Mat@cv@@SA?AVMatExpr@2@HHH@Z   public: static class cv::MatExpr __cdecl cv::Mat::ones(int,int,int)
procedure ones_Mat(Obj: pMatExpr; a: int; b: int; c: int); overload; external opencv_world_dll index 5776 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ones@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::ones(int,int const * __ptr64,int)
procedure ones_Mat(Obj: pMat; a: int; b: pInt; c: int); overload; external opencv_world_dll name '?ones@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ones@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z
// public: static class cv::MatExpr __cdecl cv::Mat::ones(class cv::Size_<int>,int)
function ones_Mat(Obj: pMat; a: pSize; b: int): pMatExpr; overload; external opencv_world_dll name '?ones@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?pop_back@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::pop_back(unsigned __int64) __ptr64
procedure pop_back_Mat(Obj: pMat; a: UInt64); overload; external opencv_world_dll name '?pop_back@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int) __ptr64
function ptr_Mat(Obj: pMat; a: int): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEHH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int,int) __ptr64
function ptr_Mat(Obj: pMat; a: int; b: int): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEHHH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int,int,int) __ptr64
function ptr_Mat(Obj: pMat; a: int; b: int; c: int): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEHHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEPEBH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int const * __ptr64) __ptr64
function ptr_Mat(Obj: pMat; a: pInt): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEPEBH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int)const __ptr64
function ptr0_Mat(Obj: pMat; a: int): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEHH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int,int)const __ptr64
function ptr1_Mat(Obj: pMat; a: int; b: int): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEHHH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int,int,int)const __ptr64
function ptr2_Mat(Obj: pMat; a: int; b: int; c: int): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEHHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEPEBH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int const * __ptr64)const __ptr64
function ptr3_Mat(Obj: pMat; a: pInt): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEPEBH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?push_back@Mat@cv@@QEAAXAEBV12@@Z
// public: void __cdecl cv::Mat::push_back(class cv::Mat const & __ptr64) __ptr64
procedure push_back_Mat(Obj: pMat; a: pMat); overload; external opencv_world_dll name '?push_back@Mat@cv@@QEAAXAEBV12@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?push_back_@Mat@cv@@QEAAXPEBX@Z
// public: void __cdecl cv::Mat::push_back_(void const * __ptr64) __ptr64
procedure push_back_Mat_(Obj: pMat; a: PointerConst); overload; external opencv_world_dll name '?push_back_@Mat@cv@@QEAAXPEBX@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?release@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::release(void) __ptr64
procedure release_Mat(Obj: pMat); overload; external opencv_world_dll name '?release@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?reserve@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::reserve(unsigned __int64) __ptr64
procedure reserve_Mat(Obj: pMat; a: UInt64); overload; external opencv_world_dll name '?reserve@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?reserveBuffer@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::reserveBuffer(unsigned __int64) __ptr64
procedure reserveBuffer_Mat(Obj: pMat; a: UInt64); overload; external opencv_world_dll name '?reserveBuffer@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?reshape@Mat@cv@@QEBA?AV12@HAEBV?$vector@HV?$allocator@H@std@@@std@@@Z
// public: class cv::Mat __cdecl cv::Mat::reshape(int,class std::vector<int,class std::allocator<int> > const & __ptr64)const __ptr64
// function reshape_Mat(Obj: pMat; a: int; b: TCVectorPointer): pMat; overload; external opencv_world_dll name '?reshape@Mat@cv@@QEBA?AV12@HAEBV?$vector@HV?$allocator@H@std@@@std@@@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?reshape@Mat@cv@@QEBA?AV12@HH@Z
// public: class cv::Mat __cdecl cv::Mat::reshape(int,int)const __ptr64
function reshape_Mat(Obj: pMat; a: int; b: int): pMat; overload; external opencv_world_dll name '?reshape@Mat@cv@@QEBA?AV12@HH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?reshape@Mat@cv@@QEBA?AV12@HHPEBH@Z
// public: class cv::Mat __cdecl cv::Mat::reshape(int,int,int const * __ptr64)const __ptr64
function reshape_Mat(Obj: pMat; a: int; b: int; c: pInt): pMat; overload; external opencv_world_dll name '?reshape@Mat@cv@@QEBA?AV12@HHPEBH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?resize@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::resize(unsigned __int64) __ptr64
procedure resize_Mat(Obj: pMat; a: UInt64); overload; external opencv_world_dll name '?resize@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?resize@Mat@cv@@QEAAX_KAEBV?$Scalar_@N@2@@Z
// public: void __cdecl cv::Mat::resize(unsigned __int64,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure resize_Mat(Obj: pMat; a: UInt64; b: pScalar); overload; external opencv_world_dll name '?resize@Mat@cv@@QEAAX_KAEBV?$Scalar_@N@2@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?row@Mat@cv@@QEBA?AV12@H@Z
// public: class cv::Mat __cdecl cv::Mat::row(int)const __ptr64
function row_Mat(Obj: pMat; a: int): pMat; overload; external opencv_world_dll name '?row@Mat@cv@@QEBA?AV12@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?rowRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z
// public: class cv::Mat __cdecl cv::Mat::rowRange(class cv::Range const & __ptr64)const __ptr64
// function rowRange_Mat(Obj: pMat; a: TCVRangePointer): pMat; overload; external opencv_world_dll name '?rowRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?rowRange@Mat@cv@@QEBA?AV12@HH@Z
// public: class cv::Mat __cdecl cv::Mat::rowRange(int,int)const __ptr64
function rowRange_Mat(Obj: pMat; a: int; b: int): pMat; overload; external opencv_world_dll name '?rowRange@Mat@cv@@QEBA?AV12@HH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?setDefaultAllocator@Mat@cv@@SAXPEAVMatAllocator@2@@Z
// public: static void __cdecl cv::Mat::setDefaultAllocator(class cv::MatAllocator * __ptr64)
// procedure setDefaultAllocator_Mat(Obj: pMat; a: pCVMatAllocator); overload; external opencv_world_dll name '?setDefaultAllocator@Mat@cv@@SAXPEAVMatAllocator@2@@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?setTo@Mat@cv@@QEAAAEAV12@AEBV_InputArray@2@0@Z
// public: class cv::Mat & __ptr64 __cdecl cv::Mat::setTo(class cv::_InputArray const & __ptr64,class cv::_InputArray const & __ptr64) __ptr64
// function setTo_Mat(Obj: pMat; a: TCVPointer; b: TCVPointer): pMat; overload; external opencv_world_dll name '?setTo@Mat@cv@@QEAAAEAV12@AEBV_InputArray@2@0@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?step1@Mat@cv@@QEBA_KH@Z
// public: unsigned __int64 __cdecl cv::Mat::step1(int)const __ptr64
function step1_Mat(Obj: pMat; a: int): UInt64; overload; external opencv_world_dll name '?step1@Mat@cv@@QEBA_KH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?t@Mat@cv@@QEBA?AVMatExpr@2@XZ
// public: class cv::MatExpr __cdecl cv::Mat::t(void)const __ptr64
function T_Mat(Obj: pMat): pMatExpr; overload; external opencv_world_dll name '?t@Mat@cv@@QEBA?AVMatExpr@2@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?total@Mat@cv@@QEBA_KHH@Z
// public: unsigned __int64 __cdecl cv::Mat::total(int,int)const __ptr64
function total_Mat(Obj: pMat; a: int; b: int): UInt64; overload; external opencv_world_dll name '?total@Mat@cv@@QEBA_KHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?total@Mat@cv@@QEBA_KXZ
// public: unsigned __int64 __cdecl cv::Mat::total(void)const __ptr64
function total_Mat(Obj: pMat): UInt64; overload; external opencv_world_dll name '?total@Mat@cv@@QEBA_KXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?type@Mat@cv@@QEBAHXZ
// public: int __cdecl cv::Mat::type(void)const __ptr64
function type_Mat(Obj: pMat): int; overload; external opencv_world_dll name '?type@Mat@cv@@QEBAHXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?updateContinuityFlag@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::updateContinuityFlag(void) __ptr64
procedure updateContinuityFlag_Mat(Obj: pMat); overload; external opencv_world_dll name '?updateContinuityFlag@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?zeros@Mat@cv@@SA?AVMatExpr@2@HHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::zeros(int,int,int)
procedure zeros_Mat(Obj: pMatExpr; a: int; b: int; c: int); overload; external opencv_world_dll name '?zeros@Mat@cv@@SA?AVMatExpr@2@HHH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?zeros@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::zeros(int,int const * __ptr64,int)
// function zeros_Mat(Obj: pMat; a: int; b: pInt; c: int): pMatExpr; overload; external opencv_world_dll name '?zeros@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z'
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?zeros@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z
// public: static class cv::MatExpr __cdecl cv::Mat::zeros(class cv::Size_<int>,int)
procedure zeros_Mat(Obj: pMatExpr; a: UInt64; b: int); overload; external opencv_world_dll
// name '?zeros@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z'
  index 6924 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// 2263
// ??4Mat@cv@@QEAAAEAV01@$$QEAV01@@Z	public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::Mat && __ptr64) __ptr64
{$IFDEF LOADSTUPID}

Type
  TOperator_Mat_Assign_Mat = procedure(Obj: pMat; m: pMat);

Var
  Operator_Mat_Assign_Mat: TOperator_Mat_Assign_Mat;
{$ELSE}
procedure Operator_Mat_Assign_Mat(Obj: pMat; m: pMat); external opencv_world_dll name '??4Mat@cv@@QEAAAEAV01@$$QEAV01@@Z'
// index 2263
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{$ENDIF}
// ??4Mat@cv@@QEAAAEAV01@AEBV01@@Z	public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::Mat const & __ptr64) __ptr64
procedure Operator_Mat_Assign_Const_Mat(Obj: pMat; m: pMat); overload; external opencv_world_dll name '??4Mat@cv@@QEAAAEAV01@AEBV01@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??4Mat@cv@@QEAAAEAV01@AEBV?$Scalar_@N@1@@Z	public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Operator_Mat_Assign_Scalar(Obj: pMat; me: pScalar); overload; external opencv_world_dll name '??4Mat@cv@@QEAAAEAV01@AEBV?$Scalar_@N@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??4Mat@cv@@QEAAAEAV01@AEBVMatExpr@1@@Z   public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::MatExpr const & __ptr64) __ptr64
procedure Operator_Mat_Assign_MatExpr(Obj: pMat; me: pMatExpr); overload; external opencv_world_dll name '??4Mat@cv@@QEAAAEAV01@AEBVMatExpr@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??RMat@cv@@QEBA?AV01@AEBV?$Rect_@H@1@@Z   public: class cv::Mat __cdecl cv::Mat::operator()(class cv::Rect_<int> const & __ptr64)const __ptr64
procedure Operator_Mat_RoiToMat(Obj: pMat; roi: pRect; dst: pMat); overload; external opencv_world_dll name '??RMat@cv@@QEBA?AV01@AEBV?$Rect_@H@1@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??RMat@cv@@QEBA?AV01@AEBV?$vector@VRange@cv@@V?$allocator@VRange@cv@@@std@@@std@@@Z	public: class cv::Mat __cdecl cv::Mat::operator()(class std::vector<class cv::Range,class std::allocator<class cv::Range> > const & __ptr64)const __ptr64
// ??RMat@cv@@QEBA?AV01@PEBVRange@1@@Z	public: class cv::Mat __cdecl cv::Mat::operator()(class cv::Range const * __ptr64)const __ptr64
// ??RMat@cv@@QEBA?AV01@VRange@1@0@Z	public: class cv::Mat __cdecl cv::Mat::operator()(class cv::Range,class cv::Range)const __ptr64
