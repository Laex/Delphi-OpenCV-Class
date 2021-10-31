unit OpenCV.Import;

{$I opencv_delphi.inc}

interface

const
  version           = '454';
  opencv_delphi_dll = 'opencv_delphi' + version + {$IFDEF DEBUG} 'd' + {$ENDIF} '.dll';
  opencv_world_dll  = 'opencv_world' + version + {$IFDEF DEBUG} 'd' + {$ENDIF} '.dll';

Type
  BOOL = LongBool;
  size_t = NativeUInt;
  psize_t = ^size_t;
  Int = integer;
  pInt = ^Int;

type
  TCVMat = record // 96 bytes, v4.5.4
    Dummy: array [1 .. 96] of Byte;
  end;

  TCVScalar = record
    Dummy: array [1 .. 32] of Byte;
  end;

type
  TCVScalarPointer = type Pointer;
  TCVMatAllocatorPointer = type Pointer;
  pUChar = type pByte;
  TCVUMatUsageFlagsEnum = type integer;
  TCVGpuMatPointer = type Pointer;
  TCVAccessFlagEnum = type integer;
  pUCharConst = type pByte;
  TCVMatExprPointer = type Pointer;
  TCVPointPointer = type Pointer;
  PointerConst = type Pointer;
  pMatAllocator = TCVMatAllocatorPointer;
  pUMatData = type Pointer;
  pCVMatPointer = ^TCVMat;
  pCVMat = pCVMatPointer;

  TCVRectPointer = type Pointer;
  TCVUMatPointer = type Pointer;
  TCVSizePointer = type Pointer;
  TCVvectorPointer = type Pointer;
  TCVPointer = type Pointer;
  TCVRangePointer = type Pointer;

  TCVInputArrayPointer = type Pointer;
  TCVOutputArrayPointer = type Pointer;
  TCVInputOutputArrayPointer = type Pointer;

  pCVMatOp = type Pointer;
  TCVMatSizePointer = type Pointer;

  { --------------- opencv_world --------------- }

  { --------------- Start Mat --------------- }
  // ??0Mat@cv@@QEAA@AEBV01@@Z
  // public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: pCVMatPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@AEBV?$Rect_@H@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class cv::Rect_<int> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: pCVMatPointer; b: TCVRectPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@AEBV?$Rect_@H@1@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@AEBV?$vector@VRange@cv@@V?$allocator@VRange@cv@@@std@@@std@@@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class std::vector<class cv::Range,class std::allocator<class cv::Range> > const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: pCVMatPointer; b: TCVvectorPointer); overload;
  external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@AEBV?$vector@VRange@cv@@V?$allocator@VRange@cv@@@std@@@std@@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@AEBVRange@1@1@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class cv::Range const & __ptr64,class cv::Range const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: pCVMatPointer; b: TCVRangePointer; c: TCVRangePointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@AEBVRange@1@1@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV01@PEBVRange@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::Mat const & __ptr64,class cv::Range const * __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: pCVMatPointer; b: TCVRangePointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV01@PEBVRange@1@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@H@Z
// public: __cdecl cv::Mat::Mat(class std::vector<int,class std::allocator<int> > const & __ptr64,int) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: TCVvectorPointer; b: Int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@H@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(class std::vector<int,class std::allocator<int> > const & __ptr64,int,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: TCVvectorPointer; b: Int; c: TCVScalarPointer); overload;
  external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HAEBV?$Scalar_@N@1@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HPEAXPEB_K@Z
// public: __cdecl cv::Mat::Mat(class std::vector<int,class std::allocator<int> > const & __ptr64,int,void * __ptr64,unsigned __int64 const * __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: TCVvectorPointer; b: Int; c: Pointer; d: UInt64); overload;
  external opencv_world_dll name '??0Mat@cv@@QEAA@AEBV?$vector@HV?$allocator@H@std@@@std@@HPEAXPEB_K@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@AEBVGpuMat@cuda@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::cuda::GpuMat const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: TCVGpuMatPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@AEBVGpuMat@cuda@1@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HHH@Z
// public: __cdecl cv::Mat::Mat(int,int,int) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: Int; b: Int; c: Int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HHHAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(int,int,int,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: Int; b: Int; c: Int; d: TCVScalarPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HHHAEBV?$Scalar_@N@1@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HHHPEAX_K@Z
// public: __cdecl cv::Mat::Mat(int,int,int,void * __ptr64,unsigned __int64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: Int; b: Int; c: Int; d: Pointer; e: UInt64); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HHHPEAX_K@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HPEBHH@Z
// public: __cdecl cv::Mat::Mat(int,int const * __ptr64,int) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: Int; b: pInt; c: Int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HPEBHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HPEBHHAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(int,int const * __ptr64,int,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: Int; b: pInt; c: Int; d: TCVScalarPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HPEBHHAEBV?$Scalar_@N@1@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@HPEBHHPEAXPEB_K@Z
// public: __cdecl cv::Mat::Mat(int,int const * __ptr64,int,void * __ptr64,unsigned __int64 const * __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: Int; b: pInt; c: Int; d: Pointer; e: UInt64); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@HPEBHHPEAXPEB_K@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@V?$Size_@H@1@H@Z
// public: __cdecl cv::Mat::Mat(class cv::Size_<int>,int) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: TCVSizePointer; b: Int); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@V?$Size_@H@1@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@V?$Size_@H@1@HAEBV?$Scalar_@N@1@@Z
// public: __cdecl cv::Mat::Mat(class cv::Size_<int>,int,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: TCVSizePointer; b: Int; c: TCVScalarPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@V?$Size_@H@1@HAEBV?$Scalar_@N@1@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@V?$Size_@H@1@HPEAX_K@Z
// public: __cdecl cv::Mat::Mat(class cv::Size_<int>,int,void * __ptr64,unsigned __int64) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer; a: TCVSizePointer; b: Int; c: Pointer; d: UInt64); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@V?$Size_@H@1@HPEAX_K@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Mat@cv@@QEAA@XZ
// public: __cdecl cv::Mat::Mat(void) __ptr64
procedure Constructor_Mat(Obj: pCVMatPointer); overload; external opencv_world_dll name '??0Mat@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??1Mat@cv@@QEAA@XZ
// public: __cdecl cv::Mat::~Mat(void) __ptr64
procedure Destructor_Mat(Obj: pCVMatPointer); overload; external opencv_world_dll name '??1Mat@cv@@QEAA@XZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?addref@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::addref(void) __ptr64
procedure addref(Obj: pCVMatPointer); overload; external opencv_world_dll name '?addref@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?adjustROI@Mat@cv@@QEAAAEAV12@HHHH@Z
// public: class cv::Mat & __ptr64 __cdecl cv::Mat::adjustROI(int,int,int,int) __ptr64
function adjustROI(Obj: pCVMatPointer; a: Int; b: Int; c: Int; d: Int): pCVMatPointer; overload; external opencv_world_dll name '?adjustROI@Mat@cv@@QEAAAEAV12@HHHH@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?assignTo@Mat@cv@@QEBAXAEAV12@H@Z
// public: void __cdecl cv::Mat::assignTo(class cv::Mat & __ptr64,int)const __ptr64
procedure assignTo(Obj: pCVMatPointer; a: pCVMatPointer; b: Int); overload; external opencv_world_dll name '?assignTo@Mat@cv@@QEBAXAEAV12@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?channels@Mat@cv@@QEBAHXZ
// public: int __cdecl cv::Mat::channels(void)const __ptr64
function channels(Obj: pCVMatPointer): Int; overload; external opencv_world_dll name '?channels@Mat@cv@@QEBAHXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?checkVector@Mat@cv@@QEBAHHH_N@Z
// public: int __cdecl cv::Mat::checkVector(int,int,bool)const __ptr64
function checkVector(Obj: pCVMatPointer; a: Int; b: Int; c: BOOL): Int; overload; external opencv_world_dll name '?checkVector@Mat@cv@@QEBAHHH_N@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?clone@Mat@cv@@QEBA?AV12@XZ
// public: class cv::Mat __cdecl cv::Mat::clone(void)const __ptr64
// function clone(const Obj: pCVMatPointer): pCVMatPointer; overload; external opencv_world name '?clone@Mat@cv@@QEBA?AV12@XZ' {{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF}};
procedure clone(const Obj: pCVMatPointer; r: pCVMatPointer); overload; external opencv_world_dll name '?clone@Mat@cv@@QEBA?AV12@XZ' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?col@Mat@cv@@QEBA?AV12@H@Z
// public: class cv::Mat __cdecl cv::Mat::col(int)const __ptr64
function col(Obj: pCVMatPointer; a: Int): pCVMatPointer; overload; external opencv_world_dll name '?col@Mat@cv@@QEBA?AV12@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?colRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z
// public: class cv::Mat __cdecl cv::Mat::colRange(class cv::Range const & __ptr64)const __ptr64
function colRange(Obj: pCVMatPointer; a: TCVRangePointer): pCVMatPointer; overload; external opencv_world_dll name '?colRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?colRange@Mat@cv@@QEBA?AV12@HH@Z
// public: class cv::Mat __cdecl cv::Mat::colRange(int,int)const __ptr64
function colRange(Obj: pCVMatPointer; a: Int; b: Int): pCVMatPointer; overload; external opencv_world_dll name '?colRange@Mat@cv@@QEBA?AV12@HH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?convertTo@Mat@cv@@QEBAXAEBV_OutputArray@2@HNN@Z
// public: void __cdecl cv::Mat::convertTo(class cv::_OutputArray const & __ptr64,int,double,double)const __ptr64
procedure convertTo(Obj: pCVMatPointer; a: TCVPointer; b: Int; c: double; d: double); overload; external opencv_world_dll name '?convertTo@Mat@cv@@QEBAXAEBV_OutputArray@2@HNN@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?copySize@Mat@cv@@QEAAXAEBV12@@Z
// public: void __cdecl cv::Mat::copySize(class cv::Mat const & __ptr64) __ptr64
procedure copySize(Obj: pCVMatPointer; a: pCVMatPointer); overload; external opencv_world_dll name '?copySize@Mat@cv@@QEAAXAEBV12@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@@Z
// public: void __cdecl cv::Mat::copyTo(class cv::_OutputArray const & __ptr64)const __ptr64
procedure copyTo(Obj: pCVMatPointer; a: TCVPointer); overload; external opencv_world_dll name '?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@AEBV_InputArray@2@@Z
// public: void __cdecl cv::Mat::copyTo(class cv::_OutputArray const & __ptr64,class cv::_InputArray const & __ptr64)const __ptr64
procedure copyTo(Obj: pCVMatPointer; a: TCVPointer; b: TCVPointer); overload; external opencv_world_dll name '?copyTo@Mat@cv@@QEBAXAEBV_OutputArray@2@AEBV_InputArray@2@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXAEBV?$vector@HV?$allocator@H@std@@@std@@H@Z
// public: void __cdecl cv::Mat::create(class std::vector<int,class std::allocator<int> > const & __ptr64,int) __ptr64
procedure create(Obj: pCVMatPointer; a: TCVvectorPointer; b: Int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXAEBV?$vector@HV?$allocator@H@std@@@std@@H@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXHHH@Z
// public: void __cdecl cv::Mat::create(int,int,int) __ptr64
procedure create(Obj: pCVMatPointer; a: Int; b: Int; c: Int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXHHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXHPEBHH@Z
// public: void __cdecl cv::Mat::create(int,int const * __ptr64,int) __ptr64
procedure create(Obj: pCVMatPointer; a: Int; b: pInt; c: Int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXHPEBHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?create@Mat@cv@@QEAAXV?$Size_@H@2@H@Z
// public: void __cdecl cv::Mat::create(class cv::Size_<int>,int) __ptr64
procedure create(Obj: pCVMatPointer; a: TCVSizePointer; b: Int); overload; external opencv_world_dll name '?create@Mat@cv@@QEAAXV?$Size_@H@2@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?cross@Mat@cv@@QEBA?AV12@AEBV_InputArray@2@@Z
// public: class cv::Mat __cdecl cv::Mat::cross(class cv::_InputArray const & __ptr64)const __ptr64
function cross(Obj: pCVMatPointer; a: TCVPointer): pCVMatPointer; overload; external opencv_world_dll name '?cross@Mat@cv@@QEBA?AV12@AEBV_InputArray@2@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?deallocate@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::deallocate(void) __ptr64
procedure deallocate(Obj: pCVMatPointer); overload; external opencv_world_dll name '?deallocate@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?depth@Mat@cv@@QEBAHXZ
// public: int __cdecl cv::Mat::depth(void)const __ptr64
function depth(Obj: pCVMatPointer): Int; overload; external opencv_world_dll name '?depth@Mat@cv@@QEBAHXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ?diag@Mat@cv@@QEBA?AV12@H@Z
// public: class cv::Mat __cdecl cv::Mat::diag(int)const __ptr64
// function diag(Obj: pCVMatPointer; a: int): pCVMatPointer; overload; external opencv_world name '?diag@Mat@cv@@QEBA?AV12@H@Z'{ {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF}};
procedure diag(Obj: pCVMatPointer; a: pCVMatPointer; d: Int); overload; external opencv_world_dll name '?diag@Mat@cv@@QEBA?AV12@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?diag@Mat@cv@@SA?AV12@AEBV12@@Z
// public: static class cv::Mat __cdecl cv::Mat::diag(class cv::Mat const & __ptr64)
procedure diag(Obj: pCVMatPointer; a: pCVMatPointer); overload; external opencv_world_dll name '?diag@Mat@cv@@SA?AV12@AEBV12@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ?dot@Mat@cv@@QEBANAEBV_InputArray@2@@Z
// public: double __cdecl cv::Mat::dot(class cv::_InputArray const & __ptr64)const __ptr64
function dot(Obj: pCVMatPointer; a: TCVPointer): double; overload; external opencv_world_dll name '?dot@Mat@cv@@QEBANAEBV_InputArray@2@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?elemSize1@Mat@cv@@QEBA_KXZ
// public: unsigned __int64 __cdecl cv::Mat::elemSize1(void)const __ptr64
function elemSize1(Obj: pCVMatPointer): UInt64; overload; external opencv_world_dll name '?elemSize1@Mat@cv@@QEBA_KXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?elemSize@Mat@cv@@QEBA_KXZ
// public: unsigned __int64 __cdecl cv::Mat::elemSize(void)const __ptr64
function elemSize(Obj: pCVMatPointer): UInt64; overload; external opencv_world_dll name '?elemSize@Mat@cv@@QEBA_KXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?empty@Mat@cv@@QEBA_NXZ
// public: bool __cdecl cv::Mat::empty(void)const __ptr64
function empty(Obj: pCVMatPointer): BOOL; overload; external opencv_world_dll name '?empty@Mat@cv@@QEBA_NXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?eye@Mat@cv@@SA?AVMatExpr@2@HHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::eye(int,int,int)
function eye(Obj: pCVMatPointer; a: Int; b: Int; c: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?eye@Mat@cv@@SA?AVMatExpr@2@HHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?eye@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z
// public: static class cv::MatExpr __cdecl cv::Mat::eye(class cv::Size_<int>,int)
function eye(Obj: pCVMatPointer; a: TCVSizePointer; b: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?eye@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?getDefaultAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ
// public: static class cv::MatAllocator * __ptr64 __cdecl cv::Mat::getDefaultAllocator(void)
function getDefaultAllocator(Obj: pCVMatPointer): TCVMatAllocatorPointer; overload; external opencv_world_dll name '?getDefaultAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?getStdAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ
// public: static class cv::MatAllocator * __ptr64 __cdecl cv::Mat::getStdAllocator(void)
function getStdAllocator(Obj: pCVMatPointer): TCVMatAllocatorPointer; overload; external opencv_world_dll name '?getStdAllocator@Mat@cv@@SAPEAVMatAllocator@2@XZ'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?getUMat@Mat@cv@@QEBA?AVUMat@2@W4AccessFlag@2@W4UMatUsageFlags@2@@Z
// public: class cv::UMat __cdecl cv::Mat::getUMat(enum cv::AccessFlag,enum cv::UMatUsageFlags)const __ptr64
function getUMat(Obj: pCVMatPointer; a: TCVAccessFlagEnum; b: TCVUMatUsageFlagsEnum): TCVUMatPointer; overload;
  external opencv_world_dll name '?getUMat@Mat@cv@@QEBA?AVUMat@2@W4AccessFlag@2@W4UMatUsageFlags@2@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?inv@Mat@cv@@QEBA?AVMatExpr@2@H@Z
// public: class cv::MatExpr __cdecl cv::Mat::inv(int)const __ptr64
function inv(Obj: pCVMatPointer; a: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?inv@Mat@cv@@QEBA?AVMatExpr@2@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?isContinuous@Mat@cv@@QEBA_NXZ
// public: bool __cdecl cv::Mat::isContinuous(void)const __ptr64
function isContinuous(Obj: pCVMatPointer): BOOL; overload; external opencv_world_dll name '?isContinuous@Mat@cv@@QEBA_NXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?isSubmatrix@Mat@cv@@QEBA_NXZ
// public: bool __cdecl cv::Mat::isSubmatrix(void)const __ptr64
function isSubmatrix(Obj: pCVMatPointer): BOOL; overload; external opencv_world_dll name '?isSubmatrix@Mat@cv@@QEBA_NXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?locateROI@Mat@cv@@QEBAXAEAV?$Size_@H@2@AEAV?$Point_@H@2@@Z
// public: void __cdecl cv::Mat::locateROI(class cv::Size_<int> & __ptr64,class cv::Point_<int> & __ptr64)const __ptr64
procedure locateROI(Obj: pCVMatPointer; a: TCVSizePointer; b: TCVPointPointer); overload; external opencv_world_dll name '?locateROI@Mat@cv@@QEBAXAEAV?$Size_@H@2@AEAV?$Point_@H@2@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?mul@Mat@cv@@QEBA?AVMatExpr@2@AEBV_InputArray@2@N@Z
// public: class cv::MatExpr __cdecl cv::Mat::mul(class cv::_InputArray const & __ptr64,double)const __ptr64
function mul(Obj: pCVMatPointer; a: TCVPointer; b: double): TCVMatExprPointer; overload; external opencv_world_dll name '?mul@Mat@cv@@QEBA?AVMatExpr@2@AEBV_InputArray@2@N@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ones@Mat@cv@@SA?AVMatExpr@2@HHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::ones(int,int,int)
function ones(Obj: pCVMatPointer; a: Int; b: Int; c: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?ones@Mat@cv@@SA?AVMatExpr@2@HHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ones@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::ones(int,int const * __ptr64,int)
function ones(Obj: pCVMatPointer; a: Int; b: pInt; c: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?ones@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ones@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z
// public: static class cv::MatExpr __cdecl cv::Mat::ones(class cv::Size_<int>,int)
function ones(Obj: pCVMatPointer; a: TCVSizePointer; b: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?ones@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?pop_back@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::pop_back(unsigned __int64) __ptr64
procedure pop_back(Obj: pCVMatPointer; a: UInt64); overload; external opencv_world_dll name '?pop_back@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int) __ptr64
function ptr(Obj: pCVMatPointer; a: Int): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEHH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int,int) __ptr64
function ptr(Obj: pCVMatPointer; a: Int; b: Int): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEHHH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int,int,int) __ptr64
function ptr(Obj: pCVMatPointer; a: Int; b: Int; c: Int): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEHHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEAAPEAEPEBH@Z
// public: unsigned char * __ptr64 __cdecl cv::Mat::ptr(int const * __ptr64) __ptr64
function ptr(Obj: pCVMatPointer; a: pInt): pUChar; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEAAPEAEPEBH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int)const __ptr64
function ptr0(Obj: pCVMatPointer; a: Int): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEHH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int,int)const __ptr64
function ptr1(Obj: pCVMatPointer; a: Int; b: Int): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEHHH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int,int,int)const __ptr64
function ptr2(Obj: pCVMatPointer; a: Int; b: Int; c: Int): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEHHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?ptr@Mat@cv@@QEBAPEBEPEBH@Z
// public: unsigned char const * __ptr64 __cdecl cv::Mat::ptr(int const * __ptr64)const __ptr64
function ptr3(Obj: pCVMatPointer; a: pInt): pUCharConst; overload; external opencv_world_dll name '?ptr@Mat@cv@@QEBAPEBEPEBH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?push_back@Mat@cv@@QEAAXAEBV12@@Z
// public: void __cdecl cv::Mat::push_back(class cv::Mat const & __ptr64) __ptr64
procedure push_back(Obj: pCVMatPointer; a: pCVMatPointer); overload; external opencv_world_dll name '?push_back@Mat@cv@@QEAAXAEBV12@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?push_back_@Mat@cv@@QEAAXPEBX@Z
// public: void __cdecl cv::Mat::push_back_(void const * __ptr64) __ptr64
procedure push_back_(Obj: pCVMatPointer; a: PointerConst); overload; external opencv_world_dll name '?push_back_@Mat@cv@@QEAAXPEBX@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?release@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::release(void) __ptr64
procedure release(Obj: pCVMatPointer); overload; external opencv_world_dll name '?release@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?reserve@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::reserve(unsigned __int64) __ptr64
procedure reserve(Obj: pCVMatPointer; a: UInt64); overload; external opencv_world_dll name '?reserve@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?reserveBuffer@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::reserveBuffer(unsigned __int64) __ptr64
procedure reserveBuffer(Obj: pCVMatPointer; a: UInt64); overload; external opencv_world_dll name '?reserveBuffer@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?reshape@Mat@cv@@QEBA?AV12@HAEBV?$vector@HV?$allocator@H@std@@@std@@@Z
// public: class cv::Mat __cdecl cv::Mat::reshape(int,class std::vector<int,class std::allocator<int> > const & __ptr64)const __ptr64
function reshape(Obj: pCVMatPointer; a: Int; b: TCVvectorPointer): pCVMatPointer; overload; external opencv_world_dll name '?reshape@Mat@cv@@QEBA?AV12@HAEBV?$vector@HV?$allocator@H@std@@@std@@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?reshape@Mat@cv@@QEBA?AV12@HH@Z
// public: class cv::Mat __cdecl cv::Mat::reshape(int,int)const __ptr64
function reshape(Obj: pCVMatPointer; a: Int; b: Int): pCVMatPointer; overload; external opencv_world_dll name '?reshape@Mat@cv@@QEBA?AV12@HH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?reshape@Mat@cv@@QEBA?AV12@HHPEBH@Z
// public: class cv::Mat __cdecl cv::Mat::reshape(int,int,int const * __ptr64)const __ptr64
function reshape(Obj: pCVMatPointer; a: Int; b: Int; c: pInt): pCVMatPointer; overload; external opencv_world_dll name '?reshape@Mat@cv@@QEBA?AV12@HHPEBH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?resize@Mat@cv@@QEAAX_K@Z
// public: void __cdecl cv::Mat::resize(unsigned __int64) __ptr64
procedure resize(Obj: pCVMatPointer; a: UInt64); overload; external opencv_world_dll name '?resize@Mat@cv@@QEAAX_K@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?resize@Mat@cv@@QEAAX_KAEBV?$Scalar_@N@2@@Z
// public: void __cdecl cv::Mat::resize(unsigned __int64,class cv::Scalar_<double> const & __ptr64) __ptr64
procedure resize(Obj: pCVMatPointer; a: UInt64; b: TCVScalarPointer); overload; external opencv_world_dll name '?resize@Mat@cv@@QEAAX_KAEBV?$Scalar_@N@2@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?row@Mat@cv@@QEBA?AV12@H@Z
// public: class cv::Mat __cdecl cv::Mat::row(int)const __ptr64
function row(Obj: pCVMatPointer; a: Int): pCVMatPointer; overload; external opencv_world_dll name '?row@Mat@cv@@QEBA?AV12@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?rowRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z
// public: class cv::Mat __cdecl cv::Mat::rowRange(class cv::Range const & __ptr64)const __ptr64
function rowRange(Obj: pCVMatPointer; a: TCVRangePointer): pCVMatPointer; overload; external opencv_world_dll name '?rowRange@Mat@cv@@QEBA?AV12@AEBVRange@2@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?rowRange@Mat@cv@@QEBA?AV12@HH@Z
// public: class cv::Mat __cdecl cv::Mat::rowRange(int,int)const __ptr64
function rowRange(Obj: pCVMatPointer; a: Int; b: Int): pCVMatPointer; overload; external opencv_world_dll name '?rowRange@Mat@cv@@QEBA?AV12@HH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?setDefaultAllocator@Mat@cv@@SAXPEAVMatAllocator@2@@Z
// public: static void __cdecl cv::Mat::setDefaultAllocator(class cv::MatAllocator * __ptr64)
procedure setDefaultAllocator(Obj: pCVMatPointer; a: TCVMatAllocatorPointer); overload; external opencv_world_dll name '?setDefaultAllocator@Mat@cv@@SAXPEAVMatAllocator@2@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?setTo@Mat@cv@@QEAAAEAV12@AEBV_InputArray@2@0@Z
// public: class cv::Mat & __ptr64 __cdecl cv::Mat::setTo(class cv::_InputArray const & __ptr64,class cv::_InputArray const & __ptr64) __ptr64
function setTo(Obj: pCVMatPointer; a: TCVPointer; b: TCVPointer): pCVMatPointer; overload; external opencv_world_dll name '?setTo@Mat@cv@@QEAAAEAV12@AEBV_InputArray@2@0@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?step1@Mat@cv@@QEBA_KH@Z
// public: unsigned __int64 __cdecl cv::Mat::step1(int)const __ptr64
function step1(Obj: pCVMatPointer; a: Int): UInt64; overload; external opencv_world_dll name '?step1@Mat@cv@@QEBA_KH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?t@Mat@cv@@QEBA?AVMatExpr@2@XZ
// public: class cv::MatExpr __cdecl cv::Mat::t(void)const __ptr64
function t(Obj: pCVMatPointer): TCVMatExprPointer; overload; external opencv_world_dll name '?t@Mat@cv@@QEBA?AVMatExpr@2@XZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?total@Mat@cv@@QEBA_KHH@Z
// public: unsigned __int64 __cdecl cv::Mat::total(int,int)const __ptr64
function total(Obj: pCVMatPointer; a: Int; b: Int): UInt64; overload; external opencv_world_dll name '?total@Mat@cv@@QEBA_KHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?total@Mat@cv@@QEBA_KXZ
// public: unsigned __int64 __cdecl cv::Mat::total(void)const __ptr64
function total(Obj: pCVMatPointer): UInt64; overload; external opencv_world_dll name '?total@Mat@cv@@QEBA_KXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?type@Mat@cv@@QEBAHXZ
// public: int __cdecl cv::Mat::type(void)const __ptr64
function &type(Obj: pCVMatPointer): Int; overload; external opencv_world_dll name '?type@Mat@cv@@QEBAHXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?updateContinuityFlag@Mat@cv@@QEAAXXZ
// public: void __cdecl cv::Mat::updateContinuityFlag(void) __ptr64
procedure updateContinuityFlag(Obj: pCVMatPointer); overload; external opencv_world_dll name '?updateContinuityFlag@Mat@cv@@QEAAXXZ' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?zeros@Mat@cv@@SA?AVMatExpr@2@HHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::zeros(int,int,int)
function zeros(Obj: pCVMatPointer; a: Int; b: Int; c: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?zeros@Mat@cv@@SA?AVMatExpr@2@HHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?zeros@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z
// public: static class cv::MatExpr __cdecl cv::Mat::zeros(int,int const * __ptr64,int)
function zeros(Obj: pCVMatPointer; a: Int; b: pInt; c: Int): TCVMatExprPointer; overload; external opencv_world_dll name '?zeros@Mat@cv@@SA?AVMatExpr@2@HPEBHH@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?zeros@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z
// public: static class cv::MatExpr __cdecl cv::Mat::zeros(class cv::Size_<int>,int)
procedure zeros(Obj: pCVMatPointer; a: UInt64; b: Int) { : TCVMatExprPointer }; overload; external opencv_world_dll
// name '?zeros@Mat@cv@@SA?AVMatExpr@2@V?$Size_@H@2@H@Z'
  index 6924 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ??4Mat@cv@@QEAAAEAV01@$$QEAV01@@Z	public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::Mat && __ptr64) __ptr64
// ??4Mat@cv@@QEAAAEAV01@AEBV01@@Z	public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::Mat const & __ptr64) __ptr64
// ??4Mat@cv@@QEAAAEAV01@AEBV?$Scalar_@N@1@@Z	public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::Scalar_<double> const & __ptr64) __ptr64

// ??4Mat@cv@@QEAAAEAV01@AEBVMatExpr@1@@Z
// public: class cv::Mat & __ptr64 __cdecl cv::Mat::operator=(class cv::MatExpr const & __ptr64) __ptr64
procedure Mat_Operator_Assign(Obj: pCVMatPointer; me: TCVMatExprPointer); overload; external opencv_world_dll name '??4Mat@cv@@QEAAAEAV01@AEBVMatExpr@1@@Z'
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ??RMat@cv@@QEBA?AV01@AEBV?$Rect_@H@1@@Z	public: class cv::Mat __cdecl cv::Mat::operator()(class cv::Rect_<int> const & __ptr64)const __ptr64
// ??RMat@cv@@QEBA?AV01@AEBV?$vector@VRange@cv@@V?$allocator@VRange@cv@@@std@@@std@@@Z	public: class cv::Mat __cdecl cv::Mat::operator()(class std::vector<class cv::Range,class std::allocator<class cv::Range> > const & __ptr64)const __ptr64
// ??RMat@cv@@QEBA?AV01@PEBVRange@1@@Z	public: class cv::Mat __cdecl cv::Mat::operator()(class cv::Range const * __ptr64)const __ptr64
// ??RMat@cv@@QEBA?AV01@VRange@1@0@Z	public: class cv::Mat __cdecl cv::Mat::operator()(class cv::Range,class cv::Range)const __ptr64

{ --------------- End Mat --------------- }

{ --------------- Start InputArray --------------- }

// ??0_InputArray@cv@@QEAA@AEBN@Z
// public: __cdecl cv::_InputArray::_InputArray(double const & __ptr64) __ptr64

// ??0_InputArray@cv@@QEAA@AEBV?$vector@VGpuMat@cuda@cv@@V?$allocator@VGpuMat@cuda@cv@@@std@@@std@@@Z
// public: __cdecl cv::_InputArray::_InputArray(class std::vector<class cv::cuda::GpuMat,class std::allocator<class cv::cuda::GpuMat> > const & __ptr64) __ptr64

// ??0_InputArray@cv@@QEAA@AEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z
// public: __cdecl cv::_InputArray::_InputArray(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > const & __ptr64) __ptr64

// ??0_InputArray@cv@@QEAA@AEBV?$vector@VUMat@cv@@V?$allocator@VUMat@cv@@@std@@@std@@@Z	public: __cdecl cv::_InputArray::_InputArray(class std::vector<class cv::UMat,class std::allocator<class cv::UMat> > const & __ptr64) __ptr64
// ??0_InputArray@cv@@QEAA@AEBV?$vector@_NV?$allocator@_N@std@@@std@@@Z	public: __cdecl cv::_InputArray::_InputArray(class std::vector<bool,class std::allocator<bool> > const & __ptr64) __ptr64
// ??0_InputArray@cv@@QEAA@AEBVBuffer@ogl@1@@Z	public: __cdecl cv::_InputArray::_InputArray(class cv::ogl::Buffer const & __ptr64) __ptr64
// ??0_InputArray@cv@@QEAA@AEBVGpuMat@cuda@1@@Z	public: __cdecl cv::_InputArray::_InputArray(class cv::cuda::GpuMat const & __ptr64) __ptr64
// ??0_InputArray@cv@@QEAA@AEBVHostMem@cuda@1@@Z	public: __cdecl cv::_InputArray::_InputArray(class cv::cuda::HostMem const & __ptr64) __ptr64

// ??0_InputArray@cv@@QEAA@AEBVMat@1@@Z
// public: __cdecl cv::_InputArray::_InputArray(class cv::Mat const & __ptr64) __ptr64
function Constructor_InputArray(Obj: TCVInputArrayPointer; m: pCVMatPointer): TCVInputArrayPointer; overload; external opencv_world_dll index 1347;
// name '??0_InputArray@cv@@QEAA@AEBVMat@1@@Z' { {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF} };

// ??0_InputArray@cv@@QEAA@AEBVMatExpr@1@@Z	public: __cdecl cv::_InputArray::_InputArray(class cv::MatExpr const & __ptr64) __ptr64
// ??0_InputArray@cv@@QEAA@AEBVUMat@1@@Z	public: __cdecl cv::_InputArray::_InputArray(class cv::UMat const & __ptr64) __ptr64
// ??0_InputArray@cv@@QEAA@HPEAX@Z	public: __cdecl cv::_InputArray::_InputArray(int,void * __ptr64) __ptr64

// ??0_InputArray@cv@@QEAA@XZ
// public: __cdecl cv::_InputArray::_InputArray(void) __ptr64
function Constructor_InputArray(Obj: TCVInputArrayPointer): TCVInputArrayPointer; overload; external opencv_world_dll index 1351 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// name '??0_InputArray@cv@@QEAA@XZ' { {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF} };

// ??1_InputArray@cv@@QEAA@XZ
// public: __cdecl cv::_InputArray::~_InputArray(void) __ptr64
procedure Destructor_InputArray(Obj: TCVInputArrayPointer); overload; external opencv_world_dll index 1812 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// name '??1_InputArray@cv@@QEAA@XZ' {{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF}};

// ??4_InputArray@cv@@QEAAAEAV01@AEBV01@@Z	public: class cv::_InputArray & __ptr64 __cdecl cv::_InputArray::operator=(class cv::_InputArray const & __ptr64) __ptr64

// ?channels@_InputArray@cv@@QEBAHH@Z	public: int __cdecl cv::_InputArray::channels(int)const __ptr64
// ?cols@_InputArray@cv@@QEBAHH@Z	public: int __cdecl cv::_InputArray::cols(int)const __ptr64
// ?copyTo@_InputArray@cv@@QEBAXAEBV_OutputArray@2@@Z	public: void __cdecl cv::_InputArray::copyTo(class cv::_OutputArray const & __ptr64)const __ptr64
// ?copyTo@_InputArray@cv@@QEBAXAEBV_OutputArray@2@AEBV12@@Z	public: void __cdecl cv::_InputArray::copyTo(class cv::_OutputArray const & __ptr64,class cv::_InputArray const & __ptr64)const __ptr64
// ?depth@_InputArray@cv@@QEBAHH@Z	public: int __cdecl cv::_InputArray::depth(int)const __ptr64
// ?dims@_InputArray@cv@@QEBAHH@Z	public: int __cdecl cv::_InputArray::dims(int)const __ptr64
// ?empty@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::empty(void)const __ptr64
// ?getFlags@_InputArray@cv@@QEBAHXZ	public: int __cdecl cv::_InputArray::getFlags(void)const __ptr64
// ?getGpuMat@_InputArray@cv@@QEBA?AVGpuMat@cuda@2@XZ	public: class cv::cuda::GpuMat __cdecl cv::_InputArray::getGpuMat(void)const __ptr64
// ?getGpuMatVector@_InputArray@cv@@QEBAXAEAV?$vector@VGpuMat@cuda@cv@@V?$allocator@VGpuMat@cuda@cv@@@std@@@std@@@Z	public: void __cdecl cv::_InputArray::getGpuMatVector(class std::vector<class cv::cuda::GpuMat,class std::allocator<class cv::cuda::GpuMat> > & __ptr64)const __ptr64
// ?getMat@_InputArray@cv@@QEBA?AVMat@2@H@Z	public: class cv::Mat __cdecl cv::_InputArray::getMat(int)const __ptr64
// ?getMatVector@_InputArray@cv@@QEBAXAEAV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z	public: void __cdecl cv::_InputArray::getMatVector(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > & __ptr64)const __ptr64
// ?getMat_@_InputArray@cv@@QEBA?AVMat@2@H@Z	public: class cv::Mat __cdecl cv::_InputArray::getMat_(int)const __ptr64
// ?getOGlBuffer@_InputArray@cv@@QEBA?AVBuffer@ogl@2@XZ	public: class cv::ogl::Buffer __cdecl cv::_InputArray::getOGlBuffer(void)const __ptr64

// ?getObj@_InputArray@cv@@QEBAPEAXXZ
// public: void * __ptr64 __cdecl cv::_InputArray::getObj(void)const __ptr64
function getObj(Obj: TCVInputArrayPointer): Pointer; external opencv_world_dll index 5051 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ?getSz@_InputArray@cv@@QEBA?AV?$Size_@H@2@XZ	public: class cv::Size_<int> __cdecl cv::_InputArray::getSz(void)const __ptr64
// ?getUMat@_InputArray@cv@@QEBA?AVUMat@2@H@Z	public: class cv::UMat __cdecl cv::_InputArray::getUMat(int)const __ptr64
// ?getUMatVector@_InputArray@cv@@QEBAXAEAV?$vector@VUMat@cv@@V?$allocator@VUMat@cv@@@std@@@std@@@Z	public: void __cdecl cv::_InputArray::getUMatVector(class std::vector<class cv::UMat,class std::allocator<class cv::UMat> > & __ptr64)const __ptr64
// ?init@_InputArray@cv@@IEAAXHPEBX@Z	protected: void __cdecl cv::_InputArray::init(int,void const * __ptr64) __ptr64
// ?init@_InputArray@cv@@IEAAXHPEBXV?$Size_@H@2@@Z	protected: void __cdecl cv::_InputArray::init(int,void const * __ptr64,class cv::Size_<int>) __ptr64
// ?isContinuous@_InputArray@cv@@QEBA_NH@Z	public: bool __cdecl cv::_InputArray::isContinuous(int)const __ptr64
// ?isGpuMat@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::isGpuMat(void)const __ptr64
// ?isGpuMatVector@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::isGpuMatVector(void)const __ptr64
// ?isMat@_InputArray@cv@@QEBA_NXZ
// public: bool __cdecl cv::_InputArray::isMat(void)const __ptr64
function isMat(Obj: TCVInputArrayPointer): BOOL; external opencv_world_dll index 5383 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// name '?isMat@_InputArray@cv@@QEBA_NXZ' { {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF} };
// ?isMatVector@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::isMatVector(void)const __ptr64
// ?isMatx@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::isMatx(void)const __ptr64
// ?isSubmatrix@_InputArray@cv@@QEBA_NH@Z	public: bool __cdecl cv::_InputArray::isSubmatrix(int)const __ptr64
// ?isUMat@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::isUMat(void)const __ptr64
// ?isUMatVector@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::isUMatVector(void)const __ptr64
// ?isVector@_InputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_InputArray::isVector(void)const __ptr64
// ?kind@_InputArray@cv@@QEBA?AW4KindFlag@12@XZ	public: enum cv::_InputArray::KindFlag __cdecl cv::_InputArray::kind(void)const __ptr64
// ?offset@_InputArray@cv@@QEBA_KH@Z	public: unsigned __int64 __cdecl cv::_InputArray::offset(int)const __ptr64
// ?rows@_InputArray@cv@@QEBAHH@Z	public: int __cdecl cv::_InputArray::rows(int)const __ptr64
// ?sameSize@_InputArray@cv@@QEBA_NAEBV12@@Z	public: bool __cdecl cv::_InputArray::sameSize(class cv::_InputArray const & __ptr64)const __ptr64
// ?size@_InputArray@cv@@QEBA?AV?$Size_@H@2@H@Z	public: class cv::Size_<int> __cdecl cv::_InputArray::size(int)const __ptr64
// ?sizend@_InputArray@cv@@QEBAHPEAHH@Z	public: int __cdecl cv::_InputArray::sizend(int * __ptr64,int)const __ptr64
// ?step@_InputArray@cv@@QEBA_KH@Z	public: unsigned __int64 __cdecl cv::_InputArray::step(int)const __ptr64
// ?total@_InputArray@cv@@QEBA_KH@Z	public: unsigned __int64 __cdecl cv::_InputArray::total(int)const __ptr64
// ?type@_InputArray@cv@@QEBAHH@Z	public: int __cdecl cv::_InputArray::type(int)const __ptr64

{ --------------- End InputArray --------------- }

{ --------------- Start OutputArray --------------- }
// ??0_OutputArray@cv@@QEAA@AEAV?$vector@VGpuMat@cuda@cv@@V?$allocator@VGpuMat@cuda@cv@@@std@@@std@@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class std::vector<class cv::cuda::GpuMat,class std::allocator<class cv::cuda::GpuMat> > & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEAV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEAV?$vector@VUMat@cv@@V?$allocator@VUMat@cv@@@std@@@std@@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class std::vector<class cv::UMat,class std::allocator<class cv::UMat> > & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEAVBuffer@ogl@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::ogl::Buffer & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEAVGpuMat@cuda@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::cuda::GpuMat & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEAVHostMem@cuda@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::cuda::HostMem & __ptr64) __ptr64

// ??0_OutputArray@cv@@QEAA@AEAVMat@1@@Z
// public: __cdecl cv::_OutputArray::_OutputArray(class cv::Mat & __ptr64) __ptr64
function Constructor_OutputArray(Obj: TCVOutputArrayPointer; m: pCVMatPointer): TCVOutputArrayPointer; overload; external opencv_world_dll index 1375 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ??0_OutputArray@cv@@QEAA@AEAVUMat@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::UMat & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > const & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEBV?$vector@VUMat@cv@@V?$allocator@VUMat@cv@@@std@@@std@@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class std::vector<class cv::UMat,class std::allocator<class cv::UMat> > const & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEBVBuffer@ogl@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::ogl::Buffer const & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEBVGpuMat@cuda@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::cuda::GpuMat const & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@AEBVHostMem@cuda@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::cuda::HostMem const & __ptr64) __ptr64

// ??0_OutputArray@cv@@QEAA@AEBVMat@1@@Z
// public: __cdecl cv::_OutputArray::_OutputArray(class cv::Mat const & __ptr64) __ptr64
// function Constructor_OutputArray(Obj: TCVOutputArrayPointer; m: pCVMatPointer): TCVOutputArrayPointer; overload; external opencv_world_dll index 1382;

// ??0_OutputArray@cv@@QEAA@AEBVUMat@1@@Z	public: __cdecl cv::_OutputArray::_OutputArray(class cv::UMat const & __ptr64) __ptr64
// ??0_OutputArray@cv@@QEAA@HPEAX@Z	public: __cdecl cv::_OutputArray::_OutputArray(int,void * __ptr64) __ptr64

// ??0_OutputArray@cv@@QEAA@XZ
// public: __cdecl cv::_OutputArray::_OutputArray(void) __ptr64
function Constructor_OutputArray(Obj: TCVOutputArrayPointer): TCVOutputArrayPointer; overload; external opencv_world_dll index 1385 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??1_OutputArray@cv@@QEAA@XZ
// public: __cdecl cv::_OutputArray::~_OutputArray(void) __ptr64
procedure Destructor_OutputArray(Obj: TCVOutputArrayPointer); overload; external opencv_world_dll index 1814 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// name '??1_InputArray@cv@@QEAA@XZ' {{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF}};

// ??4_OutputArray@cv@@QEAAAEAV01@$$QEAV01@@Z	public: class cv::_OutputArray & __ptr64 __cdecl cv::_OutputArray::operator=(class cv::_OutputArray && __ptr64) __ptr64
// ??4_OutputArray@cv@@QEAAAEAV01@AEBV01@@Z	public: class cv::_OutputArray & __ptr64 __cdecl cv::_OutputArray::operator=(class cv::_OutputArray const & __ptr64) __ptr64
// ?assign@_OutputArray@cv@@QEBAXAEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z	public: void __cdecl cv::_OutputArray::assign(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > const & __ptr64)const __ptr64
// ?assign@_OutputArray@cv@@QEBAXAEBV?$vector@VUMat@cv@@V?$allocator@VUMat@cv@@@std@@@std@@@Z	public: void __cdecl cv::_OutputArray::assign(class std::vector<class cv::UMat,class std::allocator<class cv::UMat> > const & __ptr64)const __ptr64
// ?assign@_OutputArray@cv@@QEBAXAEBVMat@2@@Z	public: void __cdecl cv::_OutputArray::assign(class cv::Mat const & __ptr64)const __ptr64
// ?assign@_OutputArray@cv@@QEBAXAEBVUMat@2@@Z	public: void __cdecl cv::_OutputArray::assign(class cv::UMat const & __ptr64)const __ptr64
// ?clear@_OutputArray@cv@@QEBAXXZ	public: void __cdecl cv::_OutputArray::clear(void)const __ptr64
// ?create@_OutputArray@cv@@QEBAXHHHH_NW4DepthMask@12@@Z	public: void __cdecl cv::_OutputArray::create(int,int,int,int,bool,enum cv::_OutputArray::DepthMask)const __ptr64
// ?create@_OutputArray@cv@@QEBAXHPEBHHH_NW4DepthMask@12@@Z	public: void __cdecl cv::_OutputArray::create(int,int const * __ptr64,int,int,bool,enum cv::_OutputArray::DepthMask)const __ptr64
// ?create@_OutputArray@cv@@QEBAXV?$Size_@H@2@HH_NW4DepthMask@12@@Z	public: void __cdecl cv::_OutputArray::create(class cv::Size_<int>,int,int,bool,enum cv::_OutputArray::DepthMask)const __ptr64
// ?createSameSize@_OutputArray@cv@@QEBAXAEBV_InputArray@2@H@Z	public: void __cdecl cv::_OutputArray::createSameSize(class cv::_InputArray const & __ptr64,int)const __ptr64
// ?fixedSize@_OutputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_OutputArray::fixedSize(void)const __ptr64
// ?fixedType@_OutputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_OutputArray::fixedType(void)const __ptr64
// ?getGpuMatRef@_OutputArray@cv@@QEBAAEAVGpuMat@cuda@2@XZ	public: class cv::cuda::GpuMat & __ptr64 __cdecl cv::_OutputArray::getGpuMatRef(void)const __ptr64
// ?getGpuMatVecRef@_OutputArray@cv@@QEBAAEAV?$vector@VGpuMat@cuda@cv@@V?$allocator@VGpuMat@cuda@cv@@@std@@@std@@XZ	public: class std::vector<class cv::cuda::GpuMat,class std::allocator<class cv::cuda::GpuMat> > & __ptr64 __cdecl cv::_OutputArray::getGpuMatVecRef(void)const __ptr64
// ?getHostMemRef@_OutputArray@cv@@QEBAAEAVHostMem@cuda@2@XZ	public: class cv::cuda::HostMem & __ptr64 __cdecl cv::_OutputArray::getHostMemRef(void)const __ptr64
// ?getMatRef@_OutputArray@cv@@QEBAAEAVMat@2@H@Z	public: class cv::Mat & __ptr64 __cdecl cv::_OutputArray::getMatRef(int)const __ptr64
// ?getOGlBufferRef@_OutputArray@cv@@QEBAAEAVBuffer@ogl@2@XZ	public: class cv::ogl::Buffer & __ptr64 __cdecl cv::_OutputArray::getOGlBufferRef(void)const __ptr64
// ?getUMatRef@_OutputArray@cv@@QEBAAEAVUMat@2@H@Z	public: class cv::UMat & __ptr64 __cdecl cv::_OutputArray::getUMatRef(int)const __ptr64
// ?move@_OutputArray@cv@@QEBAXAEAVMat@2@@Z	public: void __cdecl cv::_OutputArray::move(class cv::Mat & __ptr64)const __ptr64
// ?move@_OutputArray@cv@@QEBAXAEAVUMat@2@@Z	public: void __cdecl cv::_OutputArray::move(class cv::UMat & __ptr64)const __ptr64
// ?needed@_OutputArray@cv@@QEBA_NXZ	public: bool __cdecl cv::_OutputArray::needed(void)const __ptr64
// ?release@_OutputArray@cv@@QEBAXXZ	public: void __cdecl cv::_OutputArray::release(void)const __ptr64
// ?setTo@_OutputArray@cv@@QEBAXAEBV_InputArray@2@0@Z	public: void __cdecl cv::_OutputArray::setTo(class cv::_InputArray const & __ptr64,class cv::_InputArray const & __ptr64)const __ptr64
{ --------------- End OutputArray --------------- }

{ --------------- Start InputOutputArray --------------- }
// ??0_InputOutputArray@cv@@QEAA@AEAV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z        public: __cdecl cv::_InputOutputArray::_InputOutputArray(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEAV?$vector@VUMat@cv@@V?$allocator@VUMat@cv@@@std@@@std@@@Z      public: __cdecl cv::_InputOutputArray::_InputOutputArray(class std::vector<class cv::UMat,class std::allocator<class cv::UMat> > & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEAVBuffer@ogl@1@@Z       public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::ogl::Buffer & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEAVGpuMat@cuda@1@@Z      public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::cuda::GpuMat & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEAVHostMem@cuda@1@@Z     public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::cuda::HostMem & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEAVMat@1@@Z      public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::Mat & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEAVUMat@1@@Z     public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::UMat & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEBV?$vector@VGpuMat@cuda@cv@@V?$allocator@VGpuMat@cuda@cv@@@std@@@std@@@Z        public: __cdecl cv::_InputOutputArray::_InputOutputArray(class std::vector<class cv::cuda::GpuMat,class std::allocator<class cv::cuda::GpuMat> > const & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z        public: __cdecl cv::_InputOutputArray::_InputOutputArray(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > const & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEBV?$vector@VUMat@cv@@V?$allocator@VUMat@cv@@@std@@@std@@@Z      public: __cdecl cv::_InputOutputArray::_InputOutputArray(class std::vector<class cv::UMat,class std::allocator<class cv::UMat> > const & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEBVBuffer@ogl@1@@Z       public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::ogl::Buffer const & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEBVGpuMat@cuda@1@@Z      public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::cuda::GpuMat const & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@AEBVHostMem@cuda@1@@Z     public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::cuda::HostMem const & __ptr64) __ptr64

// ??0_InputOutputArray@cv@@QEAA@AEBVMat@1@@Z
// public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::Mat const & __ptr64) __ptr64
procedure Constructor_InputOutputArray(Obj: TCVInputOutputArrayPointer; m: pCVMatPointer); overload; external opencv_world_dll index 1365 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ??0_InputOutputArray@cv@@QEAA@AEBVUMat@1@@Z     public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::UMat const & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@HPEAX@Z   public: __cdecl cv::_InputOutputArray::_InputOutputArray(int,void * __ptr64) __ptr64

// ??0_InputOutputArray@cv@@QEAA@XZ
// public: __cdecl cv::_InputOutputArray::_InputOutputArray(void) __ptr64
procedure Constructor_InputOutputArray(Obj: TCVInputOutputArrayPointer); overload; external opencv_world_dll index 1368 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ??1_InputOutputArray@cv@@QEAA@XZ
// public: __cdecl cv::_InputOutputArray::~_InputOutputArray(void) __ptr64
procedure Destructor_InputOutputArray(Obj: TCVInputOutputArrayPointer); overload; external opencv_world_dll index 1813 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ??4_InputOutputArray@cv@@QEAAAEAV01@$$QEAV01@@Z public: class cv::_InputOutputArray & __ptr64 __cdecl cv::_InputOutputArray::operator=(class cv::_InputOutputArray && __ptr64) __ptr64
// ??4_InputOutputArray@cv@@QEAAAEAV01@AEBV01@@Z   public: class cv::_InputOutputArray & __ptr64 __cdecl cv::_InputOutputArray::operator=(class cv::_InputOutputArray const & __ptr64) __ptr64
{ --------------- End InputOutputArray --------------- }

{ --------------- Start Scalar --------------- }
// ??0Scalar@own@gapi@cv@@QEAA@N@Z
// public: __cdecl cv::gapi::own::Scalar::Scalar(double) __ptr64
function constructor_Scalar(Obj: TCVScalarPointer; v0: double): TCVScalarPointer; overload; external opencv_world_dll index 1071 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Scalar@own@gapi@cv@@QEAA@NNNN@Z
// public: __cdecl cv::gapi::own::Scalar::Scalar(double,double,double,double) __ptr64
function constructor_Scalar(Obj: TCVScalarPointer; v0, v1, v2, v3: double): TCVScalarPointer; overload; external opencv_world_dll index 1072 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??0Scalar@own@gapi@cv@@QEAA@XZ
// public: __cdecl cv::gapi::own::Scalar::Scalar(void) __ptr64
function constructor_Scalar(Obj: TCVScalarPointer): TCVScalarPointer; overload; external opencv_world_dll index 1073 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
// ??4Scalar@own@gapi@cv@@QEAAAEAV0123@$$QEAV0123@@Z	public: class cv::gapi::own::Scalar & __ptr64 __cdecl cv::gapi::own::Scalar::operator=(class cv::gapi::own::Scalar && __ptr64) __ptr64
// ??4Scalar@own@gapi@cv@@QEAAAEAV0123@AEBV0123@@Z	public: class cv::gapi::own::Scalar & __ptr64 __cdecl cv::gapi::own::Scalar::operator=(class cv::gapi::own::Scalar const & __ptr64) __ptr64
// ?all@Scalar@own@gapi@cv@@SA?AV1234@N@Z	public: static class cv::gapi::own::Scalar __cdecl cv::gapi::own::Scalar::all(double)
{ --------------- End Scalar --------------- }

{ --------------- Start MatExpr --------------- }
// ??0MatExpr@cv@@QEAA@$$QEAV01@@Z public: __cdecl cv::MatExpr::MatExpr(class cv::MatExpr && __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@AEBV01@@Z   public: __cdecl cv::MatExpr::MatExpr(class cv::MatExpr const & __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@AEBVMat@1@@Z        public: __cdecl cv::MatExpr::MatExpr(class cv::Mat const & __ptr64) __ptr64
// ??0MatExpr@cv@@QEAA@PEBVMatOp@1@HAEBVMat@1@11NNAEBV?$Scalar_@N@1@@Z     public: __cdecl cv::MatExpr::MatExpr(class cv::MatOp const * __ptr64,int,class cv::Mat const & __ptr64,class cv::Mat const & __ptr64,class cv::Mat const & __ptr64,double,double,class cv::Scalar_<double> const & __ptr64) __ptr64

// ??0MatExpr@cv@@QEAA@XZ
// public: __cdecl cv::MatExpr::MatExpr(void) __ptr64
procedure constructor_MatExpr(Obj: TCVMatExprPointer); overload; external opencv_world_dll index 787 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ??1MatExpr@cv@@QEAA@XZ
// public: __cdecl cv::MatExpr::~MatExpr(void) __ptr64
procedure Destructor_MatExpr(Obj: TCVMatExprPointer); overload; external opencv_world_dll index 1649 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

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

function MatExpr_size(Obj: TCVMatExprPointer): TCVSizePointer; external opencv_world_dll index 6473 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ?swap@MatExpr@cv@@QEAAXAEAV12@@Z        public: void __cdecl cv::MatExpr::swap(class cv::MatExpr & __ptr64) __ptr64
// ?t@MatExpr@cv@@QEBA?AV12@XZ     public: class cv::MatExpr __cdecl cv::MatExpr::t(void)const __ptr64
// ?type@MatExpr@cv@@QEBAHXZ       public: int __cdecl cv::MatExpr::type(void)const __ptr64
{ --------------- End MatExpr --------------- }

{ --------------- Start MatSize --------------- }
// ??0MatSize@cv@@QEAA@PEAH@Z      public: __cdecl cv::MatSize::MatSize(int * __ptr64) __ptr64
// ??4MatSize@cv@@QEAAAEAU01@$$QEAU01@@Z   public: struct cv::MatSize & __ptr64 __cdecl cv::MatSize::operator=(struct cv::MatSize && __ptr64) __ptr64
// ??4MatSize@cv@@QEAAAEAU01@AEBU01@@Z     public: struct cv::MatSize & __ptr64 __cdecl cv::MatSize::operator=(struct cv::MatSize const & __ptr64) __ptr64

// ??RMatSize@cv@@QEBA?AV?$Size_@H@1@XZ
// public: class cv::Size_<int> __cdecl cv::MatSize::operator()(void)const __ptr64
procedure MatSize_MatSizeToSize(Obj: TCVMatSizePointer; const s: TCVSizePointer); external opencv_world_dll index 2965 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ?dims@MatSize@cv@@QEBAHXZ       public: int __cdecl cv::MatSize::dims(void)const __ptr64
{ --------------- End MatSize --------------- }

{ --------------- Start MatOp --------------- }
// ??0MatOp@cv@@QEAA@AEBV01@@Z     public: __cdecl cv::MatOp::MatOp(class cv::MatOp const & __ptr64) __ptr64
// ??0MatOp@cv@@QEAA@XZ    public: __cdecl cv::MatOp::MatOp(void) __ptr64
// ??1MatOp@cv@@UEAA@XZ    public: virtual __cdecl cv::MatOp::~MatOp(void) __ptr64
// ??4MatOp@cv@@QEAAAEAV01@AEBV01@@Z       public: class cv::MatOp & __ptr64 __cdecl cv::MatOp::operator=(class cv::MatOp const & __ptr64) __ptr64
// ?abs@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAV32@@Z     public: virtual void __cdecl cv::MatOp::abs(class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?add@MatOp@cv@@UEBAXAEBVMatExpr@2@0AEAV32@@Z    public: virtual void __cdecl cv::MatOp::add(class cv::MatExpr const & __ptr64,class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?add@MatOp@cv@@UEBAXAEBVMatExpr@2@AEBV?$Scalar_@N@2@AEAV32@@Z   public: virtual void __cdecl cv::MatOp::add(class cv::MatExpr const & __ptr64,class cv::Scalar_<double> const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?augAssignAdd@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAVMat@2@@Z public: virtual void __cdecl cv::MatOp::augAssignAdd(class cv::MatExpr const & __ptr64,class cv::Mat & __ptr64)const __ptr64
// ?augAssignAnd@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAVMat@2@@Z public: virtual void __cdecl cv::MatOp::augAssignAnd(class cv::MatExpr const & __ptr64,class cv::Mat & __ptr64)const __ptr64
// ?augAssignDivide@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAVMat@2@@Z      public: virtual void __cdecl cv::MatOp::augAssignDivide(class cv::MatExpr const & __ptr64,class cv::Mat & __ptr64)const __ptr64
// ?augAssignMultiply@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAVMat@2@@Z    public: virtual void __cdecl cv::MatOp::augAssignMultiply(class cv::MatExpr const & __ptr64,class cv::Mat & __ptr64)const __ptr64
// ?augAssignOr@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAVMat@2@@Z  public: virtual void __cdecl cv::MatOp::augAssignOr(class cv::MatExpr const & __ptr64,class cv::Mat & __ptr64)const __ptr64
// ?augAssignSubtract@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAVMat@2@@Z    public: virtual void __cdecl cv::MatOp::augAssignSubtract(class cv::MatExpr const & __ptr64,class cv::Mat & __ptr64)const __ptr64
// ?augAssignXor@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAVMat@2@@Z public: virtual void __cdecl cv::MatOp::augAssignXor(class cv::MatExpr const & __ptr64,class cv::Mat & __ptr64)const __ptr64
// ?diag@MatOp@cv@@UEBAXAEBVMatExpr@2@HAEAV32@@Z   public: virtual void __cdecl cv::MatOp::diag(class cv::MatExpr const & __ptr64,int,class cv::MatExpr & __ptr64)const __ptr64
// ?divide@MatOp@cv@@UEBAXAEBVMatExpr@2@0AEAV32@N@Z        public: virtual void __cdecl cv::MatOp::divide(class cv::MatExpr const & __ptr64,class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64,double)const __ptr64
// ?divide@MatOp@cv@@UEBAXNAEBVMatExpr@2@AEAV32@@Z public: virtual void __cdecl cv::MatOp::divide(double,class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?elementWise@MatOp@cv@@UEBA_NAEBVMatExpr@2@@Z   public: virtual bool __cdecl cv::MatOp::elementWise(class cv::MatExpr const & __ptr64)const __ptr64
// ?invert@MatOp@cv@@UEBAXAEBVMatExpr@2@HAEAV32@@Z public: virtual void __cdecl cv::MatOp::invert(class cv::MatExpr const & __ptr64,int,class cv::MatExpr & __ptr64)const __ptr64
// ?matmul@MatOp@cv@@UEBAXAEBVMatExpr@2@0AEAV32@@Z public: virtual void __cdecl cv::MatOp::matmul(class cv::MatExpr const & __ptr64,class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?multiply@MatOp@cv@@UEBAXAEBVMatExpr@2@0AEAV32@N@Z      public: virtual void __cdecl cv::MatOp::multiply(class cv::MatExpr const & __ptr64,class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64,double)const __ptr64
// ?multiply@MatOp@cv@@UEBAXAEBVMatExpr@2@NAEAV32@@Z       public: virtual void __cdecl cv::MatOp::multiply(class cv::MatExpr const & __ptr64,double,class cv::MatExpr & __ptr64)const __ptr64
// ?roi@MatOp@cv@@UEBAXAEBVMatExpr@2@AEBVRange@2@1AEAV32@@Z        public: virtual void __cdecl cv::MatOp::roi(class cv::MatExpr const & __ptr64,class cv::Range const & __ptr64,class cv::Range const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?size@MatOp@cv@@UEBA?AV?$Size_@H@2@AEBVMatExpr@2@@Z     public: virtual class cv::Size_<int> __cdecl cv::MatOp::size(class cv::MatExpr const & __ptr64)const __ptr64
// ?subtract@MatOp@cv@@UEBAXAEBV?$Scalar_@N@2@AEBVMatExpr@2@AEAV42@@Z      public: virtual void __cdecl cv::MatOp::subtract(class cv::Scalar_<double> const & __ptr64,class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?subtract@MatOp@cv@@UEBAXAEBVMatExpr@2@0AEAV32@@Z       public: virtual void __cdecl cv::MatOp::subtract(class cv::MatExpr const & __ptr64,class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?transpose@MatOp@cv@@UEBAXAEBVMatExpr@2@AEAV32@@Z       public: virtual void __cdecl cv::MatOp::transpose(class cv::MatExpr const & __ptr64,class cv::MatExpr & __ptr64)const __ptr64
// ?type@MatOp@cv@@UEBAHAEBVMatExpr@2@@Z   public: virtual int __cdecl cv::MatOp::type(class cv::MatExpr const & __ptr64)const __ptr64
{ --------------- End MatOp --------------- }

implementation

end.
