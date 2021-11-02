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
procedure Constructor_InputOutputArray(Obj: pInputOutputArray; m: pMat); overload; external opencv_world_dll index 1365 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??0_InputOutputArray@cv@@QEAA@AEBVUMat@1@@Z     public: __cdecl cv::_InputOutputArray::_InputOutputArray(class cv::UMat const & __ptr64) __ptr64
// ??0_InputOutputArray@cv@@QEAA@HPEAX@Z   public: __cdecl cv::_InputOutputArray::_InputOutputArray(int,void * __ptr64) __ptr64

// ??0_InputOutputArray@cv@@QEAA@XZ
// public: __cdecl cv::_InputOutputArray::_InputOutputArray(void) __ptr64
procedure Constructor_InputOutputArray(Obj: pInputOutputArray); overload; external opencv_world_dll index 1368 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??1_InputOutputArray@cv@@QEAA@XZ
// public: __cdecl cv::_InputOutputArray::~_InputOutputArray(void) __ptr64
procedure Destructor_InputOutputArray(Obj: pInputOutputArray); overload; external opencv_world_dll index 1813 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??4_InputOutputArray@cv@@QEAAAEAV01@$$QEAV01@@Z public: class cv::_InputOutputArray & __ptr64 __cdecl cv::_InputOutputArray::operator=(class cv::_InputOutputArray && __ptr64) __ptr64
// ??4_InputOutputArray@cv@@QEAAAEAV01@AEBV01@@Z   public: class cv::_InputOutputArray & __ptr64 __cdecl cv::_InputOutputArray::operator=(class cv::_InputOutputArray const & __ptr64) __ptr64

// 5740
// ?noArray@cv@@YAAEBV_InputOutputArray@1@XZ
// class cv::_InputOutputArray const & cv::noArray(void)
procedure noArray_InputOutputArray(Obj: pInputOutputArray); overload; external opencv_world_dll index 5740 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
