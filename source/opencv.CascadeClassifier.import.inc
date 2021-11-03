// ??0CascadeClassifier@cv@@QEAA@AEBV01@@Z public: __cdecl cv::CascadeClassifier::CascadeClassifier(class cv::CascadeClassifier const & __ptr64) __ptr64
// ??0CascadeClassifier@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z       public: __cdecl cv::CascadeClassifier::CascadeClassifier(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64) __ptr64

// ??0CascadeClassifier@cv@@QEAA@XZ
// public: __cdecl cv::CascadeClassifier::CascadeClassifier(void) __ptr64
procedure Constructor_CascadeClassifier(Obj: pCascadeClassifier); external opencv_world_dll index 209 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ??1CascadeClassifier@cv@@QEAA@XZ
// public: __cdecl cv::CascadeClassifier::~CascadeClassifier(void) __ptr64
procedure Destructor_CascadeClassifier(Obj: pCascadeClassifier); external opencv_world_dll index 1480 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ??4CascadeClassifier@cv@@QEAAAEAV01@AEBV01@@Z   public: class cv::CascadeClassifier & __ptr64 __cdecl cv::CascadeClassifier::operator=(class cv::CascadeClassifier const & __ptr64) __ptr64
// ?convert@CascadeClassifier@cv@@SA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@0@Z public: static bool __cdecl cv::CascadeClassifier::convert(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64)

// 4431
// ?detectMultiScale@CascadeClassifier@cv@@QEAAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@HV?$allocator@H@std@@@5@AEAV?$vector@NV?$allocator@N@std@@@5@NHHV?$Size_@H@2@4_N@Z
// public: void __cdecl cv::CascadeClassifier::detectMultiScale(class cv::_InputArray const & __ptr64,class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,class std::vector<int,class std::allocator<int> > & __ptr64,class std::vector<double,class std::allocator<double> > & __ptr64,double,int,int,class cv::Size_<int>,class cv::Size_<int>,bool) __ptr64
// 4432
// ?detectMultiScale@CascadeClassifier@cv@@QEAAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@HV?$allocator@H@std@@@5@NHHV?$Size_@H@2@3@Z
// public: void __cdecl cv::CascadeClassifier::detectMultiScale(class cv::_InputArray const & __ptr64,class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,class std::vector<int,class std::allocator<int> > & __ptr64,double,int,int,class cv::Size_<int>,class cv::Size_<int>) __ptr64
// 4433
// ?detectMultiScale@CascadeClassifier@cv@@QEAAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@NHHV?$Size_@H@2@2@Z
// public: void __cdecl cv::CascadeClassifier::detectMultiScale(class cv::_InputArray const & __ptr64,class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > & __ptr64,double,int,int,class cv::Size_<int>,class cv::Size_<int>) __ptr64
procedure detectMultiScale_CascadeClassifier(Obj: pCascadeClassifier; image: pInputArray; objects: pStdVectorRect; scaleFactor: double; minNeighbors: int; flags: int;
  minSize: UInt64 { TCVSizePointer }; maxSize: UInt64 { TCVSizePointer } ); external opencv_world_dll index 4433 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// ?empty@CascadeClassifier@cv@@QEBA_NXZ   public: bool __cdecl cv::CascadeClassifier::empty(void)const __ptr64
// ?getFeatureType@CascadeClassifier@cv@@QEBAHXZ   public: int __cdecl cv::CascadeClassifier::getFeatureType(void)const __ptr64
// ?getMaskGenerator@CascadeClassifier@cv@@QEAA?AU?$Ptr@VMaskGenerator@BaseCascadeClassifier@cv@@@2@XZ     public: struct cv::Ptr<class cv::BaseCascadeClassifier::MaskGenerator> __cdecl cv::CascadeClassifier::getMaskGenerator(void) __ptr64
// ?getOldCascade@CascadeClassifier@cv@@QEAAPEAXXZ public: void * __ptr64 __cdecl cv::CascadeClassifier::getOldCascade(void) __ptr64
// ?getOriginalWindowSize@CascadeClassifier@cv@@QEBA?AV?$Size_@H@2@XZ      public: class cv::Size_<int> __cdecl cv::CascadeClassifier::getOriginalWindowSize(void)const __ptr64
// ?isOldFormatCascade@CascadeClassifier@cv@@QEBA_NXZ      public: bool __cdecl cv::CascadeClassifier::isOldFormatCascade(void)const __ptr64
// 5473
// ?load@CascadeClassifier@cv@@QEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z
// public: bool __cdecl cv::CascadeClassifier::load(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const & __ptr64) __ptr64
function load_CascadeClassifier(Obj: pCascadeClassifier; s: pCvStdString): Bool; external opencv_world_dll index 5473 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
// ?read@CascadeClassifier@cv@@QEAA_NAEBVFileNode@2@@Z     public: bool __cdecl cv::CascadeClassifier::read(class cv::FileNode const & __ptr64) __ptr64
// ?setMaskGenerator@CascadeClassifier@cv@@QEAAXAEBU?$Ptr@VMaskGenerator@BaseCascadeClassifier@cv@@@2@@Z   public: void __cdecl cv::CascadeClassifier::setMaskGenerator(struct cv::Ptr<class cv::BaseCascadeClassifier::MaskGenerator> const & __ptr64) __ptr64
