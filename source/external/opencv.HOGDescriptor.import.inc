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

  // 622	??0HOGDescriptor@cv@@QEAA@AEBU01@@Z	??0HOGDescriptor@cv@@QEAA@AEBU01@@Z	cv::HOGDescriptor::HOGDescriptor(struct cv::HOGDescriptor const &)
  // 623	??0HOGDescriptor@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	??0HOGDescriptor@cv@@QEAA@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	cv::HOGDescriptor::HOGDescriptor(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
  {
    624
    ??0HOGDescriptor@cv@@QEAA@V?$Size_@H@1@000HHNW4HistogramNormType@01@N_NH2@Z
    ??0HOGDescriptor@cv@@QEAA@V?$Size_@H@1@000HHNW4HistogramNormType@01@N_NH2@Z
    cv::HOGDescriptor::HOGDescriptor(class cv::Size_<int>,class cv::Size_<int>,class cv::Size_<int>,class cv::Size_<int>,int,int,double,enum cv::HOGDescriptor::HistogramNormType,double,bool,int,bool)
  }
procedure constructor_HOGDescriptor(
          Var Obj: THOGDescriptor;
          const _winSize: UInt64 {TSize};
          const _blockSize: UInt64{TSize};
          const _blockStride: UInt64{TSize};
          const _cellSize: UInt64{TSize};
          _nbins: int;
          _derivAperture: int = 1;
          _winSigma: double = -1;
          _histogramNormType: THOGDescriptor.HistogramNormType = L2Hys;
          _L2HysThreshold: double = 0.2;
          _gammaCorrection: bool = false;
          _nlevels: int = THOGDescriptor.DEFAULT_NLEVELS;
          _signedGradient: bool = false);
  overload; external opencv_world_dll name '??0HOGDescriptor@cv@@QEAA@V?$Size_@H@1@000HHNW4HistogramNormType@01@N_NH2@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    625
    ??0HOGDescriptor@cv@@QEAA@XZ
    ??0HOGDescriptor@cv@@QEAA@XZ
    cv::HOGDescriptor::HOGDescriptor(void)
  }
procedure constructor_HOGDescriptor(Var Obj: THOGDescriptor); overload; external opencv_world_dll name '??0HOGDescriptor@cv@@QEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    1605
    ??1HOGDescriptor@cv@@UEAA@XZ
    ??1HOGDescriptor@cv@@UEAA@XZ
    cv::HOGDescriptor::~HOGDescriptor(void)
  }
procedure destructor_HOGDescriptor(Var Obj: THOGDescriptor); external opencv_world_dll name '??1HOGDescriptor@cv@@UEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

  // 2187	??4HOGDescriptor@cv@@QEAAAEAU01@AEBU01@@Z	??4HOGDescriptor@cv@@QEAAAEAU01@AEBU01@@Z	struct cv::HOGDescriptor & cv::HOGDescriptor::operator=(struct cv::HOGDescriptor const &)
  // 3149	??_7HOGDescriptor@cv@@6B@	??_7HOGDescriptor@cv@@6B@	const cv::HOGDescriptor::`vftable'
  // 3768	?checkDetectorSize@HOGDescriptor@cv@@QEBA_NXZ	?checkDetectorSize@HOGDescriptor@cv@@QEBA_NXZ	bool cv::HOGDescriptor::checkDetectorSize(void)
  // 3991	?compute@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@MV?$allocator@M@std@@@std@@V?$Size_@H@2@2AEBV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@6@@Z	?compute@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@MV?$allocator@M@std@@@std@@V?$Size_@H@2@2AEBV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@5@@Z	void cv::HOGDescriptor::compute(class cv::_InputArray const &,class std::vector<float,class std::allocator<float> > &,class cv::Size_<int>,class cv::Size_<int>,class std::vector<class cv::Point_<int>,class std::allocator<class cv::Point_<int> > > const &)
  // 3999	?computeGradient@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEBV_InputOutputArray@42@1V?$Size_@H@2@2@Z	?computeGradient@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEBV_InputOutputArray@2@1V?$Size_@H@2@2@Z	void cv::HOGDescriptor::computeGradient(class cv::_InputArray const &,class cv::_InputOutputArray const &,class cv::_InputOutputArray const &,class cv::Size_<int>,class cv::Size_<int>)
  // 4076	?copyTo@HOGDescriptor@cv@@UEBAXAEAU12@@Z	?copyTo@HOGDescriptor@cv@@UEBAXAEAU12@@Z	void cv::HOGDescriptor::copyTo(struct cv::HOGDescriptor &)
  // 4419	?detect@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@6@NV?$Size_@H@2@3AEBV56@@Z	?detect@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@5@NV?$Size_@H@2@3AEBV45@@Z	void cv::HOGDescriptor::detect(class cv::_InputArray const &,class std::vector<class cv::Point_<int>,class std::allocator<class cv::Point_<int> > > &,class std::vector<double,class std::allocator<double> > &,double,class cv::Size_<int>,class cv::Size_<int>,class std::vector<class cv::Point_<int>,class std::allocator<class cv::Point_<int> > > const &)
  // 4420	?detect@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@std@@NV?$Size_@H@2@2AEBV56@@Z	?detect@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@std@@NV?$Size_@H@2@2AEBV45@@Z	void cv::HOGDescriptor::detect(class cv::_InputArray const &,class std::vector<class cv::Point_<int>,class std::allocator<class cv::Point_<int> > > &,double,class cv::Size_<int>,class cv::Size_<int>,class std::vector<class cv::Point_<int>,class std::allocator<class cv::Point_<int> > > const &)
  {
    4434
    ?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@6@NV?$Size_@H@2@3NN_N@Z
    ?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@5@NV?$Size_@H@2@3NN_N@Z
    void cv::HOGDescriptor::detectMultiScale(

    class cv::_InputArray const &,
    class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > &,
    class std::vector<double,class std::allocator<double> > &,
    double,
    class cv::Size_<int>,
    class cv::Size_<int>,
    double,
    double,
    bool)
  }
procedure detectMultiScale_HOGDescriptor( const Obj: THOGDescriptor;
                                          const img: TInputArray;
                                          const foundLocations: vector<TRect>;
                                          const foundWeights: vector<double>;
                                          hitThreshold: double {= 0};
                                          const winStride: UInt64 {TSize};
                                          const padding: UInt64 {TSize};
                                          scale: double = 1.05;
                                          finalThreshold: double = 2.0;
                                          useMeanshiftGrouping: BOOL = false);
overload; external opencv_world_dll
{$IFDEF DEBUG}
  name '?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@6@NV?$Size_@H@2@3NN_N@Z'
{$ELSE}
  name '?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@5@NV?$Size_@H@2@3NN_N@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    4435
    ?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@NV?$Size_@H@2@2NN_N@Z
    ?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@NV?$Size_@H@2@2NN_N@Z
    void cv::HOGDescriptor::detectMultiScale(

    class cv::_InputArray const &,
    class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > &,
    double,
    class cv::Size_<int>,
    class cv::Size_<int>,
    double,
    double,
    bool)
  }
procedure detectMultiScale_HOGDescriptor(
                                const Obj: THOGDescriptor;
                                const img:TInputArray;
                                const foundLocations:vector<TRect>;
                                hitThreshold :double {= 0};
                                const winStride :UInt64 {TSize};
                                const padding :UInt64 {TSize};
                                scale :double = 1.05;
                                finalThreshold :double = 2.0;
                                useMeanshiftGrouping :bool = false);
overload; external opencv_world_dll
{$IFDEF DEBUG}
  name '?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@NV?$Size_@H@2@2NN_N@Z'
{$ELSE}
  name '?detectMultiScale@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@NV?$Size_@H@2@2NN_N@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

  // 4436	?detectMultiScaleROI@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@UDetectionROI@cv@@V?$allocator@UDetectionROI@cv@@@std@@@6@NH@Z	?detectMultiScaleROI@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@UDetectionROI@cv@@V?$allocator@UDetectionROI@cv@@@std@@@5@NH@Z	void cv::HOGDescriptor::detectMultiScaleROI(class cv::_InputArray const &,class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > &,class std::vector<struct cv::DetectionROI,class std::allocator<struct cv::DetectionROI> > &,double,int)
  // 4437	?detectROI@HOGDescriptor@cv@@UEBAXAEBV_InputArray@debug_build_guard@2@AEBV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@std@@AEAV56@AEAV?$vector@NV?$allocator@N@std@@@6@NV?$Size_@H@2@4@Z	?detectROI@HOGDescriptor@cv@@UEBAXAEBV_InputArray@2@AEBV?$vector@V?$Point_@H@cv@@V?$allocator@V?$Point_@H@cv@@@std@@@std@@AEAV45@AEAV?$vector@NV?$allocator@N@std@@@5@NV?$Size_@H@2@4@Z	void cv::HOGDescriptor::detectROI(class cv::_InputArray const &,class std::vector<class cv::Point_<int>,class std::allocator<class cv::Point_<int> > > const &,class std::vector<class cv::Point_<int>,class std::allocator<class cv::Point_<int> > > &,class std::vector<double,class std::allocator<double> > &,double,class cv::Size_<int>,class cv::Size_<int>)
  {
    4890
    ?getDaimlerPeopleDetector@HOGDescriptor@cv@@SA?AV?$vector@MV?$allocator@M@std@@@std@@XZ
    ?getDaimlerPeopleDetector@HOGDescriptor@cv@@SA?AV?$vector@MV?$allocator@M@std@@@std@@XZ
    class std::vector<float,class std::allocator<float> > cv::HOGDescriptor::getDaimlerPeopleDetector(void)
  }
function getDaimlerPeopleDetector_HOGDescriptor( { const Obj: THOGDescriptor } ): vector<float>;
  external opencv_world_dll name '?getDaimlerPeopleDetector@HOGDescriptor@cv@@SA?AV?$vector@MV?$allocator@M@std@@@std@@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  {
    4916
    ?getDefaultPeopleDetector@HOGDescriptor@cv@@SA?AV?$vector@MV?$allocator@M@std@@@std@@XZ
    ?getDefaultPeopleDetector@HOGDescriptor@cv@@SA?AV?$vector@MV?$allocator@M@std@@@std@@XZ
    class std::vector<float,class std::allocator<float> > cv::HOGDescriptor::getDefaultPeopleDetector(void)
  }
function getDefaultPeopleDetector_HOGDescriptor( { const Obj: THOGDescriptor } ): vector<float>;
  external opencv_world_dll name '?getDefaultPeopleDetector@HOGDescriptor@cv@@SA?AV?$vector@MV?$allocator@M@std@@@std@@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

  // 4920	?getDescriptorSize@HOGDescriptor@cv@@QEBA_KXZ	?getDescriptorSize@HOGDescriptor@cv@@QEBA_KXZ	unsigned __int64 cv::HOGDescriptor::getDescriptorSize(void)
  // 5180	?getWinSigma@HOGDescriptor@cv@@QEBANXZ	?getWinSigma@HOGDescriptor@cv@@QEBANXZ	double cv::HOGDescriptor::getWinSigma(void)
  // 5204	?groupRectangles@HOGDescriptor@cv@@QEBAXAEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@4@HN@Z	?groupRectangles@HOGDescriptor@cv@@QEBAXAEAV?$vector@V?$Rect_@H@cv@@V?$allocator@V?$Rect_@H@cv@@@std@@@std@@AEAV?$vector@NV?$allocator@N@std@@@4@HN@Z	void cv::HOGDescriptor::groupRectangles(class std::vector<class cv::Rect_<int>,class std::allocator<class cv::Rect_<int> > > &,class std::vector<double,class std::allocator<double> > &,int,double)
  // 5476	?load@HOGDescriptor@cv@@UEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@0@Z	?load@HOGDescriptor@cv@@UEAA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@0@Z	bool cv::HOGDescriptor::load(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
  // 6000	?read@HOGDescriptor@cv@@UEAA_NAEAVFileNode@2@@Z	?read@HOGDescriptor@cv@@UEAA_NAEAVFileNode@2@@Z	bool cv::HOGDescriptor::read(class cv::FileNode &)
  // 6208	?save@HOGDescriptor@cv@@UEBAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@0@Z	?save@HOGDescriptor@cv@@UEBAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@0@Z	void cv::HOGDescriptor::save(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
  {
    6376
    ?setSVMDetector@HOGDescriptor@cv@@UEAAXAEBV_InputArray@debug_build_guard@2@@Z
    ?setSVMDetector@HOGDescriptor@cv@@UEAAXAEBV_InputArray@2@@Z
    void cv::HOGDescriptor::setSVMDetector(class cv::_InputArray const &)
  }
procedure setSVMDetector_HOGDescriptor(const Obj: THOGDescriptor; const svmdetector: TInputArray); external opencv_world_dll
{$IFDEF DEBUG}
  name '?setSVMDetector@HOGDescriptor@cv@@UEAAXAEBV_InputArray@debug_build_guard@2@@Z'
{$ELSE}
  name '?setSVMDetector@HOGDescriptor@cv@@UEAAXAEBV_InputArray@2@@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
  // 6876	?write@HOGDescriptor@cv@@UEBAXAEAVFileStorage@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	?write@HOGDescriptor@cv@@UEBAXAEAVFileStorage@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	void cv::HOGDescriptor::write(class cv::FileStorage &,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
