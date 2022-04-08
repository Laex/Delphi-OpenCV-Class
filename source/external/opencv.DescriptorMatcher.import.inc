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

//326	??0DescriptorCollection@DescriptorMatcher@cv@@QEAA@AEBV012@@Z	??0DescriptorCollection@DescriptorMatcher@cv@@QEAA@AEBV012@@Z	cv::DescriptorMatcher::DescriptorCollection::DescriptorCollection(class cv::DescriptorMatcher::DescriptorCollection const &)
//327	??0DescriptorCollection@DescriptorMatcher@cv@@QEAA@XZ	??0DescriptorCollection@DescriptorMatcher@cv@@QEAA@XZ	cv::DescriptorMatcher::DescriptorCollection::DescriptorCollection(void)
//328	??0DescriptorMatcher@cv@@QEAA@AEBV01@@Z	??0DescriptorMatcher@cv@@QEAA@AEBV01@@Z	cv::DescriptorMatcher::DescriptorMatcher(class cv::DescriptorMatcher const &)
//329	??0DescriptorMatcher@cv@@QEAA@XZ	??0DescriptorMatcher@cv@@QEAA@XZ	cv::DescriptorMatcher::DescriptorMatcher(void)
//1519	??1DescriptorCollection@DescriptorMatcher@cv@@UEAA@XZ	??1DescriptorCollection@DescriptorMatcher@cv@@UEAA@XZ	cv::DescriptorMatcher::DescriptorCollection::~DescriptorCollection(void)
//1520	??1DescriptorMatcher@cv@@UEAA@XZ	??1DescriptorMatcher@cv@@UEAA@XZ	cv::DescriptorMatcher::~DescriptorMatcher(void)
//2032	??4DescriptorCollection@DescriptorMatcher@cv@@QEAAAEAV012@AEBV012@@Z	??4DescriptorCollection@DescriptorMatcher@cv@@QEAAAEAV012@AEBV012@@Z	class cv::DescriptorMatcher::DescriptorCollection & cv::DescriptorMatcher::DescriptorCollection::operator=(class cv::DescriptorMatcher::DescriptorCollection const &)
//2033	??4DescriptorMatcher@cv@@QEAAAEAV01@AEBV01@@Z	??4DescriptorMatcher@cv@@QEAAAEAV01@AEBV01@@Z	class cv::DescriptorMatcher & cv::DescriptorMatcher::operator=(class cv::DescriptorMatcher const &)
//3103	??_7DescriptorCollection@DescriptorMatcher@cv@@6B@	??_7DescriptorCollection@DescriptorMatcher@cv@@6B@	const cv::DescriptorMatcher::DescriptorCollection::`vftable'
//3104	??_7DescriptorMatcher@cv@@6B@	??_7DescriptorMatcher@cv@@6B@	const cv::DescriptorMatcher::`vftable'
//3484	?add@DescriptorMatcher@cv@@UEAAXAEBV_InputArray@debug_build_guard@2@@Z	?add@DescriptorMatcher@cv@@UEAAXAEBV_InputArray@2@@Z	void cv::DescriptorMatcher::add(class cv::_InputArray const &)
//3770	?checkMasks@DescriptorMatcher@cv@@IEBAXAEBV_InputArray@debug_build_guard@2@H@Z	?checkMasks@DescriptorMatcher@cv@@IEBAXAEBV_InputArray@2@H@Z	void cv::DescriptorMatcher::checkMasks(class cv::_InputArray const &,int)
//3891	?clear@DescriptorCollection@DescriptorMatcher@cv@@UEAAXXZ	?clear@DescriptorCollection@DescriptorMatcher@cv@@UEAAXXZ	void cv::DescriptorMatcher::DescriptorCollection::clear(void)
//3892	?clear@DescriptorMatcher@cv@@UEAAXXZ	?clear@DescriptorMatcher@cv@@UEAAXXZ	void cv::DescriptorMatcher::clear(void)
//3919	?clone_op@DescriptorMatcher@cv@@KA?AVMat@2@V32@@Z	?clone_op@DescriptorMatcher@cv@@KA?AVMat@2@V32@@Z	class cv::Mat cv::DescriptorMatcher::clone_op(class cv::Mat)
{
4144
?create@DescriptorMatcher@cv@@SA?AU?$Ptr@VDescriptorMatcher@cv@@@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z
?create@DescriptorMatcher@cv@@SA?AU?$Ptr@VDescriptorMatcher@cv@@@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z
struct cv::Ptr<class cv::DescriptorMatcher> cv::DescriptorMatcher::create(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
}
function create_DescriptorMatcher(const descriptorMatcherType:CppString):TPtr<TDescriptorMatcher>;
overload; external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
name '?create@DescriptorMatcher@cv@@SA?AU?$Ptr@VDescriptorMatcher@cv@@@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z'
{$ELSE}
name '?create@DescriptorMatcher@cv@@SA?AU?$Ptr@VDescriptorMatcher@cv@@@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
4145
?create@DescriptorMatcher@cv@@SA?AU?$Ptr@VDescriptorMatcher@cv@@@2@AEBW4MatcherType@12@@Z
?create@DescriptorMatcher@cv@@SA?AU?$Ptr@VDescriptorMatcher@cv@@@2@AEBW4MatcherType@12@@Z
struct cv::Ptr<class cv::DescriptorMatcher> cv::DescriptorMatcher::create(enum cv::DescriptorMatcher::MatcherType const &)
}
//4586	?empty@DescriptorMatcher@cv@@UEBA_NXZ	?empty@DescriptorMatcher@cv@@UEBA_NXZ	bool cv::DescriptorMatcher::empty(void)
//4918	?getDescriptor@DescriptorCollection@DescriptorMatcher@cv@@QEBA?BVMat@3@H@Z	?getDescriptor@DescriptorCollection@DescriptorMatcher@cv@@QEBA?BVMat@3@H@Z	class cv::Mat const cv::DescriptorMatcher::DescriptorCollection::getDescriptor(int)
//4919	?getDescriptor@DescriptorCollection@DescriptorMatcher@cv@@QEBA?BVMat@3@HH@Z	?getDescriptor@DescriptorCollection@DescriptorMatcher@cv@@QEBA?BVMat@3@HH@Z	class cv::Mat const cv::DescriptorMatcher::DescriptorCollection::getDescriptor(int,int)
//4922	?getDescriptors@DescriptorCollection@DescriptorMatcher@cv@@QEBAAEBVMat@3@XZ	?getDescriptors@DescriptorCollection@DescriptorMatcher@cv@@QEBAAEBVMat@3@XZ	class cv::Mat const & cv::DescriptorMatcher::DescriptorCollection::getDescriptors(void)
//5006	?getLocalIdx@DescriptorCollection@DescriptorMatcher@cv@@QEBAXHAEAH0@Z	?getLocalIdx@DescriptorCollection@DescriptorMatcher@cv@@QEBAXHAEAH0@Z	void cv::DescriptorMatcher::DescriptorCollection::getLocalIdx(int,int &,int &)
//5151	?getTrainDescriptors@DescriptorMatcher@cv@@QEBAAEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@XZ	?getTrainDescriptors@DescriptorMatcher@cv@@QEBAAEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@XZ	class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > const & cv::DescriptorMatcher::getTrainDescriptors(void)
//5382	?isMaskedOut@DescriptorMatcher@cv@@KA_NAEBV_InputArray@debug_build_guard@2@H@Z	?isMaskedOut@DescriptorMatcher@cv@@KA_NAEBV_InputArray@2@H@Z	bool cv::DescriptorMatcher::isMaskedOut(class cv::_InputArray const &,int)
//5400	?isPossibleMatch@DescriptorMatcher@cv@@KA_NAEBV_InputArray@debug_build_guard@2@HH@Z	?isPossibleMatch@DescriptorMatcher@cv@@KA_NAEBV_InputArray@2@HH@Z	bool cv::DescriptorMatcher::isPossibleMatch(class cv::_InputArray const &,int,int)
//5452	?knnMatch@DescriptorMatcher@cv@@QEAAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@H0_N@Z	?knnMatch@DescriptorMatcher@cv@@QEAAXAEBV_InputArray@2@AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@H0_N@Z	void cv::DescriptorMatcher::knnMatch(class cv::_InputArray const &,class std::vector<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> >,class std::allocator<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> > > > &,int,class cv::_InputArray const &,bool)
//5453	?knnMatch@DescriptorMatcher@cv@@QEBAXAEBV_InputArray@debug_build_guard@2@0AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@H0_N@Z	?knnMatch@DescriptorMatcher@cv@@QEBAXAEBV_InputArray@2@0AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@H0_N@Z	void cv::DescriptorMatcher::knnMatch(class cv::_InputArray const &,class cv::_InputArray const &,class std::vector<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> >,class std::allocator<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> > > > &,int,class cv::_InputArray const &,bool)
//5563	?match@DescriptorMatcher@cv@@QEAAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@0@Z	?match@DescriptorMatcher@cv@@QEAAXAEBV_InputArray@2@AEAV?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@0@Z	void cv::DescriptorMatcher::match(class cv::_InputArray const &,class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> > &,class cv::_InputArray const &)
//5564	?match@DescriptorMatcher@cv@@QEBAXAEBV_InputArray@debug_build_guard@2@0AEAV?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@0@Z	?match@DescriptorMatcher@cv@@QEBAXAEBV_InputArray@2@0AEAV?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@0@Z	void cv::DescriptorMatcher::match(class cv::_InputArray const &,class cv::_InputArray const &,class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> > &,class cv::_InputArray const &)
//5982	?radiusMatch@DescriptorMatcher@cv@@QEAAXAEBV_InputArray@debug_build_guard@2@AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@M0_N@Z	?radiusMatch@DescriptorMatcher@cv@@QEAAXAEBV_InputArray@2@AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@M0_N@Z	void cv::DescriptorMatcher::radiusMatch(class cv::_InputArray const &,class std::vector<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> >,class std::allocator<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> > > > &,float,class cv::_InputArray const &,bool)
//5983	?radiusMatch@DescriptorMatcher@cv@@QEBAXAEBV_InputArray@debug_build_guard@2@0AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@M0_N@Z	?radiusMatch@DescriptorMatcher@cv@@QEBAXAEBV_InputArray@2@0AEAV?$vector@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@V?$allocator@V?$vector@VDMatch@cv@@V?$allocator@VDMatch@cv@@@std@@@std@@@2@@std@@M0_N@Z	void cv::DescriptorMatcher::radiusMatch(class cv::_InputArray const &,class cv::_InputArray const &,class std::vector<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> >,class std::allocator<class std::vector<class cv::DMatch,class std::allocator<class cv::DMatch> > > > &,float,class cv::_InputArray const &,bool)
//5994	?read@DescriptorMatcher@cv@@QEAAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	?read@DescriptorMatcher@cv@@QEAAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	void cv::DescriptorMatcher::read(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
//5995	?read@DescriptorMatcher@cv@@UEAAXAEBVFileNode@2@@Z	?read@DescriptorMatcher@cv@@UEAAXAEBVFileNode@2@@Z	void cv::DescriptorMatcher::read(class cv::FileNode const &)
//6254	?set@DescriptorCollection@DescriptorMatcher@cv@@QEAAXAEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z	?set@DescriptorCollection@DescriptorMatcher@cv@@QEAAXAEBV?$vector@VMat@cv@@V?$allocator@VMat@cv@@@std@@@std@@@Z	void cv::DescriptorMatcher::DescriptorCollection::set(class std::vector<class cv::Mat,class std::allocator<class cv::Mat> > const &)
//6467	?size@DescriptorCollection@DescriptorMatcher@cv@@QEBAHXZ	?size@DescriptorCollection@DescriptorMatcher@cv@@QEBAHXZ	int cv::DescriptorMatcher::DescriptorCollection::size(void)
//6626	?train@DescriptorMatcher@cv@@UEAAXXZ	?train@DescriptorMatcher@cv@@UEAAXXZ	void cv::DescriptorMatcher::train(void)
//6863	?write@DescriptorMatcher@cv@@QEBAXAEBU?$Ptr@VFileStorage@cv@@@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	?write@DescriptorMatcher@cv@@QEBAXAEBU?$Ptr@VFileStorage@cv@@@2@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	void cv::DescriptorMatcher::write(struct cv::Ptr<class cv::FileStorage> const &,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
//6864	?write@DescriptorMatcher@cv@@QEBAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	?write@DescriptorMatcher@cv@@QEBAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z	void cv::DescriptorMatcher::write(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
//6865	?write@DescriptorMatcher@cv@@UEBAXAEAVFileStorage@2@@Z	?write@DescriptorMatcher@cv@@UEBAXAEAVFileStorage@2@@Z	void cv::DescriptorMatcher::write(class cv::FileStorage &)

