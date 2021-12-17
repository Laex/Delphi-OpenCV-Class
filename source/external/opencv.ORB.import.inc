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

//877	??0ORB@cv@@QEAA@$$QEAV01@@Z	??0ORB@cv@@QEAA@$$QEAV01@@Z	cv::ORB::ORB(class cv::ORB &&)
//878	??0ORB@cv@@QEAA@AEBV01@@Z	??0ORB@cv@@QEAA@AEBV01@@Z	cv::ORB::ORB(class cv::ORB const &)
//879	??0ORB@cv@@QEAA@XZ	??0ORB@cv@@QEAA@XZ	cv::ORB::ORB(void)
//1672	??1ORB@cv@@UEAA@XZ	??1ORB@cv@@UEAA@XZ	cv::ORB::~ORB(void)
//2332	??4ORB@cv@@QEAAAEAV01@$$QEAV01@@Z	??4ORB@cv@@QEAAAEAV01@$$QEAV01@@Z	class cv::ORB & cv::ORB::operator=(class cv::ORB &&)
//2333	??4ORB@cv@@QEAAAEAV01@AEBV01@@Z	??4ORB@cv@@QEAAAEAV01@AEBV01@@Z	class cv::ORB & cv::ORB::operator=(class cv::ORB const &)
//3191	??_7ORB@cv@@6BAlgorithm@1@@	??_7ORB@cv@@6BAlgorithm@1@@	const cv::ORB::`vftable'{for `cv::Algorithm'}
//3192	??_7ORB@cv@@6BFeature2D@1@@	??_7ORB@cv@@6BFeature2D@1@@	const cv::ORB::`vftable'{for `cv::Feature2D'}
{
4195
?create@ORB@cv@@SA?AU?$Ptr@VORB@cv@@@2@HMHHHHW4ScoreType@12@HH@Z
?create@ORB@cv@@SA?AU?$Ptr@VORB@cv@@@2@HMHHHHW4ScoreType@12@HH@Z
struct cv::Ptr<class cv::ORB> cv::ORB::create(int,float,int,int,int,int,enum cv::ORB::ScoreType,int,int)
}
function create_ORB(nfeatures:int=500; scaleFactor:float=1.2; nlevels:int=8; edgeThreshold:int=31;
        firstLevel:int=0; WTA_K:int=2; scoreType:int = Int(HARRIS_SCORE)
        {TORB.ScoreType=TORB.HARRIS_SCORE}; patchSize:int=31; fastThreshold:int=20): TPtr<TFeature2D>;
overload; external opencv_world_dll
{$IFDEF DEBUG}
name '?create@ORB@cv@@SA?AU?$Ptr@VORB@cv@@@2@HMHHHHW4ScoreType@12@HH@Z'
{$ELSE}
name '?create@ORB@cv@@SA?AU?$Ptr@VORB@cv@@@2@HMHHHHW4ScoreType@12@HH@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//4911	?getDefaultName@ORB@cv@@UEBA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@XZ	?getDefaultName@ORB@cv@@UEBA?AV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@XZ	class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > cv::ORB::getDefaultName(void)
//5430	?kBytes@ORB@cv@@2HB	?kBytes@ORB@cv@@2HB	int const cv::ORB::kBytes
