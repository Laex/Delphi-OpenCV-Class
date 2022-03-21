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

// 690	??0KalmanFilter@cv@@QEAA@$$QEAV01@@Z	??0KalmanFilter@cv@@QEAA@$$QEAV01@@Z	cv::KalmanFilter::KalmanFilter(class cv::KalmanFilter &&)
// 691	??0KalmanFilter@cv@@QEAA@AEBV01@@Z	??0KalmanFilter@cv@@QEAA@AEBV01@@Z	cv::KalmanFilter::KalmanFilter(class cv::KalmanFilter const &)
{
  692
  ??0KalmanFilter@cv@@QEAA@HHHH@Z
  ??0KalmanFilter@cv@@QEAA@HHHH@Z
  cv::KalmanFilter::KalmanFilter(int,int,int,int)
}
procedure Constructor_KalmanFilter(Var Obj: TKalmanFilter; const dynamParams, measureParams: int; const controlParams: int = 0; const &type: int = CV_32F); overload;
  external opencv_world_dll name '??0KalmanFilter@cv@@QEAA@HHHH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// 693	??0KalmanFilter@cv@@QEAA@XZ	??0KalmanFilter@cv@@QEAA@XZ	cv::KalmanFilter::KalmanFilter(void)

{
  5866
  ?predict@KalmanFilter@cv@@QEAAAEBVMat@2@AEBV32@@Z
  ?predict@KalmanFilter@cv@@QEAAAEBVMat@2@AEBV32@@Z
  class cv::Mat const & cv::KalmanFilter::predict(class cv::Mat const &)
}
function KalmanFilter_predict(const Obj: TKalmanFilter; const control: TMat): TMat; external opencv_world_dll name '?predict@KalmanFilter@cv@@QEAAAEBVMat@2@AEBV32@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
  4092
  ?correct@KalmanFilter@cv@@QEAAAEBVMat@2@AEBV32@@Z
  ?correct@KalmanFilter@cv@@QEAAAEBVMat@2@AEBV32@@Z
  class cv::Mat const & cv::KalmanFilter::correct(class cv::Mat const &)
}
function KalmanFilter_correct(const Obj: TKalmanFilter; const measurement: TMat): TMat; external opencv_world_dll name '?correct@KalmanFilter@cv@@QEAAAEBVMat@2@AEBV32@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
