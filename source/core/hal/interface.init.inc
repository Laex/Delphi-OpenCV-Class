(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the 'License');
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an 'AS IS' BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

{$IF defined(OPENCV_CORE_HAL_INTERFACE_H) and defined(OPENCV_CORE_HAL_INTERFACE_H_IMPL) and (not defined(OPENCV_CORE_HAL_INTERFACE_H_INIT))}
{$DEFINE OPENCV_CORE_HAL_INTERFACE_H_INIT}

CV_8UC1 := CV_MAKETYPE(CV_8U, 1);
CV_8UC2 := CV_MAKETYPE(CV_8U, 2);
CV_8UC3 := CV_MAKETYPE(CV_8U, 3);
CV_8UC4 := CV_MAKETYPE(CV_8U, 4);

CV_8SC1 := CV_MAKETYPE(CV_8S, 1);
CV_8SC2 := CV_MAKETYPE(CV_8S, 2);
CV_8SC3 := CV_MAKETYPE(CV_8S, 3);
CV_8SC4 := CV_MAKETYPE(CV_8S, 4);

CV_16UC1 := CV_MAKETYPE(CV_16U, 1);
CV_16UC2 := CV_MAKETYPE(CV_16U, 2);
CV_16UC3 := CV_MAKETYPE(CV_16U, 3);
CV_16UC4 := CV_MAKETYPE(CV_16U, 4);

CV_16SC1 := CV_MAKETYPE(CV_16S, 1);
CV_16SC2 := CV_MAKETYPE(CV_16S, 2);
CV_16SC3 := CV_MAKETYPE(CV_16S, 3);
CV_16SC4 := CV_MAKETYPE(CV_16S, 4);

CV_32SC1 := CV_MAKETYPE(CV_32S, 1);
CV_32SC2 := CV_MAKETYPE(CV_32S, 2);
CV_32SC3 := CV_MAKETYPE(CV_32S, 3);
CV_32SC4 := CV_MAKETYPE(CV_32S, 4);

CV_32FC1 := CV_MAKETYPE(CV_32F, 1);
CV_32FC2 := CV_MAKETYPE(CV_32F, 2);
CV_32FC3 := CV_MAKETYPE(CV_32F, 3);
CV_32FC4 := CV_MAKETYPE(CV_32F, 4);

CV_64FC1 := CV_MAKETYPE(CV_64F, 1);
CV_64FC2 := CV_MAKETYPE(CV_64F, 2);
CV_64FC3 := CV_MAKETYPE(CV_64F, 3);
CV_64FC4 := CV_MAKETYPE(CV_64F, 4);

CV_16FC1 := CV_MAKETYPE(CV_16F, 1);
CV_16FC2 := CV_MAKETYPE(CV_16F, 2);
CV_16FC3 := CV_MAKETYPE(CV_16F, 3);
CV_16FC4 := CV_MAKETYPE(CV_16F, 4);

{$IFEND OPENCV_CORE_HAL_INTERFACE_H}