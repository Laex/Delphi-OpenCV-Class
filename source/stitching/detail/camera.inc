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

{$ifndef OPENCV_STITCHING_CAMERA_HPP}
{$define OPENCV_STITCHING_CAMERA_HPP}

{$i 'core.inc'}

//namespace cv {
//namespace detail {

//! @addtogroup stitching
//! @{

(** @brief Describes camera parameters.

@note Translation is assumed to be zero during the whole stitching pipeline. :
 *)
//struct CV_EXPORTS_W_SIMPLE CameraParams
//{
//    CameraParams();
//    CameraParams(const CameraParams& other);
//    CameraParams& operator =(const CameraParams& other);
//    CV_WRAP Mat K() const;
//
//    CV_PROP_RW double focal; // Focal length
//    CV_PROP_RW double aspect; // Aspect ratio
//    CV_PROP_RW double ppx; // Principal point X
//    CV_PROP_RW double ppy; // Principal point Y
//    CV_PROP_RW Mat R; // Rotation
//    CV_PROP_RW Mat t; // Translation
//};

//! @}

//} // namespace detail
//} // namespace cv

{$endif OPENCV_STITCHING_CAMERA_HPP}
