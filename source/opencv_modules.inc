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

{$IFNDEF OPENCV_opencv_modules_HPP}
{$DEFINE OPENCV_opencv_modules_HPP}


// This definition means that OpenCV is built with enabled non-free code.
// For example, patented algorithms for non-profit/non-commercial use only.
(* #undef OPENCV_ENABLE_NONFREE *)

{$DEFINE HAVE_OPENCV_CALIB3D}
{$DEFINE HAVE_OPENCV_CORE}
{$DEFINE HAVE_OPENCV_DNN}
{$DEFINE HAVE_OPENCV_FEATURES2D}
{$DEFINE HAVE_OPENCV_FLANN}
{$DEFINE HAVE_OPENCV_GAPI}
{$DEFINE HAVE_OPENCV_HIGHGUI}
{$DEFINE HAVE_OPENCV_IMGCODECS}
{$DEFINE HAVE_OPENCV_IMGPROC}
{$DEFINE HAVE_OPENCV_ML}
{$DEFINE HAVE_OPENCV_OBJDETECT}
{$DEFINE HAVE_OPENCV_PHOTO}
{$DEFINE HAVE_OPENCV_STITCHING}
{$DEFINE HAVE_OPENCV_VIDEO}
{$DEFINE HAVE_OPENCV_VIDEOIO}
{$DEFINE HAVE_OPENCV_WORLD}

{$ENDIF OPENCV_opencv_modules_HPP}