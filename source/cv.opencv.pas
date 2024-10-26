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

unit cv.opencv;

{$I opt.inc}

interface

uses
  System.Math,
{$IF DEFINED(USE_TYPEINFO)}
  System.TypInfo, System.Rtti,
{$IFEND}
  cpp.utils;

{$DEFINE INTERFACE}
{$I cvconfig.inc}
//
{ .$DEFINE OPENCV_ALL_HPP }
//
// File that defines what modules where included during the build of OpenCV
// These are purely the defines of the correct HAVE_OPENCV_modulename values
{$I opencv_modules.inc}
// Then the list of defines is checked to include the correct headers
// Core library is always included --> without no OpenCV functionality available
{$I core.inc}
// Then the optional modules are checked
//
{$IF DEFINED(HAVE_OPENCV_CALIB3D) or DEFINED(OPENCV_ALL_HPP)}{$I 'calib3d.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_FEATURES2D) or DEFINED(OPENCV_ALL_HPP)}{$I 'features2d.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_DNN) or DEFINED(OPENCV_ALL_HPP)}{$I 'dnn.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_FLANN) or DEFINED(OPENCV_ALL_HPP)}{$I 'flann.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_HIGHGUI) or DEFINED(OPENCV_ALL_HPP)}{$I 'highgui.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_IMGCODECS) or DEFINED(OPENCV_ALL_HPP)}{$I 'imgcodecs.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_IMGPROC) or DEFINED(OPENCV_ALL_HPP)}{$I 'imgproc.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_ML) or DEFINED(OPENCV_ALL_HPP)}{$I 'ml.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_OBJDETECT) or DEFINED(OPENCV_ALL_HPP)}{$I 'objdetect.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_PHOTO) or DEFINED(OPENCV_ALL_HPP)}{$I 'photo.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_STITCHING) or DEFINED(OPENCV_ALL_HPP)}{$I 'stitching.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_VIDEO) or DEFINED(OPENCV_ALL_HPP)}{$I 'video.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_VIDEOIO) or DEFINED(OPENCV_ALL_HPP)}{$I 'videoio.inc'}{$IFEND}

{$UNDEF INTERFACE}

implementation

uses
  cv.external;

{$DEFINE IMPLEMENTATION}
// Then the list of defines is checked to include the correct headers
// Core library is always included --> without no OpenCV functionality available
{$IF not defined(OPENCV_CORE_HPP_IMPL)}
     {$I 'core.inc'}
{$IFEND}
//
// Then the optional modules are checked
{$IF DEFINED(HAVE_OPENCV_CALIB3D)}{$I 'calib3d.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_FEATURES2D)}{$I 'features2d.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_DNN)}{$I 'dnn.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_FLANN)}{$I 'flann.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_HIGHGUI)}{$I 'highgui.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_IMGCODECS)}{$I 'imgcodecs.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_IMGPROC)}{$I 'imgproc.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_ML)}{$I 'ml.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_OBJDETECT)}{$I 'objdetect.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_PHOTO)}{$I 'photo.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_STITCHING)}{$I 'stitching.impl.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_VIDEO)}{$I 'video.inc'}{$IFEND}
{$IF DEFINED(HAVE_OPENCV_VIDEOIO)}{$I 'videoio.inc'}{$IFEND}

initialization

{$UNDEF INTERFACE}
{$UNDEF IMPLEMENTATION}
{$DEFINE INITIALIZATION}

{$I 'core.inc'}

{$IF defined(OPENCV_CORE_HAL_INTERFACE_H) and defined(OPENCV_CORE_HAL_INTERFACE_H_IMPL)}
     {$I 'core/hal/interface.init.inc'}
{$IFEND}

end.
