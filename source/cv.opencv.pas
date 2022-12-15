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

Uses
   System.Math,
{$IFDEF USE_TYPEINFO}
   System.TypInfo,
   System.Rtti,
{$ENDIF}
   cpp.utils;

{$I cvconfig.inc}

{$define OPENCV_ALL_HPP}

// File that defines what modules where included during the build of OpenCV
// These are purely the defines of the correct HAVE_OPENCV_modulename values
{$I opencv_modules.inc}
// Then the list of defines is checked to include the correct headers
// Core library is always included --> without no OpenCV functionality available
{$I core.inc} {done, no classes}
// Then the optional modules are checked
{$IFDEF HAVE_OPENCV_CALIB3D}
{.$I calib3d.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_FEATURES2D}
{$I features2d.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_DNN}
{$I dnn.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_FLANN}
{$I flann.inc}
{$endif}
{$IFDEF HAVE_OPENCV_HIGHGUI}
{$I highgui.inc} {done}
{$ENDIF}
{$IFDEF HAVE_OPENCV_IMGCODECS}
{$I imgcodecs.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_IMGPROC}
{$I imgproc.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_ML}
{$I ml.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_OBJDETECT}
{$I objdetect.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_PHOTO}
{$I photo.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_STITCHING}
{$I stitching.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_VIDEO}
{$I video.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_VIDEOIO}
{$I videoio.inc}
{$ENDIF}

implementation

{.$I opencv_modules.impl.inc}
// Then the list of defines is checked to include the correct headers
// Core library is always included --> without no OpenCV functionality available
{$I core.impl.inc}
// Then the optional modules are checked
{$IFDEF HAVE_OPENCV_CALIB3D}
{.$I calib3d.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_FEATURES2D}
{$I features2d.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_DNN}
{$I dnn.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_FLANN}
{$I flann.impl.inc}
{$endif}
{$IFDEF HAVE_OPENCV_HIGHGUI}
{$I highgui.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_IMGCODECS}
{$I imgcodecs.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_IMGPROC}
{$I imgproc.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_ML}
{$I ml.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_OBJDETECT}
{$I objdetect.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_PHOTO}
{$I photo.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_STITCHING}
{$I stitching.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_VIDEO}
{$I video.impl.inc}
{$ENDIF}
{$IFDEF HAVE_OPENCV_VIDEOIO}
{$I videoio.impl.inc}
{$ENDIF}

initialization

{$I core.init.inc}

end.
