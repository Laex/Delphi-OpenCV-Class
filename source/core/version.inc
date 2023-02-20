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

{$IFNDEF OPENCV_VERSION_HPP}
{$DEFINE OPENCV_VERSION_HPP}
{$I opt.inc}

const
{$IFDEF OPENCV_455}
  CV_VERSION_MAJOR    = '4';
  CV_VERSION_MINOR    = '5';
  CV_VERSION_REVISION = '5';
  CV_VERSION_STATUS   = '';
  (* old  style version constants *)
  CV_MAJOR_VERSION    = 4; // CV_VERSION_MAJOR;
  CV_MINOR_VERSION    = 5; // CV_VERSION_MINOR;
  CV_SUBMINOR_VERSION = 4; // CV_VERSION_REVISION;
{$ENDIF}
{$IFDEF OPENCV_460}
  CV_VERSION_MAJOR    = '4';
  CV_VERSION_MINOR    = '6';
  CV_VERSION_REVISION = '0';
  CV_VERSION_STATUS   = '';
  (* old  style version constants *)
  CV_MAJOR_VERSION    = 4; // CV_VERSION_MAJOR;
  CV_MINOR_VERSION    = 6; // CV_VERSION_MINOR;
  CV_SUBMINOR_VERSION = 0; // CV_VERSION_REVISION;
{$ENDIF}
{$IFDEF OPENCV_470}
  CV_VERSION_MAJOR    = '4';
  CV_VERSION_MINOR    = '7';
  CV_VERSION_REVISION = '0';
  CV_VERSION_STATUS   = '';
  (* old  style version constants *)
  CV_MAJOR_VERSION    = 4; // CV_VERSION_MAJOR;
  CV_MINOR_VERSION    = 7; // CV_VERSION_MINOR;
  CV_SUBMINOR_VERSION = 0; // CV_VERSION_REVISION;
{$ENDIF}
  CV_VERSION = CV_VERSION_MAJOR + '.' + CV_VERSION_MINOR + '.' + CV_VERSION_REVISION + CV_VERSION_STATUS;

  cvdllversion      = CV_VERSION_MAJOR + CV_VERSION_MINOR + CV_VERSION_REVISION;
  opencv_delphi_dll = 'opencv_delphi' + cvdllversion + {$IFDEF USE_DEBUG_DLLS} 'd' + {$ENDIF} '.dll';
  opencv_world_dll  = 'opencv_world' + cvdllversion + {$IFDEF USE_DEBUG_DLLS} 'd' + {$ENDIF} '.dll';

{$ENDIF OPENCV_VERSION_HPP}
