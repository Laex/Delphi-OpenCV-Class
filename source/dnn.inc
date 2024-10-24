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

{$IFDEF INTERFACE}

{$if not defined(OPENCV_DNN_HPP)}
{$define OPENCV_DNN_HPP}

// This is an umbrella header to include into you project.
// We are free to change headers layout in dnn subfolder, so please include
// this header for future compatibility


(** @defgroup dnn Deep Neural Network module
  @{
    This module contains:
        - API for new layers creation, layers are building bricks of neural networks;
        - set of built-in most-useful Layers;
        - API to construct and modify comprehensive neural networks from layers;
        - functionality for loading serialized networks models from different frameworks.

    Functionality of this module is designed only for forward pass computations (i.e. network testing).
    A network training is in principle not supported.
  @}
*)
(** @example samples/dnn/classification.cpp
Check @ref tutorial_dnn_googlenet "the corresponding tutorial" for more details
*)
(** @example samples/dnn/colorization.cpp
*)
(** @example samples/dnn/object_detection.cpp
Check @ref tutorial_dnn_yolo "the corresponding tutorial" for more details
*)
(** @example samples/dnn/openpose.cpp
*)
(** @example samples/dnn/segmentation.cpp
*)
(** @example samples/dnn/text_detection.cpp
*)
{$if not defined(OPENCV_DNN_DNN_HPP)}{$i 'dnn/dnn.inc'}{$ifend}

{$ifend OPENCV_DNN_HPP}

{$ENDIF INTERFACE}

// ===========================================================================

{$IFDEF IMPLEMENTATION}

{$if defined(OPENCV_DNN_HPP) and (not defined(OPENCV_DNN_HPP_IMPL))}
{$define OPENCV_DNN_HPP_IMPL}

{$include 'dnn/dnn.impl.inc'}

{$ifend}

{$ENDIF IMPLEMENTATION}

// ============================================================================

{$IFDEF INITIALIZATION}


{$ENDIF IINITIALIZATION}