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

{$ifndef OPENCV_DNN_VERSION_HPP}
{$define OPENCV_DNN_VERSION_HPP}

/// Use with major OpenCV version only.
const
//{$define OPENCV_DNN_API_VERSION 20211220
  OPENCV_DNN_API_VERSION = 20211220;

//{$if !defined CV_DOXYGEN && !defined CV_STATIC_ANALYSIS && !defined CV_DNN_DONT_ADD_INLINE_NS}
//{$define CV__DNN_INLINE_NS __CV_CAT(dnn4_v, OPENCV_DNN_API_VERSION)}
//{$define CV__DNN_INLINE_NS_BEGIN namespace CV__DNN_INLINE_NS {
//{$define CV__DNN_INLINE_NS_END }
//namespace cv { namespace dnn { namespace CV__DNN_INLINE_NS { } using namespace CV__DNN_INLINE_NS; }}
//{$else
//{$define CV__DNN_INLINE_NS_BEGIN}
//{$define CV__DNN_INLINE_NS_END}
//{$endif}

{$endif OPENCV_DNN_VERSION_HPP}
