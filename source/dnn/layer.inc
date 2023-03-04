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

{$if not defined(OPENCV_DNN_LAYER_HPP)}
{$define OPENCV_DNN_LAYER_HPP}

{$if not defined(OPENCV_DNN_DNN_HPP)}{$i 'dnn/dnn.inc'}{$ifend}

//namespace cv {
//namespace dnn {
//CV__DNN_INLINE_NS_BEGIN
//! @addtogroup dnn
//! @{
//!
//! @defgroup dnnLayerFactory Utilities for New Layers Registration
//! @{

(** @brief %Layer factory allows to create instances of registered layers. *)
//class CV_EXPORTS LayerFactory
//{
//public:

    //! Each Layer class must provide this function to the factory
//    typedef Ptr<Layer>(*Constructor)(LayerParams &params);

    //! Registers the layer class with typename @p type and specified @p constructor. Thread-safe.
//    static void registerLayer(const String &type, Constructor constructor);

    //! Unregisters registered layer with specified type name. Thread-safe.
//    static void unregisterLayer(const String &type);

    (** @brief Creates instance of registered layer.
     *  @param type type name of creating layer.
     *  @param params parameters which will be used for layer initialization.
     *  @note Thread-safe.
     *)
//    static Ptr<Layer> createLayerInstance(const String &type, LayerParams& params);

//private:
//    LayerFactory();
//};

//! @}
//! @}
//CV__DNN_INLINE_NS_END
//}
//}

{$ifend OPENCV_DNN_LAYER_HPP}
