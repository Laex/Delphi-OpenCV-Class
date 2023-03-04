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

{$IF not defined(OPENCV_DNN_UTILS_INF_ENGINE_HPP)}
{$DEFINE OPENCV_DNN_UTILS_INF_ENGINE_HPP}

{$if not defined(OPENCV_DNN_DNN_HPP)}{$i 'dnn/dnn.inc'}{$ifend}


// namespace cv { namespace dnn {
// CV__DNN_INLINE_NS_BEGIN

const
  (* Values for 'OPENCV_DNN_BACKEND_INFERENCE_ENGINE_TYPE' parameter *)
  CV_DNN_BACKEND_INFERENCE_ENGINE_NN_BUILDER_API = 'NN_BUILDER';
  CV_DNN_BACKEND_INFERENCE_ENGINE_NGRAPH         = 'NGRAPH';

  (* * @brief Returns Inference Engine internal backend API.
    *
    * See values of `CV_DNN_BACKEND_INFERENCE_ENGINE_*` macros.
    *
    * Default value is controlled through `OPENCV_DNN_BACKEND_INFERENCE_ENGINE_TYPE` runtime parameter (environment variable).
  *)
  // CV_EXPORTS_W cv::String getInferenceEngineBackendType();

  (* * @brief Specify Inference Engine internal backend API.
    *
    * See values of `CV_DNN_BACKEND_INFERENCE_ENGINE_*` macros.
    *
    * @returns previous value of internal backend API
  *)
  // CV_EXPORTS_W cv::String setInferenceEngineBackendType(const cv::String& newBackendType);

  (* * @brief Release a Myriad device (binded by OpenCV).
    *
    * Single Myriad device cannot be shared across multiple processes which uses
    * Inference Engine's Myriad plugin.
  *)
  // CV_EXPORTS_W void resetMyriadDevice();

const
  (* Values for 'OPENCV_DNN_IE_VPU_TYPE' parameter *)
  CV_DNN_INFERENCE_ENGINE_VPU_TYPE_UNSPECIFIED = '';
  /// Intel(R) Movidius(TM) Neural Compute Stick, NCS (USB 03e7:2150), Myriad2 (https://software.intel.com/en-us/movidius-ncs)
  CV_DNN_INFERENCE_ENGINE_VPU_TYPE_MYRIAD_2 = 'Myriad2';
  /// Intel(R) Neural Compute Stick 2, NCS2 (USB 03e7:2485), MyriadX (https://software.intel.com/ru-ru/neural-compute-stick)
  CV_DNN_INFERENCE_ENGINE_VPU_TYPE_MYRIAD_X    = 'MyriadX';
  CV_DNN_INFERENCE_ENGINE_CPU_TYPE_ARM_COMPUTE = 'ARM_COMPUTE';
  CV_DNN_INFERENCE_ENGINE_CPU_TYPE_X86         = 'X86';

  (* * @brief Returns Inference Engine VPU type.
    *
    * See values of `CV_DNN_INFERENCE_ENGINE_VPU_TYPE_*` macros.
  *)
  // CV_EXPORTS_W cv::String getInferenceEngineVPUType();

  (* * @brief Returns Inference Engine CPU type.
    *
    * Specify OpenVINO plugin: CPU or ARM.
  *)
  // CV_EXPORTS_W cv::String getInferenceEngineCPUType();

  (* * @brief Release a HDDL plugin.
  *)
  // CV_EXPORTS_W void releaseHDDLPlugin();


  // CV__DNN_INLINE_NS_END
  // }} // namespace

{$IFEND OPENCV_DNN_UTILS_INF_ENGINE_HPP}
