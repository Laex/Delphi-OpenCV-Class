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

{$ifndef OPENCV_CORE_CUDA_TYPES_HPP}
{$define OPENCV_CORE_CUDA_TYPES_HPP}

//#ifndef __cplusplus
//#  error cuda_types.hpp header must be compiled as C++
//#endif

//#if defined(__OPENCV_BUILD) && defined(__clang__)
//#pragma clang diagnostic ignored "-Winconsistent-missing-override"
//#endif
//#if defined(__OPENCV_BUILD) && defined(__GNUC__) && __GNUC__ >= 5
//#pragma GCC diagnostic ignored "-Wsuggest-override"
//#endif

(** @file
 * @deprecated Use @ref cudev instead.
 *)

//! @cond IGNORED

//#ifdef __CUDACC__
//    #define __CV_CUDA_HOST_DEVICE__ __host__ __device__ __forceinline__
//#else
//    #define __CV_CUDA_HOST_DEVICE__
//#endif

//namespace cv
//{
//    namespace cuda
//    {

        // Simple lightweight structures that encapsulates information about an image on device.
        // It is intended to pass to nvcc-compiled code. GpuMat depends on headers that nvcc can't compile


//        template <typename T> struct DevPtr
//        {
//            typedef T elem_type;
//            typedef int index_type;
//
//            enum { elem_size = sizeof(elem_type) };
//
//            T* data;
//
//            __CV_CUDA_HOST_DEVICE__ DevPtr() : data(0) {}
//            __CV_CUDA_HOST_DEVICE__ DevPtr(T* data_) : data(data_) {}
//
//            __CV_CUDA_HOST_DEVICE__ size_t elemSize() const { return elem_size; }
//            __CV_CUDA_HOST_DEVICE__ operator       T*()       { return data; }
//            __CV_CUDA_HOST_DEVICE__ operator const T*() const { return data; }
//        };
//
//        template <typename T> struct PtrSz : public DevPtr<T>
//        {
//            __CV_CUDA_HOST_DEVICE__ PtrSz() : size(0) {}
//            __CV_CUDA_HOST_DEVICE__ PtrSz(T* data_, size_t size_) : DevPtr<T>(data_), size(size_) {}
//
//            size_t size;
//        };
//
//        template <typename T> struct PtrStep : public DevPtr<T>
//        {
//            __CV_CUDA_HOST_DEVICE__ PtrStep() : step(0) {}
//            __CV_CUDA_HOST_DEVICE__ PtrStep(T* data_, size_t step_) : DevPtr<T>(data_), step(step_) {}
//
//            size_t step;
//
//            __CV_CUDA_HOST_DEVICE__       T* ptr(int y = 0)       { return (      T*)( (      char*)(((DevPtr<T>*)this)->data) + y * step); }
//            __CV_CUDA_HOST_DEVICE__ const T* ptr(int y = 0) const { return (const T*)( (const char*)(((DevPtr<T>*)this)->data) + y * step); }
//
//            __CV_CUDA_HOST_DEVICE__       T& operator ()(int y, int x)       { return ptr(y)[x]; }
//            __CV_CUDA_HOST_DEVICE__ const T& operator ()(int y, int x) const { return ptr(y)[x]; }
//        };
//
//        template <typename T> struct PtrStepSz : public PtrStep<T>
//        {
//            __CV_CUDA_HOST_DEVICE__ PtrStepSz() : cols(0), rows(0) {}
//            __CV_CUDA_HOST_DEVICE__ PtrStepSz(int rows_, int cols_, T* data_, size_t step_)
//                : PtrStep<T>(data_, step_), cols(cols_), rows(rows_) {}
//
//            template <typename U>
//            explicit PtrStepSz(const PtrStepSz<U>& d) : PtrStep<T>((T*)d.data, d.step), cols(d.cols), rows(d.rows){}
//
//            int cols;
//            int rows;
//        };
//
//        typedef PtrStepSz<unsigned char> PtrStepSzb;
//        typedef PtrStepSz<unsigned short> PtrStepSzus;
//        typedef PtrStepSz<float> PtrStepSzf;
//        typedef PtrStepSz<int> PtrStepSzi;
//
//        typedef PtrStep<unsigned char> PtrStepb;
//        typedef PtrStep<unsigned short> PtrStepus;
//        typedef PtrStep<float> PtrStepf;
//        typedef PtrStep<int> PtrStepi;
//
//    }
//}

//! @endcond

{$endif OPENCV_CORE_CUDA_TYPES_HPP}
