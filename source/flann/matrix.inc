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

{$ifndef OPENCV_FLANN_DATASET_H_}
{$define OPENCV_FLANN_DATASET_H_}

//! @cond IGNORED

//#include <stdio.h>

//namespace cvflann
//{

(**
 * Class that implements a simple rectangular matrix stored in a memory buffer and
 * provides convenient matrix-like access using the [] operators.
 *)
//template <typename T>
//class Matrix
//{
//public:
//    typedef T type;
//
//    size_t rows;
//    size_t cols;
//    size_t stride;
//    T* data;
//
//    Matrix() : rows(0), cols(0), stride(0), data(NULL)
//    {
//    }
//
//    Matrix(T* data_, size_t rows_, size_t cols_, size_t stride_ = 0) :
//        rows(rows_), cols(cols_),  stride(stride_), data(data_)
//    {
//        if (stride==0) stride = cols;
//    }
//
//    (**
//     * Convenience function for deallocating the storage data.
//     *)
//    CV_DEPRECATED void free()
//    {
//        fprintf(stderr, "The cvflann::Matrix<T>::free() method is deprecated "
//                "and it does not do any memory deallocation any more.  You are"
//                "responsible for deallocating the matrix memory (by doing"
//                "'delete[] matrix.data' for example)");
//    }
//
//    (**
//     * Operator that return a (pointer to a) row of the data.
//     *)
//    T* operator[](size_t index) const
//    {
//        return data+index*stride;
//    }
//};


//class UntypedMatrix
//{
//public:
//    size_t rows;
//    size_t cols;
//    void* data;
//    flann_datatype_t type;
//
//    UntypedMatrix(void* data_, long rows_, long cols_) :
//        rows(rows_), cols(cols_), data(data_)
//    {
//    }
//
//    ~UntypedMatrix()
//    {
//    }
//
//
//    template<typename T>
//    Matrix<T> as()
//    {
//        return Matrix<T>((T*)data, rows, cols);
//    }
//};

//}

//! @endcond

{$endif OPENCV_FLANN_DATASET_H_}
