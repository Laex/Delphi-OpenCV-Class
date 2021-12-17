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

{$ifndef OPENCV_FLANN_SAVING_H_}
{$define OPENCV_FLANN_SAVING_H_}

//! @cond IGNORED

//#include <cstring>
//#include <vector>

//#include "general.h"
//#include "nn_index.h"

//#ifdef FLANN_SIGNATURE_
//#undef FLANN_SIGNATURE_
//#endif
//#define FLANN_SIGNATURE_ "FLANN_INDEX"

//namespace cvflann
//{

//template <typename T>
//struct Datatype {};
//template<>
//struct Datatype<char> { static flann_datatype_t type() { return FLANN_INT8; } };
//template<>
//struct Datatype<short> { static flann_datatype_t type() { return FLANN_INT16; } };
//template<>
//struct Datatype<int> { static flann_datatype_t type() { return FLANN_INT32; } };
//template<>
//struct Datatype<unsigned char> { static flann_datatype_t type() { return FLANN_UINT8; } };
//template<>
//struct Datatype<unsigned short> { static flann_datatype_t type() { return FLANN_UINT16; } };
//template<>
//struct Datatype<unsigned int> { static flann_datatype_t type() { return FLANN_UINT32; } };
//template<>
//struct Datatype<float> { static flann_datatype_t type() { return FLANN_FLOAT32; } };
//template<>
//struct Datatype<double> { static flann_datatype_t type() { return FLANN_FLOAT64; } };
//
//
//(**
// * Structure representing the index header.
// *)
//struct IndexHeader
//{
//    char signature[16];
//    char version[16];
//    flann_datatype_t data_type;
//    flann_algorithm_t index_type;
//    size_t rows;
//    size_t cols;
//};
//
//(**
// * Saves index header to stream
// *
// * @param stream - Stream to save to
// * @param index - The index to save
// *)
//template<typename Distance>
//void save_header(FILE* stream, const NNIndex<Distance>& index)
//{
//    IndexHeader header;
//    memset(header.signature, 0, sizeof(header.signature));
//    strcpy(header.signature, FLANN_SIGNATURE_);
//    memset(header.version, 0, sizeof(header.version));
//    strcpy(header.version, FLANN_VERSION_);
//    header.data_type = Datatype<typename Distance::ElementType>::type();
//    header.index_type = index.getType();
//    header.rows = index.size();
//    header.cols = index.veclen();
//
//    std::fwrite(&header, sizeof(header),1,stream);
//}
//
//
//(**
// *
// * @param stream - Stream to load from
// * @return Index header
// *)
//inline IndexHeader load_header(FILE* stream)
//{
//    IndexHeader header;
//    size_t read_size = fread(&header,sizeof(header),1,stream);
//
//    if (read_size!=(size_t)1) {
//        FLANN_THROW(cv::Error::StsError, "Invalid index file, cannot read");
//    }
//
//    if (strcmp(header.signature,FLANN_SIGNATURE_)!=0) {
//        FLANN_THROW(cv::Error::StsError, "Invalid index file, wrong signature");
//    }
//
//    return header;
//
//}
//
//
//template<typename T>
//void save_value(FILE* stream, const T& value, size_t count = 1)
//{
//    fwrite(&value, sizeof(value),count, stream);
//}
//
//template<typename T>
//void save_value(FILE* stream, const cvflann::Matrix<T>& value)
//{
//    fwrite(&value, sizeof(value),1, stream);
//    fwrite(value.data, sizeof(T),value.rows*value.cols, stream);
//}
//
//template<typename T>
//void save_value(FILE* stream, const std::vector<T>& value)
//{
//    size_t size = value.size();
//    fwrite(&size, sizeof(size_t), 1, stream);
//    fwrite(&value[0], sizeof(T), size, stream);
//}
//
//template<typename T>
//void load_value(FILE* stream, T& value, size_t count = 1)
//{
//    size_t read_cnt = fread(&value, sizeof(value), count, stream);
//    if (read_cnt != count) {
//        FLANN_THROW(cv::Error::StsParseError, "Cannot read from file");
//    }
//}
//
//template<typename T>
//void load_value(FILE* stream, cvflann::Matrix<T>& value)
//{
//    size_t read_cnt = fread(&value, sizeof(value), 1, stream);
//    if (read_cnt != 1) {
//        FLANN_THROW(cv::Error::StsParseError, "Cannot read from file");
//    }
//    value.data = new T[value.rows*value.cols];
//    read_cnt = fread(value.data, sizeof(T), value.rows*value.cols, stream);
//    if (read_cnt != (size_t)(value.rows*value.cols)) {
//        FLANN_THROW(cv::Error::StsParseError, "Cannot read from file");
//    }
//}
//
//
//template<typename T>
//void load_value(FILE* stream, std::vector<T>& value)
//{
//    size_t size;
//    size_t read_cnt = fread(&size, sizeof(size_t), 1, stream);
//    if (read_cnt!=1) {
//        FLANN_THROW(cv::Error::StsError, "Cannot read from file");
//    }
//    value.resize(size);
//    read_cnt = fread(&value[0], sizeof(T), size, stream);
//    if (read_cnt != size) {
//        FLANN_THROW(cv::Error::StsError, "Cannot read from file");
//    }
//}
//
//}

//! @endcond

{$endif OPENCV_FLANN_SAVING_H_}
