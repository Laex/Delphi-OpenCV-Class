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

{$ifndef OPENCV_FLANN_DEFINES_H_}
{$define OPENCV_FLANN_DEFINES_H_}

//! @cond IGNORED

{$I 'flann/config.inc'}

//#ifdef FLANN_EXPORT
//#undef FLANN_EXPORT
//#endif
//#ifdef _WIN32
///* win32 dll export/import directives *)
// #ifdef FLANN_EXPORTS
//  #define FLANN_EXPORT __declspec(dllexport)
// #elif defined(FLANN_STATIC)
//  #define FLANN_EXPORT
// #else
//  #define FLANN_EXPORT __declspec(dllimport)
// #endif
//#else
///* unix needs nothing *)
// #define FLANN_EXPORT
//#endif


//#undef FLANN_PLATFORM_32_BIT
//#undef FLANN_PLATFORM_64_BIT
//#if defined __amd64__ || defined __x86_64__ || defined _WIN64 || defined _M_X64
//#define FLANN_PLATFORM_64_BIT
//#else
//#define FLANN_PLATFORM_32_BIT
//#endif


//#undef FLANN_ARRAY_LEN
//#define FLANN_ARRAY_LEN(a) (sizeof(a)/sizeof(a[0]))

//namespace cvflann {

type
(* Nearest neighbour index algorithms *)
 flann_algorithm_t = (//
    FLANN_INDEX_LINEAR = 0,          //
    FLANN_INDEX_KDTREE = 1,          //
    FLANN_INDEX_KMEANS = 2,          //
    FLANN_INDEX_COMPOSITE = 3,       //
    FLANN_INDEX_KDTREE_SINGLE = 4,   //
    FLANN_INDEX_HIERARCHICAL = 5,    //
    FLANN_INDEX_LSH = 6,             //
    FLANN_INDEX_SAVED = 254,         //
    FLANN_INDEX_AUTOTUNED = 255      //

    // deprecated constants, should use the FLANN_INDEX_* ones instead
//    LINEAR = 0,                      //
//    KDTREE = 1,                      //
//    KMEANS = 2,                      //
//    COMPOSITE = 3,                   //
//    KDTREE_SINGLE = 4,               //
//    SAVED = 254,                     //
//    AUTOTUNED = 255                  //
);



flann_centers_init_t =(//
    FLANN_CENTERS_RANDOM = 0,        //
    FLANN_CENTERS_GONZALES = 1,      //
    FLANN_CENTERS_KMEANSPP = 2,      //
    FLANN_CENTERS_GROUPWISE = 3,     //

    // deprecated constants, should use the FLANN_CENTERS_* ones instead
    CENTERS_RANDOM = 0,              //
    CENTERS_GONZALES = 1,            //
    CENTERS_KMEANSPP = 2             //
);

 flann_log_level_t =(//
    FLANN_LOG_NONE = 0,        //
    FLANN_LOG_FATAL = 1,       //
    FLANN_LOG_ERROR = 2,       //
    FLANN_LOG_WARN = 3,        //
    FLANN_LOG_INFO = 4         //
);

flann_distance_t =(//
    FLANN_DIST_EUCLIDEAN = 1,            //
    FLANN_DIST_L2 = 1,                   //
    FLANN_DIST_MANHATTAN = 2,            //
    FLANN_DIST_L1 = 2,                   //
    FLANN_DIST_MINKOWSKI = 3,            //
    FLANN_DIST_MAX   = 4,                //
    FLANN_DIST_HIST_INTERSECT   = 5,     //
    FLANN_DIST_HELLINGER = 6,            //
    FLANN_DIST_CHI_SQUARE = 7,           //
    FLANN_DIST_CS         = 7,           //
    FLANN_DIST_KULLBACK_LEIBLER  = 8,    //
    FLANN_DIST_KL                = 8,    //
    FLANN_DIST_HAMMING          = 9,     //
    FLANN_DIST_DNAMMING          = 10,   //

    // deprecated constants, should use the FLANN_DIST_* ones instead
    EUCLIDEAN = 1,                       //
    MANHATTAN = 2,                       //
    MINKOWSKI = 3,                       //
    MAX_DIST   = 4,                      //
    HIST_INTERSECT   = 5,                //
    HELLINGER = 6,                       //
    CS         = 7,                      //
    KL         = 8,                      //
    KULLBACK_LEIBLER  = 8                //
);

 flann_datatype_t =(//
    FLANN_INT8 = 0,        //
    FLANN_INT16 = 1,       //
    FLANN_INT32 = 2,       //
    FLANN_INT64 = 3,       //
    FLANN_UINT8 = 4,       //
    FLANN_UINT16 = 5,      //
    FLANN_UINT32 = 6,      //
    FLANN_UINT64 = 7,      //
    FLANN_FLOAT32 = 8,     //
    FLANN_FLOAT64 = 9      //
);

const
    FLANN_CHECKS_UNLIMITED = -1;
    FLANN_CHECKS_AUTOTUNED = -2;

//! @endcond

{$endif OPENCV_FLANN_DEFINES_H_}
