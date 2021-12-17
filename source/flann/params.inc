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

{$ifndef OPENCV_FLANN_PARAMS_H_}
{$define OPENCV_FLANN_PARAMS_H_}

//! @cond IGNORED

{$I 'flann/any.inc'}
{$I 'flann/general.inc'}
//#include <iostream>
//#include <map>


//namespace cvflann
//{

//typedef std::map<cv::String, any> IndexParams;
//
//struct SearchParams : public IndexParams
//{
//    SearchParams(int checks = 32, float eps = 0, bool sorted = true )
//    {
//        init(checks, eps, sorted, false);
//    }
//
//    SearchParams(int checks, float eps, bool sorted, bool explore_all_trees )
//    {
//        init(checks, eps, sorted, explore_all_trees);
//    }
//
//    void init(int checks = 32, float eps = 0, bool sorted = true, bool explore_all_trees = false )
//    {
//        // how many leafs to visit when searching for neighbours (-1 for unlimited)
//        (*this)["checks"] = checks;
//        // search for eps-approximate neighbours (default: 0)
//        (*this)["eps"] = eps;
//        // only for radius search, require neighbours sorted by distance (default: true)
//        (*this)["sorted"] = sorted;
//        // if false, search stops at the tree reaching the number of  max checks (original behavior).
//        // When true, we do a descent in each tree and. Like before the alternative paths
//        // stored in the heap are not be processed further when max checks is reached.
//        (*this)["explore_all_trees"] = explore_all_trees;
//    }
//};
//
//
//template<typename T>
//T get_param(const IndexParams& params, cv::String name, const T& default_value)
//{
//    IndexParams::const_iterator it = params.find(name);
//    if (it != params.end()) {
//        return it->second.cast<T>();
//    }
//    else {
//        return default_value;
//    }
//}
//
//template<typename T>
//T get_param(const IndexParams& params, cv::String name)
//{
//    IndexParams::const_iterator it = params.find(name);
//    if (it != params.end()) {
//        return it->second.cast<T>();
//    }
//    else {
//        FLANN_THROW(cv::Error::StsBadArg, cv::String("Missing parameter '")+name+cv::String("' in the parameters given"));
//    }
//}
//
//inline void print_params(const IndexParams& params, std::ostream& stream)
//{
//    IndexParams::const_iterator it;
//
//    for(it=params.begin(); it!=params.end(); ++it) {
//        stream << it->first << " : " << it->second << std::endl;
//    }
//}
//
//inline void print_params(const IndexParams& params)
//{
//    print_params(params, std::cout);
//}
//
//}

//! @endcond

{$endif OPENCV_FLANN_PARAMS_H_}
