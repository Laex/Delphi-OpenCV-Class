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

{$if not defined(OPENCV_STITCHING_UTIL_INL_HPP)}
{$define OPENCV_STITCHING_UTIL_INL_HPP}

{$IF not defined(OPENCV_CORE_HPP)}{$I 'core.inc'}{$ifend}
{$if not defined(OPENCV_STITCHING_UTIL_HPP)}{$i 'stitching/detail/util.inc'}{$ifend} // Make your IDE see declarations

//! @cond IGNORED

//namespace cv {
//namespace detail {

//template <typename B>
//B Graph::forEach(B body) const
//{
//    for (int i = 0; i < numVertices(); ++i)
//    {
//        std::list<GraphEdge>::const_iterator edge = edges_[i].begin();
//        for (; edge != edges_[i].end(); ++edge)
//            body(*edge);
//    }
//    return body;
//}



//template <typename B>
//B Graph::walkBreadthFirst(int from, B body) const
//{
//    std::vector<bool> was(numVertices(), false);
//    std::queue<int> vertices;
//
//    was[from] = true;
//    vertices.push(from);
//
//    while (!vertices.empty())
//    {
//        int vertex = vertices.front();
//        vertices.pop();
//
//        std::list<GraphEdge>::const_iterator edge = edges_[vertex].begin();
//        for (; edge != edges_[vertex].end(); ++edge)
//        {
//            if (!was[edge->to])
//            {
//                body(*edge);
//                was[edge->to] = true;
//                vertices.push(edge->to);
//            }
//        }
//    }
//
//    return body;
//}


//////////////////////////////////////////////////////////////////////////////
// Some auxiliary math functions

//static inline
//float normL2(const Point3f& a)
//{
//    return a.x * a.x + a.y * a.y + a.z * a.z;
//}


//static inline
//float normL2(const Point3f& a, const Point3f& b)
//{
//    return normL2(a - b);
//}


//static inline
//double normL2sq(const Mat &r)
//{
//    return r.dot(r);
//}


//static inline int sqr(int x) { return x * x; }
//static inline float sqr(float x) { return x * x; }
//static inline double sqr(double x) { return x * x; }

//} // namespace detail
//} // namespace cv

//! @endcond

{$ifend} //OPENCV_STITCHING_UTIL_INL_HPP
