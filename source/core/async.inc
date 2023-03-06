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

{$if not defined(OPENCV_CORE_ASYNC_HPP)}
{$define OPENCV_CORE_ASYNC_HPP}

{$IF not defined(OPENCV_CORE_MAT_HPP)}{$include core/mat.inc}{$IFEND}

{$if defined(CV_CXX11)}
//{$include <future>
{$include chrono}
{$ifend}

(** @addtogroup core_async

@{
*)

Type
(** @brief Returns result of asynchronous operations

Object has attached asynchronous state.
Assignment operator doesn't clone asynchronous state (it is shared between all instances).

Result can be fetched via get() method only once.

*)
 AsyncArray = record
public
//    ~AsyncArray() CV_NOEXCEPT;
//    CV_WRAP AsyncArray() CV_NOEXCEPT;
//    AsyncArray(const AsyncArray& o) CV_NOEXCEPT;
//    AsyncArray& operator=(const AsyncArray& o) CV_NOEXCEPT;
//    CV_WRAP void release() CV_NOEXCEPT;

    (** Fetch the result.
    @param[out] dst destination array

    Waits for result until container has valid result.
    Throws exception if exception was stored as a result.

    Throws exception on invalid container state.

    @note Result or stored exception can be fetched only once.
    *)
//    CV_WRAP void get(OutputArray dst) const;

    (** Retrieving the result with timeout
    @param[out] dst destination array
    @param[in] timeoutNs timeout in nanoseconds, -1 for infinite wait

    @returns true if result is ready, false if the timeout has expired

    @note Result or stored exception can be fetched only once.
    *)
//    bool get(OutputArray dst, int64 timeoutNs) const;

//    CV_WRAP inline
//    bool get(OutputArray dst, double timeoutNs) const { return get(dst, (int64)timeoutNs); }

//    bool wait_for(int64 timeoutNs) const;

//    CV_WRAP inline
//    bool wait_for(double timeoutNs) const { return wait_for((int64)timeoutNs); }

//    CV_WRAP bool valid() const CV_NOEXCEPT;

{$ifdef CV_CXX11}
    inline AsyncArray(AsyncArray&& o) { p = o.p; o.p = NULL; }
    inline AsyncArray& operator=(AsyncArray&& o) CV_NOEXCEPT { std::swap(p, o.p); return *this; }

    template<typename _Rep, typename _Period>
    inline bool get(OutputArray dst, const std::chrono::duration<_Rep, _Period>& timeout)
    {
        return get(dst, (int64)(std::chrono::nanoseconds(timeout).count()));
    }

    template<typename _Rep, typename _Period>
    inline bool wait_for(const std::chrono::duration<_Rep, _Period>& timeout)
    {
        return wait_for((int64)(std::chrono::nanoseconds(timeout).count()));
    }

{$ifdef 0}
    std::future<Mat> getFutureMat() const;
    std::future<UMat> getFutureUMat() const;
{$endif}
{$endif}


    // PImpl
//    struct Impl; friend struct Impl;
//    inline void* _getImpl() const CV_NOEXCEPT { return p; }
private
//    Impl* p;
end;


//! @}

{$ifend OPENCV_CORE_ASYNC_HPP}
