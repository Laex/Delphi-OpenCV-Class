(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the 'License');
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an 'AS IS' BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

{$if not defined(OPENCV_CORE_CHECK_HPP)}
{$define OPENCV_CORE_CHECK_HPP}

{$IF not defined(OPENCV_CORE_BASE_HPP)}{$I 'core/base.inc'}{$ifend}

//namespace cv {

(** Returns string of cv::Mat depth value: CV_8U -> "CV_8U" or "<invalid depth>" *)
//CV_EXPORTS const char* depthToString(int depth);
{
4384
?depthToString@cv@@YAPEBDH@Z
?depthToString@cv@@YAPEBDH@Z
char const * cv::depthToString(int)
}
{
4385
?depthToString_@detail@cv@@YAPEBDH@Z
?depthToString_@detail@cv@@YAPEBDH@Z
char const * cv::detail::depthToString_(int)
}
function depthToString(depth:int):pCVChar; {$IFDEF USE_INLINE}inline; {$ENDIF}
//external opencv_world_dll
//name '?depthToString_@detail@cv@@YAPEBDH@Z'
//{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(** Returns string of cv::Mat depth value: CV_8UC3 -> "CV_8UC3" or "<invalid type>" *)
//CV_EXPORTS const String typeToString(int type);


//! @cond IGNORED
//namespace detail {

(** Returns string of cv::Mat depth value: CV_8U -> "CV_8U" or NULL *)
//CV_EXPORTS const char* depthToString_(int depth);

(** Returns string of cv::Mat depth value: CV_8UC3 -> "CV_8UC3" or cv::String() *)
//CV_EXPORTS const cv::String typeToString_(int type);

Type

TestOp =(
  TEST_CUSTOM = 0,
  TEST_EQ = 1,
  TEST_NE = 2,
  TEST_LE = 3,
  TEST_LT = 4,
  TEST_GE = 5,
  TEST_GT = 6,
  CV__LAST_TEST_OP = 7
);

//struct CheckContext {
//    const char* func;
//    const char* file;
//    int line;
//    enum TestOp testOp;
//    const char* message;
//    const char* p1_str;
//    const char* p2_str;
//};

(*
#ifndef CV__CHECK_FILENAME
# define CV__CHECK_FILENAME __FILE__
#endif

#ifndef CV__CHECK_FUNCTION
# if defined _MSC_VER
#   define CV__CHECK_FUNCTION __FUNCSIG__
# elif defined __GNUC__
#   define CV__CHECK_FUNCTION __PRETTY_FUNCTION__
# else
#   define CV__CHECK_FUNCTION "<unknown>"
# endif
#endif

#define CV__CHECK_LOCATION_VARNAME(id) CVAUX_CONCAT(CVAUX_CONCAT(__cv_check_, id), __LINE__)
#define CV__DEFINE_CHECK_CONTEXT(id, message, testOp, p1_str, p2_str) \
    static const cv::detail::CheckContext CV__CHECK_LOCATION_VARNAME(id) = \
            { CV__CHECK_FUNCTION, CV__CHECK_FILENAME, __LINE__, testOp, "" message, "" p1_str, "" p2_str }

CV_EXPORTS void CV_NORETURN check_failed_auto(const int v1, const int v2, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const size_t v1, const size_t v2, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const float v1, const float v2, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const double v1, const double v2, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const Size_<int> v1, const Size_<int> v2, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_MatDepth(const int v1, const int v2, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_MatType(const int v1, const int v2, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_MatChannels(const int v1, const int v2, const CheckContext& ctx);

CV_EXPORTS void CV_NORETURN check_failed_auto(const int v, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const size_t v, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const float v, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const double v, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const Size_<int> v, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_auto(const std::string& v1, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_MatDepth(const int v, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_MatType(const int v, const CheckContext& ctx);
CV_EXPORTS void CV_NORETURN check_failed_MatChannels(const int v, const CheckContext& ctx);


#define CV__TEST_EQ(v1, v2) ((v1) == (v2))
#define CV__TEST_NE(v1, v2) ((v1) != (v2))
#define CV__TEST_LE(v1, v2) ((v1) <= (v2))
#define CV__TEST_LT(v1, v2) ((v1) < (v2))
#define CV__TEST_GE(v1, v2) ((v1) >= (v2))
#define CV__TEST_GT(v1, v2) ((v1) > (v2))

#define CV__CHECK(id, op, type, v1, v2, v1_str, v2_str, msg_str) do { \
    if(CV__TEST_##op((v1), (v2))) ; else { \
        CV__DEFINE_CHECK_CONTEXT(id, msg_str, cv::detail::TEST_ ## op, v1_str, v2_str); \
        cv::detail::check_failed_ ## type((v1), (v2), CV__CHECK_LOCATION_VARNAME(id)); \
    } \
} while (0)

#define CV__CHECK_CUSTOM_TEST(id, type, v, test_expr, v_str, test_expr_str, msg_str) do { \
    if(!!(test_expr)) ; else { \
        CV__DEFINE_CHECK_CONTEXT(id, msg_str, cv::detail::TEST_CUSTOM, v_str, test_expr_str); \
        cv::detail::check_failed_ ## type((v), CV__CHECK_LOCATION_VARNAME(id)); \
    } \
} while (0)

} // namespace
//! @endcond


/// Supported values of these types: int, float, double
#define CV_CheckEQ(v1, v2, msg)  CV__CHECK(_, EQ, auto, v1, v2, #v1, #v2, msg)
#define CV_CheckNE(v1, v2, msg)  CV__CHECK(_, NE, auto, v1, v2, #v1, #v2, msg)
#define CV_CheckLE(v1, v2, msg)  CV__CHECK(_, LE, auto, v1, v2, #v1, #v2, msg)
#define CV_CheckLT(v1, v2, msg)  CV__CHECK(_, LT, auto, v1, v2, #v1, #v2, msg)
#define CV_CheckGE(v1, v2, msg)  CV__CHECK(_, GE, auto, v1, v2, #v1, #v2, msg)
#define CV_CheckGT(v1, v2, msg)  CV__CHECK(_, GT, auto, v1, v2, #v1, #v2, msg)

/// Check with additional "decoding" of type values in error message
#define CV_CheckTypeEQ(t1, t2, msg)  CV__CHECK(_, EQ, MatType, t1, t2, #t1, #t2, msg)
/// Check with additional "decoding" of depth values in error message
#define CV_CheckDepthEQ(d1, d2, msg)  CV__CHECK(_, EQ, MatDepth, d1, d2, #d1, #d2, msg)

#define CV_CheckChannelsEQ(c1, c2, msg)  CV__CHECK(_, EQ, MatChannels, c1, c2, #c1, #c2, msg)

/// Example: type == CV_8UC1 || type == CV_8UC3
#define CV_CheckType(t, test_expr, msg)  CV__CHECK_CUSTOM_TEST(_, MatType, t, (test_expr), #t, #test_expr, msg)

/// Example: depth == CV_32F || depth == CV_64F
#define CV_CheckDepth(t, test_expr, msg)  CV__CHECK_CUSTOM_TEST(_, MatDepth, t, (test_expr), #t, #test_expr, msg)

/// Example: v == A || v == B
#define CV_Check(v, test_expr, msg)  CV__CHECK_CUSTOM_TEST(_, auto, v, (test_expr), #v, #test_expr, msg)

/// Some complex conditions: CV_Check(src2, src2.empty() || (src2.type() == src1.type() && src2.size() == src1.size()), "src2 should have same size/type as src1")
// TODO define pretty-printers

#ifndef NDEBUG
#define CV_DbgCheck(v, test_expr, msg)  CV__CHECK_CUSTOM_TEST(_, auto, v, (test_expr), #v, #test_expr, msg)
#define CV_DbgCheckEQ(v1, v2, msg)  CV__CHECK(_, EQ, auto, v1, v2, #v1, #v2, msg)
#define CV_DbgCheckNE(v1, v2, msg)  CV__CHECK(_, NE, auto, v1, v2, #v1, #v2, msg)
#define CV_DbgCheckLE(v1, v2, msg)  CV__CHECK(_, LE, auto, v1, v2, #v1, #v2, msg)
#define CV_DbgCheckLT(v1, v2, msg)  CV__CHECK(_, LT, auto, v1, v2, #v1, #v2, msg)
#define CV_DbgCheckGE(v1, v2, msg)  CV__CHECK(_, GE, auto, v1, v2, #v1, #v2, msg)
#define CV_DbgCheckGT(v1, v2, msg)  CV__CHECK(_, GT, auto, v1, v2, #v1, #v2, msg)
#else
#define CV_DbgCheck(v, test_expr, msg)  do { } while (0)
#define CV_DbgCheckEQ(v1, v2, msg)  do { } while (0)
#define CV_DbgCheckNE(v1, v2, msg)  do { } while (0)
#define CV_DbgCheckLE(v1, v2, msg)  do { } while (0)
#define CV_DbgCheckLT(v1, v2, msg)  do { } while (0)
#define CV_DbgCheckGE(v1, v2, msg)  do { } while (0)
#define CV_DbgCheckGT(v1, v2, msg)  do { } while (0)
#endif

} // namespace
*)

{$ELSE}
{$MESSAGE FATAL 'Duplicate included file - core/check.inc'}

{$ifend OPENCV_CORE_CHECK_HPP}
