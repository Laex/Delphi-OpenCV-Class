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

{$ifndef OPENCV_IMGPROC_SEGMENTATION_HPP}
{$define OPENCV_IMGPROC_SEGMENTATION_HPP}

{$if not defined(OPENCV_IMGPROC_HPP)}{$I 'imgproc.inc'}{$ifend}

//namespace cv {

//namespace segmentation {

//! @addtogroup imgproc_segmentation
//! @{

Type
(** @brief Intelligent Scissors image segmentation
 *
 * This class is used to find the path (contour) between two points
 * which can be used for image segmentation.
 *
 * Usage example:
 * @snippet snippets/imgproc_segmentation.cpp usage_example_intelligent_scissors
 *
 * Reference: <a href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.138.3811&rep=rep1&type=pdf">"Intelligent Scissors for Image Composition"</a>
 * algorithm designed by Eric N. Mortensen and William A. Barrett, Brigham Young University
 * @cite Mortensen95intelligentscissors
 *)

  pIntelligentScissorsMB = ^TIntelligentScissorsMB;

  TIntelligentScissorsMB = record
  public
    // CV_WRAP IntelligentScissorsMB();
    class operator Initialize(out Dest: TIntelligentScissorsMB);
    class operator Finalize(var Dest: TIntelligentScissorsMB);
    (* * @brief Specify weights of feature functions
      *
      * Consider keeping weights normalized (sum of weights equals to 1.0)
      * Discrete dynamic programming (DP) goal is minimization of costs between pixels.
      *
      * @param weight_non_edge Specify cost of non-edge pixels (default: 0.43f)
      * @param weight_gradient_direction Specify cost of gradient direction function (default: 0.43f)
      * @param weight_gradient_magnitude Specify cost of gradient magnitude function (default: 0.14f)
    *)
    // CV_WRAP
    // IntelligentScissorsMB& setWeights(float weight_non_edge, float weight_gradient_direction, float weight_gradient_magnitude);

    (* * @brief Specify gradient magnitude max value threshold
      *
      * Zero limit value is used to disable gradient magnitude thresholding (default behavior, as described in original article).
      * Otherwize pixels with `gradient magnitude >= threshold` have zero cost.
      *
      * @note Thresholding should be used for images with irregular regions (to avoid stuck on parameters from high-contract areas, like embedded logos).
      *
      * @param gradient_magnitude_threshold_max Specify gradient magnitude max value threshold (default: 0, disabled)
    *)
    // CV_WRAP
    // IntelligentScissorsMB& setGradientMagnitudeMaxLimit(float gradient_magnitude_threshold_max = 0.0f);
    function setGradientMagnitudeMaxLimit(gradient_magnitude_threshold_max: float = 0.0): pIntelligentScissorsMB; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Switch to "Laplacian Zero-Crossing" edge feature extractor and specify its parameters
      *
      * This feature extractor is used by default according to article.
      *
      * Implementation has additional filtering for regions with low-amplitude noise.
      * This filtering is enabled through parameter of minimal gradient amplitude (use some small value 4, 8, 16).
      *
      * @note Current implementation of this feature extractor is based on processing of grayscale images (color image is converted to grayscale image first).
      *
      * @note Canny edge detector is a bit slower, but provides better results (especially on color images): use setEdgeFeatureCannyParameters().
      *
      * @param gradient_magnitude_min_value Minimal gradient magnitude value for edge pixels (default: 0, check is disabled)
    *)
    // CV_WRAP
    // IntelligentScissorsMB& setEdgeFeatureZeroCrossingParameters(float gradient_magnitude_min_value = 0.0f);

    (* * @brief Switch edge feature extractor to use Canny edge detector
      *
      * @note "Laplacian Zero-Crossing" feature extractor is used by default (following to original article)
      *
      * @sa Canny
    *)
    // CV_WRAP
    // IntelligentScissorsMB& setEdgeFeatureCannyParameters(
    // double threshold1, double threshold2,
    // int apertureSize = 3, bool L2gradient = false);
    function setEdgeFeatureCannyParameters(threshold1, threshold2: double; apertureSize: Int = 3; L2gradient: BOOL = false): pIntelligentScissorsMB; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Specify input image and extract image features
      *
      * @param image input image. Type is #CV_8UC1 / #CV_8UC3
    *)
    // CV_WRAP
    // IntelligentScissorsMB& applyImage(InputArray image);
    function applyImage(const image: TInputArray): pIntelligentScissorsMB; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Specify custom features of imput image
      *
      * Customized advanced variant of applyImage() call.
      *
      * @param non_edge Specify cost of non-edge pixels. Type is CV_8UC1. Expected values are `{0, 1}`.
      * @param gradient_direction Specify gradient direction feature. Type is CV_32FC2. Values are expected to be normalized: `x^2 + y^2 == 1`
      * @param gradient_magnitude Specify cost of gradient magnitude function: Type is CV_32FC1. Values should be in range `[0, 1]`.
      * @param image **Optional parameter**. Must be specified if subset of features is specified (non-specified features are calculated internally)
    *)
    // CV_WRAP
    // IntelligentScissorsMB& applyImageFeatures(
    // InputArray non_edge, InputArray gradient_direction, InputArray gradient_magnitude,
    // InputArray image = noArray());

    (* * @brief Prepares a map of optimal paths for the given source point on the image
      *
      * @note applyImage() / applyImageFeatures() must be called before this call
      *
      * @param sourcePt The source point used to find the paths
    *)
    // CV_WRAP void buildMap(const Point& sourcePt);
    procedure buildMap(const sourcePt: TPoint); {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Extracts optimal contour for the given target point on the image
      *
      * @note buildMap() must be called before this call
      *
      * @param targetPt The target point
      * @param[out] contour The list of pixels which contains optimal path between the source and the target points of the image. Type is CV_32SC2 (compatible with `std::vector<Point>`)
      * @param backward Flag to indicate reverse order of retrived pixels (use "true" value to fetch points from the target to the source point)
    *)
    // CV_WRAP void getContour(const Point& targetPt, OutputArray contour, bool backward = false) const;
    procedure getContour(const targetPt: TPoint;const contour: TOutputArray; backward: BOOL = false); {$IFDEF USE_INLINE}inline; {$ENDIF}
  private
{$HINTS OFF}
    Dummy: array [0 .. 15] of Byte;
{$HINTS OFF}
{$IFNDEF CV_DOXYGEN}
  public
    // struct Impl;
    // inline Impl* getImpl() const { return impl.get(); }
  private
    // std::shared_ptr<Impl> impl;
{$ENDIF}
  end;

//! @}

//}  // namespace segmentation
//}  // namespace cv

{$endif OPENCV_IMGPROC_SEGMENTATION_HPP}
