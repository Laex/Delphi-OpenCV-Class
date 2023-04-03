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

{$ifndef OPENCV_STITCHING_BLENDERS_HPP}
{$define OPENCV_STITCHING_BLENDERS_HPP}

//#if defined(NO)
//#  warning Detected Apple 'NO' macro definition, it can cause build conflicts. Please, include this header before any Apple headers.
//#endif

{$IF not defined(OPENCV_CORE_HPP)}{$I 'core.inc'}{$ifend}
{$if not defined(OPENCV_CORE_CUDA_HPP)}{$i 'core/cuda.inc'}{$IFEND}

//namespace cv {
//namespace detail {

//! @addtogroup stitching_blend
//! @{

(** @brief Base class for all blenders.

Simple blender which puts one image over another
*)
//class CV_EXPORTS_W Blender
//{
//public:
//    virtual ~Blender() {}
//
//    enum { NO, FEATHER, MULTI_BAND };
//    CV_WRAP static Ptr<Blender> createDefault(int type, bool try_gpu = false);
//
//    (** @brief Prepares the blender for blending.
//
//    @param corners Source images top-left corners
//    @param sizes Source image sizes
//     *)
//    CV_WRAP virtual void prepare(const std::vector<Point> &corners, const std::vector<Size> &sizes);
//    (** @overload *)
//    CV_WRAP virtual void prepare(Rect dst_roi);
//    (** @brief Processes the image.
//
//    @param img Source image
//    @param mask Source image mask
//    @param tl Source image top-left corners
//     *)
//    CV_WRAP virtual void feed(InputArray img, InputArray mask, Point tl);
//    (** @brief Blends and returns the final pano.
//
//    @param dst Final pano
//    @param dst_mask Final pano mask
//     *)
//    CV_WRAP virtual void blend(CV_IN_OUT InputOutputArray dst,CV_IN_OUT  InputOutputArray dst_mask);
//
//protected:
//    UMat dst_, dst_mask_;
//    Rect dst_roi_;
//};

(** @brief Simple blender which mixes images at its borders.
 *)
//class CV_EXPORTS_W FeatherBlender : public Blender
//{
//public:
//    CV_WRAP FeatherBlender(float sharpness = 0.02f);
//
//    CV_WRAP float sharpness() const { return sharpness_; }
//    CV_WRAP void setSharpness(float val) { sharpness_ = val; }
//
//    CV_WRAP void prepare(Rect dst_roi) CV_OVERRIDE;
//    CV_WRAP void feed(InputArray img, InputArray mask, Point tl) CV_OVERRIDE;
//    CV_WRAP void blend(InputOutputArray dst, InputOutputArray dst_mask) CV_OVERRIDE;
//
//    //! Creates weight maps for fixed set of source images by their masks and top-left corners.
//    //! Final image can be obtained by simple weighting of the source images.
//    CV_WRAP Rect createWeightMaps(const std::vector<UMat> &masks, const std::vector<Point> &corners,
//        CV_IN_OUT std::vector<UMat> &weight_maps);
//
//private:
//    float sharpness_;
//    UMat weight_map_;
//    UMat dst_weight_map_;
//};

//inline FeatherBlender::FeatherBlender(float _sharpness) { setSharpness(_sharpness); }

(** @brief Blender which uses multi-band blending algorithm (see @cite BA83).
 *)
//class CV_EXPORTS_W MultiBandBlender : public Blender
//{
//public:
//    CV_WRAP MultiBandBlender(int try_gpu = false, int num_bands = 5, int weight_type = CV_32F);
//
//    CV_WRAP int numBands() const { return actual_num_bands_; }
//    CV_WRAP void setNumBands(int val) { actual_num_bands_ = val; }
//
//    CV_WRAP void prepare(Rect dst_roi) CV_OVERRIDE;
//    CV_WRAP void feed(InputArray img, InputArray mask, Point tl) CV_OVERRIDE;
//    CV_WRAP void blend(CV_IN_OUT InputOutputArray dst, CV_IN_OUT InputOutputArray dst_mask) CV_OVERRIDE;
//
//private:
//    int actual_num_bands_, num_bands_;
//    std::vector<UMat> dst_pyr_laplace_;
//    std::vector<UMat> dst_band_weights_;
//    Rect dst_roi_final_;
//    bool can_use_gpu_;
//    int weight_type_; //CV_32F or CV_16S
//#if defined(HAVE_OPENCV_CUDAARITHM) && defined(HAVE_OPENCV_CUDAWARPING)
//    std::vector<cuda::GpuMat> gpu_dst_pyr_laplace_;
//    std::vector<cuda::GpuMat> gpu_dst_band_weights_;
//    std::vector<Point> gpu_tl_points_;
//    std::vector<cuda::GpuMat> gpu_imgs_with_border_;
//    std::vector<std::vector<cuda::GpuMat> > gpu_weight_pyr_gauss_vec_;
//    std::vector<std::vector<cuda::GpuMat> > gpu_src_pyr_laplace_vec_;
//    std::vector<std::vector<cuda::GpuMat> > gpu_ups_;
//    cuda::GpuMat gpu_dst_mask_;
//    cuda::GpuMat gpu_mask_;
//    cuda::GpuMat gpu_img_;
//    cuda::GpuMat gpu_weight_map_;
//    cuda::GpuMat gpu_add_mask_;
//    int gpu_feed_idx_;
//    bool gpu_initialized_;
//#endif
//};


//////////////////////////////////////////////////////////////////////////////
// Auxiliary functions

//void CV_EXPORTS_W normalizeUsingWeightMap(InputArray weight, CV_IN_OUT InputOutputArray src);

//void CV_EXPORTS_W createWeightMap(InputArray mask, float sharpness, CV_IN_OUT InputOutputArray weight);

//void CV_EXPORTS_W createLaplacePyr(InputArray img, int num_levels, CV_IN_OUT std::vector<UMat>& pyr);
//void CV_EXPORTS_W createLaplacePyrGpu(InputArray img, int num_levels, CV_IN_OUT std::vector<UMat>& pyr);

// Restores source image
//void CV_EXPORTS_W restoreImageFromLaplacePyr(CV_IN_OUT std::vector<UMat>& pyr);
//void CV_EXPORTS_W restoreImageFromLaplacePyrGpu(CV_IN_OUT std::vector<UMat>& pyr);

//! @}

//} // namespace detail
//} // namespace cv

{$endif OPENCV_STITCHING_BLENDERS_HPP}
