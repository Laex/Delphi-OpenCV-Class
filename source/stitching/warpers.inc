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

{$ifndef OPENCV_STITCHING_WARPER_CREATORS_HPP}
{$define OPENCV_STITCHING_WARPER_CREATORS_HPP}

{$i 'stitching/detail/warpers.inc'}

//namespace cv {

//    class CV_EXPORTS_W PyRotationWarper
//    {
//        Ptr<detail::RotationWarper> rw;
//
//    public:
//        CV_WRAP PyRotationWarper(String type, float scale);
//        CV_WRAP PyRotationWarper() {};
//        ~PyRotationWarper() {}
//
//        (** @brief Projects the image point.
//
//        @param pt Source point
//        @param K Camera intrinsic parameters
//        @param R Camera rotation matrix
//        @return Projected point
//        *)
//        CV_WRAP Point2f warpPoint(const Point2f &pt, InputArray K, InputArray R);
//
//        (** @brief Projects the image point backward.
//
//        @param pt Projected point
//        @param K Camera intrinsic parameters
//        @param R Camera rotation matrix
//        @return Backward-projected point
//        *)
//#if CV_VERSION_MAJOR == 4
//        CV_WRAP Point2f warpPointBackward(const Point2f& pt, InputArray K, InputArray R)
//        {
//            CV_UNUSED(pt); CV_UNUSED(K); CV_UNUSED(R);
//            CV_Error(Error::StsNotImplemented, "");
//        }
//#else
//        CV_WRAP Point2f warpPointBackward(const Point2f &pt, InputArray K, InputArray R);
//#endif
//        (** @brief Builds the projection maps according to the given camera data.
//
//        @param src_size Source image size
//        @param K Camera intrinsic parameters
//        @param R Camera rotation matrix
//        @param xmap Projection map for the x axis
//        @param ymap Projection map for the y axis
//        @return Projected image minimum bounding box
//        *)
//        CV_WRAP Rect buildMaps(Size src_size, InputArray K, InputArray R, OutputArray xmap, OutputArray ymap);
//
//        (** @brief Projects the image.
//
//        @param src Source image
//        @param K Camera intrinsic parameters
//        @param R Camera rotation matrix
//        @param interp_mode Interpolation mode
//        @param border_mode Border extrapolation mode
//        @param dst Projected image
//        @return Project image top-left corner
//        *)
//        CV_WRAP Point warp(InputArray src, InputArray K, InputArray R, int interp_mode, int border_mode,
//            CV_OUT OutputArray dst);
//
//        (** @brief Projects the image backward.
//
//        @param src Projected image
//        @param K Camera intrinsic parameters
//        @param R Camera rotation matrix
//        @param interp_mode Interpolation mode
//        @param border_mode Border extrapolation mode
//        @param dst_size Backward-projected image size
//        @param dst Backward-projected image
//        *)
//        CV_WRAP void warpBackward(InputArray src, InputArray K, InputArray R, int interp_mode, int border_mode,
//            Size dst_size, CV_OUT OutputArray dst);
//
//        (**
//        @param src_size Source image bounding box
//        @param K Camera intrinsic parameters
//        @param R Camera rotation matrix
//        @return Projected image minimum bounding box
//        *)
//        CV_WRAP Rect warpRoi(Size src_size, InputArray K, InputArray R);
//
//        CV_WRAP float getScale() const { return 1.f; }
//        CV_WRAP void setScale(float) {}
//    };

//! @addtogroup stitching_warp
//! @{

(** @brief Image warper factories base class.
 *)

//class CV_EXPORTS_W WarperCreator
//{
//public:
//    CV_WRAP virtual ~WarperCreator() {}
//    virtual Ptr<detail::RotationWarper> create(float scale) const = 0;
//};


(** @brief Plane warper factory class.
  @sa detail::PlaneWarper
 *)
//class CV_EXPORTS  PlaneWarper : public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::PlaneWarper>(scale); }
//};

(** @brief Affine warper factory class.
  @sa detail::AffineWarper
 *)
//class CV_EXPORTS  AffineWarper : public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::AffineWarper>(scale); }
//};

(** @brief Cylindrical warper factory class.
@sa detail::CylindricalWarper
*)
//class CV_EXPORTS CylindricalWarper: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::CylindricalWarper>(scale); }
//};

(** @brief Spherical warper factory class *)
//class CV_EXPORTS SphericalWarper: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::SphericalWarper>(scale); }
//};

//class CV_EXPORTS FisheyeWarper : public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::FisheyeWarper>(scale); }
//};

//class CV_EXPORTS StereographicWarper: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::StereographicWarper>(scale); }
//};

//class CV_EXPORTS CompressedRectilinearWarper: public WarperCreator
//{
//    float a, b;
//public:
//    CompressedRectilinearWarper(float A = 1, float B = 1)
//    {
//        a = A; b = B;
//    }
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::CompressedRectilinearWarper>(scale, a, b); }
//};

//class CV_EXPORTS CompressedRectilinearPortraitWarper: public WarperCreator
//{
//    float a, b;
//public:
//    CompressedRectilinearPortraitWarper(float A = 1, float B = 1)
//    {
//        a = A; b = B;
//    }
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::CompressedRectilinearPortraitWarper>(scale, a, b); }
//};

//class CV_EXPORTS PaniniWarper: public WarperCreator
//{
//    float a, b;
//public:
//    PaniniWarper(float A = 1, float B = 1)
//    {
//        a = A; b = B;
//    }
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::PaniniWarper>(scale, a, b); }
//};

//class CV_EXPORTS PaniniPortraitWarper: public WarperCreator
//{
//    float a, b;
//public:
//    PaniniPortraitWarper(float A = 1, float B = 1)
//    {
//        a = A; b = B;
//    }
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::PaniniPortraitWarper>(scale, a, b); }
//};

//class CV_EXPORTS MercatorWarper: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::MercatorWarper>(scale); }
//};

//class CV_EXPORTS TransverseMercatorWarper: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::TransverseMercatorWarper>(scale); }
//};



{$ifdef HAVE_OPENCV_CUDAWARPING}
//class PlaneWarperGpu: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::PlaneWarperGpu>(scale); }
//};


//class CylindricalWarperGpu: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::CylindricalWarperGpu>(scale); }
//};


//class SphericalWarperGpu: public WarperCreator
//{
//public:
//    Ptr<detail::RotationWarper> create(float scale) const CV_OVERRIDE { return makePtr<detail::SphericalWarperGpu>(scale); }
//};
{$endif}

//! @} stitching_warp

//} // namespace cv

{$endif OPENCV_STITCHING_WARPER_CREATORS_HPP}
