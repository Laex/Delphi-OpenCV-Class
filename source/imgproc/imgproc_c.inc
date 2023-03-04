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

{$ifndef OPENCV_IMGPROC_IMGPROC_C_H}
{$define OPENCV_IMGPROC_IMGPROC_C_H}

{$include 'imgproc/types_c.inc'}

(** @addtogroup imgproc_c
@{
*)

(*********************** Background statistics accumulation *****************************)

(** @brief Adds image to accumulator
@see cv::accumulate
*)
//CVAPI(void)  cvAcc( const CvArr* image, CvArr* sum,
//                   const CvArr* mask CV_DEFAULT(NULL) );
procedure cvAcc(const image:pCvArr; sum:pCvArr; const mask:pCvArr= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7627
{$ELSE}
name 'cvAcc'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Adds squared image to accumulator
@see cv::accumulateSquare
*)
//CVAPI(void)  cvSquareAcc( const CvArr* image, CvArr* sqsum,
//                         const CvArr* mask CV_DEFAULT(NULL) );
procedure cvSquareAcc(const image:pCvArr; sqsum:pCvArr; const mask:pCvArr= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7986
{$ELSE}
name 'cvSquareAcc'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Adds a product of two images to accumulator
@see cv::accumulateProduct
*)
//CVAPI(void)  cvMultiplyAcc( const CvArr* image1, const CvArr* image2, CvArr* acc,
//                           const CvArr* mask CV_DEFAULT(NULL) );
procedure cvMultiplyAcc(const image1:pCvArr; const image2:pCvArr; acc:pCvArr; const mask:pCvArr= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7859
{$ELSE}
name 'cvMultiplyAcc'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Adds image to accumulator with weights: acc = acc*(1-alpha) + image*alpha
@see cv::accumulateWeighted
*)
//CVAPI(void)  cvRunningAvg( const CvArr* image, CvArr* acc, double alpha,
//                          const CvArr* mask CV_DEFAULT(NULL) );
procedure cvRunningAvg(const image:pCvArr; acc:pCvArr; alpha:double; const mask:pCvArr= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7921
{$ELSE}
name 'cvRunningAvg'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(****************************************************************************************\
*                                    Image Processing                                    *
\****************************************************************************************)

(** Copies source 2D array inside of the larger destination array and
   makes a border of the specified type (IPL_BORDER_* ) around the copied area. *)
//CVAPI(void) cvCopyMakeBorder( const CvArr* src, CvArr* dst, CvPoint offset,
//                              int bordertype, CvScalar value CV_DEFAULT(cvScalarAll(0)));
procedure cvCopyMakeBorder(const src:pCvArr; dst:pCvArr; offset:TCvPoint; bordertype:int; value:TCvScalar); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7686
{$ELSE}
name 'cvCopyMakeBorder'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvCopyMakeBorder(const src:pCvArr; dst:pCvArr; offset:TCvPoint; bordertype:int);
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;

(** @brief Smooths the image in one of several ways.

@param src The source image
@param dst The destination image
@param smoothtype Type of the smoothing, see SmoothMethod_c
@param size1 The first parameter of the smoothing operation, the aperture width. Must be a
positive odd number (1, 3, 5, ...)
@param size2 The second parameter of the smoothing operation, the aperture height. Ignored by
CV_MEDIAN and CV_BILATERAL methods. In the case of simple scaled/non-scaled and Gaussian blur if
size2 is zero, it is set to size1. Otherwise it must be a positive odd number.
@param sigma1 In the case of a Gaussian parameter this parameter may specify Gaussian \f$\sigma\f$
(standard deviation). If it is zero, it is calculated from the kernel size:
\f[\sigma  = 0.3 (n/2 - 1) + 0.8  \quad   \text{where}   \quad  n= \begin{array}{l l} \mbox{\texttt{size1} for horizontal kernel} \\ \mbox{\texttt{size2} for vertical kernel} \end{array}\f]
Using standard sigma for small kernels ( \f$3\times 3\f$ to \f$7\times 7\f$ ) gives better speed. If
sigma1 is not zero, while size1 and size2 are zeros, the kernel size is calculated from the
sigma (to provide accurate enough operation).
@param sigma2 additional parameter for bilateral filtering

@see cv::GaussianBlur, cv::blur, cv::medianBlur, cv::bilateralFilter.
 *)
//CVAPI(void) cvSmooth( const CvArr* src, CvArr* dst,
//                      int smoothtype CV_DEFAULT(CV_GAUSSIAN),
//                      int size1 CV_DEFAULT(3),
//                      int size2 CV_DEFAULT(0),
//                      double sigma1 CV_DEFAULT(0),
//                      double sigma2 CV_DEFAULT(0));
procedure cvSmooth(const src:pCvArr; dst:pCvArr; smoothtype:int=CV_GAUSSIAN; size1:int=3; size2:int=0; sigma1:double=0; sigma2:double=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7979
{$ELSE}
name 'cvSmooth'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Convolves an image with the kernel.

@param src input image.
@param dst output image of the same size and the same number of channels as src.
@param kernel convolution kernel (or rather a correlation kernel), a single-channel floating point
matrix; if you want to apply different kernels to different channels, split the image into
separate color planes using split and process them individually.
@param anchor anchor of the kernel that indicates the relative position of a filtered point within
the kernel; the anchor should lie within the kernel; default value (-1,-1) means that the anchor
is at the kernel center.

@see cv::filter2D
 *)
//CVAPI(void) cvFilter2D( const CvArr* src, CvArr* dst, const CvMat* kernel,
//                        CvPoint anchor CV_DEFAULT(cvPoint(-1,-1)));
procedure cvFilter2D(const src:pCvArr; dst:pCvArr; const kernel:pCvMat; anchor:TCvPoint); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7742
{$ELSE}
name 'cvFilter2D'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvFilter2D(const src:pCvArr; dst:pCvArr; const kernel:pCvMat); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}


(** @brief Finds integral image: SUM(X,Y) = sum(x<X,y<Y)I(x,y)
@see cv::integral
*)
//CVAPI(void) cvIntegral( const CvArr* image, CvArr* sum,
//                       CvArr* sqsum CV_DEFAULT(NULL),
//                       CvArr* tilted_sum CV_DEFAULT(NULL));
procedure cvIntegral(const image:pCvArr; sum:pCvArr; sqsum:pCvArr= nil; tilted_sum:pCvArr= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7828
{$ELSE}
name 'cvIntegral'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(** @brief Smoothes the input image with gaussian kernel and then down-samples it.

   dst_width = floor(src_width/2)[+1],
   dst_height = floor(src_height/2)[+1]
   @see cv::pyrDown
*)
//CVAPI(void)  cvPyrDown( const CvArr* src, CvArr* dst,
//                        int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
procedure cvPyrDown(const src:pCvArr; dst:pCvArr; filter:int=CV_GAUSSIAN_5x5);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7885
{$ELSE}
name 'cvPyrDown'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Up-samples image and smoothes the result with gaussian kernel.

   dst_width = src_width*2,
   dst_height = src_height*2
   @see cv::pyrUp
*)
//CVAPI(void)  cvPyrUp( const CvArr* src, CvArr* dst,
//                      int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
procedure cvPyrUp(const src:pCvArr; dst:pCvArr; filter:int=CV_GAUSSIAN_5x5);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7887
{$ELSE}
name 'cvPyrUp'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Builds pyramid for an image
@see buildPyramid
*)
//CVAPI(CvMat**) cvCreatePyramid( const CvArr* img, int extra_layers, double rate,
//                                const CvSize* layer_sizes CV_DEFAULT(0),
//                                CvArr* bufarr CV_DEFAULT(0),
//                                int calc CV_DEFAULT(1),
//                                int filter CV_DEFAULT(CV_GAUSSIAN_5x5) );
function cvCreatePyramid(const img:pCvArr; extra_layers:int; rate:double; const layer_sizes:pCvSize=nil; bufarr:pCvArr=nil; calc:int=1; filter:int=CV_GAUSSIAN_5x5):pCvMat;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7706
{$ELSE}
name 'cvCreatePyramid'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Releases pyramid *)
//CVAPI(void)  cvReleasePyramid( CvMat*** pyramid, int extra_layers );
procedure cvReleasePyramid(pyramid:pCvMat; extra_layers:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7907
{$ELSE}
name 'cvReleasePyramid'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Filters image using meanshift algorithm
@see cv::pyrMeanShiftFiltering
*)
//CVAPI(void) cvPyrMeanShiftFiltering( const CvArr* src, CvArr* dst, double sp, double sr,
// int max_level CV_DEFAULT(1),
// CvTermCriteria termcrit CV_DEFAULT(cvTermCriteria(CV_TERMCRIT_ITER+CV_TERMCRIT_EPS,5,1))
//);
procedure cvPyrMeanShiftFiltering(const src:pCvArr; dst:pCvArr; sp:double; sr:double; max_level:int;
          termcrit:TCvTermCriteria); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7886
{$ELSE}
name 'cvPyrMeanShiftFiltering'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvPyrMeanShiftFiltering(const src:pCvArr; dst:pCvArr; sp:double; sr:double; max_level:int);
overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure cvPyrMeanShiftFiltering(const src:pCvArr; dst:pCvArr; sp:double; sr:double);
overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Segments image using seed "markers"
@see cv::watershed
*)
//CVAPI(void) cvWatershed( const CvArr* image, CvArr* markers );
procedure cvWatershed(const image:pCvArr; markers:pCvArr);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 8009
{$ELSE}
name 'cvWatershed'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates an image derivative using generalized Sobel

   (aperture_size = 1,3,5,7) or Scharr (aperture_size = -1) operator.
   Scharr can be used only for the first dx or dy derivative
@see cv::Sobel
*)
//CVAPI(void) cvSobel( const CvArr* src, CvArr* dst,
//                    int xorder, int yorder,
//                    int aperture_size CV_DEFAULT(3));
procedure cvSobel(const src:pCvArr; dst:pCvArr; xorder:int; yorder:int; aperture_size:int=3);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7980
{$ELSE}
name 'cvSobel'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates the image Laplacian: (d2/dx + d2/dy)I
@see cv::Laplacian
*)
//CVAPI(void) cvLaplace( const CvArr* src, CvArr* dst,
//                      int aperture_size CV_DEFAULT(3) );
procedure cvLaplace(const src:pCvArr; dst:pCvArr; aperture_size:int=3);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7832
{$ELSE}
name 'cvLaplace'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Converts input array pixels from one color space to another
@see cv::cvtColor
*)
//CVAPI(void)  cvCvtColor( const CvArr* src, CvArr* dst, int code );
procedure cvCvtColor(const src:pCvArr; dst:pCvArr; code:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7716
{$ELSE}
name 'cvCvtColor'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Resizes image (input array is resized to fit the destination array)
@see cv::resize
*)
//CVAPI(void)  cvResize( const CvArr* src, CvArr* dst,
//                       int interpolation CV_DEFAULT( CV_INTER_LINEAR ));
procedure cvResize(const src:pCvArr; dst:pCvArr; interpolation:int=CV_INTER_LINEAR);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7917
{$ELSE}
name 'cvResize'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Warps image with affine transform
@note ::cvGetQuadrangleSubPix is similar to ::cvWarpAffine, but the outliers are extrapolated using
replication border mode.
@see cv::warpAffine
*)
//CVAPI(void)  cvWarpAffine( const CvArr* src, CvArr* dst, const CvMat* map_matrix,
//                           int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
//                           CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
procedure cvWarpAffine(const src:pCvArr; dst:pCvArr; const map_matrix:pCvMat;
          flags:int; fillval:TCvScalar); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 8007
{$ELSE}
name 'cvWarpAffine'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvWarpAffine(const src:pCvArr; dst:pCvArr; const map_matrix:pCvMat;
          flags:int=CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Computes affine transform matrix for mapping src[i] to dst[i] (i=0,1,2)
@see cv::getAffineTransform
*)
//CVAPI(CvMat*) cvGetAffineTransform( const CvPoint2D32f * src,
//                                    const CvPoint2D32f * dst,
//                                    CvMat * map_matrix );
function cvGetAffineTransform(const src:pCvPoint2D32f; const dst:pCvPoint2D32f; map_matrix:pCvMat):pCvMat;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7758
{$ELSE}
name 'cvGetAffineTransform'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Computes rotation_matrix matrix
@see cv::getRotationMatrix2D
*)
//CVAPI(CvMat*)  cv2DRotationMatrix( CvPoint2D32f center, double angle,
//                                   double scale, CvMat* map_matrix );
function cv2DRotationMatrix(center:TCvPoint2D32f; angle:double; scale:double; map_matrix:pCvMat):pCvMat;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7624
{$ELSE}
name 'cv2DRotationMatrix'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Warps image with perspective (projective) transform
@see cv::warpPerspective
*)
//CVAPI(void)  cvWarpPerspective( const CvArr* src, CvArr* dst, const CvMat* map_matrix,
//                                int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
//                                CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
procedure cvWarpPerspective(const src:pCvArr; dst:pCvArr; const map_matrix:pCvMat;
          flags:int; fillval:TCvScalar); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 8008
{$ELSE}
name 'cvWarpPerspective'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvWarpPerspective(const src:pCvArr; dst:pCvArr; const map_matrix:pCvMat;
          flags:int=CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Computes perspective transform matrix for mapping src[i] to dst[i] (i=0,1,2,3)
@see cv::getPerspectiveTransform
*)
//CVAPI(CvMat*) cvGetPerspectiveTransform( const CvPoint2D32f* src,
//                                         const CvPoint2D32f* dst,
//                                         CvMat* map_matrix );
function cvGetPerspectiveTransform(const src:pCvPoint2D32f; const dst:pCvPoint2D32f; map_matrix:pCvMat):pCvMat;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7780
{$ELSE}
name 'cvGetPerspectiveTransform'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Performs generic geometric transformation using the specified coordinate maps
@see cv::remap
*)
//CVAPI(void)  cvRemap( const CvArr* src, CvArr* dst,
//                      const CvArr* mapx, const CvArr* mapy,
//                      int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS),
//                      CvScalar fillval CV_DEFAULT(cvScalarAll(0)) );
procedure cvRemap(const src:pCvArr; dst:pCvArr; const mapx:pCvArr; const mapy:pCvArr;
          flags:int; fillval:TCvScalar); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7911
{$ELSE}
name 'cvRemap'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvRemap(const src:pCvArr; dst:pCvArr; const mapx:pCvArr; const mapy:pCvArr;
          flags:int=CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Converts mapx & mapy from floating-point to integer formats for cvRemap
@see cv::convertMaps
*)
//CVAPI(void)  cvConvertMaps( const CvArr* mapx, const CvArr* mapy,
//                            CvArr* mapxy, CvArr* mapalpha );
procedure cvConvertMaps(const mapx:pCvArr; const mapy:pCvArr; mapxy:pCvArr; mapalpha:pCvArr);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7679
{$ELSE}
name 'cvConvertMaps'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Performs forward or inverse log-polar image transform
@see cv::warpPolar
*)
//CVAPI(void)  cvLogPolar( const CvArr* src, CvArr* dst,
//                         CvPoint2D32f center, double M,
//                         int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS));
procedure cvLogPolar(const src:pCvArr; dst:pCvArr; center:TCvPoint2D32f; M:double; flags:int=CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7836
{$ELSE}
name 'cvLogPolar'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** Performs forward or inverse linear-polar image transform
@see cv::warpPolar
*)
//CVAPI(void)  cvLinearPolar( const CvArr* src, CvArr* dst,
//                         CvPoint2D32f center, double maxRadius,
//                         int flags CV_DEFAULT(CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS));
procedure cvLinearPolar(const src:pCvArr; dst:pCvArr; center:TCvPoint2D32f; maxRadius:double; flags:int=CV_INTER_LINEAR+CV_WARP_FILL_OUTLIERS);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7834
{$ELSE}
name 'cvLinearPolar'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Returns a structuring element of the specified size and shape for morphological operations.

@note the created structuring element IplConvKernel\* element must be released in the end using
`cvReleaseStructuringElement(&element)`.

@param cols Width of the structuring element
@param rows Height of the structuring element
@param anchor_x x-coordinate of the anchor
@param anchor_y y-coordinate of the anchor
@param shape element shape that could be one of the cv::MorphShapes_c
@param values integer array of cols*rows elements that specifies the custom shape of the
structuring element, when shape=CV_SHAPE_CUSTOM.

@see cv::getStructuringElement
 *)
// CVAPI(IplConvKernel*)  cvCreateStructuringElementEx(
//            int cols, int  rows, int  anchor_x, int  anchor_y,
//            int shape, int* values CV_DEFAULT(NULL) );
function cvCreateStructuringElementEx(cols:int; rows:int; anchor_x:int; anchor_y:int; shape:int; values:pint= nil):pIplConvKernel;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7711
{$ELSE}
name 'cvCreateStructuringElementEx'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief releases structuring element
@see cvCreateStructuringElementEx
*)
//CVAPI(void)  cvReleaseStructuringElement( IplConvKernel** element );
procedure cvReleaseStructuringElement(element:pIplConvKernel);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7909
{$ELSE}
name 'cvReleaseStructuringElement'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief erodes input image (applies minimum filter) one or more times.
   If element pointer is NULL, 3x3 rectangular element is used
@see cv::erode
*)
//CVAPI(void)  cvErode( const CvArr* src, CvArr* dst,
//                      IplConvKernel* element CV_DEFAULT(NULL),
//                      int iterations CV_DEFAULT(1) );
procedure cvErode(const src:pCvArr; dst:pCvArr; element:pIplConvKernel= nil; iterations:int=1);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7734
{$ELSE}
name 'cvErode'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief dilates input image (applies maximum filter) one or more times.

   If element pointer is NULL, 3x3 rectangular element is used
@see cv::dilate
*)
//CVAPI(void)  cvDilate( const CvArr* src, CvArr* dst,
//                       IplConvKernel* element CV_DEFAULT(NULL),
//                       int iterations CV_DEFAULT(1) );
procedure cvDilate(const src:pCvArr; dst:pCvArr; element:pIplConvKernel= nil; iterations:int=1);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7723
{$ELSE}
name 'cvDilate'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Performs complex morphological transformation
@see cv::morphologyEx
*)
//CVAPI(void)  cvMorphologyEx( const CvArr* src, CvArr* dst,
//                             CvArr* temp, IplConvKernel* element,
//                             int operation, int iterations CV_DEFAULT(1) );
procedure cvMorphologyEx(const src:pCvArr; dst:pCvArr; temp:pCvArr; element:pIplConvKernel; operation:int; iterations:int=1);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7854
{$ELSE}
name 'cvMorphologyEx'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates all spatial and central moments up to the 3rd order
@see cv::moments
*)
//CVAPI(void) cvMoments( const CvArr* arr, CvMoments* moments, int binary CV_DEFAULT(0));
procedure cvMoments(const arr:pCvArr; moments:pCvMoments; binary:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7853
{$ELSE}
name 'cvMoments'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Retrieve spatial moments *)
//CVAPI(double)  cvGetSpatialMoment( CvMoments* moments, int x_order, int y_order );
function cvGetSpatialMoment(moments:pCvMoments; x_order:int; y_order:int):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7792
{$ELSE}
name 'cvGetSpatialMoment'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Retrieve central moments *)
//CVAPI(double)  cvGetCentralMoment( CvMoments* moments, int x_order, int y_order );
function cvGetCentralMoment(moments:pCvMoments; x_order:int; y_order:int):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7761
{$ELSE}
name 'cvGetCentralMoment'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Retrieve normalized central moments *)
//CVAPI(double)  cvGetNormalizedCentralMoment( CvMoments* moments,
//                                             int x_order, int y_order );
function cvGetNormalizedCentralMoment(moments:pCvMoments; x_order:int; y_order:int):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7777
{$ELSE}
name 'cvGetNormalizedCentralMoment'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates 7 Hu's invariants from precalculated spatial and central moments
@see cv::HuMoments
*)
//CVAPI(void) cvGetHuMoments( CvMoments*  moments, CvHuMoments*  hu_moments );
procedure cvGetHuMoments(moments:pCvMoments; hu_moments:pCvHuMoments);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7770
{$ELSE}
name 'cvGetHuMoments'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*********************************** data sampling **************************************)

(** @brief Fetches pixels that belong to the specified line segment and stores them to the buffer.

   Returns the number of retrieved points.
@see cv::LineSegmentDetector
*)
//CVAPI(int)  cvSampleLine( const CvArr* image, CvPoint pt1, CvPoint pt2, void* buffer,
//                          int connectivity CV_DEFAULT(8));
function cvSampleLine(const image:pCvArr; pt1:TCvPoint; pt2:TCvPoint; buffer:pvoid; connectivity:int=8):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7924
{$ELSE}
name 'cvSampleLine'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Retrieves the rectangular image region with specified center from the input array.

 dst(x,y) <- src(x + center.x - dst_width/2, y + center.y - dst_height/2).
 Values of pixels with fractional coordinates are retrieved using bilinear interpolation
@see cv::getRectSubPix
*)
//CVAPI(void)  cvGetRectSubPix( const CvArr* src, CvArr* dst, CvPoint2D32f center );
procedure cvGetRectSubPix(const src:pCvArr; dst:pCvArr; center:TCvPoint2D32f);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7787
{$ELSE}
name 'cvGetRectSubPix'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(** @brief Retrieves quadrangle from the input array.

    matrixarr = ( a11  a12 | b1 )   dst(x,y) <- src(A[x y]' + b)
                ( a21  a22 | b2 )   (bilinear interpolation is used to retrieve pixels
                                     with fractional coordinates)
@see cvWarpAffine
*)
//CVAPI(void)  cvGetQuadrangleSubPix( const CvArr* src, CvArr* dst,
//                                    const CvMat* map_matrix );
procedure cvGetQuadrangleSubPix(const src:pCvArr; dst:pCvArr; const map_matrix:pCvMat);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7781
{$ELSE}
name 'cvGetQuadrangleSubPix'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Measures similarity between template and overlapped windows in the source image
   and fills the resultant image with the measurements
@see cv::matchTemplate
*)
//CVAPI(void)  cvMatchTemplate( const CvArr* image, const CvArr* templ,
//                              CvArr* result, int method );
procedure cvMatchTemplate(const image:pCvArr; const templ:pCvArr; result:pCvArr; method:int);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7841
{$ELSE}
name 'cvMatchTemplate'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Computes earth mover distance between
   two weighted point sets (called signatures)
@see cv::EMD
*)
//CVAPI(float)  cvCalcEMD2( const CvArr* signature1,
//                          const CvArr* signature2,
//                          int distance_type,
//                          CvDistanceFunction distance_func CV_DEFAULT(NULL),
//                          const CvArr* cost_matrix CV_DEFAULT(NULL),
//                          CvArr* flow CV_DEFAULT(NULL),
//                          float* lower_bound CV_DEFAULT(NULL),
//                          void* userdata CV_DEFAULT(NULL));
function cvCalcEMD2(const signature1:pCvArr; const signature2:pCvArr; distance_type:int;
         distance_func:TCvDistanceFunction= nil; const cost_matrix:pCvArr= nil;
         flow:pCvArr= nil; lower_bound:pfloat= nil; userdata:pvoid= nil):float;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7648
{$ELSE}
name 'cvCalcEMD2'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(****************************************************************************************\
*                              Contours retrieving                                       *
\****************************************************************************************)

(** @brief Retrieves outer and optionally inner boundaries of white (non-zero) connected
   components in the black (zero) background
@see cv::findContours, cvStartFindContours, cvFindNextContour, cvSubstituteContour, cvEndFindContours
*)
//CVAPI(int)  cvFindContours( CvArr* image, CvMemStorage* storage, CvSeq** first_contour,
//                            int header_size CV_DEFAULT(sizeof(CvContour)),
//                            int mode CV_DEFAULT(CV_RETR_LIST),
//                            int method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE),
//                            CvPoint offset CV_DEFAULT(cvPoint(0,0)));
function cvFindContours(image:pCvArr; storage:pCvMemStorage; first_contour:pCvSeq;
         header_size:int; mode:int;
         method:int; offset:TCvPoint):int; overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7743
{$ELSE}
name 'cvFindContours'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function cvFindContours(image:pCvArr; storage:pCvMemStorage; first_contour:pCvSeq;
         header_size:int=sizeof(TCvContour); mode:int=CV_RETR_LIST;
         method:int=CV_CHAIN_APPROX_SIMPLE):int;
         overload; {$IFDEF USE_INLINE}inline; {$ENDIF}


(** @brief Initializes contour retrieving process.

   Calls cvStartFindContours.
   Calls cvFindNextContour until null pointer is returned
   or some other condition becomes true.
   Calls cvEndFindContours at the end.
@see cvFindContours
*)
//CVAPI(CvContourScanner)  cvStartFindContours( CvArr* image, CvMemStorage* storage,
//                            int header_size CV_DEFAULT(sizeof(CvContour)),
//                            int mode CV_DEFAULT(CV_RETR_LIST),
//                            int method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE),
//                            CvPoint offset CV_DEFAULT(cvPoint(0,0)));
function cvStartFindContours(image:pCvArr; storage:pCvMemStorage; header_size:int;
         mode:int; method:int;
         offset:TCvPoint):TCvContourScanner; overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7988
{$ELSE}
name 'cvStartFindContours'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function cvStartFindContours(image:pCvArr; storage:pCvMemStorage; header_size:int=sizeof(TCvContour);
         mode:int=CV_RETR_LIST; method:int=CV_CHAIN_APPROX_SIMPLE):TCvContourScanner;
         overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Retrieves next contour
@see cvFindContours
*)
//CVAPI(CvSeq*)  cvFindNextContour( CvContourScanner scanner );
function cvFindNextContour(scanner:TCvContourScanner):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7747
{$ELSE}
name 'cvFindNextContour'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Substitutes the last retrieved contour with the new one

   (if the substitutor is null, the last retrieved contour is removed from the tree)
@see cvFindContours
*)
//CVAPI(void)   cvSubstituteContour( CvContourScanner scanner, CvSeq* new_contour );
procedure cvSubstituteContour(scanner:TCvContourScanner; new_contour:pCvSeq);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7996
{$ELSE}
name 'cvSubstituteContour'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Releases contour scanner and returns pointer to the first outer contour
@see cvFindContours
*)
//CVAPI(CvSeq*)  cvEndFindContours( CvContourScanner* scanner );
function cvEndFindContours(scanner:pCvContourScanner):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7731
{$ELSE}
name 'cvEndFindContours'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Approximates Freeman chain(s) with a polygonal curve.

This is a standalone contour approximation routine, not represented in the new interface. When
cvFindContours retrieves contours as Freeman chains, it calls the function to get approximated
contours, represented as polygons.

@param src_seq Pointer to the approximated Freeman chain that can refer to other chains.
@param storage Storage location for the resulting polylines.
@param method Approximation method (see the description of the function :ocvFindContours ).
@param parameter Method parameter (not used now).
@param minimal_perimeter Approximates only those contours whose perimeters are not less than
minimal_perimeter . Other chains are removed from the resulting structure.
@param recursive Recursion flag. If it is non-zero, the function approximates all chains that can
be obtained from chain by using the h_next or v_next links. Otherwise, the single input chain is
approximated.
@see cvStartReadChainPoints, cvReadChainPoint
 *)
//CVAPI(CvSeq*) cvApproxChains( CvSeq* src_seq, CvMemStorage* storage,
//                            int method CV_DEFAULT(CV_CHAIN_APPROX_SIMPLE),
//                            double parameter CV_DEFAULT(0),
//                            int  minimal_perimeter CV_DEFAULT(0),
//                            int  recursive CV_DEFAULT(0));
function cvApproxChains(src_seq:pCvSeq; storage:pCvMemStorage; method:int=CV_CHAIN_APPROX_SIMPLE; parameter:double=0; minimal_perimeter:int=0; recursive:int=0):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7635
{$ELSE}
name 'cvApproxChains'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Initializes Freeman chain reader.

   The reader is used to iteratively get coordinates of all the chain points.
   If the Freeman codes should be read as is, a simple sequence reader should be used
@see cvApproxChains
*)
//CVAPI(void) cvStartReadChainPoints( CvChain* chain, CvChainPtReader* reader );
procedure cvStartReadChainPoints(chain:pCvChain; reader:pCvChainPtReader);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7989
{$ELSE}
name 'cvStartReadChainPoints'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Retrieves the next chain point
@see cvApproxChains
*)
//CVAPI(CvPoint) cvReadChainPoint( CvChainPtReader* reader );
function cvReadChainPoint(reader:pCvChainPtReader):TCvPoint;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7893
{$ELSE}
name 'cvReadChainPoint'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(****************************************************************************************\
*                            Contour Processing and Shape Analysis                       *
\****************************************************************************************)

(** @brief Approximates a single polygonal curve (contour) or
   a tree of polygonal curves (contours)
@see cv::approxPolyDP
*)
//CVAPI(CvSeq*)  cvApproxPoly( const void* src_seq,
//                             int header_size, CvMemStorage* storage,
//                             int method, double eps,
//                             int recursive CV_DEFAULT(0));
function cvApproxPoly(const src_seq:pvoid; header_size:int; storage:pCvMemStorage; method:int; eps:double; recursive:int=0):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7636
{$ELSE}
name 'cvApproxPoly'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates perimeter of a contour or length of a part of contour
@see cv::arcLength
*)
//CVAPI(double)  cvArcLength( const void* curve,
//                            CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
//                            int is_closed CV_DEFAULT(-1));
function cvArcLength(const curve:pvoid; slice:TCvSlice; is_closed:int):double; overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7637
{$ELSE}
name 'cvArcLength'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function cvArcLength(const curve:pvoid; slice:TCvSlice):double; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
function cvArcLength(const curve:pvoid):double; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** same as cvArcLength for closed contour
*)
//CV_INLINE double cvContourPerimeter( const void* contour )
//{
//    return cvArcLength( contour, CV_WHOLE_SEQ, 1 );
//}
function cvContourPerimeter(const contour:pvoid):double;inline;


(** @brief Calculates contour bounding rectangle (update=1) or
   just retrieves pre-calculated rectangle (update=0)
@see cv::boundingRect
*)
//CVAPI(CvRect)  cvBoundingRect( CvArr* points, int update CV_DEFAULT(0) );
function cvBoundingRect(points:pCvArr; update:int=0):TCvRect;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7641
{$ELSE}
name 'cvBoundingRect'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates area of a contour or contour segment
@see cv::contourArea
*)
//CVAPI(double)  cvContourArea( const CvArr* contour,
//                              CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ),
//                              int oriented CV_DEFAULT(0));
function cvContourArea(const contour:pCvArr; slice:TCvSlice; oriented:int):double; overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7678
{$ELSE}
name 'cvContourArea'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function cvContourArea(const contour:pCvArr; slice:TCvSlice):double; overload;{$IFDEF USE_INLINE}inline; {$ENDIF}
function cvContourArea(const contour:pCvArr):double; overload;{$IFDEF USE_INLINE}inline; {$ENDIF}

(** @brief Finds minimum area rotated rectangle bounding a set of points
@see cv::minAreaRect
*)
//CVAPI(CvBox2D)  cvMinAreaRect2( const CvArr* points,
//                                CvMemStorage* storage CV_DEFAULT(NULL));
function cvMinAreaRect2(const points:pCvArr; storage:pCvMemStorage= nil):TCvBox2D;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7848
{$ELSE}
name 'cvMinAreaRect2'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds minimum enclosing circle for a set of points
@see cv::minEnclosingCircle
*)
//CVAPI(int)  cvMinEnclosingCircle( const CvArr* points,
//                                  CvPoint2D32f* center, float* radius );
function cvMinEnclosingCircle(const points:pCvArr; center:pCvPoint2D32f; radius:pfloat):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7849
{$ELSE}
name 'cvMinEnclosingCircle'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Compares two contours by matching their moments
@see cv::matchShapes
*)
//CVAPI(double)  cvMatchShapes( const void* object1, const void* object2,
//                              int method, double parameter CV_DEFAULT(0));
function cvMatchShapes(const object1:pvoid; const object2:pvoid; method:int; parameter:double=0):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7840
{$ELSE}
name 'cvMatchShapes'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates exact convex hull of 2d point set
@see cv::convexHull
*)
//CVAPI(CvSeq*) cvConvexHull2( const CvArr* input,
//                             void* hull_storage CV_DEFAULT(NULL),
//                             int orientation CV_DEFAULT(CV_CLOCKWISE),
//                             int return_points CV_DEFAULT(0));
function cvConvexHull2(const input:pCvArr; hull_storage:pvoid= nil; orientation:int=CV_CLOCKWISE; return_points:int=0):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7682
{$ELSE}
name 'cvConvexHull2'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Checks whether the contour is convex or not (returns 1 if convex, 0 if not)
@see cv::isContourConvex
*)
//CVAPI(int)  cvCheckContourConvexity( const CvArr* contour );
function cvCheckContourConvexity(const contour:pCvArr):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7656
{$ELSE}
name 'cvCheckContourConvexity'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds convexity defects for the contour
@see cv::convexityDefects
*)
//CVAPI(CvSeq*)  cvConvexityDefects( const CvArr* contour, const CvArr* convexhull,
//                                   CvMemStorage* storage CV_DEFAULT(NULL));
function cvConvexityDefects(const contour:pCvArr; const convexhull:pCvArr; storage:pCvMemStorage= nil):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7683
{$ELSE}
name 'cvConvexityDefects'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Fits ellipse into a set of 2d points
@see cv::fitEllipse
*)
//CVAPI(CvBox2D) cvFitEllipse2( const CvArr* points );
function cvFitEllipse2(const points:pCvArr):TCvBox2D;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7748
{$ELSE}
name 'cvFitEllipse2'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds minimum rectangle containing two given rectangles *)
//CVAPI(CvRect)  cvMaxRect( const CvRect* rect1, const CvRect* rect2 );
function cvMaxRect(const rect1:pCvRect; const rect2:pCvRect):TCvRect;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7843
{$ELSE}
name 'cvMaxRect'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds coordinates of the box vertices *)
//CVAPI(void) cvBoxPoints( CvBox2D box, CvPoint2D32f pt[4] );
Type
  pt4 = array[0..4-1] of TCvPoint2D32f;
procedure cvBoxPoints(box:TCvBox2D; pt:pt4);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7642
{$ELSE}
name 'cvBoxPoints'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Initializes sequence header for a matrix (column or row vector) of points

   a wrapper for cvMakeSeqHeaderForArray (it does not initialize bounding rectangle!!!) *)
//CVAPI(CvSeq*) cvPointSeqFromMat( int seq_kind, const CvArr* mat,
//                                 CvContour* contour_header,
//                                 CvSeqBlock* block );
function cvPointSeqFromMat(seq_kind:int; const mat:pCvArr; contour_header:pCvContour; block:pCvSeqBlock):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7873
{$ELSE}
name 'cvPointSeqFromMat'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Checks whether the point is inside polygon, outside, on an edge (at a vertex).

   Returns positive, negative or zero value, correspondingly.
   Optionally, measures a signed distance between
   the point and the nearest polygon edge (measure_dist=1)
@see cv::pointPolygonTest
*)
//CVAPI(double) cvPointPolygonTest( const CvArr* contour,
//                                  CvPoint2D32f pt, int measure_dist );
function cvPointPolygonTest(const contour:pCvArr; pt:TCvPoint2D32f; measure_dist:int):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7872
{$ELSE}
name 'cvPointPolygonTest'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(****************************************************************************************\
*                                  Histogram functions                                   *
\****************************************************************************************)

(** @brief Creates a histogram.

The function creates a histogram of the specified size and returns a pointer to the created
histogram. If the array ranges is 0, the histogram bin ranges must be specified later via the
function cvSetHistBinRanges. Though cvCalcHist and cvCalcBackProject may process 8-bit images
without setting bin ranges, they assume they are equally spaced in 0 to 255 bins.

@param dims Number of histogram dimensions.
@param sizes Array of the histogram dimension sizes.
@param type Histogram representation format. CV_HIST_ARRAY means that the histogram data is
represented as a multi-dimensional dense array CvMatND. CV_HIST_SPARSE means that histogram data
is represented as a multi-dimensional sparse array CvSparseMat.
@param ranges Array of ranges for the histogram bins. Its meaning depends on the uniform parameter
value. The ranges are used when the histogram is calculated or backprojected to determine which
histogram bin corresponds to which value/tuple of values from the input image(s).
@param uniform Uniformity flag. If not zero, the histogram has evenly spaced bins and for every
\f$0<=i<cDims\f$ ranges[i] is an array of two numbers: lower and upper boundaries for the i-th
histogram dimension. The whole range [lower,upper] is then split into dims[i] equal parts to
determine the i-th input tuple value ranges for every histogram bin. And if uniform=0 , then the
i-th element of the ranges array contains dims[i]+1 elements: \f$\texttt{lower}_0,
\texttt{upper}_0, \texttt{lower}_1, \texttt{upper}_1 = \texttt{lower}_2,
...
\texttt{upper}_{dims[i]-1}\f$ where \f$\texttt{lower}_j\f$ and \f$\texttt{upper}_j\f$ are lower
and upper boundaries of the i-th input tuple value for the j-th bin, respectively. In either
case, the input values that are beyond the specified range for a histogram bin are not counted
by cvCalcHist and filled with 0 by cvCalcBackProject.
 *)
//CVAPI(CvHistogram*)  cvCreateHist( int dims, int* sizes, int type,
//                                   float** ranges CV_DEFAULT(NULL),
//                                   int uniform CV_DEFAULT(1));
function cvCreateHist(dims:int; sizes:pint; &type:int; ranges:pfloat= nil; uniform:int=1):pCvHistogram;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7698
{$ELSE}
name 'cvCreateHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Sets the bounds of the histogram bins.

This is a standalone function for setting bin ranges in the histogram. For a more detailed
description of the parameters ranges and uniform, see the :ocvCalcHist function that can initialize
the ranges as well. Ranges for the histogram bins must be set before the histogram is calculated or
the backproject of the histogram is calculated.

@param hist Histogram.
@param ranges Array of bin ranges arrays. See :ocvCreateHist for details.
@param uniform Uniformity flag. See :ocvCreateHist for details.
 *)
//CVAPI(void)  cvSetHistBinRanges( CvHistogram* hist, float** ranges,
//                                int uniform CV_DEFAULT(1));
procedure cvSetHistBinRanges(hist:pCvHistogram; ranges:pfloat; uniform:int=1);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7953
{$ELSE}
name 'cvSetHistBinRanges'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Makes a histogram out of an array.

The function initializes the histogram, whose header and bins are allocated by the user.
cvReleaseHist does not need to be called afterwards. Only dense histograms can be initialized this
way. The function returns hist.

@param dims Number of the histogram dimensions.
@param sizes Array of the histogram dimension sizes.
@param hist Histogram header initialized by the function.
@param data Array used to store histogram bins.
@param ranges Histogram bin ranges. See cvCreateHist for details.
@param uniform Uniformity flag. See cvCreateHist for details.
 *)
//CVAPI(CvHistogram*)  cvMakeHistHeaderForArray(
//                            int  dims, int* sizes, CvHistogram* hist,
//                            float* data, float** ranges CV_DEFAULT(NULL),
//                            int uniform CV_DEFAULT(1));
function cvMakeHistHeaderForArray(dims:int; sizes:pint; hist:pCvHistogram; data:pfloat; ranges:pfloat= nil; uniform:int=1):pCvHistogram;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7838
{$ELSE}
name 'cvMakeHistHeaderForArray'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Releases the histogram.

The function releases the histogram (header and the data). The pointer to the histogram is cleared
by the function. If \*hist pointer is already NULL, the function does nothing.

@param hist Double pointer to the released histogram.
 *)
//CVAPI(void)  cvReleaseHist( CvHistogram** hist );
procedure cvReleaseHist(hist:pCvHistogram);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7902
{$ELSE}
name 'cvReleaseHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Clears the histogram.

The function sets all of the histogram bins to 0 in case of a dense histogram and removes all
histogram bins in case of a sparse array.

@param hist Histogram.
 *)
//CVAPI(void)  cvClearHist( CvHistogram* hist );
procedure cvClearHist(hist:pCvHistogram);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7661
{$ELSE}
name 'cvClearHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds the minimum and maximum histogram bins.

The function finds the minimum and maximum histogram bins and their positions. All of output
arguments are optional. Among several extremas with the same value the ones with the minimum index
(in the lexicographical order) are returned. In case of several maximums or minimums, the earliest
in the lexicographical order (extrema locations) is returned.

@param hist Histogram.
@param min_value Pointer to the minimum value of the histogram.
@param max_value Pointer to the maximum value of the histogram.
@param min_idx Pointer to the array of coordinates for the minimum.
@param max_idx Pointer to the array of coordinates for the maximum.
 *)
//CVAPI(void)  cvGetMinMaxHistValue( const CvHistogram* hist,
//                                   float* min_value, float* max_value,
//                                   int* min_idx CV_DEFAULT(NULL),
//                                   int* max_idx CV_DEFAULT(NULL));
procedure cvGetMinMaxHistValue(const hist:pCvHistogram; min_value:pfloat; max_value:pfloat; min_idx:pint= nil; max_idx:pint= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7775
{$ELSE}
name 'cvGetMinMaxHistValue'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Normalizes the histogram.

The function normalizes the histogram bins by scaling them so that the sum of the bins becomes equal
to factor.

@param hist Pointer to the histogram.
@param factor Normalization factor.
 *)
//CVAPI(void)  cvNormalizeHist( CvHistogram* hist, double factor );
procedure cvNormalizeHist(hist:pCvHistogram; factor:double);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7866
{$ELSE}
name 'cvNormalizeHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Thresholds the histogram.

The function clears histogram bins that are below the specified threshold.

@param hist Pointer to the histogram.
@param threshold Threshold level.
 *)
//CVAPI(void)  cvThreshHist( CvHistogram* hist, double threshold );
procedure cvThreshHist(hist:pCvHistogram; threshold:double);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7998
{$ELSE}
name 'cvThreshHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** Compares two histogram *)
//CVAPI(double)  cvCompareHist( const CvHistogram* hist1,
//                              const CvHistogram* hist2,
//                              int method);
function cvCompareHist(const hist1:pCvHistogram; const hist2:pCvHistogram; method:int):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7676
{$ELSE}
name 'cvCompareHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Copies a histogram.

The function makes a copy of the histogram. If the second histogram pointer \*dst is NULL, a new
histogram of the same size as src is created. Otherwise, both histograms must have equal types and
sizes. Then the function copies the bin values of the source histogram to the destination histogram
and sets the same bin value ranges as in src.

@param src Source histogram.
@param dst Pointer to the destination histogram.
 *)
//CVAPI(void)  cvCopyHist( const CvHistogram* src, CvHistogram** dst );
procedure cvCopyHist(const src:pCvHistogram; dst:pCvHistogram);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7685
{$ELSE}
name 'cvCopyHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates bayesian probabilistic histograms
   (each or src and dst is an array of _number_ histograms *)
//CVAPI(void)  cvCalcBayesianProb( CvHistogram** src, int number,
//                                CvHistogram** dst);
procedure cvCalcBayesianProb(src:pCvHistogram; number:int; dst:pCvHistogram);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7646
{$ELSE}
name 'cvCalcBayesianProb'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates array histogram
@see cv::calcHist
*)
//CVAPI(void)  cvCalcArrHist( CvArr** arr, CvHistogram* hist,
//                            int accumulate CV_DEFAULT(0),
//                            const CvArr* mask CV_DEFAULT(NULL) );
procedure cvCalcArrHist(arr:pCvArr; hist:pCvHistogram; accumulate:int=0; const mask:pCvArr= nil);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7645
{$ELSE}
name 'cvCalcArrHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @overload *)
//CV_INLINE  void  cvCalcHist( IplImage** image, CvHistogram* hist,
//                             int accumulate CV_DEFAULT(0),
//                             const CvArr* mask CV_DEFAULT(NULL) )
//{
//    cvCalcArrHist( (CvArr**)image, hist, accumulate, mask );
//}
procedure cvCalcHist(image:pIplImage; hist:pCvHistogram; accumulate:int=0; const mask:pCvArr= nil);inline;

(** @brief Calculates back project
@see cvCalcBackProject, cv::calcBackProject
*)
//CVAPI(void)  cvCalcArrBackProject( CvArr** image, CvArr* dst,
//                                   const CvHistogram* hist );
procedure cvCalcArrBackProject(image:pCvArr; dst:pCvArr; const hist:pCvHistogram);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7643
{$ELSE}
name 'cvCalcArrBackProject'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{TODO:cvCalcBackProject}
//#define  cvCalcBackProject(image, dst, hist) cvCalcArrBackProject((CvArr**)image, dst, hist)


(** @brief Locates a template within an image by using a histogram comparison.

The function calculates the back projection by comparing histograms of the source image patches with
the given histogram. The function is similar to matchTemplate, but instead of comparing the raster
patch with all its possible positions within the search window, the function CalcBackProjectPatch
compares histograms. See the algorithm diagram below:

![image](pics/backprojectpatch.png)

@param image Source images (though, you may pass CvMat\*\* as well).
@param dst Destination image.
@param range
@param hist Histogram.
@param method Comparison method passed to cvCompareHist (see the function description).
@param factor Normalization factor for histograms that affects the normalization scale of the
destination image. Pass 1 if not sure.

@see cvCalcBackProjectPatch
 *)
//CVAPI(void)  cvCalcArrBackProjectPatch( CvArr** image, CvArr* dst, CvSize range,
//                                        CvHistogram* hist, int method,
//                                        double factor );
procedure cvCalcArrBackProjectPatch(image:pCvArr; dst:pCvArr; range:TCvSize; hist:pCvHistogram; method:int; factor:double);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7644
{$ELSE}
name 'cvCalcArrBackProjectPatch'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{TODO:cvCalcBackProjectPatch}
//#define  cvCalcBackProjectPatch( image, dst, range, hist, method, factor ) \
//     cvCalcArrBackProjectPatch( (CvArr**)image, dst, range, hist, method, factor )


(** @brief Divides one histogram by another.

The function calculates the object probability density from two histograms as:

\f[\texttt{disthist} (I)= \forkthree{0}{if \(\texttt{hist1}(I)=0\)}{\texttt{scale}}{if \(\texttt{hist1}(I) \ne 0\) and \(\texttt{hist2}(I) > \texttt{hist1}(I)\)}{\frac{\texttt{hist2}(I) \cdot \texttt{scale}}{\texttt{hist1}(I)}}{if \(\texttt{hist1}(I) \ne 0\) and \(\texttt{hist2}(I) \le \texttt{hist1}(I)\)}\f]

@param hist1 First histogram (the divisor).
@param hist2 Second histogram.
@param dst_hist Destination histogram.
@param scale Scale factor for the destination histogram.
 *)
//CVAPI(void)  cvCalcProbDensity( const CvHistogram* hist1, const CvHistogram* hist2,
//                                CvHistogram* dst_hist, double scale CV_DEFAULT(255) );
procedure cvCalcProbDensity(const hist1:pCvHistogram; const hist2:pCvHistogram; dst_hist:pCvHistogram; scale:double=255);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7650
{$ELSE}
name 'cvCalcProbDensity'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief equalizes histogram of 8-bit single-channel image
@see cv::equalizeHist
*)
//CVAPI(void)  cvEqualizeHist( const CvArr* src, CvArr* dst );
procedure cvEqualizeHist(const src:pCvArr; dst:pCvArr);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7733
{$ELSE}
name 'cvEqualizeHist'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Applies distance transform to binary image
@see cv::distanceTransform
*)
//CVAPI(void)  cvDistTransform( const CvArr* src, CvArr* dst,
//                              int distance_type CV_DEFAULT(CV_DIST_L2),
//                              int mask_size CV_DEFAULT(3),
//                              const float* mask CV_DEFAULT(NULL),
//                              CvArr* labels CV_DEFAULT(NULL),
//                              int labelType CV_DEFAULT(CV_DIST_LABEL_CCOMP));
procedure cvDistTransform(const src:pCvArr; dst:pCvArr; distance_type:int=CV_DIST_L2;
          mask_size:int=3; const mask:pfloat= nil; labels:pCvArr= nil; labelType:int=CV_DIST_LABEL_CCOMP);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7724
{$ELSE}
name 'cvDistTransform'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(** @brief Applies fixed-level threshold to grayscale image.

   This is a basic operation applied before retrieving contours
@see cv::threshold
*)
//CVAPI(double)  cvThreshold( const CvArr*  src, CvArr*  dst,
//                            double  threshold, double  max_value,
//                            int threshold_type );
function cvThreshold(const src:pCvArr; dst:pCvArr; threshold:double; max_value:double; threshold_type:int):double;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7999
{$ELSE}
name 'cvThreshold'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Applies adaptive threshold to grayscale image.

   The two parameters for methods CV_ADAPTIVE_THRESH_MEAN_C and
   CV_ADAPTIVE_THRESH_GAUSSIAN_C are:
   neighborhood size (3, 5, 7 etc.),
   and a constant subtracted from mean (...,-3,-2,-1,0,1,2,3,...)
@see cv::adaptiveThreshold
*)
//CVAPI(void)  cvAdaptiveThreshold( const CvArr* src, CvArr* dst, double max_value,
//                                  int adaptive_method CV_DEFAULT(CV_ADAPTIVE_THRESH_MEAN_C),
//                                  int threshold_type CV_DEFAULT(CV_THRESH_BINARY),
//                                  int block_size CV_DEFAULT(3),
//                                  double param1 CV_DEFAULT(5));
procedure cvAdaptiveThreshold(const src:pCvArr; dst:pCvArr; max_value:double; adaptive_method:int=CV_ADAPTIVE_THRESH_MEAN_C; threshold_type:int=CV_THRESH_BINARY; block_size:int=3; param1:double=5);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7628
{$ELSE}
name 'cvAdaptiveThreshold'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Fills the connected component until the color difference gets large enough
@see cv::floodFill
*)
//CVAPI(void)  cvFloodFill( CvArr* image, CvPoint seed_point,
//                          CvScalar new_val,
//                          CvScalar lo_diff CV_DEFAULT(cvScalarAll(0)),
//                          CvScalar up_diff CV_DEFAULT(cvScalarAll(0)),
//                          CvConnectedComp* comp CV_DEFAULT(NULL),
//                          int flags CV_DEFAULT(4),
//                          CvArr* mask CV_DEFAULT(NULL));
procedure cvFloodFill(image:pCvArr; seed_point:TCvPoint; new_val:TCvScalar; lo_diff:TCvScalar;
          up_diff:TCvScalar; comp:pCvConnectedComp=nil; flags:int=4; mask:pCvArr=nil); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7751
{$ELSE}
name 'cvFloodFill'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//procedure cvFloodFill(image:pCvArr; seed_point:TCvPoint; new_val:TCvScalar; lo_diff:TCvScalar;
//          up_diff:TCvScalar); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure cvFloodFill(image:pCvArr; seed_point:TCvPoint; new_val:TCvScalar; lo_diff:TCvScalar
          ); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure cvFloodFill(image:pCvArr; seed_point:TCvPoint; new_val:TCvScalar); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(****************************************************************************************\
*                                  Feature detection                                     *
\****************************************************************************************)

(** @brief Runs canny edge detector
@see cv::Canny
*)
//CVAPI(void)  cvCanny( const CvArr* image, CvArr* edges, double threshold1,
//                      double threshold2, int  aperture_size CV_DEFAULT(3) );
procedure cvCanny(const image:pCvArr; edges:pCvArr; threshold1:double; threshold2:double; aperture_size:int=3);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7651
{$ELSE}
name 'cvCanny'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates constraint image for corner detection

   Dx^2 * Dyy + Dxx * Dy^2 - 2 * Dx * Dy * Dxy.
   Applying threshold to the result gives coordinates of corners
@see cv::preCornerDetect
*)
//CVAPI(void) cvPreCornerDetect( const CvArr* image, CvArr* corners,
//                               int aperture_size CV_DEFAULT(3) );
procedure cvPreCornerDetect(const image:pCvArr; corners:pCvArr; aperture_size:int=3);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7877
{$ELSE}
name 'cvPreCornerDetect'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates eigen values and vectors of 2x2
   gradient covariation matrix at every image pixel
@see cv::cornerEigenValsAndVecs
*)
//CVAPI(void)  cvCornerEigenValsAndVecs( const CvArr* image, CvArr* eigenvv,
//                                       int block_size, int aperture_size CV_DEFAULT(3) );
procedure cvCornerEigenValsAndVecs(const image:pCvArr; eigenvv:pCvArr; block_size:int; aperture_size:int=3);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7687
{$ELSE}
name 'cvCornerEigenValsAndVecs'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates minimal eigenvalue for 2x2 gradient covariation matrix at
   every image pixel
@see cv::cornerMinEigenVal
*)
//CVAPI(void)  cvCornerMinEigenVal( const CvArr* image, CvArr* eigenval,
//                                  int block_size, int aperture_size CV_DEFAULT(3) );
procedure cvCornerMinEigenVal(const image:pCvArr; eigenval:pCvArr; block_size:int; aperture_size:int=3);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7689
{$ELSE}
name 'cvCornerMinEigenVal'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Harris corner detector:

   Calculates det(M) - k*(trace(M)^2), where M is 2x2 gradient covariation matrix for each pixel
@see cv::cornerHarris
*)
//CVAPI(void)  cvCornerHarris( const CvArr* image, CvArr* harris_response,
//                             int block_size, int aperture_size CV_DEFAULT(3),
//                             double k CV_DEFAULT(0.04) );
procedure cvCornerHarris(const image:pCvArr; harris_response:pCvArr; block_size:int; aperture_size:int=3; k:double=0.04);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7688
{$ELSE}
name 'cvCornerHarris'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Adjust corner position using some sort of gradient search
@see cv::cornerSubPix
*)
//CVAPI(void)  cvFindCornerSubPix( const CvArr* image, CvPoint2D32f* corners,
//                                 int count, CvSize win, CvSize zero_zone,
//                                 CvTermCriteria  criteria );
procedure cvFindCornerSubPix(const image:pCvArr; corners:pCvPoint2D32f; count:int; win:TCvSize; zero_zone:TCvSize; criteria:TCvTermCriteria);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7744
{$ELSE}
name 'cvFindCornerSubPix'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds a sparse set of points within the selected region
   that seem to be easy to track
@see cv::goodFeaturesToTrack
*)
//CVAPI(void)  cvGoodFeaturesToTrack( const CvArr* image, CvArr* eig_image,
//                                    CvArr* temp_image, CvPoint2D32f* corners,
//                                    int* corner_count, double  quality_level,
//                                    double  min_distance,
//                                    const CvArr* mask CV_DEFAULT(NULL),
//                                    int block_size CV_DEFAULT(3),
//                                    int use_harris CV_DEFAULT(0),
//                                    double k CV_DEFAULT(0.04) );
procedure cvGoodFeaturesToTrack(const image:pCvArr; eig_image:pCvArr; temp_image:pCvArr; corners:pCvPoint2D32f; corner_count:pint; quality_level:double; min_distance:double; const mask:pCvArr= nil; block_size:int=3; use_harris:int=0; k:double=0.04);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7802
{$ELSE}
name 'cvGoodFeaturesToTrack'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds lines on binary image using one of several methods.

   line_storage is either memory storage or 1 x _max number of lines_ CvMat, its
   number of columns is changed by the function.
   method is one of CV_HOUGH_*;
   rho, theta and threshold are used for each of those methods;
   param1 ~ line length, param2 ~ line gap - for probabilistic,
   param1 ~ srn, param2 ~ stn - for multi-scale
@see cv::HoughLines
*)
//CVAPI(CvSeq*)  cvHoughLines2( CvArr* image, void* line_storage, int method,
//                              double rho, double theta, int threshold,
//                              double param1 CV_DEFAULT(0), double param2 CV_DEFAULT(0),
//                              double min_theta CV_DEFAULT(0), double max_theta CV_DEFAULT(CV_PI));
function cvHoughLines2(image:pCvArr; line_storage:pvoid; method:int; rho:double; theta:double; threshold:int; param1:double=0; param2:double=0; min_theta:double=0; max_theta:double=CV_PI):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7815
{$ELSE}
name 'cvHoughLines2'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Finds circles in the image
@see cv::HoughCircles
*)
//CVAPI(CvSeq*) cvHoughCircles( CvArr* image, void* circle_storage,
//                              int method, double dp, double min_dist,
//                              double param1 CV_DEFAULT(100),
//                              double param2 CV_DEFAULT(100),
//                              int min_radius CV_DEFAULT(0),
//                              int max_radius CV_DEFAULT(0));
function cvHoughCircles(image:pCvArr; circle_storage:pvoid; method:int; dp:double; min_dist:double; param1:double=100; param2:double=100; min_radius:int=0; max_radius:int=0):pCvSeq;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7814
{$ELSE}
name 'cvHoughCircles'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Fits a line into set of 2d or 3d points in a robust way (M-estimator technique)
@see cv::fitLine
*)
//CVAPI(void)  cvFitLine( const CvArr* points, int dist_type, double param,
//                        double reps, double aeps, float* line );
procedure cvFitLine(const points:pCvArr; dist_type:int; param:double; reps:double; aeps:double; line:pfloat);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7749
{$ELSE}
name 'cvFitLine'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(****************************************************************************************\
*                                     Drawing                                            *
\****************************************************************************************)

(****************************************************************************************\
*       Drawing functions work with images/matrices of arbitrary type.                   *
*       For color images the channel order is BGR[A]                                     *
*       Antialiasing is supported only for 8-bit image now.                              *
*       All the functions include parameter color that means rgb value (that may be      *
*       constructed with CV_RGB macro) for color images and brightness                   *
*       for grayscale images.                                                            *
*       If a drawn figure is partially or completely outside of the image, it is clipped.*
\****************************************************************************************)

const
 CV_FILLED = -1;

 CV_AA = 16;

(** @brief Draws 4-connected, 8-connected or antialiased line segment connecting two points
@see cv::line
*)
//CVAPI(void)  cvLine( CvArr* img, CvPoint pt1, CvPoint pt2,
//                     CvScalar color, int thickness CV_DEFAULT(1),
//                     int line_type CV_DEFAULT(8), int shift CV_DEFAULT(0) );
procedure cvLine(img:pCvArr; pt1:TCvPoint; pt2:TCvPoint; color:TCvScalar; thickness:int=1; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7833
{$ELSE}
name 'cvLine'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Draws a rectangle given two opposite corners of the rectangle (pt1 & pt2)

   if thickness<0 (e.g. thickness == CV_FILLED), the filled box is drawn
@see cv::rectangle
*)
//CVAPI(void)  cvRectangle( CvArr* img, CvPoint pt1, CvPoint pt2,
//                          CvScalar color, int thickness CV_DEFAULT(1),
//                          int line_type CV_DEFAULT(8),
//                          int shift CV_DEFAULT(0));
procedure cvRectangle(img:pCvArr; pt1:TCvPoint; pt2:TCvPoint; color:TCvScalar; thickness:int=1; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7894
{$ELSE}
name 'cvRectangle'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Draws a rectangle specified by a CvRect structure
@see cv::rectangle
*)
//CVAPI(void)  cvRectangleR( CvArr* img, CvRect r,
//                           CvScalar color, int thickness CV_DEFAULT(1),
//                           int line_type CV_DEFAULT(8),
//                           int shift CV_DEFAULT(0));
procedure cvRectangleR(img:pCvArr; r:TCvRect; color:TCvScalar; thickness:int=1; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7895
{$ELSE}
name 'cvRectangleR'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Draws a circle with specified center and radius.

   Thickness works in the same way as with cvRectangle
@see cv::circle
*)
//CVAPI(void)  cvCircle( CvArr* img, CvPoint center, int radius,
//                       CvScalar color, int thickness CV_DEFAULT(1),
//                       int line_type CV_DEFAULT(8), int shift CV_DEFAULT(0));
procedure cvCircle(img:pCvArr; center:TCvPoint; radius:int; color:TCvScalar; thickness:int=1; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7659
{$ELSE}
name 'cvCircle'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Draws ellipse outline, filled ellipse, elliptic arc or filled elliptic sector

   depending on _thickness_, _start_angle_ and _end_angle_ parameters. The resultant figure
   is rotated by _angle_. All the angles are in degrees
@see cv::ellipse
*)
//CVAPI(void)  cvEllipse( CvArr* img, CvPoint center, CvSize axes,
//                        double angle, double start_angle, double end_angle,
//                        CvScalar color, int thickness CV_DEFAULT(1),
//                        int line_type CV_DEFAULT(8), int shift CV_DEFAULT(0));
procedure cvEllipse(img:pCvArr; center:TCvPoint; axes:TCvSize; angle:double; start_angle:double; end_angle:double; color:TCvScalar; thickness:int=1; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7729
{$ELSE}
name 'cvEllipse'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CV_INLINE  void  cvEllipseBox( CvArr* img, CvBox2D box, CvScalar color,
//                               int thickness CV_DEFAULT(1),
//                               int line_type CV_DEFAULT(8), int shift CV_DEFAULT(0) )
//{
//    CvSize axes = cvSize(
//        cvRound(box.size.width*0.5),
//        cvRound(box.size.height*0.5)
//    );
//
//    cvEllipse( img, cvPointFrom32f( box.center ), axes, box.angle,
//               0, 360, color, thickness, line_type, shift );
//}
procedure cvEllipseBox(img:pCvArr; box:TCvBox2D; color:TCvScalar; thickness:int=1; line_type:int=8; shift:int=0);inline;


(** @brief Fills convex or monotonous polygon.
@see cv::fillConvexPoly
*)
//CVAPI(void)  cvFillConvexPoly( CvArr* img, const CvPoint* pts, int npts, CvScalar color,
//                               int line_type CV_DEFAULT(8), int shift CV_DEFAULT(0));
procedure cvFillConvexPoly(img:pCvArr; const pts:pCvPoint; npts:int; color:TCvScalar; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7740
{$ELSE}
name 'cvFillConvexPoly'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Fills an area bounded by one or more arbitrary polygons
@see cv::fillPoly
*)
//CVAPI(void)  cvFillPoly( CvArr* img, CvPoint** pts, const int* npts,
//                         int contours, CvScalar color,
//                         int line_type CV_DEFAULT(8), int shift CV_DEFAULT(0) );
procedure cvFillPoly(img:pCvArr; pts:pCvPoint; const npts:pint; contours:int; color:TCvScalar; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7741
{$ELSE}
name 'cvFillPoly'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Draws one or more polygonal curves
@see cv::polylines
*)
//CVAPI(void)  cvPolyLine( CvArr* img, CvPoint** pts, const int* npts, int contours,
//                         int is_closed, CvScalar color, int thickness CV_DEFAULT(1),
//                         int line_type CV_DEFAULT(8), int shift CV_DEFAULT(0) );
procedure cvPolyLine(img:pCvArr; pts:pCvPoint; const npts:pint; contours:int; is_closed:int; color:TCvScalar; thickness:int=1; line_type:int=8; shift:int=0);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7875
{$ELSE}
name 'cvPolyLine'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{TODO: #define cvDrawRect cvRectangle}
//#define cvDrawRect cvRectangle
//#define cvDrawLine cvLine
//#define cvDrawCircle cvCircle
//#define cvDrawEllipse cvEllipse
//#define cvDrawPolyLine cvPolyLine

(** @brief Clips the line segment connecting *pt1 and *pt2
   by the rectangular window

   (0<=x<img_size.width, 0<=y<img_size.height).
@see cv::clipLine
*)
//CVAPI(int) cvClipLine( CvSize img_size, CvPoint* pt1, CvPoint* pt2 );
function cvClipLine(img_size:TCvSize; pt1:pCvPoint; pt2:pCvPoint):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7666
{$ELSE}
name 'cvClipLine'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Initializes line iterator.

Initially, line_iterator->ptr will point to pt1 (or pt2, see left_to_right description) location in
the image. Returns the number of pixels on the line between the ending points.
@see cv::LineIterator
*)
//CVAPI(int)  cvInitLineIterator( const CvArr* image, CvPoint pt1, CvPoint pt2,
//                                CvLineIterator* line_iterator,
//                                int connectivity CV_DEFAULT(8),
//                                int left_to_right CV_DEFAULT(0));
function cvInitLineIterator(const image:pCvArr; pt1:TCvPoint; pt2:TCvPoint; line_iterator:pCvLineIterator;
         connectivity:int=8; left_to_right:int=0):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7820
{$ELSE}
name 'cvInitLineIterator'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{TODO:CV_NEXT_LINE_POINT}
//#define CV_NEXT_LINE_POINT( line_iterator )                     \
//{                                                               \
//    int _line_iterator_mask = (line_iterator).err < 0 ? -1 : 0; \
//    (line_iterator).err += (line_iterator).minus_delta +        \
//        ((line_iterator).plus_delta & _line_iterator_mask);     \
//    (line_iterator).ptr += (line_iterator).minus_step +         \
//        ((line_iterator).plus_step & _line_iterator_mask);      \
//}

const
 CV_FONT_HERSHEY_SIMPLEX         =0;
 CV_FONT_HERSHEY_PLAIN           =1;
 CV_FONT_HERSHEY_DUPLEX          =2;
 CV_FONT_HERSHEY_COMPLEX         =3;
 CV_FONT_HERSHEY_TRIPLEX         =4;
 CV_FONT_HERSHEY_COMPLEX_SMALL   =5;
 CV_FONT_HERSHEY_SCRIPT_SIMPLEX  =6;
 CV_FONT_HERSHEY_SCRIPT_COMPLEX  =7;

 CV_FONT_ITALIC                 =16;

 CV_FONT_VECTOR0    =CV_FONT_HERSHEY_SIMPLEX;


(** Font structure *)
type
pCvFont = ^_CvFont;
_CvFont = record
   nameFont:pcvchar;   //Qt:nameFont
    color:TCvScalar;       //Qt:ColorFont -> cvScalar(blue_component, green_component, red_component[, alpha_component])
    font_face:int;    //Qt: bool italic         (** =CV_FONT_* *)
    ascii:pint;      //!< font data and metrics
    greek:pint;
    cyrillic:pint;
           hscale, vscale:float;
    shear:float;      //!< slope coefficient: 0 - normal, >0 - italic
    thickness:int;    //!< Qt: weight               (** letters thickness *)
    dx:float;       //!< horizontal interval between letters
    line_type:int;    //!< Qt: PointSize
end;
TCvFont = _CvFont;

(** @brief Initializes font structure (OpenCV 1.x API).

The function initializes the font structure that can be passed to text rendering functions.

@param font Pointer to the font structure initialized by the function
@param font_face Font name identifier. See cv::HersheyFonts and corresponding old CV_* identifiers.
@param hscale Horizontal scale. If equal to 1.0f , the characters have the original width
depending on the font type. If equal to 0.5f , the characters are of half the original width.
@param vscale Vertical scale. If equal to 1.0f , the characters have the original height depending
on the font type. If equal to 0.5f , the characters are of half the original height.
@param shear Approximate tangent of the character slope relative to the vertical line. A zero
value means a non-italic font, 1.0f means about a 45 degree slope, etc.
@param thickness Thickness of the text strokes
@param line_type Type of the strokes, see line description

@sa cvPutText
 *)
//CVAPI(void)  cvInitFont( CvFont* font, int font_face,
//                         double hscale, double vscale,
//                         double shear CV_DEFAULT(0),
//                         int thickness CV_DEFAULT(1),
//                         int line_type CV_DEFAULT(8));
procedure cvInitFont(Var font:TCvFont; font_face:int; hscale:double; vscale:double; shear:double=0; thickness:int=1; line_type:int=8);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7818
{$ELSE}
name 'cvInitFont'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//CV_INLINE CvFont cvFont( double scale, int thickness CV_DEFAULT(1) )
//{
//    CvFont font;
//    cvInitFont( &font, CV_FONT_HERSHEY_PLAIN, scale, scale, 0, thickness, CV_AA );
//    return font;
//}
function cvFont(scale:double; thickness:int=1):TCvFont;inline;


(** @brief Renders text stroke with specified font and color at specified location.
   CvFont should be initialized with cvInitFont
@see cvInitFont, cvGetTextSize, cvFont, cv::putText
*)
//CVAPI(void)  cvPutText( CvArr* img, const char* text, CvPoint org,
//                        const CvFont* font, CvScalar color );
procedure cvPutText(img:pCvArr; const text:pcvchar; org:TCvPoint; const font:pCvFont; color:TCvScalar);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7884
{$ELSE}
name 'cvPutText'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Calculates bounding box of text stroke (useful for alignment)
@see cv::getTextSize
*)
//CVAPI(void)  cvGetTextSize( const char* text_string, const CvFont* font,
//                            CvSize* text_size, int* baseline );
procedure cvGetTextSize(const text_string:pcvchar; const font:pCvFont; text_size:pCvSize; baseline:pint);
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7794
{$ELSE}
name 'cvGetTextSize'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Unpacks color value

if arrtype is CV_8UC?, _color_ is treated as packed color value, otherwise the first channels
(depending on arrtype) of destination scalar are set to the same value = _color_
*)
//CVAPI(CvScalar)  cvColorToScalar( double packed_color, int arrtype );
function cvColorToScalar(packed_color:double; arrtype:int):TCvScalar;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7675
{$ELSE}
name 'cvColorToScalar'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(** @brief Returns the polygon points which make up the given ellipse.

The ellipse is define by the box of size 'axes' rotated 'angle' around the 'center'. A partial
sweep of the ellipse arc can be done by specifying arc_start and arc_end to be something other than
0 and 360, respectively. The input array 'pts' must be large enough to hold the result. The total
number of points stored into 'pts' is returned by this function.
@see cv::ellipse2Poly
*)
//CVAPI(int) cvEllipse2Poly( CvPoint center, CvSize axes,
//                 int angle, int arc_start, int arc_end, CvPoint * pts, int delta );
function cvEllipse2Poly(center:TCvPoint; axes:TCvSize; angle:int; arc_start:int; arc_end:int; pts:pCvPoint; delta:int):int;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7730
{$ELSE}
name 'cvEllipse2Poly'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};


(** @brief Draws contour outlines or filled interiors on the image
@see cv::drawContours
*)
//CVAPI(void)  cvDrawContours( CvArr *img, CvSeq* contour,
//                             CvScalar external_color, CvScalar hole_color,
//                             int max_level, int thickness CV_DEFAULT(1),
//                             int line_type CV_DEFAULT(8),
//                             CvPoint offset CV_DEFAULT(cvPoint(0,0)));
procedure cvDrawContours(img:pCvArr; contour:pCvSeq; external_color:TCvScalar;
          hole_color:TCvScalar; max_level:int; thickness:int; line_type:int; offset:TCvPoint); overload;
external opencv_world_dll
{$IFDEF CALL_BY_FNC_NUM}
index 7727
{$ELSE}
name 'cvDrawContours'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvDrawContours(img:pCvArr; contour:pCvSeq; external_color:TCvScalar;
          hole_color:TCvScalar; max_level:int; thickness:int=1; line_type:int=8); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

(** @} *)

{$endif}
