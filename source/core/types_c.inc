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

{$IFDEF INTERFACE}

{$IF not defined(OPENCV_CORE_TYPES_H)}
{$DEFINE OPENCV_CORE_TYPES_H}
{$IF not defined(OPENCV_CORE_HPP)}{$I 'core.inc'}{$IFEND}
{$IF not defined(OPENCV_CORE_CVDEF_H)}{$I 'core/cvdef.inc'}{$IFEND}
(* * @addtogroup core_c
  @{
*)

(* * @brief This is the "metatype" used *only* as a function parameter.

  It denotes that the function accepts arrays of multiple types, such as IplImage*, CvMat* or even
  CvSeq* sometimes. The particular array type is determined at runtime by analyzing the first 4
  bytes of the header. In C++ interface the role of CvArr is played by InputArray and OutputArray.
*)

const
  (* * @see cv::Error::Code *)
  // enum {
  CV_StsOk = 0;    (* *< everything is ok *)
  CV_StsBackTrace    = -1;   (* *< pseudo error for back trace *)
  CV_StsError        = -2;   (* *< unknown /unspecified error *)
  CV_StsInternal     = -3;   (* *< internal error (bad state) *)
  CV_StsNoMem        = -4;   (* *< insufficient memory *)
  CV_StsBadArg       = -5;   (* *< function arg/param is bad *)
  CV_StsBadFunc      = -6;   (* *< unsupported function *)
  CV_StsNoConv       = -7;   (* *< iter. didn't converge *)
  CV_StsAutoTrace    = -8;   (* *< tracing *)
  CV_HeaderIsNull    = -9;   (* *< image header is NULL *)
  CV_BadImageSize    = -10;  (* *< image size is invalid *)
  CV_BadOffset       = -11;  (* *< offset is invalid *)
  CV_BadDataPtr      = -12;  (* *)
  CV_BadStep         = -13;  (* *< image step is wrong, this may happen for a non-continuous matrix *)
  CV_BadModelOrChSeq = -14;  (* *)
  CV_BadNumChannels  = -15;  (* *< bad number of channels, for example, some functions accept only single channel matrices *)
  CV_BadNumChannel1U = -16;  (* *)
  CV_BadDepth        = -17;  (* *< input image depth is not supported by the function *)
  CV_BadAlphaChannel = -18;  (* *)
  CV_BadOrder        = -19;  (* *< number of dimensions is out of range *)
  CV_BadOrigin       = -20;  (* *< incorrect input origin *)
  CV_BadAlign        = -21;  (* *< incorrect input align *)
  CV_BadCallBack     = -22;  (* *)
  CV_BadTileSize     = -23;  (* *)
  CV_BadCOI= -24;  (* *< input COI is not supported *)
  CV_BadROISize      = -25;  (* *< incorrect input roi *)
  CV_MaskIsTiled     = -26;  (* *)
  CV_StsNullPtr      = -27;  (* *< null pointer *)
  CV_StsVecLengthErr = -28;  (* *< incorrect vector length *)
  CV_StsFilterStructContentErr = -29;  (* *< incorrect filter structure content *)
  CV_StsKernelStructContentErr = -30;  (* *< incorrect transform kernel content *)
  CV_StsFilterOffsetErr        = -31;  (* *< incorrect filter offset value *)
  CV_StsBadSize      = -201; (* *< the input/output structure size is incorrect *)
  CV_StsDivByZero    = -202; (* *< division by zero *)
  CV_StsInplaceNotSupported    = -203; (* *< in-place operation is not supported *)
  CV_StsObjectNotFound         = -204; (* *< request can't be completed *)
  CV_StsUnmatchedFormats       = -205; (* *< formats of input/output arrays differ *)
  CV_StsBadFlag      = -206; (* *< flag is wrong or not supported *)
  CV_StsBadPoint     = -207; (* *< bad CvPoint *)
  CV_StsBadMask      = -208; (* *< bad format of mask (neither 8uC1 nor 8sC1) *)
  CV_StsUnmatchedSizes         = -209; (* *< sizes of input/output structures do not match *)
  CV_StsUnsupportedFormat      = -210; (* *< the data format/type is not supported by the function *)
  CV_StsOutOfRange   = -211; (* *< some of parameters are out of range *)
  CV_StsParseError   = -212; (* *< invalid syntax/structure of the parsed file *)
  CV_StsNotImplemented         = -213; (* *< the requested function/feature is not implemented *)
  CV_StsBadMemBlock  = -214; (* *< an allocated block has been corrupted *)
  CV_StsAssert       = -215; (* *< assertion failed *)
  CV_GpuNotSupported = -216; (* *< no CUDA support *)
  CV_GpuApiCallError = -217; (* *< GPU API call error *)
  CV_OpenGlNotSupported        = -218; (* *< no OpenGL support *)
  CV_OpenGlApiCallError        = -219; (* *< OpenGL API call error *)
  CV_OpenCLApiCallError        = -220; (* *< OpenCL API call error *)
  CV_OpenCLDoubleNotSupported  = -221;
  CV_OpenCLInitError = -222; (* *< OpenCL initialization error *)
  CV_OpenCLNoAMDBlasFft        = -223;

  (* ************** Random number generation ****************** *)
  CV_RNG_COEFF = 4164903690;

  (* ***************************************************************************************\
    *    Image type (IplImage)   *
    \*************************************************************************************** *)

  (*
    * The following definitions (until {$endif})
    * is an extract from IPL headers.
    * Copyright (c) 1995 Intel Corporation.
  *)
const
  IPL_DEPTH_SIGN = $80000000;

  IPL_DEPTH_1U  = 1;
  IPL_DEPTH_8U  = 8;
  IPL_DEPTH_16U = 16;
  IPL_DEPTH_32F = 32;

  IPL_DEPTH_8S  = (IPL_DEPTH_SIGN or 8);
  IPL_DEPTH_16S = (IPL_DEPTH_SIGN or 16);
  IPL_DEPTH_32S = (IPL_DEPTH_SIGN or 32);

  IPL_DATA_ORDER_PIXEL = 0;
  IPL_DATA_ORDER_PLANE = 1;

  IPL_ORIGIN_TL = 0;
  IPL_ORIGIN_BL = 1;

  IPL_ALIGN_4BYTES  = 4;
  IPL_ALIGN_8BYTES  = 8;
  IPL_ALIGN_16BYTES = 16;
  IPL_ALIGN_32BYTES = 32;

  IPL_ALIGN_DWORD = IPL_ALIGN_4BYTES;
  IPL_ALIGN_QWORD = IPL_ALIGN_8BYTES;

  IPL_BORDER_CONSTANT  = 0;
  IPL_BORDER_REPLICATE = 1;
  IPL_BORDER_REFLECT   = 2;
  IPL_BORDER_WRAP      = 3;

const
  IPL_IMAGE_HEADER = 1;
  IPL_IMAGE_DATA   = 2;
  IPL_IMAGE_ROI    = 4;

const
  (* * extra border mode *)
  IPL_BORDER_REFLECT_101 = 4;
  IPL_BORDER_TRANSPARENT = 5;

  // IPL_IMAGE_MAGIC_VAL  = sizeof(TIplImage); //moved
  CV_TYPE_NAME_IMAGE = 'opencv-image';

const
  (* * for storing double-precision
    floating point data in IplImage's *)
  IPL_DEPTH_64F = 64;

  (* ***************************************************************************************\
    *    Matrix type (CvMat)     *
    \*************************************************************************************** *)
const
  CV_AUTO_STEP = $7FFFFFFF;
  // CV_WHOLE_ARR:_cvSlice  = (start_index: 0;end_index: $3fffffff );

  CV_MAGIC_MASK    = $FFFF0000;
  CV_MAT_MAGIC_VAL = $42420000;
  CV_TYPE_NAME_MAT = 'opencv-matrix';

  (* ***************************************************************************************\
    *   Multi-dimensional dense array (CvMatND)      *
    \*************************************************************************************** *)

const
  CV_MATND_MAGIC_VAL = $42430000;
  CV_TYPE_NAME_MATND = 'opencv-nd-matrix';

  CV_MAX_DIM = 32;

  (* ***************************************************************************************\
    *  Multi-dimensional sparse array (CvSparseMat)  *
    \*************************************************************************************** *)

const
  CV_SPARSE_MAT_MAGIC_VAL = $42440000;
  CV_TYPE_NAME_SPARSE_MAT = 'opencv-sparse-matrix';

  (* ***************************************************************************************\
    * Histogram*
    \*************************************************************************************** *)
const
  CV_HIST_MAGIC_VAL    = $42450000;
  CV_HIST_UNIFORM_FLAG = (1 shl 10);

  (* * indicates whether bin ranges are set already or not *)
  CV_HIST_RANGES_FLAG = (1 shl 11);

  CV_HIST_ARRAY  = 0;
  CV_HIST_SPARSE = 1;
  CV_HIST_TREE   = CV_HIST_SPARSE;

  (* * should be used as a parameter only,
    it turns to CV_HIST_UNIFORM_FLAG of hist->type *)
  CV_HIST_UNIFORM = 1;

  (* ********************************** CvTermCriteria ************************************ *)
const
  CV_TERMCRIT_ITER   = 1;
  CV_TERMCRIT_NUMBER = CV_TERMCRIT_ITER;
  CV_TERMCRIT_EPS    = 2;

  (* ************************************ CvSlice ***************************************** *)
const
  CV_WHOLE_SEQ_END_INDEX = $3FFFFFFF;
  // CV_WHOLE_SEQ  =cvSlice(0, CV_WHOLE_SEQ_END_INDEX);//moved

  (* ***************************************************************************************\
    *     Dynamic Data structures*
    \*************************************************************************************** *)

  (* ******************************* Memory storage *************************************** *)
  CV_STORAGE_MAGIC_VAL = $42890000;

  (* ********************************** Sequence ****************************************** *)

  CV_TYPE_NAME_SEQ      = 'opencv-sequence';
  CV_TYPE_NAME_SEQ_TREE = 'opencv-sequence-tree';
  CV_SET_ELEM_IDX_MASK  = ((1 shl 26) - 1);
  CV_SET_ELEM_FREE_FLAG = (1 shl (sizeof(int) * 8 - 1));

  (* ************************************ Graph ******************************************* *)
  CV_TYPE_NAME_GRAPH = 'opencv-graph';

  (* ***************************************************************************************\
    *      Sequence types*
    \*************************************************************************************** *)
  CV_SEQ_MAGIC_VAL = $42990000;

  CV_SEQ_ELTYPE_BITS = 12;
  CV_SEQ_ELTYPE_MASK = ((1 shl CV_SEQ_ELTYPE_BITS) - 1);

  CV_SEQ_ELTYPE_GENERIC        = 0;
  CV_SEQ_ELTYPE_GRAPH_EDGE     = 0; (* *< &next_o, &next_d, &vtx_o, &vtx_d *)
  CV_SEQ_ELTYPE_GRAPH_VERTEX   = 0; (* *< first_edge, &(x,y) *)
  CV_SEQ_ELTYPE_TRIAN_ATR      = 0; (* *< vertex of the binary tree *)
  CV_SEQ_ELTYPE_CONNECTED_COMP = 0; (* *< connected component *)

  CV_SEQ_KIND_BITS = 2;
  CV_SEQ_KIND_MASK = (((1 shl CV_SEQ_KIND_BITS) - 1) shl CV_SEQ_ELTYPE_BITS);

  (* * types of sequences *)
  CV_SEQ_KIND_GENERIC  = (0 shl CV_SEQ_ELTYPE_BITS);
  CV_SEQ_KIND_CURVE    = (1 shl CV_SEQ_ELTYPE_BITS);
  CV_SEQ_KIND_BIN_TREE = (2 shl CV_SEQ_ELTYPE_BITS);

  (* * types of sparse sequences (sets) *)
  CV_SEQ_KIND_GRAPH    = (1 shl CV_SEQ_ELTYPE_BITS);
  CV_SEQ_KIND_SUBDIV2D = (2 shl CV_SEQ_ELTYPE_BITS);

  CV_SEQ_FLAG_SHIFT = (CV_SEQ_KIND_BITS + CV_SEQ_ELTYPE_BITS);

  (* * flags for curves *)
  CV_SEQ_FLAG_CLOSED = (1 shl CV_SEQ_FLAG_SHIFT);
  CV_SEQ_FLAG_SIMPLE = (0 shl CV_SEQ_FLAG_SHIFT);
  CV_SEQ_FLAG_CONVEX = (0 shl CV_SEQ_FLAG_SHIFT);
  CV_SEQ_FLAG_HOLE   = (2 shl CV_SEQ_FLAG_SHIFT);

  (* * flags for graphs *)
  CV_GRAPH_FLAG_ORIENTED = (1 shl CV_SEQ_FLAG_SHIFT);

  CV_GRAPH= CV_SEQ_KIND_GRAPH;
  CV_ORIENTED_GRAPH = (CV_SEQ_KIND_GRAPH or CV_GRAPH_FLAG_ORIENTED);

  (* * binary tree for the contour *)
  CV_SEQ_POLYGON_TREE = (CV_SEQ_KIND_BIN_TREE or CV_SEQ_ELTYPE_TRIAN_ATR);

  (* * sequence of the connected components *)
  CV_SEQ_CONNECTED_COMP = (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_CONNECTED_COMP);

  (* ***************************************************************************************\
    *   Data structures for persistence (a.k.a serialization) functionality        *
    \*************************************************************************************** *)

  (* * Storage flags: *)
  CV_STORAGE_READ         = 0;
  CV_STORAGE_WRITE        = 1;
  CV_STORAGE_WRITE_TEXT   = CV_STORAGE_WRITE;
  CV_STORAGE_WRITE_BINARY = CV_STORAGE_WRITE;
  CV_STORAGE_APPEND       = 2;
  CV_STORAGE_MEMORY       = 4;
  CV_STORAGE_FORMAT_MASK  = (7 shl 3);
  CV_STORAGE_FORMAT_AUTO  = 0;
  CV_STORAGE_FORMAT_XML   = 8;
  CV_STORAGE_FORMAT_YAML  = 16;
  CV_STORAGE_FORMAT_JSON  = 24;
  CV_STORAGE_BASE64       = 64;
  CV_STORAGE_WRITE_BASE64 = (CV_STORAGE_BASE64 or CV_STORAGE_WRITE);

  CV_NODE_NONE      = 0;
  CV_NODE_INT       = 1;
  CV_NODE_INTEGER   = CV_NODE_INT;
  CV_NODE_REAL      = 2;
  CV_NODE_FLOAT     = CV_NODE_REAL;
  CV_NODE_STR       = 3;
  CV_NODE_STRING    = CV_NODE_STR;
  CV_NODE_REF       = 4; (* *< not used *)
  CV_NODE_SEQ       = 5;
  CV_NODE_MAP       = 6;
  CV_NODE_TYPE_MASK = 7;

  (* * file node flags *)
  CV_NODE_FLOW  = 8; (* *<Used only for writing structures in YAML format. *)
  CV_NODE_USER  = 16;
  CV_NODE_EMPTY = 32;
  CV_NODE_NAMED = 64;

  CV_NODE_SEQ_SIMPLE = 256;

type
  pCvArr = ^CvArr;

  CvArr = record
  end;

  CVStatus = int;

  (* ************** Random number generation ****************** *)

  pCvRNG = ^TCvRNG;
  TCvRNG = uint64;

  (* ***************************************************************************************\
    *    Image type (IplImage)   *
    \*************************************************************************************** *)

  (* * The IplImage is taken from the Intel Image Processing Library, in which the format is native. OpenCV
    only supports a subset of possible IplImage formats, as outlined in the parameter list above.

    In addition to the above restrictions, OpenCV handles ROIs differently. OpenCV functions require
    that the image size or ROI size of all source and destination images match exactly. On the other
    hand, the Intel Image Processing Library processes the area of intersection between the source and
    destination images (or ROIs), allowing them to vary independently.
  *)
  pIplROI      = ^_IplROI;
  pIplTileInfo = ^_IplTileInfo;
  pIplImage    = ^_IplImage;

  _IplImage = record
    nSize: int;        (* *< sizeof(IplImage) *)
    ID: int; (* *< version (=0) *)
    nChannels: int;    (* *< Most of OpenCV functions support 1,2,3 or 4 channels *)
    alphaChannel: int; (* *< Ignored by OpenCV *)
    depth: int; (* *< Pixel depth in bits: IPL_DEPTH_8U, IPL_DEPTH_8S, IPL_DEPTH_16S,
      IPL_DEPTH_32S, IPL_DEPTH_32F and IPL_DEPTH_64F are supported. *)
    colorModel: array [0 .. 3] of cvchar; (* *< Ignored by OpenCV *)
    channelSeq: array [0 .. 3] of cvchar; (* *< ditto *)
    dataOrder: int; (* *< 0 - interleaved color channels, 1 - separate color channels.
      cvCreateImage can only create interleaved images *)
    origin: int; (* *< 0 - top-left origin,
      1 - bottom-left origin (Windows bitmaps style). *)
    align: int; (* *< Alignment of image rows (4 or 8).
      OpenCV ignores it and uses widthStep instead. *)
    width: int;   (* *< Image width in pixels. *)
    height: int;  (* *< Image height in pixels. *)
    roi: pIplROI; (* *< Image ROI. If NULL, the whole image is selected. *)
    maskROI: pIplImage;     (* *< Must be NULL. *)
    imageId: pointer;       (* *< " " *)
    tileInfo: pIplTileInfo; (* *< " " *)
    imageSize: int; (* *< Image data size in bytes
      (==image->height*image->widthStep
      in case of interleaved data) *)
    imageData: pcvchar;       (* *< Pointer to aligned image data. *)
    widthStep: int; (* *< Size of aligned image row in bytes. *)
    BorderMode: array [0 .. 3] of int;  (* *< Ignored by OpenCV. *)
    BorderConst: array [0 .. 3] of int; (* *< Ditto. *)
    imageDataOrigin: pcvchar; (* *< Pointer to very origin of image data
      (not necessarily aligned) -
      needed for correct deallocation *)
    // _IplImage();
    class operator Initialize(out Dest: _IplImage);
    // _IplImage(const cv::Mat& m) { *this = cvIplImage(m); }
    class operator Explicit(const m: TMat): _IplImage;
  end;

  TIplImage = _IplImage;

  _IplROI = record
    coi: int; (* *< 0 - no COI (all channels are selected), 1 - 0th channel is selected ... *)
    xOffset: int;
    yOffset: int;
    width: int;
    height: int;
  end;

  TIplROI = _IplROI;

  pIplConvKernel = ^_IplConvKernel;

  _IplConvKernel = record
    nCols: int;
    nRows: int;
    anchorX: int;
    anchorY: int;
    values: pint;
    nShiftR: int;
  end;

  TIplConvKernel = _IplConvKernel;

  _IplConvKernelFP = record
    nCols: int;
    nRows: int;
    anchorX: int;
    anchorY: int;
    values: pfloat;
  end;

  TIplConvKernelFP = _IplConvKernelFP;

  _IplTileInfo = record
  end;

  TIplTileInfo = _IplTileInfo;

  (* ***************************************************************************************\
    *    Matrix type (CvMat)     *
    \*************************************************************************************** *)

  (* * Matrix elements are stored row by row. Element (i, j) (i - 0-based row index, j - 0-based column
    index) of a matrix can be retrieved or modified using CV_MAT_ELEM macro:

    uchar pixval = CV_MAT_ELEM(grayimg, uchar, i, j)
    CV_MAT_ELEM(cameraMatrix, float, 0, 2) = image.width*0.5f;

    To access multiple-channel matrices, you can use
    CV_MAT_ELEM(matrix, type, i, j\*nchannels + channel_idx).

    @deprecated CvMat is now obsolete; consider using Mat instead.
  *)
  // union
  // {
  // uchar* ptr;
  // short* s;
  // int* i;
  // float* fl;
  // double* db;
  // } data;
  union_data = record
    case byte of
      0:
        (ptr: puchar);
      1:
        (s: pshort);
      2:
        (i: pint);
      3:
        (fl: pfloat);
      4:
        (db: pdouble);
  end;

  TPointerHelper = record helper for pointer
  private
    function get_puchar: puchar; inline;
    procedure set_puchar(const Value: puchar); inline;
    function get_pshort: pshort; inline;
    procedure set_pshort(const Value: pshort); inline;
    function get_pInt: pint; inline;
    procedure set_pInt(const Value: pint); inline;
    function get_pfloat: pfloat; inline;
    procedure set_pfloat(const Value: pfloat); inline;
    function get_pdouble: pdouble; inline;
    procedure set_pdouble(const Value: pdouble); inline;
  public
    property ptr: puchar read get_puchar write set_puchar;
    property s: pshort read get_pshort write set_pshort;
    property i: pint read get_pInt write set_pInt;
    property fl: pfloat read get_pfloat write set_pfloat;
    property db: pdouble read get_pdouble write set_pdouble;
  end;

  pCvMat = ^_CvMat;

  _CvMat = record
    &type: int;
    step: int;

    (* for internal use only *)
    refcount: pint;
    hdr_refcount: int;

    data: union_data;
    rows: int;
    cols: int;
    property height: int read rows write rows;
    property width: int read cols write cols;
    // CvMat() {}
    // CvMat(const cv::Mat& m) { *this = cvMat(m); }
    class operator Explicit(const m: TMat): _CvMat; // {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TCvMat = _CvMat;

  (* ***************************************************************************************\
    *   Multi-dimensional dense array (CvMatND)      *
    \*************************************************************************************** *)

  (* *
    @deprecated consider using cv::Mat instead
  *)
  pCvMatND = ^_CvMatND;

  _CvMatND_dim = record
    size, step: int;
  end;

  _CvMatND = record
    &type: int;
    dims: int;
    refcount: pint;
    hdr_refcount: int;
    data: union_data;
    dim: array [0 .. CV_MAX_DIM - 1] of _CvMatND_dim;
    // CvMatND() {}
    // CvMatND(const cv::Mat& m) { *this = cvMatND(m); }
    class operator Explicit(const m: TMat): _CvMatND;
  end;

  TCvMatND = _CvMatND;

  {
    type
    pCV_TREE_NODE_FIELDS < T >= record
    flags: int;  (* *< Miscellaneous flags. *)
    header_size: int; (* *< Size of sequence header. *)
    h_prev: T; (* *< Previous sequence. *)
    h_next:T; (* *< Next sequence. *)
    v_prev:T; (* *< 2nd previous sequence. *)
    v_next:T (* *< 2nd next sequence. *)
    end;

    pCV_SET_ELEM_FIELDS < T >= record
    flags: int;
    next_free:T;
    end;
  }
  (* ***************************************************************************************\
    *  Multi-dimensional sparse array (CvSparseMat)  *
    \*************************************************************************************** *)
Type
  pCvSet = ^_CvSet;

  pCvSparseMat = ^_CvSparseMat;

  _CvSparseMat = record
    &type: int;
    dims: int;
    refcount: pint;
    hdr_refcount: int;

    heap: pCvSet;
    hashtable: pointer; // void** hashtable;
    hashsize: int;
    valoffset: int;
    idxoffset: int;
    size: array [0 .. CV_MAX_DIM - 1] of int;
    // CV_EXPORTS void copyToSparseMat(cv::SparseMat& m) const;
    { TODO:TSparseMat }
    // procedure copyToSparseMat(Var m:TSparseMat); {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TCvSparseMat = _CvSparseMat;

  (* *************** iteration through a sparse array **************** *)

  pCvSparseNode = ^_CvSparseNode;

  _CvSparseNode = record
    hashval: unsigned;
    next: pCvSparseNode;
  end;

  TCvSparseNode = _CvSparseNode;

  pCvSparseMatIterator = ^_CvSparseMatIterator;

  _CvSparseMatIterator = record
    mat: pCvSparseMat;
    node: pCvSparseNode;
    curidx: int;
  end;

  TCvSparseMatIterator = _CvSparseMatIterator;

  { TODO: CV_NODE_VAL }
  // #define CV_NODE_VAL(mat,node)   ((void*)((uchar*)(node) + (mat)->valoffset))
  // #define CV_NODE_IDX(mat,node)   ((int*)((uchar*)(node) + (mat)->idxoffset))

  (* ***************************************************************************************\
    * Histogram*
    \*************************************************************************************** *)

  _CvHistType  = int;
  TCvHistType  = _CvHistType;
  pCvHistogram = ^_CvHistogram;

  _CvHistogram = record
    &type: int;
    bins: pCvArr;
    thresh: array [0 .. CV_MAX_DIM - 1, 0 .. 2 - 1] of float; (* *< For uniform histograms. *)
    thresh2: pfloat; // float** thresh2;      (**< For non-uniform histograms.        *)
    mat: TCvMatND; (* *< Embedded matrix header for array histograms. *)
  end;

  TCvHistogram = _CvHistogram;

  (* ***************************************************************************************\
    *  Other supplementary data type definitions     *
    \*************************************************************************************** *)

  (* ************************************** CvRect **************************************** *)
  (* * @sa Rect_ *)
  pCvRect = ^_CvRect;

  _CvRect = record
    x: int;
    y: int;
    width: int;
    height: int;
    { TODO: cvRect }
    // CvRect(int _x = 0, int _y = 0, int w = 0, int h = 0): x(_x), y(_y), width(w), height(h) {}
    // template<typename _Tp>
    // CvRect(const cv::Rect_<_Tp>& r): x(cv::saturate_cast<int>(r.x)), y(cv::saturate_cast<int>(r.y)), width(cv::saturate_cast<int>(r.width)), height(cv::saturate_cast<int>(r.height)) {}
    // template<typename _Tp>
    // operator cv::Rect_<_Tp>() const { return cv::Rect_<_Tp>((_Tp)x, (_Tp)y, (_Tp)width, (_Tp)height); }
  end;

  TCvRect = _CvRect;

  (* ********************************** CvTermCriteria ************************************ *)

  (* * @sa TermCriteria
  *)
  pCvTermCriteria = ^_CvTermCriteria;

  _CvTermCriteria = record
    &type: int; (* *< may be combination of
      CV_TERMCRIT_ITER
      CV_TERMCRIT_EPS *)
    max_iter: int;
    epsilon: double;

    { TODO: CvTermCriteria }
    // CvTermCriteria(int _type = 0, int _iter = 0, double _eps = 0) : type(_type), max_iter(_iter), epsilon(_eps)  {}
    class function CvTermCriteria(_type: int = 0; _iter: int = 0; _eps: double = 0): _CvTermCriteria; static;
    // CvTermCriteria(const cv::TermCriteria& t) : type(t.type), max_iter(t.maxCount), epsilon(t.epsilon)  {}
    // operator cv::TermCriteria() const { return cv::TermCriteria(type, max_iter, epsilon); }
  end;

  TCvTermCriteria = _CvTermCriteria;

  (* ****************************** CvPoint and variants ********************************** *)

  pCvPoint = ^_CvPoint;

  _CvPoint = record
    x: int;
    y: int;

    { TODO:CvPoint }
    // CvPoint(int _x = 0, int _y = 0): x(_x), y(_y) {}
    class function CvPoint(const _x: int = 0; const _y: int = 0): _CvPoint; static;
    // template<typename _Tp>
    // CvPoint(const cv::Point_<_Tp>& pt): x((int)pt.x), y((int)pt.y) {}
    // template<typename _Tp>
    // operator cv::Point_<_Tp>() const { return cv::Point_<_Tp>(cv::saturate_cast<_Tp>(x), cv::saturate_cast<_Tp>(y)); }
  end;

  TCvPoint = _CvPoint;

  pCvPoint2D32f = ^_CvPoint2D32f;

  _CvPoint2D32f = record
    x: float;
    y: float;
    { TODO: CvPoint2D32f }
    // CvPoint2D32f(float _x = 0, float _y = 0): x(_x), y(_y) {}
    // template<typename _Tp>
    // CvPoint2D32f(const cv::Point_<_Tp>& pt): x((float)pt.x), y((float)pt.y) {}
    // template<typename _Tp>
    // operator cv::Point_<_Tp>() const { return cv::Point_<_Tp>(cv::saturate_cast<_Tp>(x), cv::saturate_cast<_Tp>(y)); }
  end;

  TCvPoint2D32f = _CvPoint2D32f;

  _CvPoint3D32f = record
    x: float;
    y: float;
    z: float;
    { TODO:CvPoint3D32f }
    // CvPoint3D32f(float _x = 0, float _y = 0, float _z = 0): x(_x), y(_y), z(_z) {}
    // template<typename _Tp>
    // CvPoint3D32f(const cv::Point3_<_Tp>& pt): x((float)pt.x), y((float)pt.y), z((float)pt.z) {}
    // template<typename _Tp>
    // operator cv::Point3_<_Tp>() const { return cv::Point3_<_Tp>(cv::saturate_cast<_Tp>(x), cv::saturate_cast<_Tp>(y), cv::saturate_cast<_Tp>(z)); }
  end;

  TCvPoint3D32f = _CvPoint3D32f;

  _CvPoint2D64f = record
    x: double;
    y: double;
  end;

  TCvPoint2D64f = _CvPoint2D64f;

  _CvPoint3D64f = record
    x: double;
    y: double;
    z: double;
  end;

  TCvPoint3D64f = _CvPoint3D64f;

  (* ******************************* CvSize's & CvBox ************************************* *)

  pCvSize = ^_CvSize;

  _CvSize = record
    width: int;
    height: int;
    { TODO:CvSize }
    // CvSize(int w = 0, int h = 0): width(w), height(h) {}
    class function CvSize(const w: int = 0; const h: int = 0): _CvSize; static;
    // template<typename _Tp>
    // CvSize(const cv::Size_<_Tp>& sz): width(cv::saturate_cast<int>(sz.width)), height(cv::saturate_cast<int>(sz.height)) {}
    // template<typename _Tp>
    // operator cv::Size_<_Tp>() const { return cv::Size_<_Tp>(cv::saturate_cast<_Tp>(width), cv::saturate_cast<_Tp>(height)); }
  end;

  TCvSize = _CvSize;

  _CvSize2D32f = record
    width: float;
    height: float;
    { TODO:CvSize2D32f }
    // CvSize2D32f(float w = 0, float h = 0): width(w), height(h) {}
    // template<typename _Tp>
    // CvSize2D32f(const cv::Size_<_Tp>& sz): width(cv::saturate_cast<float>(sz.width)), height(cv::saturate_cast<float>(sz.height)) {}
    // template<typename _Tp>
    // operator cv::Size_<_Tp>() const { return cv::Size_<_Tp>(cv::saturate_cast<_Tp>(width), cv::saturate_cast<_Tp>(height)); }
  end;

  TCvSize2D32f = _CvSize2D32f;

  (* * @sa RotatedRect
  *)
  _CvBox2D = record
    center: TCvPoint2D32f; (* *< Center of the box. *)
    size: TCvSize2D32f;    (* *< Box width and length. *)
    angle: float;(* *< Angle between the horizontal axis *)
    (* *< and the first side (i.e. length) in degrees *)
    { TODO:CvBox2D }
    // CvBox2D(CvPoint2D32f c = CvPoint2D32f(), CvSize2D32f s = CvSize2D32f(), float a = 0) : center(c), size(s), angle(a) {}
    // CvBox2D(const cv::RotatedRect& rr) : center(rr.center), size(rr.size), angle(rr.angle) {}
    // operator cv::RotatedRect() const { return cv::RotatedRect(center, size, angle); }
  end;

  TCvBox2D = _CvBox2D;

  (* * Line iterator state: *)
  pCvLineIterator = ^_CvLineIterator;

  _CvLineIterator = record
    (* * Pointer to the current point: *)
    ptr: puchar;

    (* Bresenham algorithm state: *)
    err: int;
    plus_delta: int;
    minus_delta: int;
    plus_step: int;
    minus_step: int;
  end;

  TCvLineIterator = _CvLineIterator;

  (* ************************************ CvSlice ***************************************** *)

  pCvSlice = ^_CvSlice;

  _CvSlice = record
    start_index, end_index: int;

    { TODO:CvSlice }
    // CvSlice(int start = 0, int end = 0) : start_index(start), end_index(end) {}
    class function CvSlice(start: int = 0; &end: int = 0): _CvSlice; static;
    // CvSlice(const cv::Range& r) { *this = (r.start != INT_MIN && r.end != INT_MAX) ? CvSlice(r.start, r.end) : CvSlice(0, CV_WHOLE_SEQ_END_INDEX); }
    // operator cv::Range() const { return (start_index == 0 && end_index == CV_WHOLE_SEQ_END_INDEX ) ? cv::Range::all() : cv::Range(start_index, end_index); }
  end;

  TCvSlice = _CvSlice;

  (* ************************************ CvScalar **************************************** *)
  (* * @sa Scalar_
  *)
  pCvScalar = ^_CvScalar;

  _CvScalar = record
    val: array [0 .. 4] of double;

    { TODO:CvScalar }
    // CvScalar() {}
    // CvScalar(double d0, double d1 = 0, double d2 = 0, double d3 = 0) { val[0] = d0; val[1] = d1; val[2] = d2; val[3] = d3; }
    class function CvScalar(d0: double; d1: double = 0; d2: double = 0; d3: double = 0): _CvScalar; static;
    // template<typename _Tp>
    // CvScalar(const cv::Scalar_<_Tp>& s) { val[0] = s.val[0]; val[1] = s.val[1]; val[2] = s.val[2]; val[3] = s.val[3]; }
    // template<typename _Tp, int cn>
    // CvScalar(const cv::Vec<_Tp, cn>& v)
    // {
    // int i;
    // for( i = 0; i < (cn < 4 ? cn : 4); i++ ) val[i] = v.val[i];
    // for( ; i < 4; i++ ) val[i] = 0;
    // }
    // template<typename _Tp>
    // operator cv::Scalar_<_Tp>() const { return cv::Scalar_<_Tp>(cv::saturate_cast<_Tp>(val[0]), cv::saturate_cast<_Tp>(val[1]), cv::saturate_cast<_Tp>(val[2]), cv::saturate_cast<_Tp>(val[3])); }
  end;

  TCvScalar = _CvScalar;

  (* ***************************************************************************************\
    *     Dynamic Data structures*
    \*************************************************************************************** *)

  (* ******************************* Memory storage *************************************** *)

  pCvMemBlock = ^_CvMemBlock;

  _CvMemBlock = record
    prev: pCvMemBlock;
    next: pCvMemBlock;
  end;

  TCvMemBlock = _CvMemBlock;

  pCvMemStorage = ^_CvMemStorage;

  _CvMemStorage = record
    signature: int;
    bottom: pCvMemBlock;   (* *< First allocated block. *)
    top: pCvMemBlock;      (* *< Current memory block - top of the stack. *)
    parent: pCvMemStorage; (* *< We get new blocks from parent as needed. *)
    block_size: int;       (* *< Block size. *)
    free_space: int;       (* *< Remaining free space in current block. *)
  end;

  TCvMemStorage = _CvMemStorage;

  pCvMemStoragePos = ^_CvMemStoragePos;

  _CvMemStoragePos = record
    top: pCvMemBlock;
    free_space: int;
  end;

  TCvMemStoragePos = _CvMemStoragePos;

  (* ********************************** Sequence ****************************************** *)

  pCvSeqBlock = ^_CvSeqBlock;

  _CvSeqBlock = record
    prev: pCvSeqBlock; (* *< Previous sequence block. *)
    next: pCvSeqBlock; (* *< Next sequence block. *)
    start_index: int;  (* *< Index of the first element in the block + *)
    (* *< sequence->first->start_index. *)
    count: int;   (* *< Number of elements in the block. *)
    data: pschar; (* *< Pointer to the first element of the block. *)
  end;

  TCvSeqBlock = _CvSeqBlock;

  (* *
    Read/Write sequence.
    Elements can be dynamically inserted to or deleted from the sequence.
  *)

  pCvSeq = ^_CvSeq;

  pCV_TREE_NODE_FIELDS_pCvSeq = record
    flags: int;       (* *< Miscellaneous flags. *)
    header_size: int; (* *< Size of sequence header. *)
    h_prev: pCvSeq;   (* *< Previous sequence. *)
    h_next: pCvSeq;   (* *< Next sequence. *)
    v_prev: pCvSeq;   (* *< 2nd previous sequence. *)
    v_next: pCvSeq;   (* *< 2nd next sequence. *)
  end;

  CV_SEQUENCE_FIELDS = record
    CV_TREE_NODE: pCV_TREE_NODE_FIELDS_pCvSeq;
    total: int;     (* *< Total number of elements. *)
    elem_size: int; (* *< Size of sequence element in bytes. *)
    block_max: pschar;        (* *< Maximal bound of the last block. *)
    ptr: pschar;    (* *< Current write pointer. *)
    delta_elems: int;         (* *< Grow seq this many at a time. *)
    storage: pCvMemStorage;   (* *< Where the seq is stored. *)
    free_blocks: pCvSeqBlock; (* *< Free blocks list. *)
    first: pCvSeqBlock;       (* *< Pointer to the first sequence block. *)
  end;

  _CvSeq = CV_SEQUENCE_FIELDS;
  TCvSeq = _CvSeq;

  (* ************************************** Set ******************************************* *)
  (* * @brief Set
    Order is not preserved. There can be gaps between sequence elements.
    After the element has been inserted it stays in the same place all the time.
    The MSB(most-significant or sign bit) of the first field (flags) is 0 iff the element exists.
  *)

  pCvSetElem = ^_CvSetElem;

  // _CvSetElem = pCV_SET_ELEM_FIELDS<pCvSetElem>;
  _CvSetElem = record
    flags: int;
    next_free: pCvSetElem;
  end;

  TCvSetElem = _CvSetElem;

  CV_SET_FIELDS = record
    // ------- SEQUENCE_FIELDS: CV_SEQUENCE_FIELDS; -------
    SEQUENCE_FIELDS: _CvSetElem;
    free_elems: pCvSetElem;
    active_count: int;
  end;

  _CvSet = CV_SET_FIELDS;
  TCvSet = _CvSet;

  (* ************************************ Graph ******************************************* *)

  (* * @name Graph

    We represent a graph as a set of vertices. Vertices contain their adjacency lists (more exactly,
    pointers to first incoming or outcoming edge (or 0 if isolated vertex)). Edges are stored in
    another set. There is a singly-linked list of incoming/outcoming edges for each vertex.

    Each edge consists of:

    - Two pointers to the starting and ending vertices (vtx[0] and vtx[1] respectively).

    A graph may be oriented or not. In the latter case, edges between vertex i to vertex j are not
    distinguished during search operations.

    - Two pointers to next edges for the starting and ending vertices, where next[0] points to the
    next edge in the vtx[0] adjacency list and next[1] points to the next edge in the vtx[1]
    adjacency list.

    @see CvGraphEdge, CvGraphVtx, CvGraphVtx2D, CvGraph
    @{
  *)

  pCvGraphEdge = ^_CvGraphEdge;
  pCvGraphVtx  = ^_CvGraphVtx;

  CV_GRAPH_EDGE_FIELDS = record
    flags: int;
    weight: float;
    next: array [0 .. 1] of pCvGraphEdge;
    vtx: array [0 .. 1] of pCvGraphVtx;
  end;

  CV_GRAPH_VERTEX_FIELDS = record
    flags: int;
    first: pCvGraphEdge;
  end;

  _CvGraphEdge = CV_GRAPH_EDGE_FIELDS;
  TCvGraphEdge = _CvGraphEdge;

  _CvGraphVtx = CV_GRAPH_VERTEX_FIELDS;
  TCvGraphVtx = _CvGraphVtx;

  _CvGraphVtx2D = record
    GRAPH_VERTEX_FIELDS: CV_GRAPH_VERTEX_FIELDS;
    ptr: pCvPoint2D32f;
  end;

  TCvGraphVtx2D = _CvGraphVtx2D;

  (* *
    Graph is "derived" from the set (this is set a of vertices)
    and includes another set (edges)
  *)
  CV_GRAPH_FIELDS = record
    SET_FIELDS: CV_SET_FIELDS;
    edges: pCvSet;
  end;

  _CvGraph = CV_GRAPH_FIELDS;
  pCvGraph = ^_CvGraph;
  TCvGraph = _CvGraph;

  (* * @} *)

  (* ********************************** Chain/Contour ************************************ *)

  pCvChain = ^_CvChain;

  _CvChain = record
    SEQUENCE_FIELDS: CV_SEQUENCE_FIELDS;
    origin: TCvPoint;
  end;

  TCvChain = _CvChain;

  CV_CONTOUR_FIELDS = record
    SEQUENCE_FIELDS: CV_SEQUENCE_FIELDS;
    rect: TCvRect;
    color: int;
    reserved: array [0 .. 2] of int;
  end;

  pCvContour = ^_CvContour;
  _CvContour = CV_CONTOUR_FIELDS;
  TCvContour = _CvContour;

  _CvPoint2DSeq = _CvContour;
  TCvPoint2DSeq = _CvPoint2DSeq;

  (* ************************************************************************************** *)
  (* Sequence writer & reader *)
  (* ************************************************************************************** *)

  CV_SEQ_WRITER_FIELDS = record
    header_size: int;
    seq: pCvSeq;        (* *< the sequence written *)
    block: pCvSeqBlock; (* *< current block *)
    ptr: pschar;        (* *< pointer to free space *)
    block_min: pschar;  (* *< pointer to the beginning of block *)
    block_max: pschar;  (* *< pointer to the end of block *)
  end;

  _CvSeqWriter = CV_SEQ_WRITER_FIELDS;
  pCvSeqWriter = ^_CvSeqWriter;
  TCvSeqWriter = _CvSeqWriter;

  CV_SEQ_READER_FIELDS = record
    header_size: int;
    seq: pCvSeq;        (* *< sequence, beign read *)
    block: pCvSeqBlock; (* *< current block *)
    ptr: pschar;        (* *< pointer to element be read next *)
    block_min: pschar;  (* *< pointer to the beginning of block *)
    block_max: pschar;  (* *< pointer to the end of block *)
    delta_index: int;   (* *< = seq->first->start_index *)
    prev_elem: pschar;  (* *< pointer to previous element *)
  end;

  _CvSeqReader = CV_SEQ_READER_FIELDS;
  pCvSeqReader = ^_CvSeqReader;
  TCvSeqReader = _CvSeqReader;

  (* ***************************************************************************************\
    *   Data structures for persistence (a.k.a serialization) functionality        *
    \*************************************************************************************** *)
type
  (* * "black box" file storage *)
  pCvFileStorage = ^_CvFileStorage;

  _CvFileStorage = record
  end;

  TCvFileStorage = _CvFileStorage;

  (* * @brief List of attributes. :

    In the current implementation, attributes are used to pass extra parameters when writing user
    objects (see cvWrite). XML attributes inside tags are not supported, aside from the object type
    specification (type_id attribute).
    @see cvAttrList, cvAttrValue
  *)
  pCvAttrList = ^_CvAttrList;

  _CvAttrList = record
    attr: pcvchar;     (* *< NULL-terminated array of (attribute_name,attribute_value) pairs. *)
    next: pCvAttrList; (* *< Pointer to next chunk of the attributes list. *)
  end;

  TCvAttrList = _CvAttrList;

  pCvTypeInfo = ^_CvTypeInfo;

  _CvString = record
    len: int;
    ptr: pcvchar;
  end;

  TCvString = _CvString;

  (* * All the keys (names) of elements in the read file storage
    are stored in the hash to speed up the lookup operations: *)
  pCvStringHashNode = ^_CvStringHashNode;

  _CvStringHashNode = record
    hashval: unsigned;
    str: TCvString;
    next: pCvStringHashNode;
  end;

  TCvStringHashNode = _CvStringHashNode;

  // typedef struct CvGenericHash CvFileNodeHash;
  pCvFileNodeHash = ^_CvFileNodeHash;

  _CvFileNodeHash = record
  end;

  (* * Basic element of the file storage - scalar or collection: *)
  pCvFileNode = ^_CvFileNode;

  _CvFileNode = record
    tag: int;
    info: pCvTypeInfo; (* *< type information
      (only for user-defined object, for others it is 0) *)
    case byte of
      0:
        (f: double); (* *< scalar floating-point number *)
      1:
        (i: int); (* *< scalar integer number *)
      2:
        (str: TCvString); (* *< text string *)
      3:
        (seq: pCvSeq); (* *< sequence (ordered collection of file nodes) *)
      4:
        (map: pCvFileNodeHash); (* *< map (collection of named file nodes) *)
  end;

  TCvFileNode = _CvFileNode;

  { TODO:CvIsInstanceFunc }
  // typedef int (CV_CDECL *CvIsInstanceFunc)( const void* struct_ptr );
  TCvIsInstanceFunc = function(const struct_ptr: pointer): int;
  // typedef void (CV_CDECL *CvReleaseFunc)( void** struct_dblptr );
  TCvReleaseFunc = procedure(struct_dblptr: pointer);
  // typedef void* (CV_CDECL *CvReadFunc)( CvFileStorage* storage, CvFileNode* node );
  TCvReadFunc = function(storage: pCvFileStorage; node: pCvFileNode): pointer;
  // typedef void (CV_CDECL *CvWriteFunc)( CvFileStorage* storage, const char* name,
  // const void* struct_ptr, CvAttrList attributes );
  TCvWriteFunc = procedure(storage: pCvFileStorage; const name: pcvchar; const struct_ptr: pointer; attributes: TCvAttrList);
  // typedef void* (CV_CDECL *CvCloneFunc)( const void* struct_ptr );
  TCvCloneFunc = function(const struct_ptr: pointer): pointer;

  (* * @brief Type information

    The structure contains information about one of the standard or user-defined types. Instances of the
    type may or may not contain a pointer to the corresponding CvTypeInfo structure. In any case, there
    is a way to find the type info structure for a given object using the cvTypeOf function.
    Alternatively, type info can be found by type name using cvFindType, which is used when an object
    is read from file storage. The user can register a new type with cvRegisterType that adds the type
    information structure into the beginning of the type list. Thus, it is possible to create
    specialized types from generic standard types and override the basic methods.
  *)
  _CvTypeInfo = record
    flags: int; (* *< not used *)
    header_size: int;     (* *< sizeof(CvTypeInfo) *)
    prev: pCvTypeInfo;    (* *< previous registered type in the list *)
    next: pCvTypeInfo;    (* *< next registered type in the list *)
    type_name: pcvchar;   (* *< type name, written to file storage *)
    is_instance: TCvIsInstanceFunc; (* *< checks if the passed object belongs to the type *)
    release: TCvReleaseFunc;        (* *< releases object (memory etc.) *)
    read: TCvReadFunc;    (* *< reads object from file storage *)
    write: TCvWriteFunc;  (* *< writes object to file storage *)
    clone: TCvCloneFunc;  (* *< creates a copy of the object *)
  end;

  TCvTypeInfo = _CvTypeInfo;

  // **************************** initizlized const *****************************************

  (* ***************************************************************************************\
    *    Image type (IplImage)   *
    \*************************************************************************************** *)
const
  IPL_IMAGE_MAGIC_VAL = sizeof(TIplImage);

  (* ***************************************************************************************\
    *    Matrix type (CvMat)     *
    \*************************************************************************************** *)
  CV_WHOLE_ARR: TCvSlice = (start_index: 0; end_index: $3FFFFFFF); // cvSlice( 0, $3fffffff );

  (* ************************************ CvSlice ***************************************** *)
  CV_WHOLE_SEQ: TCvSlice = (start_index: 0; end_index: CV_WHOLE_SEQ_END_INDEX);

  (* ***************************************************************************************\
    *      Sequence types*
    \*************************************************************************************** *)

  // CV_SEQ_ELTYPE_POINT         = CV_32SC2 ; (**< (x,y) *)
  // CV_SEQ_ELTYPE_CODE= CV_8UC1  ; (**< freeman code: 0..7 *)
  // CV_SEQ_ELTYPE_INDEX         = CV_32SC1 ; (**< #(x,y) *)
  // CV_SEQ_ELTYPE_POINT3D       = CV_32FC3 ; (**< (x,y,z)  *)
  (* * point sets *)
  // CV_SEQ_POINT_SET      = (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT) ;
  // CV_SEQ_POLYLINE       = (CV_SEQ_KIND_CURVE  or CV_SEQ_ELTYPE_POINT);
  // CV_SEQ_POINT3D_SET    = (CV_SEQ_KIND_GENERIC or CV_SEQ_ELTYPE_POINT3D);
  // CV_SEQ_POLYGON        = (CV_SEQ_FLAG_CLOSED or CV_SEQ_POLYLINE );
  // CV_SEQ_CONTOUR        = CV_SEQ_POLYGON;
  // CV_SEQ_SIMPLE_POLYGON = (CV_SEQ_FLAG_SIMPLE or CV_SEQ_POLYGON  );
  (* * chain-coded curves *)
  // CV_SEQ_CHAIN= (CV_SEQ_KIND_CURVE  or CV_SEQ_ELTYPE_CODE);
  // CV_SEQ_CHAIN_CONTOUR  = (CV_SEQ_FLAG_CLOSED or CV_SEQ_CHAIN);
  (* * sequence of the integer numbers *)
  // CV_SEQ_INDEX= (CV_SEQ_KIND_GENERIC  or CV_SEQ_ELTYPE_INDEX);

  (* ***************************************************************************************\
    *   Data structures for persistence (a.k.a serialization) functionality        *
    \*************************************************************************************** *)

  // #define CV_NODE_TYPE(flags)  ((flags) & CV_NODE_TYPE_MASK)

  // #define CV_NODE_IS_INT(flags)        (CV_NODE_TYPE(flags) == CV_NODE_INT)
  // #define CV_NODE_IS_REAL(flags)       (CV_NODE_TYPE(flags) == CV_NODE_REAL)
  // #define CV_NODE_IS_STRING(flags)     (CV_NODE_TYPE(flags) == CV_NODE_STRING)
  // #define CV_NODE_IS_SEQ(flags)        (CV_NODE_TYPE(flags) == CV_NODE_SEQ)
  // #define CV_NODE_IS_MAP(flags)        (CV_NODE_TYPE(flags) == CV_NODE_MAP)
  // #define CV_NODE_IS_COLLECTION(flags) (CV_NODE_TYPE(flags) >= CV_NODE_SEQ)
  // #define CV_NODE_IS_FLOW(flags)       (((flags) & CV_NODE_FLOW) != 0)
  // #define CV_NODE_IS_EMPTY(flags)      (((flags) & CV_NODE_EMPTY) != 0)
  // #define CV_NODE_IS_USER(flags)       (((flags) & CV_NODE_USER) != 0)
  // #define CV_NODE_HAS_NAME(flags)      (((flags) & CV_NODE_NAMED) != 0)
  // #define CV_NODE_SEQ_IS_SIMPLE(seq) (((seq)->flags & CV_NODE_SEQ_SIMPLE) != 0)

  // ****************************** function **********************************************

  (* ***************************************************************************************\
    *         Common macros and inline functions     *
    \*************************************************************************************** *)

  { TODO : Common macros and inline functions }

  // #define CV_SWAP(a,b,t) ((t) = (a), (a) = (b), (b) = (t))

  (* * min & max without jumps *)
  // #define  CV_IMIN(a, b)  ((a) ^ (((a)^(b)) & (((a) < (b)) - 1)))

  // #define  CV_IMAX(a, b)  ((a) ^ (((a)^(b)) & (((a) > (b)) - 1)))

  (* * absolute value without jumps *)
  // #ifndef __cplusplus
  // #  define  CV_IABS(a)     (((a) ^ ((a) < 0 ? -1 : 0)) - ((a) < 0 ? -1 : 0))
  // #else
  // #  define  CV_IABS(a)     abs(a)
  // {$endif}
  // #define  CV_CMP(a,b)    (((a) > (b)) - ((a) < (b)))
  // #define  CV_SIGN(a)     CV_CMP((a),0)

  // #define cvInvSqrt(value) ((float)(1./sqrt(value)))
  // #define cvSqrt(value)  ((float)sqrt(value))

  (* ************** Random number generation ****************** *)

  (* * @brief Initializes a random number generator state.

    The function initializes a random number generator and returns the state. The pointer to the state
    can be then passed to the cvRandInt, cvRandReal and cvRandArr functions. In the current
    implementation a multiply-with-carry generator is used.
    @param seed 64-bit value used to initiate a random sequence
    @sa the C++ class RNG replaced CvRNG.
  *)
  // CV_INLINE CvRNG cvRNG( int64 seed CV_DEFAULT(-1))
  // {
  // CvRNG rng = seed ? (uint64)seed : (uint64)(int64)-1;
  // return rng;
  // }
function cvRNG(const seed: Int64 = -1): TCvRNG; inline;

(* * @brief Returns a 32-bit unsigned integer and updates RNG.

  The function returns a uniformly-distributed random 32-bit unsigned integer and updates the RNG
  state. It is similar to the rand() function from the C runtime library, except that OpenCV functions
  always generates a 32-bit random number, regardless of the platform.
  @param rng CvRNG state initialized by cvRNG.
*)
// CV_INLINE unsigned cvRandInt( CvRNG* rng )
// {
// uint64 temp = *rng;
// temp = (uint64)(unsigned)temp*CV_RNG_COEFF + (temp >> 32);
// *rng = temp;
// return (unsigned)temp;
// }
  function cvRandInt(Var rng: TCvRNG): unsigned; inline;

  (* * @brief Returns a floating-point random number and updates RNG.

    The function returns a uniformly-distributed random floating-point number between 0 and 1 (1 is not
    included).
    @param rng RNG state initialized by cvRNG
  *)
  // CV_INLINE double cvRandReal( CvRNG* rng )
  // {
  // return cvRandInt(rng)*2.3283064365386962890625e-10 (* 2^-32 *);
  // }
    function cvRandReal(Var rng: TCvRNG): double; inline;

    (* ***************************************************************************************
      *    Image type (IplImage)   *
      *************************************************************************************** *)

    // CV_EXPORTS _IplImage cvIplImage(const cv::Mat& m);
      function cvIplImage(const m: TMat): TIplImage; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
      // CV_INLINE IplImage cvIplImage()
      // {
      // #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
      // IplImage self = CV_STRUCT_INITIALIZER; self.nSize = sizeof(IplImage); return self;
      // #else
      // return _IplImage();
      // {$endif}
      // }
        function cvIplImage(): TIplImage; inline; overload;

        // #define CV_IS_IMAGE_HDR(img) \
        // ((img) != NULL && ((const IplImage*)(img))->nSize == sizeof(IplImage))
function CV_IS_IMAGE_HDR(const img: pCvArr): boolean; inline;

// #define CV_IS_IMAGE(img) \
// (CV_IS_IMAGE_HDR(img) && ((IplImage*)img)->imageData != NULL)
  function CV_IS_IMAGE(const img: pCvArr): boolean; inline;

  (* * get reference to pixel at (col,row),
    for multi-channel images (col) should be multiplied by number of channels *)

  { TODO : CV_IMAGE_ELEM }
  // #define CV_IMAGE_ELEM( image, elemtype, row, col )       \
  // (((elemtype*)((image)->imageData + (image)->widthStep*(row)))[(col)])
  // function CV_IMAGE_ELEM<T>(image, elemtype, row, col)

  (* ***************************************************************************************\
    *    Matrix type (CvMat)     *
    \*************************************************************************************** *)

  { TODO : CV_IS_MAT_HDR }

  // #define CV_IS_MAT_HDR(mat) \
  // ((mat) != NULL && \
  // (((const CvMat*)(mat))->type & CV_MAGIC_MASK) == CV_MAT_MAGIC_VAL && \
  // ((const CvMat*)(mat))->cols > 0 && ((const CvMat*)(mat))->rows > 0)
    function CV_IS_MAT_HDR(const mat: pCvArr): boolean; inline;
    //
    // #define CV_IS_MAT_HDR_Z(mat) \
    // ((mat) != NULL && \
    // (((const CvMat*)(mat))->type & CV_MAGIC_MASK) == CV_MAT_MAGIC_VAL && \
    // ((const CvMat*)(mat))->cols >= 0 && ((const CvMat*)(mat))->rows >= 0)
    //
    // #define CV_IS_MAT(mat) \
    // (CV_IS_MAT_HDR(mat) && ((const CvMat*)(mat))->data.ptr != NULL)
      function CV_IS_MAT(const mat: pCvArr): boolean; inline;
      //
      // #define CV_IS_MASK_ARR(mat) \
      // (((mat)->type & (CV_MAT_TYPE_MASK & ~CV_8SC1)) == 0)
      //
      // #define CV_ARE_TYPES_EQ(mat1, mat2) \
      // ((((mat1)->type ^ (mat2)->type) & CV_MAT_TYPE_MASK) == 0)
      //
      // #define CV_ARE_CNS_EQ(mat1, mat2) \
      // ((((mat1)->type ^ (mat2)->type) & CV_MAT_CN_MASK) == 0)
      //
      // #define CV_ARE_DEPTHS_EQ(mat1, mat2) \
      // ((((mat1)->type ^ (mat2)->type) & CV_MAT_DEPTH_MASK) == 0)
      //
      // #define CV_ARE_SIZES_EQ(mat1, mat2) \
      // ((mat1)->rows == (mat2)->rows && (mat1)->cols == (mat2)->cols)
      //
      // #define CV_IS_MAT_CONST(mat)  \
      // (((mat)->rows|(mat)->cols) == 1)
      //
      // #define IPL2CV_DEPTH(depth) \
      // ((((CV_8U)+(CV_16U<<4)+(CV_32F<<8)+(CV_64F<<16)+(CV_8S<<20)+ \
      // (CV_16S<<24)+(CV_32S<<28)) >> ((((depth) & $F0) >> 2) + \
      // (((depth) & IPL_DEPTH_SIGN) ? 20 : 0))) & 15)

      (* * Inline constructor. No data is allocated internally!!!
        * (Use together with cvCreateData, or use cvCreateMat instead to
        * get a matrix with allocated data):
      *)
      // CV_INLINE CvMat cvMat( int rows, int cols, int type, void* data CV_DEFAULT(NULL))
      // {
      // CvMat m;
      //
      // assert( (unsigned)CV_MAT_DEPTH(type) <= CV_64F );
      // type = CV_MAT_TYPE(type);
      // m.type = CV_MAT_MAGIC_VAL | CV_MAT_CONT_FLAG | type;
      // m.cols = cols;
      // m.rows = rows;
      // m.step = m.cols*CV_ELEM_SIZE(type);
      // m.data.ptr = (uchar*)data;
      // m.refcount = NULL;
      // m.hdr_refcount = 0;
      //
      // return m;
      // }
        function cvMat(rows, cols, &type: int; const data: pointer = nil): TCvMat; inline; overload;

        // CV_INLINE CvMat cvMat(const cv::Mat& m);
        // function cvMat(const m:TMat):TCvMat; overload;
        // CV_INLINE CvMat cvMat(const cv::Mat& m)
        // {
        // CvMat self;
        // CV_DbgAssert(m.dims <= 2);
        // self = cvMat(m.rows, m.dims == 1 ? 1 : m.cols, m.type(), m.data);
        // self.step = (int)m.step[0];
        // self.type = (self.type & ~cv::Mat::CONTINUOUS_FLAG) | (m.flags & cv::Mat::CONTINUOUS_FLAG);
        // return self;
        // }
function cvMat(const m: TMat): TCvMat; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_INLINE CvMat cvMat()
// {
// #if !defined(CV__ENABLE_C_API_CTORS)
// CvMat self = CV_STRUCT_INITIALIZER; return self;
// {$else}
// return CvMat();
// {$endif}
// }
  function cvMat(): TCvMat; overload; inline;

  // CV_INLINE CvMat cvMat(const CvMat& m)
  // {
  // #if !defined(CV__ENABLE_C_API_CTORS)
  // CvMat self = CV_STRUCT_INITIALIZER; memcpy(&self, &m, sizeof(self)); return self;
  // {$else}
  // return CvMat(m);
  // {$endif}
  // }
    function cvMat(const m: TCvMat): TCvMat; inline; overload;

    { TODO : CV_MAT_ELEM_PTR_FAST }

    // #define CV_MAT_ELEM_PTR_FAST( mat, row, col, pix_size )  \
    // (assert( (unsigned)(row) < (unsigned)(mat).rows &&   \
    // (unsigned)(col) < (unsigned)(mat).cols ),   \
    // (mat).data.ptr + (size_t)(mat).step*(row) + (pix_size)*(col))
    //
    // #define CV_MAT_ELEM_PTR( mat, row, col )       \
    // CV_MAT_ELEM_PTR_FAST( mat, row, col, CV_ELEM_SIZE((mat).type) )
    //
    // #define CV_MAT_ELEM( mat, elemtype, row, col ) \
    // (*(elemtype*)CV_MAT_ELEM_PTR_FAST( mat, row, col, sizeof(elemtype)))

    (* * @brief Returns the particular element of single-channel floating-point matrix.

      The function is a fast replacement for cvGetReal2D in the case of single-channel floating-point
      matrices. It is faster because it is inline, it does fewer checks for array type and array element
      type, and it checks for the row and column ranges only in debug mode.
      @param mat Input matrix
      @param row The zero-based index of row
      @param col The zero-based index of column
    *)
    // CV_INLINE  double  cvmGet( const CvMat* mat, int row, int col )
    // {
    // int type;
    //
    // type = CV_MAT_TYPE(mat->type);
    // assert( (unsigned)row < (unsigned)mat->rows &&
    // (unsigned)col < (unsigned)mat->cols );
    //
    // if( type == CV_32FC1 )
    // return ((float*)(void*)(mat->data.ptr + (size_t)mat->step*row))[col];
    // else
    // {
    // assert( type == CV_64FC1 );
    // return ((double*)(void*)(mat->data.ptr + (size_t)mat->step*row))[col];
    // }
    // }
      function cvmGet(const mat: pCvMat; row, col: int): double; inline; overload;

      (* * @brief Sets a specific element of a single-channel floating-point matrix.

        The function is a fast replacement for cvSetReal2D in the case of single-channel floating-point
        matrices. It is faster because it is inline, it does fewer checks for array type and array element
        type, and it checks for the row and column ranges only in debug mode.
        @param mat The matrix
        @param row The zero-based index of row
        @param col The zero-based index of column
        @param value The new value of the matrix element
      *)
      // CV_INLINE  void  cvmSet( CvMat* mat, int row, int col, double value )
      // {
      // int type;
      // type = CV_MAT_TYPE(mat->type);
      // assert( (unsigned)row < (unsigned)mat->rows &&
      // (unsigned)col < (unsigned)mat->cols );
      //
      // if( type == CV_32FC1 )
      // ((float*)(void*)(mat->data.ptr + (size_t)mat->step*row))[col] = (float)value;
      // else
      // {
      // assert( type == CV_64FC1 );
      // ((double*)(void*)(mat->data.ptr + (size_t)mat->step*row))[col] = value;
      // }
      // }
        procedure cvmSet(Var mat: TCvMat; row, col: int; Value: double); inline;

        // CV_INLINE int cvIplDepth( int type )
        // {
        // int depth = CV_MAT_DEPTH(type);
        // return CV_ELEM_SIZE1(depth)*8 | (depth == CV_8S || depth == CV_16S ||
        // depth == CV_32S ? IPL_DEPTH_SIGN : 0);
        // }
function cvIplDepth(&type: int): int; inline;

(* ***************************************************************************************\
  *   Multi-dimensional dense array (CvMatND)      *
  \*************************************************************************************** *)

// CV_EXPORTS CvMatND cvMatND(const cv::Mat& m);
  function cvMatND(const m: TMat): TCvMatND; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
  // CV_INLINE CvMatND cvMatND()
  // {
  // {$if not (defined(CV__ENABLE_C_API_CTORS) and defined(__cplusplus))}
  // CvMatND self = CV_STRUCT_INITIALIZER; return self;
  // {$else}
  // return CvMatND();
  // {$endif}
  // }
    function cvMatND(): TCvMatND; inline; overload;

    // #define CV_IS_MATND_HDR(mat) \
    // ((mat) != NULL && (((const CvMatND*)(mat))->type & CV_MAGIC_MASK) == CV_MATND_MAGIC_VAL)
      function CV_IS_MATND_HDR(const mat: pCvArr): boolean; inline;
      //
      // #define CV_IS_MATND(mat) \
      // (CV_IS_MATND_HDR(mat) && ((const CvMatND*)(mat))->data.ptr != NULL)
function CV_IS_MATND(const mat: pCvArr): boolean; inline;

(* ***************************************************************************************\
*  Multi-dimensional sparse array (CvSparseMat)  *
\*************************************************************************************** *)

// CV_EXPORTS CvSparseMat* cvCreateSparseMat(const cv::SparseMat& m);
{ TODO:TSparseMat }
// function cvCreateSparseMat(const m:TSparseMat):pCvSparseMat;

{ TODO:CV_IS_SPARSE_MAT_HDR }
// #define CV_IS_SPARSE_MAT_HDR(mat) \
// ((mat) != NULL && \
// (((const CvSparseMat*)(mat))->type & CV_MAGIC_MASK) == CV_SPARSE_MAT_MAGIC_VAL)
//
// #define CV_IS_SPARSE_MAT(mat) \
// CV_IS_SPARSE_MAT_HDR(mat)

(* ***************************************************************************************\
* Histogram*
\*************************************************************************************** *)

{ TODO: CV_IS_HIST }
// #define CV_IS_HIST( hist ) \
// ((hist) != NULL  && \
// (((CvHistogram*)(hist))->type & CV_MAGIC_MASK) == CV_HIST_MAGIC_VAL && \
// (hist)->bins != NULL)
//
// #define CV_IS_UNIFORM_HIST( hist ) \
// (((hist)->type & CV_HIST_UNIFORM_FLAG) != 0)
//
// #define CV_IS_SPARSE_HIST( hist ) \
// CV_IS_SPARSE_MAT((hist)->bins)
//
// #define CV_HIST_HAS_RANGES( hist ) \
// (((hist)->type & CV_HIST_RANGES_FLAG) != 0)

(* ************************************** CvRect **************************************** *)

(* * constructs CvRect structure. *)
// CV_INLINE  CvRect  cvRect( int x, int y, int width, int height )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvRect r = {x, y, width, height};
// {$else}
// CvRect r(x, y , width, height);
// {$endif}
// return r;
// }
// {$ifdef __cplusplus
// CV_INLINE CvRect cvRect(const cv::Rect& rc) { return cvRect(rc.x, rc.y, rc.width, rc.height); }
// {$endif}
//
// CV_INLINE  IplROI  cvRectToROI( CvRect rect, int coi )
// {
// IplROI roi;
// roi.xOffset = rect.x;
// roi.yOffset = rect.y;
// roi.width = rect.width;
// roi.height = rect.height;
// roi.coi = coi;
//
// return roi;
// }
//
//
// CV_INLINE  CvRect  cvROIToRect( IplROI roi )
// {
// return cvRect( roi.xOffset, roi.yOffset, roi.width, roi.height );
// }

(* ********************************** CvTermCriteria ************************************ *)

// CV_INLINE  CvTermCriteria  cvTermCriteria( int type, int max_iter, double epsilon )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvTermCriteria t = { type, max_iter, (float)epsilon};
// {$else}
// CvTermCriteria t(type, max_iter, epsilon);
// {$endif}
// return t;
// }
function CvTermCriteria(&type: int; max_iter: int; epsilon: double): TCvTermCriteria; inline; overload;
// {$ifdef __cplusplus
// CV_INLINE CvTermCriteria cvTermCriteria(const cv::TermCriteria& t) { return cvTermCriteria(t.type, t.maxCount, t.epsilon); }
function CvTermCriteria(const T: TTermCriteria): TCvTermCriteria; inline; overload;
// {$endif}

(* ****************************** CvPoint and variants ********************************** *)

(* * constructs CvPoint structure. *)
// CV_INLINE  CvPoint  cvPoint( int x, int y )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvPoint p = {x, y};
// {$else}
// CvPoint p(x, y);
// {$endif}
// return p;
// }
function CvPoint(x, y: int): TCvPoint; inline; overload;
// {$ifdef __cplusplus
// CV_INLINE CvPoint cvPoint(const cv::Point& pt) { return cvPoint(pt.x, pt.y); }
function CvPoint(const pt: TPoint): TCvPoint; inline; overload;
// {$endif}
//
//
// (** constructs CvPoint2D32f structure. *)
// CV_INLINE  CvPoint2D32f  cvPoint2D32f( double x, double y )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvPoint2D32f p = { (float)x, (float)y };
// {$else}
// CvPoint2D32f p((float)x, (float)y);
// {$endif}
// return p;
// }
//
// {$ifdef __cplusplus
// template<typename _Tp>
// CvPoint2D32f cvPoint2D32f(const cv::Point_<_Tp>& pt)
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvPoint2D32f p = { (float)pt.x, (float)pt.y };
// {$else}
// CvPoint2D32f p((float)pt.x, (float)pt.y);
// {$endif}
// return p;
// }
// {$endif}
//
// (** converts CvPoint to CvPoint2D32f. *)
// CV_INLINE  CvPoint2D32f  cvPointTo32f( CvPoint point )
// {
// return cvPoint2D32f( (float)point.x, (float)point.y );
// }
//
// (** converts CvPoint2D32f to CvPoint. *)
// CV_INLINE  CvPoint  cvPointFrom32f( CvPoint2D32f point )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvPoint ipt = { cvRound(point.x), cvRound(point.y) };
// {$else}
// CvPoint ipt(cvRound(point.x), cvRound(point.y));
// {$endif}
// return ipt;
// }
function cvPointFrom32f(const point: TCvPoint2D32f): TCvPoint; inline;

//
// (** constructs CvPoint3D32f structure. *)
// CV_INLINE  CvPoint3D32f  cvPoint3D32f( double x, double y, double z )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvPoint3D32f p = { (float)x, (float)y, (float)z };
// {$else}
// CvPoint3D32f p((float)x, (float)y, (float)z);
// {$endif}
// return p;
// }
//
// {$ifdef __cplusplus
// template<typename _Tp>
// CvPoint3D32f cvPoint3D32f(const cv::Point3_<_Tp>& pt)
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvPoint3D32f p  = { (float)pt.x, (float)pt.y, (float)pt.z };
// {$else}
// CvPoint3D32f p((float)pt.x, (float)pt.y, (float)pt.z);
// {$endif}
// return p;
// }
// {$endif}
//
//
// (** constructs CvPoint2D64f structure.*)
// CV_INLINE  CvPoint2D64f  cvPoint2D64f( double x, double y )
// {
// CvPoint2D64f p = { x, y };
// return p;
// }
//
// (** constructs CvPoint3D64f structure. *)
// CV_INLINE  CvPoint3D64f  cvPoint3D64f( double x, double y, double z )
// {
// CvPoint3D64f p = { x, y, z };
// return p;
// }

(* ******************************* CvSize's & CvBox ************************************* *)

function CvSize(const w: int = 0; const h: int = 0): _CvSize; inline;

(* * constructs CvSize2D32f structure. *)
// CV_INLINE  CvSize2D32f  cvSize2D32f( double width, double height )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvSize2D32f s = { (float)width, (float)height };
// {$else}
// CvSize2D32f s((float)width, (float)height);
// {$endif}
// return s;
// }
// {$ifdef __cplusplus
// template<typename _Tp>
// CvSize2D32f cvSize2D32f(const cv::Size_<_Tp>& sz)
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvSize2D32f s = { (float)sz.width, (float)sz.height };
// {$else}
// CvSize2D32f s((float)sz.width, (float)sz.height);
// {$endif}
// return s;
// }
// {$endif}
//
// {$ifdef __cplusplus
// CV_INLINE CvBox2D cvBox2D(CvPoint2D32f c = CvPoint2D32f(), CvSize2D32f s = CvSize2D32f(), float a = 0)
// {
// CvBox2D self;
// self.center = c;
// self.size = s;
// self.angle = a;
// return self;
// }
// CV_INLINE CvBox2D cvBox2D(const cv::RotatedRect& rr)
// {
// CvBox2D self;
// self.center = cvPoint2D32f(rr.center);
// self.size = cvSize2D32f(rr.size);
// self.angle = rr.angle;
// return self;
// }
// {$endif}

(* ************************************ CvSlice ***************************************** *)

// CV_INLINE  CvSlice  cvSlice( int start, int end )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus) && !defined(__CUDACC__))
// CvSlice slice = { start, end };
// {$else}
// CvSlice slice(start, end);
// {$endif}
// return slice;
// }
function CvSlice(start: int = 0; &end: int = 0): TCvSlice; inline;

// #if defined(__cplusplus)
// CV_INLINE  CvSlice  cvSlice(const cv::Range& r)
// {
// CvSlice slice = (r.start != INT_MIN && r.end != INT_MAX) ? cvSlice(r.start, r.end) : cvSlice(0, CV_WHOLE_SEQ_END_INDEX);
// return slice;
// }
// {$endif}

(* ************************************ CvScalar **************************************** *)

// CV_INLINE  CvScalar  cvScalar( double val0, double val1 CV_DEFAULT(0),
// double val2 CV_DEFAULT(0), double val3 CV_DEFAULT(0))
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvScalar scalar = CV_STRUCT_INITIALIZER;
// {$else}
// CvScalar scalar;
// {$endif}
// scalar.val[0] = val0; scalar.val[1] = val1;
// scalar.val[2] = val2; scalar.val[3] = val3;
// return scalar;
// }
function CvScalar(val0: double; val1: double = 0; val2: double = 0; val3: double = 0): TCvScalar; inline;

// {$ifdef __cplusplus
// CV_INLINE CvScalar cvScalar()
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvScalar scalar = CV_STRUCT_INITIALIZER;
// {$else}
// CvScalar scalar;
// {$endif}
// scalar.val[0] = scalar.val[1] = scalar.val[2] = scalar.val[3] = 0;
// return scalar;
// }
// CV_INLINE CvScalar cvScalar(const cv::Scalar& s)
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvScalar scalar = CV_STRUCT_INITIALIZER;
// {$else}
// CvScalar scalar;
// {$endif}
// scalar.val[0] = s.val[0];
// scalar.val[1] = s.val[1];
// scalar.val[2] = s.val[2];
// scalar.val[3] = s.val[3];
// return scalar;
// }
// {$endif}
//
// CV_INLINE  CvScalar  cvRealScalar( double val0 )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvScalar scalar = CV_STRUCT_INITIALIZER;
// {$else}
// CvScalar scalar;
// {$endif}
// scalar.val[0] = val0;
// scalar.val[1] = scalar.val[2] = scalar.val[3] = 0;
// return scalar;
// }
function cvRealScalar(val0: double): TCvScalar; inline;

// CV_INLINE  CvScalar  cvScalarAll( double val0123 )
// {
// #if !(defined(CV__ENABLE_C_API_CTORS) && defined(__cplusplus))
// CvScalar scalar = CV_STRUCT_INITIALIZER;
// {$else}
// CvScalar scalar;
// {$endif}
// scalar.val[0] = val0123;
// scalar.val[1] = val0123;
// scalar.val[2] = val0123;
// scalar.val[3] = val0123;
// return scalar;
// }
function cvScalarAll(val0123: double): TCvScalar; inline;

(* ***************************************************************************************\
*     Dynamic Data structures*
\*************************************************************************************** *)

(* ******************************* Memory storage *************************************** *)

// #define CV_IS_STORAGE(storage)  \
// ((storage) != NULL &&       \
// (((CvMemStorage*)(storage))->signature & CV_MAGIC_MASK) == CV_STORAGE_MAGIC_VAL)

(* ************************************** Set ******************************************* *)
(* * Checks whether the element pointed by ptr belongs to a set or not *)
// #define CV_IS_SET_ELEM( ptr )  (((CvSetElem*)(ptr))->flags >= 0)
function CV_IS_SET_ELEM(const ptr: pCvArr): boolean; inline;

(* ***************************************************************************************\
*      Sequence types*
\*************************************************************************************** *)

// #define CV_IS_SEQ(seq) \
// ((seq) != NULL && (((CvSeq*)(seq))->flags & CV_MAGIC_MASK) == CV_SEQ_MAGIC_VAL)
// #define CV_IS_SET(set) \
// ((set) != NULL && (((CvSeq*)(set))->flags & CV_MAGIC_MASK) == CV_SET_MAGIC_VAL)
// #define CV_SEQ_ELTYPE_PTR  CV_MAKE_TYPE(CV_8U, 8 (*sizeof(void*)*))
// #define CV_SEQ_ELTYPE_PPOINT         CV_SEQ_ELTYPE_PTR  (**< &(x,y) *)
//
// #define CV_SEQ_ELTYPE( seq )   ((seq)->flags & CV_SEQ_ELTYPE_MASK)
// #define CV_SEQ_KIND( seq )     ((seq)->flags & CV_SEQ_KIND_MASK )
//
// (** flag checking *)
// #define CV_IS_SEQ_INDEX( seq )      ((CV_SEQ_ELTYPE(seq) == CV_SEQ_ELTYPE_INDEX) && \
// (CV_SEQ_KIND(seq) == CV_SEQ_KIND_GENERIC))
//
// #define CV_IS_SEQ_CURVE( seq )      (CV_SEQ_KIND(seq) == CV_SEQ_KIND_CURVE)
// #define CV_IS_SEQ_CLOSED( seq )     (((seq)->flags & CV_SEQ_FLAG_CLOSED) != 0)
// #define CV_IS_SEQ_CONVEX( seq )     0
// #define CV_IS_SEQ_HOLE( seq )       (((seq)->flags & CV_SEQ_FLAG_HOLE) != 0)
// #define CV_IS_SEQ_SIMPLE( seq )     1
//
// (** type checking macros *)
// #define CV_IS_SEQ_POINT_SET( seq ) \
// ((CV_SEQ_ELTYPE(seq) == CV_32SC2 || CV_SEQ_ELTYPE(seq) == CV_32FC2))
//
// #define CV_IS_SEQ_POINT_SUBSET( seq ) \
// (CV_IS_SEQ_INDEX( seq ) || CV_SEQ_ELTYPE(seq) == CV_SEQ_ELTYPE_PPOINT)
//
// #define CV_IS_SEQ_POLYLINE( seq )   \
// (CV_SEQ_KIND(seq) == CV_SEQ_KIND_CURVE && CV_IS_SEQ_POINT_SET(seq))
//
// #define CV_IS_SEQ_POLYGON( seq )   \
// (CV_IS_SEQ_POLYLINE(seq) && CV_IS_SEQ_CLOSED(seq))
//
// #define CV_IS_SEQ_CHAIN( seq )   \
// (CV_SEQ_KIND(seq) == CV_SEQ_KIND_CURVE && (seq)->elem_size == 1)
//
// #define CV_IS_SEQ_CONTOUR( seq )   \
// (CV_IS_SEQ_CLOSED(seq) && (CV_IS_SEQ_POLYLINE(seq) || CV_IS_SEQ_CHAIN(seq)))
//
// #define CV_IS_SEQ_CHAIN_CONTOUR( seq ) \
// (CV_IS_SEQ_CHAIN( seq ) && CV_IS_SEQ_CLOSED( seq ))
//
// #define CV_IS_SEQ_POLYGON_TREE( seq ) \
// (CV_SEQ_ELTYPE (seq) ==  CV_SEQ_ELTYPE_TRIAN_ATR &&    \
// CV_SEQ_KIND( seq ) ==  CV_SEQ_KIND_BIN_TREE )
//
// #define CV_IS_GRAPH( seq )    \
// (CV_IS_SET(seq) && CV_SEQ_KIND((CvSet*)(seq)) == CV_SEQ_KIND_GRAPH)
//
// #define CV_IS_GRAPH_ORIENTED( seq )   \
// (((seq)->flags & CV_GRAPH_FLAG_ORIENTED) != 0)
//
// #define CV_IS_SUBDIV2D( seq )  \
// (CV_IS_SET(seq) && CV_SEQ_KIND((CvSet*)(seq)) == CV_SEQ_KIND_SUBDIV2D)

(* ************************************************************************************** *)
(* Operations on sequences *)
(* ************************************************************************************** *)

// #define  CV_SEQ_ELEM( seq, elem_type, index )\
// (** assert gives some guarantee that <seq> parameter is valid *)  \
// (   assert(sizeof((seq)->first[0]) == sizeof(CvSeqBlock) &&      \
// (seq)->elem_size == sizeof(elem_type)),  \
// (elem_type*)((seq)->first && (unsigned)index <     \
// (unsigned)((seq)->first->count) ?        \
// (seq)->first->data + (index) * sizeof(elem_type) : \
// cvGetSeqElem( (CvSeq*)(seq), (index) )))
// #define CV_GET_SEQ_ELEM( elem_type, seq, index ) CV_SEQ_ELEM( (seq), elem_type, (index) )
//
// (** Add element to sequence: *)
// #define CV_WRITE_SEQ_ELEM_VAR( elem_ptr, writer )     \
// {   \
// if( (writer).ptr >= (writer).block_max )\
// {         \
// cvCreateSeqBlock( &writer);         \
// }         \
// memcpy((writer).ptr, elem_ptr, (writer).seq->elem_size);\
// (writer).ptr += (writer).seq->elem_size;\
// }
//
// #define CV_WRITE_SEQ_ELEM( elem, writer )   \
// {   \
// assert( (writer).seq->elem_size == sizeof(elem)); \
// if( (writer).ptr >= (writer).block_max )\
// {         \
// cvCreateSeqBlock( &writer);         \
// }         \
// assert( (writer).ptr <= (writer).block_max - sizeof(elem));\
// memcpy((writer).ptr, &(elem), sizeof(elem));      \
// (writer).ptr += sizeof(elem); \
// }
//
//
// (** Move reader position forward: *)
// #define CV_NEXT_SEQ_ELEM( elem_size, reader )       \
// { \
// if( ((reader).ptr += (elem_size)) >= (reader).block_max ) \
// {       \
// cvChangeSeqBlock( &(reader), 1 ); \
// }       \
// }
//
//
// (** Move reader position backward: *)
// #define CV_PREV_SEQ_ELEM( elem_size, reader )      \
// {\
// if( ((reader).ptr -= (elem_size)) < (reader).block_min ) \
// {      \
// cvChangeSeqBlock( &(reader), -1 );         \
// }      \
// }
//
// (** Read element and move read position forward: *)
// #define CV_READ_SEQ_ELEM( elem, reader )   \
// {  \
// assert( (reader).seq->elem_size == sizeof(elem));\
// memcpy( &(elem), (reader).ptr, sizeof((elem)));  \
// CV_NEXT_SEQ_ELEM( sizeof(elem), reader )         \
// }
//
// (** Read element and move read position backward: *)
// #define CV_REV_READ_SEQ_ELEM( elem, reader ) \
// {    \
// assert( (reader).seq->elem_size == sizeof(elem));  \
// memcpy(&(elem), (reader).ptr, sizeof((elem)));     \
// CV_PREV_SEQ_ELEM( sizeof(elem), reader ) \
// }
//
//
// #define CV_READ_CHAIN_POINT( _pt, reader )\
// { \
// (_pt) = (reader).pt;        \
// if( (reader).ptr )\
// {       \
// CV_READ_SEQ_ELEM( (reader).code, (reader)); \
// assert( ((reader).code & ~7) == 0 );        \
// (reader).pt.x += (reader).deltas[(int)(reader).code][0];        \
// (reader).pt.y += (reader).deltas[(int)(reader).code][1];        \
// }       \
// }
//
// #define CV_CURRENT_POINT( reader )  (*((CvPoint*)((reader).ptr)))
// #define CV_PREV_POINT( reader )     (*((CvPoint*)((reader).prev_elem)))
//
// #define CV_READ_EDGE( pt1, pt2, reader )     \
// {    \
// assert( sizeof(pt1) == sizeof(CvPoint) &&\
// sizeof(pt2) == sizeof(CvPoint) &&\
// reader.seq->elem_size == sizeof(CvPoint)); \
// (pt1) = CV_PREV_POINT( reader );         \
// (pt2) = CV_CURRENT_POINT( reader );      \
// (reader).prev_elem = (reader).ptr;       \
// CV_NEXT_SEQ_ELEM( sizeof(CvPoint), (reader));      \
// }

(* *********** Graph macros *********** *)

(* * Return next graph edge for given vertex: *)
// #define  CV_NEXT_GRAPH_EDGE( edge, vertex )\
// (assert((edge)->vtx[0] == (vertex) || (edge)->vtx[1] == (vertex)),  \
// (edge)->next[(edge)->vtx[1] == (vertex)])

(* ***************************************************************************************\
*   Data structures for persistence (a.k.a serialization) functionality        *
\*************************************************************************************** *)
(* * initializes CvAttrList structure *)
// CV_INLINE CvAttrList cvAttrList( const char** attr CV_DEFAULT(NULL),
// CvAttrList* next CV_DEFAULT(NULL) )
// {
// CvAttrList l;
// l.attr = attr;
// l.next = next;
//
// return l;
// }

(* * @} *)

{$IFEND} (* OPENCV_CORE_TYPES_H *)
(* End of file. *)

{$ENDIF INTERFACE}

// ===========================================================================

{$IFDEF IMPLEMENTATION}

{$IF defined(OPENCV_CORE_TYPES_H) and (not defined(OPENCV_CORE_TYPES_H_IMPL))}
{$DEFINE OPENCV_CORE_TYPES_H_IMPL}
{$IF not defined(OPENCV_CORE_CVDEF_H_IMPL)}{$INCLUDE 'core/cvdef.impl.inc'}{$IFEND}
{$IF not defined(OPENCV_CORE_HPP_IMPL)}{$INCLUDE 'core.impl.inc'}{$IFEND}

function cvRNG(const seed: Int64 = -1): TCvRNG;
begin
  if seed <> 0 then
    Result := TCvRNG(seed)
  else
    Result := TCvRNG(-1);
end;

function cvRandInt(Var rng: TCvRNG): unsigned;
begin
  Var
    temp: uint64 := rng;
  temp := uint64(unsigned(temp) * CV_RNG_COEFF) + (temp shl 32);
  rng := temp;
  Result := unsigned(temp);
end;

function cvRandReal(Var rng: TCvRNG): double; inline;
begin
  Result := cvRandInt(rng) * 2.3283064365386962890625E-10; (* 2^-32 *)
end;

function cvIplImage(): TIplImage;
begin
  Result := default (TIplImage);
end;

function CV_IS_MAT_HDR(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MAT_HDR(mat) \
  // ((mat) != NULL && \
  // (((const CvMat*)(mat))->type & CV_MAGIC_MASK) == CV_MAT_MAGIC_VAL && \
  // ((const CvMat*)(mat))->cols > 0 && ((const CvMat*)(mat))->rows > 0)
  Result :=//
    (mat <> nil) and //
    ((pCvMat(mat)^.&type and CV_MAGIC_MASK) = CV_MAT_MAGIC_VAL) and //
    ((pCvMat(mat)^.cols > 0) and (pCvMat(mat)^.rows > 0));
end;

function CV_IS_MAT(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MAT(mat) \
  // (CV_IS_MAT_HDR(mat) && ((const CvMat*)(mat))->data.ptr != NULL)
  Result := CV_IS_MAT_HDR(mat) and (pCvMat(mat)^.data.ptr <> nil);
end;

function CV_IS_MATND_HDR(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MATND_HDR(mat) \
  // ((mat) != NULL && (((const CvMatND*)(mat))->type & CV_MAGIC_MASK) == CV_MATND_MAGIC_VAL)
  Result :=//
    (mat <> nil) and //
    ((pCvMatND(mat)^.&type and CV_MAGIC_MASK) = CV_MATND_MAGIC_VAL);
end;

function CV_IS_MATND(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MATND(mat) \
  // (CV_IS_MATND_HDR(mat) && ((const CvMatND*)(mat))->data.ptr != NULL)
  Result := CV_IS_MATND_HDR(mat) and (pCvMatND(mat)^.data.ptr <> nil);
end;

function cvRealScalar(val0: double): TCvScalar;
begin
  Result.val[0] := val0;
  Result.val[1] := 0;
  Result.val[2] := 0;
  Result.val[3] := 0;
end;

function CV_IS_SET_ELEM(const ptr: pCvArr): Boolean;
begin
  // #define CV_IS_SET_ELEM( ptr )  (((CvSetElem*)(ptr))->flags >= 0)
  Result := pCvSetElem(ptr)^.flags >= 0;
end;

(* ************************************ CvSlice ***************************************** *)

function cvSlice(start: int; &end: int): TCvSlice;
begin
  Result := _cvSlice.cvSlice(start, &end);
end;

class function _cvSlice.cvSlice(start: int = 0; &end: int = 0): _cvSlice;
begin
  Result.start_index := start;
  Result.end_index := &end;
end;

{ _IplImage }

class operator _IplImage.Initialize(out Dest: _IplImage);
begin
  // memset(this, 0, sizeof(*this));  // valid for POD structure
  Dest.nSize := sizeof(TIplImage);
end;

class operator _IplImage.Explicit(const m: TMat): _IplImage;
begin
  Result := cvIplImage(m);
end;

{ _CvScalar }

function cvScalar(val0: double; val1: double = 0; val2: double = 0; val3: double = 0): TCvScalar;
begin
  Result := _CvScalar.cvScalar(val0, val1, val2, val3);
end;

class function _CvScalar.cvScalar(d0: double; d1: double = 0; d2: double = 0; d3: double = 0): _CvScalar;
begin
  { val[0] = d0; val[1] = d1; val[2] = d2; val[3] = d3; }
  With Result do
  begin
    val[0] := d0;
    val[1] := d1;
    val[2] := d2;
    val[3] := d3;
  end;
end;

(* ***************************************************************************************\
  *    Matrix type (CvMat)     *
  \*************************************************************************************** *)

function cvMat(const m: TMat): TCvMat;
begin
  Assert(m.dims <= 2);
  Result := cvMat(m.rows, ifthen(m.dims = 1, 1, m.cols), m.&type(), m.data);
  Result.step := int(m.step[0]);
  Result.&type := (Result.&type and (not TMat.CONTINUOUS_FLAG)) or (m.flags and TMat.CONTINUOUS_FLAG);
end;

function cvMat(): TCvMat;
begin
  Result := default (TCvMat);
end;

function cvMat(const m: TCvMat): TCvMat;
begin
  Result := TCvMat(m);
end;

function cvmGet(const mat: pCvMat; row, col: int): double;
begin
  var
    &type: int;

  &type := CV_MAT_TYPE(mat^.&type);
  Assert((unsigned(row) < unsigned(mat^.rows)) and (unsigned(col) < unsigned(mat^.cols)));

  if (&type = CV_32FC1) then
    Result := pfloat(mat^.data.ptr + mat^.step * row)[col]
  else
  begin
    Assert(&type = CV_64FC1);
    Result := pdouble(mat^.data.ptr + mat^.step * row)[col];
  end;
end;

procedure cvmSet(Var mat: TCvMat; row, col: int; value: double);
begin
  var
    &type: int;
  &type := CV_MAT_TYPE(mat.&type);
  Assert((unsigned(row) < unsigned(mat.rows)) and (unsigned(col) < unsigned(mat.cols)));
  if (&type = CV_32FC1) then
    pfloat(mat.data.ptr + mat.step * row)[col] := float(value)
  else
  begin
    Assert(&type = CV_64FC1);
    pdouble(mat.data.ptr + mat.step * row)[col] := value;
  end
end;

function cvIplDepth(&type: int): int;
begin
  Var
    depth: int := CV_MAT_DEPTH(&type);
  Result :=     //
    (CV_ELEM_SIZE1(depth) * 8) or   //
    (ifthen(depth = CV_8S, 1, 0) or //
    ifthen(depth = CV_16S, 1, 0) or //
    ifthen(depth = CV_32S, IPL_DEPTH_SIGN, 0));
end;

class operator _CvMat.Explicit(const m: TMat): _CvMat;
begin
  Result := cvMat(m);
end;

function TPointerHelper.get_puchar: puchar;
begin
  Result := puchar(self);
end;

procedure TPointerHelper.set_puchar(const value: puchar);
begin
  self := value;
end;

function TPointerHelper.get_pshort: pshort;
begin
  Result := pshort(self);
end;

procedure TPointerHelper.set_pshort(const value: pshort);
begin
  self := value;
end;

function TPointerHelper.get_pInt: pInt;
begin
  Result := pInt(self);
end;

procedure TPointerHelper.set_pInt(const value: pInt);
begin
  self := value;
end;

function TPointerHelper.get_pfloat: pfloat;
begin
  Result := pfloat(self);
end;

procedure TPointerHelper.set_pfloat(const value: pfloat);
begin
  self := value;
end;

function TPointerHelper.get_pdouble: pdouble;
begin
  Result := pdouble(self);
end;

procedure TPointerHelper.set_pdouble(const value: pdouble);
begin
  self := value;
end;

(* ***************************************************************************************\
  *   Multi-dimensional dense array (CvMatND)      *
  \*************************************************************************************** *)

function cvMatND(const m: TMat): TCvMatND;
begin
{$IF not defined(PACKAGE)}
  Result := func_cvMatND(m);
{$IFEND}
end;

function cvMatND(): TCvMatND;
begin
  Result := default (TCvMatND);
end;

class operator _CvMatND.Explicit(const m: TMat): _CvMatND;
begin
  Result := cvMatND(m);
end;

(* ***************************************************************************************\
  *    Image type (IplImage)   *
  \*************************************************************************************** *)

function cvIplImage(const m: TMat): TIplImage;
begin
{$IF not defined(PACKAGE)}
  Result := func_cvIplImage(m);
{$IFEND}
end;

function CV_IS_IMAGE_HDR(const img: pCvArr): Boolean;
begin
  // ((img) != NULL && ((const IplImage*)(img))->nSize == sizeof(IplImage))
  Result := Assigned(img) and (pIplImage(img)^.nSize = sizeof(TIplImage));
end;

function CV_IS_IMAGE(const img: pCvArr): Boolean;
begin
  // (CV_IS_IMAGE_HDR(img) && ((IplImage*)img)->imageData != NULL)
  Result := CV_IS_IMAGE_HDR(img) and (pIplImage(img)^.imageData <> nil);
end;

function cvMat(rows, cols, &type: int; const data: pointer = nil): TCvMat;
begin
  Assert(unsigned(CV_MAT_DEPTH(&type)) <= CV_64F);
  &type := CV_MAT_TYPE(&type);
  Result.&type := CV_MAT_MAGIC_VAL or CV_MAT_CONT_FLAG or &type;
  Result.cols := cols;
  Result.rows := rows;
  Result.step := Result.cols * CV_ELEM_SIZE(&type);
  Result.data.ptr := puchar(data);
  Result.refcount := nil;
  Result.hdr_refcount := 0;
end;

function cvScalarAll(val0123: double): TCvScalar;
begin
  Result.val[0] := val0123;
  Result.val[1] := val0123;
  Result.val[2] := val0123;
  Result.val[3] := val0123;
end;

(* ****************************** CvPoint and variants ********************************** *)

class function _CvPoint.CvPoint(const _x: int = 0; const _y: int = 0): _CvPoint;
begin
  Result.x := _x;
  Result.y := _y;
end;

function CvPoint(x, y: int): TCvPoint;
begin
  Result := _CvPoint.CvPoint(x, y);
end;

function CvPoint(const pt: TPoint): TCvPoint;
begin
  Result := CvPoint(pt.x, pt.y);
end;

(* ********************************** CvTermCriteria ************************************ *)
class function _CvTermCriteria.CvTermCriteria(_type: int = 0; _iter: int = 0; _eps: double = 0): _CvTermCriteria;
begin
  With Result do
  begin
    &type := _type;
    max_iter := _iter;
    epsilon := _eps;
  end;
end;

function CvTermCriteria(&type: int; max_iter: int; epsilon: double): TcvTermCriteria;
begin
  Result := _CvTermCriteria.CvTermCriteria(&type, max_iter, epsilon);
end;

function CvTermCriteria(const t: TTermCriteria): TcvTermCriteria;
begin
  Result := CvTermCriteria(t.&type, t.maxCount, t.epsilon);
end;

(* ******************************* CvSize's & CvBox ************************************* *)

function cvSize(const w: int = 0; const h: int = 0): _CvSize;
begin
  Result := _CvSize.cvSize(w, h);
end;

class function _CvSize.cvSize(const w: int = 0; const h: int = 0): _CvSize;
begin
  Result.width := w;
  Result.height := h;
end;

function cvPointFrom32f(const point: TCvPoint2D32f): TCvPoint;
begin
  Result := CvPoint(cvRound(point.x), cvRound(point.y));
end;

{$IFEND}


{$ENDIF IMPLEMENTATION}

// ============================================================================

{$IFDEF INITIALIZATION}


{$ENDIF IINITIALIZATION}
