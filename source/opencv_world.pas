unit opencv_world;

{$I opencv_delphi.inc}

interface

Uses
  OpenCV.Import,
  opencv_delphi;

const
  INT_MAX = MaxInt;

  // ---------------------- Interface.h ----------------------
const
  CV_CN_MAX    = 512;
  CV_CN_SHIFT  = 3;
  CV_DEPTH_MAX = (1 shl CV_CN_SHIFT);

  CV_8U  = 0;
  CV_8S  = 1;
  CV_16U = 2;
  CV_16S = 3;
  CV_32S = 4;
  CV_32F = 5;
  CV_64F = 6;
  CV_16F = 7;

  // ---------------------- cvdef.h ----------------------
const
  (* ***************************************************************************************\
    *                                  Matrix type (Mat)                                     *
    \*************************************************************************************** *)

  CV_MAT_CN_MASK = ((CV_CN_MAX - 1) shl CV_CN_SHIFT);
  // CV_MAT_CN(flags)       = ((((flags) & CV_MAT_CN_MASK) >> CV_CN_SHIFT) + 1);
  CV_MAT_TYPE_MASK = (CV_DEPTH_MAX * CV_CN_MAX - 1);
  // CV_MAT_TYPE(flags)     = ((flags) & CV_MAT_TYPE_MASK);
  CV_MAT_CONT_FLAG_SHIFT = 14;
  CV_MAT_CONT_FLAG       = (1 shl CV_MAT_CONT_FLAG_SHIFT);
  // CV_IS_MAT_CONT(flags)  = ((flags) & CV_MAT_CONT_FLAG);
  CV_IS_CONT_MAT       = CV_MAT_CONT_FLAG; // CV_IS_MAT_CONT;
  CV_SUBMAT_FLAG_SHIFT = 15;
  CV_SUBMAT_FLAG       = (1 shl CV_SUBMAT_FLAG_SHIFT);
  // CV_IS_SUBMAT(flags)    = ((flags) & CV_MAT_SUBMAT_FLAG);

  OPENCV_ABI_COMPATIBILITY = 400;

Type

  TMatSize = record
    // explicit MatSize(int* _p) CV_NOEXCEPT;
    // int dims() const CV_NOEXCEPT;
    // Size operator()() const;
    // const int& operator[](int i) const;
    // int& operator[](int i);
    // operator const int*() const CV_NOEXCEPT;  // TODO OpenCV 4.0: drop this
    // bool operator == (const MatSize& sz) const CV_NOEXCEPT;
    // bool operator != (const MatSize& sz) const CV_NOEXCEPT;
    // int* p;
    p: pInt;
  end;

  TMatStep = record
    // MatStep() CV_NOEXCEPT;
    // explicit MatStep(size_t s) CV_NOEXCEPT;
    // const size_t& operator[](int i) const CV_NOEXCEPT;
    // size_t& operator[](int i) CV_NOEXCEPT;
    // operator size_t() const;
    // MatStep& operator = (size_t s);
    p: psize_t;                    // size_t* p;
    buf: array [0 .. 1] of size_t; // size_t buf[2];
  end;

  pMat = ^TMat;

  TMat = record // 96 bytes, v4.5.4
  public
    // default constructor
    class operator Initialize(out Dest: TMat); // Mat();
    // constructors
    // Mat(Size size, int type);
    // Mat(int rows, int cols, int type, const Scalar& s);
    // Mat(Size size, int type, const Scalar& s);
    // Mat(int ndims, const int* sizes, int type);
    // Mat(const std::vector<int>& sizes, int type);
    // Mat(int ndims, const int* sizes, int type, const Scalar& s);
    // Mat(const std::vector<int>& sizes, int type, const Scalar& s);
    // Mat(const Mat& m);
    // Mat(int rows, int cols, int type, void* data, size_t step=AUTO_STEP);
    // Mat(Size size, int type, void* data, size_t step=AUTO_STEP);
    // Mat(int ndims, const int* sizes, int type, void* data, const size_t* steps=0);
    // Mat(const std::vector<int>& sizes, int type, void* data, const size_t* steps=0);
    // Mat(const Mat& m, const Range& rowRange, const Range& colRange=Range::all());
    // Mat(const Mat& m, const Rect& roi);
    // Mat(const Mat& m, const Range* ranges);
    // Mat(const Mat& m, const std::vector<Range>& ranges);

    // default destructor
    class operator Finalize(var Dest: TMat); // ~Mat();
    class operator Assign(var Dest: TMat; const [ref] Src: TMat);

    // Mat& operator = (const Mat& m);
    // Mat& operator = (const MatExpr& expr);
    // UMat getUMat(AccessFlag accessFlags, UMatUsageFlags usageFlags = USAGE_DEFAULT) const;
    // Mat row(int y) const;
    // Mat col(int x) const;
    // Mat rowRange(int startrow, int endrow) const;
    // Mat rowRange(const Range& r) const;
    // Mat colRange(int startcol, int endcol) const;
    // Mat colRange(const Range& r) const;
    function diag(d: Int = 0): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF} // Mat diag(int d=0) const;
    // CV_NODISCARD_STD static Mat diag(const Mat& d);
    // procedure clone(R: pCVMatPointer); // CV_NODISCARD_STD Mat clone() const;
    function clone: TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // void copyTo( OutputArray m ) const;
    // void copyTo( OutputArray m, InputArray mask ) const;
    // void convertTo( OutputArray m, int rtype, double alpha=1, double beta=0 ) const;
    // void assignTo( Mat& m, int type=-1 ) const;
    // Mat& operator = (const Scalar& s);
    // Mat& setTo(InputArray value, InputArray mask=noArray());
    // Mat reshape(int cn, int rows=0) const;
    // Mat reshape(int cn, int newndims, const int* newsz) const;
    // Mat reshape(int cn, const std::vector<int>& newshape) const;
    // MatExpr t() const;
    // MatExpr inv(int method=DECOMP_LU) const;
    // MatExpr mul(InputArray m, double scale=1) const;
    // Mat cross(InputArray m) const;
    // double dot(InputArray m) const;
    // CV_NODISCARD_STD static MatExpr zeros(int rows, int cols, int type);
    // CV_NODISCARD_STD static MatExpr zeros(Size size, int type);
    // CV_NODISCARD_STD static MatExpr zeros(int ndims, const int* sz, int type);
    // CV_NODISCARD_STD static MatExpr ones(int rows, int cols, int type);
    // CV_NODISCARD_STD static MatExpr ones(Size size, int type);
    // CV_NODISCARD_STD static MatExpr ones(int ndims, const int* sz, int type);
    // CV_NODISCARD_STD static MatExpr eye(int rows, int cols, int type);
    // CV_NODISCARD_STD static MatExpr eye(Size size, int type);
    procedure create(rows, cols, &type: Int); {$IFDEF USE_INLINE}inline; {$ENDIF} // void create(int rows, int cols, int type);
    // void create(Size size, int type);
    // void create(int ndims, const int* sizes, int type);
    // void create(const std::vector<int>& sizes, int type);
    // void addref();
    // void release();
    // // ! internal use function, consider to use 'release' method instead; deallocates the matrix data
    // void deallocate();
    // // ! internal use function; properly re-allocates _size, _step arrays
    // void copySize(const Mat& m);
    // void reserve(size_t sz);
    // void reserveBuffer(size_t sz);
    // void resize(size_t sz);
    // void resize(size_t sz, const Scalar& s);
    // //! internal function
    // void push_back_(const void* elem);
    // void push_back(const Mat& m);
    // void pop_back(size_t nelems=1);
    // void locateROI( Size& wholeSize, Point& ofs ) const;
    // Mat& adjustROI( int dtop, int dbottom, int dleft, int dright );
    // Mat operator()( Range rowRange, Range colRange ) const;
    // Mat operator()( const Rect& roi ) const;
    // Mat operator()( const Range* ranges ) const;
    // Mat operator()(const std::vector<Range>& ranges) const;
    function isContinuous: Bool; {$IFDEF USE_INLINE}inline; {$ENDIF} // bool isContinuous() const;
    // //! returns true if the matrix is a submatrix of another matrix
    function isSubmatrix: Bool; {$IFDEF USE_INLINE}inline; {$ENDIF}         // bool isSubmatrix() const;
    function elemSize: size_t; {$IFDEF USE_INLINE}inline; {$ENDIF}          // size_t elemSize() const;
    function elemSize1: size_t; {$IFDEF USE_INLINE}inline; {$ENDIF}         // size_t elemSize1() const;
    function &type: Int; {$IFDEF USE_INLINE}inline; {$ENDIF}                // int type() const;
    function depth: Int; {$IFDEF USE_INLINE}inline; {$ENDIF}                // int depth() const;
    function channels: Int; {$IFDEF USE_INLINE}inline; {$ENDIF}             // int channels() const;
    function step1(i: Int = 0): size_t; {$IFDEF USE_INLINE}inline; {$ENDIF} // size_t step1(int i=0) const;
    function empty: Bool; {$IFDEF USE_INLINE}inline; {$ENDIF}               // bool empty() const;
    function total: size_t; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}   // size_t total() const;
    function total(startDim: Int; endDim: Int = INT_MAX): size_t; overload; {$IFDEF USE_INLINE}inline; {$ENDIF} // size_t total(int startDim, int endDim=INT_MAX) const;
    function checkVector(elemChannels: Int; depth: Int = -1; requireContinuous: Bool = true): Int; // int checkVector(int elemChannels, int depth=-1, bool requireContinuous=true) const;
    // uchar* ptr(int i0=0);
    // const uchar* ptr(int i0=0) const;
    // uchar* ptr(int row, int col);
    // const uchar* ptr(int row, int col) const;
    // uchar* ptr(int i0, int i1, int i2);
    // const uchar* ptr(int i0, int i1, int i2) const;
    // uchar* ptr(const int* idx);
    // const uchar* ptr(const int* idx) const;
    // Mat(Mat&& m);
    // Mat& operator = (Mat&& m);
  public const
    MAGIC_VAL       = $42FF0000;
    AUTO_STEP       = 0;
    CONTINUOUS_FLAG = CV_MAT_CONT_FLAG;
    SUBMATRIX_FLAG  = CV_SUBMAT_FLAG;

    MAGIC_MASK = $FFFF0000;
    TYPE_MASK  = $00000FFF;
    DEPTH_MASK = 7;
  public
    flags: Int; // int flags;
    // //! the matrix dimensionality, >= 2
    dims: Int; // int dims;
    // //! the number of rows and columns or (-1, -1) when the matrix has more than 2 dimensions
    rows, cols: Int; // int rows, cols;
    // //! pointer to the data
    data: pUChar; // uchar* data;

    // //! helper fields used in locateROI and adjustROI
    datastart: pUChar; // const uchar* datastart;
    dataend: pUChar;   // const uchar* dataend;
    datalimit: pUChar; // const uchar* datalimit;

    // //! custom allocator
    allocator: pMatAllocator; // MatAllocator* allocator;
    // //! and the standard allocator
    // static MatAllocator* getStdAllocator();
    // static MatAllocator* getDefaultAllocator();
    // static void setDefaultAllocator(MatAllocator* allocator);

    // //! internal use method: updates the continuity flag
    // void updateContinuityFlag();

    // //! interaction with UMat
    u: pUMatData; // UMatData* u;

    size: TMatSize; // MatSize size;
    step: TMatStep; // MatStep step;
  end;

  // Mat = TMat;

  TSize_<T> = record
    // //! default constructor
    // Size_();
    // Size_(_Tp _width, _Tp _height);
    // #if OPENCV_ABI_COMPATIBILITY < 500
    // Size_(const Size_& sz) = default;
    // Size_(Size_&& sz) CV_NOEXCEPT = default;
    // #endif
    // Size_(const Point_<_Tp>& pt);
    //
    // #if OPENCV_ABI_COMPATIBILITY < 500
    // Size_& operator = (const Size_& sz) = default;
    // Size_& operator = (Size_&& sz) CV_NOEXCEPT = default;
    // #endif
    // //! the area (width*height)
    // _Tp area() const;
    // //! aspect ratio (width/height)
    // double aspectRatio() const;
    // //! true if empty
    // bool empty() const;
    //
    // //! conversion of another data type.
    // template<typename _Tp2> operator Size_<_Tp2>() const;

    width: T;  // _Tp width;  // !< the width
    height: T; // _Tp height; // !< the height
  end;

  Size2i = TSize_<Int>;
  size = Size2i;

  TScalar = record
  private
{$HINTS OFF}
    V: array [0 .. 3] of double;
{$HINTS ON}
  public
    class operator Initialize(out Dest: TScalar); // Scalar_();
    class function create(const v0, v1, v2, v3: double): TScalar; overload; static; // Scalar_(_Tp v0, _Tp v1, _Tp v2=0, _Tp v3=0);
    class function create(const v0: double): TScalar; overload; static; // Scalar_(_Tp v0);
    // Scalar_(const Scalar_& s);
    // Scalar_(Scalar_&& s) CV_NOEXCEPT;
    // Scalar_& operator=(const Scalar_& s);
    // Scalar_& operator=(Scalar_&& s) CV_NOEXCEPT;
    // Scalar_(const Vec<_Tp2, cn>& v);
    // //! returns a scalar with all elements set to v0
    // static Scalar_<_Tp> all(_Tp v0);
    // //! per-element product
    // Scalar_<_Tp> mul(const Scalar_<_Tp>& a, double scale=1 ) const;
    // //! returns (v0, -v1, -v2, -v3)
    // Scalar_<_Tp> conj() const;
    // //! returns true iff v1 == v2 == v3 == 0
    // bool isReal() const;
    class operator Implicit(const v0: double): TScalar;
  end;

  pInputArray = ^TInputArray;

  TInputArray = record
  public type
    KindFlag = (                         //
      KIND_SHIFT = 16,                   //
      FIXED_TYPE = $8000 shl KIND_SHIFT, //
      FIXED_SIZE = $4000 shl KIND_SHIFT, //
      KIND_MASK = 31 shl KIND_SHIFT,     //

      NONE = 0 shl KIND_SHIFT,              //
      Mat = 1 shl KIND_SHIFT,               //
      MATX = 2 shl KIND_SHIFT,              //
      STD_VECTOR = 3 shl KIND_SHIFT,        //
      STD_VECTOR_VECTOR = 4 shl KIND_SHIFT, //
      STD_VECTOR_MAT = 5 shl KIND_SHIFT,    //
{$IF OPENCV_ABI_COMPATIBILITY < 500}
      EXPR = 6 shl KIND_SHIFT, // !< removed: https://github.com/opencv/opencv/pull/17046
{$IFEND}
      OPENGL_BUFFER = 7 shl KIND_SHIFT,            //
      CUDA_HOST_MEM = 8 shl KIND_SHIFT,            //
      CUDA_GPU_MAT = 9 shl KIND_SHIFT,             //
      UMAT = 10 shl KIND_SHIFT,                    //
      STD_VECTOR_UMAT = 11 shl KIND_SHIFT,         //
      STD_BOOL_VECTOR = 12 shl KIND_SHIFT,         //
      STD_VECTOR_CUDA_GPU_MAT = 13 shl KIND_SHIFT, //
{$IF OPENCV_ABI_COMPATIBILITY < 500}
      STD_ARRAY = 14 shl KIND_SHIFT, // !< removed: https://github.com/opencv/opencv/issues/18897
{$IFEND}
      STD_ARRAY_MAT = 15 shl KIND_SHIFT);
  public
    class operator Initialize(out Dest: TInputArray); // _InputArray();
    // _InputArray(int _flags, void* _obj);
    procedure InputArray(const m: TMat); {$IFDEF USE_INLINE}inline; {$ENDIF} // _InputArray(const Mat& m);
    // _InputArray(const MatExpr& expr);
    // _InputArray(const std::vector<Mat>& vec);
    // _InputArray(const std::vector<bool>& vec);
    // _InputArray(const std::vector<std::vector<bool> >&) = delete;  // not supported
    // _InputArray(const double& val);
    // _InputArray(const cuda::GpuMat& d_mat);
    // _InputArray(const std::vector<cuda::GpuMat>& d_mat_array);
    // _InputArray(const ogl::Buffer& buf);
    // _InputArray(const cuda::HostMem& cuda_mem);
    // _InputArray(const UMat& um);
    // _InputArray(const std::vector<UMat>& umv);
    //
    // Mat getMat(int idx=-1) const;
    // Mat getMat_(int idx=-1) const;
    // UMat getUMat(int idx=-1) const;
    // void getMatVector(std::vector<Mat>& mv) const;
    // void getUMatVector(std::vector<UMat>& umv) const;
    // void getGpuMatVector(std::vector<cuda::GpuMat>& gpumv) const;
    // cuda::GpuMat getGpuMat() const;
    // ogl::Buffer getOGlBuffer() const;
    //
    // int getFlags() const;
    function getObj: Pointer; {$IFDEF USE_INLINE}inline; {$ENDIF} // void* getObj() const;
    // Size getSz() const;
    //
    // _InputArray::KindFlag kind() const;
    // int dims(int i=-1) const;
    // int cols(int i=-1) const;
    // int rows(int i=-1) const;
    // Size size(int i=-1) const;
    // int sizend(int* sz, int i=-1) const;
    // bool sameSize(const _InputArray& arr) const;
    // size_t total(int i=-1) const;
    // int type(int i=-1) const;
    // int depth(int i=-1) const;
    // int channels(int i=-1) const;
    // bool isContinuous(int i=-1) const;
    // bool isSubmatrix(int i=-1) const;
    // bool empty() const;
    // void copyTo(const _OutputArray& arr) const;
    // void copyTo(const _OutputArray& arr, const _InputArray & mask) const;
    // size_t offset(int i=-1) const;
    // size_t step(int i=-1) const;
    function isMat: Bool; {$IFDEF USE_INLINE}inline; {$ENDIF} // bool isMat() const;
    // bool isUMat() const;
    // bool isMatVector() const;
    // bool isUMatVector() const;
    // bool isMatx() const;
    // bool isVector() const;
    // bool isGpuMat() const;
    // bool isGpuMatVector() const;
    class operator Finalize(var Dest: TInputArray); // ~_InputArray();

    class operator Implicit(const m: TMat): TInputArray;
    class operator Implicit(const IA: TInputArray): TMat;

  private
{$HINTS OFF}
    flags: Int;   // int flags;
    obj: Pointer; // void* obj;
    sz: size;     // Size sz;
{$HINTS ON}
  end;

  InputArray = ^TInputArray;

  TOutputArray = record
  private
    InputArray: TInputArray;
  public type
    DepthMask = (                                  //
      DEPTH_MASK_8U = 1 shl CV_8U,                 //
      DEPTH_MASK_8S = 1 shl CV_8S,                 //
      DEPTH_MASK_16U = 1 shl CV_16U,               //
      DEPTH_MASK_16S = 1 shl CV_16S,               //
      DEPTH_MASK_32S = 1 shl CV_32S,               //
      DEPTH_MASK_32F = 1 shl CV_32F,               //
      DEPTH_MASK_64F = 1 shl CV_64F,               //
      DEPTH_MASK_16F = 1 shl CV_16F,               //
      DEPTH_MASK_ALL = (DEPTH_MASK_64F shl 1) - 1, //
      DEPTH_MASK_ALL_BUT_8S = DEPTH_MASK_ALL and (not DEPTH_MASK_8S), //
      DEPTH_MASK_ALL_16F = (DEPTH_MASK_16F shl 1) - 1, //
      DEPTH_MASK_FLT = DEPTH_MASK_32F + DEPTH_MASK_64F);
  public
    class operator Initialize(out Dest: TOutputArray); // _OutputArray();
    // _OutputArray(int _flags, void* _obj);
    procedure OutputArray(const m: TMat); {$IFDEF USE_INLINE}inline; {$ENDIF} // _OutputArray(Mat& m);
    // _OutputArray(std::vector<Mat>& vec);
    // _OutputArray(cuda::GpuMat& d_mat);
    // _OutputArray(std::vector<cuda::GpuMat>& d_mat);
    // _OutputArray(ogl::Buffer& buf);
    // _OutputArray(cuda::HostMem& cuda_mem);
    // _OutputArray(std::vector<bool>& vec) = delete;  // not supported
    // _OutputArray(std::vector<std::vector<bool> >&) = delete;  // not supported
    // _OutputArray(UMat& m);
    // _OutputArray(std::vector<UMat>& vec);

    // bool fixedSize() const;
    // bool fixedType() const;
    // bool needed() const;
    // Mat& getMatRef(int i=-1) const;
    // UMat& getUMatRef(int i=-1) const;
    // cuda::GpuMat& getGpuMatRef() const;
    // std::vector<cuda::GpuMat>& getGpuMatVecRef() const;
    // ogl::Buffer& getOGlBufferRef() const;
    // cuda::HostMem& getHostMemRef() const;
    // void create(Size sz, int type, int i=-1, bool allowTransposed=false, _OutputArray::DepthMask fixedDepthMask=static_cast<_OutputArray::DepthMask>(0)) const;
    // void create(int rows, int cols, int type, int i=-1, bool allowTransposed=false, _OutputArray::DepthMask fixedDepthMask=static_cast<_OutputArray::DepthMask>(0)) const;
    // void create(int dims, const int* size, int type, int i=-1, bool allowTransposed=false, _OutputArray::DepthMask fixedDepthMask=static_cast<_OutputArray::DepthMask>(0)) const;
    // void createSameSize(const _InputArray& arr, int mtype) const;
    // void release() const;
    // void clear() const;
    // void setTo(const _InputArray& value, const _InputArray & mask = _InputArray()) const;
    //
    // void assign(const UMat& u) const;
    // void assign(const Mat& m) const;
    //
    // void assign(const std::vector<UMat>& v) const;
    // void assign(const std::vector<Mat>& v) const;
    //
    // void move(UMat& u) const;
    // void move(Mat& m) const;
    class operator Finalize(var Dest: TOutputArray); // ~_InputArray();

    class operator Implicit(const m: TMat): TOutputArray;
    class operator Implicit(const OA: TOutputArray): TMat;
  end;

  // ---------------------- core.hpp ----------------------
  (* * @brief Swaps two matrices *)
  // CV_EXPORTS void swap(Mat& a, Mat& b);
  // ?swap@cv@@YAXAEAVMat@1@0@Z
  // void cv::swap(class cv::Mat &,class cv::Mat &)
procedure swap(Var a, b: TMat); external opencv_world_dll name '?swap@cv@@YAXAEAVMat@1@0@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// CV_EXPORTS void swap( UMat& a, UMat& b );

(* * @brief Computes the source location of an extrapolated pixel.

  The function computes and returns the coordinate of a donor pixel corresponding to the specified
  extrapolated pixel when using the specified extrapolation border mode. For example, if you use
  cv::BORDER_WRAP mode in the horizontal direction, cv::BORDER_REFLECT_101 in the vertical direction and
  want to compute value of the "virtual" pixel Point(-5, 100) in a floating-point image img , it
  looks like:
  @code{.cpp}
  float val = img.at<float>(borderInterpolate(100, img.rows, cv::BORDER_REFLECT_101),
  borderInterpolate(-5, img.cols, cv::BORDER_WRAP));
  @endcode
  Normally, the function is not called directly. It is used inside filtering functions and also in
  copyMakeBorder.
  @param p 0-based coordinate of the extrapolated pixel along one of the axes, likely \<0 or \>= len
  @param len Length of the array along the corresponding axis.
  @param borderType Border type, one of the #BorderTypes, except for #BORDER_TRANSPARENT and
  #BORDER_ISOLATED . When borderType==#BORDER_CONSTANT , the function always returns -1, regardless
  of p and len.

  @sa copyMakeBorder
*)
// CV_EXPORTS_W int borderInterpolate(int p, int len, int borderType);
// ?borderInterpolate@cv@@YAHHHH@Z
function borderInterpolate(p, Len, borderType: Int): Int; external opencv_world_dll name '?borderInterpolate@cv@@YAHHHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

(* * @example samples/cpp/tutorial_code/ImgTrans/copyMakeBorder_demo.cpp
  An example using copyMakeBorder function.
  Check @ref tutorial_copyMakeBorder "the corresponding tutorial" for more details
*)

(* * @brief Forms a border around an image.

  The function copies the source image into the middle of the destination image. The areas to the
  left, to the right, above and below the copied source image will be filled with extrapolated
  pixels. This is not what filtering functions based on it do (they extrapolate pixels on-fly), but
  what other more complex functions, including your own, may do to simplify image boundary handling.

  The function supports the mode when src is already in the middle of dst . In this case, the
  function does not copy src itself but simply constructs the border, for example:

  @code{.cpp}
  // let border be the same in all directions
  int border=2;
  // constructs a larger image to fit both the image and the border
  Mat gray_buf(rgb.rows + border*2, rgb.cols + border*2, rgb.depth());
  // select the middle part of it w/o copying data
  Mat gray(gray_canvas, Rect(border, border, rgb.cols, rgb.rows));
  // convert image from RGB to grayscale
  cvtColor(rgb, gray, COLOR_RGB2GRAY);
  // form a border in-place
  copyMakeBorder(gray, gray_buf, border, border,
  border, border, BORDER_REPLICATE);
  // now do some custom filtering ...
  ...
  @endcode
  @note When the source image is a part (ROI) of a bigger image, the function will try to use the
  pixels outside of the ROI to form a border. To disable this feature and always do extrapolation, as
  if src was not a ROI, use borderType | #BORDER_ISOLATED.

  @param src Source image.
  @param dst Destination image of the same type as src and the size Size(src.cols+left+right,
  src.rows+top+bottom) .
  @param top the top pixels
  @param bottom the bottom pixels
  @param left the left pixels
  @param right Parameter specifying how many pixels in each direction from the source image rectangle
  to extrapolate. For example, top=1, bottom=1, left=1, right=1 mean that 1 pixel-wide border needs
  to be built.
  @param borderType Border type. See borderInterpolate for details.
  @param value Border value if borderType==BORDER_CONSTANT .

  @sa  borderInterpolate
*)

// CV_EXPORTS_W void copyMakeBorder(InputArray src, OutputArray dst,
// int top, int bottom, int left, int right,
// int borderType, const Scalar& value = Scalar() );
// ?copyMakeBorder@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHHHHAEBV?$Scalar_@N@1@@Z
// void cv::copyMakeBorder(class cv::_InputArray const &,class cv::_OutputArray const &,int,int,int,int,int,class cv::Scalar_<double> const &)
procedure copyMakeBorder(const Src: TInputArray; Var dst: TOutputArray; top, bottom, left, right, borderType: Int; const Scalar: TScalar); overload;
  external opencv_world_dll name '?copyMakeBorder@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHHHHAEBV?$Scalar_@N@1@@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
procedure copyMakeBorder(const Src: TInputArray; Var dst: TOutputArray; top, bottom, left, right, borderType: Int); {$IFDEF USE_INLINE}inline; {$ENDIF} overload;

(* * @brief Calculates the weighted sum of two arrays.

  The function addWeighted calculates the weighted sum of two arrays as follows:
  \f[\texttt{dst} (I)= \texttt{saturate} ( \texttt{src1} (I)* \texttt{alpha} +  \texttt{src2} (I)* \texttt{beta} +  \texttt{gamma} )\f]
  where I is a multi-dimensional index of array elements. In case of multi-channel arrays, each
  channel is processed independently.
  The function can be replaced with a matrix expression:
  @code{.cpp}
  dst = src1*alpha + src2*beta + gamma;
  @endcode
  @note Saturation is not applied when the output array has the depth CV_32S. You may even get
  result of an incorrect sign in the case of overflow.
  @param src1 first input array.
  @param alpha weight of the first array elements.
  @param src2 second input array of the same size and channel number as src1.
  @param beta weight of the second array elements.
  @param gamma scalar added to each sum.
  @param dst output array that has the same size and number of channels as the input arrays.
  @param dtype optional depth of the output array; when both input arrays have the same depth, dtype
  can be set to -1, which will be equivalent to src1.depth().
  @sa  add, subtract, scaleAdd, Mat::convertTo
*)
// CV_EXPORTS_W void addWeighted(InputArray src1, double alpha, InputArray src2,
// double beta, double gamma, OutputArray dst, int dtype = -1);
// ?addWeighted@cv@@YAXAEBV_InputArray@1@N0NNAEBV_OutputArray@1@H@Z
// void cv::addWeighted(class cv::_InputArray const &,double,class cv::_InputArray const &,double,double,class cv::_OutputArray const &,int)
procedure addWeighted(src1: TInputArray; alpha: double; src2: TInputArray; beta, gamma: double; dst: TOutputArray; dtype: Int = -1);
  external opencv_world_dll { name '?addWeighted@cv@@YAXAEBV_InputArray@1@N0NNAEBV_OutputArray@1@H@Z' } index 3519 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};


// ---------------------- end core.hpp ----------------------
// ---------------------- imgcodecs.hpp ----------------------

Type
  ImreadModes = (                    //
    IMREAD_UNCHANGED = -1,           // !< If set, return the loaded image as is (with alpha channel, otherwise it gets cropped). Ignore EXIF orientation.
    IMREAD_GRAYSCALE = 0,            // !< If set, always convert image to the single channel grayscale image (codec internal conversion).
    IMREAD_COLOR = 1,                // !< If set, always convert image to the 3 channel BGR color image.
    IMREAD_ANYDEPTH = 2,             // !< If set, return 16-bit/32-bit image when the input has the corresponding depth, otherwise convert it to 8-bit.
    IMREAD_ANYCOLOR = 4,             // !< If set, the image is read in any possible color format.
    IMREAD_LOAD_GDAL = 8,            // !< If set, use the gdal driver for loading the image.
    IMREAD_REDUCED_GRAYSCALE_2 = 16, // !< If set, always convert image to the single channel grayscale image and the image size reduced 1/2.
    IMREAD_REDUCED_COLOR_2 = 17,     // !< If set, always convert image to the 3 channel BGR color image and the image size reduced 1/2.
    IMREAD_REDUCED_GRAYSCALE_4 = 32, // !< If set, always convert image to the single channel grayscale image and the image size reduced 1/4.
    IMREAD_REDUCED_COLOR_4 = 33,     // !< If set, always convert image to the 3 channel BGR color image and the image size reduced 1/4.
    IMREAD_REDUCED_GRAYSCALE_8 = 64, // !< If set, always convert image to the single channel grayscale image and the image size reduced 1/8.
    IMREAD_REDUCED_COLOR_8 = 65,     // !< If set, always convert image to the 3 channel BGR color image and the image size reduced 1/8.
    IMREAD_IGNORE_ORIENTATION = 128  // !< If set, do not rotate the image according to EXIF's orientation flag.
    );

  (* * @brief Loads an image from a file.

    @anchor imread

    The function imread loads an image from the specified file and returns it. If the image cannot be
    read (because of missing file, improper permissions, unsupported or invalid format), the function
    returns an empty matrix ( Mat::data==NULL ).

    Currently, the following file formats are supported:

    -   Windows bitmaps - \*.bmp, \*.dib (always supported)
    -   JPEG files - \*.jpeg, \*.jpg, \*.jpe (see the *Note* section)
    -   JPEG 2000 files - \*.jp2 (see the *Note* section)
    -   Portable Network Graphics - \*.png (see the *Note* section)
    -   WebP - \*.webp (see the *Note* section)
    -   Portable image format - \*.pbm, \*.pgm, \*.ppm \*.pxm, \*.pnm (always supported)
    -   PFM files - \*.pfm (see the *Note* section)
    -   Sun rasters - \*.sr, \*.ras (always supported)
    -   TIFF files - \*.tiff, \*.tif (see the *Note* section)
    -   OpenEXR Image files - \*.exr (see the *Note* section)
    -   Radiance HDR - \*.hdr, \*.pic (always supported)
    -   Raster and Vector geospatial data supported by GDAL (see the *Note* section)

    @note
    -   The function determines the type of an image by the content, not by the file extension.
    -   In the case of color images, the decoded images will have the channels stored in **B G R** order.
    -   When using IMREAD_GRAYSCALE, the codec's internal grayscale conversion will be used, if available.
    Results may differ to the output of cvtColor()
    -   On Microsoft Windows\* OS and MacOSX\*, the codecs shipped with an OpenCV image (libjpeg,
    libpng, libtiff, and libjasper) are used by default. So, OpenCV can always read JPEGs, PNGs,
    and TIFFs. On MacOSX, there is also an option to use native MacOSX image readers. But beware
    that currently these native image loaders give images with different pixel values because of
    the color management embedded into MacOSX.
    -   On Linux\*, BSD flavors and other Unix-like open-source operating systems, OpenCV looks for
    codecs supplied with an OS image. Install the relevant packages (do not forget the development
    files, for example, "libjpeg-dev", in Debian\* and Ubuntu\* ) to get the codec support or turn
    on the OPENCV_BUILD_3RDPARTY_LIBS flag in CMake.
    -   In the case you set *WITH_GDAL* flag to true in CMake and @ref IMREAD_LOAD_GDAL to load the image,
    then the [GDAL](http://www.gdal.org) driver will be used in order to decode the image, supporting
    the following formats: [Raster](http://www.gdal.org/formats_list.html),
    [Vector](http://www.gdal.org/ogr_formats.html).
    -   If EXIF information is embedded in the image file, the EXIF orientation will be taken into account
    and thus the image will be rotated accordingly except if the flags @ref IMREAD_IGNORE_ORIENTATION
    or @ref IMREAD_UNCHANGED are passed.
    -   Use the IMREAD_UNCHANGED flag to keep the floating point values from PFM image.
    -   By default number of pixels must be less than 2^30. Limit can be set using system
    variable OPENCV_IO_MAX_IMAGE_PIXELS

    @param filename Name of file to be loaded.
    @param flags Flag that can take values of cv::ImreadModes
  *)
  // CV_EXPORTS_W Mat imread(const String & filename, Int flags = IMREAD_COLOR);
  // ?imread@cv@@YA?AVMat@1@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@H@Z
  // class cv::Mat cv::imread(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int)
function imread(const filename: CppString; flag: ImreadModes = IMREAD_COLOR): TMat;
  external opencv_world_dll name '?imread@cv@@YA?AVMat@1@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@H@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

(* * @brief Destroys the specified window.
  The function destroyWindow destroys the window with the given name.
  @param winname Name of the window to be destroyed.
*)
// CV_EXPORTS_W void destroyWindow(const String& winname);

(* * @brief Destroys all of the HighGUI windows.
  The function destroyAllWindows destroys all of the opened HighGUI windows.
*)
// CV_EXPORTS_W void destroyAllWindows();

// CV_EXPORTS_W int startWindowThread();

(* * @brief Similar to #waitKey, but returns full key code.
  @note
  Key code is implementation specific and depends on used backend: QT/GTK/Win32/etc
*)
// CV_EXPORTS_W int waitKeyEx(int delay = 0);

(* * @brief Waits for a pressed key.

  The function waitKey waits for a key event infinitely (when \f$\texttt{delay}\leq 0\f$ ) or for delay
  milliseconds, when it is positive. Since the OS has a minimum time between switching threads, the
  function will not wait exactly delay ms, it will wait at least delay ms, depending on what else is
  running on your computer at that time. It returns the code of the pressed key or -1 if no key was
  pressed before the specified time had elapsed. To check for a key press but not wait for it, use
  #pollKey.

  @note The functions #waitKey and #pollKey are the only methods in HighGUI that can fetch and handle
  GUI events, so one of them needs to be called periodically for normal event processing unless
  HighGUI is used within an environment that takes care of event processing.

  @note The function only works if there is at least one HighGUI window created and the window is
  active. If there are several HighGUI windows, any of them can be active.

  @param delay Delay in milliseconds. 0 is the special value that means "forever".
*)
// CV_EXPORTS_W int waitKey(int delay = 0);
// ?waitKey@cv@@YAHH@Z
// int cv::waitKey(int)
function waitKey(delay: Int = 0): Int; external opencv_world_dll name '?waitKey@cv@@YAHH@Z' {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

(* * @brief Polls for a pressed key.

  The function pollKey polls for a key event without waiting. It returns the code of the pressed key
  or -1 if no key was pressed since the last invocation. To wait until a key was pressed, use #waitKey.

  @note The functions #waitKey and #pollKey are the only methods in HighGUI that can fetch and handle
  GUI events, so one of them needs to be called periodically for normal event processing unless
  HighGUI is used within an environment that takes care of event processing.

  @note The function only works if there is at least one HighGUI window created and the window is
  active. If there are several HighGUI windows, any of them can be active.
*)
// CV_EXPORTS_W int pollKey();

(* * @brief Displays an image in the specified window.

  The function imshow displays an image in the specified window. If the window was created with the
  cv::WINDOW_AUTOSIZE flag, the image is shown with its original size, however it is still limited by the screen resolution.
  Otherwise, the image is scaled to fit the window. The function may scale the image, depending on its depth:

  -   If the image is 8-bit unsigned, it is displayed as is.
  -   If the image is 16-bit unsigned, the pixels are divided by 256. That is, the
  value range [0,255\*256] is mapped to [0,255].
  -   If the image is 32-bit or 64-bit floating-point, the pixel values are multiplied by 255. That is, the
  value range [0,1] is mapped to [0,255].
  -   32-bit integer images are not processed anymore due to ambiguouty of required transform.
  Convert to 8-bit unsigned matrix using a custom preprocessing specific to image's context.

  If window was created with OpenGL support, cv::imshow also support ogl::Buffer , ogl::Texture2D and
  cuda::GpuMat as input.

  If the window was not created before this function, it is assumed creating a window with cv::WINDOW_AUTOSIZE.

  If you need to show an image that is bigger than the screen resolution, you will need to call namedWindow("", WINDOW_NORMAL) before the imshow.

  @note This function should be followed by a call to cv::waitKey or cv::pollKey to perform GUI
  housekeeping tasks that are necessary to actually show the given image and make the window respond
  to mouse and keyboard events. Otherwise, it won't display the image and the window might lock up.
  For example, **waitKey(0)** will display the window infinitely until any keypress (it is suitable
  for image display). **waitKey(25)** will display a frame and wait approximately 25 ms for a key
  press (suitable for displaying a video frame-by-frame). To remove the window, use cv::destroyWindow.

  @note

  [__Windows Backend Only__] Pressing Ctrl+C will copy the image to the clipboard.

  [__Windows Backend Only__] Pressing Ctrl+S will show a dialog to save the image.

  @param winname Name of the window.
  @param mat Image to be shown.
*)
// CV_EXPORTS_W void imshow(const String & winname, InputArray Mat);
// ?imshow@cv@@YAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@1@@Z
// void cv::imshow(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,class cv::_InputArray const &)
procedure imshow(const winname: CppString; Mat: TInputArray); overload; external opencv_world_dll index 5268
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ---------------------- end imgcodecs.hpp ----------------------

implementation

procedure copyMakeBorder(const Src: TInputArray; Var dst: TOutputArray; top, bottom, left, right, borderType: Int); overload;
Var
  Scalar: TScalar;
begin
  copyMakeBorder(Src, dst, top, bottom, left, right, borderType, Scalar);
end;

{ TMat }

class operator TMat.Initialize(out Dest: TMat);
begin
  constructor_Mat(@Dest);
end;

class operator TMat.Finalize(var Dest: TMat);
begin
  destructor_Mat(@Dest);
end;

function TMat.clone: TMat;
begin
  OpenCV.Import.clone(@Self, @Result);
end;

function TMat.diag(d: Int): TMat;
begin
  OpenCV.Import.diag(@Self, @Result, d);
end;

function TMat.isContinuous: Bool;
begin
  Result := OpenCV.Import.isContinuous(@Self);
end;

function TMat.isSubmatrix: Bool;
begin
  Result := OpenCV.Import.isSubmatrix(@Self);
end;

function TMat.step1(i: Int): size_t;
begin
  Result := OpenCV.Import.step1(@Self, i);
end;

class operator TMat.Assign(var Dest: TMat; const [ref] Src: TMat);
begin
  Finalize(Dest);
  OpenCV.Import.clone(@Src, @Dest);
end;

function TMat.channels: Int;
begin
  Result := OpenCV.Import.channels(@Self);
end;

function TMat.checkVector(elemChannels, depth: Int; requireContinuous: Bool): Int;
begin
  Result := OpenCV.Import.checkVector(@Self, elemChannels, depth, requireContinuous);
end;

procedure TMat.create(rows, cols, &type: Int);
begin
  OpenCV.Import.constructor_Mat(@Self, rows, cols, &type);
end;

function TMat.depth: Int;
begin
  Result := OpenCV.Import.depth(@Self);
end;

function TMat.elemSize: size_t;
begin
  Result := OpenCV.Import.elemSize(@Self);
end;

function TMat.elemSize1: size_t;
begin
  Result := OpenCV.Import.elemSize1(@Self);
end;

function TMat.empty: Bool;
begin
  Result := OpenCV.Import.empty(@Self);
end;

function TMat.total(startDim, endDim: Int): size_t;
begin
  Result := OpenCV.Import.total(@Self, startDim, endDim);
end;

function TMat.total: size_t;
begin
  Result := OpenCV.Import.total(@Self);
end;

function TMat.&type: Int;
begin
  Result := OpenCV.Import.type(@Self);
end;

{ TInputArray }

class operator TInputArray.Implicit(const m: TMat): TInputArray;
begin
  Result.InputArray(m);
end;

function TInputArray.getObj: Pointer;
begin
  Result := OpenCV.Import.getObj(@Self);
end;

class operator TInputArray.Implicit(const IA: TInputArray): TMat;
begin
  Assert(IA.isMat);
  Result := pMat(IA.getObj)^.clone;
end;

class operator TInputArray.Initialize(out Dest: TInputArray);
begin
  Constructor_InputArray(@Dest);
end;

class operator TInputArray.Finalize(var Dest: TInputArray);
begin
  Destructor_InputArray(@Dest);
end;

procedure TInputArray.InputArray(const m: TMat);
begin
  Constructor_InputArray(@Self, @m);
end;

function TInputArray.isMat: Bool;
begin
  Result := OpenCV.Import.isMat(@Self);
end;

{ TOutputArray }

class operator TOutputArray.Initialize(out Dest: TOutputArray);
begin
  Constructor_OutputArray(@Dest);
end;

class operator TOutputArray.Finalize(var Dest: TOutputArray);
begin
  Destructor_OutputArray(@Dest);
end;

class operator TOutputArray.Implicit(const m: TMat): TOutputArray;
begin
  Result.OutputArray(m);
end;

class operator TOutputArray.Implicit(const OA: TOutputArray): TMat;
begin
  Assert(OA.InputArray.isMat);
  Result := pMat(OA.InputArray.getObj)^ { .clone };
end;

procedure TOutputArray.OutputArray(const m: TMat);
begin
  Constructor_OutputArray(@Self, @m);
end;

{ TScalar }

class function TScalar.create(const v0, v1, v2, v3: double): TScalar;
begin
  constructor_Scalar(@Result, v0, v1, v2, v3);
end;

class function TScalar.create(const v0: double): TScalar;
begin
  constructor_Scalar(@Result, v0);
end;

class operator TScalar.Implicit(const v0: double): TScalar;
begin
  Result := TScalar.create(v0);
end;

class operator TScalar.Initialize(out Dest: TScalar);
begin
  constructor_Scalar(@Dest);
end;

end.
