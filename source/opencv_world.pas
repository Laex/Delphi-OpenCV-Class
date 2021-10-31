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

  // ---------------------- Start base.hpp ----------------------
type
  // ! Various border types, image boundaries are denoted with `|`
  // ! @see borderInterpolate, copyMakeBorder
  BorderTypes = (           //
    BORDER_CONSTANT = 0,    // !< `iiiiii|abcdefgh|iiiiiii`  with some specified `i`
    BORDER_REPLICATE = 1,   // !< `aaaaaa|abcdefgh|hhhhhhh`
    BORDER_REFLECT = 2,     // !< `fedcba|abcdefgh|hgfedcb`
    BORDER_WRAP = 3,        // !< `cdefgh|abcdefgh|abcdefg`
    BORDER_REFLECT_101 = 4, // !< `gfedcb|abcdefgh|gfedcba`
    BORDER_TRANSPARENT = 5, // !< `uvwxyz|abcdefgh|ijklmno`

    BORDER_REFLECT101 = BORDER_REFLECT_101, // !< same as BORDER_REFLECT_101
    BORDER_DEFAULT = BORDER_REFLECT_101,    // !< same as BORDER_REFLECT_101
    BORDER_ISOLATED = 16                    // !< do not look outside of ROI
    );
  // ---------------------- End base.hpp ----------------------

  // ---------------------- cvdef.h ----------------------
const
  (* ************************************************************************************** *)
  (* Matrix type (Mat) *)
  (* ************************************************************************************** *)

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

  // ---------------------- types.hpp ----------------------
  TSize_<T> = record
  public
    // //! default constructor
    // Size_();
    class function size(const _width, _height: T): TSize_<T>; static; // Size_(_Tp _width, _Tp _height);
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
  public
    width: T;  // _Tp width;  // !< the width
    height: T; // _Tp height; // !< the height
  end;

  TSize2i = TSize_<Int>;
  TSize = TSize2i;

function size(const _width, _height: Int): TSize; {$IFDEF USE_INLINE}inline; {$ENDIF}

type
  TPoint_<T> = record
  public
    // ! default constructor
    // Point_();
    class function Point(_x, _y: T): TPoint_<T>; static; // Point_(_Tp _x, _Tp _y);
    // #if (defined(__GNUC__) && __GNUC__ < 5)  // GCC 4.x bug. Details: https://github.com/opencv/opencv/pull/20837
    // Point_(const Point_& pt);
    // Point_(Point_&& pt) CV_NOEXCEPT = default;
    // #elif OPENCV_ABI_COMPATIBILITY < 500
    // Point_(const Point_& pt) = default;
    // Point_(Point_&& pt) CV_NOEXCEPT = default;
    // #endif
    // Point_(const Size_<_Tp>& sz);
    // Point_(const Vec<_Tp, 2>& v);
    //
    // #if (defined(__GNUC__) && __GNUC__ < 5)  // GCC 4.x bug. Details: https://github.com/opencv/opencv/pull/20837
    // Point_& operator = (const Point_& pt);
    // Point_& operator = (Point_&& pt) CV_NOEXCEPT = default;
    // #elif OPENCV_ABI_COMPATIBILITY < 500
    // Point_& operator = (const Point_& pt) = default;
    // Point_& operator = (Point_&& pt) CV_NOEXCEPT = default;
    // #endif
    // ! conversion to another data type
    // template<typename _Tp2> operator Point_<_Tp2>() const;

    // ! conversion to the old-style C structures
    // operator Vec<_Tp, 2>() const;

    // ! dot product
    // _Tp dot(const Point_& pt) const;
    // ! dot product computed in double-precision arithmetics
    // double ddot(const Point_& pt) const;
    // ! cross-product
    // double cross(const Point_& pt) const;
    // ! checks whether the point is inside the specified rectangle
    // bool inside(const Rect_<_Tp>& r) const;
  public
    x: T; // !< x coordinate of the point
    y: T; // !< y coordinate of the point
  end;

  TPoint2i = TPoint_<Int>;
  TPoint = TPoint2i;

function Point(const _x, _y: Int): TPoint; {$IFDEF USE_INLINE}inline; {$ENDIF}

// ---------------------- mat.hpp ----------------------
Type
  TMatSize = record
  public
    // explicit MatSize(int* _p) CV_NOEXCEPT;
    // int dims() const CV_NOEXCEPT;
    class operator Implicit(const m: TMatSize): TSize; // Size operator()() const;
    // const int& operator[](int i) const;
    // int& operator[](int i);
    // operator const int*() const CV_NOEXCEPT;  // TODO OpenCV 4.0: drop this
    // bool operator == (const MatSize& sz) const CV_NOEXCEPT;
    // bool operator != (const MatSize& sz) const CV_NOEXCEPT;
  private
    p: pInt; // pInt; // int* p;
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

  // pMatOp = ^TMatOp;
  pMatOp = Pointer;

  pMatExpr = ^TMatExpr;

  TMatExpr = record
  public
    class operator Initialize(out Dest: TMatExpr); // MatExpr();
    // explicit MatExpr(const Mat& m);
    //
    // MatExpr(const MatOp* _op, int _flags, const Mat& _a = Mat(), const Mat& _b = Mat(),
    // const Mat& _c = Mat(), double _alpha = 1, double _beta = 1, const Scalar& _s = Scalar());
    //
    // operator Mat() const;
    //
    function size: TSize; {$IFDEF USE_INLINE}inline; {$ENDIF} // Size size() const;
    // int type() const;
    //
    // MatExpr row(int y) const;
    // MatExpr col(int x) const;
    // MatExpr diag(int d = 0) const;
    // MatExpr operator()( const Range& rowRange, const Range& colRange ) const;
    // MatExpr operator()( const Rect& roi ) const;
    //
    // MatExpr t() const;
    // MatExpr inv(int method = DECOMP_LU) const;
    // MatExpr mul(const MatExpr& e, double scale=1) const;
    // MatExpr mul(const Mat& m, double scale=1) const;
    //
    // Mat cross(const Mat& m) const;
    // double dot(const Mat& m) const;
    //
    // void swap(MatExpr& b);
    //
    class operator Finalize(var Dest: TMatExpr);
  public
    op: pMatOp; // const MatOp* op;
    flags: Int; // int flags;
    //
    a, b, c: TCVMat;     // Mat a, b, c;
    alpha, beta: double; // double alpha, beta;
    s: TCVScalar;        // Scalar s;
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
    class operator assign(var Dest: TMat; const [ref] Src: TMat);

    // Mat& operator = (const Mat& m);
    class operator Implicit(const m: TMatExpr): TMat; // Mat& operator = (const MatExpr& expr);
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
    class function zeros(const size: TSize; &type: Int): TMatExpr; static; // CV_NODISCARD_STD static MatExpr zeros(Size size, int type);
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
    // CVMat: TCVMat;
    flags: Int; // int flags;
    // ! the matrix dimensionality, >= 2
    dims: Int; // int dims;
    // ! the number of rows and columns or (-1, -1) when the matrix has more than 2 dimensions
    rows, cols: Int; // int rows, cols;
    // ! pointer to the data
    data: pUChar; // uchar* data;
    //
    // ! helper fields used in locateROI and adjustROI
    datastart: pUChar; // const uchar* datastart;
    dataend: pUChar;   // const uchar* dataend;
    datalimit: pUChar; // const uchar* datalimit;
    //
    // ! custom allocator
    allocator: pMatAllocator; // MatAllocator* allocator;
    // ! and the standard allocator
    // static MatAllocator* getStdAllocator();
    // static MatAllocator* getDefaultAllocator();
    // static void setDefaultAllocator(MatAllocator* allocator);
    //
    // ! internal use method: updates the continuity flag
    // void updateContinuityFlag();
    //
    // ! interaction with UMat
    u: pUMatData; // UMatData* u;
    //
    size: TMatSize; // MatSize size;
    step: TMatStep; // MatStep step;
  end;

  // TMatOp_vfptr = array [0 .. 25] of Pointer;
  // pMatOp_vfptr = ^TMatOp_vfptr;
  // TMatOp = record
  // private
  // _vfptr: pMatOp_vfptr;
  // public
  // // MatOp();
  // // virtual ~MatOp();
  // //
  // // 1 virtual bool elementWise(const MatExpr& expr) const;
  // procedure assign(const expr: TMatExpr; Var m: TMat; &type: Int = -1); {$IFDEF USE_INLINE}inline; {$ENDIF}// 2 virtual void assign(const MatExpr& expr, Mat& m, int type=-1) const = 0;
  // // 3 virtual void roi(const MatExpr& expr, const Range& rowRange,
  // // const Range& colRange, MatExpr& res) const;
  // // 4 virtual void diag(const MatExpr& expr, int d, MatExpr& res) const;
  // // 5 virtual void augAssignAdd(const MatExpr& expr, Mat& m) const;
  // // 6 virtual void augAssignSubtract(const MatExpr& expr, Mat& m) const;
  // // 7 virtual void augAssignMultiply(const MatExpr& expr, Mat& m) const;
  // // 8 virtual void augAssignDivide(const MatExpr& expr, Mat& m) const;
  // // 9 virtual void augAssignAnd(const MatExpr& expr, Mat& m) const;
  // // 10 virtual void augAssignOr(const MatExpr& expr, Mat& m) const;
  // // 11 virtual void augAssignXor(const MatExpr& expr, Mat& m) const;
  // //
  // // 12 virtual void add(const MatExpr& expr1, const MatExpr& expr2, MatExpr& res) const;
  // // 13 virtual void add(const MatExpr& expr1, const Scalar& s, MatExpr& res) const;
  // //
  // // 14 virtual void subtract(const MatExpr& expr1, const MatExpr& expr2, MatExpr& res) const;
  // // 15 virtual void subtract(const Scalar& s, const MatExpr& expr, MatExpr& res) const;
  // //
  // // 16 virtual void multiply(const MatExpr& expr1, const MatExpr& expr2, MatExpr& res, double scale=1) const;
  // // 17 virtual void multiply(const MatExpr& expr1, double s, MatExpr& res) const;
  // //
  // // 18 virtual void divide(const MatExpr& expr1, const MatExpr& expr2, MatExpr& res, double scale=1) const;
  // // 19 virtual void divide(double s, const MatExpr& expr, MatExpr& res) const;
  // //
  // // 20 virtual void abs(const MatExpr& expr, MatExpr& res) const;
  // //
  // // 21 virtual void transpose(const MatExpr& expr, MatExpr& res) const;
  // // 22 virtual void matmul(const MatExpr& expr1, const MatExpr& expr2, MatExpr& res) const;
  // // 23 virtual void invert(const MatExpr& expr, int method, MatExpr& res) const;
  // //
  // // 24 virtual Size size(const MatExpr& expr) const;
  // // 25 virtual int type(const MatExpr& expr) const;
  // end;

  TScalar = record
  private
{$HINTS OFF}
    V: array [0 .. 3] of double;
{$HINTS ON}
  public
    class operator Initialize(out Dest: TScalar); // Scalar_();
    class function create(const v0, v1: double; const v2: double = 0; const v3: double = 0): TScalar; overload; static; // Scalar_(_Tp v0, _Tp v1, _Tp v2=0, _Tp v3=0);
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

function Scalar(const v0, v1: double; const v2: double = 0; const v3: double = 0): TScalar; {$IFDEF USE_INLINE}inline; {$ENDIF}

type
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
      expr = 6 shl KIND_SHIFT, // !< removed: https://github.com/opencv/opencv/pull/17046
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
    sz: TSize;    // Size sz;
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
    class operator Finalize(var Dest: TOutputArray); // destructor

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

    class operator Implicit(const m: TMat): TOutputArray;
    class operator Implicit(const OA: TOutputArray): TMat;
  end;

  TInputOutputArray = record
  private
{$HINTS OFF}
    OutputArray: TOutputArray;
{$HINTS ON}
  public
    class operator Initialize(out Dest: TInputOutputArray); // _InputOutputArray();
    // _InputOutputArray(int _flags, void* _obj);
    procedure InputOutputArray(const m: TMat); {$IFDEF USE_INLINE}inline; {$ENDIF}// _InputOutputArray(Mat& m);
    // _InputOutputArray(std::vector<Mat>& vec);
    // _InputOutputArray(cuda::GpuMat& d_mat);
    // _InputOutputArray(ogl::Buffer& buf);
    // _InputOutputArray(cuda::HostMem& cuda_mem);
    // template<typename _Tp> _InputOutputArray(cudev::GpuMat_<_Tp>& m);
    // template<typename _Tp> _InputOutputArray(std::vector<_Tp>& vec);
    // _InputOutputArray(std::vector<bool>& vec) = delete;  // not supported
    // template<typename _Tp> _InputOutputArray(std::vector<std::vector<_Tp> >& vec);
    // template<typename _Tp> _InputOutputArray(std::vector<Mat_<_Tp> >& vec);
    // template<typename _Tp> _InputOutputArray(Mat_<_Tp>& m);
    // template<typename _Tp> _InputOutputArray(_Tp* vec, int n);
    // template<typename _Tp, int m, int n> _InputOutputArray(Matx<_Tp, m, n>& matx);
    // _InputOutputArray(UMat& m);
    // _InputOutputArray(std::vector<UMat>& vec);
    class operator Finalize(var Dest: TInputOutputArray); // destructor
    //
    // _InputOutputArray(const Mat& m);
    // _InputOutputArray(const std::vector<Mat>& vec);
    // _InputOutputArray(const cuda::GpuMat& d_mat);
    // _InputOutputArray(const std::vector<cuda::GpuMat>& d_mat);
    // _InputOutputArray(const ogl::Buffer& buf);
    // _InputOutputArray(const cuda::HostMem& cuda_mem);
    // template<typename _Tp> _InputOutputArray(const cudev::GpuMat_<_Tp>& m);
    // template<typename _Tp> _InputOutputArray(const std::vector<_Tp>& vec);
    // template<typename _Tp> _InputOutputArray(const std::vector<std::vector<_Tp> >& vec);
    // template<typename _Tp> _InputOutputArray(const std::vector<Mat_<_Tp> >& vec);
    // template<typename _Tp> _InputOutputArray(const Mat_<_Tp>& m);
    // template<typename _Tp> _InputOutputArray(const _Tp* vec, int n);
    // template<typename _Tp, int m, int n> _InputOutputArray(const Matx<_Tp, m, n>& matx);
    // _InputOutputArray(const UMat& m);
    // _InputOutputArray(const std::vector<UMat>& vec);
    //
    // template<typename _Tp, std::size_t _Nm> _InputOutputArray(std::array<_Tp, _Nm>& arr);
    // template<typename _Tp, std::size_t _Nm> _InputOutputArray(const std::array<_Tp, _Nm>& arr);
    // template<std::size_t _Nm> _InputOutputArray(std::array<Mat, _Nm>& arr);
    // template<std::size_t _Nm> _InputOutputArray(const std::array<Mat, _Nm>& arr);
    //
    // template<typename _Tp> static _InputOutputArray rawInOut(std::vector<_Tp>& vec);
    // template<typename _Tp, std::size_t _Nm> _InputOutputArray rawInOut(std::array<_Tp, _Nm>& arr);

    class operator Implicit(const m: TMat): TInputOutputArray;
    class operator Implicit(const IOA: TInputOutputArray): TMat;
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

// ---------------------- highgui.hpp ----------------------
Type
  WindowFlags = (                //
    WINDOW_NORMAL = $00000000,   // !< the user can resize the window (no constraint) / also use to switch a fullscreen window to a normal size.
    WINDOW_AUTOSIZE = $00000001, // !< the user cannot resize the window, the size is constrainted by the image displayed.
    WINDOW_OPENGL = $00001000,   // !< window with opengl support.

    WINDOW_FULLSCREEN = 1,           // !< change the window to fullscreen.
    WINDOW_FREERATIO = $00000100,    // !< the image expends as much as it can (no ratio constraint).
    WINDOW_KEEPRATIO = $00000000,    // !< the ratio of the image is respected.
    WINDOW_GUI_EXPANDED = $00000000, // !< status bar and tool bar
    WINDOW_GUI_NORMAL = $00000010    // !< old fashious way
    );

  (* * @brief Creates a window.

    The function namedWindow creates a window that can be used as a placeholder for images and
    trackbars. Created windows are referred to by their names.

    If a window with the same name already exists, the function does nothing.

    You can call cv::destroyWindow or cv::destroyAllWindows to close the window and de-allocate any associated
    memory usage. For a simple program, you do not really have to call these functions because all the
    resources and windows of the application are closed automatically by the operating system upon exit.

    @note

    Qt backend supports additional flags:
    -   **WINDOW_NORMAL or WINDOW_AUTOSIZE:** WINDOW_NORMAL enables you to resize the
    window, whereas WINDOW_AUTOSIZE adjusts automatically the window size to fit the
    displayed image (see imshow ), and you cannot change the window size manually.
    -   **WINDOW_FREERATIO or WINDOW_KEEPRATIO:** WINDOW_FREERATIO adjusts the image
    with no respect to its ratio, whereas WINDOW_KEEPRATIO keeps the image ratio.
    -   **WINDOW_GUI_NORMAL or WINDOW_GUI_EXPANDED:** WINDOW_GUI_NORMAL is the old way to draw the window
    without statusbar and toolbar, whereas WINDOW_GUI_EXPANDED is a new enhanced GUI.
    By default, flags == WINDOW_AUTOSIZE | WINDOW_KEEPRATIO | WINDOW_GUI_EXPANDED

    @param winname Name of the window in the window caption that may be used as a window identifier.
    @param flags Flags of the window. The supported flags are: (cv::WindowFlags)
  *)
  // CV_EXPORTS_W void namedWindow(const String& winname, int flags = WINDOW_AUTOSIZE);
  // ?namedWindow@cv@@YAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@H@Z
  // void cv::namedWindow(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int)
procedure namedWindow(const winname: CppString; flags: WindowFlags = WINDOW_AUTOSIZE); overload; external opencv_world_dll index 5721
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ---------------------- end highgui.hpp ----------------------

// ---------------------- start imgproc.hpp ----------------------
type
  LineTypes = (  //
    FILLED = -1, //
    LINE_4 = 4,  // !< 4-connected line
    LINE_8 = 8,  // !< 8-connected line
    LINE_AA = 16 // !< antialiased line
    );

  HersheyFonts = (                   //
    FONT_HERSHEY_SIMPLEX = 0,        // !< normal size sans-serif font
    FONT_HERSHEY_PLAIN = 1,          // !< small size sans-serif font
    FONT_HERSHEY_DUPLEX = 2,         // !< normal size sans-serif font (more complex than FONT_HERSHEY_SIMPLEX)
    FONT_HERSHEY_COMPLEX = 3,        // !< normal size serif font
    FONT_HERSHEY_TRIPLEX = 4,        // !< normal size serif font (more complex than FONT_HERSHEY_COMPLEX)
    FONT_HERSHEY_COMPLEX_SMALL = 5,  // !< smaller version of FONT_HERSHEY_COMPLEX
    FONT_HERSHEY_SCRIPT_SIMPLEX = 6, // !< hand-writing style font
    FONT_HERSHEY_SCRIPT_COMPLEX = 7, // !< more complex variant of FONT_HERSHEY_SCRIPT_SIMPLEX
    FONT_ITALIC = 16                 // !< flag for italic font
    );

  (* * @brief Draws a text string.

    The function cv::putText renders the specified text string in the image. Symbols that cannot be rendered
    using the specified font are replaced by question marks. See #getTextSize for a text rendering code
    example.

    @param img Image.
    @param text Text string to be drawn.
    @param org Bottom-left corner of the text string in the image.
    @param fontFace Font type, see #HersheyFonts.
    @param fontScale Font scale factor that is multiplied by the font-specific base size.
    @param color Text color.
    @param thickness Thickness of the lines used to draw a text.
    @param lineType Line type. See #LineTypes
    @param bottomLeftOrigin When true, the image data origin is at the bottom-left corner. Otherwise,
    it is at the top-left corner.
  *)
  // CV_EXPORTS_W void putText( InputOutputArray img, const String& text, Point org,
  // int fontFace, double fontScale, Scalar color,
  // int thickness = 1, int lineType = LINE_8,
  // bool bottomLeftOrigin = false );
  // 5972
  // ?putText@cv@@YAXAEBV_InputOutputArray@1@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@V?$Point_@H@1@HNV?$Scalar_@N@1@HH_N@Z
  // void cv::putText(class cv::_InputOutputArray const &,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,class cv::Point_<int>,int,double,class cv::Scalar_<double>,int,int,bool)
procedure _putText(img: TInputOutputArray; const text: CppString; org: UInt64; fontFace: HersheyFonts; fontScale: double; color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
  bottomLeftOrigin: Bool = false); external opencv_world_dll index 5972
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
procedure putText(img: TInputOutputArray; const text: CppString; org: TPoint; fontFace: HersheyFonts; fontScale: double; color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
  bottomLeftOrigin: Bool = false); {$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Blurs an image using the normalized box filter.

  The function smooths an image using the kernel:

  \f[\texttt{K} =  \frac{1}{\texttt{ksize.width*ksize.height}} \begin{bmatrix} 1 & 1 & 1 &  \cdots & 1 & 1  \\ 1 & 1 & 1 &  \cdots & 1 & 1  \\ \hdotsfor{6} \\ 1 & 1 & 1 &  \cdots & 1 & 1  \\ \end{bmatrix}\f]

  The call `blur(src, dst, ksize, anchor, borderType)` is equivalent to `boxFilter(src, dst, src.type(), ksize,
  anchor, true, borderType)`.

  @param src input image; it can have any number of channels, which are processed independently, but
  the depth should be CV_8U, CV_16U, CV_16S, CV_32F or CV_64F.
  @param dst output image of the same size and type as src.
  @param ksize blurring kernel size.
  @param anchor anchor point; default value Point(-1,-1) means that the anchor is at the kernel
  center.
  @param borderType border mode used to extrapolate pixels outside of the image, see #BorderTypes. #BORDER_WRAP is not supported.
  @sa  boxFilter, bilateralFilter, GaussianBlur, medianBlur
*)
// CV_EXPORTS_W void blur( InputArray src, OutputArray dst,
// Size ksize, Point anchor = Point(-1,-1),
// int borderType = BORDER_DEFAULT );
// 3649
// ?blur@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@V?$Size_@H@1@V?$Point_@H@1@H@Z
// void cv::blur(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::Size_<int>,class cv::Point_<int>,int)

procedure _blur(Src: TInputArray; dst: TOutputArray; ksize: UInt64 { TSize }; anchor: UInt64 { TPoint  = Point(-1, -1) }; borderType: Int { = BORDER_DEFAULT } ); external opencv_world_dll index 3649
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
procedure blur(Src: TInputArray; dst: TOutputArray; ksize: TSize); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure blur(Src: TInputArray; dst: TOutputArray; ksize: TSize; anchor: TPoint; borderType: BorderTypes = BORDER_DEFAULT); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Blurs an image using a Gaussian filter.

  The function convolves the source image with the specified Gaussian kernel. In-place filtering is
  supported.

  @param src input image; the image can have any number of channels, which are processed
  independently, but the depth should be CV_8U, CV_16U, CV_16S, CV_32F or CV_64F.
  @param dst output image of the same size and type as src.
  @param ksize Gaussian kernel size. ksize.width and ksize.height can differ but they both must be
  positive and odd. Or, they can be zero's and then they are computed from sigma.
  @param sigmaX Gaussian kernel standard deviation in X direction.
  @param sigmaY Gaussian kernel standard deviation in Y direction; if sigmaY is zero, it is set to be
  equal to sigmaX, if both sigmas are zeros, they are computed from ksize.width and ksize.height,
  respectively (see #getGaussianKernel for details); to fully control the result regardless of
  possible future modifications of all this semantics, it is recommended to specify all of ksize,
  sigmaX, and sigmaY.
  @param borderType pixel extrapolation method, see #BorderTypes. #BORDER_WRAP is not supported.

  @sa  sepFilter2D, filter2D, blur, boxFilter, bilateralFilter, medianBlur
*)
// CV_EXPORTS_W void GaussianBlur( InputArray src, OutputArray dst, Size ksize,
// double sigmaX, double sigmaY = 0,
// int borderType = BORDER_DEFAULT );
// 3370
// ?GaussianBlur@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@V?$Size_@H@1@NNH@Z
// void cv::GaussianBlur(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::Size_<int>,double,double,int)
procedure _GaussianBlur(Src: TInputArray; dst: TOutputArray; ksize: UInt64 { TSize }; sigmaX: double; sigmaY: double { = 0 }; borderType: Int { = BORDER_DEFAULT }
  ); external opencv_world_dll index 3370 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
procedure GaussianBlur(Src: TInputArray; dst: TOutputArray; ksize: TSize; sigmaX: double; sigmaY: double = 0; borderType: BorderTypes = BORDER_DEFAULT); {$IFDEF USE_INLINE}inline;
{$ENDIF}
//
(* * @brief Applies the bilateral filter to an image.

  The function applies bilateral filtering to the input image, as described in
  http://www.dai.ed.ac.uk/CVonline/LOCAL_COPIES/MANDUCHI1/Bilateral_Filtering.html
  bilateralFilter can reduce unwanted noise very well while keeping edges fairly sharp. However, it is
  very slow compared to most filters.

  _Sigma values_: For simplicity, you can set the 2 sigma values to be the same. If they are small (\<
  10), the filter will not have much effect, whereas if they are large (\> 150), they will have a very
  strong effect, making the image look "cartoonish".

  _Filter size_: Large filters (d \> 5) are very slow, so it is recommended to use d=5 for real-time
  applications, and perhaps d=9 for offline applications that need heavy noise filtering.

  This filter does not work inplace.
  @param src Source 8-bit or floating-point, 1-channel or 3-channel image.
  @param dst Destination image of the same size and type as src .
  @param d Diameter of each pixel neighborhood that is used during filtering. If it is non-positive,
  it is computed from sigmaSpace.
  @param sigmaColor Filter sigma in the color space. A larger value of the parameter means that
  farther colors within the pixel neighborhood (see sigmaSpace) will be mixed together, resulting
  in larger areas of semi-equal color.
  @param sigmaSpace Filter sigma in the coordinate space. A larger value of the parameter means that
  farther pixels will influence each other as long as their colors are close enough (see sigmaColor
  ). When d\>0, it specifies the neighborhood size regardless of sigmaSpace. Otherwise, d is
  proportional to sigmaSpace.
  @param borderType border mode used to extrapolate pixels outside of the image, see #BorderTypes
*)
// CV_EXPORTS_W void bilateralFilter( InputArray src, OutputArray dst, int d,
// double sigmaColor, double sigmaSpace,
// int borderType = BORDER_DEFAULT );
// 3615
// ?bilateralFilter@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HNNH@Z
// void cv::bilateralFilter(class cv::_InputArray const &,class cv::_OutputArray const &,int,double,double,int)
procedure bilateralFilter(Src: TInputArray; dst: TOutputArray; d: Int; sigmaColor, sigmaSpace: double; borderType: BorderTypes = BORDER_DEFAULT);
external opencv_world_dll index 3615 {$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};
//
(* * @brief Blurs an image using the median filter.

  The function smoothes an image using the median filter with the \f$\texttt{ksize} \times
  \texttt{ksize}\f$ aperture. Each channel of a multi-channel image is processed independently.
  In-place operation is supported.

  @note The median filter uses #BORDER_REPLICATE internally to cope with border pixels, see #BorderTypes

  @param src input 1-, 3-, or 4-channel image; when ksize is 3 or 5, the image depth should be
  CV_8U, CV_16U, or CV_32F, for larger aperture sizes, it can only be CV_8U.
  @param dst destination array of the same size and type as src.
  @param ksize aperture linear size; it must be odd and greater than 1, for example: 3, 5, 7 ...
  @sa  bilateralFilter, blur, boxFilter, GaussianBlur
*)
// CV_EXPORTS_W void medianBlur( InputArray src, OutputArray dst, int ksize );
// 5628
// ?medianBlur@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@H@Z
// void cv::medianBlur(class cv::_InputArray const &,class cv::_OutputArray const &,int)
procedure medianBlur(Src: TInputArray; dst: TOutputArray; ksize: Int); external opencv_world_dll index 5628
{$IFDEF DELAYED_LOAD_DLL}delayed{$ENDIF};

// ---------------------- end imgproc.hpp ----------------------

implementation

procedure GaussianBlur(Src: TInputArray; dst: TOutputArray; ksize: TSize; sigmaX: double; sigmaY: double; borderType: BorderTypes);
begin
  _GaussianBlur(Src, dst, UInt64(ksize), sigmaX, sigmaY, Int(borderType));
end;

procedure blur(Src: TInputArray; dst: TOutputArray; ksize: TSize);
begin
  blur(Src, dst, ksize, Point(-1, -1), BORDER_DEFAULT);
end;

procedure blur(Src: TInputArray; dst: TOutputArray; ksize: TSize; anchor: TPoint; borderType: BorderTypes);
begin
  _blur(Src, dst, UInt64(ksize), UInt64(anchor), Int(borderType));
end;

procedure putText(img: TInputOutputArray; const text: CppString; org: TPoint; fontFace: HersheyFonts; fontScale: double; color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
  bottomLeftOrigin: Bool = false);
begin
  _putText(img, text, UInt64(org), fontFace, fontScale, color, thickness, lineType, bottomLeftOrigin);
end;

procedure copyMakeBorder(const Src: TInputArray; Var dst: TOutputArray; top, bottom, left, right, borderType: Int); overload;
Var
  Scalar: TScalar;
begin
  copyMakeBorder(Src, dst, top, bottom, left, right, borderType, Scalar);
end;

{ TMat }

class operator TMat.Implicit(const m: TMatExpr): TMat;
begin
  Mat_Operator_Assign(@Result, @m);
end;

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

class operator TMat.assign(var Dest: TMat; const [ref] Src: TMat);
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

class function TMat.zeros(const size: TSize; &type: Int): TMatExpr;
begin
  OpenCV.Import.zeros(@Result, UInt64(size), &type);
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
  Result := pMat(OA.InputArray.getObj)^;
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

function Scalar(const v0, v1, v2, v3: double): TScalar;
begin
  Result := TScalar.create(v0, v1, v2, v3);
end;

{ TSize_<T> }

class function TSize_<T>.size(const _width, _height: T): TSize_<T>;
begin
  Result.width := _width;
  Result.height := _height;
end;

function size(const _width, _height: Int): TSize;
begin
  Result := TSize.size(_width, _height);
end;

{ TMatExpr }

class operator TMatExpr.Finalize(var Dest: TMatExpr);
begin
  Destructor_MatExpr(@Dest);
end;

class operator TMatExpr.Initialize(out Dest: TMatExpr);
begin
  constructor_MatExpr(@Dest);
end;

function TMatExpr.size: TSize;
begin
  Result := TSize(MatExpr_size(@Self)^);
end;

{ TMatSize }

class operator TMatSize.Implicit(const m: TMatSize): TSize;
begin
  MatSize_MatSizeToSize(@m, @Result);
end;

// { TMatOp }
//
// procedure TMatOp.assign(const expr: TMatExpr; Var m: TMat; &type: Int);
// Type
// TAssignProc = procedure(const expr: pMatExpr; Var m: TMat; &type: Int);
// begin
// TAssignProc(expr.op^._vfptr[2])(@expr, m, &type);
// end;

{ TInputOutputArray }

class operator TInputOutputArray.Finalize(var Dest: TInputOutputArray);
begin
  Destructor_InputOutputArray(@Dest);
end;

class operator TInputOutputArray.Implicit(const IOA: TInputOutputArray): TMat;
begin
  Assert(IOA.OutputArray.InputArray.isMat);
  Result := pMat(IOA.OutputArray.InputArray.getObj)^;
end;

class operator TInputOutputArray.Implicit(const m: TMat): TInputOutputArray;
begin
  Result.InputOutputArray(m);
end;

class operator TInputOutputArray.Initialize(out Dest: TInputOutputArray);
begin
  Constructor_InputOutputArray(@Dest);
end;

procedure TInputOutputArray.InputOutputArray(const m: TMat);
begin
  Constructor_InputOutputArray(@Self, @m);
end;

{ TPoint_<T> }

class function TPoint_<T>.Point(_x, _y: T): TPoint_<T>;
begin
  Result.x := _x;
  Result.y := _y;
end;

function Point(const _x, _y: Int): TPoint;
begin
  Result := TPoint.Point(_x, _y);
end;

end.
