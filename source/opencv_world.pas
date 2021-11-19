unit opencv_world;

{$IFDEF RELEASE}
{$DEFINE DELAYED_LOAD_DLL}
{$DEFINE USE_INLINE}
{$ENDIF}
{$DEFINE UseSystemMath}
{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$POINTERMATH ON}
{$MINENUMSIZE 4}
{$TYPEDADDRESS ON}

interface

Uses
  System.TypInfo,
  System.SysUtils
{$IFDEF UseSystemMath}
    , System.Math
{$ENDIF}
    ;

const
  cvversion         = '454';
  opencv_delphi_dll = 'opencv_delphi' + cvversion + {$IFDEF DEBUG} 'd' + {$ENDIF} '.dll';
  opencv_world_dll  = 'opencv_world' + cvversion + {$IFDEF DEBUG} 'd' + {$ENDIF} '.dll';

{$REGION 'std::'}

Type
  BOOL = LongBool;
  size_t = NativeUInt;
  psize_t = ^size_t;
  Int = integer;
  pInt = ^Int;
  unsigned = UInt32;
  float = Single;
  pFloat = ^float;
  ppAnsiChar = ^pAnsiChar;
  uchar = Byte;
  pUChar = type pByte;
  pMatOp = type Pointer;
  pMatAllocator = type Pointer;
  pUMatData = type Pointer;
  pUCharConst = pUChar;
  PointerConst = type Pointer;

type
  // cv::std::String
  CvStdString = record
  private
{$HINTS OFF}
    Dummy: array [0 .. 39] of Byte;
{$HINTS ON}
  public
    class operator Initialize(out Dest: CvStdString);
    class operator Finalize(var Dest: CvStdString);

    function length: UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function size: UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure erase(const _Off: UInt64 = 0); {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure assign(const p: pAnsiChar); {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator assign(var Dest: CvStdString; const [ref] Src: CvStdString);
    class operator Implicit(const p: pAnsiChar): CvStdString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: string): CvStdString; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: CvStdString): string; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  CppString = CvStdString;

  pCppString = ^CvStdString;
  pCvStdString = ^CvStdString;

Type
  TPtr<T: record > = record
  public type
    pT = ^T;
  public
    _Ptr: pT;
    _Ref: Pointer;
    function v: pT; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Finalize(var Dest: TPtr<T>);
  end;

Type
  // std::vector<T>

  TStdPointer = type Pointer;

  TVectorType =        //
    (                  //
    vtMat = 1,         // vector<Mat>
    vtRect = 2,        // vector<Rect>
    vtPoint = 3,       // vector<Point>
    vtVectorMat = 4,   // vector<vector<Mat>>
    vtVectorRect = 5,  // vector<vector<Rect>>
    vtVectorPoint = 6, // vector<vector<Point>>
    vtPoint2f = 7,     // vector<Point2f>
    vtScalar = 8,      // vector<Scalar>
    vtUchar = 9,       // vector<uchar>
    vtFloat = 10,      // vector<float>
    vtInt = 11         // vector<int>
    );

  Vector<T> = record
  private
{$HINTS OFF}
    Data: array [0 .. 31] of Byte;
{$HINTS ON}
    class function vt: TVectorType; static;
    function GetItems(const index: UInt64): T; {$IFDEF USE_INLINE}inline; {$ENDIF}
  public
    class operator Initialize(out Dest: Vector<T>);
    class operator Finalize(var Dest: Vector<T>);
    class operator assign(var Dest: Vector<T>; const [ref] Src: Vector<T>);

    function size: { UInt64 } Int64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function empty: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure push_back(const Value: T); {$IFDEF USE_INLINE}inline; {$ENDIF}
    property Items[const index: UInt64]: T read GetItems; default;
  end;

  TStdVectorCppString = Vector<CvStdString>;
  pStdVectorCppString = ^TStdVectorCppString;

{$ENDREGION 'std::'}

  //
{$REGION 'CV const'}

const
  INT_MAX = MaxInt;
  DBL_MAX = 1.7976931348623158E+308; // max value
{$ENDREGION 'CV const'}
  //
{$REGION 'Interface.h'}

const
  CV_CN_MAX    = 512;
  CV_CN_SHIFT  = 3;
  CV_DEPTH_MAX = (1 shl CV_CN_SHIFT);

  CV_8U  = 0; // UInt8
  CV_8S  = 1; // Int8
  CV_16U = 2; // UInt16
  CV_16S = 3; // Int16
  CV_32S = 4; // Int32
  CV_32F = 5; // float(single)
  CV_64F = 6; // double
  CV_16F = 7;

  CV_MAT_DEPTH_MASK = (CV_DEPTH_MAX - 1);
function CV_MAT_DEPTH(flags: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}     // #define CV_MAT_DEPTH(flags)     ((flags) & CV_MAT_DEPTH_MASK)
function CV_MAKETYPE(depth, cn: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}  // #define CV_MAKETYPE(depth,cn) (CV_MAT_DEPTH(depth) + (((cn)-1) << CV_CN_SHIFT))
function CV_MAKE_TYPE(depth, cn: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // #define CV_MAKE_TYPE CV_MAKETYPE

Var
  CV_8UC1: Int;
  CV_8UC2: Int;
  CV_8UC3: Int;
  CV_8UC4: Int;
function CV_8UC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_8UC(n) CV_MAKETYPE(CV_8U,(n))

Var
  CV_8SC1: Int;
  CV_8SC2: Int;
  CV_8SC3: Int;
  CV_8SC4: Int;
function CV_8SC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_MAKETYPE(CV_8S,(n))

Var
  CV_16UC1: Int;
  CV_16UC2: Int;
  CV_16UC3: Int;
  CV_16UC4: Int;
function CV_16UC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_16U,(n))

Var
  CV_16SC1: Int;
  CV_16SC2: Int;
  CV_16SC3: Int;
  CV_16SC4: Int;
function CV_16SC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_16S,(n))

Var
  CV_32SC1: Int;
  CV_32SC2: Int;
  CV_32SC3: Int;
  CV_32SC4: Int;
function CV_32SC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_32S,(n))

Var
  CV_32FC1: Int;
  CV_32FC2: Int;
  CV_32FC3: Int;
  CV_32FC4: Int;
function CV_32FC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_32F,(n))

Var
  CV_64FC1: Int;
  CV_64FC2: Int;
  CV_64FC3: Int;
  CV_64FC4: Int;
function CV_64FC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_64F,(n))

Var
  CV_16FC1: Int;
  CV_16FC2: Int;
  CV_16FC3: Int;
  CV_16FC4: Int;
function CV_16FC(n: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_MAKETYPE(CV_16F,(n))

{$ENDREGION 'Interface.h'}
//
{$REGION 'traits.hpp'}

Type
  TDataType<T> = record
  private
    class operator Initialize(out Dest: TDataType<T>);
  public
    generic_type: Int; // = 1,
    depth: Int;        // = -1,
    channels: Int;     // = 1,
    fmt: Int;          // = 0,
    &type: Int;        // = CV_MAKETYPE(depth, channels)
  end;

  TDepth<T> = record // Need refactoring as TTraitsType<T>
    class function Value: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TTraitsType<T> = record
    class function Value: Int; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

{$ENDREGION 'traits.hpp'}
  //
{$REGION 'base.hpp'}

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

  (* * norm types
    src1 and src2 denote input arrays.
  *)
  NormTypes = ( //
    (* *
      \f[
      norm =  \forkthree
      {\|\texttt{src1}\|_{L_{\infty}} =  \max _I | \texttt{src1} (I)|}{if  \(\texttt{normType} = \texttt{NORM_INF}\) }
      {\|\texttt{src1}-\texttt{src2}\|_{L_{\infty}} =  \max _I | \texttt{src1} (I) -  \texttt{src2} (I)|}{if  \(\texttt{normType} = \texttt{NORM_INF}\) }
      {\frac{\|\texttt{src1}-\texttt{src2}\|_{L_{\infty}}    }{\|\texttt{src2}\|_{L_{\infty}} }}{if  \(\texttt{normType} = \texttt{NORM_RELATIVE | NORM_INF}\) }
      \f]
    *)
    NORM_INF = 1,
    (* *
      \f[
      norm =  \forkthree
      {\| \texttt{src1} \| _{L_1} =  \sum _I | \texttt{src1} (I)|}{if  \(\texttt{normType} = \texttt{NORM_L1}\)}
      { \| \texttt{src1} - \texttt{src2} \| _{L_1} =  \sum _I | \texttt{src1} (I) -  \texttt{src2} (I)|}{if  \(\texttt{normType} = \texttt{NORM_L1}\) }
      { \frac{\|\texttt{src1}-\texttt{src2}\|_{L_1} }{\|\texttt{src2}\|_{L_1}} }{if  \(\texttt{normType} = \texttt{NORM_RELATIVE | NORM_L1}\) }
      \f] *)
    NORM_L1 = 2,
    (* *
      \f[
      norm =  \forkthree
      { \| \texttt{src1} \| _{L_2} =  \sqrt{\sum_I \texttt{src1}(I)^2} }{if  \(\texttt{normType} = \texttt{NORM_L2}\) }
      { \| \texttt{src1} - \texttt{src2} \| _{L_2} =  \sqrt{\sum_I (\texttt{src1}(I) - \texttt{src2}(I))^2} }{if  \(\texttt{normType} = \texttt{NORM_L2}\) }
      { \frac{\|\texttt{src1}-\texttt{src2}\|_{L_2} }{\|\texttt{src2}\|_{L_2}} }{if  \(\texttt{normType} = \texttt{NORM_RELATIVE | NORM_L2}\) }
      \f]
    *)
    NORM_L2 = 4,
    (* *
      \f[
      norm =  \forkthree
      { \| \texttt{src1} \| _{L_2} ^{2} = \sum_I \texttt{src1}(I)^2} {if  \(\texttt{normType} = \texttt{NORM_L2SQR}\)}
      { \| \texttt{src1} - \texttt{src2} \| _{L_2} ^{2} =  \sum_I (\texttt{src1}(I) - \texttt{src2}(I))^2 }{if  \(\texttt{normType} = \texttt{NORM_L2SQR}\) }
      { \left(\frac{\|\texttt{src1}-\texttt{src2}\|_{L_2} }{\|\texttt{src2}\|_{L_2}}\right)^2 }{if  \(\texttt{normType} = \texttt{NORM_RELATIVE | NORM_L2SQR}\) }
      \f]
    *)
    NORM_L2SQR = 5,
    (* *
      In the case of one input array, calculates the Hamming distance of the array from zero,
      In the case of two input arrays, calculates the Hamming distance between the arrays.
    *)
    NORM_HAMMING = 6,
    (* *
      Similar to NORM_HAMMING, but in the calculation, each two bits of the input sequence will
      be added and treated as a single bit to be used in the same calculation as NORM_HAMMING.
    *)
    NORM_HAMMING2 = 7, NORM_TYPE_MASK = 7, // !< bit-mask which can be used to separate norm type from norm flags
    NORM_RELATIVE = 8,                     // !< flag
    NORM_MINMAX = 32                       // !< flag
    );
{$ENDREGION 'base.hpp'}
  //
{$REGION 'cvdef.h'}

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

function MIN(a, b: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}// ((a) > (b) ? (b) : (a))
function MAX(a, b: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}// ((a) < (b) ? (b) : (a))

type
  TRect_<T> = record
  public
    // ! default constructor
    // Rect_();
    // Rect_(_Tp _x, _Tp _y, _Tp _width, _Tp _height);
    // #if OPENCV_ABI_COMPATIBILITY < 500
    // Rect_(const Rect_& r) = default;
    // Rect_(Rect_&& r) CV_NOEXCEPT = default;
    // #endif
    // Rect_(const Point_<_Tp>& org, const Size_<_Tp>& sz);
    // Rect_(const Point_<_Tp>& pt1, const Point_<_Tp>& pt2);
    //
    // #if OPENCV_ABI_COMPATIBILITY < 500
    // Rect_& operator = (const Rect_& r) = default;
    // Rect_& operator = (Rect_&& r) CV_NOEXCEPT = default;
    // #endif
    // ! the top-left corner
    // Point_<_Tp> tl() const;
    // ! the bottom-right corner
    // Point_<_Tp> br() const;

    // ! size (width, height) of the rectangle
    // Size_<_Tp> size() const;
    // ! area (width*height) of the rectangle
    // _Tp area() const;
    // ! true if empty
    // bool empty() const;

    // ! conversion to another data type
    // template<typename _Tp2> operator Rect_<_Tp2>() const;

    // ! checks whether the rectangle contains the point
    // bool contains(const Point_<_Tp>& pt) const;
  public
    x: T;      // !< x coordinate of the top-left corner
    y: T;      // !< y coordinate of the top-left corner
    width: T;  // !< width of the rectangle
    height: T; // !< height of the rectangle
  end;

  TRect2i = TRect_<Int>;
  TRect = TRect2i;
  pRect = ^TRect;

  TStdVectorRect = Vector<TRect>;
  pStdVectorRect = ^TStdVectorRect;

{$ENDREGION 'cvdef.h'}
  //
{$REGION 'types.hpp'}

Type
  TSize_<T> = record
  public
    // //! default constructor
    // Size_();
    class function size(const _width, _height: T): TSize_<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// Size_(_Tp _width, _Tp _height);
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
    //
    class operator Implicit(const m: TSize_<T>): UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
  public
    width: T;  // _Tp width;  // !< the width
    height: T; // _Tp height; // !< the height
  end;

  TSize2i = TSize_<Int>;
  TSize = TSize2i;
  pSize = ^TSize;
  rSize = UInt64;

function size(const _width, _height: Int): TSize; {$IFDEF USE_INLINE}inline; {$ENDIF}

type
  TPoint_<T> = record
  public
    // ! default constructor
    // Point_();
    class function Point(_x, _y: T): TPoint_<T>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// Point_(_Tp _x, _Tp _y);
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
    class operator Implicit(const m: TPoint_<T>): UInt64; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const p: TPoint_<T>): TPoint_<Int>; {$IFDEF USE_INLINE}inline; {$ENDIF}
  public
    x: T; // !< x coordinate of the point
    y: T; // !< y coordinate of the point
  end;

  TPoint2i = TPoint_<Int>;
  TPoint = TPoint2i;
  pPoint = ^TPoint;
  rPoint = UInt64;

  TPoint2f = TPoint_<float>;
  TPoint2d = TPoint_<double>;

function Point(const _x, _y: Int): TPoint; {$IFDEF USE_INLINE}inline; {$ENDIF}

//
(* * @brief The class defining termination criteria for iterative algorithms.

  You can initialize it by default constructor and then override any parameters, or the structure may
  be fully initialized using the advanced variant of the constructor.
*)
type
  pTermCriteria = ^TTermCriteria;

  TTermCriteria = record
  public const
    (* *
      Criteria type, can be one of: COUNT, EPS or COUNT + EPS
    *)

    COUNT    = 1;     // !< the maximum number of iterations or elements to compute
    MAX_ITER = COUNT; // !< ditto
    EPS      = 2;     // !< the desired accuracy or change in parameters at which the iterative algorithm stops
  public
    // ! default constructor
    // TermCriteria();
    (* *
      @param type The type of termination criteria, one of TermCriteria::Type
      @param maxCount The maximum number of iterations or elements to compute.
      @param epsilon The desired accuracy or change in parameters at which the iterative algorithm stops.
    *)
    // TermCriteria(int &type, int maxCount, double epsilon);
    class function TermCriteria(const &type, maxCount: Int; const epsilon: double): TTermCriteria; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // inline bool isValid() const
    {
      const bool isCount = (type & COUNT) && maxCount > 0;
      const bool isEps = (type & EPS) && !cvIsNaN(epsilon);
      return isCount || isEps;
    }
    function isValid(): BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
  public
    &type: Int;      // !< the type of termination criteria: COUNT, EPS or COUNT + EPS
    maxCount: Int;   // !< the maximum number of iterations/elements
    epsilon: double; // !< the desired accuracy
  end;

{$ENDREGION 'types.hpp'}
  //
{$REGION 'mat.hpp'}

Type

  AccessFlag = (ACCESS_READ = 1 shl 24, ACCESS_WRITE = 1 shl 25, ACCESS_RW = 3 shl 24, ACCESS_MASK = ACCESS_RW, ACCESS_FAST = 1 shl 26);

  TCVMat = array [0 .. 95] of Byte;    // forward declaration
  TCVScalar = array [0 .. 31] of Byte; // forward declaration

  pMatSize = ^TMatSize;

  TMatSize = record
  public
    // explicit MatSize(int* _p) CV_NOEXCEPT;
    // int dims() const CV_NOEXCEPT;
    class operator Implicit(const m: TMatSize): TSize; {$IFDEF USE_INLINE}inline; {$ENDIF}// Size operator()() const;
    class operator Implicit(const m: TMatSize): String; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // const int& operator[](int i) const;
    // int& operator[](int i);
    // operator const int*() const CV_NOEXCEPT;  // TODO OpenCV 4.0: drop this
    // bool operator == (const MatSize& sz) const CV_NOEXCEPT;
    // bool operator != (const MatSize& sz) const CV_NOEXCEPT;
  private
{$HINTS OFF}
    p: pInt; // pInt; // int* p;
{$HINTS ON}
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
    op: pMatOp; // pMatOp; // const MatOp* op;
    flags: Int; // int flags;
    //
    a, b, c: TCVMat;     // Mat a, b, c;
    alpha, beta: double; // double alpha, beta;
    s: TCVScalar;        // Scalar s;
  end;

  pMat = ^TMat;

  TMat = record // 96 bytes, v4.5.4
  public const
    MAGIC_VAL       = $42FF0000;
    AUTO_STEP       = 0;
    CONTINUOUS_FLAG = CV_MAT_CONT_FLAG;
    SUBMATRIX_FLAG  = CV_SUBMAT_FLAG;

    MAGIC_MASK = $FFFF0000;
    TYPE_MASK  = $00000FFF;
    DEPTH_MASK = 7;
  public
    // default constructor
    class operator Initialize(out Dest: TMat); // Mat();
    class function Mat(): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // constructors
    class function Mat(const size: TSize; &type: Int): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// Mat(Size size, int type);
    // Mat(int rows, int cols, int type, const Scalar& s);
    // Mat(Size size, int type, const Scalar& s);
    // Mat(int ndims, const int* sizes, int type);
    // Mat(const std::vector<int>& sizes, int type);
    // Mat(int ndims, const int* sizes, int type, const Scalar& s);
    // Mat(const std::vector<int>& sizes, int type, const Scalar& s);
    // Mat(const Mat& m);
    class function Mat(rows, cols: Int; &type: Int; Data: Pointer; step: size_t = AUTO_STEP): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class function Mat<T>(rows, cols: Int; Data: TArray<T>; step: size_t = AUTO_STEP): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class function Mat<T>(rows, cols: Int; Data: TArray<TArray<T>>; step: size_t = AUTO_STEP): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // Mat(int rows, int cols, int type, void* data, size_t step=AUTO_STEP);

    // Mat(Size size, int type, void* data, size_t step=AUTO_STEP);
    // Mat(int ndims, const int* sizes, int type, void* data, const size_t* steps=0);
    // Mat(const std::vector<int>& sizes, int type, void* data, const size_t* steps=0);
    // Mat(const Mat& m, const Range& rowRange, const Range& colRange=Range::all());
    class function Mat(const m: TMat; const roi: TRect): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF} // Mat(const Mat& m, const Rect& roi);
    function Mat(const roi: TRect): TMat; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // Mat(const Mat& m, const Range* ranges);
    // Mat(const Mat& m, const std::vector<Range>& ranges);

    // default destructor
    class operator Finalize(var Dest: TMat); // ~Mat();
    class operator assign(var Dest: TMat; const [ref] Src: TMat); {$IFDEF USE_INLINE}inline; {$ENDIF}
    // Mat& operator = (const Mat& m);
    class operator Implicit(const m: TMatExpr): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}// Mat& operator = (const MatExpr& expr);
    // UMat getUMat(AccessFlag accessFlags, UMatUsageFlags usageFlags = USAGE_DEFAULT) const;
    // Mat row(int y) const;
    // Mat col(int x) const;
    // Mat rowRange(int startrow, int endrow) const;
    // Mat rowRange(const Range& r) const;
    // Mat colRange(int startcol, int endcol) const;
    // Mat colRange(const Range& r) const;
    function diag(d: Int = 0): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF} // Mat diag(int d=0) const;
    // CV_NODISCARD_STD static Mat diag(const Mat& d);
    function clone: TMat; {$IFDEF USE_INLINE}inline; {$ENDIF} // CV_NODISCARD_STD Mat clone() const;
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
    class function zeros(const rows, cols: Int; &type: Int): TMatExpr; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // CV_NODISCARD_STD static MatExpr zeros(int rows, int cols, int type);
    class function zeros(const size: TSize; &type: Int): TMatExpr; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_NODISCARD_STD static MatExpr zeros(Size size, int type);
    // CV_NODISCARD_STD static MatExpr zeros(int ndims, const int* sz, int type);
    class function ones(rows: Int; cols: Int; &type: Int): TMatExpr; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_NODISCARD_STD static MatExpr ones(int rows, int cols, int type);
    // CV_NODISCARD_STD static MatExpr ones(Size size, int type);
    class function ones(ndims: Int; const sz: pInt; &type: Int): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_NODISCARD_STD static MatExpr ones(int ndims, const int* sz, int type);
    // CV_NODISCARD_STD static MatExpr eye(int rows, int cols, int type);
    // CV_NODISCARD_STD static MatExpr eye(Size size, int type);
    procedure Create(rows, cols, &type: Int); overload; {$IFDEF USE_INLINE}inline; {$ENDIF} // void create(int rows, int cols, int type);
    procedure Create(size: TSize; &type: Int); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}// void create(Size size, int type);
    // void create(int ndims, const int* sizes, int type);
    // void create(const std::vector<int>& sizes, int type);
    procedure addref; {$IFDEF USE_INLINE}inline; {$ENDIF} // void addref();
    procedure release; {$IFDEF USE_INLINE}inline; {$ENDIF}// void release();
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
    function isContinuous: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF} // bool isContinuous() const;
    // //! returns true if the matrix is a submatrix of another matrix
    function isSubmatrix: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}         // bool isSubmatrix() const;
    function elemSize: size_t; {$IFDEF USE_INLINE}inline; {$ENDIF}          // size_t elemSize() const;
    function elemSize1: size_t; {$IFDEF USE_INLINE}inline; {$ENDIF}         // size_t elemSize1() const;
    function &type: Int; {$IFDEF USE_INLINE}inline; {$ENDIF}                // int type() const;
    function depth: Int; {$IFDEF USE_INLINE}inline; {$ENDIF}                // int depth() const;
    function channels: Int; {$IFDEF USE_INLINE}inline; {$ENDIF}             // int channels() const;
    function step1(i: Int = 0): size_t; {$IFDEF USE_INLINE}inline; {$ENDIF} // size_t step1(int i=0) const;
    function empty: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}               // bool empty() const;
    function total: size_t; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}   // size_t total() const;
    function total(startDim: Int; endDim: Int = INT_MAX): size_t; overload; {$IFDEF USE_INLINE}inline; {$ENDIF} // size_t total(int startDim, int endDim=INT_MAX) const;
    function checkVector(elemChannels: Int; depth: Int = -1; requireContinuous: BOOL = true): Int; {$IFDEF USE_INLINE}inline; {$ENDIF} // int checkVector(int elemChannels, int depth=-1, bool requireContinuous=true) const;
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
    class operator LogicalNot(const m: TMat): TMatExpr; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function at<T>(const i0: Int): T; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function at<T>(const i0, i1: Int): T; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function pT<T>(const i0: Int): Pointer; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function pT<T>(const i0, i1: Int): Pointer; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure st<T>(const i0, i1: Int; const v: T); overload;
  public
    // CVMat: TCVMat;
    flags: Int; // int flags;
    // ! the matrix dimensionality, >= 2
    dims: Int; // int dims;
    // ! the number of rows and columns or (-1, -1) when the matrix has more than 2 dimensions
    rows, cols: Int; // int rows, cols;
    // ! pointer to the data
    Data: pUChar; // uchar* data;
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

  TStdVectorMat = Vector<TMat>;
  pStdVectorMat = ^TStdVectorMat;

  pScalar = ^TScalar;

  TScalar = record
  private
{$HINTS OFF}
    v: array [0 .. 3] of double;
{$HINTS ON}
  public
    class operator Initialize(out Dest: TScalar); // Scalar_();
    class function Create(const v0, v1: double; const v2: double = 0; const v3: double = 0): TScalar; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF} // Scalar_(_Tp v0, _Tp v1, _Tp v2=0, _Tp v3=0);
    class function Create(const v0: double): TScalar; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// Scalar_(_Tp v0);
    // Scalar_(const Scalar_& s);
    // Scalar_(Scalar_&& s) CV_NOEXCEPT;
    // Scalar_& operator=(const Scalar_& s);
    // Scalar_& operator=(Scalar_&& s) CV_NOEXCEPT;
    // Scalar_(const Vec<_Tp2, cn>& v);
    // //! returns a scalar with all elements set to v0
    class function all(const v0: double): TScalar; static; {$IFDEF USE_INLINE}inline; {$ENDIF} // static Scalar_<_Tp> all(_Tp v0);
    // //! per-element product
    // Scalar_<_Tp> mul(const Scalar_<_Tp>& a, double scale=1 ) const;
    // //! returns (v0, -v1, -v2, -v3)
    // Scalar_<_Tp> conj() const;
    // //! returns true iff v1 == v2 == v3 == 0
    // bool isReal() const;
    class operator Implicit(const v0: double): TScalar; {$IFDEF USE_INLINE}inline; {$ENDIF}
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
    class function InputArray(const m: TMat): TInputArray; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// _InputArray(const Mat& m);
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
    function getMat(idx: Int = -1): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}     // Mat getMat(int idx=-1) const;
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
    function isMat: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF} // bool isMat() const;
    // bool isUMat() const;
    // bool isMatVector() const;
    // bool isUMatVector() const;
    // bool isMatx() const;
    // bool isVector() const;
    // bool isGpuMat() const;
    // bool isGpuMatVector() const;
    class operator Finalize(var Dest: TInputArray); // ~_InputArray();
    //
    class function noArray: TInputArray; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const m: TMat): TInputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const m: TMatExpr): TInputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // class operator Implicit(const IA: TInputArray): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector < Vector < TPoint >> ): TInputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector<TPoint2f>): TInputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector<uchar>): TInputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // private
  public
{$HINTS OFF}
    flags: Int;   // int flags;
    Obj: Pointer; // void* obj;
    sz: TSize;    // Size sz;
{$HINTS ON}
  end;

  TInputArrayOfArrays = type TInputArray;

  TOutputArray = type TInputArray;
  pOutputArray = ^TOutputArray;

  TOutputArrayHelper = record helper for TOutputArray
    // private
    // InputArray: TInputArray;
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
    // class operator Initialize(out Dest: TOutputArray); // _OutputArray();
    // _OutputArray(int _flags, void* _obj);
    class function OutputArray(const m: TMat): TOutputArray; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF} // _OutputArray(Mat& m);
    class function OutputArray(const vec: TStdVectorMat): TOutputArray; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}// _OutputArray(std::vector<Mat>& vec);
    // _OutputArray(cuda::GpuMat& d_mat);
    // _OutputArray(std::vector<cuda::GpuMat>& d_mat);
    // _OutputArray(ogl::Buffer& buf);
    // _OutputArray(cuda::HostMem& cuda_mem);
    // _OutputArray(std::vector<bool>& vec) = delete;  // not supported
    // _OutputArray(std::vector<std::vector<bool> >&) = delete;  // not supported
    // _OutputArray(UMat& m);
    // _OutputArray(std::vector<UMat>& vec);
    // class operator Finalize(var Dest: TOutputArray); // destructor

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

    class function noArray: TOutputArray; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const m: TMat): TOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const m: TStdVectorMat): TOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const OA: TOutputArray): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector<TPoint>): TOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector<TPoint2f>): TOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector<uchar>): TOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector<float>): TOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  TOutputArrayOfArrays = TOutputArray;
  pOutputArrayOfArrays = ^TOutputArrayOfArrays;

  TInputOutputArray = type TOutputArray;
  pInputOutputArray = ^TInputOutputArray;

  TInputOutputArrayHelper = record helper for TInputOutputArray
    // private
    // {$HINTS OFF}
    // OutputArray: TOutputArray;
    // {$HINTS ON}
  public
    // class operator Initialize(out Dest: TInputOutputArray); // _InputOutputArray();
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
    // class operator Finalize(var Dest: TInputOutputArray); // destructor
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

    class function noArray: TInputOutputArray; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const m: TMat): TInputOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const IOA: TInputOutputArray): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const IOA: TInputOutputArray): TInputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const v: Vector<TPoint2f>): TInputOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  // function noArray(): TInputOutputArray; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$ENDREGION 'mat.hpp'}
  //
{$REGION 'matx.hpp'}

type
  TMatX<T> = record

  end;

  Vec6f = array [0 .. 5] of float;
  pVec6f = ^Vec6f;

  Vec3b = record
  private
    _Data: array [0 .. 2] of uchar;
    function getItem(const index: integer): uchar; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure setItem(const index: integer; const Value: uchar); {$IFDEF USE_INLINE}inline; {$ENDIF}
  public
    property Items[const Index: integer]: uchar read getItem write setItem; default;
    class operator Implicit(const a: TArray<uchar>): Vec3b; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;

  pVec3b = ^Vec3b;

  Vec2f = array [0 .. 1] of float;
  pVec2f = ^Vec2f;

{$ENDREGION 'matx.hpp'}
  //
{$REGION 'core.hpp'}
  (* * @brief Swaps two matrices *)
  // CV_EXPORTS void swap(Mat& a, Mat& b);
  // 6587
  // ?swap@cv@@YAXAEAVMat@1@0@Z
  // void cv::swap(class cv::Mat &,class cv::Mat &)
procedure swap(a, b: TMat); external opencv_world_dll name '?swap@cv@@YAXAEAVMat@1@0@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

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
function borderInterpolate(p, len, borderType: Int): Int; external opencv_world_dll name '?borderInterpolate@cv@@YAHHHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

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
  external opencv_world_dll name '?copyMakeBorder@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHHHHAEBV?$Scalar_@N@1@@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
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
procedure addWeighted(src1: TInputArray; alpha: double; src2: TInputArray; beta, gamma: double; dst: TOutputArray; dtype: Int = -1); external opencv_world_dll index 3519
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * @brief  Inverts every bit of an array.

  The function cv::bitwise_not calculates per-element bit-wise inversion of the input
  array:
  \f[\texttt{dst} (I) =  \neg \texttt{src} (I)\f]
  In case of a floating-point input array, its machine-specific bit
  representation (usually IEEE754-compliant) is used for the operation. In
  case of multi-channel arrays, each channel is processed independently.
  @param src input array.
  @param dst output array that has the same size and type as the input
  array.
  @param mask optional operation mask, 8-bit single channel array, that
  specifies elements of the output array to be changed.
*)
// CV_EXPORTS_W void bitwise_not(InputArray src, OutputArray dst,
// InputArray mask = noArray());
// 3629
// ?bitwise_not@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@0@Z
// void cv::bitwise_not(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::_InputArray const &)
procedure _bitwise_not(Src: TInputArray; dst: TOutputArray; mask: TInputArray { = noArray() } ); external opencv_world_dll index 3629 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure bitwise_not(Src: TInputArray; dst: TOutputArray; mask: TInputArray { = noArray() } ); overload; {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure bitwise_not(Src: TInputArray; dst: TOutputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF} overload;

(* * @overload
  @param m input multi-channel array.
  @param mv output vector of arrays; the arrays themselves are reallocated, if needed.
*)
// CV_EXPORTS_W void split(InputArray m, OutputArrayOfArrays mv);
// 6515
// ?split@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@@Z
// void cv::split(class cv::_InputArray const &,class cv::_OutputArray const &)
procedure split(const m: TInputArray; const mv: TOutputArrayOfArrays); external opencv_world_dll index 6515 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Normalizes the norm or value range of an array.

  The function cv::normalize normalizes scale and shift the input array elements so that
  \f[\| \texttt{dst} \| _{L_p}= \texttt{alpha}\f]
  (where p=Inf, 1 or 2) when normType=NORM_INF, NORM_L1, or NORM_L2, respectively; or so that
  \f[\min _I  \texttt{dst} (I)= \texttt{alpha} , \, \, \max _I  \texttt{dst} (I)= \texttt{beta}\f]

  when normType=NORM_MINMAX (for dense arrays only). The optional mask specifies a sub-array to be
  normalized. This means that the norm or min-n-max are calculated over the sub-array, and then this
  sub-array is modified to be normalized. If you want to only use the mask to calculate the norm or
  min-max but modify the whole array, you can use norm and Mat::convertTo.

  In case of sparse matrices, only the non-zero values are analyzed and transformed. Because of this,
  the range transformation for sparse matrices is not allowed since it can shift the zero level.

  Possible usage with some positive example data:
  @code{.cpp}
  vector<double> positiveData = { 2.0, 8.0, 10.0 };
  vector<double> normalizedData_l1, normalizedData_l2, normalizedData_inf, normalizedData_minmax;

  // Norm to probability (total count)
  // sum(numbers) = 20.0
  // 2.0      0.1     (2.0/20.0)
  // 8.0      0.4     (8.0/20.0)
  // 10.0     0.5     (10.0/20.0)
  normalize(positiveData, normalizedData_l1, 1.0, 0.0, NORM_L1);

  // Norm to unit vector: ||positiveData|| = 1.0
  // 2.0      0.15
  // 8.0      0.62
  // 10.0     0.77
  normalize(positiveData, normalizedData_l2, 1.0, 0.0, NORM_L2);

  // Norm to max element
  // 2.0      0.2     (2.0/10.0)
  // 8.0      0.8     (8.0/10.0)
  // 10.0     1.0     (10.0/10.0)
  normalize(positiveData, normalizedData_inf, 1.0, 0.0, NORM_INF);

  // Norm to range [0.0;1.0]
  // 2.0      0.0     (shift to left border)
  // 8.0      0.75    (6.0/8.0)
  // 10.0     1.0     (shift to right border)
  normalize(positiveData, normalizedData_minmax, 1.0, 0.0, NORM_MINMAX);
  @endcode

  @param src input array.
  @param dst output array of the same size as src .
  @param alpha norm value to normalize to or the lower range boundary in case of the range
  normalization.
  @param beta upper range boundary in case of the range normalization; it is not used for the norm
  normalization.
  @param norm_type normalization type (see cv::NormTypes).
  @param dtype when negative, the output array has the same type as src; otherwise, it has the same
  number of channels as src and the depth =CV_MAT_DEPTH(dtype).
  @param mask optional operation mask.
  @sa norm, Mat::convertTo, SparseMat::convertTo
*)
// CV_EXPORTS_W void normalize( InputArray src, InputOutputArray dst, double alpha = 1, double beta = 0,
// int norm_type = NORM_L2, int dtype = -1, InputArray mask = noArray());
// 5761
// ?normalize@cv@@YAXAEBV_InputArray@1@AEBV_InputOutputArray@1@NNHH0@Z
// void cv::normalize(class cv::_InputArray const &,class cv::_InputOutputArray const &,double,double,int,int,class cv::_InputArray const &)
procedure _normalize(                  //
  Src: TInputArray;                    //
  dst: TInputOutputArray;              //
  alpha: double { = 1 };               //
  beta: double { = 0 };                //
  norm_type: Int { = int(NORM_L2) };   //
  dtype: Int { = -1 };                 //
  mask: TInputArray { = noArray() } ); //
  external opencv_world_dll index 5761 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

procedure normalize(const Src: TInputArray; const dst: TInputOutputArray; const alpha: double { = 1 }; beta: double { = 0 }; norm_type: NormTypes { = NORM_L2 }; dtype: Int { = -1 };
  const mask: TInputArray { = noArray() } ); overload; {$IFDEF USE_INLINE}inline; {$ENDIF} overload;
procedure normalize(const Src: TInputArray; const dst: TInputOutputArray; const alpha: double = 1; beta: double = 0; norm_type: NormTypes = NORM_L2; dtype: Int = -1); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF} overload;

Type
  (* * @brief Random Number Generator

    Random number generator. It encapsulates the state (currently, a 64-bit
    integer) and has methods to return scalar random values and to fill
    arrays with random values. Currently it supports uniform and Gaussian
    (normal) distributions. The generator uses Multiply-With-Carry
    algorithm, introduced by G. Marsaglia (
    <http://en.wikipedia.org/wiki/Multiply-with-carry> ).
    Gaussian-distribution random numbers are generated using the Ziggurat
    algorithm ( <http://en.wikipedia.org/wiki/Ziggurat_algorithm> ),
    introduced by G. Marsaglia and W. W. Tsang.
  *)
  pRNG = ^TRNG;

  TRNG = record
  public const
    RNG_UNIFORM = 0;
    RNG_NORMAL  = 1;
  public

    (* * @brief constructor
      These are the RNG constructors. The first form sets the state to some
      pre-defined value, equal to 2\*\*32-1 in the current implementation. The
      second form sets the state to the specified value. If you passed state=0
      , the constructor uses the above default value instead to avoid the
      singular random number sequence, consisting of all zeros.
    *)
    class operator Initialize(out Dest: TRNG); // RNG();
    (* * @overload
      @param state 64-bit value used to initialize the RNG.
    *)
    class function RNG(state: UInt64): TRNG; static; {$IFDEF USE_INLINE}inline; {$ENDIF}    // RNG(UInt64 state);
    class operator Implicit(const u: UInt64): TRNG; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* *The method updates the state using the MWC algorithm and returns the
      next 32-bit random number. *)
    // unsigned next();

    (* *Each of the methods updates the state using the MWC algorithm and
      returns the next random number of the specified type. In case of integer
      types, the returned number is from the available value range for the
      specified type. In case of floating-point types, the returned value is
      from [0,1) range.
    *)
    // operator uchar();
    (* * @overload *)
    // operator schar();
    (* * @overload *)
    // operator ushort();
    (* * @overload *)
    // operator short();
    (* * @overload *)
    class operator Implicit(const p: TRNG): unsigned; {$IFDEF USE_INLINE}inline; {$ENDIF} // operator unsigned();
    (* * @overload *)
    // operator Int();
    (* * @overload *)
    // operator float();
    (* * @overload *)
    // operator double();

    (* * @brief returns a random integer sampled uniformly from [0, N).

      The methods transform the state using the MWC algorithm and return the
      next random number. The first form is equivalent to RNG::next . The
      second form returns the random number modulo N , which means that the
      result is in the range [0, N) .
    *)
    // unsigned operator()();
    (* * @overload
      @param N upper non-inclusive boundary of the returned random number.
    *)
    // unsigned operator()(unsigned n);

    (* * @brief returns uniformly distributed integer random number from [a,b) range

      The methods transform the state using the MWC algorithm and return the
      next uniformly-distributed random number of the specified type, deduced
      from the input parameter type, from the range [a, b) . There is a nuance
      illustrated by the following sample:

      @code{.cpp}
      RNG rng;

      // always produces 0
      double a = rng.uniform(0, 1);

      // produces double from [0, 1)
      double a1 = rng.uniform((double)0, (double)1);

      // produces float from [0, 1)
      float b = rng.uniform(0.f, 1.f);

      // produces double from [0, 1)
      double c = rng.uniform(0., 1.);

      // may cause compiler error because of ambiguity:
      //  RNG::uniform(0, (int)0.999999)? or RNG::uniform((double)0, 0.99999)?
      double d = rng.uniform(0, 0.999999);
      @endcode

      The compiler does not take into account the type of the variable to
      which you assign the result of RNG::uniform . The only thing that
      matters to the compiler is the type of a and b parameters. So, if you
      want a floating-point random number, but the range boundaries are
      integer numbers, either put dots in the end, if they are constants, or
      use explicit type cast operators, as in the a1 initialization above.
      @param a lower inclusive boundary of the returned random number.
      @param b upper non-inclusive boundary of the returned random number.
    *)
    function UNIFORM(a, b: Int): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}// Int UNIFORM(Int a, Int b);
    (* * @overload *)
    // float UNIFORM(float a, float b);
    (* * @overload *)
    // double UNIFORM(double a, double b);

    (* * @brief Fills arrays with random numbers.

      @param mat 2D or N-dimensional matrix; currently matrices with more than
      4 channels are not supported by the methods, use Mat::reshape as a
      possible workaround.
      @param distType distribution type, RNG::UNIFORM or RNG::NORMAL.
      @param a first distribution parameter; in case of the uniform
      distribution, this is an inclusive lower boundary, in case of the normal
      distribution, this is a mean value.
      @param b second distribution parameter; in case of the uniform
      distribution, this is a non-inclusive upper boundary, in case of the
      normal distribution, this is a standard deviation (diagonal of the
      standard deviation matrix or the full standard deviation matrix).
      @param saturateRange pre-saturation flag; for uniform distribution only;
      if true, the method will first convert a and b to the acceptable value
      range (according to the mat datatype) and then will generate uniformly
      distributed random numbers within the range [saturate(a), saturate(b)),
      if saturateRange=false, the method will generate uniformly distributed
      random numbers in the original range [a, b) and then will saturate them,
      it means, for example, that
      <tt>theRNG().fill(mat_8u, RNG::UNIFORM, -DBL_MAX, DBL_MAX)</tt> will likely
      produce array mostly filled with 0's and 255's, since the range (0, 255)
      is significantly smaller than [-DBL_MAX, DBL_MAX).

      Each of the methods fills the matrix with the random values from the
      specified distribution. As the new numbers are generated, the RNG state
      is updated accordingly. In case of multiple-channel images, every
      channel is filled independently, which means that RNG cannot generate
      samples from the multi-dimensional Gaussian distribution with
      non-diagonal covariance matrix directly. To do that, the method
      generates samples from multi-dimensional standard Gaussian distribution
      with zero mean and identity covariation matrix, and then transforms them
      using transform to get samples from the specified Gaussian distribution.
    *)
    // void fill(InputOutputArray Mat, Int distType, InputArray a, InputArray b, BOOL saturateRange = false);

    (* * @brief Returns the next random number sampled from the Gaussian distribution
      @param sigma standard deviation of the distribution.

      The method transforms the state using the MWC algorithm and returns the
      next random number from the Gaussian distribution N(0,sigma) . That is,
      the mean value of the returned random numbers is zero and the standard
      deviation is the specified sigma .
    *)
    // double gaussian(double sigma);
  public
    state: UInt64; // UInt64 state;

    // BOOL operator==(const RNG & other)  const;
  end;

  (* * @brief Finds the global minimum and maximum in an array.

    The function cv::minMaxLoc finds the minimum and maximum element values and their positions. The
    extremums are searched across the whole array or, if mask is not an empty array, in the specified
    array region.

    The function do not work with multi-channel arrays. If you need to find minimum or maximum
    elements across all the channels, use Mat::reshape first to reinterpret the array as
    single-channel. Or you may extract the particular channel using either extractImageCOI , or
    mixChannels , or split .
    @param src input single-channel array.
    @param minVal pointer to the returned minimum value; NULL is used if not required.
    @param maxVal pointer to the returned maximum value; NULL is used if not required.
    @param minLoc pointer to the returned minimum location (in 2D case); NULL is used if not required.
    @param maxLoc pointer to the returned maximum location (in 2D case); NULL is used if not required.
    @param mask optional mask used to select a sub-array.
    @sa max, min, compare, inRange, extractImageCOI, mixChannels, split, Mat::reshape
  *)

  // CV_EXPORTS_W void minMaxLoc(InputArray src, CV_OUT double* minVal,
  // CV_OUT double* maxVal = 0, CV_OUT Point* minLoc = 0,
  // CV_OUT Point* maxLoc = 0, InputArray mask = noArray());
  // 5673
  // ?minMaxLoc@cv@@YAXAEBV_InputArray@1@PEAN1PEAV?$Point_@H@1@20@Z
  // void cv::minMaxLoc(class cv::_InputArray const &,double *,double *,class cv::Point_<int> *,class cv::Point_<int> *,class cv::_InputArray const &)
procedure minMaxLoc(Src: TInputArray; Var minVal: double; const maxVal: pDouble { = nil }; const minLoc: pPoint { = nil }; const maxLoc: pPoint { = nil }; mask: TInputArray { = noArray() }
  ); overload; external opencv_world_dll
// name '?minMaxLoc@cv@@YAXAEBV_InputArray@1@PEAN1PEAV?$Point_@H@1@20@Z'
  index 5673
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil }; var maxLoc: TPoint { = nil };
  const mask: TInputArray { = noArray() } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil }; var maxLoc: TPoint { = nil } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure minMaxLoc(const Src: TInputArray; Var minVal: double); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Calculates the per-element sum of two arrays or an array and a scalar.

  The function add calculates:
  - Sum of two arrays when both input arrays have the same size and the same number of channels:
  \f[\texttt{dst}(I) =  \texttt{saturate} ( \texttt{src1}(I) +  \texttt{src2}(I)) \quad \texttt{if mask}(I) \ne0\f]
  - Sum of an array and a scalar when src2 is constructed from Scalar or has the same number of
  elements as `src1.channels()`:
  \f[\texttt{dst}(I) =  \texttt{saturate} ( \texttt{src1}(I) +  \texttt{src2} ) \quad \texttt{if mask}(I) \ne0\f]
  - Sum of a scalar and an array when src1 is constructed from Scalar or has the same number of
  elements as `src2.channels()`:
  \f[\texttt{dst}(I) =  \texttt{saturate} ( \texttt{src1} +  \texttt{src2}(I) ) \quad \texttt{if mask}(I) \ne0\f]
  where `I` is a multi-dimensional index of array elements. In case of multi-channel arrays, each
  channel is processed independently.

  The first function in the list above can be replaced with matrix expressions:
  @code{.cpp}
  dst = src1 + src2;
  dst += src1; // equivalent to add(dst, src1, dst);
  @endcode
  The input arrays and the output array can all have the same or different depths. For example, you
  can add a 16-bit unsigned array to a 8-bit signed array and store the sum as a 32-bit
  floating-point array. Depth of the output array is determined by the dtype parameter. In the second
  and third cases above, as well as in the first case, when src1.depth() == src2.depth(), dtype can
  be set to the default -1. In this case, the output array will have the same depth as the input
  array, be it src1, src2 or both.
  @note Saturation is not applied when the output array has the depth CV_32S. You may even get
  result of an incorrect sign in the case of overflow.
  @param src1 first input array or a scalar.
  @param src2 second input array or a scalar.
  @param dst output array that has the same size and number of channels as the input array(s); the
  depth is defined by dtype or src1/src2.
  @param mask optional operation mask - 8-bit single channel array, that specifies elements of the
  output array to be changed.
  @param dtype optional depth of the output array (see the discussion below).
  @sa subtract, addWeighted, scaleAdd, Mat::convertTo
*)
// CV_EXPORTS_W void add(InputArray src1, InputArray src2, OutputArray dst,
// InputArray mask = noArray(), int dtype = -1);
// 3488
// ?add@cv@@YAXAEBV_InputArray@1@0AEBV_OutputArray@1@0H@Z
// void cv::add(class cv::_InputArray const &,class cv::_InputArray const &,class cv::_OutputArray const &,class cv::_InputArray const &,int)
procedure add(src1: TInputArray; src2: TInputArray; dst: TOutputArray; mask: TInputArray { = noArray() }; dtype: Int { = -1 } ); overload; external opencv_world_dll
// name '?add@cv@@YAXAEBV_InputArray@1@0AEBV_OutputArray@1@0H@Z'
  index 3488{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
//
{$ENDREGION 'core.hpp'}
//
{$REGION 'imgcodecs.hpp'}

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
  external opencv_world_dll name '?imread@cv@@YA?AVMat@1@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@H@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

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
function waitKey(delay: Int = 0): Int; external opencv_world_dll name '?waitKey@cv@@YAHH@Z' {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

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
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{$ENDREGION 'imgcodecs.hpp'}
//
{$REGION 'highgui.hpp'}

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
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//
(* * @brief Creates a trackbar and attaches it to the specified window.

  The function createTrackbar creates a trackbar (a slider or range control) with the specified name
  and range, assigns a variable value to be a position synchronized with the trackbar and specifies
  the callback function onChange to be called on the trackbar position change. The created trackbar is
  displayed in the specified window winname.

  @note

  [__Qt Backend Only__] winname can be empty if the trackbar should be attached to the
  control panel.

  Clicking the label of each trackbar enables editing the trackbar values manually.

  @param trackbarname Name of the created trackbar.
  @param winname Name of the window that will be used as a parent of the created trackbar.
  @param value Optional pointer to an integer variable whose value reflects the position of the
  slider. Upon creation, the slider position is defined by this variable.
  @param count Maximal position of the slider. The minimal position is always 0.
  @param onChange Pointer to the function to be called every time the slider changes position. This
  function should be prototyped as void Foo(int,void\ * );
  , where the first parameter is the trackbar position and the second parameter is the user data(see the next parameter).If the callback is the NULL Pointer, no callbacks are called,
  but only value is updated.@param userdata user data that is passed as is to the callback.It can be used to handle trackbar events without using global variables.
*)
// CV_EXPORTS int createTrackbar(const String& trackbarname, const String& winname,
// int* value, int count,
// TrackbarCallback onChange = 0,
// void* userdata = 0);
// 4302
// ?createTrackbar@cv@@YAHAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@0PEAHHP6AXHPEAX@Z2@Z
// int cv::createTrackbar(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int *,int,void (*)(int,void *),void *)
Type
  TTrackbarCallback = procedure(pos: Int; userdata: Pointer);
function createTrackbar(const trackbarname: CppString; const winname: CppString; Value: pInt; COUNT: Int; onChange: TTrackbarCallback = nil; userdata: Pointer = nil): Int;
  external opencv_world_dll index 4302{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Moves the window to the specified position

  @param winname Name of the window.
  @param x The new x-coordinate of the window.
  @param y The new y-coordinate of the window.
*)
// CV_EXPORTS_W void moveWindow(const String& winname, int x, int y);
// 5691
// ?moveWindow@cv@@YAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HH@Z
// void cv::moveWindow(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,int)
procedure moveWindow(const winname: CppString; x, y: Int); external opencv_world_dll index 5691{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Destroys the specified window.
  The function destroyWindow destroys the window with the given name.
  @param winname Name of the window to be destroyed.
*)
// CV_EXPORTS_W void destroyWindow(const String& winname);
// 4411
// ?destroyWindow@cv@@YAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z
// void cv::destroyWindow(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
procedure destroyWindow(const winname: CppString); external opencv_world_dll index 4411{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * @brief Resizes the window to the specified size

  @note

  -   The specified window size is for the image area. Toolbars are not counted.
  -   Only windows created without cv::WINDOW_AUTOSIZE flag can be resized.

  @param winname Window name.
  @param width The new window width.
  @param height The new window height.
*)
// CV_EXPORTS_W void resizeWindow(const String& winname, int width, int height);
// 6154
// ?resizeWindow@cv@@YAXAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HH@Z
// dvoid cv::resizeWindow(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,int)
procedure resizeWindow(const winname: CppString; width, height: Int); external opencv_world_dll index 6154{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{$ENDREGION 'highgui.hpp'}
//
{$REGION 'imgproc.hpp'}

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

  (* * Possible set of marker types used for the cv::drawMarker function
    @ingroup imgproc_draw
  *)
  MarkerTypes = (            //
    MARKER_CROSS = 0,        // !< A crosshair marker shape
    MARKER_TILTED_CROSS = 1, // !< A 45 degree tilted crosshair marker shape
    MARKER_STAR = 2,         // !< A star marker shape, combination of cross and tilted cross
    MARKER_DIAMOND = 3,      // !< A diamond marker shape
    MARKER_SQUARE = 4,       // !< A square marker shape
    MARKER_TRIANGLE_UP = 5,  // !< An upwards pointing triangle marker shape
    MARKER_TRIANGLE_DOWN = 6 // !< A downwards pointing triangle marker shape
    );

  ColorConversionCodes = (           //
    COLOR_BGR2BGRA = 0,              // !< add alpha channel to RGB or BGR image
    COLOR_RGB2RGBA = COLOR_BGR2BGRA, //
    //
    COLOR_BGRA2BGR = 1,              // !< remove alpha channel from RGB or BGR image
    COLOR_RGBA2RGB = COLOR_BGRA2BGR, //
    //
    COLOR_BGR2RGBA = 2,              // !< convert between RGB and BGR color spaces (with or without alpha channel)
    COLOR_RGB2BGRA = COLOR_BGR2RGBA, //
    //
    COLOR_RGBA2BGR = 3,              //
    COLOR_BGRA2RGB = COLOR_RGBA2BGR, //
    //
    COLOR_BGR2RGB = 4,             //
    COLOR_RGB2BGR = COLOR_BGR2RGB, //
    //
    COLOR_BGRA2RGBA = 5,               //
    COLOR_RGBA2BGRA = COLOR_BGRA2RGBA, //
    //
    COLOR_BGR2GRAY = 6,                // !< convert between RGB/BGR and grayscale, @ref color_convert_rgb_gray "color conversions"
    COLOR_RGB2GRAY = 7,                //
    COLOR_GRAY2BGR = 8,                //
    COLOR_GRAY2RGB = COLOR_GRAY2BGR,   //
    COLOR_GRAY2BGRA = 9,               //
    COLOR_GRAY2RGBA = COLOR_GRAY2BGRA, //
    COLOR_BGRA2GRAY = 10,              //
    COLOR_RGBA2GRAY = 11,              //
    //
    COLOR_BGR2BGR565 = 12,  // !< convert between RGB/BGR and BGR565 (16-bit images)
    COLOR_RGB2BGR565 = 13,  //
    COLOR_BGR5652BGR = 14,  //
    COLOR_BGR5652RGB = 15,  //
    COLOR_BGRA2BGR565 = 16, //
    COLOR_RGBA2BGR565 = 17, //
    COLOR_BGR5652BGRA = 18, //
    COLOR_BGR5652RGBA = 19, //
    //
    COLOR_GRAY2BGR565 = 20, // !< convert between grayscale to BGR565 (16-bit images)
    COLOR_BGR5652GRAY = 21, //
    //
    COLOR_BGR2BGR555 = 22,  // !< convert between RGB/BGR and BGR555 (16-bit images)
    COLOR_RGB2BGR555 = 23,  //
    COLOR_BGR5552BGR = 24,  //
    COLOR_BGR5552RGB = 25,  //
    COLOR_BGRA2BGR555 = 26, //
    COLOR_RGBA2BGR555 = 27, //
    COLOR_BGR5552BGRA = 28, //
    COLOR_BGR5552RGBA = 29, //
    //
    COLOR_GRAY2BGR555 = 30, // !< convert between grayscale and BGR555 (16-bit images)
    COLOR_BGR5552GRAY = 31, //
    //
    COLOR_BGR2XYZ = 32, // !< convert RGB/BGR to CIE XYZ, @ref color_convert_rgb_xyz "color conversions"
    COLOR_RGB2XYZ = 33, //
    COLOR_XYZ2BGR = 34, //
    COLOR_XYZ2RGB = 35, //
    //
    COLOR_BGR2YCrCb = 36, // !< convert RGB/BGR to luma-chroma (aka YCC), @ref color_convert_rgb_ycrcb "color conversions"
    COLOR_RGB2YCrCb = 37, //
    COLOR_YCrCb2BGR = 38, //
    COLOR_YCrCb2RGB = 39, //
    //
    COLOR_BGR2HSV = 40, // !< convert RGB/BGR to HSV (hue saturation value) with H range 0..180 if 8 bit image, @ref color_convert_rgb_hsv "color conversions"
    COLOR_RGB2HSV = 41, //
    //
    COLOR_BGR2Lab = 44, // !< convert RGB/BGR to CIE Lab, @ref color_convert_rgb_lab "color conversions"
    COLOR_RGB2Lab = 45, //
    //
    COLOR_BGR2Luv = 50, // !< convert RGB/BGR to CIE Luv, @ref color_convert_rgb_luv "color conversions"
    COLOR_RGB2Luv = 51, //
    COLOR_BGR2HLS = 52, // !< convert RGB/BGR to HLS (hue lightness saturation) with H range 0..180 if 8 bit image, @ref color_convert_rgb_hls "color conversions"
    COLOR_RGB2HLS = 53, //
    //
    COLOR_HSV2BGR = 54, // !< backward conversions HSV to RGB/BGR with H range 0..180 if 8 bit image
    COLOR_HSV2RGB = 55, //
    //
    COLOR_Lab2BGR = 56, //
    COLOR_Lab2RGB = 57, //
    COLOR_Luv2BGR = 58, //
    COLOR_Luv2RGB = 59, //
    COLOR_HLS2BGR = 60, // !< backward conversions HLS to RGB/BGR with H range 0..180 if 8 bit image
    COLOR_HLS2RGB = 61, //
    //
    COLOR_BGR2HSV_FULL = 66, // !< convert RGB/BGR to HSV (hue saturation value) with H range 0..255 if 8 bit image, @ref color_convert_rgb_hsv "color conversions"
    COLOR_RGB2HSV_FULL = 67, //
    COLOR_BGR2HLS_FULL = 68, // !< convert RGB/BGR to HLS (hue lightness saturation) with H range 0..255 if 8 bit image, @ref color_convert_rgb_hls "color conversions"
    COLOR_RGB2HLS_FULL = 69, //

    COLOR_HSV2BGR_FULL = 70, // !< backward conversions HSV to RGB/BGR with H range 0..255 if 8 bit image
    COLOR_HSV2RGB_FULL = 71, //
    COLOR_HLS2BGR_FULL = 72, // !< backward conversions HLS to RGB/BGR with H range 0..255 if 8 bit image
    COLOR_HLS2RGB_FULL = 73, //
    //
    COLOR_LBGR2Lab = 74, //
    COLOR_LRGB2Lab = 75, //
    COLOR_LBGR2Luv = 76, //
    COLOR_LRGB2Luv = 77, //
    //
    COLOR_Lab2LBGR = 78, //
    COLOR_Lab2LRGB = 79, //
    COLOR_Luv2LBGR = 80, //
    COLOR_Luv2LRGB = 81, //
    //
    COLOR_BGR2YUV = 82, // !< convert between RGB/BGR and YUV
    COLOR_RGB2YUV = 83, //
    COLOR_YUV2BGR = 84, //
    COLOR_YUV2RGB = 85, //
    //
    // ! YUV 4:2:0 family to RGB
    COLOR_YUV2RGB_NV12 = 90,                 //
    COLOR_YUV2BGR_NV12 = 91,                 //
    COLOR_YUV2RGB_NV21 = 92,                 //
    COLOR_YUV2BGR_NV21 = 93,                 //
    COLOR_YUV420sp2RGB = COLOR_YUV2RGB_NV21, //
    COLOR_YUV420sp2BGR = COLOR_YUV2BGR_NV21, //

    COLOR_YUV2RGBA_NV12 = 94,                  //
    COLOR_YUV2BGRA_NV12 = 95,                  //
    COLOR_YUV2RGBA_NV21 = 96,                  //
    COLOR_YUV2BGRA_NV21 = 97,                  //
    COLOR_YUV420sp2RGBA = COLOR_YUV2RGBA_NV21, //
    COLOR_YUV420sp2BGRA = COLOR_YUV2BGRA_NV21, //
    //
    COLOR_YUV2RGB_YV12 = 98,                 //
    COLOR_YUV2BGR_YV12 = 99,                 //
    COLOR_YUV2RGB_IYUV = 100,                //
    COLOR_YUV2BGR_IYUV = 101,                //
    COLOR_YUV2RGB_I420 = COLOR_YUV2RGB_IYUV, //
    COLOR_YUV2BGR_I420 = COLOR_YUV2BGR_IYUV, //
    COLOR_YUV420p2RGB = COLOR_YUV2RGB_YV12,  //
    COLOR_YUV420p2BGR = COLOR_YUV2BGR_YV12,  //
    //
    COLOR_YUV2RGBA_YV12 = 102,                 //
    COLOR_YUV2BGRA_YV12 = 103,                 //
    COLOR_YUV2RGBA_IYUV = 104,                 //
    COLOR_YUV2BGRA_IYUV = 105,                 //
    COLOR_YUV2RGBA_I420 = COLOR_YUV2RGBA_IYUV, //
    COLOR_YUV2BGRA_I420 = COLOR_YUV2BGRA_IYUV, //
    COLOR_YUV420p2RGBA = COLOR_YUV2RGBA_YV12,  //
    COLOR_YUV420p2BGRA = COLOR_YUV2BGRA_YV12,  //
    //
    COLOR_YUV2GRAY_420 = 106,                 //
    COLOR_YUV2GRAY_NV21 = COLOR_YUV2GRAY_420, //
    COLOR_YUV2GRAY_NV12 = COLOR_YUV2GRAY_420, //
    COLOR_YUV2GRAY_YV12 = COLOR_YUV2GRAY_420, //
    COLOR_YUV2GRAY_IYUV = COLOR_YUV2GRAY_420, //
    COLOR_YUV2GRAY_I420 = COLOR_YUV2GRAY_420, //
    COLOR_YUV420sp2GRAY = COLOR_YUV2GRAY_420, //
    COLOR_YUV420p2GRAY = COLOR_YUV2GRAY_420,  //
    //
    // ! YUV 4:2:2 family to RGB
    COLOR_YUV2RGB_UYVY = 107, //
    COLOR_YUV2BGR_UYVY = 108, //
    // COLOR_YUV2RGB_VYUY = 109,
    // COLOR_YUV2BGR_VYUY = 110,
    COLOR_YUV2RGB_Y422 = COLOR_YUV2RGB_UYVY, //
    COLOR_YUV2BGR_Y422 = COLOR_YUV2BGR_UYVY, //
    COLOR_YUV2RGB_UYNV = COLOR_YUV2RGB_UYVY, //
    COLOR_YUV2BGR_UYNV = COLOR_YUV2BGR_UYVY, //
    //
    COLOR_YUV2RGBA_UYVY = 111, //
    COLOR_YUV2BGRA_UYVY = 112, //
    // COLOR_YUV2RGBA_VYUY = 113,
    // COLOR_YUV2BGRA_VYUY = 114,
    COLOR_YUV2RGBA_Y422 = COLOR_YUV2RGBA_UYVY, //
    COLOR_YUV2BGRA_Y422 = COLOR_YUV2BGRA_UYVY, //
    COLOR_YUV2RGBA_UYNV = COLOR_YUV2RGBA_UYVY, //
    COLOR_YUV2BGRA_UYNV = COLOR_YUV2BGRA_UYVY, //
    //
    COLOR_YUV2RGB_YUY2 = 115,                //
    COLOR_YUV2BGR_YUY2 = 116,                //
    COLOR_YUV2RGB_YVYU = 117,                //
    COLOR_YUV2BGR_YVYU = 118,                //
    COLOR_YUV2RGB_YUYV = COLOR_YUV2RGB_YUY2, //
    COLOR_YUV2BGR_YUYV = COLOR_YUV2BGR_YUY2, //
    COLOR_YUV2RGB_YUNV = COLOR_YUV2RGB_YUY2, //
    COLOR_YUV2BGR_YUNV = COLOR_YUV2BGR_YUY2, //
    //
    COLOR_YUV2RGBA_YUY2 = 119,                 //
    COLOR_YUV2BGRA_YUY2 = 120,                 //
    COLOR_YUV2RGBA_YVYU = 121,                 //
    COLOR_YUV2BGRA_YVYU = 122,                 //
    COLOR_YUV2RGBA_YUYV = COLOR_YUV2RGBA_YUY2, //
    COLOR_YUV2BGRA_YUYV = COLOR_YUV2BGRA_YUY2, //
    COLOR_YUV2RGBA_YUNV = COLOR_YUV2RGBA_YUY2, //
    COLOR_YUV2BGRA_YUNV = COLOR_YUV2BGRA_YUY2, //
    //
    COLOR_YUV2GRAY_UYVY = 123, //
    COLOR_YUV2GRAY_YUY2 = 124, //
    // CV_YUV2GRAY_VYUY    = CV_YUV2GRAY_UYVY,
    COLOR_YUV2GRAY_Y422 = COLOR_YUV2GRAY_UYVY, //
    COLOR_YUV2GRAY_UYNV = COLOR_YUV2GRAY_UYVY, //
    COLOR_YUV2GRAY_YVYU = COLOR_YUV2GRAY_YUY2, //
    COLOR_YUV2GRAY_YUYV = COLOR_YUV2GRAY_YUY2, //
    COLOR_YUV2GRAY_YUNV = COLOR_YUV2GRAY_YUY2, //
    //
    // ! alpha premultiplication
    COLOR_RGBA2mRGBA = 125, //
    COLOR_mRGBA2RGBA = 126, //
    //
    // ! RGB to YUV 4:2:0 family
    COLOR_RGB2YUV_I420 = 127,                //
    COLOR_BGR2YUV_I420 = 128,                //
    COLOR_RGB2YUV_IYUV = COLOR_RGB2YUV_I420, //
    COLOR_BGR2YUV_IYUV = COLOR_BGR2YUV_I420, //
    //
    COLOR_RGBA2YUV_I420 = 129,                 //
    COLOR_BGRA2YUV_I420 = 130,                 //
    COLOR_RGBA2YUV_IYUV = COLOR_RGBA2YUV_I420, //
    COLOR_BGRA2YUV_IYUV = COLOR_BGRA2YUV_I420, //
    COLOR_RGB2YUV_YV12 = 131,                  //
    COLOR_BGR2YUV_YV12 = 132,                  //
    COLOR_RGBA2YUV_YV12 = 133,                 //
    COLOR_BGRA2YUV_YV12 = 134,                 //
    //
    // ! Demosaicing
    COLOR_BayerBG2BGR = 46, //
    COLOR_BayerGB2BGR = 47, //
    COLOR_BayerRG2BGR = 48, //
    COLOR_BayerGR2BGR = 49, //
    //
    COLOR_BayerBG2RGB = COLOR_BayerRG2BGR, //
    COLOR_BayerGB2RGB = COLOR_BayerGR2BGR, //
    COLOR_BayerRG2RGB = COLOR_BayerBG2BGR, //
    COLOR_BayerGR2RGB = COLOR_BayerGB2BGR, //
    //
    COLOR_BayerBG2GRAY = 86, //
    COLOR_BayerGB2GRAY = 87, //
    COLOR_BayerRG2GRAY = 88, //
    COLOR_BayerGR2GRAY = 89, //
    //
    // ! Demosaicing using Variable Number of Gradients
    COLOR_BayerBG2BGR_VNG = 62, //
    COLOR_BayerGB2BGR_VNG = 63, //
    COLOR_BayerRG2BGR_VNG = 64, //
    COLOR_BayerGR2BGR_VNG = 65, //
    //
    COLOR_BayerBG2RGB_VNG = COLOR_BayerRG2BGR_VNG, //
    COLOR_BayerGB2RGB_VNG = COLOR_BayerGR2BGR_VNG, //
    COLOR_BayerRG2RGB_VNG = COLOR_BayerBG2BGR_VNG, //
    COLOR_BayerGR2RGB_VNG = COLOR_BayerGB2BGR_VNG, //
    //
    // ! Edge-Aware Demosaicing
    COLOR_BayerBG2BGR_EA = 135, //
    COLOR_BayerGB2BGR_EA = 136, //
    COLOR_BayerRG2BGR_EA = 137, //
    COLOR_BayerGR2BGR_EA = 138, //
    //
    COLOR_BayerBG2RGB_EA = COLOR_BayerRG2BGR_EA, //
    COLOR_BayerGB2RGB_EA = COLOR_BayerGR2BGR_EA, //
    COLOR_BayerRG2RGB_EA = COLOR_BayerBG2BGR_EA, //
    COLOR_BayerGR2RGB_EA = COLOR_BayerGB2BGR_EA, //
    //
    // ! Demosaicing with alpha channel
    COLOR_BayerBG2BGRA = 139, //
    COLOR_BayerGB2BGRA = 140, //
    COLOR_BayerRG2BGRA = 141, //
    COLOR_BayerGR2BGRA = 142, //
    //
    COLOR_BayerBG2RGBA = COLOR_BayerRG2BGRA, //
    COLOR_BayerGB2RGBA = COLOR_BayerGR2BGRA, //
    COLOR_BayerRG2RGBA = COLOR_BayerBG2BGRA, //
    COLOR_BayerGR2RGBA = COLOR_BayerGB2BGRA, //
    //
    COLOR_COLORCVT_MAX = 143 //
    );

  // ! type of the threshold operation
  // ! ![threshold types](pics/threshold.png)
  ThresholdTypes = (       //
    THRESH_BINARY = 0,     // !< \f[\texttt{dst} (x,y) =  \fork{\texttt{maxval}}{if \(\texttt{src}(x,y) > \texttt{thresh}\)}{0}{otherwise}\f]
    THRESH_BINARY_INV = 1, // !< \f[\texttt{dst} (x,y) =  \fork{0}{if \(\texttt{src}(x,y) > \texttt{thresh}\)}{\texttt{maxval}}{otherwise}\f]
    THRESH_TRUNC = 2,      // !< \f[\texttt{dst} (x,y) =  \fork{\texttt{threshold}}{if \(\texttt{src}(x,y) > \texttt{thresh}\)}{\texttt{src}(x,y)}{otherwise}\f]
    THRESH_TOZERO = 3,     // !< \f[\texttt{dst} (x,y) =  \fork{\texttt{src}(x,y)}{if \(\texttt{src}(x,y) > \texttt{thresh}\)}{0}{otherwise}\f]
    THRESH_TOZERO_INV = 4, // !< \f[\texttt{dst} (x,y) =  \fork{0}{if \(\texttt{src}(x,y) > \texttt{thresh}\)}{\texttt{src}(x,y)}{otherwise}\f]
    THRESH_MASK = 7,       //
    THRESH_OTSU = 8,       // !< flag, use Otsu algorithm to choose the optimal threshold value
    THRESH_TRIANGLE = 16   // !< flag, use Triangle algorithm to choose the optimal threshold value
    );

  // ! adaptive threshold algorithm
  // ! @see adaptiveThreshold
  AdaptiveThresholdTypes = ( //
    (* * the threshold value \f$T(x,y)\f$ is a mean of the \f$\texttt{blockSize} \times
      \texttt{blockSize}\f$ neighborhood of \f$(x, y)\f$ minus C *)
    ADAPTIVE_THRESH_MEAN_C = 0,
    (* * the threshold value \f$T(x, y)\f$ is a weighted sum (cross-correlation with a Gaussian
      window) of the \f$\texttt{blockSize} \times \texttt{blockSize}\f$ neighborhood of \f$(x, y)\f$
      minus C . The default sigma (standard deviation) is used for the specified blockSize . See
      #getGaussianKernel *)
    ADAPTIVE_THRESH_GAUSSIAN_C = 1);

  // ! shape of the structuring element
  MorphShapes = (    //
    MORPH_RECT = 0,  // !< a rectangular structuring element:  \f[E_{ij}=1\f]
    MORPH_CROSS = 1, // !< a cross-shaped structuring element:
    // !< \f[E_{ij} = \begin{cases} 1 & \texttt{if } {i=\texttt{anchor.y } {or } {j=\texttt{anchor.x}}} \\0 & \texttt{otherwise} \end{cases}\f]
    MORPH_ELLIPSE = 2 // !< an elliptic structuring element, that is, a filled ellipse inscribed
    // !< into the rectangle Rect(0, 0, esize.width, 0.esize.height)
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
  bottomLeftOrigin: BOOL = false); external opencv_world_dll index 5972{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

procedure putText(img: TInputOutputArray; const text: CppString; org: TPoint; fontFace: HersheyFonts; fontScale: double; color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
  bottomLeftOrigin: BOOL = false); {$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Calculates the width and height of a text string.

  The function cv::getTextSize calculates and returns the size of a box that contains the specified text.
  That is, the following code renders some text, the tight box surrounding it, and the baseline: :
  @code
  String text = "Funny text inside the box";
  int fontFace = FONT_HERSHEY_SCRIPT_SIMPLEX;
  double fontScale = 2;
  int thickness = 3;

  Mat img(600, 800, CV_8UC3, Scalar::all(0));

  int baseline=0;
  Size textSize = getTextSize(text, fontFace,
  fontScale, thickness, &baseline);
  baseline += thickness;

  // center the text
  Point textOrg((img.cols - textSize.width)/2,
  (img.rows + textSize.height)/2);

  // draw the box
  rectangle(img, textOrg + Point(0, baseline),
  textOrg + Point(textSize.width, -textSize.height),
  Scalar(0,0,255));
  // ... and the baseline first
  line(img, textOrg + Point(0, thickness),
  textOrg + Point(textSize.width, thickness),
  Scalar(0, 0, 255));

  // then put the text itself
  putText(img, text, textOrg, fontFace, fontScale,
  Scalar::all(255), thickness, 8);
  @endcode

  @param text Input text string.
  @param fontFace Font to use, see #HersheyFonts.
  @param fontScale Font scale factor that is multiplied by the font-specific base size.
  @param thickness Thickness of lines used to render the text. See #putText for details.
  @param[out] baseLine y-coordinate of the baseline relative to the bottom-most text
  point.
  @return The size of a box that contains the specified text.

  @see putText
*)
// CV_EXPORTS_W Size getTextSize(const String& text, int fontFace,
// double fontScale, int thickness,
// CV_OUT int* baseLine);
// 5135
// ?getTextSize@cv@@YA?AV?$Size_@H@1@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HNHPEAH@Z
// class cv::Size_<int> cv::getTextSize(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,double,int,int *)
// function _getTextSize(const text: CvStdString; fontFace: Int; fontScale: double; thickness: Int; baseLine: pInt = nil): pSize;
// external opencv_world_dll index 5135{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure _getTextSize(const R: pSize; text: CvStdString; fontFace: Int; fontScale: double; thickness: Int; baseLine: pInt = nil);
  external opencv_world_dll index 5135{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function getTextSize(const text: String; fontFace: Int; fontScale: double; thickness: Int; baseLine: pInt = nil): TSize; {$IFDEF USE_INLINE}inline; {$ENDIF}
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
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
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
  ); external opencv_world_dll index 3370 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
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
procedure bilateralFilter(Src: TInputArray; dst: TOutputArray; d: Int; sigmaColor, sigmaSpace: double; borderType: BorderTypes = BORDER_DEFAULT); external opencv_world_dll index 3615
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
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
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * @brief Applies a fixed-level threshold to each array element.

  The function applies fixed-level thresholding to a multiple-channel array. The function is typically
  used to get a bi-level (binary) image out of a grayscale image ( #compare could be also used for
  this purpose) or for removing a noise, that is, filtering out pixels with too small or too large
  values. There are several types of thresholding supported by the function. They are determined by
  type parameter.

  Also, the special values #THRESH_OTSU or #THRESH_TRIANGLE may be combined with one of the
  above values. In these cases, the function determines the optimal threshold value using the Otsu's
  or Triangle algorithm and uses it instead of the specified thresh.

  @note Currently, the Otsu's and Triangle methods are implemented only for 8-bit single-channel images.

  @param src input array (multiple-channel, 8-bit or 32-bit floating point).
  @param dst output array of the same size  and type and the same number of channels as src.
  @param thresh threshold value.
  @param maxval maximum value to use with the #THRESH_BINARY and #THRESH_BINARY_INV thresholding
  types.
  @param type thresholding type (see #ThresholdTypes).
  @return the computed threshold value if Otsu's or Triangle methods used.

  @sa  adaptiveThreshold, findContours, compare, min, max
*)
// CV_EXPORTS_W double threshold( InputArray src, OutputArray dst,
// double thresh, double maxval, int type );
// 6609
// ?threshold@cv@@YANAEBV_InputArray@1@AEBV_OutputArray@1@NNH@Z
// double cv::threshold(class cv::_InputArray const &,class cv::_OutputArray const &,double,double,int)
function threshold(Src: TInputArray; dst: TOutputArray; thresh, maxVal: double; &type: Int): double; external opencv_world_dll index 6609 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * @brief Applies an adaptive threshold to an array.

  The function transforms a grayscale image to a binary image according to the formulae:
  -   **THRESH_BINARY**
  \f[dst(x,y) =  \fork{\texttt{maxValue}}{if \(src(x,y) > T(x,y)\)}{0}{otherwise}\f]
  -   **THRESH_BINARY_INV**
  \f[dst(x,y) =  \fork{0}{if \(src(x,y) > T(x,y)\)}{\texttt{maxValue}}{otherwise}\f]
  where \f$T(x,y)\f$ is a threshold calculated individually for each pixel (see adaptiveMethod parameter).

  The function can process the image in-place.

  @param src Source 8-bit single-channel image.
  @param dst Destination image of the same size and the same type as src.
  @param maxValue Non-zero value assigned to the pixels for which the condition is satisfied
  @param adaptiveMethod Adaptive thresholding algorithm to use, see #AdaptiveThresholdTypes.
  The #BORDER_REPLICATE | #BORDER_ISOLATED is used to process boundaries.
  @param thresholdType Thresholding type that must be either #THRESH_BINARY or #THRESH_BINARY_INV,
  see #ThresholdTypes.
  @param blockSize Size of a pixel neighborhood that is used to calculate a threshold value for the
  pixel: 3, 5, 7, and so on.
  @param C Constant subtracted from the mean or weighted mean (see the details below). Normally, it
  is positive but may be zero or negative as well.

  @sa  threshold, blur, GaussianBlur
*)
// CV_EXPORTS_W void adaptiveThreshold( InputArray src, OutputArray dst,
// double maxValue, int adaptiveMethod,
// int thresholdType, int blockSize, double C );
// 3475
// ?adaptiveThreshold@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@NHHHN@Z
// void cv::adaptiveThreshold(class cv::_InputArray const &,class cv::_OutputArray const &,double,int,int,int,double)
procedure _adaptiveThreshold(Src: TInputArray; dst: TOutputArray; maxValue: double; adaptiveMethod: Int; thresholdType: Int; blockSize: Int; c: double); external opencv_world_dll index 3475
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure adaptiveThreshold(Src: TInputArray; dst: TOutputArray; maxValue: double; adaptiveMethod: AdaptiveThresholdTypes; thresholdType: ThresholdTypes; blockSize: Int; c: double);
{$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Converts an image from one color space to another.

  The function converts an input image from one color space to another. In case of a transformation
  to-from RGB color space, the order of the channels should be specified explicitly (RGB or BGR). Note
  that the default color format in OpenCV is often referred to as RGB but it is actually BGR (the
  bytes are reversed). So the first byte in a standard (24-bit) color image will be an 8-bit Blue
  component, the second byte will be Green, and the third byte will be Red. The fourth, fifth, and
  sixth bytes would then be the second pixel (Blue, then Green, then Red), and so on.

  The conventional ranges for R, G, and B channel values are:
  -   0 to 255 for CV_8U images
  -   0 to 65535 for CV_16U images
  -   0 to 1 for CV_32F images

  In case of linear transformations, the range does not matter. But in case of a non-linear
  transformation, an input RGB image should be normalized to the proper value range to get the correct
  results, for example, for RGB \f$\rightarrow\f$ L\*u\*v\* transformation. For example, if you have a
  32-bit floating-point image directly converted from an 8-bit image without any scaling, then it will
  have the 0..255 value range instead of 0..1 assumed by the function. So, before calling #cvtColor ,
  you need first to scale the image down:
  @code
  img *= 1./255;
  cvtColor(img, img, COLOR_BGR2Luv);
  @endcode
  If you use #cvtColor with 8-bit images, the conversion will have some information lost. For many
  applications, this will not be noticeable but it is recommended to use 32-bit images in applications
  that need the full range of colors or that convert an image before an operation and then convert
  back.

  If conversion adds the alpha channel, its value will set to the maximum of corresponding channel
  range: 255 for CV_8U, 65535 for CV_16U, 1 for CV_32F.

  @param src input image: 8-bit unsigned, 16-bit unsigned ( CV_16UC... ), or single-precision
  floating-point.
  @param dst output image of the same size and depth as src.
  @param code color space conversion code (see #ColorConversionCodes).
  @param dstCn number of channels in the destination image; if the parameter is 0, the number of the
  channels is derived automatically from src and code.

  @see @ref imgproc_color_conversions
*)
// CV_EXPORTS_W void cvtColor( InputArray src, OutputArray dst, int code, int dstCn = 0 );
// 4338
// ?cvtColor@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HH@Z
// void cv::cvtColor(class cv::_InputArray const &,class cv::_OutputArray const &,int,int)
procedure _cvtColor(Src: TInputArray; dst: TOutputArray; code: Int; dstCn: Int = 0); external opencv_world_dll index 4338 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure cvtColor(Src: TInputArray; dst: TOutputArray; code: ColorConversionCodes; dstCn: Int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
//
// ! returns "magic" border value for erosion and dilation. It is automatically transformed to Scalar::all(-DBL_MAX) for dilation.
function morphologyDefaultBorderValue(): TScalar; {$IFDEF USE_INLINE}inline; {$ENDIF} { return Scalar::all(DBL_MAX); }
//
(* * @brief Returns a structuring element of the specified size and shape for morphological operations.

  The function constructs and returns the structuring element that can be further passed to #erode,
  #dilate or #morphologyEx. But you can also construct an arbitrary binary mask yourself and use it as
  the structuring element.

  @param shape Element shape that could be one of #MorphShapes
  @param ksize Size of the structuring element.
  @param anchor Anchor position within the element. The default value \f$(-1, -1)\f$ means that the
  anchor is at the center. Note that only the shape of a cross-shaped element depends on the anchor
  position. In other cases the anchor just regulates how much the result of the morphological
  operation is shifted.
*)
// CV_EXPORTS_W Mat getStructuringElement(int shape, Size ksize, Point anchor = Point(-1,-1));
// 5125
// ?getStructuringElement@cv@@YA?AVMat@1@HV?$Size_@H@1@V?$Point_@H@1@@Z
// class cv::Mat cv::getStructuringElement(int,class cv::Size_<int>,class cv::Point_<int>)
function _getStructuringElement(shape: Int; ksize: UInt64; anchor: UInt64): TMat; external opencv_world_dll index 5125 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function getStructuringElement(shape: MorphShapes; ksize: TSize): TMat; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
function getStructuringElement(shape: MorphShapes; ksize: TSize; anchor: TPoint): TMat; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Erodes an image by using a specific structuring element.

  The function erodes the source image using the specified structuring element that determines the
  shape of a pixel neighborhood over which the minimum is taken:

  \f[\texttt{dst} (x,y) =  \min _{(x',y'):  \, \texttt{element} (x',y') \ne0 } \texttt{src} (x+x',y+y')\f]

  The function supports the in-place mode. Erosion can be applied several ( iterations ) times. In
  case of multi-channel images, each channel is processed independently.

  @param src input image; the number of channels can be arbitrary, but the depth should be one of
  CV_8U, CV_16U, CV_16S, CV_32F or CV_64F.
  @param dst output image of the same size and type as src.
  @param kernel structuring element used for erosion; if `element=Mat()`, a `3 x 3` rectangular
  structuring element is used. Kernel can be created using #getStructuringElement.
  @param anchor position of the anchor within the element; default value (-1, -1) means that the
  anchor is at the element center.
  @param iterations number of times erosion is applied.
  @param borderType pixel extrapolation method, see #BorderTypes. #BORDER_WRAP is not supported.
  @param borderValue border value in case of a constant border
  @sa  dilate, morphologyEx, getStructuringElement
*)
// CV_EXPORTS_W void erode( InputArray src, OutputArray dst, InputArray kernel,
// Point anchor = Point(-1,-1), int iterations = 1,
// int borderType = BORDER_CONSTANT,
// const Scalar& borderValue = morphologyDefaultBorderValue() );
// 4631
// ?erode@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@0V?$Point_@H@1@HHAEBV?$Scalar_@N@1@@Z
// void cv::erode(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::_InputArray const &,class cv::Point_<int>,int,int,class cv::Scalar_<double> const &)
procedure _erode(Src: TInputArray; dst: TOutputArray; kernel: TInputArray; anchor: UInt64 { Point = Point(-1,-1) }; iterations: Int { = 1 }; borderType: Int { = BORDER_CONSTANT };
  const borderValue: TScalar { = morphologyDefaultBorderValue() } ); external opencv_world_dll index 4631 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT }; const borderValue: TScalar { = morphologyDefaultBorderValue() } ); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) } ); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Dilates an image by using a specific structuring element.

  The function dilates the source image using the specified structuring element that determines the
  shape of a pixel neighborhood over which the maximum is taken:
  \f[\texttt{dst} (x,y) =  \max _{(x',y'):  \, \texttt{element} (x',y') \ne0 } \texttt{src} (x+x',y+y')\f]

  The function supports the in-place mode. Dilation can be applied several ( iterations ) times. In
  case of multi-channel images, each channel is processed independently.

  @param src input image; the number of channels can be arbitrary, but the depth should be one of
  CV_8U, CV_16U, CV_16S, CV_32F or CV_64F.
  @param dst output image of the same size and type as src.
  @param kernel structuring element used for dilation; if elemenat=Mat(), a 3 x 3 rectangular
  structuring element is used. Kernel can be created using #getStructuringElement
  @param anchor position of the anchor within the element; default value (-1, -1) means that the
  anchor is at the element center.
  @param iterations number of times dilation is applied.
  @param borderType pixel extrapolation method, see #BorderTypes. #BORDER_WRAP is not suported.
  @param borderValue border value in case of a constant border
  @sa  erode, morphologyEx, getStructuringElement
*)
// CV_EXPORTS_W void dilate( InputArray src, OutputArray dst, InputArray kernel,
// Point anchor = Point(-1,-1), int iterations = 1,
// int borderType = BORDER_CONSTANT,
// const Scalar& borderValue = morphologyDefaultBorderValue());
// 4492
// ?dilate@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@0V?$Point_@H@1@HHAEBV?$Scalar_@N@1@@Z
// void cv::dilate(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::_InputArray const &,class cv::Point_<int>,int,int,class cv::Scalar_<double> const &)
procedure _dilate(Src: TInputArray; dst: TOutputArray; kernel: TInputArray; anchor: UInt64 { Point= Point(-1,-1) }; iterations: Int { = 1 }; borderType: Int { = BORDER_CONSTANT };
  const borderValue: TScalar { = morphologyDefaultBorderValue() } ); external opencv_world_dll index 4492 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT }; const borderValue: TScalar { = morphologyDefaultBorderValue() } ); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) } ); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Equalizes the histogram of a grayscale image.

  The function equalizes the histogram of the input image using the following algorithm:

  - Calculate the histogram \f$H\f$ for src .
  - Normalize the histogram so that the sum of histogram bins is 255.
  - Compute the integral of the histogram:
  \f[H'_i =  \sum _{0  \le j < i} H(j)\f]
  - Transform the image using \f$H'\f$ as a look-up table: \f$\texttt{dst}(x,y) = H'(\texttt{src}(x,y))\f$

  The algorithm normalizes the brightness and increases the contrast of the image.

  @param src Source 8-bit single channel image.
  @param dst Destination image of the same size and type as src .
*)
// CV_EXPORTS_W void equalizeHist( InputArray src, OutputArray dst );
// 4624
// ?equalizeHist@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@@Z
// void cv::equalizeHist(class cv::_InputArray const &,class cv::_OutputArray const &)
procedure equalizeHist(Src: TInputArray; dst: TOutputArray); external opencv_world_dll index 4624 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Draws a simple or thick elliptic arc or fills an ellipse sector.

  The function cv::ellipse with more parameters draws an ellipse outline, a filled ellipse, an elliptic
  arc, or a filled ellipse sector. The drawing code uses general parametric form.
  A piecewise-linear curve is used to approximate the elliptic arc
  boundary. If you need more control of the ellipse rendering, you can retrieve the curve using
  #ellipse2Poly and then render it with #polylines or fill it with #fillPoly. If you use the first
  variant of the function and want to draw the whole ellipse, not an arc, pass `startAngle=0` and
  `endAngle=360`. If `startAngle` is greater than `endAngle`, they are swapped. The figure below explains
  the meaning of the parameters to draw the blue arc.

  ![Parameters of Elliptic Arc](pics/ellipse.svg)

  @param img Image.
  @param center Center of the ellipse.
  @param axes Half of the size of the ellipse main axes.
  @param angle Ellipse rotation angle in degrees.
  @param startAngle Starting angle of the elliptic arc in degrees.
  @param endAngle Ending angle of the elliptic arc in degrees.
  @param color Ellipse color.
  @param thickness Thickness of the ellipse arc outline, if positive. Otherwise, this indicates that
  a filled ellipse sector is to be drawn.
  @param lineType Type of the ellipse boundary. See #LineTypes
  @param shift Number of fractional bits in the coordinates of the center and values of axes.
*)
// CV_EXPORTS_W void ellipse(InputOutputArray img, Point center, Size axes,
// double angle, double startAngle, double endAngle,
// const Scalar& color, int thickness = 1,
// int lineType = LINE_8, int shift = 0);
// 4580
// ?ellipse@cv@@YAXAEBV_InputOutputArray@1@V?$Point_@H@1@V?$Size_@H@1@NNNAEBV?$Scalar_@N@1@HHH@Z
// void cv::ellipse(class cv::_InputOutputArray const &,class cv::Point_<int>,class cv::Size_<int>,double,double,double,class cv::Scalar_<double> const &,int,int,int)
procedure _ellipse(img: TInputOutputArray; center: UInt64 { TPoint }; axes: UInt64 { TSize }; angle, startAngle, endAngle: double; const color: TScalar; thickness: Int = 1;
  lineType: Int = Int(LINE_8); shift: Int = 0); external opencv_world_dll index 4580 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure ellipse(const img: TInputOutputArray; const center: TPoint; const axes: TSize; angle, startAngle, endAngle: double; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
  shift: Int = 0); {$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Draws a marker on a predefined position in an image.

  The function cv::drawMarker draws a marker on a given position in the image. For the moment several
  marker types are supported, see #MarkerTypes for more information.

  @param img Image.
  @param position The point where the crosshair is positioned.
  @param color Line color.
  @param markerType The specific type of marker you want to use, see #MarkerTypes
  @param thickness Line thickness.
  @param line_type Type of the line, See #LineTypes
  @param markerSize The length of the marker axis [default = 20 pixels]
*)
// CV_EXPORTS_W void drawMarker(InputOutputArray img, Point position, const Scalar& color,
// int markerType = MARKER_CROSS, int markerSize=20, int thickness=1,
// int line_type=8);
// 4534
// ?drawMarker@cv@@YAXAEBV_InputOutputArray@1@V?$Point_@H@1@AEBV?$Scalar_@N@1@HHHH@Z
// void cv::drawMarker(class cv::_InputOutputArray const &,class cv::Point_<int>,class cv::Scalar_<double> const &,int,int,int,int)
procedure _drawMarker(img: TInputOutputArray; position: UInt64 { TPoint }; const color: TScalar; markerType: Int = Int(MARKER_CROSS); markerSize: Int = 20; thickness: Int = 1; line_type: Int = 8);
  external opencv_world_dll index 4534 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure drawMarker(const img: TInputOutputArray; const position: TPoint; const color: TScalar; const markerType: MarkerTypes = MARKER_CROSS; const markerSize: Int = 20; const thickness: Int = 1;
  const line_type: LineTypes = LineTypes(8)); {$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Draws a circle.

  The function cv::circle draws a simple or filled circle with a given center and radius.
  @param img Image where the circle is drawn.
  @param center Center of the circle.
  @param radius Radius of the circle.
  @param color Circle color.
  @param thickness Thickness of the circle outline, if positive. Negative values, like #FILLED,
  mean that a filled circle is to be drawn.
  @param lineType Type of the circle boundary. See #LineTypes
  @param shift Number of fractional bits in the coordinates of the center and in the radius value.
*)
// CV_EXPORTS_W void circle(InputOutputArray img, Point center, int radius,
// const Scalar& color, int thickness = 1,
// int lineType = LINE_8, int shift = 0);
// 3795
// ?circle@cv@@YAXAEBV_InputOutputArray@1@V?$Point_@H@1@HAEBV?$Scalar_@N@1@HHH@Z
// void cv::circle(class cv::_InputOutputArray const &,class cv::Point_<int>,int,class cv::Scalar_<double> const &,int,int,int)
procedure _circle(img: TInputOutputArray; center: UInt64 { TPoint }; radius: Int; const color: TScalar; thickness: Int = 1; lineType: Int = Int(LINE_8); shift: Int = 0);
  external opencv_world_dll index 3795 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure circle(img: TInputOutputArray; center: TPoint; radius: Int; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8; shift: Int = 0); {$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Calculates a histogram of a set of arrays.

  The function cv::calcHist calculates the histogram of one or more arrays. The elements of a tuple used
  to increment a histogram bin are taken from the corresponding input arrays at the same location. The
  sample below shows how to compute a 2D Hue-Saturation histogram for a color image. :
  @include snippets/imgproc_calcHist.cpp

  @param images Source arrays. They all should have the same depth, CV_8U, CV_16U or CV_32F , and the same
  size. Each of them can have an arbitrary number of channels.
  @param nimages Number of source images.
  @param channels List of the dims channels used to compute the histogram. The first array channels
  are numerated from 0 to images[0].channels()-1 , the second array channels are counted from
  images[0].channels() to images[0].channels() + images[1].channels()-1, and so on.
  @param mask Optional mask. If the matrix is not empty, it must be an 8-bit array of the same size
  as images[i] . The non-zero mask elements mark the array elements counted in the histogram.
  @param hist Output histogram, which is a dense or sparse dims -dimensional array.
  @param dims Histogram dimensionality that must be positive and not greater than CV_MAX_DIMS
  (equal to 32 in the current OpenCV version).
  @param histSize Array of histogram sizes in each dimension.
  @param ranges Array of the dims arrays of the histogram bin boundaries in each dimension. When the
  histogram is uniform ( uniform =true), then for each dimension i it is enough to specify the lower
  (inclusive) boundary \f$L_0\f$ of the 0-th histogram bin and the upper (exclusive) boundary
  \f$U_{\texttt{histSize}[i]-1}\f$ for the last histogram bin histSize[i]-1 . That is, in case of a
  uniform histogram each of ranges[i] is an array of 2 elements. When the histogram is not uniform (
  uniform=false ), then each of ranges[i] contains histSize[i]+1 elements:
  \f$L_0, U_0=L_1, U_1=L_2, ..., U_{\texttt{histSize[i]}-2}=L_{\texttt{histSize[i]}-1}, U_{\texttt{histSize[i]}-1}\f$
  . The array elements, that are not between \f$L_0\f$ and \f$U_{\texttt{histSize[i]}-1}\f$ , are not
  counted in the histogram.
  @param uniform Flag indicating whether the histogram is uniform or not (see above).
  @param accumulate Accumulation flag. If it is set, the histogram is not cleared in the beginning
  when it is allocated. This feature enables you to compute a single histogram from several sets of
  arrays, or to update the histogram in time.
*)
// CV_EXPORTS void calcHist( const Mat* images, int nimages,
// const int* channels, InputArray mask,
// OutputArray hist, int dims, const int* histSize,
// const float** ranges, bool uniform = true, bool accumulate = false );
// 3717
// ?calcHist@cv@@YAXPEBVMat@1@HPEBHAEBV_InputArray@1@AEBV_OutputArray@1@H1PEAPEBM_N5@Z
// void cv::calcHist(
// class cv::Mat const *,
// int,
// int const *,
// class cv::_InputArray const &,
// class cv::_OutputArray const &,
// int,
// int const *,
// float const * *,
// bool,
// bool)
procedure calcHist(const images: pMat; nimages: Int; channels: pInt; mask: TInputArray; hist: TOutputArray; dims: Int; const histSize: pInt; const ranges: pFloat; UNIFORM: BOOL = true;
  accumulate: BOOL = false); external opencv_world_dll index 3717 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Draws a line segment connecting two points.

  The function line draws the line segment between pt1 and pt2 points in the image. The line is
  clipped by the image boundaries. For non-antialiased lines with integer coordinates, the 8-connected
  or 4-connected Bresenham algorithm is used. Thick lines are drawn with rounding endings. Antialiased
  lines are drawn using Gaussian filtering.

  @param img Image.
  @param pt1 First point of the line segment.
  @param pt2 Second point of the line segment.
  @param color Line color.
  @param thickness Line thickness.
  @param lineType Type of the line. See #LineTypes.
  @param shift Number of fractional bits in the point coordinates.
*)
// CV_EXPORTS_W void line(InputOutputArray img, Point pt1, Point pt2, const Scalar& color,
// int thickness = 1, int lineType = LINE_8, int shift = 0);
// 5464
// ?line@cv@@YAXAEBV_InputOutputArray@1@V?$Point_@H@1@1AEBV?$Scalar_@N@1@HHH@Z
// void cv::line(class cv::_InputOutputArray const &,class cv::Point_<int>,class cv::Point_<int>,class cv::Scalar_<double> const &,int,int,int)
procedure _line(img: TInputOutputArray; pt1: UInt64 { TPoint }; pt2: UInt64 { TPoint }; const color: TScalar; thickness: Int = 1; lineType: Int = Int(LINE_8); shift: Int = 0);
  external opencv_world_dll index 5464{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure line(img: TInputOutputArray; pt1: TPoint; pt2: TPoint; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8; shift: Int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Draws an arrow segment pointing from the first point to the second one.

  The function cv::arrowedLine draws an arrow between pt1 and pt2 points in the image. See also #line.

  @param img Image.
  @param pt1 The point the arrow starts from.
  @param pt2 The point the arrow points to.
  @param color Line color.
  @param thickness Line thickness.
  @param line_type Type of the line. See #LineTypes
  @param shift Number of fractional bits in the point coordinates.
  @param tipLength The length of the arrow tip in relation to the arrow length
*)
// CV_EXPORTS_W void arrowedLine(InputOutputArray img, Point pt1, Point pt2, const Scalar& color,
// int thickness=1, int line_type=8, int shift=0, double tipLength=0.1);
// 3560
// ?arrowedLine@cv@@YAXAEBV_InputOutputArray@1@V?$Point_@H@1@1AEBV?$Scalar_@N@1@HHHN@Z
// void cv::arrowedLine(class cv::_InputOutputArray const &,class cv::Point_<int>,class cv::Point_<int>,class cv::Scalar_<double> const &,int,int,int,double)
procedure _arrowedLine(img: TInputOutputArray; pt1: UInt64 { TPoint }; pt2: UInt64 { TPoint }; const color: TScalar; thickness: Int = 1; line_type: Int = 8; shift: Int = 0; tipLength: double = 0.1);
  external opencv_world_dll index 3560{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure arrowedLine(const img: TInputOutputArray; const pt1: TPoint; const pt2: TPoint; const color: TScalar; const thickness: Int = 1; const line_type: LineTypes = LineTypes(8);
  const shift: Int = 0; const tipLength: double = 0.1); {$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Draws a simple, thick, or filled up-right rectangle.

  The function cv::rectangle draws a rectangle outline or a filled rectangle whose two opposite corners
  are pt1 and pt2.

  @param img Image.
  @param pt1 Vertex of the rectangle.
  @param pt2 Vertex of the rectangle opposite to pt1 .
  @param color Rectangle color or brightness (grayscale image).
  @param thickness Thickness of lines that make up the rectangle. Negative values, like #FILLED,
  mean that the function has to draw a filled rectangle.
  @param lineType Type of the line. See #LineTypes
  @param shift Number of fractional bits in the point coordinates.
*)
// CV_EXPORTS_W void rectangle(InputOutputArray img, Point pt1, Point pt2,
// const Scalar& color, int thickness = 1,
// int lineType = LINE_8, int shift = 0);
// 6066
// ?rectangle@cv@@YAXAEBV_InputOutputArray@1@V?$Point_@H@1@1AEBV?$Scalar_@N@1@HHH@Z
// void cv::rectangle(class cv::_InputOutputArray const &,class cv::Point_<int>,class cv::Point_<int>,class cv::Scalar_<double> const &,int,int,int)
procedure _rectangle(img: TInputOutputArray; pt1, pt2: UInt64 { TPoint }; const color: TScalar; thickness: Int = 1; lineType: Int = Int(LINE_8); shift: Int = 0);
  external opencv_world_dll index 6066{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure rectangle(const img: TInputOutputArray; const pt1, pt2: TPoint; const color: TScalar; const thickness: Int = 1; const lineType: LineTypes = LINE_8; const shift: Int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Fills the area bounded by one or more polygons.

  The function cv::fillPoly fills an area bounded by several polygonal contours. The function can fill
  complex areas, for example, areas with holes, contours with self-intersections (some of their
  parts), and so forth.

  @param img Image.
  @param pts Array of polygons where each polygon is represented as an array of points.
  @param color Polygon color.
  @param lineType Type of the polygon boundaries. See #LineTypes
  @param shift Number of fractional bits in the vertex coordinates.
  @param offset Optional offset of all points of the contours.
*)
// CV_EXPORTS_W void fillPoly(InputOutputArray img, InputArrayOfArrays pts,
// const Scalar& color, int lineType = LINE_8, int shift = 0,
// Point offset = Point() );
// 4711
// ?fillPoly@cv@@YAXAEBV_InputOutputArray@1@AEBV_InputArray@1@AEBV?$Scalar_@N@1@HHV?$Point_@H@1@@Z
// void cv::fillPoly(class cv::_InputOutputArray const &,class cv::_InputArray const &,class cv::Scalar_<double> const &,int,int,class cv::Point_<int>)

(* * @overload *)
// CV_EXPORTS void fillPoly(InputOutputArray img, const Point** pts,
// const int* npts, int ncontours,
// const Scalar& color, int lineType = LINE_8, int shift = 0,
// Point offset = Point() );
// 4712
// ?fillPoly@cv@@YAXAEBV_InputOutputArray@1@PEAPEBV?$Point_@H@1@PEBHHAEBV?$Scalar_@N@1@HHV31@@Z
// void cv::fillPoly(class cv::_InputOutputArray const &,class cv::Point_<int> const **,int const *,int,class cv::Scalar_<double> const &,int,int,class cv::Point_<int>)
procedure _fillPoly(img: TInputOutputArray; const pts: pPoint; const npts: pInt; ncontours: Int; const color: TScalar; lineType: Int = Int(LINE_8); shift: Int = 0;
  offset: UInt64 { TPoint } { = Point() } = 0); external opencv_world_dll index 4712{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const color: TScalar; const lineType: LineTypes { = LINE_8 }; const shift: Int { = 0 };
  const offset: TPoint); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const color: TScalar; const lineType: LineTypes = LINE_8; const shift: Int = 0); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Draws several polygonal curves.

  @param img Image.
  @param pts Array of polygonal curves.
  @param isClosed Flag indicating whether the drawn polylines are closed or not. If they are closed,
  the function draws a line from the last vertex of each curve to its first vertex.
  @param color Polyline color.
  @param thickness Thickness of the polyline edges.
  @param lineType Type of the line segments. See #LineTypes
  @param shift Number of fractional bits in the vertex coordinates.

  The function cv::polylines draws one or more polygonal curves.
*)
// CV_EXPORTS_W void polylines(InputOutputArray img, InputArrayOfArrays pts,
// bool isClosed, const Scalar& color,
// int thickness = 1, int lineType = LINE_8, int shift = 0 );
// 5854
// ?polylines@cv@@YAXAEBV_InputOutputArray@1@AEBV_InputArray@1@_NAEBV?$Scalar_@N@1@HHH@Z	void cv::polylines(class cv::_InputOutputArray const &,class cv::_InputArray const &,bool,class cv::Scalar_<double> const &,int,int,int)

(* * @overload *)
// CV_EXPORTS void polylines(InputOutputArray img, const Point* const* pts, const int* npts,
// int ncontours, bool isClosed, const Scalar& color,
// int thickness = 1, int lineType = LINE_8, int shift = 0 );
// 5855
// ?polylines@cv@@YAXAEBV_InputOutputArray@1@PEBQEBV?$Point_@H@1@PEBHH_NAEBV?$Scalar_@N@1@HHH@Z
// void cv::polylines(class cv::_InputOutputArray const &,class cv::Point_<int> const * const *,int const *,int,bool,class cv::Scalar_<double> const &,int,int,int)
procedure _polylines(img: TInputOutputArray; const pts: pPoint; const npts: pInt; ncontours: Int; isClosed: BOOL; const color: TScalar; thickness: Int = 1; lineType: Int = Int(LINE_8);
  shift: Int = 0); external opencv_world_dll index 5855{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure polylines(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const isClosed: BOOL; const color: TScalar; const thickness: Int = 1;
  const lineType: LineTypes = LINE_8; const shift: Int = 0); {$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @example samples/cpp/edge.cpp This program demonstrates usage of the Canny edge detector
  Check@ref tutorial_canny_detector " the corresponding tutorial "
  for more details *)

(* * @brief Finds edges in an image using the Canny algorithm @cite Canny86 .

  The function finds edges in the input image and marks them in the output map edges using the
  Canny algorithm. The smallest value between threshold1 and threshold2 is used for edge linking. The
  largest value is used to find initial segments of strong edges. See
  <http://en.wikipedia.org/wiki/Canny_edge_detector>

  @param image 8-bit input image.
  @param edges output edge map; single channels 8-bit image, which has the same size as image .
  @param threshold1 first threshold for the hysteresis procedure.
  @param threshold2 second threshold for the hysteresis procedure.
  @param apertureSize aperture size for the Sobel operator.
  @param L2gradient a flag, indicating whether a more accurate \f$L_2\f$ norm
  \f$=\sqrt{(dI/dx)^2 + (dI/dy)^2}\f$ should be used to calculate the image gradient magnitude (
  L2gradient=true ), or whether the default \f$L_1\f$ norm \f$=|dI/dx|+|dI/dy|\f$ is enough (
  L2gradient=false ).
*)
// CV_EXPORTS_W void Canny( InputArray image, OutputArray edges,
// double threshold1, double threshold2,
// int apertureSize = 3, bool L2gradient = false );
// 3355
// ?Canny@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@NNH_N@Z
// void cv::Canny(class cv::_InputArray const &,class cv::_OutputArray const &,double,double,int,bool)
procedure Canny(image: TInputArray; edges: TOutputArray; threshold1, threshold2: double; apertureSize: Int = 3; L2gradient: BOOL = false); overload;
  external opencv_world_dll index 3355{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * \overload

  Finds edges in an image using the Canny algorithm with custom image gradient.

  @param dx 16-bit x derivative of input image (CV_16SC1 or CV_16SC3).
  @param dy 16-bit y derivative of input image (same type as dx).
  @param edges output edge map; single channels 8-bit image, which has the same size as image .
  @param threshold1 first threshold for the hysteresis procedure.
  @param threshold2 second threshold for the hysteresis procedure.
  @param L2gradient a flag, indicating whether a more accurate \f$L_2\f$ norm
  \f$=\sqrt{(dI/dx)^2 + (dI/dy)^2}\f$ should be used to calculate the image gradient magnitude (
  L2gradient=true ), or whether the default \f$L_1\f$ norm \f$=|dI/dx|+|dI/dy|\f$ is enough (
  L2gradient=false ).
*)
// CV_EXPORTS_W void Canny( InputArray dx, InputArray dy,
// OutputArray edges,
// double threshold1, double threshold2,
// bool L2gradient = false );
// 3354
// ?Canny@cv@@YAXAEBV_InputArray@1@0AEBV_OutputArray@1@NN_N@Z
// void cv::Canny(class cv::_InputArray const &,class cv::_InputArray const &,class cv::_OutputArray const &,double,double,bool)
procedure Canny(dx: TInputArray; dy: TInputArray; edges: TOutputArray; threshold1, threshold2: double; L2gradient: BOOL = false); overload;
  external opencv_world_dll index 3354{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * @brief Calculates the minimal eigenvalue of gradient matrices for corner detection.

  The function is similar to cornerEigenValsAndVecs but it calculates and stores only the minimal
  eigenvalue of the covariance matrix of derivatives, that is, \f$\min(\lambda_1, \lambda_2)\f$ in terms
  of the formulae in the cornerEigenValsAndVecs description.

  @param src Input single-channel 8-bit or floating-point image.
  @param dst Image to store the minimal eigenvalues. It has the type CV_32FC1 and the same size as
  src .
  @param blockSize Neighborhood size (see the details on #cornerEigenValsAndVecs ).
  @param ksize Aperture parameter for the Sobel operator.
  @param borderType Pixel extrapolation method. See #BorderTypes. #BORDER_WRAP is not supported.
*)
// CV_EXPORTS_W void cornerMinEigenVal( InputArray src, OutputArray dst,
// int blockSize, int ksize = 3,
// int borderType = BORDER_DEFAULT );
// 4090
// ?cornerMinEigenVal@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHH@Z
// void cv::cornerMinEigenVal(class cv::_InputArray const &,class cv::_OutputArray const &,int,int,int)
procedure cornerMinEigenVal(Src: TInputArray; dst: TOutputArray; blockSize: Int; ksize: Int = 3; borderType: BorderTypes = BORDER_DEFAULT); external opencv_world_dll
// name '?cornerMinEigenVal@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHH@Z'
  index 4090
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//
(* * @brief Calculates the first x- or y- image derivative using Scharr operator.

  The function computes the first x- or y- spatial image derivative using the Scharr operator. The
  call

  \f[\texttt{Scharr(src, dst, ddepth, dx, dy, scale, delta, borderType)}\f]

  is equivalent to

  \f[\texttt{Sobel(src, dst, ddepth, dx, dy, FILTER_SCHARR, scale, delta, borderType)} .\f]

  @param src input image.
  @param dst output image of the same size and the same number of channels as src.
  @param ddepth output image depth, see @ref filter_depths "combinations"
  @param dx order of the derivative x.
  @param dy order of the derivative y.
  @param scale optional scale factor for the computed derivative values; by default, no scaling is
  applied (see #getDerivKernels for details).
  @param delta optional delta value that is added to the results prior to storing them in dst.
  @param borderType pixel extrapolation method, see #BorderTypes. #BORDER_WRAP is not supported.
  @sa  cartToPolar
*)
// CV_EXPORTS_W void Scharr( InputArray src, OutputArray dst, int ddepth,
// int dx, int dy, double scale = 1, double delta = 0,
// int borderType = BORDER_DEFAULT );
// 3441
// ?Scharr@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHHNNH@Z
// void cv::Scharr(class cv::_InputArray const &,class cv::_OutputArray const &,int,int,int,double,double,int)
procedure _Scharr(Src: TInputArray; dst: TOutputArray; depth: Int; dx, dy: Int; scale: double = 1; delta: double = 0; borderType: Int = Int(BORDER_DEFAULT));
  external opencv_world_dll index 3441{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure Scharr(const Src: TInputArray; const dst: TOutputArray; const depth: Int; const dx, dy: Int; const scale: double = 1; const delta: double = 0;
  const borderType: BorderTypes = BORDER_DEFAULT);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Draws contours outlines or filled contours.

  The function draws contour outlines in the image if \f$\texttt{thickness} \ge 0\f$ or fills the area
  bounded by the contours if \f$\texttt{thickness}<0\f$ . The example below shows how to retrieve
  connected components from the binary image and label them: :
  @include snippets/imgproc_drawContours.cpp

  @param image Destination image.
  @param contours All the input contours. Each contour is stored as a point vector.
  @param contourIdx Parameter indicating a contour to draw. If it is negative, all the contours are drawn.
  @param color Color of the contours.
  @param thickness Thickness of lines the contours are drawn with. If it is negative (for example,
  thickness=#FILLED ), the contour interiors are drawn.
  @param lineType Line connectivity. See #LineTypes
  @param hierarchy Optional information about hierarchy. It is only needed if you want to draw only
  some of the contours (see maxLevel ).
  @param maxLevel Maximal level for drawn contours. If it is 0, only the specified contour is drawn.
  If it is 1, the function draws the contour(s) and all the nested contours. If it is 2, the function
  draws the contours, all the nested contours, all the nested-to-nested contours, and so on. This
  parameter is only taken into account when there is hierarchy available.
  @param offset Optional contour shift parameter. Shift all the drawn contours by the specified
  \f$\texttt{offset}=(dx,dy)\f$ .
  @note When thickness=#FILLED, the function is designed to handle connected components with holes correctly
  even when no hierarchy data is provided. This is done by analyzing all the outlines together
  using even-odd rule. This may give incorrect results if you have a joint collection of separately retrieved
  contours. In order to solve this problem, you need to call #drawContours separately for each sub-group
  of contours, or iterate over the collection using contourIdx parameter.
*)
// CV_EXPORTS_W void drawContours( InputOutputArray image, InputArrayOfArrays contours,
// int contourIdx, const Scalar& color,
// int thickness = 1, int lineType = LINE_8,
// InputArray hierarchy = noArray(),
// int maxLevel = INT_MAX, Point offset = Point() );
// 4531
// ?drawContours@cv@@YAXAEBV_InputOutputArray@1@AEBV_InputArray@1@HAEBV?$Scalar_@N@1@HH1HV?$Point_@H@1@@Z
// void cv::drawContours(class cv::_InputOutputArray const &,class cv::_InputArray const &,int,class cv::Scalar_<double> const &,int,int,class cv::_InputArray const &,int,class cv::Point_<int>)
procedure _drawContours(image: TInputOutputArray; contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int { = 1 }; lineType: LineTypes { = LINE_8 };
  hierarchy: TInputArray { = noArray() }; maxLevel: Int { = INT_MAX }; offset: UInt64 { TPoint  = Point() } ); external opencv_world_dll
// name '?drawContours@cv@@YAXAEBV_InputOutputArray@1@AEBV_InputArray@1@HAEBV?$Scalar_@N@1@HH1HV?$Point_@H@1@@Z'
  index 4531
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int = 1; const lineType: LineTypes = LINE_8);
  overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int; const lineType: LineTypes;
  const hierarchy: TInputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int; const lineType: LineTypes;
  const hierarchy: TInputArray; const maxLevel: Int); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int; const lineType: LineTypes;
  const hierarchy: TInputArray; const maxLevel: Int; const offset: TPoint); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
(* * @brief Saves an image to a specified file.

  The function imwrite saves the image to the specified file. The image format is chosen based on the
  filename extension (see cv::imread for the list of extensions). In general, only 8-bit
  single-channel or 3-channel (with 'BGR' channel order) images
  can be saved using this function, with these exceptions:

  - 16-bit unsigned (CV_16U) images can be saved in the case of PNG, JPEG 2000, and TIFF formats
  - 32-bit float (CV_32F) images can be saved in PFM, TIFF, OpenEXR, and Radiance HDR formats;
  3-channel (CV_32FC3) TIFF images will be saved using the LogLuv high dynamic range encoding
  (4 bytes per pixel)
  - PNG images with an alpha channel can be saved using this function. To do this, create
  8-bit (or 16-bit) 4-channel image BGRA, where the alpha channel goes last. Fully transparent pixels
  should have alpha set to 0, fully opaque pixels should have alpha set to 255/65535 (see the code sample below).
  - Multiple images (vector of Mat) can be saved in TIFF format (see the code sample below).

  If the image format is not supported, the image will be converted to 8-bit unsigned (CV_8U) and saved that way.

  If the format, depth or channel order is different, use
  Mat::convertTo and cv::cvtColor to convert it before saving. Or, use the universal FileStorage I/O
  functions to save the image to XML or YAML format.

  The sample below shows how to create a BGRA image, how to set custom compression parameters and save it to a PNG file.
  It also demonstrates how to save multiple images in a TIFF file:
  @include snippets/imgcodecs_imwrite.cpp
  @param filename Name of the file.
  @param img (Mat or vector of Mat) Image or Images to be saved.
  @param params Format-specific parameters encoded as pairs (paramId_1, paramValue_1, paramId_2, paramValue_2, ... .) see cv::ImwriteFlags
*)
// CV_EXPORTS_W bool imwrite( const String& filename, InputArray img,
// const std::vector<int>& params = std::vector<int>());
// 5269
// ?imwrite@cv@@YA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@1@AEBV?$vector@HV?$allocator@H@std@@@3@@Z
// bool cv::imwrite(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,class cv::_InputArray const &,class std::vector<int,class std::allocator<int> > const &)
function imwrite(const filename: CppString; img: TInputArray; const params: Vector<Int> { = std::vector<int>() }
  ): BOOL; overload; external opencv_world_dll
// name '?imwrite@cv@@YA_NAEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AEBV_InputArray@1@AEBV?$vector@HV?$allocator@H@std@@@3@@Z'
  index 5269
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function imwrite(const filename: CppString; const img: TInputArray): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
//
(* * @brief Calculates eigenvalues and eigenvectors of image blocks for corner detection.

  For every pixel \f$p\f$ , the function cornerEigenValsAndVecs considers a blockSize \f$\times\f$ blockSize
  neighborhood \f$S(p)\f$ . It calculates the covariation matrix of derivatives over the neighborhood as:

  \f[M =  \begin{bmatrix} \sum _{S(p)}(dI/dx)^2 &  \sum _{S(p)}dI/dx dI/dy  \\ \sum _{S(p)}dI/dx dI/dy &  \sum _{S(p)}(dI/dy)^2 \end{bmatrix}\f]

  where the derivatives are computed using the Sobel operator.

  After that, it finds eigenvectors and eigenvalues of \f$M\f$ and stores them in the destination image as
  \f$(\lambda_1, \lambda_2, x_1, y_1, x_2, y_2)\f$ where

  -   \f$\lambda_1, \lambda_2\f$ are the non-sorted eigenvalues of \f$M\f$
  -   \f$x_1, y_1\f$ are the eigenvectors corresponding to \f$\lambda_1\f$
  -   \f$x_2, y_2\f$ are the eigenvectors corresponding to \f$\lambda_2\f$

  The output of the function can be used for robust edge or corner detection.

  @param src Input single-channel 8-bit or floating-point image.
  @param dst Image to store the results. It has the same size as src and the type CV_32FC(6) .
  @param blockSize Neighborhood size (see details below).
  @param ksize Aperture parameter for the Sobel operator.
  @param borderType Pixel extrapolation method. See #BorderTypes. #BORDER_WRAP is not supported.

  @sa  cornerMinEigenVal, cornerHarris, preCornerDetect
*)
// CV_EXPORTS_W void cornerEigenValsAndVecs( InputArray src, OutputArray dst,
// int blockSize, int ksize,
// int borderType = BORDER_DEFAULT );
// 4088
// ?cornerEigenValsAndVecs@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHH@Z
// void cv::cornerEigenValsAndVecs(class cv::_InputArray const &,class cv::_OutputArray const &,int,int,int)
procedure cornerEigenValsAndVecs(Src: TInputArray; dst: TOutputArray; blockSize, ksize: Int; borderType: BorderTypes = BORDER_DEFAULT); external opencv_world_dll
// name '?cornerEigenValsAndVecs@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HHH@Z'
  index 4088 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * @brief Determines strong corners on an image.

  The function finds the most prominent corners in the image or in the specified image region, as
  described in @cite Shi94

  -   Function calculates the corner quality measure at every source image pixel using the
  #cornerMinEigenVal or #cornerHarris .
  -   Function performs a non-maximum suppression (the local maximums in *3 x 3* neighborhood are
  retained).
  -   The corners with the minimal eigenvalue less than
  \f$\texttt{qualityLevel} \cdot \max_{x,y} qualityMeasureMap(x,y)\f$ are rejected.
  -   The remaining corners are sorted by the quality measure in the descending order.
  -   Function throws away each corner for which there is a stronger corner at a distance less than
  maxDistance.

  The function can be used to initialize a point-based tracker of an object.

  @note If the function is called with different values A and B of the parameter qualityLevel , and
  A \> B, the vector of returned corners with qualityLevel=A will be the prefix of the output vector
  with qualityLevel=B .

  @param image Input 8-bit or floating-point 32-bit, single-channel image.
  @param corners Output vector of detected corners.
  @param maxCorners Maximum number of corners to return. If there are more corners than are found,
  the strongest of them is returned. `maxCorners <= 0` implies that no limit on the maximum is set
  and all detected corners are returned.
  @param qualityLevel Parameter characterizing the minimal accepted quality of image corners. The
  parameter value is multiplied by the best corner quality measure, which is the minimal eigenvalue
  (see #cornerMinEigenVal ) or the Harris function response (see #cornerHarris ). The corners with the
  quality measure less than the product are rejected. For example, if the best corner has the
  quality measure = 1500, and the qualityLevel=0.01 , then all the corners with the quality measure
  less than 15 are rejected.
  @param minDistance Minimum possible Euclidean distance between the returned corners.
  @param mask Optional region of interest. If the image is not empty (it needs to have the type
  CV_8UC1 and the same size as image ), it specifies the region in which the corners are detected.
  @param blockSize Size of an average block for computing a derivative covariation matrix over each
  pixel neighborhood. See cornerEigenValsAndVecs .
  @param useHarrisDetector Parameter indicating whether to use a Harris detector (see #cornerHarris)
  or #cornerMinEigenVal.
  @param k Free parameter of the Harris detector.

  @sa  cornerMinEigenVal, cornerHarris, calcOpticalFlowPyrLK, estimateRigidTransform,
*)

// CV_EXPORTS_W void goodFeaturesToTrack(
// InputArray image,
// OutputArray corners,
// int maxCorners,
// double qualityLevel,
// double minDistance,
// InputArray mask = noArray(),
// int blockSize = 3,
// bool useHarrisDetector = false,
// double k = 0.04);
// 5200
// ?goodFeaturesToTrack@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HNN0H_NN@Z
// void cv::goodFeaturesToTrack(
// class cv::_InputArray const &,
// class cv::_OutputArray const &,
// int,
// double,
// double,
// class cv::_InputArray const &,
// int,
// bool,
// double)
procedure goodFeaturesToTrack(image: TInputArray; corners: TOutputArray; maxCorners: Int; qualityLevel: double; minDistance: double; mask: TInputArray { = noArray() }; blockSize: Int = 3;
  useHarrisDetector: BOOL = false; k: double = 0.04); overload; external opencv_world_dll
// name '?goodFeaturesToTrack@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HNN0H_NN@Z'
  index 5200 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//
// CV_EXPORTS_W void goodFeaturesToTrack( InputArray image, OutputArray corners,
// int maxCorners, double qualityLevel, double minDistance,
// InputArray mask, int blockSize,
// int gradientSize, bool useHarrisDetector = false,
// double k = 0.04 );
// 5199
// ?goodFeaturesToTrack@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HNN0HH_NN@Z
// void cv::goodFeaturesToTrack(
// class cv::_InputArray const &,
// class cv::_OutputArray const &,
// int,
// double,
// double,
// class cv::_InputArray const &,
// int,
// int,
// bool,
// double)
procedure goodFeaturesToTrack(image: TInputArray; corners: TOutputArray; maxCorners: Int; qualityLevel: double; minDistance: double; mask: TInputArray; blockSize: Int; gradientSize: Int;
  useHarrisDetector: BOOL = false; k: double = 0.04); overload; external opencv_world_dll
// name '?goodFeaturesToTrack@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HNN0H_NN@Z'
  index 5199 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
//
(* * @brief The function is used to detect translational shifts that occur between two images.

  The operation takes advantage of the Fourier shift theorem for detecting the translational shift in
  the frequency domain. It can be used for fast image registration as well as motion estimation. For
  more information please see <http://en.wikipedia.org/wiki/Phase_correlation>

  Calculates the cross-power spectrum of two supplied source arrays. The arrays are padded if needed
  with getOptimalDFTSize.

  The function performs the following equations:
  - First it applies a Hanning window (see <http://en.wikipedia.org/wiki/Hann_function>) to each
  image to remove possible edge effects. This window is cached until the array size changes to speed
  up processing time.
  - Next it computes the forward DFTs of each source array:
  \f[\mathbf{G}_a = \mathcal{F}\{src_1\}, \; \mathbf{G}_b = \mathcal{F}\{src_2\}\f]
  where \f$\mathcal{F}\f$ is the forward DFT.
  - It then computes the cross-power spectrum of each frequency domain array:
  \f[R = \frac{ \mathbf{G}_a \mathbf{G}_b^*}{|\mathbf{G}_a \mathbf{G}_b^*|}\f]
  - Next the cross-correlation is converted back into the time domain via the inverse DFT:
  \f[r = \mathcal{F}^{-1}\{R\}\f]
  - Finally, it computes the peak location and computes a 5x5 weighted centroid around the peak to
  achieve sub-pixel accuracy.
  \f[(\Delta x, \Delta y) = \texttt{weightedCentroid} \{\arg \max_{(x, y)}\{r\}\}\f]
  - If non-zero, the response parameter is computed as the sum of the elements of r within the 5x5
  centroid around the peak location. It is normalized to a maximum of 1 (meaning there is a single
  peak) and will be smaller when there are multiple peaks.

  @param src1 Source floating point array (CV_32FC1 or CV_64FC1)
  @param src2 Source floating point array (CV_32FC1 or CV_64FC1)
  @param window Floating point array with windowing coefficients to reduce edge effects (optional).
  @param response Signal power within the 5x5 centroid around the peak, between 0 and 1 (optional).
  @returns detected phase shift (sub-pixel) between the two arrays.

  @sa dft, getOptimalDFTSize, idft, mulSpectrums createHanningWindow
*)
// CV_EXPORTS_W Point2d phaseCorrelate(InputArray src1, InputArray src2,
// InputArray window = noArray(), CV_OUT double* response = 0);
// 5846
// ?phaseCorrelate@cv@@YA?AV?$Point_@N@1@AEBV_InputArray@1@00PEAN@Z
// class cv::Point_<double> cv::phaseCorrelate(class cv::_InputArray const &,class cv::_InputArray const &,class cv::_InputArray const &,double *)
function phaseCorrelate(src1: TInputArray; src2: TInputArray; window: TInputArray { = noArray() }; response: pDouble { = 0 } ): TPoint2d; overload; external opencv_world_dll index 5846
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function phaseCorrelate(src1: TInputArray; src2: TInputArray; window: TInputArray { = noArray() }; Var response: double { = 0 } ): TPoint2d; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
function phaseCorrelate(src1: TInputArray; src2: TInputArray; window: TInputArray { = noArray() } ): TPoint2d; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
function phaseCorrelate(src1: TInputArray; src2: TInputArray): TPoint2d; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$ENDREGION 'imgproc.hpp'}
//
(* * @brief This function computes a Hanning window coefficients in two dimensions.

  See (http://en.wikipedia.org/wiki/Hann_function) and (http://en.wikipedia.org/wiki/Window_function)
  for more information.

  An example is shown below:
  @code
  // create hanning window of size 100x100 and type CV_32F
  Mat hann;
  createHanningWindow(hann, Size(100, 100), CV_32F);
  @endcode
  @param dst Destination array to place Hann coefficients in
  @param winSize The window size specifications (both width and height must be > 1)
  @param type Created array type
*)
// CV_EXPORTS_W void createHanningWindow(OutputArray dst, Size winSize, int type);
// 4283
// ?createHanningWindow@cv@@YAXAEBV_OutputArray@1@V?$Size_@H@1@H@Z
// void cv::createHanningWindow(class cv::_OutputArray const &,class cv::Size_<int>,int)
procedure createHanningWindow(dst: TOutputArray; winSize: UInt64 { TSize }; &type: Int); overload; external opencv_world_dll index 4283
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure createHanningWindow(const dst: TOutputArray; const winSize: TSize; &type: Int); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
//
{$REGION 'objectdetect.hpp'}

Type
  (* * @example samples/cpp/facedetect.cpp
    This program demonstrates usage of the Cascade classifier class
    \image html Cascade_Classifier_Tutorial_Result_Haar.jpg "Sample screenshot" width=321 height=254
  *)
  (* * @brief Cascade classifier class for object detection. *)

  pCascadeClassifier = ^TCascadeClassifier;

  TCascadeClassifier = record
  private
{$HINTS OFF}
    Dummy: array [0 .. 1] of UInt64;
{$HINTS ON}
  public
    class operator Initialize(out Dest: TCascadeClassifier); // CV_WRAP CascadeClassifier();
    (* * @brief Loads a classifier from a file.
      @param filename Name of the file from which the classifier is loaded. *)
    // CV_WRAP CascadeClassifier(const String& filename);
    class operator Finalize(var Dest: TCascadeClassifier); // ~CascadeClassifier();
    (* * @brief Checks whether the classifier has been loaded. *)
    // CV_WRAP bool empty() const;
    (* * @brief Loads a classifier from a file.

      @param filename Name of the file from which the classifier is loaded. The file may contain an old
      HAAR classifier trained by the haartraining application or a new cascade classifier trained by the
      traincascade application.
    *)
    function load(const filename: String): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_WRAP bool load( const String& filename );
    function load(const filename: CvStdString): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Reads a classifier from a FileStorage node.
      @note The file may contain a new cascade classifier (trained traincascade application) only. *)
    // CV_WRAP bool read( const FileNode& node );

    (* * @brief Detects objects of different sizes in the input image. The detected objects are returned as a list of rectangles.

      @param image Matrix of the type CV_8U containing an image where objects are detected.
      @param objects Vector of rectangles where each rectangle contains the detected object, the
      rectangles may be partially outside the original image.
      @param scaleFactor Parameter specifying how much the image size is reduced at each image scale.
      @param minNeighbors Parameter specifying how many neighbors each candidate rectangle should have
      to retain it.
      @param flags Parameter with the same meaning for an old cascade as in the function
      cvHaarDetectObjects. It is not used for a new cascade.
      @param minSize Minimum possible object size. Objects smaller than that are ignored.
      @param maxSize Maximum possible object size. Objects larger than that are ignored. If `maxSize == minSize` model is evaluated on single scale.

      The function is parallelized with the TBB library.

      @note
      -   (Python) A face detection example using cascade classifiers can be found at
      opencv_source_code/samples/python/facedetect.py
    *)
    procedure detectMultiScale(const image: TInputArray; const objects: TStdVectorRect; scaleFactor: double = 1.1; minNeighbors: Int = 3; flags: Int = 0); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure detectMultiScale(const image: TInputArray; const objects: TStdVectorRect; scaleFactor: double; minNeighbors: Int; flags: Int; const minSize: TSize { = Size() } ); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure detectMultiScale(const image: TInputArray; const objects: TStdVectorRect; scaleFactor: double; minNeighbors: Int; flags: Int; const minSize: TSize; const maxSize: TSize { = Size() } );
      overload; {$IFDEF USE_INLINE}inline; {$ENDIF}

    // CV_WRAP void detectMultiScale( InputArray image,
    // CV_OUT std::vector<Rect>& objects,
    // double scaleFactor = 1.1,
    // int minNeighbors = 3, int flags = 0,
    // Size minSize = Size(),
    // Size maxSize = Size() );

    (* * @overload
      @param image Matrix of the type CV_8U containing an image where objects are detected.
      @param objects Vector of rectangles where each rectangle contains the detected object, the
      rectangles may be partially outside the original image.
      @param numDetections Vector of detection numbers for the corresponding objects. An object's number
      of detections is the number of neighboring positively classified rectangles that were joined
      together to form the object.
      @param scaleFactor Parameter specifying how much the image size is reduced at each image scale.
      @param minNeighbors Parameter specifying how many neighbors each candidate rectangle should have
      to retain it.
      @param flags Parameter with the same meaning for an old cascade as in the function
      cvHaarDetectObjects. It is not used for a new cascade.
      @param minSize Minimum possible object size. Objects smaller than that are ignored.
      @param maxSize Maximum possible object size. Objects larger than that are ignored. If `maxSize == minSize` model is evaluated on single scale.
    *)
    // CV_WRAP_AS(detectMultiScale2) void detectMultiScale( InputArray image,
    // CV_OUT std::vector<Rect>& objects,
    // CV_OUT std::vector<int>& numDetections,
    // double scaleFactor=1.1,
    // int minNeighbors=3, int flags=0,
    // Size minSize=Size(),
    // Size maxSize=Size() );

    (* * @overload
      This function allows you to retrieve the final stage decision certainty of classification.
      For this, one needs to set `outputRejectLevels` on true and provide the `rejectLevels` and `levelWeights` parameter.
      For each resulting detection, `levelWeights` will then contain the certainty of classification at the final stage.
      This value can then be used to separate strong from weaker classifications.

      A code sample on how to use it efficiently can be found below:
      @code
      Mat img;
      vector<double> weights;
      vector<int> levels;
      vector<Rect> detections;
      CascadeClassifier model("/path/to/your/model.xml");
      model.detectMultiScale(img, detections, levels, weights, 1.1, 3, 0, Size(), Size(), true);
      cerr << "Detection " << detections[0] << " with weight " << weights[0] << endl;
      @endcode
    *)
    // CV_WRAP_AS(detectMultiScale3) void detectMultiScale( InputArray image,
    // CV_OUT std::vector<Rect>& objects,
    // CV_OUT std::vector<int>& rejectLevels,
    // CV_OUT std::vector<double>& levelWeights,
    // double scaleFactor = 1.1,
    // int minNeighbors = 3, int flags = 0,
    // Size minSize = Size(),
    // Size maxSize = Size(),
    // bool outputRejectLevels = false );

    // CV_WRAP bool isOldFormatCascade() const;
    // CV_WRAP Size getOriginalWindowSize() const;
    // CV_WRAP int getFeatureType() const;
    // void* getOldCascade();

    // CV_WRAP static bool convert(const String& oldcascade, const String& newcascade);

    // void setMaskGenerator(const Ptr<BaseCascadeClassifier::MaskGenerator>& maskGenerator);
    // Ptr<BaseCascadeClassifier::MaskGenerator> getMaskGenerator();

    // Ptr<BaseCascadeClassifier> cc;
  end;

  pImpl = type Pointer;

  pQRCodeDetector = ^TQRCodeDetector;

  TQRCodeDetector = record
  public
    class operator Initialize(out Dest: TQRCodeDetector); // CV_WRAP QRCodeDetector();
    class operator Finalize(var Dest: TQRCodeDetector);   // ~QRCodeDetector();

    (* * @brief sets the epsilon used during the horizontal scan of QR code stop marker detection.
      @param epsX Epsilon neighborhood, which allows you to determine the horizontal pattern
      of the scheme 1:1:3:1:1 according to QR code standard.
    *)
    // CV_WRAP void setEpsX(double epsX);
    (* * @brief sets the epsilon used during the vertical scan of QR code stop marker detection.
      @param epsY Epsilon neighborhood, which allows you to determine the vertical pattern
      of the scheme 1:1:3:1:1 according to QR code standard.
    *)
    // CV_WRAP void setEpsY(double epsY);

    (* * @brief Detects QR code in image and returns the quadrangle containing the code.
      @param img grayscale or color (BGR) image containing (or not) QR code.
      @param points Output vector of vertices of the minimum-area quadrangle containing the code.
    *)
    function detect(const img: TInputArray; const points: TOutputArray): BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // CV_WRAP bool detect(InputArray img, OutputArray points) const;

    (* * @brief Decodes QR code in image once it's found by the detect() method.

      Returns UTF8-encoded output string or empty string if the code cannot be decoded.
      @param img grayscale or color (BGR) image containing QR code.
      @param points Quadrangle vertices found by detect() method (or some other algorithm).
      @param straight_qrcode The optional output image containing rectified and binarized QR code
    *)
    // CV_WRAP std::string decode(InputArray img, InputArray points, OutputArray straight_qrcode = noArray());

    (* * @brief Decodes QR code on a curved surface in image once it's found by the detect() method.

      Returns UTF8-encoded output string or empty string if the code cannot be decoded.
      @param img grayscale or color (BGR) image containing QR code.
      @param points Quadrangle vertices found by detect() method (or some other algorithm).
      @param straight_qrcode The optional output image containing rectified and binarized QR code
    *)
    // CV_WRAP cv::String decodeCurved(InputArray img, InputArray points, OutputArray straight_qrcode = noArray());

    (* * @brief Both detects and decodes QR code

      @param img grayscale or color (BGR) image containing QR code.
      @param points optional output array of vertices of the found QR code quadrangle. Will be empty if not found.
      @param straight_qrcode The optional output image containing rectified and binarized QR code
    *)
    function detectAndDecode(const img: TInputArray; const points: TOutputArray { =noArray() }; const straight_qrcode: TOutputArray { = noArray() } ): CppString; overload; {$IFDEF USE_INLINE}inline;
{$ENDIF}
    function detectAndDecode(const img: TInputArray; const points: TOutputArray { =noArray() } ): CppString; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function detectAndDecode(const img: TInputArray): CppString; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // CV_WRAP std::string detectAndDecode(InputArray img, OutputArray points=noArray(),
    // OutputArray straight_qrcode = noArray());

    (* * @brief Both detects and decodes QR code on a curved surface

      @param img grayscale or color (BGR) image containing QR code.
      @param points optional output array of vertices of the found QR code quadrangle. Will be empty if not found.
      @param straight_qrcode The optional output image containing rectified and binarized QR code
    *)
    // CV_WRAP std::string detectAndDecodeCurved(InputArray img, OutputArray points=noArray(),
    // OutputArray straight_qrcode = noArray());

    (* * @brief Detects QR codes in image and returns the vector of the quadrangles containing the codes.
      @param img grayscale or color (BGR) image containing (or not) QR codes.
      @param points Output vector of vector of vertices of the minimum-area quadrangle containing the codes.
    *)
    // CV_WRAP bool detectMulti(InputArray img, OutputArray points) const;
    function detectMulti(const img: TInputArray; const points: TOutputArray): BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Decodes QR codes in image once it's found by the detect() method.
      @param img grayscale or color (BGR) image containing QR codes.
      @param decoded_info UTF8-encoded output vector of string or empty vector of string if the codes cannot be decoded.
      @param points vector of Quadrangle vertices found by detect() method (or some other algorithm).
      @param straight_qrcode The optional output vector of images containing rectified and binarized QR codes
    *)
    function decodeMulti(const img: TInputArray; const points: TInputArray; const decoded_info: TStdVectorCppString; const straight_qrcode: TOutputArrayOfArrays): BOOL; overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
    function decodeMulti(const img: TInputArray; const points: TInputArray; const decoded_info: TStdVectorCppString): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // CV_WRAP bool decodeMulti(
    // InputArray img, InputArray points,
    // CV_OUT std::vector<std::string>& decoded_info,
    // OutputArrayOfArrays straight_qrcode = noArray()
    // ) const;

    (* * @brief Both detects and decodes QR codes
      @param img grayscale or color (BGR) image containing QR codes.
      @param decoded_info UTF8-encoded output vector of string or empty vector of string if the codes cannot be decoded.
      @param points optional output vector of vertices of the found QR code quadrangles. Will be empty if not found.
      @param straight_qrcode The optional output vector of images containing rectified and binarized QR codes
    *)
    function detectAndDecodeMulti(const img: TInputArray; const decoded_info: TStdVectorCppString; const points: TOutputArray { = noArray() };
      const straight_qrcode: TOutputArrayOfArrays { = noArray() } ): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function detectAndDecodeMulti(const img: TInputArray; const decoded_info: TStdVectorCppString; const points: TOutputArray { = noArray() }
      ): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function detectAndDecodeMulti(const img: TInputArray; const decoded_info: TStdVectorCppString): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // CV_WRAP
    // bool detectAndDecodeMulti(
    // InputArray img, CV_OUT std::vector<std::string>& decoded_info,
    // OutputArray points = noArray(),
    // OutputArrayOfArrays straight_qrcode = noArray()
    // ) const;

  private
{$HINTS OFF}
    // struct Impl;
    p: pImpl; // Ptr<Impl> p;
{$HINTS ON}
  end;

{$ENDREGION 'objectdetect.hpp'}
  //
{$REGION 'photo.hpp'}

  (* * @brief Transforms a color image to a grayscale image. It is a basic tool in digital printing, stylized
    black-and-white photograph rendering, and in many single channel image processing applications
    @cite CL12 .

    @param src Input 8-bit 3-channel image.
    @param grayscale Output 8-bit 1-channel image.
    @param color_boost Output 8-bit 3-channel image.

    This function is to be applied on color images.
  *)
  // CV_EXPORTS_W void decolor( InputArray src, OutputArray grayscale, OutputArray color_boost);
  // 4365
  // ?decolor@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@1@Z
  // void cv::decolor(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::_OutputArray const &)
procedure decolor(Src: TInputArray; grayscale: TOutputArray; color_boost: TOutputArray); external opencv_world_dll
// name '?decolor@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@1@Z'
  index 4365
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Filtering is the fundamental operation in image and video processing. Edge-preserving smoothing
  filters are used in many different applications @cite EM11 .

  @param src Input 8-bit 3-channel image.
  @param dst Output 8-bit 3-channel image.
  @param flags Edge preserving filters: cv::RECURS_FILTER or cv::NORMCONV_FILTER
  @param sigma_s %Range between 0 to 200.
  @param sigma_r %Range between 0 to 1.
*)
// CV_EXPORTS_W void edgePreservingFilter(InputArray src, OutputArray dst, int flags = 1,
// float sigma_s = 60, float sigma_r = 0.4f);
// 4555
// ?edgePreservingFilter@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@HMM@Z
// void cv::edgePreservingFilter(class cv::_InputArray const &,class cv::_OutputArray const &,int,float,float)
procedure edgePreservingFilter(Src: TInputArray; dst: TOutputArray; flags: Int = 1; sigma_s: float = 60; sigma_r: float = 0.4); external opencv_world_dll index 4555
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief This filter enhances the details of a particular image.

  @param src Input 8-bit 3-channel image.
  @param dst Output image with the same size and type as src.
  @param sigma_s %Range between 0 to 200.
  @param sigma_r %Range between 0 to 1.
*)
// CV_EXPORTS_W void detailEnhance(InputArray src, OutputArray dst, float sigma_s = 10,
// float sigma_r = 0.15f);
// 4415
// ?detailEnhance@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@MM@Z
// void cv::detailEnhance(class cv::_InputArray const &,class cv::_OutputArray const &,float,float)
procedure detailEnhance(Src: TInputArray; dst: TOutputArray; sigma_s: float = 10; sigma_r: float = 0.15); external opencv_world_dll index 4415
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Pencil-like non-photorealistic line drawing
  @param src Input 8-bit 3-channel image.
  @param dst1 Output 8-bit 1-channel image.
  @param dst2 Output image with the same size and type as src.
  @param sigma_s %Range between 0 to 200.
  @param sigma_r %Range between 0 to 1.
  @param shade_factor %Range between 0 to 0.1.
*)
// CV_EXPORTS_W void pencilSketch(InputArray src, OutputArray dst1, OutputArray dst2,
// float sigma_s = 60, float sigma_r = 0.07f, float shade_factor = 0.02f);
// 5841
// ?pencilSketch@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@1MMM@Z
// void cv::pencilSketch(class cv::_InputArray const &,class cv::_OutputArray const &,class cv::_OutputArray const &,float,float,float)
procedure pencilSketch(Src: TInputArray; dst1: TOutputArray; dst2: TOutputArray; sigma_s: float = 60; sigma_r: float = 0.07; shade_factor: float = 0.02); external opencv_world_dll index 5841
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* * @brief Stylization aims to produce digital imagery with a wide variety of effects not focused on
  photorealism. Edge-aware filters are ideal for stylization, as they can abstract regions of low
  contrast while preserving, or enhancing, high-contrast features.

  @param src Input 8-bit 3-channel image.
  @param dst Output image with the same size and type as src.
  @param sigma_s %Range between 0 to 200.
  @param sigma_r %Range between 0 to 1.
*)
// CV_EXPORTS_W void stylization(InputArray src, OutputArray dst, float sigma_s = 60,
// float sigma_r = 0.45f);
// 6561
// ?stylization@cv@@YAXAEBV_InputArray@1@AEBV_OutputArray@1@MM@Z
// void cv::stylization(class cv::_InputArray const &,class cv::_OutputArray const &,float,float)
procedure stylization(Src: TInputArray; dst: TOutputArray; sigma_s: float = 60; sigma_r: float = 0.45); external opencv_world_dll index 6561
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{$ENDREGION 'photo.hpp'}
//
{$REGION 'fast_math.hpp'}
(* * @brief Rounds floating-point number to the nearest integer

  @param value floating-point number. If the value is outside of INT_MIN ... INT_MAX range, the
  result is not defined.
*)
// CV_INLINE int cvRound( double value )
// 4320
// ?cvRound@@YAHAEBUsoftdouble@cv@@@Z
// int cvRound(struct cv::softdouble const &)
// function cvRound(value: double): int; external opencv_world_dll index 4320 {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
function cvRound(Value: double): Int; {$IFDEF USE_INLINE}inline; {$ENDIF}
{$ENDREGION 'fast_math.hpp'}
//
{$REGION 'videoio.hpp'}

type
  VideoCaptureAPIs = (           //
    CAP_ANY = 0,                 // !< Auto detect == 0
    CAP_VFW = 200,               // !< Video For Windows (obsolete, removed)
    CAP_V4L = 200,               // !< V4L/V4L2 capturing support
    CAP_V4L2 = CAP_V4L,          // !< Same as CAP_V4L
    CAP_FIREWIRE = 300,          // !< IEEE 1394 drivers
    CAP_FIREWARE = CAP_FIREWIRE, // !< Same value as CAP_FIREWIRE
    CAP_IEEE1394 = CAP_FIREWIRE, // !< Same value as CAP_FIREWIRE
    CAP_DC1394 = CAP_FIREWIRE,   // !< Same value as CAP_FIREWIRE
    CAP_CMU1394 = CAP_FIREWIRE,  // !< Same value as CAP_FIREWIRE
    CAP_QT = 500,                // !< QuickTime (obsolete, removed)
    CAP_UNICAP = 600,            // !< Unicap drivers (obsolete, removed)
    CAP_DSHOW = 700,             // !< DirectShow (via videoInput)
    CAP_PVAPI = 800,             // !< PvAPI, Prosilica GigE SDK
    CAP_OPENNI = 900,            // !< OpenNI (for Kinect)
    CAP_OPENNI_ASUS = 910,       // !< OpenNI (for Asus Xtion)
    CAP_ANDROID = 1000,          // !< Android - not used
    CAP_XIAPI = 1100,            // !< XIMEA Camera API
    CAP_AVFOUNDATION = 1200,     // !< AVFoundation framework for iOS (OS X Lion will have the same API)
    CAP_GIGANETIX = 1300,        // !< Smartek Giganetix GigEVisionSDK
    CAP_MSMF = 1400,             // !< Microsoft Media Foundation (via videoInput)
    CAP_WINRT = 1410,            // !< Microsoft Windows Runtime using Media Foundation
    CAP_INTELPERC = 1500,        // !< RealSense (former Intel Perceptual Computing SDK)
    CAP_REALSENSE = 1500,        // !< Synonym for CAP_INTELPERC
    CAP_OPENNI2 = 1600,          // !< OpenNI2 (for Kinect)
    CAP_OPENNI2_ASUS = 1610,     // !< OpenNI2 (for Asus Xtion and Occipital Structure sensors)
    CAP_OPENNI2_ASTRA = 1620,    // !< OpenNI2 (for Orbbec Astra)
    CAP_GPHOTO2 = 1700,          // !< gPhoto2 connection
    CAP_GSTREAMER = 1800,        // !< GStreamer
    CAP_FFMPEG = 1900,           // !< Open and record video file or stream using the FFMPEG library
    CAP_IMAGES = 2000,           // !< OpenCV Image Sequence (e.g. img_%02d.jpg)
    CAP_ARAVIS = 2100,           // !< Aravis SDK
    CAP_OPENCV_MJPEG = 2200,     // !< Built-in OpenCV MotionJPEG codec
    CAP_INTEL_MFX = 2300,        // !< Intel MediaSDK
    CAP_XINE = 2400,             // !< XINE engine (Linux)
    CAP_UEYE = 2500              // !< uEye Camera API
    );

  pVideoCapture = ^TVideoCapture;

  TVideoCapture = record
  public
    (* * @brief Default constructor
      @note In @ref videoio_c "C API", when you finished working with video, release CvCapture structure with
      cvReleaseCapture(), or use Ptr\<CvCapture\> that calls cvReleaseCapture() automatically in the
      destructor.
    *)
    class operator Initialize(out Dest: TVideoCapture); // CV_WRAP VideoCapture();
    class operator assign(var Dest: TVideoCapture; const [ref] Src: TVideoCapture);

    (* * @overload
      @brief  Opens a video file or a capturing device or an IP video stream for video capturing with API Preference

      @param filename it can be:
      - name of video file (eg. `video.avi`)
      - or image sequence (eg. `img_%02d.jpg`, which will read samples like `img_00.jpg, img_01.jpg, img_02.jpg, ...`)
      - or URL of video stream (eg. `protocol://host:port/script_name?script_params|auth`)
      - or GStreamer pipeline string in gst-launch tool format in case if GStreamer is used as backend
      Note that each video stream or IP camera feed has its own URL scheme. Please refer to the
      documentation of source stream to know the right URL.
      @param apiPreference preferred Capture API backends to use. Can be used to enforce a specific reader
      implementation if multiple are available: e.g. cv::CAP_FFMPEG or cv::CAP_IMAGES or cv::CAP_DSHOW.

      @sa cv::VideoCaptureAPIs
    *)

    // CV_WRAP explicit VideoCapture(const String & filename, int apiPreference = CAP_ANY);

    (* * @overload
      @brief Opens a video file or a capturing device or an IP video stream for video capturing with API Preference and parameters

      The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
      See cv::VideoCaptureProperties
    *)
    // CV_WRAP explicit VideoCapture(const String & filename, int apiPreference, const std: : vector<int> & params);

    (* * @overload
      @brief  Opens a camera for video capturing

      @param index id of the video capturing device to open. To open default camera using default backend just pass 0.
      (to backward compatibility usage of camera_id + domain_offset (CAP_* ) is valid when apiPreference is CAP_ANY)
      @param apiPreference preferred Capture API backends to use. Can be used to enforce a specific reader
      implementation if multiple are available: e.g. cv::CAP_DSHOW or cv::CAP_MSMF or cv::CAP_V4L.

      @sa cv::VideoCaptureAPIs
    *)
    // CV_WRAP explicit VideoCapture(int index, int apiPreference = CAP_ANY);

    (* * @overload
      @brief Opens a camera for video capturing with API Preference and parameters

      The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
      See cv::VideoCaptureProperties
    *)
    // CV_WRAP explicit VideoCapture(int index, int apiPreference, const std: : vector<int> & params);

    (* * @brief Default destructor

      The method first calls VideoCapture::release to close the already opened file or camera.
    *)
    class operator Finalize(var Dest: TVideoCapture); // virtual ~ VideoCapture();

    (* * @brief  Opens a video file or a capturing device or an IP video stream for video capturing.

      @overload

      Parameters are same as the constructor VideoCapture(const String& filename, int apiPreference = CAP_ANY)
      @return `true` if the file has been successfully opened

      The method first calls VideoCapture::release to close the already opened file or camera.
    *)
    // CV_WRAP virtual Bool open(const String & filename, int apiPreference = CAP_ANY);
    function open(const filename: String; const apiPreference: VideoCaptureAPIs = CAP_ANY): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief  Opens a camera for video capturing

      @overload

      The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
      See cv::VideoCaptureProperties

      @return `true` if the file has been successfully opened

      The method first calls VideoCapture::release to close the already opened file or camera.
    *)
    // CV_WRAP virtual Bool open(const String & filename, int apiPreference, const std: : vector<int> & params);
    function open(const filename: String; const apiPreference: VideoCaptureAPIs { = CAP_ANY }; const params: Vector<Int>): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief  Opens a camera for video capturing

      @overload

      Parameters are same as the constructor VideoCapture(int index, int apiPreference = CAP_ANY)
      @return `true` if the camera has been successfully opened.

      The method first calls VideoCapture::release to close the already opened file or camera.
    *)
    // CV_WRAP virtual Bool open(int index, int apiPreference = CAP_ANY);
    function open(const index: Int; const apiPreference: VideoCaptureAPIs = CAP_ANY): BOOL; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Returns true if video capturing has been initialized already.

      @overload

      The `params` parameter allows to specify extra parameters encoded as pairs `(paramId_1, paramValue_1, paramId_2, paramValue_2, ...)`.
      See cv::VideoCaptureProperties

      @return `true` if the camera has been successfully opened.

      The method first calls VideoCapture::release to close the already opened file or camera.
    *)
    // CV_WRAP virtual Bool open(int index, int apiPreference, const std: : vector<int> & params);

    (* * @brief Returns true if video capturing has been initialized already.

      If the previous call to VideoCapture constructor or VideoCapture::open() succeeded, the method returns
      true.
    *)
    function isOpened: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_WRAP virtual Bool isOpened()  const;

    (* * @brief Closes video file or capturing device.

      The method is automatically called by subsequent VideoCapture::open and by VideoCapture
      destructor.

      The C function also deallocates memory and clears \*capture pointer.
    *)
    // CV_WRAP virtual void release();

    (* * @brief Grabs the next frame from video file or capturing device.

      @return `true` (non-zero) in the case of success.

      The method/function grabs the next frame from video file or camera and returns true (non-zero) in
      the case of success.

      The primary use of the function is in multi-camera environments, especially when the cameras do not
      have hardware synchronization. That is, you call VideoCapture::grab() for each camera and after that
      call the slower method VideoCapture::retrieve() to decode and get frame from each camera. This way
      the overhead on demosaicing or motion jpeg decompression etc. is eliminated and the retrieved frames
      from different cameras will be closer in time.

      Also, when a connected camera is multi-head (for example, a stereo camera or a Kinect device), the
      correct way of retrieving data from it is to call VideoCapture::grab() first and then call
      VideoCapture::retrieve() one or more times with different values of the channel parameter.

      @ref tutorial_kinect_openni
    *)
    // CV_WRAP virtual Bool grab();

    (* * @brief Decodes and returns the grabbed video frame.

      @param [out] image the video frame is returned here. If no frames has been grabbed the image will be empty.
      @param flag it could be a frame index or a driver specific flag
      @return `false` if no frames has been grabbed

      The method decodes and returns the just grabbed frame. If no frames has been grabbed
      (camera has been disconnected, or there are no more frames in video file), the method returns false
      and the function returns an empty image (with %cv::Mat, test it with Mat::empty()).

      @sa read()

      @note In @ref videoio_c "C API", functions cvRetrieveFrame() and cv.RetrieveFrame() return image stored inside the video
      capturing structure. It is not allowed to modify or release the image! You can copy the frame using
      cvCloneImage and then do whatever you want with the copy.
    *)
    // CV_WRAP virtual Bool retrieve(OutputArray image, int flag = 0);

    (* * @brief Stream operator to read the next video frame.
      @sa read()
    *)
    // virtual VideoCapture & operator >> (CV_OUT Mat & image);

    (* * @overload
      @sa read()
    *)
    // virtual VideoCapture & operator >> (CV_OUT UMAT & image);

    (* * @brief Grabs, decodes and returns the next video frame.

      @param [out] image the video frame is returned here. If no frames has been grabbed the image will be empty.
      @return `false` if no frames has been grabbed

      The method/function combines VideoCapture::grab() and VideoCapture::retrieve() in one call. This is the
      most convenient method for reading video files or capturing data from decode and returns the just
      grabbed frame. If no frames has been grabbed (camera has been disconnected, or there are no more
      frames in video file), the method returns false and the function returns empty image (with %cv::Mat, test it with Mat::empty()).

      @note In @ref videoio_c "C API", functions cvRetrieveFrame() and cv.RetrieveFrame() return image stored inside the video
      capturing structure. It is not allowed to modify or release the image! You can copy the frame using
      cvCloneImage and then do whatever you want with the copy.
    *)
    function read(const image: TOutputArray): BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}// CV_WRAP virtual Bool read(OutputArray image);

    (* * @brief Sets a property in the VideoCapture.

      @param propId Property identifier from cv::VideoCaptureProperties (eg. cv::CAP_PROP_POS_MSEC, cv::CAP_PROP_POS_FRAMES, ...)
      or one from @ref videoio_flags_others
      @param value Value of the property.
      @return `true` if the property is supported by backend used by the VideoCapture instance.
      @note Even if it returns `true` this doesn't ensure that the property
      value has been accepted by the capture device. See note in VideoCapture::get()
    *)
    // CV_WRAP virtual Bool set (int propId, double value);

    (* * @brief Returns the specified VideoCapture property

      @param propId Property identifier from cv::VideoCaptureProperties (eg. cv::CAP_PROP_POS_MSEC, cv::CAP_PROP_POS_FRAMES, ...)
      or one from @ref videoio_flags_others
      @return Value for the specified property. Value 0 is returned when querying a property that is
      not supported by the backend used by the VideoCapture instance.

      @note Reading / writing properties involves many layers. Some unexpected result might happens
      along this chain.
      @code{.txt}
      VideoCapture -> API Backend -> Operating System -> Device Driver -> Device Hardware
      @endcode
      The returned value might be different from what really used by the device or it could be encoded
      using device dependent rules (eg. steps or percentage). Effective behaviour depends from device
      driver and API Backend

    *)
    // CV_WRAP virtual double get(int propId)  const;

    (* * @brief Returns used backend API name

      @note Stream should be opened.
    *)
    // CV_WRAP String getBackendName()  const;

    (* * Switches exceptions mode
      *
      * methods raise exceptions if not successful instead of returning an error code
    *)
    // CV_WRAP void setExceptionMode(Bool enable) { throwOnFail = enable; }

    /// query if exception mode is active
    // CV_WRAP Bool getExceptionMode() { return throwOnFail; }

    (* * @brief Wait for ready frames from VideoCapture.

      @param streams input video streams
      @param readyIndex stream indexes with grabbed frames (ready to use .retrieve() to fetch actual frame)
      @param timeoutNs number of nanoseconds (0 - infinite)
      @return `true` if streamReady is not empty

      @throws Exception %Exception on stream errors (check .isOpened() to filter out malformed streams) or VideoCapture type is not supported

      The primary use of the function is in multi-camera environments.
      The method fills the ready state vector, grabs video frame, if camera is ready.

      After this call use VideoCapture::retrieve() to decode and fetch frame data.
    *)
    // static (* CV_WRAP *) Bool waitAny(const std: : vector<VideoCapture> & streams, CV_OUT std: : vector<int> & readyIndex, int64 timeoutNs = 0);
    //
    class operator Implicit(const filename: string): TVideoCapture; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const index: Int): TVideoCapture; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator GreaterThan(const VideoCapture: TVideoCapture; Var frame: TMat): BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
  private
{$HINTS OFF}
    Dummy: array [0 .. 47] of Byte;
    // Ptr<CvCapture> cap;
    // Ptr<IVideoCapture> icap;
    // bool throwOnFail;

    // friend class internal::VideoCapturePrivateAccessor;
{$HINTS ON}
  end;

{$ENDREGION 'videoio.hpp'}
  //
{$REGION 'tracking.hpp'}

  (* * @brief Calculates an optical flow for a sparse feature set using the iterative Lucas-Kanade method with
    pyramids.

    @param prevImg first 8-bit input image or pyramid constructed by buildOpticalFlowPyramid.
    @param nextImg second input image or pyramid of the same size and the same type as prevImg.
    @param prevPts vector of 2D points for which the flow needs to be found; point coordinates must be
    single-precision floating-point numbers.
    @param nextPts output vector of 2D points (with single-precision floating-point coordinates)
    containing the calculated new positions of input features in the second image; when
    OPTFLOW_USE_INITIAL_FLOW flag is passed, the vector must have the same size as in the input.
    @param status output status vector (of unsigned chars); each element of the vector is set to 1 if
    the flow for the corresponding features has been found, otherwise, it is set to 0.
    @param err output vector of errors; each element of the vector is set to an error for the
    corresponding feature, type of the error measure can be set in flags parameter; if the flow wasn't
    found then the error is not defined (use the status parameter to find such cases).
    @param winSize size of the search window at each pyramid level.
    @param maxLevel 0-based maximal pyramid level number; if set to 0, pyramids are not used (single
    level), if set to 1, two levels are used, and so on; if pyramids are passed to input then
    algorithm will use as many levels as pyramids have but no more than maxLevel.
    @param criteria parameter, specifying the termination criteria of the iterative search algorithm
    (after the specified maximum number of iterations criteria.maxCount or when the search window
    moves by less than criteria.epsilon.
    @param flags operation flags:
    -   **OPTFLOW_USE_INITIAL_FLOW** uses initial estimations, stored in nextPts; if the flag is
    not set, then prevPts is copied to nextPts and is considered the initial estimate.
    -   **OPTFLOW_LK_GET_MIN_EIGENVALS** use minimum eigen values as an error measure (see
    minEigThreshold description); if the flag is not set, then L1 distance between patches
    around the original and a moved point, divided by number of pixels in a window, is used as a
    error measure.
    @param minEigThreshold the algorithm calculates the minimum eigen value of a 2x2 normal matrix of
    optical flow equations (this matrix is called a spatial gradient matrix in @cite Bouguet00), divided
    by number of pixels in a window; if this value is less than minEigThreshold, then a corresponding
    feature is filtered out and its flow is not processed, so it allows to remove bad points and get a
    performance boost.

    The function implements a sparse iterative version of the Lucas-Kanade optical flow in pyramids. See
    @cite Bouguet00 . The function is parallelized with the TBB library.

    @note

    -   An example using the Lucas-Kanade optical flow algorithm can be found at
    opencv_source_code/samples/cpp/lkdemo.cpp
    -   (Python) An example using the Lucas-Kanade optical flow algorithm can be found at
    opencv_source_code/samples/python/lk_track.py
    -   (Python) An example using the Lucas-Kanade tracker for homography matching can be found at
    opencv_source_code/samples/python/lk_homography.py
  *)
  // CV_EXPORTS_W void calcOpticalFlowPyrLK( InputArray prevImg, InputArray nextImg,
  // InputArray prevPts, InputOutputArray nextPts,
  // OutputArray status, OutputArray err,
  // Size winSize = Size(21,21), int maxLevel = 3,
  // TermCriteria criteria = TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01),
  // int flags = 0, double minEigThreshold = 1e-4 );
  // 3724
  // ?calcOpticalFlowPyrLK@cv@@YAXAEBV_InputArray@1@00AEBV_InputOutputArray@1@AEBV_OutputArray@1@2V?$Size_@H@1@HVTermCriteria@1@HN@Z
  // void cv::calcOpticalFlowPyrLK(class cv::_InputArray const &,class cv::_InputArray const &,class cv::_InputArray const &,class cv::_InputOutputArray const &,class cv::_OutputArray const &,class cv::_OutputArray const &,class cv::Size_<int>,int,class cv::TermCriteria,int,double)
procedure calcOpticalFlowPyrLK(prevImg: TInputArray; nextImg: TInputArray; prevPts: TInputArray; nextPts: TInputOutputArray; status: TOutputArray; err: TOutputArray;
  winSize: UInt64 { TSize = Size(21,21) }; maxLevel: Int { = 3 }; criteria: TTermCriteria { = TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01) }; flags: Int = 0;
  minEigThreshold: double = 1E-4); overload; external opencv_world_dll
// name '?calcOpticalFlowPyrLK@cv@@YAXAEBV_InputArray@1@00AEBV_InputOutputArray@1@AEBV_OutputArray@1@2V?$Size_@H@1@HVTermCriteria@1@HN@Z'
  index 3724
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray;
  const err: TOutputArray; const winSize: TSize { = Size(21,21) }; maxLevel: Int = 3); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
// ; criteria: TTermCriteria { = TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01) }; flags: Int = 0; minEigThreshold: double = 1E-4
procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray;
  const err: TOutputArray); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// ; winSize:  TSize {= Size(21,21) }; maxLevel: Int { = 3 }
// ; criteria: TTermCriteria { = TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01) }; flags: Int = 0; minEigThreshold: double = 1E-4

{$ENDREGION 'tracking.hpp'}
//
{$REGION 'utility.hpp'}

type

  TParam = (Param_INT = 0, Param_BOOLEAN = 1, Param_REAL = 2, Param_STRING = 3, Param_MAT = 4, Param_MAT_VECTOR = 5, Param_ALGORITHM = 6, Param_FLOAT = 7, Param_UNSIGNED_INT = 8, Param_UInt64 = 9,
    Param_UCHAR = 11, Param_SCALAR = 12);

  pCommandLineParser = ^TCommandLineParser;

  TCommandLineParser = record
  public
    (* * @brief Constructor

      Initializes command line parser object

      @param argc number of command line arguments (from main())
      @param argv array of command line arguments (from main())
      @param keys string describing acceptable command line parameters (see class description for syntax)
    *)
    // class function CommandLineParser(const keys: String): TCommandLineParser; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    procedure CommandLineParser(const keys: String); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // CommandLineParser(int argc, const char* const argv[], const String& keys);

    (* * @brief Copy constructor *)
    // CommandLineParser(const CommandLineParser& parser);

    (* * @brief Assignment operator *)
    // CommandLineParser& operator= (const CommandLineParser& parser);

    (* * @brief Destructor *)
    class operator Finalize(var Dest: TCommandLineParser); // ~CommandLineParser();

    (* * @brief Returns application path

      This method returns the path to the executable from the command line (`argv[0]`).

      For example, if the application has been started with such a command:
      @code{.sh}
      $ ./bin/my-executable
      @endcode
      this method will return `./bin`.
    *)
    // String getPathToApplication() const;

    (* * @brief Access arguments by name

      Returns argument converted to selected type. If the argument is not known or can not be
      converted to selected type, the error flag is set (can be checked with @ref check).

      For example, define:
      @code{.cpp}
      String keys = "{N count||}";
      @endcode

      Call:
      @code{.sh}
      $ ./my-app -N=20
      # or
      $ ./my-app --count=20
      @endcode

      Access:
      @code{.cpp}
      int N = parser.get<int>("N");
      @endcode

      @param name name of the argument
      @param space_delete remove spaces from the left and right of the string
      @tparam T the argument will be converted to this type if possible

      @note You can access positional arguments by their `@`-prefixed name:
      @code{.cpp}
      parser.get<String>("@image");
      @endcode
    *)
    // template <typename T>
    // T get(const String& name, bool space_delete = true) const
    function get<T>(const name: String; space_delete: BOOL = true): T; {$IFDEF USE_INLINE}inline; {$ENDIF}
    {
      T val = T();
      getByName(name, space_delete, ParamType<T>::type, (void*)&val);
      return val;
    }

    (* * @brief Access positional arguments by index

      Returns argument converted to selected type. Indexes are counted from zero.

      For example, define:
      @code{.cpp}
      String keys = "{@arg1||}{@arg2||}"
      @endcode

      Call:
      @code{.sh}
      ./my-app abc qwe
      @endcode

      Access arguments:
      @code{.cpp}
      String val_1 = parser.get<String>(0); // returns "abc", arg1
      String val_2 = parser.get<String>(1); // returns "qwe", arg2
      @endcode

      @param index index of the argument
      @param space_delete remove spaces from the left and right of the string
      @tparam T the argument will be converted to this type if possible
    *)
    // template <typename T>
    // T get(int index, bool space_delete = true) const
    {
      T val = T();
      getByIndex(index, space_delete, ParamType<T>::type, (void*)&val);
      return val;
    }

    (* * @brief Check if field was provided in the command line

      @param name argument name to check
    *)
    function has(const name: String): BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}  // bool has(const String& name) const;

    (* * @brief Check for parsing errors

      Returns false if error occurred while accessing the parameters (bad conversion, missing arguments,
      etc.). Call @ref printErrors to print error messages list.
    *)
    function check: BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // bool check() const;

    (* * @brief Set the about message

      The about message will be shown when @ref printMessage is called, right before arguments table.
    *)
    procedure about(const message: String); {$IFDEF USE_INLINE}inline; {$ENDIF}
    // void about(const String& message);

    (* * @brief Print help message

      This method will print standard help message containing the about message and arguments description.

      @sa about
    *)
    procedure printMessage; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // void printMessage() const;

    (* * @brief Print list of errors occurred
      @sa check *)
    procedure printErrors; {$IFDEF USE_INLINE}inline; {$ENDIF}// void printErrors() const;
    //
    // class operator assign(var Dest: TCommandLineParser; const [ref] Src: TCommandLineParser);
    class operator Implicit(const keys: String): TCommandLineParser; {$IFDEF USE_INLINE}inline; {$ENDIF}
  private
    function TypeToTParam<T>(): TParam; {$IFDEF USE_INLINE}inline; {$ENDIF}
    //
    // void getByName(const String& name, bool space_delete, Param type, void* dst) const;
    procedure getByName(const name: String; const space_delete: BOOL; &type: TParam; dst: Pointer); {$IFDEF USE_INLINE}inline; {$ENDIF}
    // void getByIndex(int index, bool space_delete, Param type, void* dst) const;
  private
{$HINTS OFF}
    // struct Impl;
    impl: Pointer; // Impl* impl;
{$HINTS ON}
  end;
{$ENDREGION 'utility.hpp'}
  //
{$REGION 'check.hpp'}

  (* * Returns string of cv::Mat depth value: CV_8UC3 -> "CV_8UC3" or "<invalid type>" *)
  // CV_EXPORTS const String typeToString(int type);
  // ?typeToString@cv@@YA?BV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@H@Z
  // class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const cv::typeToString(int)
function typeToString(&type: Int): CppString; external opencv_world_dll name '?typeToString@cv@@YA?BV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@H@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{$ENDREGION 'check.hpp'}
//
{$REGION 'ml.hpp'}

(* ***************************************************************************************\
  *                                   Support Vector Machines                              *
  \*************************************************************************************** *)
type

  (* * @brief Sample types *)
  SampleTypes = (   //
    ROW_SAMPLE = 0, // !< each training sample is a row of samples
    COL_SAMPLE = 1  // !< each training sample occupies a column of samples
    );

  (* * @brief Support Vector Machines.
    @sa @ref ml_intro_svm
  *)

  pSVM = ^TSVM;

  TSVM = record
  private type
    vftable_func = type Pointer;
    pvftable = ^vftable_func;
  public type
    TPtrSVM = TPtr<TSVM>;
  public
    _vftable: vftable_func; // ppvftable_SVM;
    class function vftable(const s: TSVM; const index: integer): Pointer; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Finalize(var Dest: TSVM);
  public
    // class CV_EXPORTS Kernel : public Algorithm
    // {
    // public:
    // virtual int getType() const = 0;
    // virtual void calc( int vcount, int n, const float* vecs, const float* another, float* results ) = 0;
    // };

    (* * Type of a %SVM formulation.
      See SVM::Types. Default value is SVM::C_SVC. *)
    (* * @see setType *)
    // CV_WRAP virtual int getType() const = 0;
    function getType: Int; {$IFDEF USE_INLINE}inline; {$ENDIF}
    //
    (* * @copybrief getType @see getType *)
    // CV_WRAP virtual void setType(int val) = 0;
    procedure setType(val: Int); {$IFDEF USE_INLINE}inline; {$ENDIF}
    //
    (* * Parameter \f$\gamma\f$ of a kernel function.
      For SVM::POLY, SVM::RBF, SVM::SIGMOID or SVM::CHI2. Default value is 1. *)
    (* * @see setGamma *)
    // CV_WRAP virtual double getGamma() const = 0;

    (* * @copybrief getGamma @see getGamma *)
    // CV_WRAP virtual void setGamma(double val) = 0;

    (* * Parameter _coef0_ of a kernel function.
      For SVM::POLY or SVM::SIGMOID. Default value is 0. *)
    (* * @see setCoef0 *)
    // CV_WRAP virtual double getCoef0() const = 0;

    (* * @copybrief getCoef0 @see getCoef0 *)
    // CV_WRAP virtual void setCoef0(double val) = 0;

    (* * Parameter _degree_ of a kernel function.
      For SVM::POLY. Default value is 0. *)
    (* * @see setDegree *)
    // CV_WRAP virtual double getDegree() const = 0;

    (* * @copybrief getDegree @see getDegree *)
    // CV_WRAP virtual void setDegree(double val) = 0;

    (* * Parameter _C_ of a %SVM optimization problem.
      For SVM::C_SVC, SVM::EPS_SVR or SVM::NU_SVR. Default value is 0. *)
    (* * @see setC *)
    // CV_WRAP virtual double getC() const = 0;

    (* * @copybrief getC @see getC *)
    // CV_WRAP virtual void setC(double val) = 0;

    (* * Parameter \f$\nu\f$ of a %SVM optimization problem.
      For SVM::NU_SVC, SVM::ONE_CLASS or SVM::NU_SVR. Default value is 0. *)
    (* * @see setNu *)
    // CV_WRAP virtual double getNu() const = 0;

    (* * @copybrief getNu @see getNu *)
    // CV_WRAP virtual void setNu(double val) = 0;

    (* * Parameter \f$\epsilon\f$ of a %SVM optimization problem.
      For SVM::EPS_SVR. Default value is 0. *)
    (* * @see setP *)
    // CV_WRAP virtual double getP() const = 0;

    (* * @copybrief getP @see getP *)
    // CV_WRAP virtual void setP(double val) = 0;

    (* * Optional weights in the SVM::C_SVC problem, assigned to particular classes.
      They are multiplied by _C_ so the parameter _C_ of class _i_ becomes `classWeights(i) * C`. Thus
      these weights affect the misclassification penalty for different classes. The larger weight,
      the larger penalty on misclassification of data from the corresponding class. Default value is
      empty Mat. *)
    (* * @see setClassWeights *)
    // CV_WRAP virtual cv::Mat getClassWeights() const = 0;

    (* * @copybrief getClassWeights @see getClassWeights *)
    // CV_WRAP virtual void setClassWeights(const cv::Mat &val) = 0;

    (* * Termination criteria of the iterative %SVM training procedure which solves a partial
      case of constrained quadratic optimization problem.
      You can specify tolerance and/or the maximum number of iterations. Default value is
      `TermCriteria( TermCriteria::MAX_ITER + TermCriteria::EPS, 1000, FLT_EPSILON )`; *)
    (* * @see setTermCriteria *)
    // CV_WRAP virtual cv::TermCriteria getTermCriteria() const = 0;

    (* * @copybrief getTermCriteria @see getTermCriteria *)
    // CV_WRAP virtual void setTermCriteria(const cv::TermCriteria &val) = 0;
    procedure setTermCriteria(const val: TTermCriteria); {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * Type of a %SVM kernel.
      See SVM::KernelTypes. Default value is SVM::RBF. *)
    // CV_WRAP virtual int getKernelType() const = 0;

    (* * Initialize with one of predefined kernels.
      See SVM::KernelTypes. *)
    // CV_WRAP virtual void setKernel(int kernelType) = 0;
    procedure setKernel(kernelType: Int); {$IFDEF USE_INLINE}inline; {$ENDIF}
    //
    (* * Initialize with custom kernel.
      See SVM::Kernel class for implementation details *)
    // virtual void setCustomKernel(const Ptr<Kernel> &_kernel) = 0;

  public const
    // ! %SVM type
    // SVMTypes
    (* * C-Support Vector Classification. n-class classification (n \f$\geq\f$ 2), allows
      imperfect separation of classes with penalty multiplier C for outliers. *)
    C_SVC = 100;
    (* * \f$\nu\f$-Support Vector Classification. n-class classification with possible
      imperfect separation. Parameter \f$\nu\f$ (in the range 0..1, the larger the value, the smoother
      the decision boundary) is used instead of C. *)
    NU_SVC = 101;
    (* * Distribution Estimation (One-class %SVM). All the training data are from
      the same class, %SVM builds a boundary that separates the class from the rest of the feature
      space. *)
    ONE_CLASS = 102;
    (* * \f$\epsilon\f$-Support Vector Regression. The distance between feature vectors
      from the training set and the fitting hyper-plane must be less than p. For outliers the
      penalty multiplier C is used. *)
    EPS_SVR = 103;
    (* * \f$\nu\f$-Support Vector Regression. \f$\nu\f$ is used instead of p.
      See @cite LibSVM for details. *)
    NU_SVR = 104;

    (* * @brief %SVM kernel type

      A comparison of different kernels on the following 2D test case with four classes. Four
      SVM::C_SVC SVMs have been trained (one against rest) with auto_train. Evaluation on three
      different kernels (SVM::CHI2, SVM::INTER, SVM::RBF). The color depicts the class with max score.
      Bright means max-score \> 0, dark means max-score \< 0.
      ![image](pics/SVM_Comparison.png)
    *)
    // KernelTypes
    (* * Returned by SVM::getKernelType in case when custom kernel has been set *)
    CUSTOM = -1;
    (* * Linear kernel. No mapping is done, linear discrimination (or regression) is
      done in the original feature space. It is the fastest option. \f$K(x_i, x_j) = x_i^T x_j\f$. *)
    LINEAR = 0;
    (* * Polynomial kernel:
      \f$K(x_i, x_j) = (\gamma x_i^T x_j + coef0)^{degree}, \gamma > 0\f$. *)
    POLY = 1;
    (* * Radial basis function (RBF), a good choice in most cases.
      \f$K(x_i, x_j) = e^{-\gamma ||x_i - x_j||^2}, \gamma > 0\f$. *)
    RBF = 2;
    (* * Sigmoid kernel: \f$K(x_i, x_j) = \tanh(\gamma x_i^T x_j + coef0)\f$. *)
    SIGMOID = 3;
    (* * Exponential Chi2 kernel, similar to the RBF kernel:
      \f$K(x_i, x_j) = e^{-\gamma \chi^2(x_i,x_j)}, \chi^2(x_i,x_j) = (x_i-x_j)^2/(x_i+x_j), \gamma > 0\f$. *)
    CHI2 = 4;
    (* * Histogram intersection kernel. A fast kernel. \f$K(x_i, x_j) = min(x_i,x_j)\f$. *)
    INTER = 5;

    // ! %SVM params type
    // enum ParamTypes {
    c      = 0;
    gamma  = 1;
    p      = 2;
    NU     = 3;
    COEF   = 4;
    DEGREE = 5;
  public
    (* * @brief Trains an %SVM with optimal parameters.

      @param data the training data that can be constructed using TrainData::create or
      TrainData::loadFromCSV.
      @param kFold Cross-validation parameter. The training set is divided into kFold subsets. One
      subset is used to test the model, the others form the train set. So, the %SVM algorithm is
      executed kFold times.
      @param Cgrid grid for C
      @param gammaGrid grid for gamma
      @param pGrid grid for p
      @param nuGrid grid for nu
      @param coeffGrid grid for coeff
      @param degreeGrid grid for degree
      @param balanced If true and the problem is 2-class classification then the method creates more
      balanced cross-validation subsets that is proportions between classes in subsets are close
      to such proportion in the whole train dataset.

      The method trains the %SVM model automatically by choosing the optimal parameters C, gamma, p,
      nu, coef0, degree. Parameters are considered optimal when the cross-validation
      estimate of the test set error is minimal.

      If there is no need to optimize a parameter, the corresponding grid step should be set to any
      value less than or equal to 1. For example, to avoid optimization in gamma, set `gammaGrid.step
      = 0`, `gammaGrid.minVal`, `gamma_grid.maxVal` as arbitrary numbers. In this case, the value
      `Gamma` is taken for gamma.

      And, finally, if the optimization in a parameter is required but the corresponding grid is
      unknown, you may call the function SVM::getDefaultGrid. To generate a grid, for example, for
      gamma, call `SVM::getDefaultGrid(SVM::GAMMA)`.

      This function works for the classification (SVM::C_SVC or SVM::NU_SVC) as well as for the
      regression (SVM::EPS_SVR or SVM::NU_SVR). If it is SVM::ONE_CLASS, no optimization is made and
      the usual %SVM with parameters specified in params is executed.
    *)
    // virtual bool trainAuto( const Ptr<TrainData>& data, int kFold = 10,
    // ParamGrid Cgrid = getDefaultGrid(C),
    // ParamGrid gammaGrid  = getDefaultGrid(GAMMA),
    // ParamGrid pGrid      = getDefaultGrid(P),
    // ParamGrid nuGrid     = getDefaultGrid(NU),
    // ParamGrid coeffGrid  = getDefaultGrid(COEF),
    // ParamGrid degreeGrid = getDefaultGrid(DEGREE),
    // bool balanced=false) = 0;

    (* * @brief Trains an %SVM with optimal parameters

      @param samples training samples
      @param layout See ml::SampleTypes.
      @param responses vector of responses associated with the training samples.
      @param kFold Cross-validation parameter. The training set is divided into kFold subsets. One
      subset is used to test the model, the others form the train set. So, the %SVM algorithm is
      @param Cgrid grid for C
      @param gammaGrid grid for gamma
      @param pGrid grid for p
      @param nuGrid grid for nu
      @param coeffGrid grid for coeff
      @param degreeGrid grid for degree
      @param balanced If true and the problem is 2-class classification then the method creates more
      balanced cross-validation subsets that is proportions between classes in subsets are close
      to such proportion in the whole train dataset.

      The method trains the %SVM model automatically by choosing the optimal parameters C, gamma, p,
      nu, coef0, degree. Parameters are considered optimal when the cross-validation
      estimate of the test set error is minimal.

      This function only makes use of SVM::getDefaultGrid for parameter optimization and thus only
      offers rudimentary parameter options.

      This function works for the classification (SVM::C_SVC or SVM::NU_SVC) as well as for the
      regression (SVM::EPS_SVR or SVM::NU_SVR). If it is SVM::ONE_CLASS, no optimization is made and
      the usual %SVM with parameters specified in params is executed.
    *)
    // CV_WRAP virtual bool trainAuto(InputArray samples,
    // int layout,
    // InputArray responses,
    // int kFold = 10,
    // Ptr<ParamGrid> Cgrid = SVM::getDefaultGridPtr(SVM::C),
    // Ptr<ParamGrid> gammaGrid  = SVM::getDefaultGridPtr(SVM::GAMMA),
    // Ptr<ParamGrid> pGrid      = SVM::getDefaultGridPtr(SVM::P),
    // Ptr<ParamGrid> nuGrid     = SVM::getDefaultGridPtr(SVM::NU),
    // Ptr<ParamGrid> coeffGrid  = SVM::getDefaultGridPtr(SVM::COEF),
    // Ptr<ParamGrid> degreeGrid = SVM::getDefaultGridPtr(SVM::DEGREE),
    // bool balanced=false) = 0;

    (* * @brief Retrieves all the support vectors

      The method returns all the support vectors as a floating-point matrix, where support vectors are
      stored as matrix rows.
    *)
    // CV_WRAP virtual Mat getSupportVectors() const = 0;

    (* * @brief Retrieves all the uncompressed support vectors of a linear %SVM

      The method returns all the uncompressed support vectors of a linear %SVM that the compressed
      support vector, used for prediction, was derived from. They are returned in a floating-point
      matrix, where the support vectors are stored as matrix rows.
    *)
    // CV_WRAP virtual Mat getUncompressedSupportVectors() const = 0;
    function getUncompressedSupportVectors(): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}
    (* * @brief Retrieves the decision function

      @param i the index of the decision function. If the problem solved is regression, 1-class or
      2-class classification, then there will be just one decision function and the index should
      always be 0. Otherwise, in the case of N-class classification, there will be \f$N(N-1)/2\f$
      decision functions.
      @param alpha the optional output vector for weights, corresponding to different support vectors.
      In the case of linear %SVM all the alpha's will be 1's.
      @param svidx the optional output vector of indices of support vectors within the matrix of
      support vectors (which can be retrieved by SVM::getSupportVectors). In the case of linear
      %SVM each decision function consists of a single "compressed" support vector.

      The method returns rho parameter of the decision function, a scalar subtracted from the weighted
      sum of kernel responses.
    *)
    // CV_WRAP virtual double getDecisionFunction(int i, OutputArray alpha, OutputArray svidx) const = 0;

    (* * @brief Generates a grid for %SVM parameters.

      @param param_id %SVM parameters IDs that must be one of the SVM::ParamTypes. The grid is
      generated for the parameter with this ID.

      The function generates a grid for the specified parameter of the %SVM algorithm. The grid may be
      passed to the function SVM::trainAuto.
    *)
    // static ParamGrid getDefaultGrid( int param_id );

    (* * @brief Generates a grid for %SVM parameters.

      @param param_id %SVM parameters IDs that must be one of the SVM::ParamTypes. The grid is
      generated for the parameter with this ID.

      The function generates a grid pointer for the specified parameter of the %SVM algorithm.
      The grid may be passed to the function SVM::trainAuto.
    *)
    // CV_WRAP static Ptr<ParamGrid> getDefaultGridPtr( int param_id );

    (* * Creates empty model.
      Use StatModel::train to train the model. Since %SVM has several parameters, you may want to
      find the best parameters for your problem, it can be done with SVM::trainAuto. *)
    class function Create: TPtr<TSVM>; static; {$IFDEF USE_INLINE}inline; {$ENDIF}
    // CV_WRAP static Ptr<SVM> create();

    (* * @brief Loads and creates a serialized svm from a file
      *
      * Use SVM::save to serialize and store an SVM to disk.
      * Load the SVM from this file again, by calling this function with the path to the file.
      *
      * @param filepath path to serialized svm
    *)
    // CV_WRAP static Ptr<SVM> load(const String& filepath);
    //
    // Frm SVM
    //
    function train(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray): BOOL; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function predict(const samples: TInputArray; const results: TOutputArray; flags: Int = 0): float; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
    function predict(const samples: TInputArray): float; overload; {$IFDEF USE_INLINE}inline; {$ENDIF}
  end;
{$ENDREGION 'ml.hpp'}
  //
{$REGION 'helpers'}

Type
  TMatHelper = record helper for TMat
  public
    class function Mat(rows, cols, &type: Int; const s: TScalar): TMat; overload; static; {$IFDEF USE_INLINE}inline; {$ENDIF}  // Mat(int rows, int cols, int type, const Scalar& s);
    procedure copyTo(m: TOutputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}         // void copyTo( OutputArray m ) const;
    procedure copyTo(m: TOutputArray; mask: TInputArray); overload; {$IFDEF USE_INLINE}inline; {$ENDIF}// void copyTo( OutputArray m, InputArray mask ) const;
    procedure convertTo(const m: TOutputArray; rtype: Int; alpha: double = 1; beta: double = 0); // void convertTo( OutputArray m, int rtype, double alpha=1, double beta=0 ) const;
    class operator Subtract(const m: TMat; const s: TScalar): TMatExpr; {$IFDEF USE_INLINE}inline; {$ENDIF}
    class operator Implicit(const s: TScalar): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}// Mat& operator = (const Scalar& s);
  end;

{$ENDREGION 'helpers'}
  //
{$REGION 'import'}
  // Std
{$I cpp.std.import.inc} // std::
  // OpenCV
{$I opencv.mat.import.inc}
{$I opencv.InputArray.import.inc}
{$I opencv.Scalar.import.inc}
{$I opencv.OutputArray.import.inc}
{$I opencv.InputOutputArray.import.inc}
{$I opencv.MatExpr.import.inc}
{$I opencv.MatSize.import.inc}
{$I opencv.operator.import.inc}
{$I opencv.CascadeClassifier.import.inc}
{$I opencv.VideoCapture.import.inc}
{$I opencv.rng.import.inc}
{$I opencv.CommandLineParser.import.inc}
{$I opencv.QRCodeDetector.import.inc}
{$I opencv.SVM.import.inc}
{$ENDREGION 'import'}

  //
implementation

Uses
{$IFDEF LOADSTUPID}
  WinApi.Windows,
{$ENDIF}
  System.Rtti;

{$REGION 'Std'}
{ CppString }

procedure CvStdString.assign(const p: pAnsiChar);
begin
  assign_CppString(@Self, p);
end;

class operator CvStdString.assign(var Dest: CvStdString; const [ref] Src: CvStdString);
begin
  Finalize(Dest);
  assign_CppString(pCppString(@Dest), pCppString(@Src));
end;

procedure CvStdString.erase(const _Off: UInt64);
begin
  erase_CppString(@Self, _Off);
end;

class operator CvStdString.Finalize(var Dest: CvStdString);
begin
  Destructor_CppString(@Dest);
end;

class operator CvStdString.Implicit(const s: string): CvStdString;
begin
  Result.assign(pAnsiChar(AnsiString(s)));
end;

class operator CvStdString.Implicit(const p: pAnsiChar): CvStdString;
begin
  Result.assign(p);
end;

class operator CvStdString.Implicit(const s: CvStdString): string;
var
  R: pAnsiChar;
begin
  R := c_str_CppString(@s);
  Result := string(R);
end;

class operator CvStdString.Initialize(out Dest: CvStdString);
begin
  Constructor_CppString(@Dest);
end;

function CvStdString.length: UInt64;
begin
  Result := length_CppString(@Self);
end;

function CvStdString.size: UInt64;
begin
  Result := size_CppString(@Self);
end;

{ TPtr<T> }

class operator TPtr<T>.Finalize(var Dest: TPtr<T>);
begin
  // Assert(false);
  Finalize(Dest._Ptr^);
end;

function TPtr<T>.v: pT;
begin
  Result := _Ptr;
end;

{ TStdVector<T> }

class operator Vector<T>.assign(var Dest: Vector<T>; const [ref] Src: Vector<T>);
begin
  CopyStdVector(@Dest, @Src, vt);
end;

function Vector<T>.empty: BOOL;
begin
  Result := StdEmpty(@Self, vt);
end;

class operator Vector<T>.Finalize(var Dest: Vector<T>);
begin
  DestroyStdVector(@Dest, vt);
end;

function Vector<T>.GetItems(const index: UInt64): T;
begin
  StdItem(@Self, vt, index, @Result);
end;

class operator Vector<T>.Initialize(out Dest: Vector<T>);
begin
  FillChar(Dest, SizeOf(Dest), 0);
  CreateStdVector(@Dest, vt);
end;

procedure Vector<T>.push_back(const Value: T);
begin
  StdPushBack(@Self, @Value, vt);
end;

function Vector<T>.size: { UInt64 } Int64;
begin
  Result := StdSize(@Self, vt);
end;

class function Vector<T>.vt: TVectorType;
Var
  TypeName: string;
begin
  if TypeInfo(T) = TypeInfo(TMat) then
    vt := vtMat
  else if TypeInfo(T) = TypeInfo(TRect) then
    vt := vtRect
  else if TypeInfo(T) = TypeInfo(TPoint) then
    vt := vtPoint
  else if TypeInfo(T) = TypeInfo(Vector<TPoint>) then
    vt := vtVectorPoint
  else if TypeInfo(T) = TypeInfo(TPoint2f) then
    vt := vtPoint2f
  else if TypeInfo(T) = TypeInfo(TScalar) then
    vt := vtScalar
  else if TypeInfo(T) = TypeInfo(uchar) then // vector<uchar>
    vt := vtUchar
  else if TypeInfo(T) = TypeInfo(float) then // vector<float>
    vt := vtFloat
  else if TypeInfo(T) = TypeInfo(Int) then // vector<float>
    vt := vtInt
  else
  begin
    TypeName := GetTypeName(TypeInfo(T));
    Assert(false, 'Can''t define type "' + TypeName + '"');
  end;
end;

{$ENDREGION 'std'}

function MIN(a, b: Int): Int;
begin
{$IFDEF UseSystemMath}
  Result := System.Math.MIN(a, b);
{$ELSE}
  // ((a) > (b) ? (b) : (a))
  if a > b then
    Result := b
  else
    Result := a;
{$ENDIF}
end;

function MAX(a, b: Int): Int;
begin
{$IFDEF UseSystemMath}
  Result := System.Math.MAX(a, b);
{$ELSE}
  // ((a) < (b) ? (b) : (a))
  if a < b then
    Result := b
  else
    Result := a;
{$ENDIF}
end;

function cvRound(Value: double): Int;
begin
  Result := Round(Value);
end;

procedure circle(img: TInputOutputArray; center: TPoint; radius: Int; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8; shift: Int = 0);
begin
  _circle(img, center, radius, color, thickness, Int(lineType), shift);
end;

procedure rectangle(const img: TInputOutputArray; const pt1, pt2: TPoint; const color: TScalar; const thickness: Int = 1; const lineType: LineTypes = LINE_8; const shift: Int = 0);
begin
  _rectangle(img, pt1, pt2, color, thickness, Int(lineType), shift);
end;

procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const color: TScalar; const lineType: LineTypes { = LINE_8 }; const shift: Int { = 0 };
  const offset: TPoint); overload;
begin
  _fillPoly(img, pts, npts, ncontours, color, Int(lineType), shift, UInt64(offset));
end;

procedure fillPoly(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const color: TScalar; const lineType: LineTypes = LINE_8; const shift: Int = 0);
begin
  fillPoly(img, pts, npts, ncontours, color, lineType, shift, Point(0, 0));
end;

procedure polylines(const img: TInputOutputArray; const pts: pPoint; const npts: pInt; const ncontours: Int; const isClosed: BOOL; const color: TScalar; const thickness: Int = 1;
  const lineType: LineTypes = LINE_8; const shift: Int = 0);
begin
  _polylines(img, pts, npts, ncontours, isClosed, color, thickness, Int(lineType), shift);
end;

procedure Scharr(const Src: TInputArray; const dst: TOutputArray; const depth: Int; const dx, dy: Int; const scale: double = 1; const delta: double = 0;
  const borderType: BorderTypes = BORDER_DEFAULT);
begin
  _Scharr(Src, dst, depth, dx, dy, scale, delta, Int(borderType));
end;

// procedure _drawContours(image: TInputOutputArray; contours: TInputArrayOfArrays; contourIdx: Int; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
// hierarchy: TInputArray { = noArray() }; maxLevel: Int { = INT_MAX }; offset: TPoint { = Point() } );
procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int = 1; const lineType: LineTypes = LINE_8);
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType, TInputArray.noArray);
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int; const lineType: LineTypes;
  const hierarchy: TInputArray);
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType, hierarchy, INT_MAX);
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int; const lineType: LineTypes;
  const hierarchy: TInputArray; const maxLevel: Int);
begin
  drawContours(image, contours, contourIdx, color, thickness, lineType, hierarchy, maxLevel, Point(0, 0));
end;

procedure drawContours(const image: TInputOutputArray; const contours: TInputArrayOfArrays; const contourIdx: Int; const color: TScalar; const thickness: Int; const lineType: LineTypes;
  const hierarchy: TInputArray; const maxLevel: Int; const offset: TPoint);
begin
  _drawContours(image, contours, contourIdx, color, thickness, lineType, hierarchy, maxLevel, UInt64(offset));
end;

function imwrite(const filename: CppString; const img: TInputArray): BOOL;
begin
  Var
    a: Vector<Int>;
  Result := imwrite(filename, img, a);
end;

function phaseCorrelate(src1: TInputArray; src2: TInputArray; window: TInputArray { = noArray() }; Var response: double { = 0 } ): TPoint2d;
begin
  Result := phaseCorrelate(src1, src2, window, @response);
end;

function phaseCorrelate(src1: TInputArray; src2: TInputArray; window: TInputArray { = noArray() } ): TPoint2d;
begin
  Result := phaseCorrelate(src1, src2, window, nil);
end;

function phaseCorrelate(src1: TInputArray; src2: TInputArray): TPoint2d;
begin
  Result := phaseCorrelate(src1, src2, TInputArray.noArray);
end;

procedure createHanningWindow(const dst: TOutputArray; const winSize: TSize; &type: Int);
begin
  createHanningWindow(dst, UInt64(winSize), &type);
end;

procedure arrowedLine(const img: TInputOutputArray; const pt1: TPoint; const pt2: TPoint; const color: TScalar; const thickness: Int = 1; const line_type: LineTypes = LineTypes(8);
  const shift: Int = 0; const tipLength: double = 0.1);
begin
  _arrowedLine(img, UInt64(pt1), UInt64(pt2), color, thickness, Int(line_type), shift, tipLength);
end;

procedure line(img: TInputOutputArray; pt1: TPoint; pt2: TPoint; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8; shift: Int = 0);
begin
  _line(img, UInt64(pt1), UInt64(pt2), color, thickness, Int(lineType), shift);
end;

procedure drawMarker(const img: TInputOutputArray; const position: TPoint; const color: TScalar; const markerType: MarkerTypes = MARKER_CROSS; const markerSize: Int = 20; const thickness: Int = 1;
  const line_type: LineTypes = LineTypes(8));
begin
  _drawMarker(img, UInt64(position), color, Int(markerType), markerSize, thickness, Int(line_type));
end;

procedure ellipse(const img: TInputOutputArray; const center: TPoint; const axes: TSize; angle, startAngle, endAngle: double; const color: TScalar; thickness: Int = 1; lineType: LineTypes = LINE_8;
  shift: Int = 0);
begin
  _ellipse(img, UInt64(center), UInt64(axes), angle, startAngle, endAngle, color, thickness, Int(lineType), shift);
end;

// function noArray(): TInputOutputArray;
// begin
// Result := TInputOutputArray.noArray;
// end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT }; const borderValue: TScalar { = morphologyDefaultBorderValue() } );
begin
  _dilate(Src, dst, kernel, UInt64(anchor), iterations, Int(borderType), borderValue);
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 };
  const borderType: BorderTypes { = BORDER_CONSTANT } );
begin
  dilate(Src, dst, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) }; const iterations: Int { = 1 } );
begin
  dilate(Src, dst, kernel, anchor, iterations, BORDER_CONSTANT);
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) } );
begin
  dilate(Src, dst, kernel, anchor, 1);
end;

procedure dilate(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray);
begin
  dilate(Src, dst, kernel, Point(-1, -1));
end;

function morphologyDefaultBorderValue(): TScalar;
begin
  Result := TScalar.all(DBL_MAX);
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint; const iterations: Int; const borderType: BorderTypes;
  const borderValue: TScalar { = morphologyDefaultBorderValue() } );
begin
  _erode(Src, dst, kernel, UInt64(anchor), iterations, Int(borderType), borderValue);
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint; const iterations: Int; const borderType: BorderTypes { = BORDER_CONSTANT } );
begin
  erode(Src, dst, kernel, anchor, iterations, borderType, morphologyDefaultBorderValue());
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint; const iterations: Int { = 1 } );
begin
  erode(Src, dst, kernel, anchor, iterations, BORDER_CONSTANT);
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray; const anchor: TPoint { = Point(-1,-1) } );
begin
  erode(Src, dst, kernel, anchor, 1);
end;

procedure erode(const Src: TInputArray; const dst: TOutputArray; const kernel: TInputArray);
begin
  erode(Src, dst, kernel, Point(-1, -1));
end;

function getStructuringElement(shape: MorphShapes; ksize: TSize): TMat;
begin
  Result := getStructuringElement(shape, ksize, Point(-1, -1));
end;

function getStructuringElement(shape: MorphShapes; ksize: TSize; anchor: TPoint): TMat;
begin
  Result := _getStructuringElement(Int(shape), UInt64(ksize), UInt64(anchor));
end;

procedure adaptiveThreshold(Src: TInputArray; dst: TOutputArray; maxValue: double; adaptiveMethod: AdaptiveThresholdTypes; thresholdType: ThresholdTypes; blockSize: Int; c: double);
begin
  _adaptiveThreshold(Src, dst, maxValue, Int(adaptiveMethod), Int(thresholdType), blockSize, c);
end;

procedure cvtColor(Src: TInputArray; dst: TOutputArray; code: ColorConversionCodes; dstCn: Int = 0);
begin
  _cvtColor(Src, dst, Int(code), dstCn);
end;

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
  bottomLeftOrigin: BOOL = false);
begin
  _putText(img, text, UInt64(org), fontFace, fontScale, color, thickness, lineType, bottomLeftOrigin);
end;

function getTextSize(const text: String; fontFace: Int; fontScale: double; thickness: Int; baseLine: pInt = nil): TSize;
begin
  _getTextSize(@Result, text, fontFace, fontScale, thickness, baseLine);
end;

procedure copyMakeBorder(const Src: TInputArray; Var dst: TOutputArray; top, bottom, left, right, borderType: Int); overload;
Var
  Scalar: TScalar;
begin
  copyMakeBorder(Src, dst, top, bottom, left, right, borderType, Scalar);
end;

procedure bitwise_not(Src: TInputArray; dst: TOutputArray; mask: TInputArray { = noArray() } );
begin
  _bitwise_not(Src, dst, mask);
end;

procedure bitwise_not(Src: TInputArray; dst: TOutputArray);
begin
  _bitwise_not(Src, dst, TInputArray.noArray());
end;

procedure normalize(const Src: TInputArray; const dst: TInputOutputArray; const alpha: double { = 1 }; beta: double { = 0 }; norm_type: NormTypes { = NORM_L2 }; dtype: Int { = -1 };
  const mask: TInputArray { = noArray() } );
begin
  _normalize(Src, dst, alpha, beta, Int(norm_type), dtype, mask);
end;

procedure normalize(const Src: TInputArray; const dst: TInputOutputArray; const alpha: double = 1; beta: double = 0; norm_type: NormTypes = NORM_L2; dtype: Int = -1);
begin
  normalize(Src, dst, alpha, beta, norm_type, dtype, TInputArray.noArray);
end;

{ TMat }

class operator TMat.Implicit(const m: TMatExpr): TMat;
begin
  Operator_Mat_Assign_MatExpr(@Result, @m);
end;

class operator TMat.Initialize(out Dest: TMat);
begin
  Constructor_Mat(@Dest);
end;

class operator TMat.Finalize(var Dest: TMat);
begin
  Destructor_Mat(@Dest);
end;

class operator TMat.assign(var Dest: TMat; const [ref] Src: TMat);
begin
  // Operator_Mat_Assign_Const_Mat(@Dest, @Src);
  Operator_Mat_Assign_Const_Mat(@Dest, @Src);
end;

function TMat.clone: TMat;
begin
  clone_Mat(@Self, @Result);
end;

procedure TMat.Create(size: TSize; &type: Int);
begin
  create_Mat(@Self, @size, &type);
end;

function TMat.diag(d: Int): TMat;
begin
  diag_Mat(@Self, @Result, d);
end;

function TMat.isContinuous: BOOL;
begin
  Result := isContinuous_Mat(@Self);
end;

function TMat.isSubmatrix: BOOL;
begin
  Result := isSubmatrix_Mat(@Self);
end;

class operator TMat.LogicalNot(const m: TMat): TMatExpr;
begin
  MatExpr_LogicalNot_Mat(@Result, @m);
end;

class function TMat.Mat(const size: TSize; &type: Int): TMat;
begin
  Constructor_Mat(@Result, rSize(size), &type);
end;

function TMat.Mat(const roi: TRect): TMat;
begin
  Result := TMat.Mat(Self, roi);
end;

class function TMat.Mat<T>(rows, cols: Int; Data: TArray<TArray<T>>; step: size_t): TMat;
Type
  pType = ^T;
Var
  d: TDataType<T>;
  p: pType;
begin
  p := AllocMem(rows * cols * SizeOf(T));
  try
    for Var i := 0 to cols - 1 do
      for Var j := 0 to rows - 1 do
        p[j * cols + i] := Data[j, i];
    Result := TMat.Mat(rows, cols, d.&type, p, step);
  finally
    FreeMem(p);
  end;
end;

class function TMat.Mat<T>(rows, cols: Int; Data: TArray<T>; step: size_t): TMat;
Var
  d: TDataType<T>;
begin
  Result := TMat.Mat(rows, cols, d.&type, @Data[0], step);
end;

class function TMat.Mat(rows, cols, &type: Int; Data: Pointer; step: size_t): TMat;
begin
  Constructor_Mat(@Result, rows, cols, &type, Data, step);
end;

class function TMat.Mat: TMat;
begin
  Constructor_Mat(@Result);
end;

class function TMat.Mat(const m: TMat; const roi: TRect): TMat;
begin
  Constructor_Mat(@Result, @m, @roi);
end;

class function TMat.ones(rows, cols, &type: Int): TMatExpr;
begin
  ones_Mat(@Result, rows, cols, &type);
end;

class function TMat.ones(ndims: Int; const sz: pInt; &type: Int): TMat;
begin
  ones_Mat(@Result, ndims, sz, &type);
end;

function TMat.pT<T>(const i0: Int): Pointer;
Type
  pType = ^T;
begin
  // CV_DbgAssert( y == 0 || (data && dims >= 1 && (unsigned)y < (unsigned)size.p[0]) );
{$WARNINGS OFF}
  Result := (Data + step.p[0] * i0);
{$WARNINGS ON}
end;

function TMat.pT<T>(const i0, i1: Int): Pointer;
Type
  pType = ^T;
begin
{$WARNINGS OFF}
  Result := Pointer(Data + step.p[0] * i0 + i1 * SizeOf(T));
{$WARNINGS ON}
end;

procedure TMat.release;
begin
  release_Mat(@Self);
end;

procedure TMat.st<T>(const i0, i1: Int; const v: T);
Type
  pType = ^T;
var
  p: pType;
begin
{$WARNINGS OFF}
  p := pType(Data + step.p[0] * i0);
  p[i1] := v;
{$WARNINGS ON}
end;

function TMat.step1(i: Int): size_t;
begin
  Result := step1_Mat(@Self, i);
end;

procedure TMat.addref;
begin
  addref_Mat(@Self);
end;

function TMat.at<T>(const i0, i1: Int): T;
Type
  pType = ^T;
begin
  // CV_DbgAssert(dims <= 2);
  // CV_DbgAssert(data);
  // CV_DbgAssert((unsigned)i0 < (unsigned)size.p[0]);
  // CV_DbgAssert((unsigned)(i1 * DataType<_Tp>::channels) < (unsigned)(size.p[1] * channels()));
  // CV_DbgAssert(CV_ELEM_SIZE1(traits::Depth<_Tp>::value) == elemSize1());
{$WARNINGS OFF}
  Result := pType(Data + step.p[0] * i0)[i1];
{$WARNINGS ON}
end;

function TMat.at<T>(const i0: Int): T;
Type
  pType = ^T;
begin
  // CV_DbgAssert(dims <= 2);
  // CV_DbgAssert(data);
  // CV_DbgAssert((unsigned)i0 < (unsigned)(size.p[0] * size.p[1]));
  // CV_DbgAssert(elemSize() == sizeof(_Tp));
{$WARNINGS OFF}
  if (isContinuous() or (size.p[0] = 1)) then
    Exit(pType(Data)[i0]);
  if (size.p[1] = 1) then
    Exit((pType(Data + step.p[0] * i0))[0]); // *(_Tp*)(data + step.p[0] * i0);
  Var
    i: Int := i0 div cols;
  Var
    j: Int := i0 - i * cols;
  Exit((pType(Data + step.p[0] * i0))[j]);
{$WARNINGS ON}
end;

function TMat.channels: Int;
begin
  Result := channels_Mat(@Self);
end;

function TMat.checkVector(elemChannels, depth: Int; requireContinuous: BOOL): Int;
begin
  Result := checkVector_Mat(@Self, elemChannels, depth, requireContinuous);
end;

procedure TMat.Create(rows, cols, &type: Int);
begin
  Constructor_Mat(@Self, rows, cols, &type);
end;

function TMat.depth: Int;
begin
  Result := depth_Mat(@Self);
end;

function TMat.elemSize: size_t;
begin
  Result := elemSize_Mat(@Self);
end;

function TMat.elemSize1: size_t;
begin
  Result := elemSize1_Mat(@Self);
end;

function TMat.empty: BOOL;
begin
  Result := empty_Mat(@Self);
end;

function TMat.total(startDim, endDim: Int): size_t;
begin
  Result := total_Mat(@Self, startDim, endDim);
end;

function TMat.total: size_t;
begin
  Result := total_Mat(@Self);
end;

function TMat.&type: Int;
begin
  Result := type_Mat(@Self);
end;

class function TMat.zeros(const rows, cols: Int; &type: Int): TMatExpr;
begin
  zeros_Mat(@Result, rows, cols, &type);
end;

class function TMat.zeros(const size: TSize; &type: Int): TMatExpr;
begin
  zeros_Mat(@Result, UInt64(size), &type);
end;

{ TInputArray }

class function TInputArray.InputArray(const m: TMat): TInputArray;
begin
  Constructor_InputArray(@Result, pMat(@m));
end;

class operator TInputArray.Implicit(const m: TMat): TInputArray;
begin
  Result := TInputArray.InputArray(m);
end;

function TInputArray.getMat(idx: Int): TMat;
begin
  // Result := default (TMat);
  getMat_InputArray(@Self, Result, idx);
end;

function TInputArray.getObj: Pointer;
begin
  Result := getObj_InputArray(@Self);
end;

// class operator TInputArray.Implicit(const IA: TInputArray): TMat;
// begin
// // 1//  getMat_InputArray(@IA, @Result);
// // 2//   Result := pMat(IA.Obj)^;
// // 3
// Var
// R: pMat;
// Var
// p: TMat;
// R := getMat_InputArray(@IA, @p);
// end;

class operator TInputArray.Implicit(const m: TMatExpr): TInputArray;
begin
  Constructor_InputArray(@Result, pMatExpr(@m));
end;

class operator TInputArray.Initialize(out Dest: TInputArray);
begin
  Constructor_InputArray(@Dest);
end;

class operator TInputArray.Finalize(var Dest: TInputArray);
begin
  Destructor_InputArray(@Dest);
end;

function TInputArray.isMat: BOOL;
begin
  Result := isMat_InputArray(@Self);
end;

class function TInputArray.noArray: TInputArray;
begin
  noArray_InputOutputArray(@TInputOutputArray(Result));
end;

class operator TInputArray.Implicit(const v: Vector < Vector < TPoint >> ): TInputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR_VECTOR) + TTraitsType<TPoint>.Value + Int(ACCESS_READ); // -2130444276;
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: Vector<TPoint2f>): TInputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR) + TTraitsType<TPoint2f>.Value + Int(ACCESS_READ); // -2130444276;
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TInputArray.Implicit(const v: Vector<uchar>): TInputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR) + TTraitsType<uchar>.Value + Int(ACCESS_READ); // -2130444276;
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

{ TOutputArray }

class function TOutputArrayHelper.noArray: TOutputArray;
begin
  noArray_InputOutputArray(@TInputOutputArray(Result));
end;

class function TOutputArrayHelper.OutputArray(const vec: TStdVectorMat): TOutputArray;
begin
  Constructor_OutputArray(@Result, @vec);
end;

class function TOutputArrayHelper.OutputArray(const m: TMat): TOutputArray;
begin
  Constructor_OutputArray(@Result, pMat(@m));
end;

class operator TOutputArrayHelper.Implicit(const m: TMat): TOutputArray;
begin
  Result := TOutputArray.OutputArray(m);
end;

class operator TOutputArrayHelper.Implicit(const OA: TOutputArray): TMat;
begin
  Assert(OA.isMat);
  Result := pMat(OA.getObj)^;
end;

class operator TOutputArrayHelper.Implicit(const m: TStdVectorMat): TOutputArray;
begin
  Result := TOutputArray.OutputArray(m);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<TPoint>): TOutputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR) + TTraitsType<TPoint>.Value + Int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<TPoint2f>): TOutputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR) + TTraitsType<TPoint2f>.Value + Int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<uchar>): TOutputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR) + TTraitsType<uchar>.Value + Int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

class operator TOutputArrayHelper.Implicit(const v: Vector<float>): TOutputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR) + TTraitsType<float>.Value + Int(ACCESS_WRITE);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

{ TScalar }

class function TScalar.Create(const v0, v1, v2, v3: double): TScalar;
begin
  constructor_Scalar(@Result, v0, v1, v2, v3);
end;

class function TScalar.all(const v0: double): TScalar;
begin
  Result := TScalar.Create(v0, v0, v0, v0);
end;

class function TScalar.Create(const v0: double): TScalar;
begin
  constructor_Scalar(@Result, v0);
end;

class operator TScalar.Implicit(const v0: double): TScalar;
begin
  Result := TScalar.Create(v0);
end;

class operator TScalar.Initialize(out Dest: TScalar);
begin
  constructor_Scalar(@Dest);
end;

function Scalar(const v0, v1, v2, v3: double): TScalar;
begin
  Result := TScalar.Create(v0, v1, v2, v3);
end;

{ TSize_<T> }

class operator TSize_<T>.Implicit(const m: TSize_<T>): UInt64;
begin
  Assert(SizeOf(m) = 8);
  Move(m, Result, 8);
end;

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
  Result := MatExpr_size(@Self, @Result)^;
end;

{ TMatSize }

class operator TMatSize.Implicit(const m: TMatSize): TSize;
begin
  operator_MatSize_MatSizeToSize(@m, @Result);
end;

class operator TMatSize.Implicit(const m: TMatSize): String;
begin
  With TSize(m) do
    Result := '(' + width.ToString + ',' + height.ToString + ')';
end;

{ TInputOutputArray }

class operator TInputOutputArrayHelper.Implicit(const IOA: TInputOutputArray): TMat;
begin
  Assert(IOA.isMat);
  Result := pMat(IOA.getObj)^;
end;

class operator TInputOutputArrayHelper.Implicit(const m: TMat): TInputOutputArray;
begin
  Result.InputOutputArray(m);
end;

procedure TInputOutputArrayHelper.InputOutputArray(const m: TMat);
begin
  Constructor_InputOutputArray(@Self, @m);
end;

class function TInputOutputArrayHelper.noArray: TInputOutputArray;
begin
  noArray_InputOutputArray(@Result);
end;

class operator TInputOutputArrayHelper.Implicit(const IOA: TInputOutputArray): TInputArray;
begin
  Move(IOA, Result, SizeOf(IOA));
end;

class operator TInputOutputArrayHelper.Implicit(const v: Vector<TPoint2f>): TInputOutputArray;
begin
  Result.flags := Int(FIXED_TYPE) + Int(STD_VECTOR) + TTraitsType<TPoint2f>.Value + Int(ACCESS_READ);
  Result.Obj := @v;
  Result.sz := size(0, 0);
end;

{ TPoint_<T> }

class operator TPoint_<T>.Implicit(const m: TPoint_<T>): UInt64;
begin
  Assert(SizeOf(m) = 8);
  Move(m, Result, 8);
end;

class operator TPoint_<T>.Implicit(const p: TPoint_<T>): TPoint_<Int>;
begin
  if TypeInfo(T) = TypeInfo(float) then
  begin
    Result.x := Trunc(pFloat(@p.x)^);
    Result.y := Trunc(pFloat(@p.y)^);
  end
  else if TypeInfo(T) = TypeInfo(double) then
  begin
    Result.x := Trunc(pDouble(@p.x)^);
    Result.y := Trunc(pDouble(@p.y)^);
  end
  else
    Assert(false, 'Point type must be Single(Float) or Double');
end;

class function TPoint_<T>.Point(_x, _y: T): TPoint_<T>;
begin
  Result.x := _x;
  Result.y := _y;
end;

function Point(const _x, _y: Int): TPoint;
begin
  Result := TPoint.Point(_x, _y);
end;

function CV_MAT_DEPTH(flags: Int): Int;
begin
  Result := flags and CV_MAT_DEPTH_MASK;
end;

function CV_MAKETYPE(depth, cn: Int): Int;
begin
  Result := (CV_MAT_DEPTH(depth) + (((cn) - 1) shl CV_CN_SHIFT));
end;

function CV_MAKE_TYPE(depth, cn: Int): Int;
begin
  Result := CV_MAKETYPE(depth, cn);
end;

function CV_8UC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_8U, n);
end;

function CV_8SC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_8S, n);
end;

function CV_16UC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_16U, n);
end;

function CV_16SC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_16S, n);
end;

function CV_32SC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_32S, n);
end;

function CV_32FC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_32F, n);
end;

function CV_64FC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_64F, n);
end;

function CV_16FC(n: Int): Int;
begin
  Result := CV_MAKETYPE(CV_16F, n);
end;

{ TMatHelper }

procedure TMatHelper.copyTo(m: TOutputArray);
begin
  copyTo_Mat(@Self, @m);
end;

procedure TMatHelper.convertTo(const m: TOutputArray; rtype: Int; alpha, beta: double);
begin
  convertTo_Mat(@Self, @m, rtype, alpha, beta);
end;

procedure TMatHelper.copyTo(m: TOutputArray; mask: TInputArray);
begin
  copyTo_Mat(@Self, @m, @mask);
end;

class function TMatHelper.Mat(rows, cols, &type: Int; const s: TScalar): TMat;
begin
  Constructor_Mat(@Result, rows, cols, &type, @s);
end;

class operator TMatHelper.Implicit(const s: TScalar): TMat;
begin
  Operator_Mat_Assign_Scalar(@Result, @s);
end;

class operator TMatHelper.Subtract(const m: TMat; const s: TScalar): TMatExpr;
begin
  MatExpr_Subtract_Mat_MatExpr(@Result, @m, @s);
end;

{ TCascadeClassifier }

procedure TCascadeClassifier.detectMultiScale(const image: TInputArray; const objects: TStdVectorRect; scaleFactor: double; minNeighbors, flags: Int);
begin
  detectMultiScale(image, objects, scaleFactor, minNeighbors, flags, size(0, 0));
end;

procedure TCascadeClassifier.detectMultiScale(const image: TInputArray; const objects: TStdVectorRect; scaleFactor: double; minNeighbors, flags: Int; const minSize: TSize);
begin
  detectMultiScale(image, objects, scaleFactor, minNeighbors, flags, minSize, size(0, 0));
end;

procedure TCascadeClassifier.detectMultiScale(const image: TInputArray; const objects: TStdVectorRect; scaleFactor: double; minNeighbors, flags: Int; const minSize, maxSize: TSize);
begin
  detectMultiScale_CascadeClassifier(@Self, @image, @objects, scaleFactor, minNeighbors, flags, UInt64(minSize), UInt64(maxSize));
end;

class operator TCascadeClassifier.Finalize(var Dest: TCascadeClassifier);
begin
  Destructor_CascadeClassifier(@Dest);
end;

class operator TCascadeClassifier.Initialize(out Dest: TCascadeClassifier);
begin
  Constructor_CascadeClassifier(@Dest);
end;

function TCascadeClassifier.load(const filename: CvStdString): BOOL;
begin
  Result := load_CascadeClassifier(@Self, @filename);
end;

function TCascadeClassifier.load(const filename: String): BOOL;
begin
  Result := load_CascadeClassifier(@Self, @CvStdString(filename));
end;

{ TVideoCapture }

class operator TVideoCapture.assign(var Dest: TVideoCapture; const [ref] Src: TVideoCapture);
begin
  Move(Src, Dest, SizeOf(Dest));
  if Src.isOpened then
    FillChar((@Src.Dummy)^, SizeOf(Dest.Dummy), 0);
end;

class operator TVideoCapture.Finalize(var Dest: TVideoCapture);
begin
  destructor_VideoCapture(@Dest);
end;

class operator TVideoCapture.Implicit(const filename: string): TVideoCapture;
begin
  Result.open(filename);
end;

class operator TVideoCapture.GreaterThan(const VideoCapture: TVideoCapture; var frame: TMat): BOOL;
begin
  Result := VideoCapture.read(frame);
end;

class operator TVideoCapture.Implicit(const index: Int): TVideoCapture;
begin
  Result.open(Index);
end;

class operator TVideoCapture.Initialize(out Dest: TVideoCapture);
begin
  constructor_VideoCapture(@Dest);
end;

function TVideoCapture.isOpened: BOOL;
begin
  Result := isOpened_VideoCapture(@Self);
end;

function TVideoCapture.open(const filename: String; const apiPreference: VideoCaptureAPIs; const params: Vector<Int>): BOOL;
begin
  Result := open_VideoCapture(@Self, filename, Int(apiPreference), params);
end;

function TVideoCapture.open(const filename: String; const apiPreference: VideoCaptureAPIs): BOOL;
begin
  Result := open_VideoCapture(@Self, filename, Int(apiPreference));
end;

function TVideoCapture.open(const index: Int; const apiPreference: VideoCaptureAPIs): BOOL;
begin
  Result := open_VideoCapture(@Self, index, Int(apiPreference));
end;

function TVideoCapture.read(const image: TOutputArray): BOOL;
begin
  Result := read_VideoCapture(@Self, @image);
end;

{ TRNG }

class operator TRNG.Implicit(const p: TRNG): unsigned;
begin
  Result := operator_RNG_ToUnsignedInt(@p);
end;

class operator TRNG.Implicit(const u: UInt64): TRNG;
begin
  Result := TRNG.RNG(u);
end;

class operator TRNG.Initialize(out Dest: TRNG);
begin
  constructor_RNG(@Dest);
end;

class function TRNG.RNG(state: UInt64): TRNG;
begin
  constructor_RNG(@Result, state);
end;

function TRNG.UNIFORM(a, b: Int): Int;
begin
  Result := uniform_RNG(@Self, a, b);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil }; var maxLoc: TPoint { = nil };
  const mask: TInputArray { = noArray() } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, @minLoc, @maxLoc, mask);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil }; var maxLoc: TPoint { = nil } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, @minLoc, @maxLoc, TInputArray.noArray);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil }; var minLoc: TPoint { = nil } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, @minLoc, nil, TInputArray.noArray);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double; Var maxVal: double { = nil } ); overload;
begin
  minMaxLoc(Src, minVal, @maxVal, nil, nil, TInputArray.noArray);
end;

procedure minMaxLoc(const Src: TInputArray; Var minVal: double); overload;
begin
  minMaxLoc(Src, minVal, nil, nil, nil, TInputArray.noArray);
end;

procedure add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray);
begin
  add(src1, src2, dst, TInputArray.noArray);
end;

procedure add(const src1: TInputArray; const src2: TInputArray; const dst: TOutputArray; const mask: TInputArray);
begin
  add(src1, src2, dst, mask, -1);
end;

{ TCommandLineParser }

procedure TCommandLineParser.about(const message: String);
begin
  About_CommandLineParser(@Self, @(CvStdString(message)));
end;

// class operator TCommandLineParser.assign(var Dest: TCommandLineParser; const [ref] Src: TCommandLineParser);
// begin
// Finalize(Dest);
// operator_CommandLineParser_Assign_CommandLineParser(@Dest, @Src);
// end;

// class function TCommandLineParser.CommandLineParser(const keys: String): TCommandLineParser;
function TCommandLineParser.check: BOOL;
begin
  Result := check_CommandLineParser(@Self);
end;

procedure TCommandLineParser.CommandLineParser(const keys: String);
Var
  argv: ppAnsiChar;
  argc: Int;
begin
  argc := ParamCount + 1;
  argv := AllocMem(SizeOf(pAnsiChar) * argc);
  for Var i := 0 to argc - 1 do
  begin
    argv[i] := pAnsiChar(AllocMem(SizeOf(AnsiChar) * length(ParamStr(i)) + 1));
    Move(pAnsiChar(AnsiString(ParamStr(i)))^, argv[i]^, length(ParamStr(i)));
  end;
  Constructor_CommandLineParser(@Self, argc, argv, @CvStdString(keys));
  for Var i := 0 to argc - 1 do
    FreeMem(argv[i]);
  FreeMem(argv);
end;

class operator TCommandLineParser.Finalize(var Dest: TCommandLineParser);
begin
  Destructor_CommandLineParser(@Dest);
end;

function TCommandLineParser.get<T>(const name: String; space_delete: BOOL): T;
begin
  getByName(name, space_delete, TypeToTParam<T>, @Result);
end;

procedure TCommandLineParser.getByName(const name: String; const space_delete: BOOL; &type: TParam; dst: Pointer);
begin
  getByName_CommandLineParser(@Self, @CvStdString(Name), space_delete, Int(&type), dst);
end;

function TCommandLineParser.has(const name: String): BOOL;
begin
  Result := Has_CommandLineParser(@Self, @CvStdString(Name));
end;

class operator TCommandLineParser.Implicit(const keys: String): TCommandLineParser;
begin
  Result.CommandLineParser(keys);
end;

procedure TCommandLineParser.printErrors;
begin
  printErrors_CommandLineParser(@Self);
end;

procedure TCommandLineParser.printMessage;
begin
  printMessage_CommandLineParser(@Self);
end;

function TCommandLineParser.TypeToTParam<T>: TParam;
Var
  s: string;
begin
  //
  // (INTp = 0, BOOLEANp = 1, REAL = 2, STRINGp = 3, MATp = 4, MAT_VECTOR = 5, ALGORITHM = 6, FLOATp = 7, UNSIGNED_INT = 8, UInt64p = 9, UCHAR = 11, SCALARp = 12);
  s := GetTypeName(TypeInfo(T));
  if SameText('Integer', s) then
    Exit(Param_INT)
  else if SameText('LongBool', s) then
    Exit(Param_BOOLEAN)
  else if SameText('Double', s) then
    Exit(Param_REAL)
  else if SameText('CvStdString', s) then
    Exit(Param_STRING)
  else if SameText('TMat', s) then
    Exit(Param_MAT)
  else if SameText('StdVectorRect', s) then
    Exit(Param_MAT_VECTOR)
  else
    // if SameText('TAlgorithm', s) then
    // Exit(Param_ALGORITHM) else
    if SameText('Single', s) then
      Exit(Param_FLOAT)
    else if SameText('Cardinal', s) then
      Exit(Param_UNSIGNED_INT)
    else if SameText('UInt64', s) then
      Exit(Param_UInt64)
    else if SameText('Byte', s) then
      Exit(Param_UCHAR)
    else if SameText('TScalar', s) then
      Exit(Param_SCALAR)
    else if SameText('string', s) then
      Exit(Param_STRING)
    else
      Assert(false, 'TODO Supplement types');
end;

{ TQRCodeDetector }

function TQRCodeDetector.detectAndDecode(const img: TInputArray; const points, straight_qrcode: TOutputArray): CppString;
begin
  Result := detectAndDecode_QRCodeDetector(@Self, @img, @points, @straight_qrcode);
end;

function TQRCodeDetector.detectAndDecode(const img: TInputArray; const points: TOutputArray): CppString;
begin
  Result := detectAndDecode(img, points, TOutputArray.noArray);
end;

function TQRCodeDetector.decodeMulti(const img, points: TInputArray; const decoded_info: TStdVectorCppString): BOOL;
begin
  Result := decodeMulti(img, points, decoded_info, TOutputArrayOfArrays.noArray);
end;

function TQRCodeDetector.decodeMulti(const img, points: TInputArray; const decoded_info: TStdVectorCppString; const straight_qrcode: TOutputArrayOfArrays): BOOL;
begin
  Result := decodeMulti_QRCodeDetector(@Self, @img, @points, @decoded_info, @straight_qrcode);
end;

function TQRCodeDetector.detect(const img: TInputArray; const points: TOutputArray): BOOL;
begin
  Result := detect_QRCodeDetector(@Self, @img, @points);
end;

function TQRCodeDetector.detectAndDecode(const img: TInputArray): CppString;
begin
  Result := detectAndDecode(img, TOutputArray.noArray);
end;

function TQRCodeDetector.detectAndDecodeMulti(const img: TInputArray; const decoded_info: TStdVectorCppString; const points: TOutputArray; const straight_qrcode: TOutputArrayOfArrays): BOOL;
begin
  Result := DetectAndDecodeMulti_QRCodeDetector(@Self, @img, @decoded_info, @points, @straight_qrcode);
end;

function TQRCodeDetector.detectAndDecodeMulti(const img: TInputArray; const decoded_info: TStdVectorCppString; const points: TOutputArray): BOOL;
begin
  Result := detectAndDecodeMulti(img, decoded_info, points, TOutputArrayOfArrays.noArray);
end;

function TQRCodeDetector.detectAndDecodeMulti(const img: TInputArray; const decoded_info: TStdVectorCppString): BOOL;
begin
  Result := detectAndDecodeMulti(img, decoded_info, TOutputArrayOfArrays.noArray);
end;

function TQRCodeDetector.detectMulti(const img: TInputArray; const points: TOutputArray): BOOL;
begin
  Result := detectMulti_QRCodeDetector(@Self, @img, @points);
end;

class operator TQRCodeDetector.Finalize(var Dest: TQRCodeDetector);
begin
  Constructor_QRCodeDetector(@Dest);
end;

class operator TQRCodeDetector.Initialize(out Dest: TQRCodeDetector);
begin
  Destructor_QRCodeDetector(@Dest);
end;

{ TTermCriteria }

function TTermCriteria.isValid: BOOL;
begin
  Var
    isCount: BOOL := ((&type and COUNT) <> 0) and (maxCount > 0);
  var
    isEps: BOOL := ((&type and EPS) <> 0) and (not IsNaN(epsilon));
  Result := isCount or isEps;
end;

class function TTermCriteria.TermCriteria(const &type, maxCount: Int; const epsilon: double): TTermCriteria;
begin
  Result.&type := &type;
  Result.maxCount := maxCount;
  Result.epsilon := epsilon;
end;

{$REGION 'tracking.hpp'}

procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray;
  const err: TOutputArray; const winSize: TSize { = Size(21,21) }; maxLevel: Int { = 3 } );
begin
  calcOpticalFlowPyrLK(prevImg, nextImg, prevPts, nextPts, status, err, winSize, maxLevel, TTermCriteria.TermCriteria(TTermCriteria.COUNT + TTermCriteria.EPS, 30, 0.01));
end;

procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray;
  const err: TOutputArray); overload;
begin
  calcOpticalFlowPyrLK(prevImg, nextImg, prevPts, nextPts, status, err, size(21, 21));
  // ; winSize:  TSize {= Size(21,21) }; maxLevel: Int { = 3 }
  // ; criteria: TTermCriteria { = TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01) }; flags: Int = 0; minEigThreshold: double = 1E-4
end;
{$ENDREGION 'tracking.hpp'}
//
{$REGION 'traits.hpp'}
{ TDataType<T> }

class operator TDataType<T>.Initialize(out Dest: TDataType<T>);
begin
  if TypeInfo(T) = TypeInfo(BOOL) then
    with Dest do
    begin
      generic_type := 0;
      depth := CV_8U;
      channels := 1;
      fmt := Int('u');
      &type := CV_MAKETYPE(depth, channels)
    end
  else if TypeInfo(T) = TypeInfo(uchar) then
    with Dest do
    begin
      generic_type := 0;
      depth := CV_8U;
      channels := 1;
      fmt := Int('u');
      &type := CV_MAKETYPE(depth, channels)
    end
  else
    // template<> class DataType<schar>
    // {
    // public:
    // typedef schar       value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_8S,
    // channels     = 1,
    // fmt          = (int)'c',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    // template<> class DataType<char>
    // {
    // public:
    // typedef schar       value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_8S,
    // channels     = 1,
    // fmt          = (int)'c',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    //
    // template<> class DataType<ushort>
    // {
    // public:
    // typedef ushort      value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_16U,
    // channels     = 1,
    // fmt          = (int)'w',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    //
    // template<> class DataType<short>
    // {
    // public:
    // typedef short       value_type;
    // typedef int         work_type;
    // typedef value_type  channel_type;
    // typedef value_type  vec_type;
    // enum { generic_type = 0,
    // depth        = CV_16S,
    // channels     = 1,
    // fmt          = (int)'s',
    // type         = CV_MAKETYPE(depth, channels)
    // };
    // };
    if TypeInfo(T) = TypeInfo(Int) then
      with Dest do
      begin
        generic_type := 0;
        depth := CV_32S;
        channels := 1;
        fmt := Int('i');
        &type := CV_MAKETYPE(depth, channels)
      end
    else if TypeInfo(T) = TypeInfo(float) then
      with Dest do
      begin
        generic_type := 0;
        depth := CV_32F;
        channels := 1;
        fmt := Int('f');
        &type := CV_MAKETYPE(depth, channels)
      end
    else
      // template<> class DataType<double>
      // {
      // public:
      // typedef double      value_type;
      // typedef value_type  work_type;
      // typedef value_type  channel_type;
      // typedef value_type  vec_type;
      // enum { generic_type = 0,
      // depth        = CV_64F,
      // channels     = 1,
      // fmt          = (int)'d',
      // type         = CV_MAKETYPE(depth, channels)
      // };
      // };
      //
      // template<> class DataType<float16_t>
      // {
      // public:
      // typedef float16_t   value_type;
      // typedef float       work_type;
      // typedef value_type  channel_type;
      // typedef value_type  vec_type;
      // enum { generic_type = 0,
      // depth        = CV_16F,
      // channels     = 1,
      // fmt          = (int)'h',
      // type         = CV_MAKETYPE(depth, channels)
      // };
      // };
      Assert(false, 'Define more types');
end;

{ TDepth<T> }

class function TDepth<T>.Value: Int;
var
  a: TDataType<T>;
begin
  Result := a.depth;
end;

{ TTraitsType<T> }

class function TTraitsType<T>.Value: Int;
var
  TypeName: String;
begin
  if TypeInfo(T) = TypeInfo(TPoint2f) then
    Result := CV_MAKETYPE(TDepth<float>.Value, 2)
  else if TypeInfo(T) = TypeInfo(uchar) then
  begin
    Var
      a: TDataType<uchar>;
    Result := a.&type;
  end
  else if TypeInfo(T) = TypeInfo(float) then
  begin
    Var
      a: TDataType<float>;
    Result := a.&type;
  end
  else
  begin
    TypeName := GetTypeName(TypeInfo(T));
    Assert(false, 'Define type "' + TypeName + '"');
  end;
end;

{$ENDREGION 'traits.hpp'}
{$REGION 'ml.hpp'}
{ TSVM }

class function TSVM.Create: TPtr<TSVM>;
begin
  create_SVM(Result._Ptr);
end;

class operator TSVM.Finalize(var Dest: TSVM);
Type
  TVirtualDestructor = procedure(Obj: Pointer);
begin
  TVirtualDestructor(vftable(Dest, 0))(@Dest);
end;

function TSVM.getType: Int;
Type
  TgetType = function(Obj: Pointer): Int;
begin
  Result := TgetType(vftable(Self, 14))(@Self);
end;

function TSVM.getUncompressedSupportVectors: TMat;
Type
  TgetUncompressedSupportVectors = procedure(Obj: Pointer; m: pMat);
begin
  TgetUncompressedSupportVectors(vftable(Self, $130 div SizeOf(Pointer)))(@Self, @Result);
end;

function TSVM.predict(const samples: TInputArray): float;
begin
  Result := predict(samples, TOutputArray.noArray);
end;

function TSVM.predict(const samples: TInputArray; const results: TOutputArray; flags: Int): float;
Type
  Tpredict = function(Obj: Pointer; samples: TInputArray; results: TOutputArray; flags: Int): float;
begin
  Result := Tpredict(vftable(Self, $68 div SizeOf(Pointer)))(@Self, samples, results, flags);
end;

procedure TSVM.setKernel(kernelType: Int);
Type
  TsetKernel = procedure(Obj: Pointer; kernelType: Int);
begin
  TsetKernel(vftable(Self, $108 div SizeOf(Pointer)))(@Self, kernelType);
end;

procedure TSVM.setTermCriteria(const val: TTermCriteria);
Type
  TsetTermCriteria = procedure(Obj: Pointer; val: pTermCriteria);
begin
  TsetTermCriteria(vftable(Self, $0F8 div SizeOf(Pointer)))(@Self, @val);
end;

procedure TSVM.setType(val: Int);
Type
  TsetType = procedure(Obj: Pointer; val: Int);
begin
  TsetType(vftable(Self, 15))(@Self, val);
end;

function TSVM.train(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray): BOOL;
Type
  Ttrain = function(Obj: Pointer; const samples: TInputArray; layout: Int; const responses: TInputArray): BOOL;
begin
  Result := Ttrain(vftable(Self, $50 div SizeOf(Pointer)))(@Self, samples, Int(layout), responses);
end;

class function TSVM.vftable(const s: TSVM; const index: integer): Pointer;
begin
  Result := pvftable(s._vftable)[index];
end;

{$ENDREGION 'ml.hpp'}
{$REGION 'matx.hpp'}
{ Vec3b }

function Vec3b.getItem(const index: integer): uchar;
begin
  Assert((index >= 0) and (index < 3));
  Result := _Data[index];
end;

class operator Vec3b.Implicit(const a: TArray<uchar>): Vec3b;
begin
  Assert(length(a) > 2);
  // slow
  for Var i := 0 to High(Result._Data) do
    Result._Data[i] := a[i];
end;

procedure Vec3b.setItem(const index: integer; const Value: uchar);
begin
  Assert((index >= 0) and (index < 3));
  _Data[index] := Value;
end;
{$ENDREGION 'matx.hpp'}

initialization

{$REGION 'Interface.h'}
  CV_8UC1 := CV_MAKETYPE(CV_8U, 1);
CV_8UC2 := CV_MAKETYPE(CV_8U, 2);
CV_8UC3 := CV_MAKETYPE(CV_8U, 3);
CV_8UC4 := CV_MAKETYPE(CV_8U, 4);

CV_8SC1 := CV_MAKETYPE(CV_8S, 1);
CV_8SC2 := CV_MAKETYPE(CV_8S, 2);
CV_8SC3 := CV_MAKETYPE(CV_8S, 3);
CV_8SC4 := CV_MAKETYPE(CV_8S, 4);

CV_16UC1 := CV_MAKETYPE(CV_16U, 1);
CV_16UC2 := CV_MAKETYPE(CV_16U, 2);
CV_16UC3 := CV_MAKETYPE(CV_16U, 3);
CV_16UC4 := CV_MAKETYPE(CV_16U, 4);

CV_16SC1 := CV_MAKETYPE(CV_16S, 1);
CV_16SC2 := CV_MAKETYPE(CV_16S, 2);
CV_16SC3 := CV_MAKETYPE(CV_16S, 3);
CV_16SC4 := CV_MAKETYPE(CV_16S, 4);

CV_32SC1 := CV_MAKETYPE(CV_32S, 1);
CV_32SC2 := CV_MAKETYPE(CV_32S, 2);
CV_32SC3 := CV_MAKETYPE(CV_32S, 3);
CV_32SC4 := CV_MAKETYPE(CV_32S, 4);

CV_32FC1 := CV_MAKETYPE(CV_32F, 1);
CV_32FC2 := CV_MAKETYPE(CV_32F, 2);
CV_32FC3 := CV_MAKETYPE(CV_32F, 3);
CV_32FC4 := CV_MAKETYPE(CV_32F, 4);

CV_64FC1 := CV_MAKETYPE(CV_64F, 1);
CV_64FC2 := CV_MAKETYPE(CV_64F, 2);
CV_64FC3 := CV_MAKETYPE(CV_64F, 3);
CV_64FC4 := CV_MAKETYPE(CV_64F, 4);

CV_16FC1 := CV_MAKETYPE(CV_16F, 1);
CV_16FC2 := CV_MAKETYPE(CV_16F, 2);
CV_16FC3 := CV_MAKETYPE(CV_16F, 3);
CV_16FC4 := CV_MAKETYPE(CV_16F, 4);
{$ENDREGION}

end.
