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
{$IF not defined(OPENCV_CORE_C_H)}
{$DEFINE OPENCV_CORE_C_H}
{$IF not defined(OPENCV_CORE_TYPES_H)}{$I 'core/types_c.inc'}{$IFEND}
(* @addtogroup core_c
    @{
*)

(*  **************************************************************************************\
*          Array allocation, deallocation, initialization and access to elements         *
\*************************************************************************************** *)

(* `malloc` wrapper.
   If there is no enough memory, the function
   (as well as other OpenCV functions that call cvAlloc)
   raises an error. *)
// CVAPI(void*)  cvAlloc( size_t size );
function cvAlloc(size: size_t): pointer; {$IFDEF USE_INLINE}inline; {$ENDIF}
(* `free` wrapper.
   Here and further all the memory releasing functions
   (that all call cvFree) take double pointer in order to
   to clear pointer to the data after releasing it.
   Passing pointer to NULL pointer is Ok: nothing happens in this case
*)
// CVAPI(void)   cvFree_( void* ptr );
// #define cvFree(ptr) (cvFree_(*(ptr)), *(ptr)=0)
  procedure cvFree(ptr: pointer); {$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Creates an image header but does not allocate the image data.
@param size Image width and height
@param depth Image depth (see cvCreateImage )
@param channels Number of channels (see cvCreateImage )
 *)
// CVAPI(IplImage*)  cvCreateImageHeader( CvSize size, int depth, int channels );
    function cvCreateImageHeader(size: TCvSize; depth, channels: int): pIplImage; {$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Initializes an image header that was previously allocated.
The returned IplImage\* points to the initialized header.
@param image Image header to initialize
@param size Image width and height
@param depth Image depth (see cvCreateImage )
@param channels Number of channels (see cvCreateImage )
@param origin Top-left IPL_ORIGIN_TL or bottom-left IPL_ORIGIN_BL
@param align Alignment for image rows, typically 4 or 8 bytes
 *)
// CVAPI(IplImage*) cvInitImageHeader( IplImage* image, CvSize size, int depth,
// int channels, int origin CV_DEFAULT(0),
// int align CV_DEFAULT(4));
      function cvInitImageHeader(var image: TIplImage; size: TCvSize; depth: int; channels: int; origin: int = 0; align: int = 4): pIplImage;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Creates an image header and allocates the image data.

This function call is equivalent to the following code:
@code
    header = cvCreateImageHeader(size, depth, channels);
    cvCreateData(header);
@endcode
@param size Image width and height
@param depth Bit depth of image elements. See IplImage for valid depths.
@param channels Number of channels per pixel. See IplImage for details. This function only creates
images with interleaved channels.
 *)
// CVAPI(IplImage*)  cvCreateImage( CvSize size, int depth, int channels );
        function cvCreateImage(size: TCvSize; depth, channels: int): pIplImage;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Deallocates an image header.

This call is an analogue of :
@code
    if(image )
    {
        iplDeallocate(*image, IPL_IMAGE_HEADER | IPL_IMAGE_ROI);
        *image = 0;
    }
@endcode
but it does not use IPL functions by default (see the CV_TURN_ON_IPL_COMPATIBILITY macro).
@param image Double pointer to the image header
 *)
// CVAPI(void)  cvReleaseImageHeader( IplImage** image );
          procedure cvReleaseImageHeader(var image: pIplImage);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Deallocates the image header and the image data.

This call is a shortened form of :
@code
    if(*image )
    {
        cvReleaseData(*image);
        cvReleaseImageHeader(image);
    }
@endcode
@param image Double pointer to the image header
*)
// CVAPI(void)  cvReleaseImage( IplImage** image );
            procedure cvReleaseImage(var image: pIplImage);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* Creates a copy of IPL image (widthStep may differ) *)
// CVAPI(IplImage*) cvCloneImage( const IplImage* image );
              function cvCloneImage(const image: pIplImage): pIplImage;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Sets the channel of interest in an IplImage.

If the ROI is set to NULL and the coi is *not* 0, the ROI is allocated. Most OpenCV functions do
*not* support the COI setting, so to process an individual image/matrix channel one may copy (via
cvCopy or cvSplit) the channel to a separate image/matrix, process it and then copy the result
back (via cvCopy or cvMerge) if needed.
@param image A pointer to the image header
@param coi The channel of interest. 0 - all channels are selected, 1 - first channel is selected,
etc. Note that the channel indices become 1-based.
 *)
// CVAPI(void)  cvSetImageCOI( IplImage* image, int coi );
                procedure cvSetImageCOI(var image: TIplImage; coi: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Returns the index of the channel of interest.

Returns the channel of interest of in an IplImage. Returned values correspond to the coi in
cvSetImageCOI.
@param image A pointer to the image header
 *)
// CVAPI(int)  cvGetImageCOI( const IplImage* image );
                  function cvGetImageCOI(const image: pIplImage): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Sets an image Region Of Interest (ROI) for a given rectangle.

If the original image ROI was NULL and the rect is not the whole image, the ROI structure is
allocated.

Most OpenCV functions support the use of ROI and treat the image rectangle as a separate image. For
example, all of the pixel coordinates are counted from the top-left (or bottom-left) corner of the
ROI, not the original image.
@param image A pointer to the image header
@param rect The ROI rectangle
 *)
// CVAPI(void)  cvSetImageROI( IplImage* image, CvRect rect );
                    procedure cvSetImageROI(var image: TIplImage; rect: TCvRect);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Resets the image ROI to include the entire image and releases the ROI structure.

This produces a similar result to the following, but in addition it releases the ROI structure. :
@code
    cvSetImageROI(image, cvRect(0, 0, image->width, image->height ));
    cvSetImageCOI(image, 0);
@endcode
@param image A pointer to the image header
 *)
// CVAPI(void)  cvResetImageROI( IplImage* image );
                      procedure cvResetImageROI(var image: TIplImage);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Returns the image ROI.

If there is no ROI set, cvRect(0,0,image-\>width,image-\>height) is returned.
@param image A pointer to the image header
 *)
// CVAPI(CvRect) cvGetImageROI( const IplImage* image );
                        function cvGetImageROI(const image: pIplImage): TCvRect;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Creates a matrix header but does not allocate the matrix data.

The function allocates a new matrix header and returns a pointer to it. The matrix data can then be
allocated using cvCreateData or set explicitly to user-allocated data via cvSetData.
@param rows Number of rows in the matrix
@param cols Number of columns in the matrix
@param type Type of the matrix elements, see cvCreateMat
 *)
// CVAPI(CvMat*)  cvCreateMatHeader( int rows, int cols, int type );
                          function cvCreateMatHeader(rows, cols, &type: int): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
                          const
                            CV_AUTOSTEP = $7FFFFFFF;

(* @brief Initializes a pre-allocated matrix header.

This function is often used to process raw data with OpenCV matrix functions. For example, the
following code computes the matrix product of two matrices, stored as ordinary arrays:
@code
    double a[] = { 1, 2, 3, 4,
                   5, 6, 7, 8,
                   9, 10, 11, 12 };

    double b[] = { 1, 5, 9,
                   2, 6, 10,
                   3, 7, 11,
                   4, 8, 12 };

    double c[9];
    CvMat Ma, Mb, Mc ;

    cvInitMatHeader(&Ma, 3, 4, CV_64FC1, a);
    cvInitMatHeader(&Mb, 4, 3, CV_64FC1, b);
    cvInitMatHeader(&Mc, 3, 3, CV_64FC1, c);

    cvMatMulAdd(&Ma, &Mb, 0, &Mc);
    // the c array now contains the product of a (3x4) and b (4x3)
@endcode
@param mat A pointer to the matrix header to be initialized
@param rows Number of rows in the matrix
@param cols Number of columns in the matrix
@param type Type of the matrix elements, see cvCreateMat .
@param data Optional: data pointer assigned to the matrix header
@param step Optional: full row width in bytes of the assigned data. By default, the minimal
possible step is used which assumes there are no gaps between subsequent rows of the matrix.
 *)
// CVAPI(CvMat*) cvInitMatHeader( CvMat* mat, int rows, int cols,
// int type, void* data CV_DEFAULT(NULL),
// int step CV_DEFAULT(CV_AUTOSTEP) );
                            function cvInitMatHeader(var mat: TCvMat; rows, cols, &type: int; data: pointer = nil; step: int = CV_AUTOSTEP): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Creates a matrix header and allocates the matrix data.

The function call is equivalent to the following code:
@code
    CvMat* mat = cvCreateMatHeader(rows, cols, type);
    cvCreateData(mat);
@endcode
@param rows Number of rows in the matrix
@param cols Number of columns in the matrix
@param type The type of the matrix elements in the form
CV_\<bit depth\>\<S|U|F\>C\<number of channels\> , where S=signed, U=unsigned, F=float. For
example, CV _ 8UC1 means the elements are 8-bit unsigned and the there is 1 channel, and CV _
32SC2 means the elements are 32-bit signed and there are 2 channels.
 *)
// CVAPI(CvMat*)  cvCreateMat( int rows, int cols, int type );
                              function cvCreateMat(rows, cols, &type: int): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Deallocates a matrix.

The function decrements the matrix data reference counter and deallocates matrix header. If the data
reference counter is 0, it also deallocates the data. :
@code
    if(*mat )
        cvDecRefData(*mat);
    cvFree((void** )mat);
@endcode
@param mat Double pointer to the matrix
 *)
// CVAPI(void)  cvReleaseMat( CvMat** mat );
                                procedure cvReleaseMat(var mat: pCvMat);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Decrements an array data reference counter.

The function decrements the data reference counter in a CvMat or CvMatND if the reference counter

pointer is not NULL. If the counter reaches zero, the data is deallocated. In the current
implementation the reference counter is not NULL only if the data was allocated using the
cvCreateData function. The counter will be NULL in other cases such as: external data was assigned
to the header using cvSetData, header is part of a larger matrix or image, or the header was
converted from an image or n-dimensional matrix header.
@param arr Pointer to an array header
 *)
// CV_INLINE  void  cvDecRefData( CvArr* arr )
// {
// if( CV_IS_MAT( arr ))
// {
// CvMat* mat = (CvMat*)arr;
// mat->data.ptr = NULL;
// if( mat->refcount != NULL && --*mat->refcount == 0 )
// cvFree( &mat->refcount );
// mat->refcount = NULL;
// }
// else if( CV_IS_MATND( arr ))
// {
// CvMatND* mat = (CvMatND*)arr;
// mat->data.ptr = NULL;
// if( mat->refcount != NULL && --*mat->refcount == 0 )
// cvFree( &mat->refcount );
// mat->refcount = NULL;
// }
// }
                                  procedure cvDecRefData(arr: pCvArr); inline;

(* @brief Increments array data reference counter.

The function increments CvMat or CvMatND data reference counter and returns the new counter value if
the reference counter pointer is not NULL, otherwise it returns zero.
@param arr Array header
 *)
// CV_INLINE  int  cvIncRefData( CvArr* arr )
// {
// int refcount = 0;
// if( CV_IS_MAT( arr ))
// {
// CvMat* mat = (CvMat*)arr;
// if( mat->refcount != NULL )
// refcount = ++*mat->refcount;
// }
// else if( CV_IS_MATND( arr ))
// {
// CvMatND* mat = (CvMatND*)arr;
// if( mat->refcount != NULL )
// refcount = ++*mat->refcount;
// }
// return refcount;
// }
                                    function cvIncRefData(arr: pCvArr): int; inline;

(* Creates an exact copy of the input matrix (except, may be, step value) *)
// CVAPI(CvMat*) cvCloneMat( const CvMat* mat );
                                      function cvCloneMat(const mat: pCvMat): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Returns matrix header corresponding to the rectangular sub-array of input image or matrix.

The function returns header, corresponding to a specified rectangle of the input array. In other

words, it allows the user to treat a rectangular part of input array as a stand-alone array. ROI is
taken into account by the function so the sub-array of ROI is actually extracted.
@param arr Input array
@param submat Pointer to the resultant sub-array header
@param rect Zero-based coordinates of the rectangle of interest
 *)
// CVAPI(CvMat*) cvGetSubRect( const CvArr* arr, CvMat* submat, CvRect rect );
 function cvGetSubRect(const arr: pCvArr; submat: pCvMat; rect: TCvRect): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
 function cvGetSubArr(const arr: pCvArr; submat: pCvMat; rect: TCvRect): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Returns array row or row span.

The function returns the header, corresponding to a specified row/row span of the input array.
cvGetRow(arr, submat, row) is a shortcut for cvGetRows(arr, submat, row, row+1).
@param arr Input array
@param submat Pointer to the resulting sub-array header
@param start_row Zero-based index of the starting row (inclusive) of the span
@param end_row Zero-based index of the ending row (exclusive) of the span
@param delta_row Index step in the row span. That is, the function extracts every delta_row -th
row from start_row and up to (but not including) end_row .
 *)
// CVAPI(CvMat*) cvGetRows( const CvArr* arr, CvMat* submat,
// int start_row, int end_row,
// int delta_row CV_DEFAULT(1));
 function cvGetRows(const arr: pCvArr; submat: pCvMat; start_row, end_row: int; delta_row: int = 1): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @overload
@param arr Input array
@param submat Pointer to the resulting sub-array header
@param row Zero-based index of the selected row
*)
// CV_INLINE  CvMat*  cvGetRow( const CvArr* arr, CvMat* submat, int row )
// {
// return cvGetRows( arr, submat, row, row + 1, 1 );
// }
 function cvGetRow(const arr: pCvArr; submat: pCvMat; row: int): pCvMat; inline;

(* @brief Returns one of more array columns.

The function returns the header, corresponding to a specified column span of the input array. That

is, no data is copied. Therefore, any modifications of the submatrix will affect the original array.
If you need to copy the columns, use cvCloneMat. cvGetCol(arr, submat, col) is a shortcut for
cvGetCols(arr, submat, col, col+1).
@param arr Input array
@param submat Pointer to the resulting sub-array header
@param start_col Zero-based index of the starting column (inclusive) of the span
@param end_col Zero-based index of the ending column (exclusive) of the span
 *)
// CVAPI(CvMat*) cvGetCols( const CvArr* arr, CvMat* submat,
// int start_col, int end_col );
 function cvGetCols(const arr: pCvArr; submat: pCvMat; start_col, end_col: int): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @overload
@param arr Input array
@param submat Pointer to the resulting sub-array header
@param col Zero-based index of the selected column
*)
// CV_INLINE  CvMat*  cvGetCol( const CvArr* arr, CvMat* submat, int col )
// {
// return cvGetCols( arr, submat, col, col + 1 );
// }
 function cvGetCol(const arr: pCvArr; submat: pCvMat; col: int): pCvMat; inline;

(* @brief Returns one of array diagonals.

The function returns the header, corresponding to a specified diagonal of the input array.
@param arr Input array
@param submat Pointer to the resulting sub-array header
@param diag Index of the array diagonal. Zero value corresponds to the main diagonal, -1
corresponds to the diagonal above the main, 1 corresponds to the diagonal below the main, and so
forth.
 *)
// CVAPI(CvMat*) cvGetDiag( const CvArr* arr, CvMat* submat,
// int diag CV_DEFAULT(0));
 function cvGetDiag(const arr: pCvArr; submat: pCvMat; diag: int = 0): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* low-level scalar <-> raw data conversion functions *)
// CVAPI(void) cvScalarToRawData( const CvScalar* scalar, void* data, int type,
// int extend_to_12 CV_DEFAULT(0) );
 procedure cvScalarToRawData(const scalar: pCvScalar; data: pointer; &type: int; extend_to_12: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// CVAPI(void) cvRawDataToScalar( const void* data, int type, CvScalar* scalar );
 procedure cvRawDataToScalar(const data: pointer; &type: int; scalar: pCvScalar);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Creates a new matrix header but does not allocate the matrix data.

The function allocates a header for a multi-dimensional dense array. The array data can further be
allocated using cvCreateData or set explicitly to user-allocated data via cvSetData.
@param dims Number of array dimensions
@param sizes Array of dimension sizes
@param type Type of array elements, see cvCreateMat
 *)
// CVAPI(CvMatND*)  cvCreateMatNDHeader( int dims, const int* sizes, int type );
 function cvCreateMatNDHeader(dims: int; const sizes: pint; &type: int): pCvMatND;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Creates the header and allocates the data for a multi-dimensional dense array.

This function call is equivalent to the following code:
@code
    CvMatND* mat = cvCreateMatNDHeader(dims, sizes, type);
    cvCreateData(mat);
@endcode
@param dims Number of array dimensions. This must not exceed CV_MAX_DIM (32 by default, but can be
changed at build time).
@param sizes Array of dimension sizes.
@param type Type of array elements, see cvCreateMat .
 *)
// CVAPI(CvMatND*)  cvCreateMatND( int dims, const int* sizes, int type );
 function cvCreateMatND(dims: int; const sizes: pint; &type: int): pCvMatND;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Initializes a pre-allocated multi-dimensional array header.

@param mat A pointer to the array header to be initialized
@param dims The number of array dimensions
@param sizes An array of dimension sizes
@param type Type of array elements, see cvCreateMat
@param data Optional data pointer assigned to the matrix header
 *)
// CVAPI(CvMatND*)  cvInitMatNDHeader( CvMatND* mat, int dims, const int* sizes,
// int type, void* data CV_DEFAULT(NULL) );
 function cvInitMatNDHeader(mat: pCvMatND; dims: int; const sizes: pint; &type: int; data: pointer = nil)
 : pCvMatND;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Deallocates a multi-dimensional array.

The function decrements the array data reference counter and releases the array header. If the
reference counter reaches 0, it also deallocates the data. :
@code
    if(*mat )
        cvDecRefData(*mat);
    cvFree((void** )mat);
@endcode
@param mat Double pointer to the array
 *)
// CV_INLINE  void  cvReleaseMatND( CvMatND** mat )
// {
// cvReleaseMat( (CvMat**)mat );
// }
 procedure cvReleaseMatND(var mat: pCvMatND); inline;

(* Creates a copy of CvMatND (except, may be, steps) *)
// CVAPI(CvMatND*) cvCloneMatND( const CvMatND* mat );
 function cvCloneMatND(const mat: pCvMatND): pCvMatND;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Creates sparse array.

The function allocates a multi-dimensional sparse array. Initially the array contain no elements,
that is PtrND and other related functions will return 0 for every index.
@param dims Number of array dimensions. In contrast to the dense matrix, the number of dimensions is
practically unlimited (up to \f$2^{16}\f$ ).
@param sizes Array of dimension sizes
@param type Type of array elements. The same as for CvMat
 *)
// CVAPI(CvSparseMat*)  cvCreateSparseMat( int dims, const int* sizes, int type );
 function cvCreateSparseMat(dims: int; const sizes: pint; &type: int): pCvSparseMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Deallocates sparse array.

The function releases the sparse array and clears the array pointer upon exit.
@param mat Double pointer to the array
 *)
// CVAPI(void)  cvReleaseSparseMat( CvSparseMat** mat );
 procedure cvReleaseSparseMat(var mat: pCvSparseMat);
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* Creates a copy of CvSparseMat (except, may be, zero items) *)
// CVAPI(CvSparseMat*) cvCloneSparseMat( const CvSparseMat* mat );
 function cvCloneSparseMat(const mat: pCvSparseMat): pCvSparseMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Initializes sparse array elements iterator.

The function initializes iterator of sparse array elements and returns pointer to the first element,
or NULL if the array is empty.
@param mat Input array
@param mat_iterator Initialized iterator
 *)
// CVAPI(CvSparseNode*) cvInitSparseMatIterator( const CvSparseMat* mat,
// CvSparseMatIterator* mat_iterator );
 function cvInitSparseMatIterator(const mat: pCvSparseMat; mat_iterator: pCvSparseMatIterator): pCvSparseNode;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Returns the next sparse matrix element

The function moves iterator to the next sparse matrix element and returns pointer to it. In the
current version there is no any particular order of the elements, because they are stored in the
hash table. The sample below demonstrates how to iterate through the sparse matrix:
@code
    // print all the non-zero sparse matrix elements and compute their sum
    double sum = 0;
    int i, dims = cvGetDims(sparsemat);
    CvSparseMatIterator it;
    CvSparseNode* node = cvInitSparseMatIterator(sparsemat, &it);

    for(; node != 0; node = cvGetNextSparseNode(&it))
    {
        int* idx = CV_NODE_IDX(array, node);
        float val = *(float* )CV_NODE_VAL(array, node);
        printf("M");
        for(i = 0; i < dims; i++ )
            printf("[%d]", idx[i]);
        printf("=%g\n", val);

        sum += val;
    }

    printf("nTotal sum = %g\n", sum);
@endcode
@param mat_iterator Sparse array iterator
 *)
// CV_INLINE CvSparseNode* cvGetNextSparseNode( CvSparseMatIterator* mat_iterator )
// {
// if( mat_iterator->node->next )
// return mat_iterator->node = mat_iterator->node->next;
// else
// {
// int idx;
// for( idx = ++mat_iterator->curidx; idx < mat_iterator->mat->hashsize; idx++ )
// {
// CvSparseNode* node = (CvSparseNode*)mat_iterator->mat->hashtable[idx];
// if( node )
// {
// mat_iterator->curidx = idx;
// return mat_iterator->node = node;
// }
// }
// return NULL;
// }
// }

 function cvGetNextSparseNode(mat_iterator: pCvSparseMatIterator): pCvSparseNode; inline;

 const
 CV_MAX_ARR = 10;

 type
(* matrix iterator: used for n-ary operations on dense arrays *)
 pCvNArrayIterator = ^_CvNArrayIterator;

 _CvNArrayIterator = record
 count: int;    (*  < number of arrays *)
 dims: int;     (*  < number of dimensions to iterate *)
 size: TCvSize; (*  < maximal common linear size: { width = size, height = 1 } *)
 ptr: array [0 .. CV_MAX_ARR - 1] of puchar; (*  < pointers to the array slices *)
 stack: array [0 .. CV_MAX_DIM - 1] of int; (*  < for internal use *)
 hdr: array [0 .. CV_MAX_ARR - 1] of pCvMatND; (*  < pointers to the headers of the
                                 matrices that are processed *)
 end;

 TCvNArrayIterator = _CvNArrayIterator;

 const
 CV_NO_DEPTH_CHECK = 1;
 CV_NO_CN_CHECK    = 2;
 CV_NO_SIZE_CHECK  = 4;

(* initializes iterator that traverses through several arrays simultaneously
   (the function together with cvNextArraySlice is used for
    N-ari element-wise operations) *)
// CVAPI(int) cvInitNArrayIterator( int count, CvArr** arrs,
// const CvArr* mask, CvMatND* stubs,
// CvNArrayIterator* array_iterator,
// int flags CV_DEFAULT(0) );
 function cvInitNArrayIterator(count: int; var arrs: pCvArr; const mask: pCvArr; stubs: pCvMatND;
 array_iterator: pCvNArrayIterator; flags: int = 0): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* returns zero value if iteration is finished, non-zero (slice length) otherwise *)
// CVAPI(int) cvNextNArraySlice( CvNArrayIterator* array_iterator );
 function cvNextNArraySlice(array_iterator: pCvNArrayIterator): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Returns type of array elements.

The function returns type of the array elements. In the case of IplImage the type is converted to
CvMat-like representation. For example, if the image has been created as:
@code
    IplImage* img = cvCreateImage(cvSize(640, 480), IPL_DEPTH_8U, 3);
@endcode
The code cvGetElemType(img) will return CV_8UC3.
@param arr Input array
 *)
// CVAPI(int) cvGetElemType( const CvArr* arr );
 function cvGetElemType(const arr: pCvArr): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Return number of array dimensions

The function returns the array dimensionality and the array of dimension sizes. In the case of
IplImage or CvMat it always returns 2 regardless of number of image/matrix rows. For example, the
following code calculates total number of array elements:
@code
    int sizes[CV_MAX_DIM];
    int i, total = 1;
    int dims = cvGetDims(arr, size);
    for(i = 0; i < dims; i++ )
        total *= sizes[i];
@endcode
@param arr Input array
@param sizes Optional output vector of the array dimension sizes. For 2d arrays the number of rows
(height) goes first, number of columns (width) next.
 *)
// CVAPI(int) cvGetDims( const CvArr* arr, int* sizes CV_DEFAULT(NULL) );
 function cvGetDims(const arr: pCvArr; sizes: pint = nil): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Returns array size along the specified dimension.

@param arr Input array
@param index Zero-based dimension index (for matrices 0 means number of rows, 1 means number of
columns; for images 0 means height, 1 means width)
 *)
// CVAPI(int) cvGetDimSize( const CvArr* arr, int index );
 function cvGetDimSize(const arr: pCvArr; index: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Return pointer to a particular array element.

The functions return a pointer to a specific array element. Number of array dimension should match
to the number of indices passed to the function except for cvPtr1D function that can be used for
sequential access to 1D, 2D or nD dense arrays.

The functions can be used for sparse arrays as well - if the requested node does not exist they
create it and set it to zero.

All these as well as other functions accessing array elements ( cvGetND , cvGetRealND , cvSet
, cvSetND , cvSetRealND ) raise an error in case if the element index is out of range.
@param arr Input array
@param idx0 The first zero-based component of the element index
@param type Optional output parameter: type of matrix elements
 *)
// CVAPI(uchar*) cvPtr1D( const CvArr* arr, int idx0, int* type CV_DEFAULT(NULL));
 function cvPtr1D(const arr: pCvArr; idx0: int; &type: pint = nil): puchar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @overload *)
// CVAPI(uchar*) cvPtr2D( const CvArr* arr, int idx0, int idx1, int* type CV_DEFAULT(NULL) );
 function cvPtr2D(const arr: pCvArr; idx0, idx1: int; &type: pint = nil): puchar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @overload *)
// CVAPI(uchar*) cvPtr3D( const CvArr* arr, int idx0, int idx1, int idx2,
// int* type CV_DEFAULT(NULL));
 function cvPtr3D(const arr: pCvArr; idx0, idx1, idx2: int; &type: pint = nil): puchar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @overload
@param arr Input array
@param idx Array of the element indices
@param type Optional output parameter: type of matrix elements
@param create_node Optional input parameter for sparse matrices. Non-zero value of the parameter
means that the requested element is created if it does not exist already.
@param precalc_hashval Optional input parameter for sparse matrices. If the pointer is not NULL,
the function does not recalculate the node hash value, but takes it from the specified location.
It is useful for speeding up pair-wise operations (TODO: provide an example)
*)
// CVAPI(uchar*) cvPtrND( const CvArr* arr, const int* idx, int* type CV_DEFAULT(NULL),
// int create_node CV_DEFAULT(1),
// unsigned* precalc_hashval CV_DEFAULT(NULL));
 function cvPtrND(const arr: pCvArr; const idx: pint; &type: pint = nil; create_node: int = 1;
 precalc_hashval: punsigned = nil): puchar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @brief Return a specific array element.

The functions return a specific array element. In the case of a sparse array the functions return 0
if the requested node does not exist (no new node is created by the functions).
@param arr Input array
@param idx0 The first zero-based component of the element index
 *)
// CVAPI(CvScalar) cvGet1D( const CvArr* arr, int idx0 );
 function cvGet1D(const arr: pCvArr; idx0: int): TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @overload *)
// CVAPI(CvScalar) cvGet2D( const CvArr* arr, int idx0, int idx1 );
 function cvGet2D(const arr: pCvArr; idx0, idx1: int): TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
(* @overload *)
// CVAPI(CvScalar) cvGet3D( const CvArr* arr, int idx0, int idx1, int idx2 );
 function cvGet3D(const arr: pCvArr; idx0, idx1, idx2: int): TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7757
// {$ELSE}
// name 'cvGet3D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload
@param arr Input array
@param idx Array of the element indices
*)
// CVAPI(CvScalar) cvGetND( const CvArr* arr, const int* idx );
 function cvGetND(const arr: pCvArr; const idx: pint): TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7776
// {$ELSE}
// name 'cvGetND'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Return a specific element of single-channel 1D, 2D, 3D or nD array.

Returns a specific element of a single-channel array. If the array has multiple channels, a runtime
error is raised. Note that Get?D functions can be used safely for both single-channel and
multiple-channel arrays though they are a bit slower.

In the case of a sparse array the functions return 0 if the requested node does not exist (no new
node is created by the functions).
@param arr Input array. Must have a single channel.
@param idx0 The first zero-based component of the element index
 *)
// CVAPI(double) cvGetReal1D( const CvArr* arr, int idx0 );
 function cvGetReal1D(const arr: pCvArr; idx0: int): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7783
// {$ELSE}
// name 'cvGetReal1D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload *)
// CVAPI(double) cvGetReal2D( const CvArr* arr, int idx0, int idx1 );
 function cvGetReal2D(const arr: pCvArr; idx0, idx1: int): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7784
// {$ELSE}
// name 'cvGetReal2D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload *)
// CVAPI(double) cvGetReal3D( const CvArr* arr, int idx0, int idx1, int idx2 );
 function cvGetReal3D(const arr: pCvArr; idx0, idx1, idx2: int): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7785
// {$ELSE}
// name 'cvGetReal3D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload
@param arr Input array. Must have a single channel.
@param idx Array of the element indices
*)
// CVAPI(double) cvGetRealND( const CvArr* arr, const int* idx );
 function cvGetRealND(const arr: pCvArr; const idx: pint): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7786
// {$ELSE}
// name 'cvGetRealND'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Change the particular array element.

The functions assign the new value to a particular array element. In the case of a sparse array the
functions create the node if it does not exist yet.
@param arr Input array
@param idx0 The first zero-based component of the element index
@param value The assigned value
 *)
// CVAPI(void) cvSet1D( CvArr* arr, int idx0, CvScalar value );
 procedure cvSet1D(arr: pCvArr; idx0: int; value: TCvScalar);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7945
// {$ELSE}
// name 'cvSet1D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload *)
// CVAPI(void) cvSet2D( CvArr* arr, int idx0, int idx1, CvScalar value );
 procedure cvSet2D(arr: pCvArr; idx0, idx1: int; value: TCvScalar);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7946
// {$ELSE}
// name 'cvSet2D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload *)
// CVAPI(void) cvSet3D( CvArr* arr, int idx0, int idx1, int idx2, CvScalar value );
 procedure cvSet3D(arr: pCvArr; idx0, idx1, idx2: int; value: TCvScalar);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7947
// {$ELSE}
// name 'cvSet3D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload
@param arr Input array
@param idx Array of the element indices
@param value The assigned value
*)
// CVAPI(void) cvSetND( CvArr* arr, const int* idx, CvScalar value );
 procedure cvSetND(arr: pCvArr; const idx: pint; value: TCvScalar);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7959
// {$ELSE}
// name 'cvSetND'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Change a specific array element.

The functions assign a new value to a specific element of a single-channel array. If the array has
multiple channels, a runtime error is raised. Note that the Set\*D function can be used safely for
both single-channel and multiple-channel arrays, though they are a bit slower.

In the case of a sparse array the functions create the node if it does not yet exist.
@param arr Input array
@param idx0 The first zero-based component of the element index
@param value The assigned value
 *)
// CVAPI(void) cvSetReal1D( CvArr* arr, int idx0, double value );
 procedure cvSetReal1D(arr: pCvArr; idx0: int; value: double);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7965
// {$ELSE}
// name 'cvSetReal1D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload *)
// CVAPI(void) cvSetReal2D( CvArr* arr, int idx0, int idx1, double value );
 procedure cvSetReal2D(arr: pCvArr; idx0, idx1: int; value: double);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7966
// {$ELSE}
// name 'cvSetReal2D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload *)
// CVAPI(void) cvSetReal3D( CvArr* arr, int idx0,
// int idx1, int idx2, double value );
 procedure cvSetReal3D(arr: pCvArr; idx0, idx1, idx2: int; value: double);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7967
// {$ELSE}
// name 'cvSetReal3D'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @overload
@param arr Input array
@param idx Array of the element indices
@param value The assigned value
*)
// CVAPI(void) cvSetRealND( CvArr* arr, const int* idx, double value );
 procedure cvSetRealND(arr: pCvArr; const idx: pint; value: double);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7968
// {$ELSE}
// name 'cvSetRealND'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* clears element of ND dense array,
   in case of sparse arrays it deletes the specified node *)
// CVAPI(void) cvClearND( CvArr* arr, const int* idx );
 procedure cvClearND(arr: pCvArr; const idx: pint);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7663
// {$ELSE}
// name 'cvClearND'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Returns matrix header for arbitrary array.

The function returns a matrix header for the input array that can be a matrix - CvMat, an image -
IplImage, or a multi-dimensional dense array - CvMatND (the third option is allowed only if
allowND != 0) . In the case of matrix the function simply returns the input pointer. In the case of
IplImage\* or CvMatND it initializes the header structure with parameters of the current image ROI
and returns &header. Because COI is not supported by CvMat, it is returned separately.

The function provides an easy way to handle both types of arrays - IplImage and CvMat using the same
code. Input array must have non-zero data pointer, otherwise the function will report an error.

@note If the input array is IplImage with planar data layout and COI set, the function returns the
pointer to the selected plane and COI == 0. This feature allows user to process IplImage structures
with planar data layout, even though OpenCV does not support such images.
@param arr Input array
@param header Pointer to CvMat structure used as a temporary buffer
@param coi Optional output parameter for storing COI
@param allowND If non-zero, the function accepts multi-dimensional dense arrays (CvMatND\* ) and
returns 2D matrix (if CvMatND has two dimensions) or 1D matrix (when CvMatND has 1 dimension or
more than 2 dimensions). The CvMatND array must be continuous.
@sa cvGetImage, cvarrToMat.
 *)
// CVAPI(CvMat*) cvGetMat( const CvArr* arr, CvMat* header,
// int* coi CV_DEFAULT(NULL),
// int allowND CV_DEFAULT(0));
 function cvGetMat(const arr: pCvArr; header: pCvMat; coi: pint = nil; allowND: int = 0): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7774
// {$ELSE}
// name 'cvGetMat'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Returns image header for arbitrary array.

The function returns the image header for the input array that can be a matrix (CvMat) or image
(IplImage). In the case of an image the function simply returns the input pointer. In the case of
CvMat it initializes an image_header structure with the parameters of the input matrix. Note that
if we transform IplImage to CvMat using cvGetMat and then transform CvMat back to IplImage using
this function, we will get different headers if the ROI is set in the original image.
@param arr Input array
@param image_header Pointer to IplImage structure used as a temporary buffer
 *)
// CVAPI(IplImage*) cvGetImage( const CvArr* arr, IplImage* image_header );
 function cvGetImage(const arr: pCvArr; image_header: pIplImage): pIplImage;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7771
// {$ELSE}
// name 'cvGetImage'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Changes the shape of a multi-dimensional array without copying the data.

The function is an advanced version of cvReshape that can work with multi-dimensional arrays as
well (though it can work with ordinary images and matrices) and change the number of dimensions.

Below are the two samples from the cvReshape description rewritten using cvReshapeMatND:
@code
    IplImage* color_img = cvCreateImage(cvSize(320,240), IPL_DEPTH_8U, 3);
    IplImage gray_img_hdr, *gray_img;
    gray_img = (IplImage* )cvReshapeMatND(color_img, sizeof(gray_img_hdr), &gray_img_hdr, 1, 0, 0);
    ...
    int size[] = { 2, 2, 2 };
    CvMatND* mat = cvCreateMatND(3, size, CV_32F);
    CvMat row_header, *row;
    row = (CvMat* )cvReshapeMatND(mat, sizeof(row_header), &row_header, 0, 1, 0);
@endcode
In C, the header file for this function includes a convenient macro cvReshapeND that does away with
the sizeof_header parameter. So, the lines containing the call to cvReshapeMatND in the examples
may be replaced as follow:
@code
    gray_img = (IplImage* )cvReshapeND(color_img, &gray_img_hdr, 1, 0, 0);
    ...
    row = (CvMat* )cvReshapeND(mat, &row_header, 0, 1, 0);
@endcode
@param arr Input array
@param sizeof_header Size of output header to distinguish between IplImage, CvMat and CvMatND
output headers
@param header Output header to be filled
@param new_cn New number of channels. new_cn = 0 means that the number of channels remains
unchanged.
@param new_dims New number of dimensions. new_dims = 0 means that the number of dimensions
remains the same.
@param new_sizes Array of new dimension sizes. Only new_dims-1 values are used, because the
total number of elements must remain the same. Thus, if new_dims = 1, new_sizes array is not
used.
 *)
// CVAPI(CvArr*) cvReshapeMatND( const CvArr* arr,
// int sizeof_header, CvArr* header,
// int new_cn, int new_dims, int* new_sizes );
 function cvReshapeMatND(const arr: pCvArr; sizeof_header: int; header: pCvArr; new_cn, new_dims: int;
 new_sizes: pint): pCvArr;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7916
// {$ELSE}
// name 'cvReshapeMatND'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvReshapeND( arr, header, new_cn, new_dims, new_sizes )   \
// cvReshapeMatND( (arr), sizeof( *(header)), (header),         \
// (new_cn), (new_dims), (new_sizes))
 function cvReshapeND(const arr: pCvArr; header: pCvArr; new_cn, new_dims: int; new_sizes: pint)
 : pCvArr; inline;

(* @brief Changes shape of matrix/image without copying data.

The function initializes the CvMat header so that it points to the same data as the original array
but has a different shape - different number of channels, different number of rows, or both.

The following example code creates one image buffer and two image headers, the first is for a
320x240x3 image and the second is for a 960x240x1 image:
@code
    IplImage* color_img = cvCreateImage(cvSize(320,240), IPL_DEPTH_8U, 3);
    CvMat gray_mat_hdr;
    IplImage gray_img_hdr, *gray_img;
    cvReshape(color_img, &gray_mat_hdr, 1);
    gray_img = cvGetImage(&gray_mat_hdr, &gray_img_hdr);
@endcode
And the next example converts a 3x3 matrix to a single 1x9 vector:
@code
    CvMat* mat = cvCreateMat(3, 3, CV_32F);
    CvMat row_header, *row;
    row = cvReshape(mat, &row_header, 0, 1);
@endcode
@param arr Input array
@param header Output header to be filled
@param new_cn New number of channels. 'new_cn = 0' means that the number of channels remains
unchanged.
@param new_rows New number of rows. 'new_rows = 0' means that the number of rows remains
unchanged unless it needs to be changed according to new_cn value.
*)
// CVAPI(CvMat*) cvReshape( const CvArr* arr, CvMat* header,
// int new_cn, int new_rows CV_DEFAULT(0) );
 function cvReshape(const arr: pCvArr; header: pCvMat; new_cn: int; new_rows: int = 0): pCvMat;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7915
// {$ELSE}
// name 'cvReshape'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Repeats source 2d array several times in both horizontal and
   vertical direction to fill destination array *)
// CVAPI(void) cvRepeat( const CvArr* src, CvArr* dst );
 procedure cvRepeat(const src: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7913
// {$ELSE}
// name 'cvRepeat'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Allocates array data

The function allocates image, matrix or multi-dimensional dense array data. Note that in the case of
matrix types OpenCV allocation functions are used. In the case of IplImage they are used unless
CV_TURN_ON_IPL_COMPATIBILITY() has been called before. In the latter case IPL functions are used
to allocate the data.
@param arr Array header
 *)
// CVAPI(void)  cvCreateData( CvArr* arr );
 procedure cvCreateData(arr: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7693
// {$ELSE}
// name 'cvCreateData'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Releases array data.

The function releases the array data. In the case of CvMat or CvMatND it simply calls
cvDecRefData(), that is the function can not deallocate external data. See also the note to
cvCreateData .
@param arr Array header
 *)
// CVAPI(void)  cvReleaseData( CvArr* arr );
 procedure cvReleaseData(arr: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7900
// {$ELSE}
// name 'cvReleaseData'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Assigns user data to the array header.

The function assigns user data to the array header. Header should be initialized before using
cvCreateMatHeader, cvCreateImageHeader, cvCreateMatNDHeader, cvInitMatHeader,
cvInitImageHeader or cvInitMatNDHeader.
@param arr Array header
@param data User data
@param step Full row length in bytes
 *)
// CVAPI(void)  cvSetData( CvArr* arr, void* data, int step );
 procedure cvSetData(arr: pCvArr; data: pointer; step: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7950
// {$ELSE}
// name 'cvSetData'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Retrieves low-level information about the array.

The function fills output variables with low-level information about the array data. All output

parameters are optional, so some of the pointers may be set to NULL. If the array is IplImage with
ROI set, the parameters of ROI are returned.

The following example shows how to get access to array elements. It computes absolute values of the
array elements :
@code
    float* data;
    int step;
    CvSize size;

    cvGetRawData(array, (uchar** )&data, &step, &size);
    step /= sizeof(data[0]);

    for(int y = 0; y < size.height; y++, data += step )
        for(int x = 0; x < size.width; x++ )
            data[x] = (float)fabs(data[x]);
@endcode
@param arr Array header
@param data Output pointer to the whole image origin or ROI origin if ROI is set
@param step Output full row length in bytes
@param roi_size Output ROI size
 *)
// CVAPI(void) cvGetRawData( const CvArr* arr, uchar** data,
// int* step CV_DEFAULT(NULL),
// CvSize* roi_size CV_DEFAULT(NULL));
 procedure cvGetRawData(const arr: pCvArr; data: puchar; step: pint = nil; roi_size: pCvSize = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7782
// {$ELSE}
// name 'cvGetRawData'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Returns size of matrix or image ROI.

The function returns number of rows (CvSize::height) and number of columns (CvSize::width) of the
input matrix or image. In the case of image the size of ROI is returned.
@param arr array header
 *)
// CVAPI(CvSize) cvGetSize( const CvArr* arr );
 function cvGetSize(const arr: pCvArr): TCvSize;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7791
// {$ELSE}
// name 'cvGetSize'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Copies one array to another.

The function copies selected elements from an input array to an output array:

\f[\texttt{dst} (I)= \texttt{src} (I)  \quad \text{if} \quad \texttt{mask} (I)  \ne 0.\f]

If any of the passed arrays is of IplImage type, then its ROI and COI fields are used. Both arrays
must have the same type, the same number of dimensions, and the same size. The function can also
copy sparse arrays (mask is not supported in this case).
@param src The source array
@param dst The destination array
@param mask Operation mask, 8-bit single channel array; specifies elements of the destination array
to be changed
 *)
// CVAPI(void)  cvCopy( const CvArr* src, CvArr* dst,
// const CvArr* mask CV_DEFAULT(NULL) );
 procedure cvCopy(const src: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7684
// {$ELSE}
// name 'cvCopy'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Sets every element of an array to a given value.

The function copies the scalar value to every selected element of the destination array:
\f[\texttt{arr} (I)= \texttt{value} \quad \text{if} \quad \texttt{mask} (I)  \ne 0\f]
If array arr is of IplImage type, then is ROI used, but COI must not be set.
@param arr The destination array
@param value Fill value
@param mask Operation mask, 8-bit single channel array; specifies elements of the destination
array to be changed
 *)
// CVAPI(void)  cvSet( CvArr* arr, CvScalar value,
// const CvArr* mask CV_DEFAULT(NULL) );
 procedure cvSet(arr: pCvArr; value: TCvScalar; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7944
// {$ELSE}
// name 'cvSet'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Clears the array.

The function clears the array. In the case of dense arrays (CvMat, CvMatND or IplImage),
cvZero(array) is equivalent to cvSet(array,cvScalarAll(0),0). In the case of sparse arrays all the
elements are removed.
@param arr Array to be cleared
 *)
// CVAPI(void)  cvSetZero( CvArr* arr );
 procedure cvSetZero(arr: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7976
// {$ELSE}
// name 'cvSetZero'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvZero  cvSetZero
 procedure cvZero(arr: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7976
// {$ELSE}
// name 'cvSetZero'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Splits a multi-channel array into the set of single-channel arrays or
   extracts particular [color] plane *)
// CVAPI(void)  cvSplit( const CvArr* src, CvArr* dst0, CvArr* dst1,
// CvArr* dst2, CvArr* dst3 );
 procedure cvSplit(const src: pCvArr; dst0: pCvArr; dst1: pCvArr; dst2: pCvArr; dst3: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7985
// {$ELSE}
// name 'cvSplit'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Merges a set of single-channel arrays into the single multi-channel array
   or inserts one particular [color] plane to the array *)
// CVAPI(void)  cvMerge( const CvArr* src0, const CvArr* src1,
// const CvArr* src2, const CvArr* src3,
// CvArr* dst );
 procedure cvMerge(const src0: pCvArr; const src1: pCvArr; const src2: pCvArr; const src3: pCvArr;
 dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7846
// {$ELSE}
// name 'cvMerge'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Copies several channels from input arrays to
   certain channels of output arrays *)
// CVAPI(void)  cvMixChannels( const CvArr** src, int src_count,
// CvArr** dst, int dst_count,
// const int* from_to, int pair_count );
 procedure cvMixChannels(const src: pCvArr; src_count: int; dst: pCvArr; dst_count: int; const from_to: pint;
 pair_count: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7852
// {$ELSE}
// name 'cvMixChannels'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Converts one array to another with optional linear transformation.

The function has several different purposes, and thus has several different names. It copies one
array to another with optional scaling, which is performed first, and/or optional type conversion,
performed after:

\f[\texttt{dst} (I) =  \texttt{scale} \texttt{src} (I) + ( \texttt{shift} _0, \texttt{shift} _1,...)\f]

All the channels of multi-channel arrays are processed independently.

The type of conversion is done with rounding and saturation, that is if the result of scaling +
conversion can not be represented exactly by a value of the destination array element type, it is
set to the nearest representable value on the real axis.
@param src Source array
@param dst Destination array
@param scale Scale factor
@param shift Value added to the scaled source array elements
 *)
// CVAPI(void)  cvConvertScale( const CvArr* src, CvArr* dst,
// double scale CV_DEFAULT(1),
// double shift CV_DEFAULT(0) );
 procedure cvConvertScale(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7680
// {$ELSE}
// name 'cvConvertScale'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvCvtScale cvConvertScale
 procedure cvCvtScale(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7680
// {$ELSE}
// name 'cvConvertScale'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvScale  cvConvertScale
 procedure cvScale(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7680
// {$ELSE}
// name 'cvConvertScale'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvConvert( src, dst )  cvConvertScale( (src), (dst), 1, 0 )
 procedure cvConvert(const src: pCvArr; dst: pCvArr); inline;

(* Performs linear transformation on every source array element,
   stores absolute value of the result:
   dst(x,y,c) = abs(scale*src(x,y,c)+shift).
   destination array must have 8u type.
   In other cases one may use cvConvertScale + cvAbsDiffS *)
// CVAPI(void)  cvConvertScaleAbs( const CvArr* src, CvArr* dst,
// double scale CV_DEFAULT(1),
// double shift CV_DEFAULT(0) );
 procedure cvConvertScaleAbs(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7681
// {$ELSE}
// name 'cvConvertScaleAbs'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvCvtScaleAbs  cvConvertScaleAbs
 procedure cvCvtScaleAbs(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7681
// {$ELSE}
// name 'cvConvertScaleAbs'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* checks termination criteria validity and
   sets eps to default_eps (if it is not set),
   max_iter to default_max_iters (if it is not set)
*)
// CVAPI(CvTermCriteria) cvCheckTermCriteria( CvTermCriteria criteria,
// double default_eps,
// int default_max_iters );
 function cvCheckTermCriteria(criteria: TCvTermCriteria; default_eps: double; default_max_iters: int)
 : TCvTermCriteria;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7658
// {$ELSE}
// name 'cvCheckTermCriteria'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  **************************************************************************************\
*                   Arithmetic, logic and comparison operations                          *
\*************************************************************************************** *)

(* dst(mask) = src1(mask) + src2(mask) *)
// CVAPI(void)  cvAdd( const CvArr* src1, const CvArr* src2, CvArr* dst,
// const CvArr* mask CV_DEFAULT(NULL));
 procedure cvAdd(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7629
// {$ELSE}
// name 'cvAdd'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(mask) = src(mask) + value *)
// CVAPI(void)  cvAddS( const CvArr* src, CvScalar value, CvArr* dst,
// const CvArr* mask CV_DEFAULT(NULL));
 procedure cvAddS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7630
// {$ELSE}
// name 'cvAddS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(mask) = src1(mask) - src2(mask) *)
// CVAPI(void)  cvSub( const CvArr* src1, const CvArr* src2, CvArr* dst,
// const CvArr* mask CV_DEFAULT(NULL));
 procedure cvSub(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7994
// {$ELSE}
// name 'cvSub'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(mask) = src(mask) - value = src(mask) + (-value) *)
// CV_INLINE  void  cvSubS( const CvArr* src, CvScalar value, CvArr* dst,
// const CvArr* mask CV_DEFAULT(NULL))
// {
// cvAddS( src, cvScalar( -value.val[0], -value.val[1], -value.val[2], -value.val[3]),
// dst, mask );
// }
 procedure cvSubS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil); inline;

(* dst(mask) = value - src(mask) *)
// CVAPI(void)  cvSubRS( const CvArr* src, CvScalar value, CvArr* dst,
// const CvArr* mask CV_DEFAULT(NULL));
 procedure cvSubRS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7995
// {$ELSE}
// name 'cvSubRS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src1(idx) * src2(idx) * scale
   (scaled element-wise multiplication of 2 arrays) *)
// CVAPI(void)  cvMul( const CvArr* src1, const CvArr* src2,
// CvArr* dst, double scale CV_DEFAULT(1) );
 procedure cvMul(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; scale: double = 1);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7856
// {$ELSE}
// name 'cvMul'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* element-wise division/inversion with scaling:
    dst(idx) = src1(idx) * scale / src2(idx)
    or dst(idx) = scale / src2(idx) if src1 == 0 *)
// CVAPI(void)  cvDiv( const CvArr* src1, const CvArr* src2,
// CvArr* dst, double scale CV_DEFAULT(1));
 procedure cvDiv(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; scale: double = 1);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7725
// {$ELSE}
// name 'cvDiv'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst = src1 * scale + src2 *)
// CVAPI(void)  cvScaleAdd( const CvArr* src1, CvScalar scale,
// const CvArr* src2, CvArr* dst );
 procedure cvScaleAdd(const src1: pCvArr; scale: TCvScalar; const src2: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7927
// {$ELSE}
// name 'cvScaleAdd'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{ TODO:cvAXPY }
// #define cvAXPY( A, real_scalar, B, C ) cvScaleAdd(A, cvRealScalar(real_scalar), B, C)

(* dst = src1 * alpha + src2 * beta + gamma *)
// CVAPI(void)  cvAddWeighted( const CvArr* src1, double alpha,
// const CvArr* src2, double beta,
// double gamma, CvArr* dst );
 procedure cvAddWeighted(const src1: pCvArr; alpha: double; const src2: pCvArr; beta: double; gamma: double;
 dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7631
// {$ELSE}
// name 'cvAddWeighted'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Calculates the dot product of two arrays in Euclidean metrics.

The function calculates and returns the Euclidean dot product of two arrays.

\f[src1  \bullet src2 =  \sum _I ( \texttt{src1} (I)  \texttt{src2} (I))\f]

In the case of multiple channel arrays, the results for all channels are accumulated. In particular,
cvDotProduct(a,a) where a is a complex vector, will return \f$||\texttt{a}||^2\f$. The function can
process multi-dimensional arrays, row by row, layer by layer, and so on.
@param src1 The first source array
@param src2 The second source array
 *)
// CVAPI(double)  cvDotProduct( const CvArr* src1, const CvArr* src2 );
 function cvDotProduct(const src1: pCvArr; const src2: pCvArr): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7726
// {$ELSE}
// name 'cvDotProduct'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src1(idx) & src2(idx) *)
// CVAPI(void) cvAnd( const CvArr* src1, const CvArr* src2,
// CvArr* dst, const CvArr* mask CV_DEFAULT(NULL));
 procedure cvAnd(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7633
// {$ELSE}
// name 'cvAnd'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src(idx) & value *)
// CVAPI(void) cvAndS( const CvArr* src, CvScalar value,
// CvArr* dst, const CvArr* mask CV_DEFAULT(NULL));
 procedure cvAndS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7634
// {$ELSE}
// name 'cvAndS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src1(idx) | src2(idx) *)
// CVAPI(void) cvOr( const CvArr* src1, const CvArr* src2,
// CvArr* dst, const CvArr* mask CV_DEFAULT(NULL));
 procedure cvOr(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7869
// {$ELSE}
// name 'cvOr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src(idx) | value *)
// CVAPI(void) cvOrS( const CvArr* src, CvScalar value,
// CvArr* dst, const CvArr* mask CV_DEFAULT(NULL));
 procedure cvOrS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7870
// {$ELSE}
// name 'cvOrS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src1(idx) ^ src2(idx) *)
// CVAPI(void) cvXor( const CvArr* src1, const CvArr* src2,
// CvArr* dst, const CvArr* mask CV_DEFAULT(NULL));
 procedure cvXor(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8011
// {$ELSE}
// name 'cvXor'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src(idx) ^ value *)
// CVAPI(void) cvXorS( const CvArr* src, CvScalar value,
// CvArr* dst, const CvArr* mask CV_DEFAULT(NULL));
 procedure cvXorS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8012
// {$ELSE}
// name 'cvXorS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = ~src(idx) *)
// CVAPI(void) cvNot( const CvArr* src, CvArr* dst );
 procedure cvNot(const src: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7867
// {$ELSE}
// name 'cvNot'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = lower(idx) <= src(idx) < upper(idx) *)
// CVAPI(void) cvInRange( const CvArr* src, const CvArr* lower,
// const CvArr* upper, CvArr* dst );
 procedure cvInRange(const src: pCvArr; const lower: pCvArr; const upper: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7816
// {$ELSE}
// name 'cvInRange'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = lower <= src(idx) < upper *)
// CVAPI(void) cvInRangeS( const CvArr* src, CvScalar lower,
// CvScalar upper, CvArr* dst );
 procedure cvInRangeS(const src: pCvArr; lower: TCvScalar; upper: TCvScalar; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7817
// {$ELSE}
// name 'cvInRangeS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_CMP_EQ = 0;
 CV_CMP_GT = 1;
 CV_CMP_GE = 2;
 CV_CMP_LT = 3;
 CV_CMP_LE = 4;
 CV_CMP_NE = 5;

(* The comparison operation support single-channel arrays only.
   Destination image should be 8uC1 or 8sC1 *)

(* dst(idx) = src1(idx) _cmp_op_ src2(idx) *)
// CVAPI(void) cvCmp( const CvArr* src1, const CvArr* src2, CvArr* dst, int cmp_op );
 procedure cvCmp(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; cmp_op: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7673
// {$ELSE}
// name 'cvCmp'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = src1(idx) _cmp_op_ value *)
// CVAPI(void) cvCmpS( const CvArr* src, double value, CvArr* dst, int cmp_op );
 procedure cvCmpS(const src: pCvArr; value: double; dst: pCvArr; cmp_op: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7674
// {$ELSE}
// name 'cvCmpS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = min(src1(idx),src2(idx)) *)
// CVAPI(void) cvMin( const CvArr* src1, const CvArr* src2, CvArr* dst );
 procedure cvMin(const src1: pCvArr; const src2: pCvArr; dst: pCvArr); overload; {$IFDEF USE_INLINE}inline;
 {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7847
// {$ELSE}
// name 'cvMin'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = max(src1(idx),src2(idx)) *)
// CVAPI(void) cvMax( const CvArr* src1, const CvArr* src2, CvArr* dst );
 procedure cvMax(const src1: pCvArr; const src2: pCvArr; dst: pCvArr); overload; {$IFDEF USE_INLINE}inline;
 {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7842
// {$ELSE}
// name 'cvMax'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = min(src(idx),value) *)
// CVAPI(void) cvMinS( const CvArr* src, double value, CvArr* dst );
 procedure cvMinS(const src: pCvArr; value: double; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7851
// {$ELSE}
// name 'cvMinS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(idx) = max(src(idx),value) *)
// CVAPI(void) cvMaxS( const CvArr* src, double value, CvArr* dst );
 procedure cvMaxS(const src: pCvArr; value: double; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7844
// {$ELSE}
// name 'cvMaxS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(x,y,c) = abs(src1(x,y,c) - src2(x,y,c)) *)
// CVAPI(void) cvAbsDiff( const CvArr* src1, const CvArr* src2, CvArr* dst );
 procedure cvAbsDiff(const src1: pCvArr; const src2: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7625
// {$ELSE}
// name 'cvAbsDiff'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* dst(x,y,c) = abs(src(x,y,c) - value(c)) *)
// CVAPI(void) cvAbsDiffS( const CvArr* src, CvArr* dst, CvScalar value );
 procedure cvAbsDiffS(const src: pCvArr; dst: pCvArr; value: TCvScalar);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7626
// {$ELSE}
// name 'cvAbsDiffS'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{ TODO:cvAbs }
// #define cvAbs( src, dst ) cvAbsDiffS( (src), (dst), cvScalarAll(0))

(*  **************************************************************************************\
*                                Math operations  *
\*************************************************************************************** *)

(* Does cartesian->polar coordinates conversion.
   Either of output components (magnitude or angle) is optional *)
// CVAPI(void)  cvCartToPolar( const CvArr* x, const CvArr* y,
// CvArr* magnitude, CvArr* angle CV_DEFAULT(NULL),
// int angle_in_degrees CV_DEFAULT(0));
 procedure cvCartToPolar(const x: pCvArr; const y: pCvArr; magnitude: pCvArr; angle: pCvArr = nil;
 angle_in_degrees: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7652
// {$ELSE}
// name 'cvCartToPolar'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Does polar->cartesian coordinates conversion.
   Either of output components (magnitude or angle) is optional.
   If magnitude is missing it is assumed to be all 1's *)
// CVAPI(void)  cvPolarToCart( const CvArr* magnitude, const CvArr* angle,
// CvArr* x, CvArr* y,
// int angle_in_degrees CV_DEFAULT(0));
 procedure cvPolarToCart(const magnitude: pCvArr; const angle: pCvArr; x: pCvArr; y: pCvArr;
 angle_in_degrees: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7874
// {$ELSE}
// name 'cvPolarToCart'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Does powering: dst(idx) = src(idx)^power *)
// CVAPI(void)  cvPow( const CvArr* src, CvArr* dst, double power );
 procedure cvPow(const src: pCvArr; dst: pCvArr; power: double);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7876
// {$ELSE}
// name 'cvPow'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Does exponention: dst(idx) = exp(src(idx)).
   Overflow is not handled yet. Underflow is handled.
   Maximal relative error is ~7e-6 for single-precision input *)
// CVAPI(void)  cvExp( const CvArr* src, CvArr* dst );
 procedure cvExp(const src: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7738
// {$ELSE}
// name 'cvExp'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates natural logarithms: dst(idx) = log(abs(src(idx))).
   Logarithm of 0 gives large negative number(~-700)
   Maximal relative error is ~3e-7 for single-precision output
*)
// CVAPI(void)  cvLog( const CvArr* src, CvArr* dst );
 procedure cvLog(const src: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7835
// {$ELSE}
// name 'cvLog'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Fast arctangent calculation *)
// CVAPI(float) cvFastArctan( float y, float x );
 function cvFastArctan(y: float; x: float): float;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7739
// {$ELSE}
// name 'cvFastArctan'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Fast cubic root calculation *)
// CVAPI(float)  cvCbrt( float value );
 function cvCbrt(value: float): float;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7653
// {$ELSE}
// name 'cvCbrt'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_CHECK_RANGE = 1;
 CV_CHECK_QUIET = 2;
(* Checks array values for NaNs, Infs or simply for too large numbers
   (if CV_CHECK_RANGE is set). If CV_CHECK_QUIET is set,
   no runtime errors is raised (function returns zero value in case of "bad" values).
   Otherwise cvError is called *)
// CVAPI(int)  cvCheckArr( const CvArr* arr, int flags CV_DEFAULT(0),
// double min_val CV_DEFAULT(0), double max_val CV_DEFAULT(0));
 function cvCheckArr(const arr: pCvArr; flags: int = 0; min_val: double = 0; max_val: double = 0): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7655
// {$ELSE}
// name 'cvCheckArr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvCheckArray cvCheckArr
 function cvCheckArray(const arr: pCvArr; flags: int = 0; min_val: double = 0; max_val: double = 0): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7655
// {$ELSE}
// name 'cvCheckArr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_RAND_UNI    = 0;
 CV_RAND_NORMAL = 1;

(* @brief Fills an array with random numbers and updates the RNG state.

The function fills the destination array with uniformly or normally distributed random numbers.
@param rng CvRNG state initialized by cvRNG
@param arr The destination array
@param dist_type Distribution type
> -   **CV_RAND_UNI** uniform distribution
> -   **CV_RAND_NORMAL** normal or Gaussian distribution
@param param1 The first parameter of the distribution. In the case of a uniform distribution it is
the inclusive lower boundary of the random numbers range. In the case of a normal distribution it
is the mean value of the random numbers.
@param param2 The second parameter of the distribution. In the case of a uniform distribution it
is the exclusive upper boundary of the random numbers range. In the case of a normal distribution
it is the standard deviation of the random numbers.
@sa randu, randn, RNG::fill.
 *)
// CVAPI(void) cvRandArr( CvRNG* rng, CvArr* arr, int dist_type,
// CvScalar param1, CvScalar param2 );
 procedure cvRandArr(rng: pCvRNG; arr: pCvArr; dist_type: int; param1: TCvScalar; param2: TCvScalar);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7889
// {$ELSE}
// name 'cvRandArr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void) cvRandShuffle( CvArr* mat, CvRNG* rng,
// double iter_factor CV_DEFAULT(1.));
 procedure cvRandShuffle(mat: pCvArr; rng: pCvRNG; iter_factor: double = 1);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7890
// {$ELSE}
// name 'cvRandShuffle'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_SORT_EVERY_ROW = 0;
 CV_SORT_EVERY_COLUMN = 1;
 CV_SORT_ASCENDING  = 0;
 CV_SORT_DESCENDING = 16;

// CVAPI(void) cvSort( const CvArr* src, CvArr* dst CV_DEFAULT(NULL),
// CvArr* idxmat CV_DEFAULT(NULL),
// int flags CV_DEFAULT(0));
 procedure cvSort(const src: pCvArr; dst: pCvArr = nil; idxmat: pCvArr = nil; flags: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7984
// {$ELSE}
// name 'cvSort'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Finds real roots of a cubic equation *)
// CVAPI(int) cvSolveCubic( const CvMat* coeffs, CvMat* roots );
 function cvSolveCubic(const coeffs: pCvMat; roots: pCvMat): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7982
// {$ELSE}
// name 'cvSolveCubic'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Finds all real and complex roots of a polynomial equation *)
// CVAPI(void) cvSolvePoly(const CvMat* coeffs, CvMat *roots2,
// int maxiter CV_DEFAULT(20), int fig CV_DEFAULT(100));
 procedure cvSolvePoly(const coeffs: pCvMat; roots2: pCvMat; maxiter: int = 20; fig: int = 100);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7983
// {$ELSE}
// name 'cvSolvePoly'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  **************************************************************************************\
*                                Matrix operations*
\*************************************************************************************** *)

(* @brief Calculates the cross product of two 3D vectors.

The function calculates the cross product of two 3D vectors:
\f[\texttt{dst} =  \texttt{src1} \times \texttt{src2}\f]
or:
\f[\begin{array}{l} \texttt{dst} _1 =  \texttt{src1} _2  \texttt{src2} _3 -  \texttt{src1} _3  \texttt{src2} _2 \\ \texttt{dst} _2 =  \texttt{src1} _3  \texttt{src2} _1 -  \texttt{src1} _1  \texttt{src2} _3 \\ \texttt{dst} _3 =  \texttt{src1} _1  \texttt{src2} _2 -  \texttt{src1} _2  \texttt{src2} _1 \end{array}\f]
@param src1 The first source vector
@param src2 The second source vector
@param dst The destination vector
 *)
// CVAPI(void)  cvCrossProduct( const CvArr* src1, const CvArr* src2, CvArr* dst );
 procedure cvCrossProduct(const src1: pCvArr; const src2: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7715
// {$ELSE}
// name 'cvCrossProduct'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{ TODO: cvMatMulAdd and cvMatMul }
(* Matrix transform: dst = A*B + C, C is optional *)
// #define cvMatMulAdd( src1, src2, src3, dst ) cvGEMM( (src1), (src2), 1., (src3), 1., (dst), 0 )
// #define cvMatMul( src1, src2, dst )  cvMatMulAdd( (src1), (src2), NULL, (dst))

 const
 CV_GEMM_A_T = 1;
 CV_GEMM_B_T = 2;
 CV_GEMM_C_T = 4;
(* Extended matrix transform:
   dst = alpha*op(A)*op(B) + beta*op(C), where op(X) is X or X^T *)
// CVAPI(void)  cvGEMM( const CvArr* src1, const CvArr* src2, double alpha,
// const CvArr* src3, double beta, CvArr* dst,
// int tABC CV_DEFAULT(0));
 procedure cvGEMM(const src1: pCvArr; const src2: pCvArr; alpha: double; const src3: pCvArr; beta: double;
 dst: pCvArr; tABC: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7754
// {$ELSE}
// name 'cvGEMM'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvMatMulAddEx cvGEMM
 procedure cvMatMulAddEx(const src1: pCvArr; const src2: pCvArr; alpha: double; const src3: pCvArr;
 beta: double; dst: pCvArr; tABC: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7754
// {$ELSE}
// name 'cvGEMM'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Transforms each element of source array and stores
   resultant vectors in destination array *)
// CVAPI(void)  cvTransform( const CvArr* src, CvArr* dst,
// const CvMat* transmat,
// const CvMat* shiftvec CV_DEFAULT(NULL));
 procedure cvTransform(const src: pCvArr; dst: pCvArr; const transmat: pCvMat; const shiftvec: pCvMat = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8001
// {$ELSE}
// name 'cvTransform'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvMatMulAddS cvTransform
 procedure cvMatMulAddS(const src: pCvArr; dst: pCvArr; const transmat: pCvMat; const shiftvec: pCvMat = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8001
// {$ELSE}
// name 'cvTransform'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Does perspective transform on every element of input array *)
// CVAPI(void)  cvPerspectiveTransform( const CvArr* src, CvArr* dst,
// const CvMat* mat );
 procedure cvPerspectiveTransform(const src: pCvArr; dst: pCvArr; const mat: pCvMat);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7871
// {$ELSE}
// name 'cvPerspectiveTransform'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates (A-delta)*(A-delta)^T (order=0) or (A-delta)^T*(A-delta) (order=1) *)
// CVAPI(void) cvMulTransposed( const CvArr* src, CvArr* dst, int order,
// const CvArr* delta CV_DEFAULT(NULL),
// double scale CV_DEFAULT(1.) );
 procedure cvMulTransposed(const src: pCvArr; dst: pCvArr; order: int; const delta: pCvArr = nil;
 scale: double = 1);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7858
// {$ELSE}
// name 'cvMulTransposed'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Transposes matrix. Square matrices can be transposed in-place *)
// CVAPI(void)  cvTranspose( const CvArr* src, CvArr* dst );
 procedure cvTranspose(const src: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8002
// {$ELSE}
// name 'cvTranspose'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvT cvTranspose
 procedure cvT(const src: pCvArr; dst: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8002
// {$ELSE}
// name 'cvTranspose'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Completes the symmetric matrix from the lower (LtoR=0) or from the upper (LtoR!=0) part *)
// CVAPI(void)  cvCompleteSymm( CvMat* matrix, int LtoR CV_DEFAULT(0) );
 procedure cvCompleteSymm(matrix: pCvMat; LtoR: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7677
// {$ELSE}
// name 'cvCompleteSymm'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Mirror array data around horizontal (flip=0),
   vertical (flip=1) or both(flip=-1) axises:
   cvFlip(src) flips images vertically and sequences horizontally (inplace) *)
// CVAPI(void)  cvFlip( const CvArr* src, CvArr* dst CV_DEFAULT(NULL),
// int flip_mode CV_DEFAULT(0));
 procedure cvFlip(const src: pCvArr; dst: pCvArr = nil; flip_mode: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7750
// {$ELSE}
// name 'cvFlip'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvMirror cvFlip
 procedure cvMirror(const src: pCvArr; dst: pCvArr = nil; flip_mode: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7750
// {$ELSE}
// name 'cvFlip'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_SVD_MODIFY_A = 1;
 CV_SVD_U_T      = 2;
 CV_SVD_V_T      = 4;

(* Performs Singular Value Decomposition of a matrix *)
// CVAPI(void)   cvSVD( CvArr* A, CvArr* W, CvArr* U CV_DEFAULT(NULL),
// CvArr* V CV_DEFAULT(NULL), int flags CV_DEFAULT(0));
 procedure cvSVD(A: pCvArr; W: pCvArr; U: pCvArr = nil; V: pCvArr = nil; flags: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7923
// {$ELSE}
// name 'cvSVD'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Performs Singular Value Back Substitution (solves A*X = B):
   flags must be the same as in cvSVD *)
// CVAPI(void)   cvSVBkSb( const CvArr* W, const CvArr* U,
// const CvArr* V, const CvArr* B,
// CvArr* X, int flags );
 procedure cvSVBkSb(const W: pCvArr; const U: pCvArr; const V: pCvArr; const B: pCvArr; x: pCvArr; flags: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7922
// {$ELSE}
// name 'cvSVBkSb'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_LU       = 0;
 CV_SVD      = 1;
 CV_SVD_SYM  = 2;
 CV_CHOLESKY = 3;
 CV_QR       = 4;
 CV_NORMAL   = 16;

(* Inverts matrix *)
// CVAPI(double)  cvInvert( const CvArr* src, CvArr* dst,
// int method CV_DEFAULT(CV_LU));
 function cvInvert(const src: pCvArr; dst: pCvArr; method: int = CV_LU): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7829
// {$ELSE}
// name 'cvInvert'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvInv cvInvert
 function cvInv(const src: pCvArr; dst: pCvArr; method: int = CV_LU): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7829
// {$ELSE}
// name 'cvInvert'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Solves linear system (src1)*(dst) = (src2)
   (returns 0 if src1 is a singular and CV_LU method is used) *)
// CVAPI(int)  cvSolve( const CvArr* src1, const CvArr* src2, CvArr* dst,
// int method CV_DEFAULT(CV_LU));
 function cvSolve(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; method: int = CV_LU): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7981
// {$ELSE}
// name 'cvSolve'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates determinant of input matrix *)
// CVAPI(double) cvDet( const CvArr* mat );
 function cvDet(const mat: pCvArr): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7722
// {$ELSE}
// name 'cvDet'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates trace of the matrix (sum of elements on the main diagonal) *)
// CVAPI(CvScalar) cvTrace( const CvArr* mat );
 function cvTrace(const mat: pCvArr): TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8000
// {$ELSE}
// name 'cvTrace'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Finds eigen values and vectors of a symmetric matrix *)
// CVAPI(void)  cvEigenVV( CvArr* mat, CvArr* evects, CvArr* evals,
// double eps CV_DEFAULT(0),
// int lowindex CV_DEFAULT(-1),
// int highindex CV_DEFAULT(-1));
 procedure cvEigenVV(mat: pCvArr; evects: pCvArr; evals: pCvArr; eps: double = 0; lowindex: int = -1;
 highindex: int = -1);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7728
// {$ELSE}
// name 'cvEigenVV'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// (* Finds selected eigen values and vectors of a symmetric matrix *)
// CVAPI(void)  cvSelectedEigenVV( CvArr* mat, CvArr* evects, CvArr* evals,
// int lowindex, int highindex );

(* Makes an identity matrix (mat_ij = i == j) *)
// CVAPI(void)  cvSetIdentity( CvArr* mat, CvScalar value CV_DEFAULT(cvRealScalar(1)) );
 procedure cvSetIdentity(mat: pCvArr; value: TCvScalar); overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7955
// {$ELSE}
// name 'cvSetIdentity'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
 procedure cvSetIdentity(mat: pCvArr); overload;

(* Fills matrix with given range of numbers *)
// CVAPI(CvArr*)  cvRange( CvArr* mat, double start, double end );
 function cvRange(mat: pCvArr; start: double; &end: double): pCvArr;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7891
// {$ELSE}
// name 'cvRange'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*   @anchor core_c_CovarFlags
@name Flags for cvCalcCovarMatrix
@see cvCalcCovarMatrix
  @{
*)

 const
(* flag for cvCalcCovarMatrix, transpose([v1-avg, v2-avg,...]) * [v1-avg,v2-avg,...] *)
 CV_COVAR_SCRAMBLED = 0;

(* flag for cvCalcCovarMatrix, [v1-avg, v2-avg,...] * transpose([v1-avg,v2-avg,...]) *)
 CV_COVAR_NORMAL = 1;

(* flag for cvCalcCovarMatrix, do not calc average (i.e. mean vector) - use the input vector instead
   (useful for calculating covariance matrix by parts) *)
 CV_COVAR_USE_AVG = 2;

(* flag for cvCalcCovarMatrix, scale the covariance matrix coefficients by number of the vectors *)
 CV_COVAR_SCALE = 4;

(* flag for cvCalcCovarMatrix, all the input vectors are stored in a single matrix, as its rows *)
 CV_COVAR_ROWS = 8;

(* flag for cvCalcCovarMatrix, all the input vectors are stored in a single matrix, as its columns *)
 CV_COVAR_COLS = 16;

(* @} *)

(* Calculates covariation matrix for a set of vectors
@see @ref core_c_CovarFlags "flags"
*)
// CVAPI(void)  cvCalcCovarMatrix( const CvArr** vects, int count,
// CvArr* cov_mat, CvArr* avg, int flags );
 procedure cvCalcCovarMatrix(const vects: pCvArr; count: int; cov_mat: pCvArr; avg: pCvArr; flags: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7647
// {$ELSE}
// name 'cvCalcCovarMatrix'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_PCA_DATA_AS_ROW = 0;
 CV_PCA_DATA_AS_COL = 1;
 CV_PCA_USE_AVG     = 2;

// CVAPI(void)  cvCalcPCA( const CvArr* data, CvArr* mean,
// CvArr* eigenvals, CvArr* eigenvects, int flags );
 procedure cvCalcPCA(const data: pCvArr; mean: pCvArr; eigenvals: pCvArr; eigenvects: pCvArr; flags: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7649
// {$ELSE}
// name 'cvCalcPCA'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void)  cvProjectPCA( const CvArr* data, const CvArr* mean,
// const CvArr* eigenvects, CvArr* result );
 procedure cvProjectPCA(const data: pCvArr; const mean: pCvArr; const eigenvects: pCvArr; result: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7879
// {$ELSE}
// name 'cvProjectPCA'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void)  cvBackProjectPCA( const CvArr* proj, const CvArr* mean,
// const CvArr* eigenvects, CvArr* result );
 procedure cvBackProjectPCA(const proj: pCvArr; const mean: pCvArr; const eigenvects: pCvArr; result: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7640
// {$ELSE}
// name 'cvBackProjectPCA'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates Mahalanobis(weighted) distance *)
// CVAPI(double)  cvMahalanobis( const CvArr* vec1, const CvArr* vec2, const CvArr* mat );
 function cvMahalanobis(const vec1: pCvArr; const vec2: pCvArr; const mat: pCvArr): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7837
// {$ELSE}
// name 'cvMahalanobis'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvMahalonobis  cvMahalanobis
 function cvMahalonobis(const vec1: pCvArr; const vec2: pCvArr; const mat: pCvArr): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7837
// {$ELSE}
// name 'cvMahalanobis'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  **************************************************************************************\
*                                    Array Statistics                                    *
\*************************************************************************************** *)

(* Finds sum of array elements *)
// CVAPI(CvScalar)  cvSum( const CvArr* arr );
 function cvSum(const arr: pCvArr): TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7997
// {$ELSE}
// name 'cvSum'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates number of non-zero pixels *)
// CVAPI(int)  cvCountNonZero( const CvArr* arr );
 function cvCountNonZero(const arr: pCvArr): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7690
// {$ELSE}
// name 'cvCountNonZero'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates mean value of array elements *)
// CVAPI(CvScalar)  cvAvg( const CvArr* arr, const CvArr* mask CV_DEFAULT(NULL) );
 function cvAvg(const arr: pCvArr; const mask: pCvArr = nil): TCvScalar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7638
// {$ELSE}
// name 'cvAvg'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates mean and standard deviation of pixel values *)
// CVAPI(void)  cvAvgSdv( const CvArr* arr, CvScalar* mean, CvScalar* std_dev,
// const CvArr* mask CV_DEFAULT(NULL) );
 procedure cvAvgSdv(const arr: pCvArr; mean: pCvScalar; std_dev: pCvScalar; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7639
// {$ELSE}
// name 'cvAvgSdv'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Finds global minimum, maximum and their positions *)
// CVAPI(void)  cvMinMaxLoc( const CvArr* arr, double* min_val, double* max_val,
// CvPoint* min_loc CV_DEFAULT(NULL),
// CvPoint* max_loc CV_DEFAULT(NULL),
// const CvArr* mask CV_DEFAULT(NULL) );
 procedure cvMinMaxLoc(const arr: pCvArr; min_val: pdouble; max_val: pdouble; min_loc: pCvPoint = nil;
 max_loc: pCvPoint = nil; const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7850
// {$ELSE}
// name 'cvMinMaxLoc'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @anchor core_c_NormFlags
  @name Flags for cvNorm and cvNormalize
  @{
*)
 const
 CV_C         = 1;
 CV_L1        = 2;
 CV_L2        = 4;
 CV_NORM_MASK = 7;
 CV_RELATIVE  = 8;
 CV_DIFF      = 16;
 CV_MINMAX    = 32;

 CV_DIFF_C      = (CV_DIFF or CV_C);
 CV_DIFF_L1     = (CV_DIFF or CV_L1);
 CV_DIFF_L2     = (CV_DIFF or CV_L2);
 CV_RELATIVE_C  = (CV_RELATIVE or CV_C);
 CV_RELATIVE_L1 = (CV_RELATIVE or CV_L1);
 CV_RELATIVE_L2 = (CV_RELATIVE or CV_L2);
(* @} *)

(* Finds norm, difference norm or relative difference norm for an array (or two arrays)
@see ref core_c_NormFlags "flags"
*)
// CVAPI(double)  cvNorm( const CvArr* arr1, const CvArr* arr2 CV_DEFAULT(NULL),
// int norm_type CV_DEFAULT(CV_L2),
// const CvArr* mask CV_DEFAULT(NULL) );
 function cvNorm(const arr1: pCvArr; const arr2: pCvArr = nil; norm_type: int = CV_L2;
 const mask: pCvArr = nil): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7864
// {$ELSE}
// name 'cvNorm'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @see ref core_c_NormFlags "flags" *)
// CVAPI(void)  cvNormalize( const CvArr* src, CvArr* dst,
// double a CV_DEFAULT(1.), double b CV_DEFAULT(0.),
// int norm_type CV_DEFAULT(CV_L2),
// const CvArr* mask CV_DEFAULT(NULL) );
 procedure cvNormalize(const src: pCvArr; dst: pCvArr; A: double = 1.; B: double = 0.; norm_type: int = CV_L2;
 const mask: pCvArr = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7865
// {$ELSE}
// name 'cvNormalize'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @anchor core_c_ReduceFlags
  @name Flags for cvReduce
  @{
*)
 const
 CV_REDUCE_SUM = 0;
 CV_REDUCE_AVG = 1;
 CV_REDUCE_MAX = 2;
 CV_REDUCE_MIN = 3;
(* @} *)

(* @see @ref core_c_ReduceFlags "flags" *)
// CVAPI(void)  cvReduce( const CvArr* src, CvArr* dst, int dim CV_DEFAULT(-1),
// int op CV_DEFAULT(CV_REDUCE_SUM) );
 procedure cvReduce(const src: pCvArr; dst: pCvArr; dim: int = -1; op: int = CV_REDUCE_SUM);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7897
// {$ELSE}
// name 'cvReduce'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  **************************************************************************************\
*                      Discrete Linear Transforms and Related Functions                  *
\*************************************************************************************** *)

(* @anchor core_c_DftFlags
  @name Flags for cvDFT, cvDCT and cvMulSpectrums
  @{
 *)
 const
 CV_DXT_FORWARD   = 0;
 CV_DXT_INVERSE   = 1;
 CV_DXT_SCALE     = 2; (*  < divide result by size of array *)
 CV_DXT_INV_SCALE = (CV_DXT_INVERSE + CV_DXT_SCALE);
 CV_DXT_INVERSE_SCALE = CV_DXT_INV_SCALE;
 CV_DXT_ROWS     = 4; (*  < transform each row individually *)
 CV_DXT_MUL_CONJ = 8; (*  < conjugate the second argument of cvMulSpectrums *)
(* @} *)

(* Discrete Fourier Transform:
    complex->complex,
    real->ccs (forward),
    ccs->real (inverse)
@see core_c_DftFlags "flags"
*)
// CVAPI(void)  cvDFT( const CvArr* src, CvArr* dst, int flags,
// int nonzero_rows CV_DEFAULT(0) );
 procedure cvDFT(const src: pCvArr; dst: pCvArr; flags: int; nonzero_rows: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7719
// {$ELSE}
// name 'cvDFT'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvFFT cvDFT
 procedure cvFFT(const src: pCvArr; dst: pCvArr; flags: int; nonzero_rows: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7719
// {$ELSE}
// name 'cvDFT'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Multiply results of DFTs: DFT(X)*DFT(Y) or DFT(X)*conj(DFT(Y))
@see core_c_DftFlags "flags"
*)
// CVAPI(void)  cvMulSpectrums( const CvArr* src1, const CvArr* src2,
// CvArr* dst, int flags );
 procedure cvMulSpectrums(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; flags: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7857
// {$ELSE}
// name 'cvMulSpectrums'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Finds optimal DFT vector size >= size0 *)
// CVAPI(int)  cvGetOptimalDFTSize( int size0 );
 function cvGetOptimalDFTSize(size0: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7779
// {$ELSE}
// name 'cvGetOptimalDFTSize'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Discrete Cosine Transform
@see core_c_DftFlags "flags"
*)
// CVAPI(void)  cvDCT( const CvArr* src, CvArr* dst, int flags );
 procedure cvDCT(const src: pCvArr; dst: pCvArr; flags: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7718
// {$ELSE}
// name 'cvDCT'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  **************************************************************************************\
*                              Dynamic data structures                                   *
\*************************************************************************************** *)

(* Calculates length of sequence slice (with support of negative indices). *)
// CVAPI(int) cvSliceLength( CvSlice slice, const CvSeq* seq );
 function cvSliceLength(slice: TCvSlice; const seq: pCvSeq): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7978
// {$ELSE}
// name 'cvSliceLength'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Creates new memory storage.
   block_size == 0 means that default,
   somewhat optimal size, is used (currently, it is 64K) *)
// CVAPI(CvMemStorage*)  cvCreateMemStorage( int block_size CV_DEFAULT(0));
 function cvCreateMemStorage(block_size: int = 0): pCvMemStorage;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7705
// {$ELSE}
// name 'cvCreateMemStorage'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Creates a memory storage that will borrow memory blocks from parent storage *)
// CVAPI(CvMemStorage*)  cvCreateChildMemStorage( CvMemStorage* parent );
 function cvCreateChildMemStorage(parent: pCvMemStorage): pCvMemStorage;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7692
// {$ELSE}
// name 'cvCreateChildMemStorage'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Releases memory storage. All the children of a parent must be released before
   the parent. A child storage returns all the blocks to parent when it is released *)
// CVAPI(void)  cvReleaseMemStorage( CvMemStorage** storage );
 procedure cvReleaseMemStorage(storage: pCvMemStorage);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7906
// {$ELSE}
// name 'cvReleaseMemStorage'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Clears memory storage. This is the only way(!!!) (besides cvRestoreMemStoragePos)
   to reuse memory allocated for the storage - cvClearSeq,cvClearSet ...
   do not free any memory.
   A child storage returns all the blocks to the parent when it is cleared *)
// CVAPI(void)  cvClearMemStorage( CvMemStorage* storage );
 procedure cvClearMemStorage(storage: pCvMemStorage);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7662
// {$ELSE}
// name 'cvClearMemStorage'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Remember a storage "free memory" position *)
// CVAPI(void)  cvSaveMemStoragePos( const CvMemStorage* storage, CvMemStoragePos* pos );
 procedure cvSaveMemStoragePos(const storage: pCvMemStorage; pos: pCvMemStoragePos);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7925
// {$ELSE}
// name 'cvSaveMemStoragePos'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Restore a storage "free memory" position *)
// CVAPI(void)  cvRestoreMemStoragePos( CvMemStorage* storage, CvMemStoragePos* pos );
 procedure cvRestoreMemStoragePos(storage: pCvMemStorage; pos: pCvMemStoragePos);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7919
// {$ELSE}
// name 'cvRestoreMemStoragePos'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Allocates continuous buffer of the specified size in the storage *)
// CVAPI(void*) cvMemStorageAlloc( CvMemStorage* storage, size_t size );
 function cvMemStorageAlloc(storage: pCvMemStorage; size: size_t): pointer;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7845
// {$ELSE}
// name 'cvMemStorageAlloc'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Allocates string in memory storage *)
// CVAPI(CvString) cvMemStorageAllocString( CvMemStorage* storage, const char* ptr,
// int len CV_DEFAULT(-1) );
// function cvMemStorageAllocString(storage:pCvMemStorage; const ptr:pcvchar; len:int=-1):TCvString;

(* Creates new empty sequence that will reside in the specified storage *)
// CVAPI(CvSeq*)  cvCreateSeq( int seq_flags, size_t header_size,
// size_t elem_size, CvMemStorage* storage );
 function cvCreateSeq(seq_flags: int; header_size: size_t; elem_size: size_t; storage: pCvMemStorage): pCvSeq;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7707
// {$ELSE}
// name 'cvCreateSeq'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Changes default size (granularity) of sequence blocks.
   The default size is ~1Kbyte *)
// CVAPI(void)  cvSetSeqBlockSize( CvSeq* seq, int delta_elems );
 procedure cvSetSeqBlockSize(seq: pCvSeq; delta_elems: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7970
// {$ELSE}
// name 'cvSetSeqBlockSize'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Adds new element to the end of sequence. Returns pointer to the element *)
// CVAPI(schar*)  cvSeqPush( CvSeq* seq, const void* element CV_DEFAULT(NULL));
 function cvSeqPush(seq: pCvSeq; const element: pvoid = nil): pschar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7936
// {$ELSE}
// name 'cvSeqPush'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Adds new element to the beginning of sequence. Returns pointer to it *)
// CVAPI(schar*)  cvSeqPushFront( CvSeq* seq, const void* element CV_DEFAULT(NULL));
 function cvSeqPushFront(seq: pCvSeq; const element: pvoid = nil): pschar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7937
// {$ELSE}
// name 'cvSeqPushFront'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Removes the last element from sequence and optionally saves it *)
// CVAPI(void)  cvSeqPop( CvSeq* seq, void* element CV_DEFAULT(NULL));
 procedure cvSeqPop(seq: pCvSeq; element: pvoid = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7933
// {$ELSE}
// name 'cvSeqPop'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Removes the first element from sequence and optioanally saves it *)
// CVAPI(void)  cvSeqPopFront( CvSeq* seq, void* element CV_DEFAULT(NULL));
 procedure cvSeqPopFront(seq: pCvSeq; element: pvoid = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7934
// {$ELSE}
// name 'cvSeqPopFront'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_FRONT = 1;
 CV_BACK  = 0;
(* Adds several new elements to the end of sequence *)
// CVAPI(void)  cvSeqPushMulti( CvSeq* seq, const void* elements,
// int count, int in_front CV_DEFAULT(0) );
 procedure cvSeqPushMulti(seq: pCvSeq; const elements: pvoid; count: int; in_front: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7938
// {$ELSE}
// name 'cvSeqPushMulti'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Removes several elements from the end of sequence and optionally saves them *)
// CVAPI(void)  cvSeqPopMulti( CvSeq* seq, void* elements,
// int count, int in_front CV_DEFAULT(0) );
 procedure cvSeqPopMulti(seq: pCvSeq; elements: pvoid; count: int; in_front: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7935
// {$ELSE}
// name 'cvSeqPopMulti'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Inserts a new element in the middle of sequence.
   cvSeqInsert(seq,0,elem) == cvSeqPushFront(seq,elem) *)
// CVAPI(schar*)  cvSeqInsert( CvSeq* seq, int before_index,
// const void* element CV_DEFAULT(NULL));
 function cvSeqInsert(seq: pCvSeq; before_index: int; const element: pvoid = nil): pschar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7929
// {$ELSE}
// name 'cvSeqInsert'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Removes specified sequence element *)
// CVAPI(void)  cvSeqRemove( CvSeq* seq, int index );
 procedure cvSeqRemove(seq: pCvSeq; index: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7939
// {$ELSE}
// name 'cvSeqRemove'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Removes all the elements from the sequence. The freed memory
   can be reused later only by the same sequence unless cvClearMemStorage
   or cvRestoreMemStoragePos is called *)
// CVAPI(void)  cvClearSeq( CvSeq* seq );
 procedure cvClearSeq(seq: pCvSeq);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7664
// {$ELSE}
// name 'cvClearSeq'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Retrieves pointer to specified sequence element.
   Negative indices are supported and mean counting from the end
   (e.g -1 means the last sequence element) *)
// CVAPI(schar*)  cvGetSeqElem( const CvSeq* seq, int index );
 function cvGetSeqElem(const seq: pCvSeq; index: int): pschar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7789
// {$ELSE}
// name 'cvGetSeqElem'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Calculates index of the specified sequence element.
   Returns -1 if element does not belong to the sequence *)
// CVAPI(int)  cvSeqElemIdx( const CvSeq* seq, const void* element,
// CvSeqBlock** block CV_DEFAULT(NULL) );
 function cvSeqElemIdx(const seq: pCvSeq; const element: pvoid; block: pCvSeqBlock = nil): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7928
// {$ELSE}
// name 'cvSeqElemIdx'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Initializes sequence writer. The new elements will be added to the end of sequence *)
// CVAPI(void)  cvStartAppendToSeq( CvSeq* seq, CvSeqWriter* writer );
 procedure cvStartAppendToSeq(seq: pCvSeq; writer: pCvSeqWriter);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7987
// {$ELSE}
// name 'cvStartAppendToSeq'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Combination of cvCreateSeq and cvStartAppendToSeq *)
// CVAPI(void)  cvStartWriteSeq( int seq_flags, int header_size,
// int elem_size, CvMemStorage* storage,
// CvSeqWriter* writer );
 procedure cvStartWriteSeq(seq_flags: int; header_size: int; elem_size: int; storage: pCvMemStorage;
 writer: pCvSeqWriter);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7992
// {$ELSE}
// name 'cvStartWriteSeq'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Closes sequence writer, updates sequence header and returns pointer
   to the resultant sequence
   (which may be useful if the sequence was created using cvStartWriteSeq))
*)
// CVAPI(CvSeq*)  cvEndWriteSeq( CvSeqWriter* writer );
 function cvEndWriteSeq(writer: pCvSeqWriter): pCvSeq;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7732
// {$ELSE}
// name 'cvEndWriteSeq'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Updates sequence header. May be useful to get access to some of previously
   written elements via cvGetSeqElem or sequence reader *)
// CVAPI(void)   cvFlushSeqWriter( CvSeqWriter* writer );
 procedure cvFlushSeqWriter(writer: pCvSeqWriter);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7752
// {$ELSE}
// name 'cvFlushSeqWriter'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Initializes sequence reader.
   The sequence can be read in forward or backward direction *)
// CVAPI(void) cvStartReadSeq( const CvSeq* seq, CvSeqReader* reader,
// int reverse CV_DEFAULT(0) );
 procedure cvStartReadSeq(const seq: pCvSeq; reader: pCvSeqReader; reverse: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7990
// {$ELSE}
// name 'cvStartReadSeq'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Returns current sequence reader position (currently observed sequence element) *)
// CVAPI(int)  cvGetSeqReaderPos( CvSeqReader* reader );
 function cvGetSeqReaderPos(reader: pCvSeqReader): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7790
// {$ELSE}
// name 'cvGetSeqReaderPos'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Changes sequence reader position. It may seek to an absolute or
   to relative to the current position *)
// CVAPI(void)   cvSetSeqReaderPos( CvSeqReader* reader, int index,
// int is_relative CV_DEFAULT(0));
 procedure cvSetSeqReaderPos(reader: pCvSeqReader; index: int; is_relative: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7971
// {$ELSE}
// name 'cvSetSeqReaderPos'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Copies sequence content to a continuous piece of memory *)
// CVAPI(void*)  cvCvtSeqToArray( const CvSeq* seq, void* elements,
// CvSlice slice CV_DEFAULT(CV_WHOLE_SEQ) );
 function cvCvtSeqToArray(const seq: pCvSeq; elements: pvoid; slice: TCvSlice): pointer; overload;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7717
// {$ELSE}
// name 'cvCvtSeqToArray'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
 function cvCvtSeqToArray(const seq: pCvSeq; elements: pvoid): pointer; overload; inline;

(* Creates sequence header for array.
   After that all the operations on sequences that do not alter the content
   can be applied to the resultant sequence *)
// CVAPI(CvSeq*) cvMakeSeqHeaderForArray( int seq_type, int header_size,
// int elem_size, void* elements, int total,
// CvSeq* seq, CvSeqBlock* block );
 function cvMakeSeqHeaderForArray(seq_type: int; header_size: int; elem_size: int; elements: pvoid; total: int;
 seq: pCvSeq; block: pCvSeqBlock): pCvSeq;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7839
// {$ELSE}
// name 'cvMakeSeqHeaderForArray'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Extracts sequence slice (with or without copying sequence elements) *)
// CVAPI(CvSeq*) cvSeqSlice( const CvSeq* seq, CvSlice slice,
// CvMemStorage* storage CV_DEFAULT(NULL),
// int copy_data CV_DEFAULT(0));
 function cvSeqSlice(const seq: pCvSeq; slice: TCvSlice; storage: pCvMemStorage = nil;
 copy_data: int = 0): pCvSeq;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7942
// {$ELSE}
// name 'cvSeqSlice'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CV_INLINE CvSeq* cvCloneSeq( const CvSeq* seq, CvMemStorage* storage CV_DEFAULT(NULL))
// {
// return cvSeqSlice( seq, CV_WHOLE_SEQ, storage, 1 );
// }
 function cvCloneSeq(const seq: pCvSeq; storage: pCvMemStorage = nil): pCvSeq; inline;

(* Removes sequence slice *)
// CVAPI(void)  cvSeqRemoveSlice( CvSeq* seq, CvSlice slice );
 procedure cvSeqRemoveSlice(seq: pCvSeq; slice: TCvSlice);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7940
// {$ELSE}
// name 'cvSeqRemoveSlice'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Inserts a sequence or array into another sequence *)
// CVAPI(void)  cvSeqInsertSlice( CvSeq* seq, int before_index, const CvArr* from_arr );
 procedure cvSeqInsertSlice(seq: pCvSeq; before_index: int; const from_arr: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7930
// {$ELSE}
// name 'cvSeqInsertSlice'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* a < b ? -1 : a > b ? 1 : 0 *)
// typedef int (CV_CDECL* CvCmpFunc)(const void* a, const void* b, void* userdata );
 type
 TCvCmpFunc = function(const A: pointer; const B: pointer; userdata: pointer): int;

(* Sorts sequence in-place given element comparison function *)
// CVAPI(void) cvSeqSort( CvSeq* seq, CvCmpFunc func, void* userdata CV_DEFAULT(NULL) );
 procedure cvSeqSort(seq: pCvSeq; func: TCvCmpFunc; userdata: pvoid = nil);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7943
// {$ELSE}
// name 'cvSeqSort'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Finds element in a [sorted] sequence *)
// CVAPI(schar*) cvSeqSearch( CvSeq* seq, const void* elem, CvCmpFunc func,
// int is_sorted, int* elem_idx,
// void* userdata CV_DEFAULT(NULL) );
 function cvSeqSearch(seq: pCvSeq; const elem: pvoid; func: TCvCmpFunc; is_sorted: int; elem_idx: pint;
 userdata: pvoid = nil): pschar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7941
// {$ELSE}
// name 'cvSeqSearch'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Reverses order of sequence elements in-place *)
// CVAPI(void) cvSeqInvert( CvSeq* seq );
 procedure cvSeqInvert(seq: pCvSeq);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7931
// {$ELSE}
// name 'cvSeqInvert'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Splits sequence into one or more equivalence classes using the specified criteria *)
// CVAPI(int)  cvSeqPartition( const CvSeq* seq, CvMemStorage* storage,
// CvSeq** labels, CvCmpFunc is_equal, void* userdata );
 function cvSeqPartition(const seq: pCvSeq; storage: pCvMemStorage; labels: pCvSeq; is_equal: TCvCmpFunc;
 userdata: pvoid): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7932
// {$ELSE}
// name 'cvSeqPartition'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  ********** Internal sequence functions *********** *)
// CVAPI(void)  cvChangeSeqBlock( void* reader, int direction );
 procedure cvChangeSeqBlock(reader: pvoid; direction: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7654
// {$ELSE}
// name 'cvChangeSeqBlock'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void)  cvCreateSeqBlock( CvSeqWriter* writer );
 procedure cvCreateSeqBlock(writer: pCvSeqWriter);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7708
// {$ELSE}
// name 'cvCreateSeqBlock'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Creates a new set *)
// CVAPI(CvSet*)  cvCreateSet( int set_flags, int header_size,
// int elem_size, CvMemStorage* storage );
 function cvCreateSet(set_flags: int; header_size: int; elem_size: int; storage: pCvMemStorage): pCvSet;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7709
// {$ELSE}
// name 'cvCreateSet'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Adds new element to the set and returns pointer to it *)
// CVAPI(int)  cvSetAdd( CvSet* set_header, CvSetElem* elem CV_DEFAULT(NULL),
// CvSetElem** inserted_elem CV_DEFAULT(NULL) );
 function cvSetAdd(set_header: pCvSet; elem: pCvSetElem = nil; inserted_elem: pCvSetElem = nil): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7948
// {$ELSE}
// name 'cvSetAdd'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Fast variant of cvSetAdd *)
// CV_INLINE  CvSetElem* cvSetNew( CvSet* set_header )
// {
// CvSetElem* elem = set_header->free_elems;
// if( elem )
// {
// set_header->free_elems = elem->next_free;
// elem->flags = elem->flags & CV_SET_ELEM_IDX_MASK;
// set_header->active_count++;
// }
// else
// cvSetAdd( set_header, NULL, &elem );
// return elem;
// }
 function cvSetNew(set_header: pCvSet): pCvSetElem; inline;

(* Removes set element given its pointer *)
// CV_INLINE  void cvSetRemoveByPtr( CvSet* set_header, void* elem )
// {
// CvSetElem* _elem = (CvSetElem*)elem;
// assert( _elem->flags >= 0 (*&& (elem->flags & CV_SET_ELEM_IDX_MASK) < set_header->total*) );
// _elem->next_free = set_header->free_elems;
// _elem->flags = (_elem->flags & CV_SET_ELEM_IDX_MASK) | CV_SET_ELEM_FREE_FLAG;
// set_header->free_elems = _elem;
// set_header->active_count--;
// }
 procedure cvSetRemoveByPtr(set_header: pCvSet; elem: pvoid); inline;

(* Removes element from the set by its index *)
// CVAPI(void)   cvSetRemove( CvSet* set_header, int index );
 procedure cvSetRemove(set_header: pCvSet; index: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7969
// {$ELSE}
// name 'cvSetRemove'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Returns a set element by index. If the element doesn't belong to the set,
   NULL is returned *)
// CV_INLINE CvSetElem* cvGetSetElem( const CvSet* set_header, int idx )
// {
// CvSetElem* elem = (CvSetElem*)(void *)cvGetSeqElem( (CvSeq*)set_header, idx );
// return elem && CV_IS_SET_ELEM( elem ) ? elem : 0;
// }
 function cvGetSetElem(const set_header: pCvSet; idx: int): pCvSetElem; inline;

(* Removes all the elements from the set *)
// CVAPI(void)  cvClearSet( CvSet* set_header );
 procedure cvClearSet(set_header: pCvSet);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7665
// {$ELSE}
// name 'cvClearSet'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Creates new graph *)
// CVAPI(CvGraph*)  cvCreateGraph( int graph_flags, int header_size,
// int vtx_size, int edge_size,
// CvMemStorage* storage );
 function cvCreateGraph(graph_flags: int; header_size: int; vtx_size: int; edge_size: int;
 storage: pCvMemStorage): pCvGraph;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7696
// {$ELSE}
// name 'cvCreateGraph'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Adds new vertex to the graph *)
// CVAPI(int)  cvGraphAddVtx( CvGraph* graph, const CvGraphVtx* vtx CV_DEFAULT(NULL),
// CvGraphVtx** inserted_vtx CV_DEFAULT(NULL) );
 function cvGraphAddVtx(graph: pCvGraph; const vtx: pCvGraphVtx = nil; inserted_vtx: pCvGraphVtx = nil): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7806
// {$ELSE}
// name 'cvGraphAddVtx'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Removes vertex from the graph together with all incident edges *)
// CVAPI(int)  cvGraphRemoveVtx( CvGraph* graph, int index );
 function cvGraphRemoveVtx(graph: pCvGraph; index: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7809
// {$ELSE}
// name 'cvGraphRemoveVtx'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(int)  cvGraphRemoveVtxByPtr( CvGraph* graph, CvGraphVtx* vtx );
 function cvGraphRemoveVtxByPtr(graph: pCvGraph; vtx: pCvGraphVtx): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7810
// {$ELSE}
// name 'cvGraphRemoveVtxByPtr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Link two vertices specified by indices or pointers if they
   are not connected or return pointer to already existing edge
   connecting the vertices.
   Functions return 1 if a new edge was created, 0 otherwise *)
// CVAPI(int)  cvGraphAddEdge( CvGraph* graph,
// int start_idx, int end_idx,
// const CvGraphEdge* edge CV_DEFAULT(NULL),
// CvGraphEdge** inserted_edge CV_DEFAULT(NULL) );
 function cvGraphAddEdge(graph: pCvGraph; start_idx: int; end_idx: int; const edge: pCvGraphEdge = nil;
 inserted_edge: pCvGraphEdge = nil): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7804
// {$ELSE}
// name 'cvGraphAddEdge'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(int)  cvGraphAddEdgeByPtr( CvGraph* graph,
// CvGraphVtx* start_vtx, CvGraphVtx* end_vtx,
// const CvGraphEdge* edge CV_DEFAULT(NULL),
// CvGraphEdge** inserted_edge CV_DEFAULT(NULL) );
 function cvGraphAddEdgeByPtr(graph: pCvGraph; start_vtx: pCvGraphVtx; end_vtx: pCvGraphVtx;
 const edge: pCvGraphEdge = nil; inserted_edge: pCvGraphEdge = nil): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7805
// {$ELSE}
// name 'cvGraphAddEdgeByPtr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Remove edge connecting two vertices *)
// CVAPI(void)  cvGraphRemoveEdge( CvGraph* graph, int start_idx, int end_idx );
 procedure cvGraphRemoveEdge(graph: pCvGraph; start_idx: int; end_idx: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7807
// {$ELSE}
// name 'cvGraphRemoveEdge'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void)  cvGraphRemoveEdgeByPtr( CvGraph* graph, CvGraphVtx* start_vtx,
// CvGraphVtx* end_vtx );
 procedure cvGraphRemoveEdgeByPtr(graph: pCvGraph; start_vtx: pCvGraphVtx; end_vtx: pCvGraphVtx);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7808
// {$ELSE}
// name 'cvGraphRemoveEdgeByPtr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Find edge connecting two vertices *)
// CVAPI(CvGraphEdge*)  cvFindGraphEdge( const CvGraph* graph, int start_idx, int end_idx );
 function cvFindGraphEdge(const graph: pCvGraph; start_idx: int; end_idx: int): pCvGraphEdge;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7745
// {$ELSE}
// name 'cvFindGraphEdge'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvGraphFindEdge cvFindGraphEdge
 function cvGraphFindEdge(const graph: pCvGraph; start_idx: int; end_idx: int): pCvGraphEdge;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7745
// {$ELSE}
// name 'cvFindGraphEdge'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(CvGraphEdge*)  cvFindGraphEdgeByPtr( const CvGraph* graph,
// const CvGraphVtx* start_vtx,
// const CvGraphVtx* end_vtx );
 function cvFindGraphEdgeByPtr(const graph: pCvGraph; const start_vtx: pCvGraphVtx; const end_vtx: pCvGraphVtx)
 : pCvGraphEdge;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7746
// {$ELSE}
// name 'cvFindGraphEdgeByPtr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// #define cvGraphFindEdgeByPtr cvFindGraphEdgeByPtr
 function cvGraphFindEdgeByPtr(const graph: pCvGraph; const start_vtx: pCvGraphVtx; const end_vtx: pCvGraphVtx)
 : pCvGraphEdge;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7746
// {$ELSE}
// name 'cvFindGraphEdgeByPtr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Remove all vertices and edges from the graph *)
// CVAPI(void)  cvClearGraph( CvGraph* graph );
 procedure cvClearGraph(graph: pCvGraph);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7660
// {$ELSE}
// name 'cvClearGraph'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Count number of edges incident to the vertex *)
// CVAPI(int)  cvGraphVtxDegree( const CvGraph* graph, int vtx_idx );
 function cvGraphVtxDegree(const graph: pCvGraph; vtx_idx: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7811
// {$ELSE}
// name 'cvGraphVtxDegree'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(int)  cvGraphVtxDegreeByPtr( const CvGraph* graph, const CvGraphVtx* vtx );
 function cvGraphVtxDegreeByPtr(const graph: pCvGraph; const vtx: pCvGraphVtx): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7812
// {$ELSE}
// name 'cvGraphVtxDegreeByPtr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Retrieves graph vertex by given index *)
{ TODO:cvGetGraphVtx }
// #define cvGetGraphVtx( graph, idx ) (CvGraphVtx*)cvGetSetElem((CvSet*)(graph), (idx))

(* Retrieves index of a graph vertex given its pointer *)
{ TODO:cvGraphVtxIdx }
// #define cvGraphVtxIdx( graph, vtx ) ((vtx)->flags & CV_SET_ELEM_IDX_MASK)

(* Retrieves index of a graph edge given its pointer *)
{ TODO:cvGraphEdgeIdx }
// #define cvGraphEdgeIdx( graph, edge ) ((edge)->flags & CV_SET_ELEM_IDX_MASK)

{ TODO:cvGraphGetVtxCount }
// #define cvGraphGetVtxCount( graph ) ((graph)->active_count)
{ TODO:cvGraphGetEdgeCount }
// #define cvGraphGetEdgeCount( graph ) ((graph)->edges->active_count)

 const
 CV_GRAPH_VERTEX    = 1;
 CV_GRAPH_TREE_EDGE = 2;
 CV_GRAPH_BACK_EDGE = 4;
 CV_GRAPH_FORWARD_EDGE = 8;
 CV_GRAPH_CROSS_EDGE = 16;
 CV_GRAPH_ANY_EDGE = 30;
 CV_GRAPH_NEW_TREE = 32;
 CV_GRAPH_BACKTRACKING = 64;
 CV_GRAPH_OVER = -1;

 CV_GRAPH_ALL_ITEMS = -1;

(* flags for graph vertices and edges *)
 CV_GRAPH_ITEM_VISITED_FLAG = (1 shl 30);

{ TODO:CV_IS_GRAPH_VERTEX_VISITED }
// #define  CV_IS_GRAPH_VERTEX_VISITED(vtx) \
// (((CvGraphVtx*)(vtx))->flags & CV_GRAPH_ITEM_VISITED_FLAG)
// #define  CV_IS_GRAPH_EDGE_VISITED(edge) \
// (((CvGraphEdge*)(edge))->flags & CV_GRAPH_ITEM_VISITED_FLAG)

 CV_GRAPH_SEARCH_TREE_NODE_FLAG = (1 shl 29);
 CV_GRAPH_FORWARD_EDGE_FLAG = (1 shl 28);

 type
 pCvGraphScanner = ^_CvGraphScanner;

 _CvGraphScanner = record
 vtx: pCvGraphVtx; (* current graph vertex (or current edge origin) *)
 dst: pCvGraphVtx; (* current graph edge destination vertex *)
 edge: pCvGraphEdge; (* current edge *)

 graph: pCvGraph; (* the graph *)
 stack: pCvSeq;   (* the graph vertex stack *)
 index: int;      (* the lower bound of certainly visited vertices *)
 mask: int;       (* event mask *)
 end;

 TCvGraphScanner = _CvGraphScanner;

(* Creates new graph scanner. *)
// CVAPI(CvGraphScanner*)  cvCreateGraphScanner( CvGraph* graph,
// CvGraphVtx* vtx CV_DEFAULT(NULL),
// int mask CV_DEFAULT(CV_GRAPH_ALL_ITEMS));
 function cvCreateGraphScanner(graph: pCvGraph; vtx: pCvGraphVtx = nil; mask: int = CV_GRAPH_ALL_ITEMS)
 : pCvGraphScanner;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7697
// {$ELSE}
// name 'cvCreateGraphScanner'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Releases graph scanner. *)
// CVAPI(void) cvReleaseGraphScanner( CvGraphScanner** scanner );
 procedure cvReleaseGraphScanner(scanner: pCvGraphScanner);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7901
// {$ELSE}
// name 'cvReleaseGraphScanner'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Get next graph element *)
// CVAPI(int)  cvNextGraphItem( CvGraphScanner* scanner );
 function cvNextGraphItem(scanner: pCvGraphScanner): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7861
// {$ELSE}
// name 'cvNextGraphItem'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Creates a copy of graph *)
// CVAPI(CvGraph*) cvCloneGraph( const CvGraph* graph, CvMemStorage* storage );
 function cvCloneGraph(const graph: pCvGraph; storage: pCvMemStorage): pCvGraph;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7668
// {$ELSE}
// name 'cvCloneGraph'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Does look-up transformation. Elements of the source array
   (that should be 8uC1 or 8sC1) are used as indexes in lutarr 256-element table *)
// CVAPI(void) cvLUT( const CvArr* src, CvArr* dst, const CvArr* lut );
 procedure cvLUT(const src: pCvArr; dst: pCvArr; const lut: pCvArr);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7831
// {$ELSE}
// name 'cvLUT'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  ***************** Iteration through the sequence tree **************** *)
 type
 pCvTreeNodeIterator = ^_CvTreeNodeIterator;

 _CvTreeNodeIterator = record
 node: pointer;
 level: int;
 max_level: int;
 end;

 TCvTreeNodeIterator = _CvTreeNodeIterator;

// CVAPI(void) cvInitTreeNodeIterator( CvTreeNodeIterator* tree_iterator,
// const void* first, int max_level );
 procedure cvInitTreeNodeIterator(tree_iterator: pCvTreeNodeIterator; const first: pvoid; max_level: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7826
// {$ELSE}
// name 'cvInitTreeNodeIterator'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void*) cvNextTreeNode( CvTreeNodeIterator* tree_iterator );
 function cvNextTreeNode(tree_iterator: pCvTreeNodeIterator): pointer;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7863
// {$ELSE}
// name 'cvNextTreeNode'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void*) cvPrevTreeNode( CvTreeNodeIterator* tree_iterator );
 function cvPrevTreeNode(tree_iterator: pCvTreeNodeIterator): pointer;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7878
// {$ELSE}
// name 'cvPrevTreeNode'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Inserts sequence into tree with specified "parent" sequence.
   If parent is equal to frame (e.g. the most external contour),
   then added contour will have null pointer to parent. *)
// CVAPI(void) cvInsertNodeIntoTree( void* node, void* parent, void* frame );
 procedure cvInsertNodeIntoTree(node: pvoid; parent: pvoid; frame: pvoid);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7827
// {$ELSE}
// name 'cvInsertNodeIntoTree'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Removes contour from tree (together with the contour children). *)
// CVAPI(void) cvRemoveNodeFromTree( void* node, void* frame );
 procedure cvRemoveNodeFromTree(node: pvoid; frame: pvoid);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7912
// {$ELSE}
// name 'cvRemoveNodeFromTree'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Gathers pointers to all the sequences,
   accessible from the `first`, to the single sequence *)
// CVAPI(CvSeq*) cvTreeToNodeSeq( const void* first, int header_size,
// CvMemStorage* storage );
 function cvTreeToNodeSeq(const first: pvoid; header_size: int; storage: pCvMemStorage): pCvSeq;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8003
// {$ELSE}
// name 'cvTreeToNodeSeq'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* The function implements the K-means algorithm for clustering an array of sample
   vectors in a specified number of classes *)
 const
 CV_KMEANS_USE_INITIAL_LABELS = 1;

// CVAPI(int) cvKMeans2( const CvArr* samples, int cluster_count, CvArr* labels,
// CvTermCriteria termcrit, int attempts CV_DEFAULT(1),
// CvRNG* rng CV_DEFAULT(0), int flags CV_DEFAULT(0),
// CvArr* _centers CV_DEFAULT(0), double* compactness CV_DEFAULT(0) );
 function cvKMeans2(const samples: pCvArr; cluster_count: int; labels: pCvArr; termcrit: TCvTermCriteria;
 attempts: int = 1; rng: pCvRNG = nil; flags: int = 0; _centers: pCvArr = nil;
 compactness: pdouble = nil): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7830
// {$ELSE}
// name 'cvKMeans2'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  **************************************************************************************\
*                                    System functions                                    *
\*************************************************************************************** *)

(* Loads optimized functions from IPP, MKL etc. or switches back to pure C code *)
// CVAPI(int)  cvUseOptimized( int on_off );
 function cvUseOptimized(on_off: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 8005
// {$ELSE}
// name 'cvUseOptimized'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 type
// IplImage* (CV_STDCALL* Cv_iplCreateImageHeader)
// (int,int,int,char*,char*,int,int,int,int,int,
// IplROI*,IplImage*,void*,IplTileInfo*);
 TCv_iplCreateImageHeader = function(A: int; B: int; c: int; d: pcvchar; e: pcvchar; f: int; g: int; h: int;
 i: int; j: int; k: pIplROI; l: pIplImage; m: pointer; n: pIplTileInfo): pIplImage;
// void (CV_STDCALL* Cv_iplAllocateImageData)(IplImage*,int,int);
 TCv_iplAllocateImageData = procedure(A: pIplImage; B: int; c: int);
// void (CV_STDCALL* Cv_iplDeallocate)(IplImage*,int);
 TCv_iplDeallocate = procedure(A: pIplImage; B: int);
// IplROI* (CV_STDCALL* Cv_iplCreateROI)(int,int,int,int,int);
 TCv_iplCreateROI = function(A: int; B: int; c: int; d: int; e: int): pIplROI;
// IplImage* (CV_STDCALL* Cv_iplCloneImage)(const IplImage*);
 TCv_iplCloneImage = function(const A: pIplImage): pIplImage;

(* @brief Makes OpenCV use IPL functions for allocating IplImage and IplROI structures.

Normally, the function is not called directly. Instead, a simple macro
CV_TURN_ON_IPL_COMPATIBILITY() is used that calls cvSetIPLAllocators and passes there pointers
to IPL allocation functions. :
@code
    ...
    CV_TURN_ON_IPL_COMPATIBILITY()
    ...
@endcode
@param create_header pointer to a function, creating IPL image header.
@param allocate_data pointer to a function, allocating IPL image data.
@param deallocate pointer to a function, deallocating IPL image.
@param create_roi pointer to a function, creating IPL image ROI (i.e. Region of Interest).
@param clone_image pointer to a function, cloning an IPL image.
 *)
// CVAPI(void) cvSetIPLAllocators( Cv_iplCreateImageHeader create_header,
// Cv_iplAllocateImageData allocate_data,
// Cv_iplDeallocate deallocate,
// Cv_iplCreateROI create_roi,
// Cv_iplCloneImage clone_image );
 procedure cvSetIPLAllocators(create_header: TCv_iplCreateImageHeader; allocate_data: TCv_iplAllocateImageData;
 deallocate: TCv_iplDeallocate; create_roi: TCv_iplCreateROI; clone_image: TCv_iplCloneImage);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7954
// {$ELSE}
// name 'cvSetIPLAllocators'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{ TODO:CV_TURN_ON_IPL_COMPATIBILITY }
// #define CV_TURN_ON_IPL_COMPATIBILITY()                                  \
// cvSetIPLAllocators( iplCreateImageHeader, iplAllocateImage,         \
// iplDeallocate, iplCreateROI, iplCloneImage )

(*  **************************************************************************************\
*                                    Data Persistence                                    *
\*************************************************************************************** *)

(* @brief Releases an object.

 The function finds the type of a given object and calls release with the double pointer.
 @param struct_ptr Double pointer to the object
 *)
// CVAPI(void) cvRelease( void** struct_ptr );
 procedure cvRelease(struct_ptr: pvoid);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7898
// {$ELSE}
// name 'cvRelease'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* @brief Makes a clone of an object.

The function finds the type of a given object and calls clone with the passed object. Of course, if
you know the object type, for example, struct_ptr is CvMat\*, it is faster to call the specific
function, like cvCloneMat.
@param struct_ptr The object to clone
 *)
// CVAPI(void*) cvClone( const void* struct_ptr );
 function cvClone(const struct_ptr: pvoid): pointer;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7667
// {$ELSE}
// name 'cvClone'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  ********************************* Measuring Execution Time ************************** *)

(* helper functions for RNG initialization and accurate time measurement:
   uses internal clock counter on x86 *)
// CVAPI(int64)  cvGetTickCount( void );
 function cvGetTickCount(): int64;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7796
// {$ELSE}
// name 'cvGetTickCount'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(double) cvGetTickFrequency( void );
 function cvGetTickFrequency(): double;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7797
// {$ELSE}
// name 'cvGetTickFrequency'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  ********************************* CPU capabilities ********************************** *)

// CVAPI(int) cvCheckHardwareSupport(int feature);
 function cvCheckHardwareSupport(feature: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7657
// {$ELSE}
// name 'cvCheckHardwareSupport'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  ********************************* Multi-Threading *********************************** *)

(* retrieve/set the number of threads used in OpenMP implementations *)
// CVAPI(int)  cvGetNumThreads( void );
 function cvGetNumThreads(): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7778
// {$ELSE}
// name 'cvGetNumThreads'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

// CVAPI(void) cvSetNumThreads( int threads CV_DEFAULT(0) );
 procedure cvSetNumThreads(threads: int = 0);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7960
// {$ELSE}
// name 'cvSetNumThreads'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* get index of the thread being executed *)
// CVAPI(int)  cvGetThreadNum( void );
 function cvGetThreadNum(): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7795
// {$ELSE}
// name 'cvGetThreadNum'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(*  ******************************** Error Handling ************************************* *)

(* Get current OpenCV error status *)
// CVAPI(int) cvGetErrStatus( void );
 function cvGetErrStatus(): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7769
// {$ELSE}
// name 'cvGetErrStatus'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Sets error status silently *)
// CVAPI(void) cvSetErrStatus( int status );
 procedure cvSetErrStatus(status: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7952
// {$ELSE}
// name 'cvSetErrStatus'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 const
 CV_ErrModeLeaf   = 0; (* Print error and exit program *)
 CV_ErrModeParent = 1; (* Print error and continue *)
 CV_ErrModeSilent = 2; (* Don't print and continue *)

(* Retrieves current error processing mode *)
// CVAPI(int)  cvGetErrMode( void );
 function cvGetErrMode(): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7768
// {$ELSE}
// name 'cvGetErrMode'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Sets error processing mode, returns previously used mode *)
// CVAPI(int) cvSetErrMode( int mode );
 function cvSetErrMode(mode: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7951
// {$ELSE}
// name 'cvSetErrMode'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Sets error status and performs some additional actions (displaying message box,
 writing message to stderr, terminating application etc.)
 depending on the current error mode *)
// CVAPI(void) cvError( int status, const char* func_name,
// const char* err_msg, const char* file_name, int line );
 procedure cvError(status: int; const func_name: pcvchar; const err_msg: pcvchar; const file_name: pcvchar;
 line: int);
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7735
// {$ELSE}
// name 'cvError'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Retrieves textual description of the error given its code *)
// CVAPI(const char*) cvErrorStr( int status );
 function cvErrorStr(status: int): pcvchar;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7737
// {$ELSE}
// name 'cvErrorStr'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Retrieves detailed information about the last error occurred *)
// CVAPI(int) cvGetErrInfo( const char** errcode_desc, const char** description,
// const char** filename, int* line );
 function cvGetErrInfo(const errcode_desc: pcvchar; const description: pcvchar; const filename: pcvchar;
 line: pint): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7767
// {$ELSE}
// name 'cvGetErrInfo'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Maps IPP error codes to the counterparts from OpenCV *)
// CVAPI(int) cvErrorFromIppStatus( int ipp_status );
 function cvErrorFromIppStatus(ipp_status: int): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7736
// {$ELSE}
// name 'cvErrorFromIppStatus'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

 type
// int (CV_CDECL *CvErrorCallback)( int status, const char* func_name,
// const char* err_msg, const char* file_name, int line, void* userdata );
 TCvErrorCallback = function(status: int; const func_name: pcvchar; const err_msg: pcvchar;
 const file_name: pcvchar; line: int; userdata: pointer): int;

(* Assigns a new error-handling function *)
{
 CVAPI(CvErrorCallback) cvRedirectError
	(
	CvErrorCallback error_handler,
	void* userdata CV_DEFAULT(NULL),
	void** prev_userdata CV_DEFAULT(NULL)
	);
}
function cvRedirectError(
		error_handler: TCvErrorCallback; 
		userdata: pvoid = nil; 
		prev_userdata: pvoid = nil
		): TCvErrorCallback; {$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7896
// {$ELSE}
// name 'cvRedirectError'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Output nothing *)
// CVAPI(int) cvNulDevReport( int status, const char* func_name, const char* err_msg,
// const char* file_name, int line, void* userdata );
 function cvNulDevReport(status: int; const func_name: pcvchar; const err_msg: pcvchar;
 const file_name: pcvchar; line: int; userdata: pvoid): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7868
// {$ELSE}
// name 'cvNulDevReport'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Output to console(fprintf(stderr,...)) *)
// CVAPI(int) cvStdErrReport( int status, const char* func_name, const char* err_msg,
// const char* file_name, int line, void* userdata );
 function cvStdErrReport(status: int; const func_name: pcvchar; const err_msg: pcvchar;
 const file_name: pcvchar; line: int; userdata: pvoid): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7993
// {$ELSE}
// name 'cvStdErrReport'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

(* Output to MessageBox(WIN32) *)
// CVAPI(int) cvGuiBoxReport( int status, const char* func_name, const char* err_msg,
// const char* file_name, int line, void* userdata );
 function cvGuiBoxReport(status: int; const func_name: pcvchar; const err_msg: pcvchar;
 const file_name: pcvchar; line: int; userdata: pvoid): int;
{$IFDEF USE_INLINE}inline; {$ENDIF}
// external opencv_world_dll
// {$IFDEF CALL_BY_FNC_NUM}
// index 7813
// {$ELSE}
// name 'cvGuiBoxReport'
// {$ENDIF}
// {$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{ TODO:OPENCV_ERROR }
// #define OPENCV_ERROR(status,func,context)                           \
// cvError((status),(func),(context),__FILE__,__LINE__)

{ TODO:OPENCV_ASSERT }
// #define OPENCV_ASSERT(expr,func,context)                            \
// {if (! (expr))                                      \
// {OPENCV_ERROR(CV_StsInternal,(func),(context));}}

{ TODO:OPENCV_CALL }
// #define OPENCV_CALL( Func )  \
// {                            \
// Func;                    \
// }

(* CV_FUNCNAME macro defines icvFuncName constant which is used by CV_ERROR macro *)
// #ifdef CV_NO_FUNC_NAMES
// #define CV_FUNCNAME( Name )
// #define cvFuncName ""
// #else
// #define CV_FUNCNAME( Name )  \
// static char cvFuncName[] = Name
// #endif

(*  
 CV_ERROR macro unconditionally raises error with passed code and message.
 After raising error, control will be transferred to the exit label.
 *)
 { TODO:CV_ERROR }
// #define CV_ERROR( Code, Msg )\
// {                            \
// cvError( (Code), cvFuncName, Msg, __FILE__, __LINE__ );        \
// __CV_EXIT__;            \
// }

(*  
 CV_CHECK macro checks error status after CV (or IPL)
 function call. If error detected, control will be transferred to the exit
 label.
 *)
 { TODO:CV_CHECK }
// #define CV_CHECK()           \
// {                            \
// if( cvGetErrStatus() < 0 )                                      \
// CV_ERROR( CV_StsBackTrace, "Inner function failed." );      \
// }

(*  
 CV_CALL macro calls CV (or IPL) function, checks error status and
 signals a error if the function failed. Useful in "parent node"
 error processing mode
 *)
 { TODO:CV_CALL }
// #define CV_CALL( Func )      \
// {                            \
// Func;                    \
// CV_CHECK();              \
// }

(* Runtime assertion macro *)
{ TODO:CV_ASSERT }
// #define CV_ASSERT( Condition )   \
// {                                \
// if( !(Condition) )           \
// CV_ERROR( CV_StsInternal, "Assertion: " #Condition " failed" ); \
// }

// #define __CV_BEGIN__       {
// #define __CV_END__         goto exit; exit: ; }
// #define __CV_EXIT__        goto exit

(* @} core_c *)

{$IF not defined(OPENCV_CORE_UTILITY_H)}{$I 'core/utility.inc'}{$IFEND}
// namespace cv
// {

// ! @addtogroup core_c_glue
// ! @{

/// //////////////////////////////////////// glue ///////////////////////////////////////////

// ! converts array (CvMat or IplImage) to cv::Mat
// CV_EXPORTS Mat cvArrToMat(const CvArr* arr, bool copyData=false,
// bool allowND=true, int coiMode=0,
// AutoBuffer<double>* buf=0);
 type
 AutoBuffer_of_double = AutoBuffer<double>;
 pAutoBuffer_of_double = ^AutoBuffer_of_double;

 function cvArrToMat(const arr: pCvArr; copyData: bool = false; allowND: bool = true; coiMode: int = 0;
 buf: pAutoBuffer_of_double = nil): TMat; {$IFDEF USE_INLINE}inline; {$ENDIF}
// static inline Mat cvarrToMatND(const CvArr* arr, bool copyData=false, int coiMode=0)
// {
// return cvarrToMat(arr, copyData, true, coiMode);
// }
 function cvArrToMatND(const arr: pCvArr; copyData: bool = false; coiMode: int = 0): TMat;
 {$IFDEF USE_INLINE}inline; {$ENDIF}
// ! extracts Channel of Interest from CvMat or IplImage and makes cv::Mat out of it.
// CV_EXPORTS void extractImageCOI(const CvArr* arr, OutputArray coiimg, int coi=-1);
 procedure extractImageCOI(const arr: pCvArr; coiimg: TOutputArray; coi: int = -1); {$IFDEF USE_INLINE}inline;
 {$ENDIF}
// ! inserts single-channel cv::Mat into a multi-channel CvMat or IplImage
// CV_EXPORTS void insertImageCOI(InputArray coiimg, CvArr* arr, int coi=-1);
 procedure insertImageCOI(const coiimg: TInputArray; arr: pCvArr; coi: int = -1); {$IFDEF USE_INLINE}inline;
 {$ENDIF}
/// /// specialized implementations of DefaultDeleter::operator() for classic OpenCV types //////

{ TODO:DefaultDeleter }
// template<> struct DefaultDeleter<CvMat>{ CV_EXPORTS void operator ()(CvMat* obj) const; };
// template<> struct DefaultDeleter<IplImage>{ CV_EXPORTS void operator ()(IplImage* obj) const; };
// template<> struct DefaultDeleter<CvMatND>{ CV_EXPORTS void operator ()(CvMatND* obj) const; };
// template<> struct DefaultDeleter<CvSparseMat>{ CV_EXPORTS void operator ()(CvSparseMat* obj) const; };
// template<> struct DefaultDeleter<CvMemStorage>{ CV_EXPORTS void operator ()(CvMemStorage* obj) const; };

/// /////////// convenient wrappers for operating old-style dynamic structures //////////////
{ TODO:SeqIterator }
// template<typename _Tp> class SeqIterator;

{ TODO:MemStorage }
// typedef Ptr<CvMemStorage> MemStorage;

(* !
 Template Sequence Class derived from CvSeq

 The class provides more convenient access to sequence elements,
 STL-style operations and iterators.

 \note The class is targeted for simple data types,
    i.e. no constructors or destructors
    are called for the sequence elements.
*)
{ TODO:Seq }
// template<typename _Tp> class Seq
{
public:
    typedef SeqIterator<_Tp> iterator;
    typedef SeqIterator<_Tp> const_iterator;

    //! the default constructor
    Seq();
    //! the constructor for wrapping CvSeq structure. The real element type in CvSeq should match _Tp.
    Seq(const CvSeq* seq);
    //! creates the empty sequence that resides in the specified storage
    Seq(MemStorage& storage, int headerSize = sizeof(CvSeq));
    //! returns read-write reference to the specified element
    _Tp& operator [](int idx);
    //! returns read-only reference to the specified element
    const _Tp& operator[](int idx) const;
    //! returns iterator pointing to the beginning of the sequence
    SeqIterator<_Tp> begin() const;
    //! returns iterator pointing to the element following the last sequence element
    SeqIterator<_Tp> end() const;
    //! returns the number of elements in the sequence
    size_t size() const;
    //! returns the type of sequence elements (CV_8UC1 ... CV_64FC(CV_CN_MAX) ...)
    int type() const;
    //! returns the depth of sequence elements (CV_8U ... CV_64F)
    int depth() const;
    //! returns the number of channels in each sequence element
    int channels() const;
    //! returns the size of each sequence element
    size_t elemSize() const;
    //! returns index of the specified sequence element
    size_t index(const _Tp& elem) const;
    //! appends the specified element to the end of the sequence
    void push_back(const _Tp& elem);
    //! appends the specified element to the front of the sequence
    void push_front(const _Tp& elem);
    //! appends zero or more elements to the end of the sequence
    void push_back(const _Tp* elems, size_t count);
    //! appends zero or more elements to the front of the sequence
    void push_front(const _Tp* elems, size_t count);
    //! inserts the specified element to the specified position
    void insert(int idx, const _Tp& elem);
    //! inserts zero or more elements to the specified position
    void insert(int idx, const _Tp* elems, size_t count);
    //! removes element at the specified position
    void remove(int idx);
    //! removes the specified subsequence
    void remove(const Range& r);

    //! returns reference to the first sequence element
    _Tp& front();
    //! returns read-only reference to the first sequence element
    const _Tp& front() const;
    //! returns reference to the last sequence element
    _Tp& back();
    //! returns read-only reference to the last sequence element
    const _Tp& back() const;
    //! returns true iff the sequence contains no elements
    bool empty() const;

    //! removes all the elements from the sequence
    void clear();
    //! removes the first element from the sequence
    void pop_front();
    //! removes the last element from the sequence
    void pop_back();
    //! removes zero or more elements from the beginning of the sequence
    void pop_front(_Tp* elems, size_t count);
    //! removes zero or more elements from the end of the sequence
    void pop_back(_Tp* elems, size_t count);

    //! copies the whole sequence or the sequence slice to the specified vector
    void copyTo(std::vector<_Tp>& vec, const Range& range=Range::all()) const;
    //! returns the vector containing all the sequence elements
    operator std::vector<_Tp>() const;

    CvSeq* seq;
}
// ;

(* !
 STL-style Sequence Iterator inherited from the CvSeqReader structure
*)
{ TODO:CvSeqReader }
// template<typename _Tp> class SeqIterator : public CvSeqReader
{
public:
    //! the default constructor
    SeqIterator();
    //! the constructor setting the iterator to the beginning or to the end of the sequence
    SeqIterator(const Seq<_Tp>& seq, bool seekEnd=false);
    //! positions the iterator within the sequence
    void seek(size_t pos);
    //! reports the current iterator position
    size_t tell() const;
    //! returns reference to the current sequence element
    _Tp& operator *();
    //! returns read-only reference to the current sequence element
    const _Tp& operator *() const;
    //! moves iterator to the next sequence element
    SeqIterator& operator ++();
    //! moves iterator to the next sequence element
    SeqIterator operator ++(int) const;
    //! moves iterator to the previous sequence element
    SeqIterator& operator --();
    //! moves iterator to the previous sequence element
    SeqIterator operator --(int) const;

    //! moves iterator forward by the specified offset (possibly negative)
    SeqIterator& operator +=(int);
    //! moves iterator backward by the specified offset (possibly negative)
    SeqIterator& operator -=(int);

    // this is index of the current element module seq->total*2
    // (to distinguish between 0 and seq->total)
    int index;
}
// ;

// bridge C++ => C Seq API
// CV_EXPORTS schar*  seqPush( CvSeq* seq, const void* element=0);
 function seqPush(seq: pCvSeq; const element: pvoid = nil): pschar; {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_EXPORTS schar*  seqPushFront( CvSeq* seq, const void* element=0);
 function seqPushFront(seq: pCvSeq; const element: pvoid = nil): pschar; {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_EXPORTS void  seqPop( CvSeq* seq, void* element=0);
 procedure seqPop(seq: pCvSeq; element: pvoid = nil); {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_EXPORTS void  seqPopFront( CvSeq* seq, void* element=0);
 procedure seqPopFront(seq: pCvSeq; element: pvoid = nil); {$IFDEF USE_INLINE}inline; {$ENDIF}
{ TODO:seqPopMulti }
// CV_EXPORTS void  seqPopMulti( CvSeq* seq, void* elements,
// int count, int in_front=0 );
 procedure seqPopMulti(seq: pCvSeq; elements: pvoid; count: int; in_front: int = 0); {$IFDEF USE_INLINE}inline;
 {$ENDIF}
// CV_EXPORTS void  seqRemove( CvSeq* seq, int index );
 procedure seqRemove(seq: pCvSeq; index: int); {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_EXPORTS void  clearSeq( CvSeq* seq );
 procedure clearSeq(seq: pCvSeq); {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_EXPORTS schar*  getSeqElem( const CvSeq* seq, int index );
 function getSeqElem(const seq: pCvSeq; index: int): pschar; {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_EXPORTS void  seqRemoveSlice( CvSeq* seq, CvSlice slice );
 procedure seqRemoveSlice(seq: pCvSeq; slice: TCvSlice); {$IFDEF USE_INLINE}inline; {$ENDIF}
// CV_EXPORTS void  seqInsertSlice( CvSeq* seq, int before_index, const CvArr* from_arr );
 procedure seqInsertSlice(seq: pCvSeq; before_index: int; const from_arr: pCvArr); {$IFDEF USE_INLINE}inline;
 {$ENDIF}
{ TODO:Seq<_Tp> }
// template<typename _Tp> inline Seq<_Tp>::Seq() : seq(0) {}
// template<typename _Tp> inline Seq<_Tp>::Seq( const CvSeq* _seq ) : seq((CvSeq*)_seq)
// {
// CV_Assert(!_seq || _seq->elem_size == sizeof(_Tp));
// }

// template<typename _Tp> inline Seq<_Tp>::Seq( MemStorage& storage,
// int headerSize )
// {
// CV_Assert(headerSize >= (int)sizeof(CvSeq));
// seq = cvCreateSeq(DataType<_Tp>::type, headerSize, sizeof(_Tp), storage);
// }

// template<typename _Tp> inline _Tp& Seq<_Tp>::operator [](int idx)
{ return *(_Tp*)getSeqElem(seq, idx); }

// template<typename _Tp> inline const _Tp& Seq<_Tp>::operator [](int idx) const
{ return *(_Tp*)getSeqElem(seq, idx); }

// template<typename _Tp> inline SeqIterator<_Tp> Seq<_Tp>::begin() const
{ return SeqIterator<_Tp>(*this); }

// template<typename _Tp> inline SeqIterator<_Tp> Seq<_Tp>::end() const
{ return SeqIterator<_Tp>(*this, true); }

// template<typename _Tp> inline size_t Seq<_Tp>::size() const
{ return seq ? seq->total : 0; }

// template<typename _Tp> inline int Seq<_Tp>::type() const
{ return seq ? CV_MAT_TYPE(seq->flags) : 0; }

// template<typename _Tp> inline int Seq<_Tp>::depth() const
{ return seq ? CV_MAT_DEPTH(seq->flags) : 0; }

// template<typename _Tp> inline int Seq<_Tp>::channels() const
{ return seq ? CV_MAT_CN(seq->flags) : 0; }

// template<typename _Tp> inline size_t Seq<_Tp>::elemSize() const
{ return seq ? seq->elem_size : 0; }

// template<typename _Tp> inline size_t Seq<_Tp>::index(const _Tp& elem) const
{ return cvSeqElemIdx(seq, &elem); }

// template<typename _Tp> inline void Seq<_Tp>::push_back(const _Tp& elem)
{ cvSeqPush(seq, &elem); }

// template<typename _Tp> inline void Seq<_Tp>::push_front(const _Tp& elem)
{ cvSeqPushFront(seq, &elem); }

// template<typename _Tp> inline void Seq<_Tp>::push_back(const _Tp* elem, size_t count)
{ cvSeqPushMulti(seq, elem, (int)count, 0); }

// template<typename _Tp> inline void Seq<_Tp>::push_front(const _Tp* elem, size_t count)
{ cvSeqPushMulti(seq, elem, (int)count, 1); }

// template<typename _Tp> inline _Tp& Seq<_Tp>::back()
{ return *(_Tp*)getSeqElem(seq, -1); }

// template<typename _Tp> inline const _Tp& Seq<_Tp>::back() const
{ return *(const _Tp*)getSeqElem(seq, -1); }

// template<typename _Tp> inline _Tp& Seq<_Tp>::front()
{ return *(_Tp*)getSeqElem(seq, 0); }

// template<typename _Tp> inline const _Tp& Seq<_Tp>::front() const
{ return *(const _Tp*)getSeqElem(seq, 0); }

// template<typename _Tp> inline bool Seq<_Tp>::empty() const
{ return !seq || seq->total == 0; }

// template<typename _Tp> inline void Seq<_Tp>::clear()
{ if(seq) clearSeq(seq); }

// template<typename _Tp> inline void Seq<_Tp>::pop_back()
{ seqPop(seq); }

// template<typename _Tp> inline void Seq<_Tp>::pop_front()
{ seqPopFront(seq); }

// template<typename _Tp> inline void Seq<_Tp>::pop_back(_Tp* elem, size_t count)
{ seqPopMulti(seq, elem, (int)count, 0); }

// template<typename _Tp> inline void Seq<_Tp>::pop_front(_Tp* elem, size_t count)
{ seqPopMulti(seq, elem, (int)count, 1); }

// template<typename _Tp> inline void Seq<_Tp>::insert(int idx, const _Tp& elem)
{ seqInsert(seq, idx, &elem); }

// template<typename _Tp> inline void Seq<_Tp>::insert(int idx, const _Tp* elems, size_t count)
{
    CvMat m = cvMat(1, count, DataType<_Tp>::type, elems);
    seqInsertSlice(seq, idx, &m);
}

// template<typename _Tp> inline void Seq<_Tp>::remove(int idx)
{ seqRemove(seq, idx); }

// template<typename _Tp> inline void Seq<_Tp>::remove(const Range& r)
{ seqRemoveSlice(seq, cvSlice(r.start, r.end)); }

// template<typename _Tp> inline void Seq<_Tp>::copyTo(std::vector<_Tp>& vec, const Range& range) const
{
    size_t len = !seq ? 0 : range == Range::all() ? seq->total : range.end - range.start;
    vec.resize(len);
    if( seq && len )
        cvCvtSeqToArray(seq, &vec[0], cvSlice(range));
}

// template<typename _Tp> inline Seq<_Tp>::operator std::vector<_Tp>() const
{
    std::vector<_Tp> vec;
    copyTo(vec);
    return vec;
}

// template<typename _Tp> inline SeqIterator<_Tp>::SeqIterator()
{ memset(this, 0, sizeof(*this)); }

// template<typename _Tp> inline SeqIterator<_Tp>::SeqIterator(const Seq<_Tp>& _seq, bool seekEnd)
{
    cvStartReadSeq(_seq.seq, this);
    index = seekEnd ? _seq.seq->total : 0;
}

// template<typename _Tp> inline void SeqIterator<_Tp>::seek(size_t pos)
{
    cvSetSeqReaderPos(this, (int)pos, false);
    index = pos;
}

// template<typename _Tp> inline size_t SeqIterator<_Tp>::tell() const
{ return index; }

// template<typename _Tp> inline _Tp& SeqIterator<_Tp>::operator *()
{ return *(_Tp*)ptr; }

// template<typename _Tp> inline const _Tp& SeqIterator<_Tp>::operator *() const
{ return *(const _Tp*)ptr; }

// template<typename _Tp> inline SeqIterator<_Tp>& SeqIterator<_Tp>::operator ++()
{
    CV_NEXT_SEQ_ELEM(sizeof(_Tp), *this);
    if( ++index >= seq->total*2 )
        index = 0;
    return *this;
}

// template<typename _Tp> inline SeqIterator<_Tp> SeqIterator<_Tp>::operator ++(int) const
{
    SeqIterator<_Tp> it = *this;
    ++*this;
    return it;
}

// template<typename _Tp> inline SeqIterator<_Tp>& SeqIterator<_Tp>::operator --()
{
    CV_PREV_SEQ_ELEM(sizeof(_Tp), *this);
    if( --index < 0 )
        index = seq->total*2-1;
    return *this;
}

// template<typename _Tp> inline SeqIterator<_Tp> SeqIterator<_Tp>::operator --(int) const
{
    SeqIterator<_Tp> it = *this;
    --*this;
    return it;
}

// template<typename _Tp> inline SeqIterator<_Tp>& SeqIterator<_Tp>::operator +=(int delta)
{
    cvSetSeqReaderPos(this, delta, 1);
    index += delta;
    int n = seq->total*2;
    if( index < 0 )
        index += n;
    if( index >= n )
        index -= n;
    return *this;
}

// template<typename _Tp> inline SeqIterator<_Tp>& SeqIterator<_Tp>::operator -=(int delta)
{
    return (*this += -delta);
}

// template<typename _Tp> inline ptrdiff_t operator - (const SeqIterator<_Tp>& a,
// const SeqIterator<_Tp>& b)
{
    ptrdiff_t delta = a.index - b.index, n = a.seq->total;
    if( delta > n || delta < -n )
        delta += delta < 0 ? n : -n;
    return delta;
}

// template<typename _Tp> inline bool operator == (const SeqIterator<_Tp>& a,
// const SeqIterator<_Tp>& b)
{
    return a.seq == b.seq && a.index == b.index;
}

// template<typename _Tp> inline bool operator != (const SeqIterator<_Tp>& a,
// const SeqIterator<_Tp>& b)
{
    return !(a == b);
}

// ! @}

// } // cv

// #endif

{$IFEND} // OPENCV_CORE_C_H

{$ENDIF INTERFACE}
// ===========================================================================

{$IFDEF IMPLEMENTATION}
{$IF defined(OPENCV_CORE_C_H) and (not defined(OPENCV_CORE_C_H_IMPL))}
{$DEFINE  OPENCV_CORE_C_H_IMPL}
{$IF not defined(OPENCV_CORE_TYPES_H_IMPL)}{$I 'core/types_c.inc'}{$IFEND}
 procedure cvDecRefData(arr: pCvArr);
 begin
 if (CV_IS_MAT(arr)) then
 begin
 var
 mat: pCvMat   := pCvMat(arr);
 mat^.data.ptr := nil;
 if (mat^.refcount <> nil) then
 begin
 Dec(mat^.refcount^);
 if (mat^.refcount^ = 0) then
 cvFree(mat^.refcount);
 end;
 mat^.refcount := nil;
 end
 else if (CV_IS_MATND(arr)) then
 begin
 var
 mat: pCvMatND := pCvMatND(arr);
 mat^.data.ptr := nil;
 if (mat^.refcount <> nil) then
 begin
 Dec(mat^.refcount^);
 if (mat^.refcount^ = 0) then
 cvFree(mat^.refcount);
 end;
 mat^.refcount := nil;
 end;
 end;

 function cvIncRefData(arr: pCvArr): int; inline;
 begin
 var
 refcount: int := 0;
 if (CV_IS_MAT(arr)) then
 begin
 var
 mat: pCvMat := pCvMat(arr);
 if (mat^.refcount <> nil) then
 begin
 inc(mat^.refcount^);
 refcount := mat^.refcount^;
 end;
 end
 else if (CV_IS_MATND(arr)) then
 begin
 var
 mat: pCvMatND := pCvMatND(arr);
 if (mat^.refcount <> nil) then
 begin
 inc(mat^.refcount^);
 refcount := mat^.refcount^;
 end;
 end;
 result := refcount;
 end;

 function cvGetRow(const arr: pCvArr; submat: pCvMat; row: int): pCvMat;
 begin
 result := cvGetRows(arr, submat, row, row + 1, 1);
 end;

 function cvGetCol(const arr: pCvArr; submat: pCvMat; col: int): pCvMat;
 begin
 result := cvGetCols(arr, submat, col, col + 1);
 end;

 procedure cvReleaseMatND(var mat: pCvMatND);
 begin
 cvReleaseMat(pCvMat(mat));
 end;

 function cvGetNextSparseNode(mat_iterator: pCvSparseMatIterator): pCvSparseNode;
 type
 pArrayOfPoiner = ^TArrayOfPoiner;
 TArrayOfPoiner = array [0 .. 0] of pCvSparseNode;
 begin
 if (mat_iterator^.node^.next <> nil) then
 begin
 mat_iterator^.node := mat_iterator^.node^.next;
 Exit(mat_iterator^.node);
 end
 else
 begin
 var
 idx: int;
 inc(mat_iterator^.curidx);
 for idx := mat_iterator^.curidx to mat_iterator^.mat^.hashsize - 1 do
 begin
 var
 node: pCvSparseNode := pArrayOfPoiner(mat_iterator^.mat^.hashtable)[idx];
 if (node <> nil) then
 begin
 mat_iterator^.curidx := idx;
 mat_iterator^.node := node;
 Exit(mat_iterator^.node);
 end;
 end;
 result := nil;
 end;
 end;

// #define cvReshapeND( arr, header, new_cn, new_dims, new_sizes )   \
// cvReshapeMatND( (arr), sizeof( *(header)), (header),         \
// (new_cn), (new_dims), (new_sizes))
 function cvReshapeND(const arr: pCvArr; header: pCvArr; new_cn, new_dims: int; new_sizes: pint): pCvArr;
 begin
 result := cvReshapeMatND(arr, sizeof(pCvMat(header)^), header, new_cn, new_dims, new_sizes);
 end;

 procedure cvConvert(const src: pCvArr; dst: pCvArr);
 begin
 cvConvertScale(src, dst, 1, 0);
 end;

 procedure cvSubS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
 begin
 cvAddS(src, cvScalar(-value.val[0], -value.val[1], -value.val[2], -value.val[3]), dst, mask);
 end;

 procedure cvSetIdentity(mat: pCvArr);
 begin
 cvSetIdentity(mat, cvRealScalar(1));
 end;

 function cvCloneSeq(const seq: pCvSeq; storage: pCvMemStorage = nil): pCvSeq;
 begin
 result := cvSeqSlice(seq, CV_WHOLE_SEQ, storage, 1);
 end;

 function cvCvtSeqToArray(const seq: pCvSeq; elements: pvoid): pointer;
 begin
 result := cvCvtSeqToArray(seq, elements, CV_WHOLE_SEQ);
 end;

 function cvSetNew(set_header: pCvSet): pCvSetElem;
 begin
 var
 elem: pCvSetElem := set_header^.free_elems;
 if (elem <> nil) then
 begin
 set_header^.free_elems := elem^.next_free;
 elem^.flags := elem^.flags and CV_SET_ELEM_IDX_MASK;
 inc(set_header^.active_count);
 end
 else
 cvSetAdd(set_header, nil, elem);
 result := elem;
 end;

 procedure cvSetRemoveByPtr(set_header: pCvSet; elem: pvoid);
 begin
 var
 _elem: pCvSetElem := pCvSetElem(elem);
 assert(_elem^.flags >= 0 (* && (elem^.flags & CV_SET_ELEM_IDX_MASK) < set_header^.total *) );
 _elem^.next_free := set_header^.free_elems;
 _elem^.flags     := (_elem^.flags and CV_SET_ELEM_IDX_MASK) or CV_SET_ELEM_FREE_FLAG;
 set_header^.free_elems := _elem;
 Dec(set_header^.active_count);
 end;

 function cvGetSetElem(const set_header: pCvSet; idx: int): pCvSetElem;
 begin
 var
 elem: pCvSetElem := pCvSetElem(cvGetSeqElem(pCvSeq(set_header), idx));
 if Assigned(elem) and CV_IS_SET_ELEM(pCvArr(elem)) then
 result := elem
 else
 result := nil;
 end;

 function cvArrToMat(const arr: pCvArr; copyData: bool = false; allowND: bool = true; coiMode: int = 0;
 buf: pAutoBuffer_of_double = nil): TMat;
 begin
{$IF not defined(PACKAGE)}
 result := func_cvarrToMat(pvoid(arr), copyData, allowND, coiMode, buf);
{$IFEND}
 end;

 function cvArrToMatND(const arr: pCvArr; copyData: bool = false; coiMode: int = 0): TMat;
 begin
 result := cvArrToMat(arr, copyData, true, coiMode);
 end;

 procedure extractImageCOI(const arr: pCvArr; coiimg: TOutputArray; coi: int = -1);
 begin
{$IF not defined(PACKAGE)}
 proc_extractImageCOI(arr, coiimg, coi);
{$IFEND}
 end;

 procedure insertImageCOI(const coiimg: TInputArray; arr: pCvArr; coi: int = -1);
 begin
{$IF not defined(PACKAGE)}
 proc_insertImageCOI(coiimg, arr, coi);
{$IFEND}
 end;

 function seqPush(seq: pCvSeq; const element: pvoid = nil): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := func_seqPush(seq, element);
{$IFEND}
 end;

 function seqPushFront(seq: pCvSeq; const element: pvoid = nil): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := func_seqPushFront(seq, element);
{$IFEND}
 end;

 procedure seqPop(seq: pCvSeq; element: pvoid = nil);
 begin
{$IF not defined(PACKAGE)}
 proc_seqPop(seq, element);
{$IFEND}
 end;

 procedure seqPopFront(seq: pCvSeq; element: pvoid = nil);
 begin
{$IF not defined(PACKAGE)}
 proc_seqPopFront(seq, element);
{$IFEND}
 end;

 procedure seqPopMulti(seq: pCvSeq; elements: pvoid; count: int; in_front: int = 0);
 begin
  { TODO:seqPopMulti }
 end;

 procedure seqRemove(seq: pCvSeq; index: int);
 begin
{$IF not defined(PACKAGE)}
 proc_seqRemove(seq, index);
{$IFEND}
 end;

 procedure clearSeq(seq: pCvSeq);
 begin
{$IF not defined(PACKAGE)}
 proc_clearSeq(seq);
{$IFEND}
 end;

 function getSeqElem(const seq: pCvSeq; index: int): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := func_getSeqElem(seq, index);
{$IFEND}
 end;

 procedure seqRemoveSlice(seq: pCvSeq; slice: TCvSlice);
 begin
{$IF not defined(PACKAGE)}
 proc_seqRemoveSlice(seq, slice);
{$IFEND}
 end;

 procedure seqInsertSlice(seq: pCvSeq; before_index: int; const from_arr: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 proc_seqInsertSlice(seq, before_index, from_arr);
{$IFEND}
 end;

 function cvAlloc(size: size_t): pointer;
 begin
{$IF not defined(PACKAGE)}
 result := _cvAlloc(size);
{$IFEND}
 end;

 procedure cvFree(ptr: pointer);
 begin
{$IF not defined(PACKAGE)}
 _cvFree(ptr);
{$IFEND}
 end;

 function cvCreateImageHeader(size: TCvSize; depth, channels: int): pIplImage;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateImageHeader(size, depth, channels);
{$IFEND}
 end;

 function cvInitImageHeader(var image: TIplImage; size: TCvSize; depth: int; channels: int; origin: int = 0;
 align: int = 4): pIplImage;
 begin
{$IF not defined(PACKAGE)}
 result := _cvInitImageHeader(image, size, depth, channels, origin, align);
{$IFEND}
 end;

 function cvCreateImage(size: TCvSize; depth, channels: int): pIplImage;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateImage(size, depth, channels);
{$IFEND}
 end;

 procedure cvReleaseImageHeader(var image: pIplImage);
 begin
{$IF not defined(PACKAGE)}
 _cvReleaseImageHeader(image);
{$IFEND}
 end;

 procedure cvReleaseImage(var image: pIplImage);
 begin
{$IF not defined(PACKAGE)}
 _cvReleaseImage(image);
{$IFEND}
 end;

 function cvCloneImage(const image: pIplImage): pIplImage;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCloneImage(image);
{$IFEND}
 end;

 procedure cvSetImageCOI(var image: TIplImage; coi: int);
 begin
{$IF not defined(PACKAGE)}
 _cvSetImageCOI(image, coi);
{$IFEND}
 end;

 function cvGetImageCOI(const image: pIplImage): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetImageCOI(image);
{$IFEND}
 end;

 procedure cvSetImageROI(var image: TIplImage; rect: TCvRect);
 begin
{$IF not defined(PACKAGE)}
 _cvSetImageROI(image, rect);
{$IFEND}
 end;

 procedure cvResetImageROI(var image: TIplImage);
 begin
{$IF not defined(PACKAGE)}
 _cvResetImageROI(image);
{$IFEND}
 end;

 function cvGetImageROI(const image: pIplImage): TCvRect;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetImageROI(image);
{$IFEND}
 end;

 function cvCreateMatHeader(rows, cols, &type: int): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateMatHeader(rows, cols, &type);
{$IFEND}
 end;

 function cvInitMatHeader(var mat: TCvMat; rows, cols, &type: int; data: pointer = nil;
 step: int = CV_AUTOSTEP): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvInitMatHeader(mat, rows, cols, &type, data, step);
{$IFEND}
 end;

 function cvCreateMat(rows, cols, &type: int): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateMat(rows, cols, &type);
{$IFEND}
 end;

 procedure cvReleaseMat(var mat: pCvMat);
 begin
{$IF not defined(PACKAGE)}
 _cvReleaseMat(mat);
{$IFEND}
 end;

 function cvCloneMat(const mat: pCvMat): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCloneMat(mat);
{$IFEND}
 end;

 function cvGetSubRect(const arr: pCvArr; submat: pCvMat; rect: TCvRect): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetSubRect(arr, submat, rect);
{$IFEND}
 end;

 function cvGetSubArr(const arr: pCvArr; submat: pCvMat; rect: TCvRect): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetSubArr(arr, submat, rect);
{$IFEND}
 end;

 function cvGetRows(const arr: pCvArr; submat: pCvMat; start_row, end_row: int; delta_row: int = 1): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetRows(arr, submat, start_row, end_row, delta_row);
{$IFEND}
 end;

 function cvGetCols(const arr: pCvArr; submat: pCvMat; start_col, end_col: int): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetCols(arr, submat, start_col, end_col);
{$IFEND}
 end;

 function cvGetDiag(const arr: pCvArr; submat: pCvMat; diag: int = 0): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetDiag(arr, submat, diag);
{$IFEND}
 end;

 procedure cvScalarToRawData(const scalar: pCvScalar; data: pointer; &type: int; extend_to_12: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvScalarToRawData(scalar, data, &type, extend_to_12);
{$IFEND}
 end;

 procedure cvRawDataToScalar(const data: pointer; &type: int; scalar: pCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvRawDataToScalar(data, &type, scalar);
{$IFEND}
 end;

 function cvCreateMatNDHeader(dims: int; const sizes: pint; &type: int): pCvMatND;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateMatNDHeader(dims, sizes, &type);
{$IFEND}
 end;

 function cvCreateMatND(dims: int; const sizes: pint; &type: int): pCvMatND;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateMatND(dims, sizes, &type);
{$IFEND}
 end;

 function cvInitMatNDHeader(mat: pCvMatND; dims: int; const sizes: pint; &type: int; data: pointer = nil)
 : pCvMatND;
 begin
{$IF not defined(PACKAGE)}
 result := _cvInitMatNDHeader(mat, dims, sizes, &type, data);
{$IFEND}
 end;

 function cvCloneMatND(const mat: pCvMatND): pCvMatND;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCloneMatND(mat);
{$IFEND}
 end;

 function cvCreateSparseMat(dims: int; const sizes: pint; &type: int): pCvSparseMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateSparseMat(dims, sizes, &type);
{$IFEND}
 end;

 procedure cvReleaseSparseMat(var mat: pCvSparseMat);
 begin
{$IF not defined(PACKAGE)}
 _cvReleaseSparseMat(mat);
{$IFEND}
 end;

 function cvCloneSparseMat(const mat: pCvSparseMat): pCvSparseMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCloneSparseMat(mat);
{$IFEND}
 end;

 function cvInitSparseMatIterator(const mat: pCvSparseMat; mat_iterator: pCvSparseMatIterator): pCvSparseNode;
 begin
{$IF not defined(PACKAGE)}
 result := _cvInitSparseMatIterator(mat, mat_iterator);
{$IFEND}
 end;

 function cvInitNArrayIterator(count: int; var arrs: pCvArr; const mask: pCvArr; stubs: pCvMatND;
 array_iterator: pCvNArrayIterator; flags: int = 0): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvInitNArrayIterator(count, arrs, mask, stubs, array_iterator, flags);
{$IFEND}
 end;

 function cvNextNArraySlice(array_iterator: pCvNArrayIterator): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvNextNArraySlice(array_iterator);
{$IFEND}
 end;

 function cvGetElemType(const arr: pCvArr): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetElemType(arr);
{$IFEND}
 end;

 function cvGetDims(const arr: pCvArr; sizes: pint = nil): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetDims(arr, sizes);
{$IFEND}
 end;

 function cvGetDimSize(const arr: pCvArr; index: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetDimSize(arr, index);
{$IFEND}
 end;

 function cvPtr1D(const arr: pCvArr; idx0: int; &type: pint = nil): puchar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvPtr1D(arr, idx0, &type);
{$IFEND}
 end;

 function cvPtr2D(const arr: pCvArr; idx0, idx1: int; &type: pint = nil): puchar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvPtr2D(arr, idx0, idx1, &type);
{$IFEND}
 end;

 function cvPtr3D(const arr: pCvArr; idx0, idx1, idx2: int; &type: pint = nil): puchar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvPtr3D(arr, idx0, idx1, idx2, &type);
{$IFEND}
 end;

 function cvPtrND(const arr: pCvArr; const idx: pint; &type: pint = nil; create_node: int = 1;
 precalc_hashval: punsigned = nil): puchar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvPtrND(arr, idx, &type, create_node, precalc_hashval);
{$IFEND}
 end;

 function cvGet1D(const arr: pCvArr; idx0: int): TCvScalar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGet1D(arr, idx0);
{$IFEND}
 end;

 function cvGet2D(const arr: pCvArr; idx0, idx1: int): TCvScalar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGet2D(arr, idx0, idx1);
{$IFEND}
 end;

 function cvGet3D(const arr: pCvArr; idx0, idx1, idx2: int): TCvScalar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGet3D(arr, idx0, idx1, idx2);
{$IFEND}
 end;

 function cvGetND(const arr: pCvArr; const idx: pint): TCvScalar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetND(arr, idx);
{$IFEND}
 end;

 function cvGetReal1D(const arr: pCvArr; idx0: int): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetReal1D(arr, idx0);
{$IFEND}
 end;

 function cvGetReal2D(const arr: pCvArr; idx0, idx1: int): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetReal2D(arr, idx0, idx1);
{$IFEND}
 end;

 function cvGetReal3D(const arr: pCvArr; idx0, idx1, idx2: int): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetReal3D(arr, idx0, idx1, idx2);
{$IFEND}
 end;

 function cvGetRealND(const arr: pCvArr; const idx: pint): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetRealND(arr, idx);
{$IFEND}
 end;

 procedure cvSet1D(arr: pCvArr; idx0: int; value: TCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvSet1D(arr, idx0, value);
{$IFEND}
 end;

 procedure cvSet2D(arr: pCvArr; idx0, idx1: int; value: TCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvSet2D(arr, idx0, idx1, value);
{$IFEND}
 end;

 procedure cvSet3D(arr: pCvArr; idx0, idx1, idx2: int; value: TCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvSet3D(arr, idx0, idx1, idx2, value);
{$IFEND}
 end;

 procedure cvSetND(arr: pCvArr; const idx: pint; value: TCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvSetND(arr, idx, value);
{$IFEND}
 end;

 procedure cvSetReal1D(arr: pCvArr; idx0: int; value: double);
 begin
{$IF not defined(PACKAGE)}
 _cvSetReal1D(arr, idx0, value);
{$IFEND}
 end;

 procedure cvSetReal2D(arr: pCvArr; idx0, idx1: int; value: double);
 begin
{$IF not defined(PACKAGE)}
 _cvSetReal2D(arr, idx0, idx1, value);
{$IFEND}
 end;

 procedure cvSetReal3D(arr: pCvArr; idx0, idx1, idx2: int; value: double);
 begin
{$IF not defined(PACKAGE)}
 _cvSetReal3D(arr, idx0, idx1, idx2, value);
{$IFEND}
 end;

 procedure cvSetRealND(arr: pCvArr; const idx: pint; value: double);
 begin
{$IF not defined(PACKAGE)}
 _cvSetRealND(arr, idx, value);
{$IFEND}
 end;

 procedure cvClearND(arr: pCvArr; const idx: pint);
 begin
{$IF not defined(PACKAGE)}
 _cvClearND(arr, idx);
{$IFEND}
 end;

 function cvGetMat(const arr: pCvArr; header: pCvMat; coi: pint = nil; allowND: int = 0): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetMat(arr, header, coi, allowND);
{$IFEND}
 end;

 function cvGetImage(const arr: pCvArr; image_header: pIplImage): pIplImage;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetImage(arr, image_header);
{$IFEND}
 end;

 function cvReshapeMatND(const arr: pCvArr; sizeof_header: int; header: pCvArr; new_cn, new_dims: int;
 new_sizes: pint): pCvArr;
 begin
{$IF not defined(PACKAGE)}
 result := _cvReshapeMatND(arr, sizeof_header, header, new_cn, new_dims, new_sizes);
{$IFEND}
 end;

 function cvReshape(const arr: pCvArr; header: pCvMat; new_cn: int; new_rows: int = 0): pCvMat;
 begin
{$IF not defined(PACKAGE)}
 result := _cvReshape(arr, header, new_cn, new_rows);
{$IFEND}
 end;

 procedure cvRepeat(const src: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvRepeat(src, dst);
{$IFEND}
 end;

 procedure cvCreateData(arr: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvCreateData(arr);
{$IFEND}
 end;

 procedure cvReleaseData(arr: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvReleaseData(arr);
{$IFEND}
 end;

 procedure cvSetData(arr: pCvArr; data: pointer; step: int);
 begin
{$IF not defined(PACKAGE)}
 _cvSetData(arr, data, step);
{$IFEND}
 end;

 procedure cvGetRawData(const arr: pCvArr; data: puchar; step: pint = nil; roi_size: pCvSize = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvGetRawData(arr, data, step, roi_size);
{$IFEND}
 end;

 function cvGetSize(const arr: pCvArr): TCvSize;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetSize(arr);
{$IFEND}
 end;

 procedure cvCopy(const src: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvCopy(src, dst, mask);
{$IFEND}
 end;

 procedure cvSet(arr: pCvArr; value: TCvScalar; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvSet_1(arr, value, mask);
{$IFEND}
 end;

 procedure cvSetZero(arr: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvSetZero(arr);
{$IFEND}
 end;

 procedure cvZero(arr: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvZero(arr);
{$IFEND}
 end;

 procedure cvSplit(const src: pCvArr; dst0: pCvArr; dst1: pCvArr; dst2: pCvArr; dst3: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvSplit(src, dst0, dst1, dst2, dst3);
{$IFEND}
 end;

 procedure cvMerge(const src0: pCvArr; const src1: pCvArr; const src2: pCvArr; const src3: pCvArr;
 dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvMerge(src0, src1, src2, src3, dst);
{$IFEND}
 end;

 procedure cvMixChannels(const src: pCvArr; src_count: int; dst: pCvArr; dst_count: int; const from_to: pint;
 pair_count: int);
 begin
{$IF not defined(PACKAGE)}
 _cvMixChannels(src, src_count, dst, dst_count, from_to, pair_count);
{$IFEND}
 end;

 procedure cvConvertScale(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvConvertScale(src, dst, scale, shift);
{$IFEND}
 end;

 procedure cvCvtScale(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvCvtScale(src, dst, scale, shift);
{$IFEND}
 end;

 procedure cvScale(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvScale(src, dst, scale, shift);
{$IFEND}
 end;

 procedure cvConvertScaleAbs(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvConvertScaleAbs(src, dst, scale, shift);
{$IFEND}
 end;

 procedure cvCvtScaleAbs(const src: pCvArr; dst: pCvArr; scale: double = 1; shift: double = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvCvtScaleAbs(src, dst, scale, shift);
{$IFEND}
 end;

 function cvCheckTermCriteria(criteria: TCvTermCriteria; default_eps: double; default_max_iters: int)
 : TCvTermCriteria;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCheckTermCriteria(criteria, default_eps, default_max_iters);
{$IFEND}
 end;

 procedure cvAdd(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvAdd(src1, src2, dst, mask);
{$IFEND}
 end;

 procedure cvAddS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvAddS(src, value, dst, mask);
{$IFEND}
 end;

 procedure cvSub(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvSub(src1, src2, dst, mask);
{$IFEND}
 end;

 procedure cvSubRS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvSubRS(src, value, dst, mask);
{$IFEND}
 end;

 procedure cvMul(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; scale: double = 1);
 begin
{$IF not defined(PACKAGE)}
 _cvMul(src1, src2, dst, scale);
{$IFEND}
 end;

 procedure cvDiv(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; scale: double = 1);
 begin
{$IF not defined(PACKAGE)}
 _cvDiv(src1, src2, dst, scale);
{$IFEND}
 end;

 procedure cvScaleAdd(const src1: pCvArr; scale: TCvScalar; const src2: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvScaleAdd(src1, scale, src2, dst);
{$IFEND}
 end;

 procedure cvAddWeighted(const src1: pCvArr; alpha: double; const src2: pCvArr; beta: double; gamma: double;
 dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvAddWeighted(src1, alpha, src2, beta, gamma, dst);
{$IFEND}
 end;

 function cvDotProduct(const src1: pCvArr; const src2: pCvArr): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvDotProduct(src1, src2);
{$IFEND}
 end;

 procedure cvAnd(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvAnd(src1, src2, dst, mask);
{$IFEND}
 end;

 procedure cvAndS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvAndS(src, value, dst, mask);
{$IFEND}
 end;

 procedure cvOr(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvOr(src1, src2, dst, mask);
{$IFEND}
 end;

 procedure cvOrS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvOrS(src, value, dst, mask);
{$IFEND}
 end;

 procedure cvXor(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvXor(src1, src2, dst, mask);
{$IFEND}
 end;

 procedure cvXorS(const src: pCvArr; value: TCvScalar; dst: pCvArr; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvXorS(src, value, dst, mask);
{$IFEND}
 end;

 procedure cvNot(const src: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvNot(src, dst);
{$IFEND}
 end;

 procedure cvInRange(const src: pCvArr; const lower: pCvArr; const upper: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvInRange(src, lower, upper, dst);
{$IFEND}
 end;

 procedure cvInRangeS(const src: pCvArr; lower: TCvScalar; upper: TCvScalar; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvInRangeS(src, lower, upper, dst);
{$IFEND}
 end;

 procedure cvCmp(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; cmp_op: int);
 begin
{$IF not defined(PACKAGE)}
 _cvCmp(src1, src2, dst, cmp_op);
{$IFEND}
 end;

 procedure cvCmpS(const src: pCvArr; value: double; dst: pCvArr; cmp_op: int);
 begin
{$IF not defined(PACKAGE)}
 _cvCmpS(src, value, dst, cmp_op);
{$IFEND}
 end;

 procedure cvMin(const src1: pCvArr; const src2: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvMin(src1, src2, dst);
{$IFEND}
 end;

 procedure cvMax(const src1: pCvArr; const src2: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvMax(src1, src2, dst);
{$IFEND}
 end;

 procedure cvMinS(const src: pCvArr; value: double; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvMinS(src, value, dst);
{$IFEND}
 end;

 procedure cvMaxS(const src: pCvArr; value: double; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvMaxS(src, value, dst);
{$IFEND}
 end;

 procedure cvAbsDiff(const src1: pCvArr; const src2: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvAbsDiff(src1, src2, dst);
{$IFEND}
 end;

 procedure cvAbsDiffS(const src: pCvArr; dst: pCvArr; value: TCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvAbsDiffS(src, dst, value);
{$IFEND}
 end;

 procedure cvCartToPolar(const x: pCvArr; const y: pCvArr; magnitude: pCvArr; angle: pCvArr = nil;
 angle_in_degrees: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvCartToPolar(x, y, magnitude, angle, angle_in_degrees);
{$IFEND}
 end;

 procedure cvPolarToCart(const magnitude: pCvArr; const angle: pCvArr; x: pCvArr; y: pCvArr;
 angle_in_degrees: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvPolarToCart(magnitude, angle, x, y, angle_in_degrees);
{$IFEND}
 end;

 procedure cvPow(const src: pCvArr; dst: pCvArr; power: double);
 begin
{$IF not defined(PACKAGE)}
 _cvPow(src, dst, power);
{$IFEND}
 end;

 procedure cvExp(const src: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvExp(src, dst);
{$IFEND}
 end;

 procedure cvLog(const src: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvLog(src, dst);
{$IFEND}
 end;

 function cvFastArctan(y: float; x: float): float;
 begin
{$IF not defined(PACKAGE)}
 result := _cvFastArctan(y, x);
{$IFEND}
 end;

 function cvCbrt(value: float): float;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCbrt(value);
{$IFEND}
 end;

 function cvCheckArr(const arr: pCvArr; flags: int = 0; min_val: double = 0; max_val: double = 0): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCheckArr(arr, flags, min_val, max_val);
{$IFEND}
 end;

 function cvCheckArray(const arr: pCvArr; flags: int = 0; min_val: double = 0; max_val: double = 0): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCheckArray(arr, flags, min_val, max_val);
{$IFEND}
 end;

 procedure cvRandArr(rng: pCvRNG; arr: pCvArr; dist_type: int; param1: TCvScalar; param2: TCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvRandArr(rng, arr, dist_type, param1, param2);
{$IFEND}
 end;

 procedure cvRandShuffle(mat: pCvArr; rng: pCvRNG; iter_factor: double = 1);
 begin
{$IF not defined(PACKAGE)}
 _cvRandShuffle(mat, rng, iter_factor);
{$IFEND}
 end;

 procedure cvSort(const src: pCvArr; dst: pCvArr = nil; idxmat: pCvArr = nil; flags: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvSort(src, dst, idxmat, flags);
{$IFEND}
 end;

 function cvSolveCubic(const coeffs: pCvMat; roots: pCvMat): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSolveCubic(coeffs, roots);
{$IFEND}
 end;

 procedure cvSolvePoly(const coeffs: pCvMat; roots2: pCvMat; maxiter: int = 20; fig: int = 100);
 begin
{$IF not defined(PACKAGE)}
 _cvSolvePoly(coeffs, roots2, maxiter, fig);
{$IFEND}
 end;

 procedure cvCrossProduct(const src1: pCvArr; const src2: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvCrossProduct(src1, src2, dst);
{$IFEND}
 end;

 procedure cvGEMM(const src1: pCvArr; const src2: pCvArr; alpha: double; const src3: pCvArr; beta: double;
 dst: pCvArr; tABC: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvGEMM(src1, src2, alpha, src3, beta, dst, tABC);
{$IFEND}
 end;

 procedure cvMatMulAddEx(const src1: pCvArr; const src2: pCvArr; alpha: double; const src3: pCvArr;
 beta: double; dst: pCvArr; tABC: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvGEMM(src1, src2, alpha, src3, beta, dst, tABC);
{$IFEND}
 end;

 procedure cvTransform(const src: pCvArr; dst: pCvArr; const transmat: pCvMat; const shiftvec: pCvMat = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvTransform(src, dst, transmat, shiftvec);
{$IFEND}
 end;

 procedure cvMatMulAddS(const src: pCvArr; dst: pCvArr; const transmat: pCvMat; const shiftvec: pCvMat = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvMatMulAddS(src, dst, transmat, shiftvec);
{$IFEND}
 end;

 procedure cvPerspectiveTransform(const src: pCvArr; dst: pCvArr; const mat: pCvMat);
 begin
{$IF not defined(PACKAGE)}
 _cvPerspectiveTransform(src, dst, mat);
{$IFEND}
 end;

 procedure cvMulTransposed(const src: pCvArr; dst: pCvArr; order: int; const delta: pCvArr = nil;
 scale: double = 1);
 begin
{$IF not defined(PACKAGE)}
 _cvMulTransposed(src, dst, order, delta, scale);
{$IFEND}
 end;

 procedure cvTranspose(const src: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvTranspose(src, dst);
{$IFEND}
 end;

 procedure cvT(const src: pCvArr; dst: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvT(src, dst);
{$IFEND}
 end;

 procedure cvCompleteSymm(matrix: pCvMat; LtoR: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvCompleteSymm(matrix, LtoR);
{$IFEND}
 end;

 procedure cvFlip(const src: pCvArr; dst: pCvArr = nil; flip_mode: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvFlip(src, dst, flip_mode);
{$IFEND}
 end;

 procedure cvMirror(const src: pCvArr; dst: pCvArr = nil; flip_mode: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvMirror(src, dst, flip_mode);
{$IFEND}
 end;

 procedure cvSVD(A: pCvArr; W: pCvArr; U: pCvArr = nil; V: pCvArr = nil; flags: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvSVD(A, W, U, V, flags);
{$IFEND}
 end;

 procedure cvSVBkSb(const W: pCvArr; const U: pCvArr; const V: pCvArr; const B: pCvArr; x: pCvArr; flags: int);
 begin
{$IF not defined(PACKAGE)}
 _cvSVBkSb(W, U, V, B, x, flags);
{$IFEND}
 end;

 function cvInvert(const src: pCvArr; dst: pCvArr; method: int = CV_LU): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvInvert(src, dst, method);
{$IFEND}
 end;

 function cvInv(const src: pCvArr; dst: pCvArr; method: int = CV_LU): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvInv(src, dst, method);
{$IFEND}
 end;

 function cvSolve(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; method: int = CV_LU): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSolve(src1, src2, dst, method);
{$IFEND}
 end;

 function cvDet(const mat: pCvArr): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvDet(mat);
{$IFEND}
 end;

 function cvTrace(const mat: pCvArr): TCvScalar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvTrace(mat);
{$IFEND}
 end;

 procedure cvEigenVV(mat: pCvArr; evects: pCvArr; evals: pCvArr; eps: double = 0; lowindex: int = -1;
 highindex: int = -1);
 begin
{$IF not defined(PACKAGE)}
 _cvEigenVV(mat, evects, evals, eps, lowindex, highindex);
{$IFEND}
 end;

 procedure cvSetIdentity(mat: pCvArr; value: TCvScalar);
 begin
{$IF not defined(PACKAGE)}
 _cvSetIdentity(mat, value);
{$IFEND}
 end;

 function cvRange(mat: pCvArr; start: double; &end: double): pCvArr;
 begin
{$IF not defined(PACKAGE)}
 result := _cvRange(mat, start, &end);
{$IFEND}
 end;

 procedure cvCalcCovarMatrix(const vects: pCvArr; count: int; cov_mat: pCvArr; avg: pCvArr; flags: int);
 begin
{$IF not defined(PACKAGE)}
 _cvCalcCovarMatrix(vects, count, cov_mat, avg, flags);
{$IFEND}
 end;

 procedure cvCalcPCA(const data: pCvArr; mean: pCvArr; eigenvals: pCvArr; eigenvects: pCvArr; flags: int);
 begin
{$IF not defined(PACKAGE)}
 _cvCalcPCA(data, mean, eigenvals, eigenvects, flags);
{$IFEND}
 end;

 procedure cvProjectPCA(const data: pCvArr; const mean: pCvArr; const eigenvects: pCvArr; result: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvProjectPCA(data, mean, eigenvects, result);
{$IFEND}
 end;

 procedure cvBackProjectPCA(const proj: pCvArr; const mean: pCvArr; const eigenvects: pCvArr; result: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvBackProjectPCA(proj, mean, eigenvects, result);
{$IFEND}
 end;

 function cvMahalanobis(const vec1: pCvArr; const vec2: pCvArr; const mat: pCvArr): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvMahalanobis(vec1, vec2, mat);
{$IFEND}
 end;

 function cvMahalonobis(const vec1: pCvArr; const vec2: pCvArr; const mat: pCvArr): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvMahalonobis(vec1, vec2, mat);
{$IFEND}
 end;

 function cvSum(const arr: pCvArr): TCvScalar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSum(arr);
{$IFEND}
 end;

 function cvCountNonZero(const arr: pCvArr): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCountNonZero(arr);
{$IFEND}
 end;

 function cvAvg(const arr: pCvArr; const mask: pCvArr = nil): TCvScalar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvAvg(arr, mask);
{$IFEND}
 end;

 procedure cvAvgSdv(const arr: pCvArr; mean: pCvScalar; std_dev: pCvScalar; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvAvgSdv(arr, mean, std_dev, mask);
{$IFEND}
 end;

 procedure cvMinMaxLoc(const arr: pCvArr; min_val: pdouble; max_val: pdouble; min_loc: pCvPoint = nil;
 max_loc: pCvPoint = nil; const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvMinMaxLoc(arr, min_val, max_val, min_loc, max_loc, mask);
{$IFEND}
 end;

 function cvNorm(const arr1: pCvArr; const arr2: pCvArr = nil; norm_type: int = CV_L2;
 const mask: pCvArr = nil): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvNorm(arr1, arr2, norm_type, mask);
{$IFEND}
 end;

 procedure cvNormalize(const src: pCvArr; dst: pCvArr; A: double = 1.; B: double = 0.; norm_type: int = CV_L2;
 const mask: pCvArr = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvNormalize(src, dst, A, B, norm_type, mask);
{$IFEND}
 end;

 procedure cvReduce(const src: pCvArr; dst: pCvArr; dim: int = -1; op: int = CV_REDUCE_SUM);
 begin
{$IF not defined(PACKAGE)}
 _cvReduce(src, dst, dim, op);
{$IFEND}
 end;

 procedure cvDFT(const src: pCvArr; dst: pCvArr; flags: int; nonzero_rows: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvDFT(src, dst, flags, nonzero_rows);
{$IFEND}
 end;

 procedure cvFFT(const src: pCvArr; dst: pCvArr; flags: int; nonzero_rows: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvFFT(src, dst, flags, nonzero_rows);
{$IFEND}
 end;

 procedure cvMulSpectrums(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; flags: int);
 begin
{$IF not defined(PACKAGE)}
 _cvMulSpectrums(src1, src2, dst, flags);
{$IFEND}
 end;

 function cvGetOptimalDFTSize(size0: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetOptimalDFTSize(size0);
{$IFEND}
 end;

 procedure cvDCT(const src: pCvArr; dst: pCvArr; flags: int);
 begin
{$IF not defined(PACKAGE)}
 _cvDCT(src, dst, flags);
{$IFEND}
 end;

 function cvSliceLength(slice: TCvSlice; const seq: pCvSeq): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSliceLength(slice, seq);
{$IFEND}
 end;

 function cvCreateMemStorage(block_size: int = 0): pCvMemStorage;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateMemStorage(block_size);
{$IFEND}
 end;

 function cvCreateChildMemStorage(parent: pCvMemStorage): pCvMemStorage;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateChildMemStorage(parent);
{$IFEND}
 end;

 procedure cvReleaseMemStorage(storage: pCvMemStorage);
 begin
{$IF not defined(PACKAGE)}
 _cvReleaseMemStorage(storage);
{$IFEND}
 end;

 procedure cvClearMemStorage(storage: pCvMemStorage);
 begin
{$IF not defined(PACKAGE)}
 _cvClearMemStorage(storage);
{$IFEND}
 end;

 procedure cvSaveMemStoragePos(const storage: pCvMemStorage; pos: pCvMemStoragePos);
 begin
{$IF not defined(PACKAGE)}
 _cvSaveMemStoragePos(storage, pos);
{$IFEND}
 end;

 procedure cvRestoreMemStoragePos(storage: pCvMemStorage; pos: pCvMemStoragePos);
 begin
{$IF not defined(PACKAGE)}
 _cvRestoreMemStoragePos(storage, pos);
{$IFEND}
 end;

 function cvMemStorageAlloc(storage: pCvMemStorage; size: size_t): pointer;
 begin
{$IF not defined(PACKAGE)}
 result := _cvMemStorageAlloc(storage, size);
{$IFEND}
 end;

 function cvCreateSeq(seq_flags: int; header_size: size_t; elem_size: size_t; storage: pCvMemStorage): pCvSeq;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateSeq(seq_flags, header_size, elem_size, storage);
{$IFEND}
 end;

 procedure cvSetSeqBlockSize(seq: pCvSeq; delta_elems: int);
 begin
{$IF not defined(PACKAGE)}
 _cvSetSeqBlockSize(seq, delta_elems);
{$IFEND}
 end;

 function cvSeqPush(seq: pCvSeq; const element: pvoid = nil): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSeqPush(seq, element);
{$IFEND}
 end;

 function cvSeqPushFront(seq: pCvSeq; const element: pvoid = nil): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSeqPushFront(seq, element);
{$IFEND}
 end;

 procedure cvSeqPop(seq: pCvSeq; element: pvoid = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqPop(seq, element);
{$IFEND}
 end;

 procedure cvSeqPopFront(seq: pCvSeq; element: pvoid = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqPopFront(seq, element);
{$IFEND}
 end;

 procedure cvSeqPushMulti(seq: pCvSeq; const elements: pvoid; count: int; in_front: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqPushMulti(seq, elements, count, in_front);
{$IFEND}
 end;

 procedure cvSeqPopMulti(seq: pCvSeq; elements: pvoid; count: int; in_front: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqPopMulti(seq, elements, count, in_front);
{$IFEND}
 end;

 function cvSeqInsert(seq: pCvSeq; before_index: int; const element: pvoid = nil): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSeqInsert(seq, before_index, element);
{$IFEND}
 end;

 procedure cvSeqRemove(seq: pCvSeq; index: int);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqRemove(seq, index);
{$IFEND}
 end;

 procedure cvClearSeq(seq: pCvSeq);
 begin
{$IF not defined(PACKAGE)}
 _cvClearSeq(seq);
{$IFEND}
 end;

 function cvGetSeqElem(const seq: pCvSeq; index: int): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetSeqElem(seq, index);
{$IFEND}
 end;

 function cvSeqElemIdx(const seq: pCvSeq; const element: pvoid; block: pCvSeqBlock = nil): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSeqElemIdx(seq, element, block);
{$IFEND}
 end;

 procedure cvStartAppendToSeq(seq: pCvSeq; writer: pCvSeqWriter);
 begin
{$IF not defined(PACKAGE)}
 _cvStartAppendToSeq(seq, writer);
{$IFEND}
 end;

 procedure cvStartWriteSeq(seq_flags: int; header_size: int; elem_size: int; storage: pCvMemStorage;
 writer: pCvSeqWriter);
 begin
{$IF not defined(PACKAGE)}
 _cvStartWriteSeq(seq_flags, header_size, elem_size, storage, writer);
{$IFEND}
 end;

 function cvEndWriteSeq(writer: pCvSeqWriter): pCvSeq;
 begin
{$IF not defined(PACKAGE)}
 result := _cvEndWriteSeq(writer);
{$IFEND}
 end;

 procedure cvFlushSeqWriter(writer: pCvSeqWriter);
 begin
{$IF not defined(PACKAGE)}
 _cvFlushSeqWriter(writer);
{$IFEND}
 end;

 procedure cvStartReadSeq(const seq: pCvSeq; reader: pCvSeqReader; reverse: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvStartReadSeq(seq, reader, reverse);
{$IFEND}
 end;

 function cvGetSeqReaderPos(reader: pCvSeqReader): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetSeqReaderPos(reader);
{$IFEND}
 end;

 procedure cvSetSeqReaderPos(reader: pCvSeqReader; index: int; is_relative: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvSetSeqReaderPos(reader, index, is_relative);
{$IFEND}
 end;

 function cvCvtSeqToArray(const seq: pCvSeq; elements: pvoid; slice: TCvSlice): pointer; overload;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCvtSeqToArray(seq, elements, slice);
{$IFEND}
 end;

 function cvMakeSeqHeaderForArray(seq_type: int; header_size: int; elem_size: int; elements: pvoid; total: int;
 seq: pCvSeq; block: pCvSeqBlock): pCvSeq;
 begin
{$IF not defined(PACKAGE)}
 result := _cvMakeSeqHeaderForArray(seq_type, header_size, elem_size, elements, total, seq, block);
{$IFEND}
 end;

 function cvSeqSlice(const seq: pCvSeq; slice: TCvSlice; storage: pCvMemStorage = nil;
 copy_data: int = 0): pCvSeq;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSeqSlice(seq, slice, storage, copy_data);
{$IFEND}
 end;

 procedure cvSeqRemoveSlice(seq: pCvSeq; slice: TCvSlice);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqRemoveSlice(seq, slice);
{$IFEND}
 end;

 procedure cvSeqInsertSlice(seq: pCvSeq; before_index: int; const from_arr: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqInsertSlice(seq, before_index, from_arr);
{$IFEND}
 end;

 procedure cvSeqSort(seq: pCvSeq; func: TCvCmpFunc; userdata: pvoid = nil);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqSort(seq, func, userdata);
{$IFEND}
 end;

 function cvSeqSearch(seq: pCvSeq; const elem: pvoid; func: TCvCmpFunc; is_sorted: int; elem_idx: pint;
 userdata: pvoid = nil): pschar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSeqSearch(seq, elem, func, is_sorted, elem_idx, userdata);
{$IFEND}
 end;

 procedure cvSeqInvert(seq: pCvSeq);
 begin
{$IF not defined(PACKAGE)}
 _cvSeqInvert(seq);
{$IFEND}
 end;

 function cvSeqPartition(const seq: pCvSeq; storage: pCvMemStorage; labels: pCvSeq; is_equal: TCvCmpFunc;
 userdata: pvoid): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSeqPartition(seq, storage, labels, is_equal, userdata);
{$IFEND}
 end;

 procedure cvChangeSeqBlock(reader: pvoid; direction: int);
 begin
{$IF not defined(PACKAGE)}
 _cvChangeSeqBlock(reader, direction);
{$IFEND}
 end;

 procedure cvCreateSeqBlock(writer: pCvSeqWriter);
 begin
{$IF not defined(PACKAGE)}
 _cvCreateSeqBlock(writer);
{$IFEND}
 end;

 function cvCreateSet(set_flags: int; header_size: int; elem_size: int; storage: pCvMemStorage): pCvSet;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateSet(set_flags, header_size, elem_size, storage);
{$IFEND}
 end;

 function cvSetAdd(set_header: pCvSet; elem: pCvSetElem = nil; inserted_elem: pCvSetElem = nil): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSetAdd(set_header, elem, inserted_elem);
{$IFEND}
 end;

 procedure cvSetRemove(set_header: pCvSet; index: int);
 begin
{$IF not defined(PACKAGE)}
 _cvSetRemove(set_header, index);
{$IFEND}
 end;

 procedure cvClearSet(set_header: pCvSet);
 begin
{$IF not defined(PACKAGE)}
 _cvClearSet(set_header);
{$IFEND}
 end;

 function cvCreateGraph(graph_flags: int; header_size: int; vtx_size: int; edge_size: int;
 storage: pCvMemStorage): pCvGraph;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateGraph(graph_flags, header_size, vtx_size, edge_size, storage);
{$IFEND}
 end;

 function cvGraphAddVtx(graph: pCvGraph; const vtx: pCvGraphVtx = nil; inserted_vtx: pCvGraphVtx = nil): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphAddVtx(graph, vtx, inserted_vtx);
{$IFEND}
 end;

 function cvGraphRemoveVtx(graph: pCvGraph; index: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphRemoveVtx(graph, index);
{$IFEND}
 end;

 function cvGraphRemoveVtxByPtr(graph: pCvGraph; vtx: pCvGraphVtx): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphRemoveVtxByPtr(graph, vtx);
{$IFEND}
 end;

 function cvGraphAddEdge(graph: pCvGraph; start_idx: int; end_idx: int; const edge: pCvGraphEdge = nil;
 inserted_edge: pCvGraphEdge = nil): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphAddEdge(graph, start_idx, end_idx, edge, inserted_edge);
{$IFEND}
 end;

 function cvGraphAddEdgeByPtr(graph: pCvGraph; start_vtx: pCvGraphVtx; end_vtx: pCvGraphVtx;
 const edge: pCvGraphEdge = nil; inserted_edge: pCvGraphEdge = nil): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphAddEdgeByPtr(graph, start_vtx, end_vtx, edge, inserted_edge);
{$IFEND}
 end;

 procedure cvGraphRemoveEdge(graph: pCvGraph; start_idx: int; end_idx: int);
 begin
{$IF not defined(PACKAGE)}
 _cvGraphRemoveEdge(graph, start_idx, end_idx);
{$IFEND}
 end;

 procedure cvGraphRemoveEdgeByPtr(graph: pCvGraph; start_vtx: pCvGraphVtx; end_vtx: pCvGraphVtx);
 begin
{$IF not defined(PACKAGE)}
 _cvGraphRemoveEdgeByPtr(graph, start_vtx, end_vtx);
{$IFEND}
 end;

 function cvFindGraphEdge(const graph: pCvGraph; start_idx: int; end_idx: int): pCvGraphEdge;
 begin
{$IF not defined(PACKAGE)}
 result := _cvFindGraphEdge(graph, start_idx, end_idx);
{$IFEND}
 end;

 function cvGraphFindEdge(const graph: pCvGraph; start_idx: int; end_idx: int): pCvGraphEdge;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphFindEdge(graph, start_idx, end_idx);
{$IFEND}
 end;

 function cvFindGraphEdgeByPtr(const graph: pCvGraph; const start_vtx: pCvGraphVtx; const end_vtx: pCvGraphVtx)
 : pCvGraphEdge;
 begin
{$IF not defined(PACKAGE)}
 result := _cvFindGraphEdgeByPtr(graph, start_vtx, end_vtx);
{$IFEND}
 end;

 function cvGraphFindEdgeByPtr(const graph: pCvGraph; const start_vtx: pCvGraphVtx; const end_vtx: pCvGraphVtx)
 : pCvGraphEdge;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphFindEdgeByPtr(graph, start_vtx, end_vtx);
{$IFEND}
 end;

 procedure cvClearGraph(graph: pCvGraph);
 begin
{$IF not defined(PACKAGE)}
 _cvClearGraph(graph);
{$IFEND}
 end;

 function cvGraphVtxDegree(const graph: pCvGraph; vtx_idx: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphVtxDegree(graph, vtx_idx);
{$IFEND}
 end;

 function cvGraphVtxDegreeByPtr(const graph: pCvGraph; const vtx: pCvGraphVtx): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGraphVtxDegreeByPtr(graph, vtx);
{$IFEND}
 end;

 function cvCreateGraphScanner(graph: pCvGraph; vtx: pCvGraphVtx = nil; mask: int = CV_GRAPH_ALL_ITEMS)
 : pCvGraphScanner;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCreateGraphScanner(graph, vtx, mask);
{$IFEND}
 end;

 procedure cvReleaseGraphScanner(scanner: pCvGraphScanner);
 begin
{$IF not defined(PACKAGE)}
 _cvReleaseGraphScanner(scanner);
{$IFEND}
 end;

 function cvNextGraphItem(scanner: pCvGraphScanner): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvNextGraphItem(scanner);
{$IFEND}
 end;

 function cvCloneGraph(const graph: pCvGraph; storage: pCvMemStorage): pCvGraph;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCloneGraph(graph, storage);
{$IFEND}
 end;

 procedure cvLUT(const src: pCvArr; dst: pCvArr; const lut: pCvArr);
 begin
{$IF not defined(PACKAGE)}
 _cvLUT(src, dst, lut);
{$IFEND}
 end;

 procedure cvInitTreeNodeIterator(tree_iterator: pCvTreeNodeIterator; const first: pvoid; max_level: int);
 begin
{$IF not defined(PACKAGE)}
 _cvInitTreeNodeIterator(tree_iterator, first, max_level);
{$IFEND}
 end;

 function cvNextTreeNode(tree_iterator: pCvTreeNodeIterator): pointer;
 begin
{$IF not defined(PACKAGE)}
 result := _cvNextTreeNode(tree_iterator);
{$IFEND}
 end;

 function cvPrevTreeNode(tree_iterator: pCvTreeNodeIterator): pointer;
 begin
{$IF not defined(PACKAGE)}
 result := _cvPrevTreeNode(tree_iterator);
{$IFEND}
 end;

 procedure cvInsertNodeIntoTree(node: pvoid; parent: pvoid; frame: pvoid);
 begin
{$IF not defined(PACKAGE)}
 _cvInsertNodeIntoTree(node, parent, frame);
{$IFEND}
 end;

 procedure cvRemoveNodeFromTree(node: pvoid; frame: pvoid);
 begin
{$IF not defined(PACKAGE)}
 _cvRemoveNodeFromTree(node, frame);
{$IFEND}
 end;

 function cvTreeToNodeSeq(const first: pvoid; header_size: int; storage: pCvMemStorage): pCvSeq;
 begin
{$IF not defined(PACKAGE)}
 result := _cvTreeToNodeSeq(first, header_size, storage);
{$IFEND}
 end;

 function cvKMeans2(const samples: pCvArr; cluster_count: int; labels: pCvArr; termcrit: TCvTermCriteria;
 attempts: int = 1; rng: pCvRNG = nil; flags: int = 0; _centers: pCvArr = nil;
 compactness: pdouble = nil): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvKMeans2(samples, cluster_count, labels, termcrit, attempts, rng, flags, _centers, compactness);
{$IFEND}
 end;

 function cvUseOptimized(on_off: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvUseOptimized(on_off);
{$IFEND}
 end;

 procedure cvSetIPLAllocators(create_header: TCv_iplCreateImageHeader; allocate_data: TCv_iplAllocateImageData;
 deallocate: TCv_iplDeallocate; create_roi: TCv_iplCreateROI; clone_image: TCv_iplCloneImage);
 begin
{$IF not defined(PACKAGE)}
 _cvSetIPLAllocators(create_header, allocate_data, deallocate, create_roi, clone_image);
{$IFEND}
 end;

 procedure cvRelease(struct_ptr: pvoid);
 begin
{$IF not defined(PACKAGE)}
 _cvRelease(struct_ptr);
{$IFEND}
 end;

 function cvClone(const struct_ptr: pvoid): pointer;
 begin
{$IF not defined(PACKAGE)}
 result := _cvClone(struct_ptr);
{$IFEND}
 end;

 function cvGetTickCount(): int64;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetTickCount();
{$IFEND}
 end;

 function cvGetTickFrequency(): double;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetTickFrequency();
{$IFEND}
 end;

 function cvCheckHardwareSupport(feature: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvCheckHardwareSupport(feature);
{$IFEND}
 end;

 function cvGetNumThreads(): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetNumThreads();
{$IFEND}
 end;

 procedure cvSetNumThreads(threads: int = 0);
 begin
{$IF not defined(PACKAGE)}
 _cvSetNumThreads(threads);
{$IFEND}
 end;

 function cvGetThreadNum(): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetThreadNum();
{$IFEND}
 end;

 function cvGetErrStatus(): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetErrStatus();
{$IFEND}
 end;

 procedure cvSetErrStatus(status: int);
 begin
{$IF not defined(PACKAGE)}
 _cvSetErrStatus(status);
{$IFEND}
 end;

 function cvGetErrMode(): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetErrMode();
{$IFEND}
 end;

 function cvSetErrMode(mode: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvSetErrMode(mode);
{$IFEND}
 end;

 procedure cvError(status: int; const func_name: pcvchar; const err_msg: pcvchar; const file_name: pcvchar;
 line: int);
 begin
{$IF not defined(PACKAGE)}
 _cvError(status, func_name, err_msg, file_name, line);
{$IFEND}
 end;

 function cvErrorStr(status: int): pcvchar;
 begin
{$IF not defined(PACKAGE)}
 result := _cvErrorStr(status);
{$IFEND}
 end;

 function cvGetErrInfo(const errcode_desc: pcvchar; const description: pcvchar; const filename: pcvchar;
 line: pint): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGetErrInfo(errcode_desc, description, filename, line);
{$IFEND}
 end;

 function cvErrorFromIppStatus(ipp_status: int): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvErrorFromIppStatus(ipp_status);
{$IFEND}
 end;

 function cvRedirectError(error_handler: TCvErrorCallback; userdata: pvoid = nil; prev_userdata: pvoid = nil)
 : TCvErrorCallback;
 begin
{$IF not defined(PACKAGE)}
 result := _cvRedirectError(error_handler, userdata, prev_userdata);
{$IFEND}
 end;

 function cvNulDevReport(status: int; const func_name: pcvchar; const err_msg: pcvchar;
 const file_name: pcvchar; line: int; userdata: pvoid): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvNulDevReport(status, func_name, err_msg, file_name, line, userdata);
{$IFEND}
 end;

 function cvStdErrReport(status: int; const func_name: pcvchar; const err_msg: pcvchar;
 const file_name: pcvchar; line: int; userdata: pvoid): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvStdErrReport(status, func_name, err_msg, file_name, line, userdata);
{$IFEND}
 end;

 function cvGuiBoxReport(status: int; const func_name: pcvchar; const err_msg: pcvchar;
 const file_name: pcvchar; line: int; userdata: pvoid): int;
 begin
{$IF not defined(PACKAGE)}
 result := _cvGuiBoxReport(status, func_name, err_msg, file_name, line, userdata);
{$IFEND}
 end;

{$IFEND}
{$ENDIF IMPLEMENTATION}
// ============================================================================

{$IFDEF INITIALIZATION}
{$ENDIF IINITIALIZATION}
