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

{$IF defined(OPENCV_CORE_TYPES_H) and (not defined(OPENCV_CORE_TYPES_H_IMPL))}
{$DEFINE OPENCV_CORE_TYPES_H_IMPL}
{$INCLUDE 'core/cvdef.impl.inc'}
{$INCLUDE 'core.impl.inc'}

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
  Result :=          //
    (mat <> nil) and //
    ((pCvMat(mat)^.&type and CV_MAGIC_MASK) = CV_MAT_MAGIC_VAL) and //
    ((pCvMat(mat)^.r2.cols > 0) and (pCvMat(mat)^.r1.rows > 0));
end;

function CV_IS_MAT(const mat: pCvArr): Boolean;
begin
  // #define CV_IS_MAT(mat) \
  // (CV_IS_MAT_HDR(mat) && ((const CvMat*)(mat))->data.ptr != NULL)
  Result := CV_IS_MAT_HDR(mat) and (pCvMat(mat)^.data.ptr <> nil);
end;

function CV_IS_MATND_HDR(const mat:pCvArr):Boolean;
begin
  //#define CV_IS_MATND_HDR(mat) \
//    ((mat) != NULL && (((const CvMatND*)(mat))->type & CV_MAGIC_MASK) == CV_MATND_MAGIC_VAL)
  Result := //
  (mat <> nil) and//
  ((pCvMatND(mat)^.&type and CV_MAGIC_MASK) = CV_MATND_MAGIC_VAL);
end;

function CV_IS_MATND(const mat:pCvArr):Boolean;
begin
//#define CV_IS_MATND(mat) \
//    (CV_IS_MATND_HDR(mat) && ((const CvMatND*)(mat))->data.ptr != NULL)
 Result := CV_IS_MATND_HDR(mat) and (pCvMatND(mat)^.data.ptr <> nil);
end;

function cvRealScalar(val0:double):TCvScalar;
begin
 Result.val[0] := val0;
 Result.val[1] := 0;
 Result.val[2] := 0;
 Result.val[3] := 0;
end;

function CV_IS_SET_ELEM(const ptr:pCvArr):Boolean;
begin
  //#define CV_IS_SET_ELEM( ptr )  (((CvSetElem*)(ptr))->flags >= 0)
  Result := pCvSetElem(ptr)^.flags>=0;
end;

(************************************* CvSlice ******************************************)

function cvSlice(start:int; &end:int):TCvSlice;
begin
  Result := _cvSlice.cvSlice(start,&end);
end;

class function _cvSlice.CvSlice(start:int = 0; &end:int = 0):_CvSlice;
begin
  Result.
  start_index:=start;
  Result.
  end_index:=&end;
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

function cvScalar(val0:double; val1:double=0; val2:double=0; val3:double=0):TCvScalar;
begin
  Result := _CvScalar.CvScalar(val0,val1,val2,val3);
end;

constructor _CvScalar.CvScalar(d0:double; d1:double = 0; d2:double = 0; d3:double = 0);
begin
{ val[0] = d0; val[1] = d1; val[2] = d2; val[3] = d3; }
  val[0] := d0; val[1] := d1; val[2] := d2; val[3] := d3;
end;

(****************************************************************************************\
*                                  Matrix type (CvMat)                                   *
\****************************************************************************************)

function cvMat(const m:TMat):TCvMat;
begin
    Assert(m.dims <= 2);
    Result := cvMat(m.rows,
    ifthen(m.dims = 1,1,m.cols),
    m.&type(), m.data);
    Result.step := int(m.step[0]);
    Result.&type := (Result.&type and (not TMat.CONTINUOUS_FLAG)) or (m.flags and TMat.CONTINUOUS_FLAG);
end;

function cvMat():TCvMat;
begin
  Result := default(TCvMat);
end;

function cvMat(const m:TCvMat):TCvMat;
begin
  Result := TCvMat(m);
end;

function cvmGet(const mat:pCvMat; row,col:int):double;
begin
    var &type:int;

    &type := CV_MAT_TYPE(mat^.&type);
    assert( (unsigned(row) < unsigned(mat^.r1.rows)) and
            (unsigned(col) < unsigned(mat^.r2.cols)) );

    if( &type = CV_32FC1 ) then
        Result := pfloat(mat^.data.ptr + mat^.step*row)[col]
    else
    begin
        assert( &type = CV_64FC1 );
        Result := pdouble(mat^.data.ptr + mat^.step*row)[col];
    end;
end;

class operator _CvMat.Explicit(const m: TMat): _CvMat;
begin
  Result := cvMat(m);
end;

(****************************************************************************************\
*                       Multi-dimensional dense array (CvMatND)                          *
\****************************************************************************************)

class operator _CvMatND.Explicit(const m: TMat): _CvMatND;
begin
  Result := cvMatND(m);
end;

(****************************************************************************************\
*                                  Image type (IplImage)                                 *
\****************************************************************************************)

function cvIplImage(const m:TMat):TIplImage;
begin
  Result := func_cvIplImage(m);
end;

function CV_IS_IMAGE_HDR(const img:pcvArr):boolean;
begin
  //    ((img) != NULL && ((const IplImage*)(img))->nSize == sizeof(IplImage))
  Result := Assigned(Img) and (pIplImage(img)^.nSize=sizeof(TIplImage));
end;

function CV_IS_IMAGE(const img:pcvArr):boolean;
begin
//    (CV_IS_IMAGE_HDR(img) && ((IplImage*)img)->imageData != NULL)
  Result := CV_IS_IMAGE_HDR(img) and (pIplImage(img)^.imageData<>nil);
end;

function cvMat( rows, cols, &type:int; const data:pointer=nil):TCvMat;
begin
    assert( unsigned(CV_MAT_DEPTH(&type)) <= CV_64F );
    &type := CV_MAT_TYPE(&type);
    Result.&type := CV_MAT_MAGIC_VAL or CV_MAT_CONT_FLAG or &type;
    Result.r2.cols := cols;
    Result.r1.rows := rows;
    Result.step := Result.r2.cols*CV_ELEM_SIZE(&type);
    Result.data.ptr := puchar(data);
    Result.refcount := nil;
    Result.hdr_refcount := 0;
end;

{$IFEND}
