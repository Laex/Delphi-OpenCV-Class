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

{$IF defined(OPENCV_CORE_C_H) and (not defined(OPENCV_CORE_C_H_IMPL))}
{$DEFINE  OPENCV_CORE_C_H_IMPL}

{$IF not defined(OPENCV_CORE_TYPES_H_IMPL)}{$I 'core/types_c.impl.inc'}{$ifend}

procedure cvDecRefData(arr: pCvArr);
begin
  if (CV_IS_MAT(arr)) then
  begin
    Var
      mat: pCvMat := pCvMat(arr);
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
    Var
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
  Var
    refcount: int := 0;
  if (CV_IS_MAT(arr)) then
  begin
    Var
      mat: pCvMat := pCvMat(arr);
    if (mat^.refcount <> nil) then
    begin
      inc(mat^.refcount^);
      refcount := mat^.refcount^;
    end;
  end
  else if (CV_IS_MATND(arr)) then
  begin
    Var
      mat: pCvMatND := pCvMatND(arr);
    if (mat^.refcount <> nil) then
    begin
      inc(mat^.refcount^);
      refcount := mat^.refcount^;
    end;
  end;
  Result := refcount;
end;

function cvGetRow(const arr: pCvArr; submat: pCvMat; row: int): pCvMat;
begin
  Result := cvGetRows(arr, submat, row, row + 1, 1);
end;

function cvGetCol(const arr: pCvArr; submat: pCvMat; col: int): pCvMat;
begin
  Result := cvGetCols(arr, submat, col, col + 1);
end;

procedure cvReleaseMatND(var mat: pCvMatND);
begin
  cvReleaseMat(pCvMat(mat));
end;

function cvGetNextSparseNode(mat_iterator: pCvSparseMatIterator): pCvSparseNode;
Type
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
    Var
      idx: int;
    inc(mat_iterator^.curidx);
    for idx := mat_iterator^.curidx to mat_iterator^.mat^.hashsize - 1 do
    begin
      Var
        node: pCvSparseNode := pArrayOfPoiner(mat_iterator^.mat^.hashtable)[idx];
      if (node <> nil) then
      begin
        mat_iterator^.curidx := idx;
        mat_iterator^.node := node;
        Exit(mat_iterator^.node);
      end;
    end;
    Result := Nil;
  end;
end;

// #define cvReshapeND( arr, header, new_cn, new_dims, new_sizes )   \
// cvReshapeMatND( (arr), sizeof( *(header)), (header),         \
// (new_cn), (new_dims), (new_sizes))
function cvReshapeND(const arr: pCvArr; header: pCvArr; new_cn, new_dims: int; new_sizes: pint): pCvArr;
begin
  Result := cvReshapeMatND(arr, sizeof(pCvMat(header)^), header, new_cn, new_dims, new_sizes);
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
  Result := cvSeqSlice(seq, CV_WHOLE_SEQ, storage, 1);
end;

function cvCvtSeqToArray(const seq: pCvSeq; elements: pvoid): pointer;
begin
  Result := cvCvtSeqToArray(seq, elements, CV_WHOLE_SEQ);
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
  Result := elem;
end;

procedure cvSetRemoveByPtr(set_header: pCvSet; elem: pvoid);
begin
  var
    _elem: pCvSetElem := pCvSetElem(elem);
  assert(_elem^.flags >= 0 (* && (elem^.flags & CV_SET_ELEM_IDX_MASK) < set_header^.total *) );
  _elem^.next_free := set_header^.free_elems;
  _elem^.flags := (_elem^.flags and CV_SET_ELEM_IDX_MASK) or CV_SET_ELEM_FREE_FLAG;
  set_header^.free_elems := _elem;
  Dec(set_header^.active_count);
end;

function cvGetSetElem(const set_header: pCvSet; idx: int): pCvSetElem;
begin
  var
    elem: pCvSetElem := pCvSetElem(cvGetSeqElem(pCvSeq(set_header), idx));
  if Assigned(elem) and CV_IS_SET_ELEM(pCvArr(elem)) then
    Result := elem
  else
    Result := nil;
end;

function cvArrToMat(const arr: pCvArr; copyData: bool = false; allowND: bool = true; coiMode: int = 0; buf: pAutoBuffer_of_double = nil): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := func_cvarrToMat(pVoid(arr), copyData, allowND, coiMode, buf);
{$IFEND}
end;

function cvArrToMatND(const arr: pCvArr; copyData: bool = false; coiMode: int = 0): TMat;
begin
  Result := cvArrToMat(arr, copyData, true, coiMode);
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
  Result := func_seqPush(seq, element);
{$IFEND}
end;

function seqPushFront(seq: pCvSeq; const element: pvoid = nil): pschar;
begin
{$IF not defined(PACKAGE)}
  Result := func_seqPushFront(seq, element);
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
  Result := func_getSeqElem(seq, index);
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
  Result := _cvAlloc(size);
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
  Result := _cvCreateImageHeader(size, depth, channels);
{$IFEND}
end;

function cvInitImageHeader(Var image: TIplImage; size: TCvSize; depth: int; channels: int; origin: int = 0; align: int = 4): pIplImage;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInitImageHeader(image, size, depth, channels, origin, align);
{$IFEND}
end;

function cvCreateImage(size: TCvSize; depth, channels: int): pIplImage;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateImage(size, depth, channels);
{$IFEND}
end;

procedure cvReleaseImageHeader(Var image: pIplImage);
begin
{$IF not defined(PACKAGE)}
  _cvReleaseImageHeader(image);
{$IFEND}
end;

procedure cvReleaseImage(Var image: pIplImage);
begin
{$IF not defined(PACKAGE)}
  _cvReleaseImage(image);
{$IFEND}
end;

function cvCloneImage(const image: pIplImage): pIplImage;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCloneImage(image);
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
  Result := _cvGetImageCOI(image);
{$IFEND}
end;

procedure cvSetImageROI(var image: TIplImage; rect: TCvRect);
begin
{$IF not defined(PACKAGE)}
  _cvSetImageROI(image, rect);
{$IFEND}
end;

procedure cvResetImageROI(Var image: TIplImage);
begin
{$IF not defined(PACKAGE)}
  _cvResetImageROI(image);
{$IFEND}
end;

function cvGetImageROI(const image: pIplImage): TCvRect;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetImageROI(image);
{$IFEND}
end;

function cvCreateMatHeader(rows, cols, &type: int): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateMatHeader(rows, cols, &type);
{$IFEND}
end;

function cvInitMatHeader(var mat: TCvMat; rows, cols, &type: int; data: pointer = nil; step: int = CV_AUTOSTEP): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInitMatHeader(mat, rows, cols, &type, data, step);
{$IFEND}
end;

function cvCreateMat(rows, cols, &type: int): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateMat(rows, cols, &type);
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
  Result := _cvCloneMat(mat);
{$IFEND}
end;

function cvGetSubRect(const arr: pCvArr; submat: pCvMat; rect: TCvRect): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetSubRect(arr, submat, rect);
{$IFEND}
end;

function cvGetSubArr(const arr: pCvArr; submat: pCvMat; rect: TCvRect): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetSubArr(arr, submat, rect);
{$IFEND}
end;

function cvGetRows(const arr: pCvArr; submat: pCvMat; start_row, end_row: int; delta_row: int = 1): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetRows(arr, submat, start_row, end_row, delta_row);
{$IFEND}
end;

function cvGetCols(const arr: pCvArr; submat: pCvMat; start_col, end_col: int): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetCols(arr, submat, start_col, end_col);
{$IFEND}
end;

function cvGetDiag(const arr: pCvArr; submat: pCvMat; diag: int = 0): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetDiag(arr, submat, diag);
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
  Result := _cvCreateMatNDHeader(dims, sizes, &type);
{$IFEND}
end;

function cvCreateMatND(dims: int; const sizes: pint; &type: int): pCvMatND;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateMatND(dims, sizes, &type);
{$IFEND}
end;

function cvInitMatNDHeader(mat: pCvMatND; dims: int; const sizes: pint; &type: int; data: pointer = nil): pCvMatND;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInitMatNDHeader(mat, dims, sizes, &type, data);
{$IFEND}
end;

function cvCloneMatND(const mat: pCvMatND): pCvMatND;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCloneMatND(mat);
{$IFEND}
end;

function cvCreateSparseMat(dims: int; const sizes: pint; &type: int): pCvSparseMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateSparseMat(dims, sizes, &type);
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
  Result := _cvCloneSparseMat(mat);
{$IFEND}
end;

function cvInitSparseMatIterator(const mat: pCvSparseMat; mat_iterator: pCvSparseMatIterator): pCvSparseNode;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInitSparseMatIterator(mat, mat_iterator);
{$IFEND}
end;

function cvInitNArrayIterator(count: int; var arrs: pCvArr; const mask: pCvArr; stubs: pCvMatND; array_iterator: pCvNArrayIterator; flags: int = 0): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInitNArrayIterator(count, arrs, mask, stubs, array_iterator, flags);
{$IFEND}
end;

function cvNextNArraySlice(array_iterator: pCvNArrayIterator): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvNextNArraySlice(array_iterator);
{$IFEND}
end;

function cvGetElemType(const arr: pCvArr): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetElemType(arr);
{$IFEND}
end;

function cvGetDims(const arr: pCvArr; sizes: pint = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetDims(arr, sizes);
{$IFEND}
end;

function cvGetDimSize(const arr: pCvArr; index: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetDimSize(arr, index);
{$IFEND}
end;

function cvPtr1D(const arr: pCvArr; idx0: int; &type: pint = nil): puchar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvPtr1D(arr, idx0, &type);
{$IFEND}
end;

function cvPtr2D(const arr: pCvArr; idx0, idx1: int; &type: pint = nil): puchar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvPtr2D(arr, idx0, idx1, &type);
{$IFEND}
end;

function cvPtr3D(const arr: pCvArr; idx0, idx1, idx2: int; &type: pint = nil): puchar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvPtr3D(arr, idx0, idx1, idx2, &type);
{$IFEND}
end;

function cvPtrND(const arr: pCvArr; const idx: pint; &type: pint = nil; create_node: int = 1; precalc_hashval: punsigned = nil): puchar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvPtrND(arr, idx, &type, create_node, precalc_hashval);
{$IFEND}
end;

function cvGet1D(const arr: pCvArr; idx0: int): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGet1D(arr, idx0);
{$IFEND}
end;

function cvGet2D(const arr: pCvArr; idx0, idx1: int): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGet2D(arr, idx0, idx1);
{$IFEND}
end;

function cvGet3D(const arr: pCvArr; idx0, idx1, idx2: int): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGet3D(arr, idx0, idx1, idx2);
{$IFEND}
end;

function cvGetND(const arr: pCvArr; const idx: pint): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetND(arr, idx);
{$IFEND}
end;

function cvGetReal1D(const arr: pCvArr; idx0: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetReal1D(arr, idx0);
{$IFEND}
end;

function cvGetReal2D(const arr: pCvArr; idx0, idx1: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetReal2D(arr, idx0, idx1);
{$IFEND}
end;

function cvGetReal3D(const arr: pCvArr; idx0, idx1, idx2: int): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetReal3D(arr, idx0, idx1, idx2);
{$IFEND}
end;

function cvGetRealND(const arr: pCvArr; const idx: pint): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetRealND(arr, idx);
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
  Result := _cvGetMat(arr, header, coi, allowND);
{$IFEND}
end;

function cvGetImage(const arr: pCvArr; image_header: pIplImage): pIplImage;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetImage(arr, image_header);
{$IFEND}
end;

function cvReshapeMatND(const arr: pCvArr; sizeof_header: int; header: pCvArr; new_cn, new_dims: int; new_sizes: pint): pCvArr;
begin
{$IF not defined(PACKAGE)}
  Result := _cvReshapeMatND(arr, sizeof_header, header, new_cn, new_dims, new_sizes);
{$IFEND}
end;

function cvReshape(const arr: pCvArr; header: pCvMat; new_cn: int; new_rows: int = 0): pCvMat;
begin
{$IF not defined(PACKAGE)}
  Result := _cvReshape(arr, header, new_cn, new_rows);
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
  Result := _cvGetSize(arr);
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

procedure cvMerge(const src0: pCvArr; const src1: pCvArr; const src2: pCvArr; const src3: pCvArr; dst: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvMerge(src0, src1, src2, src3, dst);
{$IFEND}
end;

procedure cvMixChannels(const src: pCvArr; src_count: int; dst: pCvArr; dst_count: int; const from_to: pint; pair_count: int);
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

function cvCheckTermCriteria(criteria: TCvTermCriteria; default_eps: double; default_max_iters: int): TCvTermCriteria;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCheckTermCriteria(criteria, default_eps, default_max_iters);
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

procedure cvAddWeighted(const src1: pCvArr; alpha: double; const src2: pCvArr; beta: double; gamma: double; dst: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvAddWeighted(src1, alpha, src2, beta, gamma, dst);
{$IFEND}
end;

function cvDotProduct(const src1: pCvArr; const src2: pCvArr): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvDotProduct(src1, src2);
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

procedure cvCartToPolar(const x: pCvArr; const y: pCvArr; magnitude: pCvArr; angle: pCvArr = nil; angle_in_degrees: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvCartToPolar(x, y, magnitude, angle, angle_in_degrees);
{$IFEND}
end;

procedure cvPolarToCart(const magnitude: pCvArr; const angle: pCvArr; x: pCvArr; y: pCvArr; angle_in_degrees: int = 0);
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
  Result := _cvFastArctan(y, x);
{$IFEND}
end;

function cvCbrt(value: float): float;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCbrt(value);
{$IFEND}
end;

function cvCheckArr(const arr: pCvArr; flags: int = 0; min_val: double = 0; max_val: double = 0): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCheckArr(arr, flags, min_val, max_val);
{$IFEND}
end;

function cvCheckArray(const arr: pCvArr; flags: int = 0; min_val: double = 0; max_val: double = 0): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCheckArray(arr, flags, min_val, max_val);
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
  Result := _cvSolveCubic(coeffs, roots);
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

procedure cvGEMM(const src1: pCvArr; const src2: pCvArr; alpha: double; const src3: pCvArr; beta: double; dst: pCvArr; tABC: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvGEMM(src1, src2, alpha, src3, beta, dst, tABC);
{$IFEND}
end;

procedure cvMatMulAddEx(const src1: pCvArr; const src2: pCvArr; alpha: double; const src3: pCvArr; beta: double; dst: pCvArr; tABC: int = 0);
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

procedure cvMulTransposed(const src: pCvArr; dst: pCvArr; order: int; const delta: pCvArr = nil; scale: double = 1);
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

procedure cvSVD(a: pCvArr; w: pCvArr; u: pCvArr = nil; V: pCvArr = nil; flags: int = 0);
begin
{$IF not defined(PACKAGE)}
  _cvSVD(a, w, u, V, flags);
{$IFEND}
end;

procedure cvSVBkSb(const w: pCvArr; const u: pCvArr; const V: pCvArr; const b: pCvArr; x: pCvArr; flags: int);
begin
{$IF not defined(PACKAGE)}
  _cvSVBkSb(w, u, V, b, x, flags);
{$IFEND}
end;

function cvInvert(const src: pCvArr; dst: pCvArr; method: int = CV_LU): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInvert(src, dst, method);
{$IFEND}
end;

function cvInv(const src: pCvArr; dst: pCvArr; method: int = CV_LU): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvInv(src, dst, method);
{$IFEND}
end;

function cvSolve(const src1: pCvArr; const src2: pCvArr; dst: pCvArr; method: int = CV_LU): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSolve(src1, src2, dst, method);
{$IFEND}
end;

function cvDet(const mat: pCvArr): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvDet(mat);
{$IFEND}
end;

function cvTrace(const mat: pCvArr): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvTrace(mat);
{$IFEND}
end;

procedure cvEigenVV(mat: pCvArr; evects: pCvArr; evals: pCvArr; eps: double = 0; lowindex: int = -1; highindex: int = -1);
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
  Result := _cvRange(mat, start, &end);
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

procedure cvProjectPCA(const data: pCvArr; const mean: pCvArr; const eigenvects: pCvArr; Result: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvProjectPCA(data, mean, eigenvects, Result);
{$IFEND}
end;

procedure cvBackProjectPCA(const proj: pCvArr; const mean: pCvArr; const eigenvects: pCvArr; Result: pCvArr);
begin
{$IF not defined(PACKAGE)}
  _cvBackProjectPCA(proj, mean, eigenvects, Result);
{$IFEND}
end;

function cvMahalanobis(const vec1: pCvArr; const vec2: pCvArr; const mat: pCvArr): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMahalanobis(vec1, vec2, mat);
{$IFEND}
end;

function cvMahalonobis(const vec1: pCvArr; const vec2: pCvArr; const mat: pCvArr): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMahalonobis(vec1, vec2, mat);
{$IFEND}
end;

function cvSum(const arr: pCvArr): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSum(arr);
{$IFEND}
end;

function cvCountNonZero(const arr: pCvArr): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCountNonZero(arr);
{$IFEND}
end;

function cvAvg(const arr: pCvArr; const mask: pCvArr = nil): TCvScalar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvAvg(arr, mask);
{$IFEND}
end;

procedure cvAvgSdv(const arr: pCvArr; mean: pCvScalar; std_dev: pCvScalar; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvAvgSdv(arr, mean, std_dev, mask);
{$IFEND}
end;

procedure cvMinMaxLoc(const arr: pCvArr; min_val: pdouble; max_val: pdouble; min_loc: pCvPoint = nil; max_loc: pCvPoint = nil; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvMinMaxLoc(arr, min_val, max_val, min_loc, max_loc, mask);
{$IFEND}
end;

function cvNorm(const arr1: pCvArr; const arr2: pCvArr = nil; norm_type: int = CV_L2; const mask: pCvArr = nil): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvNorm(arr1, arr2, norm_type, mask);
{$IFEND}
end;

procedure cvNormalize(const src: pCvArr; dst: pCvArr; a: double = 1.; b: double = 0.; norm_type: int = CV_L2; const mask: pCvArr = nil);
begin
{$IF not defined(PACKAGE)}
  _cvNormalize(src, dst, a, b, norm_type, mask);
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
  Result := _cvGetOptimalDFTSize(size0);
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
  Result := _cvSliceLength(slice, seq);
{$IFEND}
end;

function cvCreateMemStorage(block_size: int = 0): pCvMemStorage;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateMemStorage(block_size);
{$IFEND}
end;

function cvCreateChildMemStorage(parent: pCvMemStorage): pCvMemStorage;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateChildMemStorage(parent);
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
  Result := _cvMemStorageAlloc(storage, size);
{$IFEND}
end;

function cvCreateSeq(seq_flags: int; header_size: size_t; elem_size: size_t; storage: pCvMemStorage): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateSeq(seq_flags, header_size, elem_size, storage);
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
  Result := _cvSeqPush(seq, element);
{$IFEND}
end;

function cvSeqPushFront(seq: pCvSeq; const element: pvoid = nil): pschar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSeqPushFront(seq, element);
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
  Result := _cvSeqInsert(seq, before_index, element);
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
  Result := _cvGetSeqElem(seq, index);
{$IFEND}
end;

function cvSeqElemIdx(const seq: pCvSeq; const element: pvoid; block: pCvSeqBlock = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSeqElemIdx(seq, element, block);
{$IFEND}
end;

procedure cvStartAppendToSeq(seq: pCvSeq; writer: pCvSeqWriter);
begin
{$IF not defined(PACKAGE)}
  _cvStartAppendToSeq(seq, writer);
{$IFEND}
end;

procedure cvStartWriteSeq(seq_flags: int; header_size: int; elem_size: int; storage: pCvMemStorage; writer: pCvSeqWriter);
begin
{$IF not defined(PACKAGE)}
  _cvStartWriteSeq(seq_flags, header_size, elem_size, storage, writer);
{$IFEND}
end;

function cvEndWriteSeq(writer: pCvSeqWriter): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvEndWriteSeq(writer);
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
  Result := _cvGetSeqReaderPos(reader);
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
  Result := _cvCvtSeqToArray(seq, elements, slice);
{$IFEND}
end;

function cvMakeSeqHeaderForArray(seq_type: int; header_size: int; elem_size: int; elements: pvoid; total: int; seq: pCvSeq; block: pCvSeqBlock): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvMakeSeqHeaderForArray(seq_type, header_size, elem_size, elements, total, seq, block);
{$IFEND}
end;

function cvSeqSlice(const seq: pCvSeq; slice: TCvSlice; storage: pCvMemStorage = nil; copy_data: int = 0): pCvSeq;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSeqSlice(seq, slice, storage, copy_data);
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

function cvSeqSearch(seq: pCvSeq; const elem: pvoid; func: TCvCmpFunc; is_sorted: int; elem_idx: pint; userdata: pvoid = nil): pschar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSeqSearch(seq, elem, func, is_sorted, elem_idx, userdata);
{$IFEND}
end;

procedure cvSeqInvert(seq: pCvSeq);
begin
{$IF not defined(PACKAGE)}
  _cvSeqInvert(seq);
{$IFEND}
end;

function cvSeqPartition(const seq: pCvSeq; storage: pCvMemStorage; labels: pCvSeq; is_equal: TCvCmpFunc; userdata: pvoid): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSeqPartition(seq, storage, labels, is_equal, userdata);
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
  Result := _cvCreateSet(set_flags, header_size, elem_size, storage);
{$IFEND}
end;

function cvSetAdd(set_header: pCvSet; elem: pCvSetElem = nil; inserted_elem: pCvSetElem = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSetAdd(set_header, elem, inserted_elem);
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

function cvCreateGraph(graph_flags: int; header_size: int; vtx_size: int; edge_size: int; storage: pCvMemStorage): pCvGraph;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateGraph(graph_flags, header_size, vtx_size, edge_size, storage);
{$IFEND}
end;

function cvGraphAddVtx(graph: pCvGraph; const vtx: pCvGraphVtx = nil; inserted_vtx: pCvGraphVtx = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphAddVtx(graph, vtx, inserted_vtx);
{$IFEND}
end;

function cvGraphRemoveVtx(graph: pCvGraph; index: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphRemoveVtx(graph, index);
{$IFEND}
end;

function cvGraphRemoveVtxByPtr(graph: pCvGraph; vtx: pCvGraphVtx): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphRemoveVtxByPtr(graph, vtx);
{$IFEND}
end;

function cvGraphAddEdge(graph: pCvGraph; start_idx: int; end_idx: int; const edge: pCvGraphEdge = nil; inserted_edge: pCvGraphEdge = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphAddEdge(graph, start_idx, end_idx, edge, inserted_edge);
{$IFEND}
end;

function cvGraphAddEdgeByPtr(graph: pCvGraph; start_vtx: pCvGraphVtx; end_vtx: pCvGraphVtx; const edge: pCvGraphEdge = nil; inserted_edge: pCvGraphEdge = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphAddEdgeByPtr(graph, start_vtx, end_vtx, edge, inserted_edge);
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
  Result := _cvFindGraphEdge(graph, start_idx, end_idx);
{$IFEND}
end;

function cvGraphFindEdge(const graph: pCvGraph; start_idx: int; end_idx: int): pCvGraphEdge;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphFindEdge(graph, start_idx, end_idx);
{$IFEND}
end;

function cvFindGraphEdgeByPtr(const graph: pCvGraph; const start_vtx: pCvGraphVtx; const end_vtx: pCvGraphVtx): pCvGraphEdge;
begin
{$IF not defined(PACKAGE)}
  Result := _cvFindGraphEdgeByPtr(graph, start_vtx, end_vtx);
{$IFEND}
end;

function cvGraphFindEdgeByPtr(const graph: pCvGraph; const start_vtx: pCvGraphVtx; const end_vtx: pCvGraphVtx): pCvGraphEdge;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphFindEdgeByPtr(graph, start_vtx, end_vtx);
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
  Result := _cvGraphVtxDegree(graph, vtx_idx);
{$IFEND}
end;

function cvGraphVtxDegreeByPtr(const graph: pCvGraph; const vtx: pCvGraphVtx): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGraphVtxDegreeByPtr(graph, vtx);
{$IFEND}
end;

function cvCreateGraphScanner(graph: pCvGraph; vtx: pCvGraphVtx = nil; mask: int = CV_GRAPH_ALL_ITEMS): pCvGraphScanner;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCreateGraphScanner(graph, vtx, mask);
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
  Result := _cvNextGraphItem(scanner);
{$IFEND}
end;

function cvCloneGraph(const graph: pCvGraph; storage: pCvMemStorage): pCvGraph;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCloneGraph(graph, storage);
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
  Result := _cvNextTreeNode(tree_iterator);
{$IFEND}
end;

function cvPrevTreeNode(tree_iterator: pCvTreeNodeIterator): pointer;
begin
{$IF not defined(PACKAGE)}
  Result := _cvPrevTreeNode(tree_iterator);
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
  Result := _cvTreeToNodeSeq(first, header_size, storage);
{$IFEND}
end;

function cvKMeans2(const samples: pCvArr; cluster_count: int; labels: pCvArr; termcrit: TCvTermCriteria; attempts: int = 1; rng: pCvRNG = nil; flags: int = 0; _centers: pCvArr = nil; compactness: pdouble = nil): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvKMeans2(samples, cluster_count, labels, termcrit, attempts, rng, flags, _centers, compactness);
{$IFEND}
end;

function cvUseOptimized(on_off: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvUseOptimized(on_off);
{$IFEND}
end;

procedure cvSetIPLAllocators(create_header: TCv_iplCreateImageHeader; allocate_data: TCv_iplAllocateImageData; deallocate: TCv_iplDeallocate; create_roi: TCv_iplCreateROI; clone_image: TCv_iplCloneImage);
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
  Result := _cvClone(struct_ptr);
{$IFEND}
end;

function cvGetTickCount(): int64;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetTickCount();
{$IFEND}
end;

function cvGetTickFrequency(): double;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetTickFrequency();
{$IFEND}
end;

function cvCheckHardwareSupport(feature: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvCheckHardwareSupport(feature);
{$IFEND}
end;

function cvGetNumThreads(): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetNumThreads();
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
  Result := _cvGetThreadNum();
{$IFEND}
end;

function cvGetErrStatus(): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetErrStatus();
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
  Result := _cvGetErrMode();
{$IFEND}
end;

function cvSetErrMode(mode: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvSetErrMode(mode);
{$IFEND}
end;

procedure cvError(status: int; const func_name: pcvchar; const err_msg: pcvchar; const file_name: pcvchar; line: int);
begin
{$IF not defined(PACKAGE)}
  _cvError(status, func_name, err_msg, file_name, line);
{$IFEND}
end;

function cvErrorStr(status: int): pcvchar;
begin
{$IF not defined(PACKAGE)}
  Result := _cvErrorStr(status);
{$IFEND}
end;

function cvGetErrInfo(const errcode_desc: pcvchar; const description: pcvchar; const filename: pcvchar; line: pint): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGetErrInfo(errcode_desc, description, filename, line);
{$IFEND}
end;

function cvErrorFromIppStatus(ipp_status: int): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvErrorFromIppStatus(ipp_status);
{$IFEND}
end;

function cvRedirectError(error_handler: TCvErrorCallback; userdata: pvoid = nil; prev_userdata: pvoid = nil): TCvErrorCallback;
begin
{$IF not defined(PACKAGE)}
  Result := _cvRedirectError(error_handler, userdata, prev_userdata);
{$IFEND}
end;

function cvNulDevReport(status: int; const func_name: pcvchar; const err_msg: pcvchar; const file_name: pcvchar; line: int; userdata: pvoid): int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvNulDevReport(status, func_name, err_msg, file_name, line, userdata);
{$IFEND}
end;

function cvStdErrReport(status:int; const func_name:pcvchar; const err_msg:pcvchar; const file_name:pcvchar; line:int; userdata:pvoid):int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvStdErrReport(status, func_name, err_msg, file_name, line, userdata);
{$IFEND}
end;

function cvGuiBoxReport(status:int; const func_name:pcvchar; const err_msg:pcvchar; const file_name:pcvchar; line:int; userdata:pvoid):int;
begin
{$IF not defined(PACKAGE)}
  Result := _cvGuiBoxReport(status, func_name, err_msg, file_name, line, userdata);
{$IFEND}
end;

{$IFEND}
