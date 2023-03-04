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

{$include 'core/types_c.impl.inc'}

procedure cvDecRefData( arr:pCvArr );
begin
    if( CV_IS_MAT( arr )) then
    begin
        Var mat:pCvMat := pCvMat(arr);
        mat^.data.ptr := nil;
        if( mat^.refcount <> nil) then
        begin
          Dec(mat^.refcount^);
        if (mat^.refcount^ = 0 ) then
            cvFree( mat^.refcount );
        end;
        mat^.refcount := nil;
    end
    else if ( CV_IS_MATND( arr )) then
    begin
        Var mat:pCvMatND := pCvMatND(arr);
        mat^.data.ptr := nil;
        if( mat^.refcount <> nil) then
        begin
         dec(mat^.refcount^);
         if (mat^.refcount^ = 0 ) then
            cvFree( mat^.refcount );
        end;
        mat^.refcount := nil;
    end;
end;

function cvIncRefData( arr:pCvArr ):int;inline;
begin
    Var refcount:int := 0;
    if( CV_IS_MAT( arr )) then
    begin
        Var mat:pCvMat := pCvMat(arr);
        if( mat^.refcount <> nil ) then
        begin
          inc(mat^.refcount^);
            refcount := mat^.refcount^;
            end;
    end
    else if( CV_IS_MATND( arr )) then
    begin
        Var mat:pCvMatND := pCvMatND(arr);
        if( mat^.refcount <> nil ) then
        begin
          inc(mat^.refcount^);
            refcount := mat^.refcount^;
            end;
    end;
    Result :=refcount;
end;

function cvGetRow( const arr:pCvArr;  submat:pCvMat; row:int ):pCvMat;
begin
    Result := cvGetRows( arr, submat, row, row + 1, 1 );
end;

function cvGetCol( const arr:pCvArr;  submat:pCvMat; col:int ):pCvMat;
begin
  Result := cvGetCols( arr, submat, col, col + 1 );
end;

procedure cvReleaseMatND( var mat:pCvMatND );
begin
  cvReleaseMat(pCvMat(mat));
end;

function cvGetNextSparseNode( mat_iterator:pCvSparseMatIterator ):pCvSparseNode;
Type
  pArrayOfPoiner = ^TArrayOfPoiner;
  TArrayOfPoiner = array[0..0] of pCvSparseNode;
begin
    if ( mat_iterator^.node^.next<>nil ) then
    begin
      mat_iterator^.node := mat_iterator^.node^.next;
        Exit(mat_iterator^.node);
    end
    else
    begin
        Var idx:int;
        inc(mat_iterator^.curidx);
        for idx := mat_iterator^.curidx to mat_iterator^.mat^.hashsize-1 do
        begin
            Var node:pCvSparseNode := pArrayOfPoiner(mat_iterator^.mat^.hashtable)[idx];
            if( node<>nil ) then
            begin
                mat_iterator^.curidx := idx;
                mat_iterator^.node := node;
                Exit(mat_iterator^.node);
            end;
        end;
        Result:=Nil;
    end;
end;

//#define cvReshapeND( arr, header, new_cn, new_dims, new_sizes )   \
//      cvReshapeMatND( (arr), sizeof( *(header)), (header),         \
//                      (new_cn), (new_dims), (new_sizes))
function cvReshapeND(const arr:pCvArr;
                           header:pCvArr;
                           new_cn, new_dims:int; new_sizes:pint ):pCvArr;
begin
 Result := cvReshapeMatND( arr, sizeof( pCvMat(header)^), header,
                      new_cn, new_dims, new_sizes);
end;

procedure cvConvert( const src:pCvArr; dst:pCvArr);
begin
  cvConvertScale( src, dst, 1, 0 );
end;

procedure cvSubS(const src:pCvArr; value:TCvScalar; dst:pCvArr; const mask:pCvArr= nil);
begin
    cvAddS( src, cvScalar( -value.val[0], -value.val[1], -value.val[2], -value.val[3]),
            dst, mask );
end;

procedure cvSetIdentity(mat:pCvArr);
begin
  cvSetIdentity(mat,cvRealScalar(1));
end;

function cvCloneSeq(const seq:pCvSeq; storage:pCvMemStorage= nil):pCvSeq;
begin
  Result:=cvSeqSlice( seq, CV_WHOLE_SEQ, storage, 1 );
end;

function cvCvtSeqToArray(const seq:pCvSeq; elements:pvoid):pointer;
begin
 Result := cvCvtSeqToArray(seq, elements, CV_WHOLE_SEQ);
end;

function cvSetNew(set_header:pCvSet):pCvSetElem;
begin
    var elem:pCvSetElem := set_header^.free_elems;
    if( elem<>nil ) then
    begin
        set_header^.free_elems := elem^.next_free;
        elem^.flags := elem^.flags and CV_SET_ELEM_IDX_MASK;
        Inc(set_header^.active_count);
    end
    else
        cvSetAdd( set_header, nil, elem );
    Result:= elem;
end;

procedure cvSetRemoveByPtr(set_header:pCvSet; elem:pvoid);
begin
    var _elem:pCvSetElem := pCvSetElem(elem);
    assert( _elem^.flags >= 0 (*&& (elem^.flags & CV_SET_ELEM_IDX_MASK) < set_header^.total*) );
    _elem^.next_free := set_header^.free_elems;
    _elem^.flags := (_elem^.flags and CV_SET_ELEM_IDX_MASK) or CV_SET_ELEM_FREE_FLAG;
    set_header^.free_elems := _elem;
    dec(set_header^.active_count);
end;

function cvGetSetElem(const set_header:pCvSet; idx:int):pCvSetElem;
begin
   var elem:pCvSetElem := pCvSetElem(cvGetSeqElem( pCvSeq(set_header), idx ));
   if Assigned(elem) and CV_IS_SET_ELEM(pcvArr(elem)) then
   Result := elem else result:=nil;
end;

function cvArrToMat(const arr:pCvArr; copyData:bool=false;
                          allowND:bool=true; coiMode:int=0;
                          buf:pAutoBuffer_of_double=nil):TMat;
begin
  Result := func_cvarrToMat(arr,copyData,allowND,coiMode,buf);
end;


function cvArrToMatND(const arr:pCvArr; copyData:bool=false; coiMode:int=0):TMat;
begin
     Result:= cvarrToMat(arr, copyData, true, coiMode);
end;

procedure extractImageCOI(const arr:pCvArr; coiimg:TOutputArray; coi:int=-1);
begin
  proc_extractImageCOI(arr, coiimg, coi);
end;

procedure insertImageCOI(const coiimg:TInputArray; arr:pCvArr; coi:int=-1);
begin
 proc_insertImageCOI(coiimg,arr,coi);
end;

function seqPush(seq:pCvSeq; const element:pvoid=nil):pschar;
begin
  result := func_seqPush(seq,element);
end;

function seqPushFront(seq:pCvSeq; const element:pvoid=nil):pschar;
begin
  Result := func_seqPushFront(seq,element);
end;

procedure seqPop(seq:pCvSeq; element:pvoid=nil);
begin
  proc_seqPop(seq,element);
end;

procedure seqPopFront(seq:pCvSeq; element:pvoid=nil);
begin
  proc_seqPopFront(seq,element);
end;

procedure seqPopMulti(seq:pCvSeq; elements:pvoid; count:int; in_front:int=0);
begin
  {TODO:seqPopMulti}
end;

procedure seqRemove(seq:pCvSeq; index:int);
begin
  proc_seqRemove(seq,index);
end;
procedure clearSeq(seq:pCvSeq);
begin
  proc_clearSeq(seq);
end;
function getSeqElem(const seq:pCvSeq; index:int):pschar;
begin
  Result := func_getSeqElem(seq,index);
end;
procedure seqRemoveSlice(seq:pCvSeq; slice:TCvSlice);
begin
  proc_seqRemoveSlice(seq,slice);
end;
procedure seqInsertSlice(seq:pCvSeq; before_index:int; const from_arr:pCvArr);
begin
  proc_seqInsertSlice(seq,before_index,from_arr);
end;

{$IFEND}
