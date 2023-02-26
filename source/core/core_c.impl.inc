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

CV_INLINE  void  cvDecRefData( CvArr* arr )
{
    if( CV_IS_MAT( arr ))
    {
        CvMat* mat = (CvMat*)arr;
        mat->data.ptr = NULL;
        if( mat->refcount != NULL && --*mat->refcount == 0 )
            cvFree( &mat->refcount );
        mat->refcount = NULL;
    }
    else if( CV_IS_MATND( arr ))
    {
        CvMatND* mat = (CvMatND*)arr;
        mat->data.ptr = NULL;
        if( mat->refcount != NULL && --*mat->refcount == 0 )
            cvFree( &mat->refcount );
        mat->refcount = NULL;
    }
}
end;

function cvIncRefData( arr:pCvArr ):int;inline;
begin
CV_INLINE  int  cvIncRefData( CvArr* arr )
{
    int refcount = 0;
    if( CV_IS_MAT( arr ))
    {
        CvMat* mat = (CvMat*)arr;
        if( mat->refcount != NULL )
            refcount = ++*mat->refcount;
    }
    else if( CV_IS_MATND( arr ))
    {
        CvMatND* mat = (CvMatND*)arr;
        if( mat->refcount != NULL )
            refcount = ++*mat->refcount;
    }
    return refcount;
}
end;

function cvGetRow( const arr:pCvArr;  submat:pCvMat; row:int ):pCvMat;
begin
    return cvGetRows( arr, submat, row, row + 1, 1 );
end;

function cvGetCol( const arr:pCvArr;  submat:pCvMat; col:int ):pCvMat;
begin
  return cvGetCols( arr, submat, col, col + 1 );
end;

procedure cvReleaseMatND( var mat:pCvMatND );
begin
  cvReleaseMat( (CvMat**)mat );
end;

function cvGetNextSparseNode( mat_iterator:pCvSparseMatIterator ):pCvSparseNode;
begin
{
    if( mat_iterator->node->next )
        return mat_iterator->node = mat_iterator->node->next;
    else
    {
        int idx;
        for( idx = ++mat_iterator->curidx; idx < mat_iterator->mat->hashsize; idx++ )
        {
            CvSparseNode* node = (CvSparseNode*)mat_iterator->mat->hashtable[idx];
            if( node )
            {
                mat_iterator->curidx = idx;
                return mat_iterator->node = node;
            }
        }
        return NULL;
    }
}
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
  cvSeqSlice( seq, CV_WHOLE_SEQ, storage, 1 );
end;

function cvCvtSeqToArray(const seq:pCvSeq; elements:pvoid):pointer;
begin
 Result := cvCvtSeqToArray(seq, elements, CvSlice(CV_WHOLE_SEQ));
end;

function cvSetNew(set_header:pCvSet):pCvSetElem;
begin
    CvSetElem* elem = set_header->free_elems;
    if( elem )
    {
        set_header->free_elems = elem->next_free;
        elem->flags = elem->flags & CV_SET_ELEM_IDX_MASK;
        set_header->active_count++;
    }
    else
        cvSetAdd( set_header, NULL, &elem );
    return elem;
end;

procedure cvSetRemoveByPtr(set_header:pCvSet; elem:pvoid);
begin
    CvSetElem* _elem = (CvSetElem*)elem;
    assert( _elem->flags >= 0 (*&& (elem->flags & CV_SET_ELEM_IDX_MASK) < set_header->total*) );
    _elem->next_free = set_header->free_elems;
    _elem->flags = (_elem->flags & CV_SET_ELEM_IDX_MASK) | CV_SET_ELEM_FREE_FLAG;
    set_header->free_elems = _elem;
    set_header->active_count--;
end;

function cvGetSetElem(const set_header:pCvSet; idx:int):pCvSetElem;
begin
   CvSetElem* elem = (CvSetElem*)(void *)cvGetSeqElem( (CvSeq*)set_header, idx );
   return elem && CV_IS_SET_ELEM( elem ) ? elem : 0;
end;

function cvArrToMat(const arr:pCvArr; copyData:bool=false;
                          allowND:bool=true; coiMode:int=0;
                          buf:pAutoBuffer_of_double=nil):TMat;
begin
  Result := func_cvarrToMat(arr,copyData,allowND,coiMode,buf);
end;


function cvArrToMatND(const arr:pCvArr; copyData:bool=false; coiMode:int=0):TMat;
begin
     return cvarrToMat(arr, copyData, true, coiMode);
end;

function cvArrToMatND(const arr:pCvArr; copyData:bool=false; coiMode:int=0):TMat;
begin
 !!!!
end;

procedure insertImageCOI(coiimg:TInputArray; arr:pCvArr; coi:int=-1);
begin
!!!
end;

{$IFEND}
