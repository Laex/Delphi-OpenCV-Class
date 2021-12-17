/*
 This file is part of Delphi-OpenCV-Class project.
 https://github.com/Laex/Delphi-OpenCV-Class

 It is subject to the license terms in the LICENSE file found in the top-level directory
 of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

Copyright 2021, Laentir Valetov, laex@bk.ru

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#pragma once

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include "opencv2\opencv.hpp"
#include "opencv2\gapi.hpp"

#define BODY_API __declspec(dllexport)

using namespace cv;
using namespace std;

class BODY_API ExportString : public String {};
//class BODY_API ExportGRunArgBase : public GRunArgBase {};

enum VectorType {
#include "../source/vectortype.inc"
};

BODY_API void clearStdVector(void* obj, int vt)
{
#define DefClearStdVector(T) (*(static_cast<vector<T>*>(obj))).clear(); break;
#define caseDefClearStdVector(T) case vt ## T : DefClearStdVector(T)

	if (vt)
	{
		switch (vt)
		{
			caseDefClearStdVector(Mat)
			caseDefClearStdVector(Rect)
			caseDefClearStdVector(Point)
		case vtVectorMat: DefClearStdVector(vector<Mat>)
		case vtVectorRect: DefClearStdVector(vector<Rect>)
		case vtVectorPoint: DefClearStdVector(vector<Point>)
			caseDefClearStdVector(Point2f)
			caseDefClearStdVector(Scalar)
			caseDefClearStdVector(uchar)
			caseDefClearStdVector(float)
			caseDefClearStdVector(int)
			caseDefClearStdVector(Vec4i)
			caseDefClearStdVector(GMat)
			caseDefClearStdVector(GCompileArg)
		}
	}
}


BODY_API void resizeStdVector(void* obj, size_t NewSize, int vt)
{
#define DefResizeStdVector(T) (*(static_cast<vector<T>*>(obj))).resize(NewSize); break;
#define caseDefResizeStdVector(T) case vt ## T : DefResizeStdVector(T)

	if (vt)
	{
		switch (vt)
		{
			caseDefResizeStdVector(Mat)
			caseDefResizeStdVector(Rect)
			caseDefResizeStdVector(Point)
		case vtVectorMat: DefResizeStdVector(vector<Mat>)
		case vtVectorRect: DefResizeStdVector(vector<Rect>)
		case vtVectorPoint: DefResizeStdVector(vector<Point>)				
			caseDefResizeStdVector(Point2f)
			caseDefResizeStdVector(Scalar)
			caseDefResizeStdVector(uchar)
			caseDefResizeStdVector(float)		
			caseDefResizeStdVector(int)
			caseDefResizeStdVector(Vec4i)
			caseDefResizeStdVector(GMat)
			caseDefResizeStdVector(GCompileArg)
		}
	}
}


BODY_API void CopyStdVector(void* obj, void* src, int vt)
{

#define DefCopyStdVector(T) *(static_cast<vector<T>*>(obj))=*(static_cast<vector<T>*>(src));
#define caseDefCopyStdVector(T) case vt ## T : DefCopyStdVector(T) break;

	if (vt)
	{
		switch (vt)
		{
		case vtMat:
			DefCopyStdVector(Mat)
				break;
		case vtRect:
			DefCopyStdVector(Rect)
				break;
		case vtPoint:
			DefCopyStdVector(Point)
				break;
		case vtVectorMat:
			DefCopyStdVector(vector<Mat>)
				break;
		case vtVectorRect:
			DefCopyStdVector(vector<Rect>)
				break;
		case vtVectorPoint:
			DefCopyStdVector(vector<Point>)
				break;
		case vtPoint2f:
			DefCopyStdVector(Point2f)
				break;
		case vtScalar:
			DefCopyStdVector(Scalar)
				break;
		case vtuchar:
			DefCopyStdVector(uchar)
				break;
		case vtfloat:
			DefCopyStdVector(float)
				break;
		case vtint:
			DefCopyStdVector(int)
				break;
		case vtVec4i:
			DefCopyStdVector(Vec4i)
				break;
		caseDefCopyStdVector(GMat)
		caseDefCopyStdVector(GCompileArg)
		}
	}
}

BODY_API void CreateStdVector(void* obj, int vt)
{

#define DefCreateStdVector(T) *(static_cast<vector<T>*>(obj)) = vector<T>();
#define caseDefCreateStdVector(T) case vt ## T : DefCreateStdVector(T) break;

	if (vt)
	{
		switch (vt)
		{
		case vtMat:
			*(static_cast<vector<Mat>*>(obj)) = vector<Mat>();
			break;
		case vtRect:
			*(static_cast<vector<Rect>*>(obj)) = vector<Rect>();
			break;
		case vtPoint:
			*(static_cast<vector<Point>*>(obj)) = vector<Point>();
			break;
		case vtVectorMat:
			*(static_cast<vector<vector<Mat>>*>(obj)) = vector<vector<Mat>>();
			break;
		case vtVectorRect:
			*(static_cast<vector<vector<Rect>>*>(obj)) = vector<vector<Rect>>();
			break;
		case vtVectorPoint:
			*(static_cast<vector<vector<Point>>*>(obj)) = vector<vector<Point>>();
			break;
		case vtPoint2f:
			*(static_cast<vector<Point2f>*>(obj)) = vector<Point2f>();
			break;
		case vtScalar:
			DefCreateStdVector(Scalar)
				break;
		case vtuchar:
			DefCreateStdVector(uchar)
				break;
		case vtfloat:
			DefCreateStdVector(float)
				break;
		case vtint:
			DefCreateStdVector(int)
				break;
		case vtVec4i:
			DefCreateStdVector(Vec4i)
				break;
		caseDefCreateStdVector(GMat)
		caseDefCreateStdVector(GCompileArg)
//		caseDefCreateStdVector(void)
		default:
			obj = nullptr;
		}
	}
}

BODY_API void DestroyStdVector(void* p, int vt)
{

#define DefDestroyStdVector(T) static_cast<vector<T>*>(p)->~vector();
#define caseDefDestroyStdVector(T) case vt ## T : DefDestroyStdVector(T) break;

	if (p && vt)
	{
		switch (vt)
		{
		case vtMat:
			static_cast<vector<Mat>*>(p)->~vector();
			break;
		case vtRect:
			static_cast<vector<Rect>*>(p)->~vector();
			break;
		case vtPoint:
			static_cast<vector<Point>*>(p)->~vector();
			break;
		case vtVectorMat:
			static_cast<vector<vector<Mat>>*>(p)->~vector();
			break;
		case vtVectorRect:
			static_cast<vector<vector<Rect>>*>(p)->~vector();
			break;
		case vtVectorPoint:
			static_cast<vector<vector<Point>>*>(p)->~vector();
			break;
		case vtPoint2f:
			static_cast<vector<Point2f>*>(p)->~vector();
			break;
		case vtScalar:
			DefDestroyStdVector(Scalar)
				break;
		case vtuchar:
			DefDestroyStdVector(uchar)
				break;
		case vtfloat:
			DefDestroyStdVector(float)
				break;
		case vtint:
			DefDestroyStdVector(int)
				break;
		case vtVec4i:
			DefDestroyStdVector(Vec4i)
				break;
		caseDefDestroyStdVector(GMat)
		caseDefDestroyStdVector(GCompileArg)
		}
	}
}

BODY_API void StdPushBack(void* p, void* o, int vt)
{

#define defpush_back(T) static_cast<vector<T>*>(p)->push_back(*(static_cast<T*>(o)));
#define casedefpush_back(T) case vt ## T : defpush_back(T) break;

	if (p && o && vt)
	{
		switch (vt)
		{
		case vtMat:
			defpush_back(Mat)
				break;
		case vtRect:
			defpush_back(Rect)
				break;
		case vtPoint:
			defpush_back(Point)
				break;
		case vtVectorMat:
			defpush_back(vector<Mat>)
				break;
		case vtVectorRect:
			defpush_back(vector<Rect>)
				break;
		case vtVectorPoint:
			defpush_back(vector<Point>)
				break;
		case vtPoint2f:
			defpush_back(Point2f)
				break;
		case vtScalar:
			defpush_back(Scalar)
				break;
		case vtuchar:
			defpush_back(uchar)
				break;
		case vtfloat:
			defpush_back(float)
				break;
		case vtint:
			defpush_back(int)
				break;
		case vtVec4i:
			defpush_back(Vec4i)
				break;
		casedefpush_back(GMat)
		casedefpush_back(GCompileArg)
		}
	}
}

BODY_API bool StdEmpty(void* p, int vt)
{

#define DefStdEmpty(T) return static_cast<vector<T>*>(p)->empty();
#define caseDefStdEmpty(T) case vt ## T : DefStdEmpty(T)

	if (p)
	{
		switch (vt)
		{
		case vtMat:
			return static_cast<vector<Mat>*>(p)->empty();
		case vtRect:
			return static_cast<vector<Rect>*>(p)->empty();
		case vtPoint:
			return static_cast<vector<Point>*>(p)->empty();
		case vtVectorMat:
			return static_cast<vector<vector<Mat>>*>(p)->empty();
		case vtVectorRect:
			return static_cast<vector<vector<Rect>>*>(p)->empty();
		case vtVectorPoint:
			return static_cast<vector<vector<Point>>*>(p)->empty();
		case vtPoint2f:
			return static_cast<vector<Point2f>*>(p)->empty();
		case vtScalar:
			DefStdEmpty(Scalar)
		case vtuchar:
			DefStdEmpty(uchar)
		case vtfloat:
			DefStdEmpty(float)
		case vtint:
			DefStdEmpty(int)
		case vtVec4i:
			DefStdEmpty(Vec4i)
		caseDefStdEmpty(GMat)
		caseDefStdEmpty(GCompileArg)
		}
	}
	return true;
}

BODY_API void StdItem(void* p, int vt, unsigned __int64 index, void* dst)
{

#define DefStdItem(T) *(static_cast<T*>(dst)) = (*(static_cast<vector<T>*>(p)))[index];
#define caseDefStdItem(T) case vt ## T : DefStdItem(T) break;

	if (p && dst)
	{
		switch (vt)
		{
		case vtMat:
			*(static_cast<Mat*>(dst)) = (*(static_cast<vector<Mat>*>(p)))[index];
			break;
		case vtRect:
			*(static_cast<Rect*>(dst)) = (*(static_cast<vector<Rect>*>(p)))[index];
			break;
		case vtPoint:
			*(static_cast<Point*>(dst)) = (*(static_cast<vector<Point>*>(p)))[index];
			break;
		case vtVectorMat:
			*(static_cast<vector<Mat>*>(dst)) = (*(static_cast<vector<vector<Mat>>*>(p)))[index];
			break;
		case vtVectorRect:
			*(static_cast<vector<Rect>*>(dst)) = (*(static_cast<vector<vector<Rect>>*>(p)))[index];
			break;
		case vtVectorPoint:
			*(static_cast<vector<Point>*>(dst)) = (*(static_cast<vector<vector<Point>>*>(p)))[index];
			break;
		case vtPoint2f:
			*(static_cast<Point2f*>(dst)) = (*(static_cast<vector<Point2f>*>(p)))[index];
			break;
		case vtScalar:
			DefStdItem(Scalar)
				break;
		case vtuchar:
			DefStdItem(uchar)
				break;
		case vtfloat:
			DefStdItem(float)
				break;
		case vtint:
			DefStdItem(int)
				break;
		case vtVec4i:
			DefStdItem(Vec4i)
				break;
		caseDefStdItem(GMat)
		caseDefStdItem(GCompileArg)
		}
	}
}

BODY_API void StdPItem(void* p, int vt, unsigned __int64 index, void** dst)
{

#define DefStdPItem(T) *dst = (void*)&(*(static_cast<vector<T>*>(p)))[index];
#define caseDefStdPItem(T) case vt ## T : DefStdPItem(T) break;

	if (p && dst)
	{
		switch (vt)
		{
		case vtMat:
			DefStdPItem(Mat)
				break;
		case vtRect:
			DefStdPItem(Rect)
				break;
		case vtPoint:
			DefStdPItem(Point)
				break;
		case vtVectorMat:
			DefStdPItem(vector<vector<Mat>>)
				break;
		case vtVectorRect:
			DefStdPItem(vector<vector<Rect>>)
				break;
		case vtVectorPoint:
			DefStdPItem(vector<vector<Point>>)
				break;
		case vtPoint2f:
			DefStdPItem(vector<vector<Point2f>>)
				break;
		case vtScalar:
			DefStdPItem(Scalar)
				break;
		case vtuchar:
			DefStdPItem(uchar)
				break;
		case vtfloat:
			DefStdPItem(float)
				break;
		case vtint:
			DefStdPItem(int)
				break;
		case vtVec4i:
			DefStdPItem(Vec4i)
				break;
		caseDefStdPItem(GMat)
		caseDefStdPItem(GCompileArg)
		}
	}
}

BODY_API unsigned __int64 StdSize(void* p, int vt)
{

#define DefStdSize(T) return static_cast<vector<T>*>(p)->size();
#define caseDefStdSize(T) case vt ## T : DefStdSize(T)

	if (p)
	{
		switch (vt)
		{
		case vtMat:
			return static_cast<vector<Mat>*>(p)->size();
		case vtRect:
			return static_cast<vector<Rect>*>(p)->size();
		case vtPoint:
			return static_cast<vector<Point>*>(p)->size();
		case vtVectorMat:
			return static_cast<vector<vector<Mat>>*>(p)->size();
		case vtVectorRect:
			return static_cast<vector<vector<Rect>>*>(p)->size();
		case vtVectorPoint:
			return static_cast<vector<vector<Point>>*>(p)->size();
		case vtPoint2f:
			return static_cast<vector<Point2f>*>(p)->size();
		case vtScalar:
			DefStdSize(Scalar)
		case vtuchar:
			DefStdSize(uchar)
		case vtfloat:
			DefStdSize(float)
		case vtint:
			DefStdSize(int)
		case vtVec4i:
			DefStdSize(Vec4i)
		caseDefStdSize(GMat)
		caseDefStdSize(GCompileArg)
		}
	}
	return 0;
}

BODY_API void StdSetItem(void* p, int vt, unsigned __int64 index, void* dst)
{

#define DefStdSetItem(T) (*(static_cast<vector<T>*>(p)))[index] = *(static_cast<T*>(dst));
#define caseDefStdSetItem(T) case vt ## T : DefStdSetItem(T) break;

	if (p && dst)
	{
		switch (vt)
		{
		case vtMat:
			DefStdSetItem(Mat)
				break;
		case vtRect:
			DefStdSetItem(Rect)
				break;
		case vtPoint:
			DefStdSetItem(Point)
				break;
		case vtVectorMat:
			DefStdSetItem(vector<Mat>)
				break;
		case vtVectorRect:
			DefStdSetItem(vector<Rect>)
				break;
		case vtVectorPoint:
			DefStdSetItem(vector<Point>)
				break;
		case vtPoint2f:
			DefStdSetItem(Point2f)
				break;
		case vtScalar:
			DefStdSetItem(Scalar)
				break;
		case vtuchar:
			DefStdSetItem(uchar)
				break;
		case vtfloat:
			DefStdSetItem(float)
				break;
		case vtint:
			DefStdSetItem(int)
				break;
		case vtVec4i:
			DefStdSetItem(Vec4i)
				break;
		caseDefStdSetItem(GMat)
		caseDefStdSetItem(GCompileArg)
		}
	}
}