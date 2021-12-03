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

#define BODY_API __declspec(dllexport)

using namespace cv;
using namespace std;

class BODY_API ExportString : public String {};

enum VectorType {
	vtMat           = 1,	// vector<Mat>
	vtRect			= 2,	// vector<Rect>
	vtPoint			= 3,	// vector<Point>
	vtVectorMat		= 4,	// vector<vector<Mat>>
	vtVectorRect	= 5,	// vector<vector<Rect>>
	vtVectorPoint	= 6,	// vector<vector<Point>>
	vtPoint2f		= 7,    // vector<Point2f>
	vtScalar		= 8,    // vector<Scalar>
	vtUchar			= 9,    // vector<uchar>
	vtFloat			= 10,   // vector<float>
	vtInt			= 11,   // vector<int>
	vtVec4i			= 12,   // vector<Vec4i>
};

BODY_API void CopyStdVector(void* obj, void* src, int vt)
{

#define DefCopyStdVector(T) *(static_cast<vector<T>*>(obj))=*(static_cast<vector<T>*>(src));

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
		case vtUchar:
			DefCopyStdVector(uchar)			
			break;
		case vtFloat:
			DefCopyStdVector(float)
			break;
		case vtInt:
			DefCopyStdVector(int)
			break;
		case vtVec4i:
			DefCopyStdVector(Vec4i)
			break;
		}
	}
}

BODY_API void CreateStdVector(void* obj, int vt)
{

#define DefCreateStdVector(T) *(static_cast<vector<T>*>(obj)) = vector<T>();

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
		case vtUchar:
			DefCreateStdVector(uchar)
			break;
		case vtFloat:
			DefCreateStdVector(float)
			break;
		case vtInt:
			DefCreateStdVector(int)
			break;
		case vtVec4i:
			DefCreateStdVector(Vec4i)
			break;
		}
	}
}

BODY_API void DestroyStdVector(void* p, int vt)
{

#define DefDestroyStdVector(T) static_cast<vector<T>*>(p)->~vector();

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
		case vtUchar:
			DefDestroyStdVector(uchar)
			break;
		case vtFloat:
			DefDestroyStdVector(float)
			break;
		case vtInt:
			DefDestroyStdVector(int)
			break;
		case vtVec4i:
			DefDestroyStdVector(Vec4i)
			break;
		}
	}
}

BODY_API void StdPushBack(void* p, void* o, int vt)
{

#define defpush_back(T) static_cast<vector<T>*>(p)->push_back(*(static_cast<T*>(o)));

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
		case vtUchar:
			defpush_back(uchar)
			break;
		case vtFloat:
			defpush_back(float)
			break;
		case vtInt:
			defpush_back(int)
			break;
		case vtVec4i:
			defpush_back(Vec4i)
			break;
		}
	}
}

BODY_API bool StdEmpty(void* p, int vt)
{
	
#define DefStdEmpty(T) static_cast<vector<T>*>(p)->empty();

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
			return DefStdEmpty(Scalar)
		case vtUchar:
			return DefStdEmpty(uchar)			
		case vtFloat:
			return DefStdEmpty(float)
		case vtInt:
			return DefStdEmpty(int)
		case vtVec4i:
			return DefStdEmpty(Vec4i)
		}
	}
	return true;
}

BODY_API void StdItem(void* p, int vt, unsigned __int64 index, void* dst)
{

#define DefStdItem(T) *(static_cast<T*>(dst)) = (*(static_cast<vector<T>*>(p)))[index];

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
		case vtUchar:
			DefStdItem(uchar)
			break;
		case vtFloat:
			DefStdItem(float)
			break;
		case vtInt:
			DefStdItem(int)
			break;
		case vtVec4i:
			DefStdItem(Vec4i)
			break;
		}
	}
}

BODY_API void StdPItem(void* p, int vt, unsigned __int64 index, void** dst)
{

#define DefStdPItem(T) *dst = (void*)&(*(static_cast<vector<T>*>(p)))[index];

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
		case vtUchar:
			DefStdPItem(uchar)
				break;
		case vtFloat:
			DefStdPItem(float)
				break;
		case vtInt:
			DefStdPItem(int)
				break;
		case vtVec4i:
			DefStdPItem(Vec4i)
				break;
		}
	}
}

BODY_API unsigned __int64 StdSize(void* p, int vt)
{

#define DefStdSize(T) static_cast<vector<T>*>(p)->size();

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
			return DefStdSize(Scalar)
		case vtUchar:
			return DefStdSize(uchar)
		case vtFloat:
			return DefStdSize(float)
		case vtInt:
			return DefStdSize(int)
		case vtVec4i:
			return DefStdSize(Vec4i)
		}
	}
	return 0;
}

BODY_API void StdSetItem(void* p, int vt, unsigned __int64 index, void* dst)
{

#define DefStdSetItem(T) (*(static_cast<vector<T>*>(p)))[index] = *(static_cast<T*>(dst))  ;

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
		case vtUchar:
			DefStdSetItem(uchar)
			break;
		case vtFloat:
			DefStdSetItem(float)
			break;
		case vtInt:
			DefStdSetItem(int)
			break;
		case vtVec4i:
			DefStdSetItem(Vec4i)
			break;
		}
	}
}