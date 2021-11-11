#pragma once

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include "opencv2\opencv.hpp"

#define BODY_API __declspec(dllexport)

using namespace cv;
using namespace std;

class BODY_API ExportString : public String {};

enum VectorType {
	vtMat = 1,			// vector<Mat>
	vtRect = 2,			// vector<Rect>
	vtPoint = 3,		// vector<Point>
	vtVectorMat = 4,	// vector<vector<Mat>>
	vtVectorRect = 5,	// vector<vector<Rect>>
	vtVectorPoint = 6,	// vector<vector<Point>>
	vtPoint2f = 7 // vector<Point2f>
};

BODY_API void CreateStdVector(void* obj, int vt)
{
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
		}
	}
}

BODY_API void DestroyStdVector(void* p, int vt)
{
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
		}
	}
}

BODY_API bool StdEmpty(void* p, int vt)
{
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
			
		}
	}
	return true;
}

BODY_API void StdItem(void* p, int vt, unsigned __int64 index, void* dst)
{
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
		}
	}
}

BODY_API unsigned __int64 StdSize(void* p, int vt)
{
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
		}
	}
	return 0;
}
