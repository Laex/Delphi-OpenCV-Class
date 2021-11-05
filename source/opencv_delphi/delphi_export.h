#pragma once

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include "opencv2\opencv.hpp"

#define BODY_API __declspec(dllexport)

using namespace cv;

class BODY_API ExportString : public String {};
class BODY_API ExportVectorRect : public std::vector<Rect> {};
class BODY_API ExportMatRect : public std::vector<Mat> {};