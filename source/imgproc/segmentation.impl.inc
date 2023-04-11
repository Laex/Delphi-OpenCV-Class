(*
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
*)

{$IF DEFINED(OPENCV_IMGPROC_SEGMENTATION_HPP) and (not DEFINED(OPENCV_IMGPROC_SEGMENTATION_HPP_IMPL))}
{$DEFINE OPENCV_IMGPROC_SEGMENTATION_HPP_IMPL}

{ TIntelligentScissorsMB }

function TIntelligentScissorsMB.applyImage(const image: TInputArray): pIntelligentScissorsMB;
begin
{$IF not defined(PACKAGE)}
  Result := pIntelligentScissorsMB(class_func_CV_SEGMENTATION_INTELLIGENTSCISSORSMB_applyImage(Self, image));
{$IFEND}
end;

procedure TIntelligentScissorsMB.buildMap(const sourcePt: TPoint);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_SEGMENTATION_INTELLIGENTSCISSORSMB_buildMap(Self, @sourcePt);
{$IFEND}
end;

class operator TIntelligentScissorsMB.Finalize(var Dest: TIntelligentScissorsMB);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_SEGMENTATION_INTELLIGENTSCISSORSMB(Dest);
{$IFEND}
end;

procedure TIntelligentScissorsMB.getContour(const targetPt: TPoint; const contour: TOutputArray; backward: BOOL);
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_SEGMENTATION_INTELLIGENTSCISSORSMB_getContour(Self, @targetPt, contour, backward);
{$IFEND}
end;

class operator TIntelligentScissorsMB.Initialize(out Dest: TIntelligentScissorsMB);
begin
{$IF not defined(PACKAGE)}
  constructor_CV_SEGMENTATION_INTELLIGENTSCISSORSMB_2(Dest);
{$IFEND}
end;

function TIntelligentScissorsMB.setEdgeFeatureCannyParameters(threshold1, threshold2: double; apertureSize: Int; L2gradient: BOOL): pIntelligentScissorsMB;
begin
{$IF not defined(PACKAGE)}
  Result := pIntelligentScissorsMB(class_func_CV_SEGMENTATION_INTELLIGENTSCISSORSMB_setEdgeFeatureCannyParameters(Self, threshold1, threshold2, apertureSize, L2gradient));
{$IFEND}
end;

function TIntelligentScissorsMB.setGradientMagnitudeMaxLimit(gradient_magnitude_threshold_max: float): pIntelligentScissorsMB;
begin
{$IF not defined(PACKAGE)}
  Result := pIntelligentScissorsMB(class_func_CV_SEGMENTATION_INTELLIGENTSCISSORSMB_setGradientMagnitudeMaxLimit(Self, gradient_magnitude_threshold_max));
{$IFEND}
end;

{$IFEND}
