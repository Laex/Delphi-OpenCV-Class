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

{$IF DEFined(OPENCV_TRACKING_HPP) and (not DEFINED(OPENCV_TRACKING_HPP_IMPL))}
{$DEFINE OPENCV_TRACKING_HPP_IMPL}
{ TDenseOpticalFlow }

procedure TDenseOpticalFlow.calc(const i0, i1: TInputArray; const flow: TInputOutputArray);
Type
  Tcalc = procedure(const Obj: TDenseOpticalFlow; const i0, i1: TInputArray; const flow: TInputOutputArray);
begin
  Tcalc(vftable(Self.vft, $38 div SizeOf(Pointer)))(Self, i0, i1, flow);
end;

class operator TDenseOpticalFlow.Finalize(var Dest: TDenseOpticalFlow);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_DENSEOPTICALFLOW(Dest);
{$IFEND}
end;

{ TDISOpticalFlow }

class function TDISOpticalFlowHelper.Create<T>(preset: Int): TPtr<T>;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_DISOPTICALFLOW_create(@Result, preset);
{$IFEND}
end;

class function TDISOpticalFlowHelper.Create(preset: Int = PRESET_FAST): TPtr<TDISOpticalFlow>;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_DISOPTICALFLOW_create(Result, preset);
{$IFEND}
end;

{ TKalmanFilter }

class function TKalmanFilter.KalmanFilter(const dynamParams, measureParams: Int; const controlParams: Int = 0; const &type: Int = CV_32F): TKalmanFilter;
begin
{$IF not defined(PACKAGE)}
  constructor_CV_KALMANFILTER_2(Result, dynamParams, measureParams, controlParams, &type);
{$IFEND}
end;

function TKalmanFilter.predict(const control: TMat { = Mat() } ): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_KALMANFILTER_predict(Self, control)^;
{$IFEND}
end;

function TKalmanFilter.predict(): TMat;
Var
  M: TMat;
begin
  Result := predict(M);
end;

function TKalmanFilter.correct(const measurement: TMat): TMat;
begin
{$IF not defined(PACKAGE)}
  Result := class_func_CV_KALMANFILTER_correct(Self, measurement)^;
{$IFEND}
end;

function CamShift(const probImage: TInputArray; const window: TRect; const criteria: TTermCriteria): TRotatedRect;
begin
{$IF not defined(PACKAGE)}
  Result := func_CamShift(probImage, window, criteria);
{$IFEND}
end;

procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray; const err: TOutputArray; winSize: TSize { = Size(21,21) }; maxLevel: Int { = 3 }; const criteria: TTermCriteria { = TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, 0.01) }; flags: Int = 0; minEigThreshold: double = 1E-4);
begin
{$IF not defined(PACKAGE)}
  proc_calcOpticalFlowPyrLK(prevImg, nextImg, prevPts, nextPts, status, err, winSize, maxLevel, criteria, flags, minEigThreshold);
{$IFEND}
end;

procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray; const err: TOutputArray; winSize: TSize { = Size(21,21) }; maxLevel: Int = 3);
begin
  calcOpticalFlowPyrLK(prevImg, nextImg, prevPts, nextPts, status, err, winSize, maxLevel, TTermCriteria.TermCriteria(TTermCriteria.COUNT + TTermCriteria.EPS, 30, 0.01));
end;

procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray; const err: TOutputArray);
begin
  calcOpticalFlowPyrLK(prevImg, nextImg, prevPts, nextPts, status, err, Size(21, 21));
end;

{$ENDIF}
