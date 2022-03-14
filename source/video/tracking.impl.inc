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

{$ifdef OPENCV_TRACKING_HPP}

procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray;
  const err: TOutputArray; const winSize: TSize { = Size(21,21) }; maxLevel: Int { = 3 } );
begin
  calcOpticalFlowPyrLK(prevImg, nextImg, prevPts, nextPts, status, err, winSize, maxLevel, TTermCriteria.TermCriteria(TTermCriteria.COUNT + TTermCriteria.EPS, 30, 0.01));
end;

procedure calcOpticalFlowPyrLK(const prevImg: TInputArray; const nextImg: TInputArray; const prevPts: TInputArray; const nextPts: TInputOutputArray; const status: TOutputArray;
  const err: TOutputArray); overload;
begin
  calcOpticalFlowPyrLK(prevImg, nextImg, prevPts, nextPts, status, err, size(21, 21));
end;

{ TDenseOpticalFlow }

procedure TDenseOpticalFlow.calc(const i0, i1: TInputArray; const flow: TInputOutputArray);
Type
  Tcalc = procedure(const Obj: TDenseOpticalFlow; const i0, i1: TInputArray; const flow: TInputOutputArray);
begin
  Tcalc(vftable(Self.vft, $38 div SizeOf(Pointer)))(Self, i0, i1, flow);
end;

class operator TDenseOpticalFlow.Finalize(var Dest: TDenseOpticalFlow);
begin
  Destructor_DenseOpticalFlow(Dest);
end;

{ TDISOpticalFlow }

class function TDISOpticalFlowHelper.Create<T>(preset: Int): TPtr<T>;
begin
  create_DISOpticalFlow(Pointer(@Result), preset);
end;

{ TKalmanFilter }

class function TKalmanFilter.KalmanFilter(const dynamParams, measureParams:int; const controlParams :int = 0; const &type :int= CV_32F ):TKalmanFilter;
begin
   Constructor_KalmanFilter(Result,dynamParams, measureParams,controlParams,&type);
end;

{$endif}