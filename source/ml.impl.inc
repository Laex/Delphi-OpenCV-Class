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

{$IF defined(OPENCV_ML_HPP) and (not defined(OPENCV_ML_HPP_IMPL))}
{$DEFINE OPENCV_ML_HPP_IMPL}
{ TSVM }

class function TSVM.Create: TPtr<TSVM>;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_ML_SVM_create(Result._Ptr);
{$IFEND}
end;

class operator TSVM.Finalize(var Dest: TSVM);
Type
  TVirtualDestructor = procedure(const Obj: TSVM);
begin
  TVirtualDestructor(vftable(Dest.vft, 0))(Dest);
end;

function TSVM.getType: Int;
Type
  TgetType = function(const Obj: TSVM): Int;
begin
  Result := TgetType(vftable(Self.vft, 14))(Self);
end;

function TSVM.getUncompressedSupportVectors: TMat;
Type
  TgetUncompressedSupportVectors = procedure(const Obj: TSVM; const m: TMat);
begin
  TgetUncompressedSupportVectors(vftable(Self.vft, $130 div SizeOf(Pointer)))(Self, Result);
end;

function TSVM.predict(const samples: TInputArray): float;
begin
  Result := predict(samples, TOutputArray.noArray);
end;

function TSVM.predict(const samples: TInputArray; const results: TOutputArray; flags: Int): float;
Type
  Tpredict = function(const Obj: TSVM; const samples: TInputArray; const results: TOutputArray; flags: Int): float;
begin
  Result := Tpredict(vftable(Self.vft, $68 div SizeOf(Pointer)))(Self, samples, results, flags);
end;

procedure TSVM.setKernel(kernelType: Int);
Type
  TsetKernel = procedure(const Obj: TSVM; kernelType: Int);
begin
  TsetKernel(vftable(Self.vft, $108 div SizeOf(Pointer)))(Self, kernelType);
end;

procedure TSVM.setTermCriteria(const val: TTermCriteria);
Type
  TsetTermCriteria = procedure(const Obj: TSVM; const val: TTermCriteria);
begin
  TsetTermCriteria(vftable(Self.vft, $0F8 div SizeOf(Pointer)))(Self, val);
end;

procedure TSVM.setType(val: Int);
Type
  TsetType = procedure(const Obj: TSVM; val: Int);
begin
  TsetType(vftable(Self.vft, 15))(Self, val);
end;

function TSVM.train(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray): BOOL;
Type
  Ttrain = function(const Obj: TSVM; const samples: TInputArray; layout: Int; const responses: TInputArray): BOOL;
begin
  Result := Ttrain(vftable(Self.vft, $50 div SizeOf(Pointer)))(Self, samples, Int(layout), responses);
end;

{ TANN_MLP }

class operator TANN_MLP.Finalize(var Dest: TANN_MLP);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_ML_ANN_MLP(Dest);
{$ifend}
end;

class function TANN_MLP.Create(): TPtr<TANN_MLP>;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_ML_ANN_MLP_create(Result);
{$ifend}
end;

procedure TANN_MLP.setLayerSizes(const _layer_sizes: TInputArray);
Type
  TsetLayerSizes = procedure(const Obj: TANN_MLP; const _layer_sizes: TInputArray);
begin
  TsetLayerSizes(vftable(Self.vft, $88 div SizeOf(Pointer)))(Self, _layer_sizes);
end;

procedure TANN_MLP.setActivationFunction(&type: ActivationFunctions; param1: double = 0; param2: double = 0);
Type
  TsetActivationFunction = procedure(const Obj: TANN_MLP; &type: ActivationFunctions; param1: double = 0; param2: double = 0);
begin
  TsetActivationFunction(vftable(Self.vft, $80 div SizeOf(Pointer)))(Self, &type, param1, param2);
end;

procedure TANN_MLP.setTrainMethod(method: TrainingMethods; param1: double = 0; param2: double = 0);
Type
  TsetTrainMethod = procedure(const Obj: TANN_MLP; method: TrainingMethods; param1: double = 0; param2: double = 0);
begin
  TsetTrainMethod(vftable(Self.vft, $70 div SizeOf(Pointer)))(Self, method, param1, param2);
end;

function TANN_MLP.train(const trainData: TPtr<TTrainData>; flags: Int = 0): BOOL;
Type
  Ttrain = function(const Obj: TANN_MLP; const trainData: TPtr<TTrainData>; flags: Int): BOOL;
begin
  Result := Ttrain(vftable(Self.vft, $58 div SizeOf(Pointer)))(Self, trainData, flags);
end;

function TANN_MLP.isTrained(): BOOL;
Type
  TisTrained = function(const Obj: TANN_MLP): BOOL;
begin
  Result := TisTrained(vftable(Self.vft, $40 div SizeOf(Pointer)))(Self);
end;

function TANN_MLP.predict(const samples: TInputArray; const results: TOutputArray { =noArray() }; flags: Int { =0 } ): float;
Type
  Tpredict = function(const Obj: TANN_MLP; const samples: TInputArray; const results: TOutputArray { =noArray() }; flags: Int { =0 } ): float;
begin
  Result := Tpredict(vftable(Self.vft, $68 div SizeOf(Pointer)))(Self, samples, results, flags);
end;

function TANN_MLP.predict(const samples: TInputArray; const results: TOutputArray { =noArray() } ): float;
begin
  Result := predict(samples, results, 0);
end;

function TANN_MLP.predict(const samples: TInputArray): float;
begin
  Result := predict(samples, TOutputArray.noArray);
end;

{ TTrainData }

class operator TTrainData.Finalize(var Dest: TTrainData);
begin
{$IF not defined(PACKAGE)}
  destructor_CV_ML_TRAINDATA(Dest);
{$ifend}
end;

class function TTrainData.Create(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray; const varIdx: TInputArray { =noArray() }; const sampleIdx: TInputArray { =noArray() }; const sampleWeights: TInputArray { =noArray() }; const varType: TInputArray { =noArray() }
  ): TPtr<TTrainData>;
begin
{$IF not defined(PACKAGE)}
  class_proc_CV_ML_TRAINDATA_create(Result, samples, Int(layout), responses, varIdx, sampleIdx, sampleWeights, varType);
{$ifend}
end;

class function TTrainData.Create(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray; const varIdx: TInputArray { =noArray() }; const sampleIdx: TInputArray { =noArray() }; const sampleWeights: TInputArray { =noArray() } ): TPtr<TTrainData>;
begin
  Result := TTrainData.Create(samples, layout, responses, varIdx, sampleIdx, sampleWeights, TInputArray.noArray);
end;

class function TTrainData.Create(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray; const varIdx: TInputArray { =noArray() }; const sampleIdx: TInputArray { =noArray() }
  ): TPtr<TTrainData>;
begin
  Result := TTrainData.Create(samples, layout, responses, varIdx, sampleIdx, TInputArray.noArray);
end;

class function TTrainData.Create(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray; const varIdx: TInputArray { =noArray() } ): TPtr<TTrainData>;
begin
  Result := TTrainData.Create(samples, layout, responses, varIdx, TInputArray.noArray);
end;

class function TTrainData.Create(const samples: TInputArray; layout: SampleTypes; const responses: TInputArray): TPtr<TTrainData>;
begin
  Result := TTrainData.Create(samples, layout, responses, TInputArray.noArray);
end;

{$IFEND}
