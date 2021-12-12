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

{$IFDEF OPENCV_ML_HPP}
  { TSVM }

class function TSVM.Create: TPtr<TSVM>;
begin
  create_SVM(Result._Ptr);
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

{$ENDIF}
