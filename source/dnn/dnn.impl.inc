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

{$IF defined(OPENCV_DNN_DNN_HPP) and (not defined(OPENCV_DNN_DNN_HPP_IMPL))}
{$DEFINE OPENCV_DNN_DNN_HPP_IMPL}
{ TNet }

class operator TNet.Initialize(out Dest: TNet); // CV_WRAP Net();  //!< Default constructor.
begin
  constructor_CV_DNN_DNN_NET_1(Dest);
end;

class operator TNet.Finalize(var Dest: TNet); // CV_WRAP ~Net(); //!< Destructor frees the net only if there aren't references to the net anymore.
begin
  destructor_CV_DNN_DNN_NET(Dest);
end;

function TNet.forward(const outputName: String = ''): TMat;
begin
  class_func_CV_DNN_DNN_NET_forward(Self, Result, outputName);
  // Result := class_func_CV_DNN_DNN_NET_forward(Self, CppString(outputName));
end;

procedure TNet.setPreferableBackend(backendId: int);
begin
  class_proc_CV_DNN_DNN_NET_setPreferableBackend(Self, backendId);
end;

procedure TNet.setPreferableTarget(targetId: int);
begin
  class_proc_CV_DNN_DNN_NET_setPreferableTarget(Self, targetId);
end;

procedure TNet.setInput(const blob: TInputArray; const name: String { = '' }; scalefactor: double { = 1.0 }; const mean: TScalar { = Scalar() } );
begin
  class_proc_CV_DNN_DNN_NET_setInput(Self, blob, name, scalefactor, mean);
end;

procedure TNet.setInput(const blob: TInputArray; const name: String { = '' }; scalefactor: double = 1.0);
begin
  setInput(blob, name, scalefactor, Scalar());
end;

procedure TNet.setInput(const blob: TInputArray);
begin
  setInput(blob, '');
end;

// --------------------------

function blobFromImages(const images: TInputArrayOfArrays; scalefactor: double { =1.0 }; const size: TSize { = Size() }; const mean: TScalar { = Scalar() }; swapRB: bool = false; crop: bool = false; ddepth: int = CV_32F): TMat;
begin
  Result := func_blobFromImages(images, scalefactor, size, mean, swapRB, crop, ddepth);
end;

function blobFromImages(const images: TInputArrayOfArrays; scalefactor: double { =1.0 }; const size: TSize { = Size() }
  ): TMat;
begin
  Result := blobFromImages(images, scalefactor, size, Scalar());
end;

function blobFromImages(const images: TInputArrayOfArrays; scalefactor: double = 1.0): TMat;
begin
  Result := blobFromImages(images, scalefactor, size());
end;

procedure blobFromImages(const images: TInputArrayOfArrays; const blob: TOutputArray; scalefactor: double { =1.0 }; const size: TSize { = Size() }; const mean: TScalar { = Scalar() }; swapRB: bool = false; crop: bool = false; ddepth: int = CV_32F);
begin
  proc_blobFromImages(images, blob, scalefactor, size, mean, swapRB, crop, ddepth);
end;

procedure blobFromImages(const images: TInputArrayOfArrays; const blob: TOutputArray; scalefactor: double { =1.0 }; const size: TSize { = Size() }
  );
begin
  blobFromImages(images, blob, scalefactor, size, Scalar());
end;

procedure blobFromImages(const images: TInputArrayOfArrays; const blob: TOutputArray; scalefactor: double = 1.0);
begin
  blobFromImages(images, blob, scalefactor, size());
end;

function readNet(const model: String; const config: String = ''; const framework: String = ''): TNet;
begin
  Result := func_readNet(CppString(model), CppString(config), CppString(framework));
end;

{ TModel }

procedure _Model.setInputParams(scale: double { = 1.0 }; const size: TSize { = Size() }; const mean: TScalar { = Scalar() }; swapRB: bool = false; crop: bool = false);
begin
  class_proc_CV_DNN_DNN_MODEL_setInputParams(Self, scale, size, mean, swapRB, crop);
end;

{ TTextRecognitionModel }

class operator _TextRecognitionModel.Initialize(out Dest: _TextRecognitionModel);
begin
  constructor_CV_DNN_DNN_TEXTRECOGNITIONMODEL_4(Dest);
end;

class operator _TextRecognitionModel.Finalize(var Dest: _TextRecognitionModel);
begin
  destructor_CV_DNN_DNN_TEXTRECOGNITIONMODEL(Dest);
end;

constructor _TextRecognitionModel.TextRecognitionModel(const network: _Net);
begin
  constructor_CV_DNN_DNN_TEXTRECOGNITIONMODEL_3(Self, network);
end;

constructor _TextRecognitionModel.TextRecognitionModel(const model: CppString; const config: CppString);
begin
  constructor_CV_DNN_DNN_TEXTRECOGNITIONMODEL_2(Self, model, config);
end;

constructor _TextRecognitionModel.TextRecognitionModel(const model: CppString);
begin
  _TextRecognitionModel.TextRecognitionModel(CppString(model), CppString(''));
end;

constructor _TextRecognitionModel.TextRecognitionModel(const model: String; const config: String = '');
begin
  _TextRecognitionModel.TextRecognitionModel(CppString(model), CppString(config));
end;

class operator _TextRecognitionModel.Implicit(const model: String): _TextRecognitionModel;
begin
  Result := _TextRecognitionModel.TextRecognitionModel(model);
end;

function _TextRecognitionModel.setVocabulary(const vocabulary: vector<CppString>): pTextRecognitionModel;
begin
  Result := class_func_CV_DNN_DNN_TEXTRECOGNITIONMODEL_setVocabulary(Self, vocabulary);

end;

function _TextRecognitionModel.setDecodeType(const decodeType: CppString): pTextRecognitionModel;
begin
  Result := class_func_CV_DNN_DNN_TEXTRECOGNITIONMODEL_setDecodeType(Self, decodeType);
end;

procedure _TextRecognitionModel.setInputParams(scale: double { = 1.0 }; const size: TSize { = Size() }; const mean: TScalar { = Scalar() }; swapRB: bool = false; crop: bool = false);
begin
  class_proc_CV_DNN_DNN_MODEL_setInputParams(Self.model, scale, size, mean, swapRB, crop);
end;

procedure _TextRecognitionModel.setInputParams(scale: double { = 1.0 }; const size: TSize { = Size() } );
begin
  setInputParams(scale, size, Scalar());
end;

procedure _TextRecognitionModel.setInputParams(scale: double = 1.0);
begin
  setInputParams(scale, size());
end;

function _TextRecognitionModel.recognize(const frame: TInputArray): String;
begin
  Result := class_func_CV_DNN_DNN_TEXTRECOGNITIONMODEL_recognize(Self, frame);
end;

{$IFEND}
