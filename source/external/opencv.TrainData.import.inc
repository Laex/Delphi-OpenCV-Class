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

{
1792
??1TrainData@ml@cv@@UEAA@XZ
??1TrainData@ml@cv@@UEAA@XZ
cv::ml::TrainData::~TrainData(void)

}
procedure destructor_TrainData(Var obj:TTrainData);
external opencv_world_dll name '??1TrainData@ml@cv@@UEAA@XZ'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
{
4252
?create@TrainData@ml@cv@@SA?AU?$Ptr@VTrainData@ml@cv@@@3@AEBV_InputArray@debug_build_guard@3@H00000@Z
?create@TrainData@ml@cv@@SA?AU?$Ptr@VTrainData@ml@cv@@@3@AEBV_InputArray@3@H00000@Z
struct cv::Ptr<class cv::ml::TrainData> cv::ml::TrainData::create(class cv::_InputArray const &,int,class cv::_InputArray const &,class cv::_InputArray const &,class cv::_InputArray const &,class cv::_InputArray const &,class cv::_InputArray const &)
}
procedure create_TrainData(Var obj: TPtr<TTrainData>;
    const samples:TInputArray; layout:SampleTypes;const responses:TInputArray;
    const varIdx:TInputArray {=noArray()};const sampleIdx:TInputArray {=noArray()};
    const sampleWeights:TInputArray{=noArray()};const varType:TInputArray{=noArray()});
overload; external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
name '?create@TrainData@ml@cv@@SA?AU?$Ptr@VTrainData@ml@cv@@@3@AEBV_InputArray@debug_build_guard@3@H00000@Z'
{$ELSE}
name '?create@TrainData@ml@cv@@SA?AU?$Ptr@VTrainData@ml@cv@@@3@AEBV_InputArray@3@H00000@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
