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
4244	
?create@Stitcher@cv@@SA?AU?$Ptr@VStitcher@cv@@@2@W4Mode@12@@Z	
?create@Stitcher@cv@@SA?AU?$Ptr@VStitcher@cv@@@2@W4Mode@12@@Z	
struct cv::Ptr<class cv::Stitcher> cv::Stitcher::create(enum cv::Stitcher::Mode)
}
procedure create_Stitcher(Var obj: TPtr<TStitcher>.pT; const mode : TStitcher.Mode);
external opencv_world_dll name '?create@Stitcher@cv@@SA?AU?$Ptr@VStitcher@cv@@@2@W4Mode@12@@Z'{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

//6550	?stitch@Stitcher@cv@@QEAA?AW4Status@12@AEBV_InputArray@debug_build_guard@2@0AEBV_OutputArray@52@@Z	?stitch@Stitcher@cv@@QEAA?AW4Status@12@AEBV_InputArray@2@0AEBV_OutputArray@2@@Z	enum cv::Stitcher::Status cv::Stitcher::stitch(class cv::_InputArray const &,class cv::_InputArray const &,class cv::_OutputArray const &)
{
6551
?stitch@Stitcher@cv@@QEAA?AW4Status@12@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@52@@Z
?stitch@Stitcher@cv@@QEAA?AW4Status@12@AEBV_InputArray@2@AEBV_OutputArray@2@@Z
enum cv::Stitcher::Status cv::Stitcher::stitch(class cv::_InputArray const &,class cv::_OutputArray const &)
}
function stitch_Stitcher(const Obj:TStitcher;const images:TInputArrayOfArrays;const pano:TOutputArray):TStitcher.Status;
overload; external opencv_world_dll
{$IFDEF DEBUG}
  name '?stitch@Stitcher@cv@@QEAA?AW4Status@12@AEBV_InputArray@debug_build_guard@2@AEBV_OutputArray@52@@Z'
{$ELSE}
  name '?stitch@Stitcher@cv@@QEAA?AW4Status@12@AEBV_InputArray@2@AEBV_OutputArray@2@@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
