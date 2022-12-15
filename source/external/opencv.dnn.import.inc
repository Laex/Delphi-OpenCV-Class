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
858	
??0Net@dnn4_v20211004@dnn@cv@@QEAA@XZ	
??0Net@dnn4_v20211004@dnn@cv@@QEAA@XZ	
cv::dnn::dnn4_v20211004::Net::Net(void)
}
procedure constructor_Net(Var obj:TNet);
external opencv_world_dll name '??0Net@dnn4_v20211004@dnn@cv@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
1665	
??1Net@dnn4_v20211004@dnn@cv@@QEAA@XZ	
??1Net@dnn4_v20211004@dnn@cv@@QEAA@XZ	
cv::dnn::dnn4_v20211004::Net::~Net(void)

}
procedure destructor_Net(Var obj:TNet);
external opencv_world_dll name '??1Net@dnn4_v20211004@dnn@cv@@QEAA@XZ'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
6370	
?setPreferableBackend@Net@dnn4_v20211004@dnn@cv@@QEAAXH@Z
?setPreferableBackend@Net@dnn4_v20211004@dnn@cv@@QEAAXH@Z
void cv::dnn::dnn4_v20211004::Net::setPreferableBackend(int)
}
procedure setPreferableBackend_Net(const Obj:TNet;backendId:int);
external opencv_world_dll name '?setPreferableBackend@Net@dnn4_v20211004@dnn@cv@@QEAAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
6372	
?setPreferableTarget@Net@dnn4_v20211004@dnn@cv@@QEAAXH@Z	
?setPreferableTarget@Net@dnn4_v20211004@dnn@cv@@QEAAXH@Z	
void cv::dnn::dnn4_v20211004::Net::setPreferableTarget(int)
}
procedure setPreferableTarget_Net(const Obj:TNet;targetId:int);
external opencv_world_dll name '?setPreferableTarget@Net@dnn4_v20211004@dnn@cv@@QEAAXH@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
6320
?setInput@Net@dnn4_v20211004@dnn@cv@@QEAAXAEBV_InputArray@debug_build_guard@4@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@NAEBV?$Scalar_@N@4@@Z
?setInput@Net@dnn4_v20211004@dnn@cv@@QEAAXAEBV_InputArray@4@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@NAEBV?$Scalar_@N@4@@Z
void cv::dnn::dnn4_v20211004::Net::setInput(
     class cv::_InputArray const &,
     class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,
     double,
     class cv::Scalar_<double> const &)
}
procedure setInput_Net(const Obj:TNet; const blob:TInputArray; const name : CppString;
                            scalefactor :double; const mean :TScalar);
overload; external opencv_world_dll
{$IFDEF USE_DEBUG_DLLS}
  name '?setInput@Net@dnn4_v20211004@dnn@cv@@QEAAXAEBV_InputArray@debug_build_guard@4@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@NAEBV?$Scalar_@N@4@@Z'
{$ELSE}
  name '?setInput@Net@dnn4_v20211004@dnn@cv@@QEAAXAEBV_InputArray@4@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@NAEBV?$Scalar_@N@4@@Z'
{$ENDIF}
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};

{
4799
?forward@Net@dnn4_v20211004@dnn@cv@@QEAA?AVMat@4@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z
?forward@Net@dnn4_v20211004@dnn@cv@@QEAA?AVMat@4@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z
class cv::Mat cv::dnn::dnn4_v20211004::Net::forward(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &)
}
function forward_Net(const Obj:TNet;const outputName :CppString):TMat;
external opencv_world_dll name '?forward@Net@dnn4_v20211004@dnn@cv@@QEAA?AVMat@4@AEBV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@@Z'
{$IFDEF DELAYED_LOAD_DLL} delayed{$ENDIF};
