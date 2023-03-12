(*
  This file is part of Delphi-OpenCV-Class project.
  https://github.com/Laex/Delphi-OpenCV-Class

  It is subject to the license terms in the LICENSE file found in the top-level directory
  of this distribution and at https://www.apache.org/licenses/LICENSE-2.0.txt

  Copyright 2021, Laentir Valetov, laex@bk.ru

  Licensed under the Apache License, Version 2.0 (the 'License');
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an 'AS IS' BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

{$IF defined(OPENCV_CORE_UTILITY_H) and (not defined(OPENCV_CORE_UTILITY_H_IMPL))}
{$DEFINE OPENCV_CORE_UTILITY_H_IMPL}

function setBreakOnError(flag: bool): bool;
begin
  Result := func_setBreakOnError(flag);
end;

function redirectError(errCallback: TErrorCallback; userdata: Pointer = nil; prevUserdata: Pointer = nil): TErrorCallback;
begin
  Result := func_redirectError(errCallback, userdata, prevUserdata);
end;

procedure setNumThreads(nthreads: int);
begin
  proc_setNumThreads(nthreads);
end;

function getNumThreads: int;
begin
  Result := func_getNumThreads;
end;

function getThreadNum: int;
begin
  Result := func_getThreadNum;
end;

function getBuildInformation: String;
begin
  Result := func_getBuildInformation^;
end;

function getVersionString: string;
begin
  Result := func_getVersionString;
end;

function getVersionMajor: int;
begin
  Result := func_getVersionMajor;
end;

function getVersionMinor: int;
begin
  Result := func_getVersionMinor;
end;

function getVersionRevision: int;
begin
  Result := func_getVersionRevision;
end;

function getTickCount: Int64;
begin
  Result := func_getTickCount;
end;

function getTickFrequency: Double;
begin
  Result := func_getTickFrequency;
end;

function getCPUTickCount: Int64;
begin
  Result := func_getCPUTickCount;
end;

function checkHardwareSupport(feature: int): bool;
begin
  Result := func_checkHardwareSupport(feature);
end;

function getHardwareFeatureName(feature: int): string;
begin
  Result := func_getHardwareFeatureName(feature);
end;

function getCPUFeaturesLine: string;
begin
  Result := func_getCPUFeaturesLine();
end;

function getNumberOfCPUs: int;
begin
  Result := func_getNumberOfCPUs;
end;

procedure setUseOptimized(onoff: bool);
begin
  proc_setUseOptimized(onoff);
end;

function useOptimized: bool;
begin
  Result := func_useOptimized;
end;

function findFileOrKeep(const relative_path:string; silentMode:Boolean = False):string;
begin
 Result := func_findFileOrKeep(relative_path,silentMode);
end;

procedure addSamplesDataSearchPath(const path: String);
begin
  proc_addSamplesDataSearchPath(path);
end;

procedure addSamplesDataSearchSubDirectory(const subdir:string);
begin
  proc_addSamplesDataSearchSubDirectory(subdir);
end;


{ TTickMeter }

class operator _TickMeter.Initialize(out Dest: _TickMeter);
begin
  Dest.reset();
end;

procedure _TickMeter.start();
begin
  startTime := getTickCount();
end;

procedure _TickMeter.stop();
begin
  Var
    time: Int64 := getTickCount();
  if (startTime = 0) then
    Exit;
  Inc(counter);
  sumTime := sumTime + (time - startTime);
  startTime := 0;
end;

function _TickMeter.getTimeTicks(): Int64;
begin
  Result := sumTime;
end;

function _TickMeter.getTimeMicro(): Double;
begin
  Result := getTimeMilli() * 1E3;
end;

function _TickMeter.getTimeMilli(): Double;
begin
  Result := getTimeSec() * 1E3;
end;

function _TickMeter.getTimeSec(): Double;
begin
  Result := getTimeTicks() / getTickFrequency();
end;

function _TickMeter.getCounter(): Int64;
begin
  Result := counter;
end;

function _TickMeter.getFPS(): Double;
begin
  Var
    sec: Double := getTimeSec();
  if (sec < DBL_EPSILON) then
    Exit(0);
  Result := counter / sec;
end;

function _TickMeter.getAvgTimeSec(): Double;
begin
  if (counter <= 0) then
    Exit(0);
  Result := getTimeSec() / counter;
end;

function _TickMeter.getAvgTimeMilli(): Double;
begin
  Result := getAvgTimeSec() * 1E3;
end;

procedure _TickMeter.reset();
begin
  startTime := 0;
  sumTime := 0;
  counter := 0;
end;

{ TCommandLineParser }

function findFile(const relative_path: string; required: bool = true; silentMode: bool = false): String;
begin
  Result := func_findFile(relative_path, required, silentMode);
end;

{$IFEND OPENCV_CORE_UTILITY_H}
