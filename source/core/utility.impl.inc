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
{$IF not defined(PACKAGE)}
  Result := func_setBreakOnError(flag);
{$IFEND}
end;

function redirectError(errCallback: TErrorCallback; userdata: Pointer = nil; prevUserdata: Pointer = nil): TErrorCallback;
begin
{$IF not defined(PACKAGE)}
  Result := func_redirectError(errCallback, userdata, prevUserdata);
{$IFEND}
end;

procedure setNumThreads(nthreads: int);
begin
{$IF not defined(PACKAGE)}
  proc_setNumThreads(nthreads);
{$IFEND}
end;

function getNumThreads: int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getNumThreads;
{$IFEND}
end;

function getThreadNum: int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getThreadNum;
{$IFEND}
end;

function getBuildInformation: String;
begin
{$IF not defined(PACKAGE)}
  Result := func_getBuildInformation^;
{$IFEND}
end;

function getVersionString: string;
begin
{$IF not defined(PACKAGE)}
  Result := func_getVersionString;
{$IFEND}
end;

function getVersionMajor: int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getVersionMajor;
{$IFEND}
end;

function getVersionMinor: int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getVersionMinor;
{$IFEND}
end;

function getVersionRevision: int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getVersionRevision;
{$IFEND}
end;

function getTickCount: Int64;
begin
{$IF not defined(PACKAGE)}
  Result := func_getTickCount;
{$IFEND}
end;

function getTickFrequency: Double;
begin
{$IF not defined(PACKAGE)}
  Result := func_getTickFrequency;
{$IFEND}
end;

function getCPUTickCount: Int64;
begin
{$IF not defined(PACKAGE)}
  Result := func_getCPUTickCount;
{$IFEND}
end;

function checkHardwareSupport(feature: int): bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_checkHardwareSupport(feature);
{$IFEND}
end;

function getHardwareFeatureName(feature: int): string;
begin
{$IF not defined(PACKAGE)}
  Result := func_getHardwareFeatureName(feature);
{$IFEND}
end;

function getCPUFeaturesLine: string;
begin
{$IF not defined(PACKAGE)}
  Result := func_getCPUFeaturesLine();
{$IFEND}
end;

function getNumberOfCPUs: int;
begin
{$IF not defined(PACKAGE)}
  Result := func_getNumberOfCPUs;
{$IFEND}
end;

procedure setUseOptimized(onoff: bool);
begin
{$IF not defined(PACKAGE)}
  proc_setUseOptimized(onoff);
{$IFEND}
end;

function useOptimized: bool;
begin
{$IF not defined(PACKAGE)}
  Result := func_useOptimized;
{$IFEND}
end;

function findFileOrKeep(const relative_path: string; silentMode: Boolean = False): string;
begin
{$IF not defined(PACKAGE)}
  Result := func_findFileOrKeep(relative_path, silentMode);
{$IFEND}
end;

procedure addSamplesDataSearchPath(const path: String);
begin
{$IF not defined(PACKAGE)}
  proc_addSamplesDataSearchPath(path);
{$IFEND}
end;

procedure addSamplesDataSearchSubDirectory(const subdir: string);
begin
{$IF not defined(PACKAGE)}
  proc_addSamplesDataSearchSubDirectory(subdir);
{$IFEND}
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

function findFile(const relative_path: string; required: bool = true; silentMode: bool = False): String;
begin
{$IF not defined(PACKAGE)}
  Result := func_findFile(relative_path, required, silentMode);
{$IFEND}
end;

{ AutoBuffer<T> }

class operator AutoBuffer<T>.Initialize(out Dest: AutoBuffer<T>);
begin
  Dest.ptr := AllocMem(1024 div sizeof(T) + 8);
end;

class operator AutoBuffer<T>.Finalize(var Dest: AutoBuffer<T>);
begin
  FreeMem(Dest.ptr);
end;

{$IFEND OPENCV_CORE_UTILITY_H}
