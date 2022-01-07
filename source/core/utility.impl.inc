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

{$IFDEF OPENCV_CORE_UTILITY_H}

function getBuildInformation: String;
begin
  Result := _getBuildInformation^;
end;

{ TCommandLineParser }

function TCommandLineParser.getDefaultByName(const name: string; out R: string): boolean;
Var
  i: Integer;
begin
  if getIndexByName(name, i) and (Length(Value[i]) > 0) then
  begin
    if Length(CmdValue[i]) > 0 then
      R := CmdValue[i]
    else
      R := Value[i];
    Exit(True);
  end;
  Result := False;
end;

function TCommandLineParser.getIndexByName(const name: string; out Index: Integer): boolean;
Var
  i, j: Integer;
begin
  for i := 0 to High(CmdParam) do
    for j := 0 to High(CmdParam[i]) do
      if CmdParam[i][j] = name then
      begin
        Index := i;
        Exit(True);
      end;
  Result := False;
end;

function TCommandLineParser.getIndexInput(out Index: Integer): boolean;
Var
  i, j: Integer;
begin
  for i := 0 to High(CmdParam) do
    for j := 0 to High(CmdParam[i]) do
      if (Length(CmdParam[i][j]) > 0) and (CmdParam[i][j][1] = '@') then
      begin
        Index := i;
        Exit(True);
      end;
  Result := False;
end;

procedure TCommandLineParser.help;
var
  i, j: Integer;
  n: boolean;
begin
  if Length(FAbout) > 0 then
    WriteLn(FAbout);
  Write('Usage: ', argv[0]);
  if Length(CmdParam) > 0 then
  begin
    n := getIndexInput(i);
    if (Length(CmdParam) > 1) or (not n) then
      Write(' [params] ');
    if n then
    begin
      if Length(Value[i]) <> 0 then
        Write('[')
      else
        Write('<');
      if Length(Info[i]) <> 0 then
        Write(Info[i])
      else
        Write('input');
      if Length(Value[i]) <> 0 then
        write(' default:', Value[i], ']')
      else
        write('>');
    end;
  end;
  WriteLn;
  WriteLn;

  for i := 0 to High(CmdParam) do
    if (Length(CmdParam[i]) > 0) and (CmdParam[i][0][1] <> '@') then
    begin
      write(#9);
      for j := 0 to High(CmdParam[i]) do
      begin
        Write('-', CmdParam[i][j]);
        if j < High(CmdParam[i]) then
          Write(', ');
      end;
      if Length(Value[i]) > 0 then
        write(' (default:', Value[i], ')');
      WriteLn;
      if Length(Info[i]) > 0 then
        printf(#9#9 + Info[i] + #13#10);
    end;
end;

function TCommandLineParser.get<T>(const name: string): T;
Var
  R: string;
begin
  Result := default (T);
  if getDefaultByName(name, R) then
  begin
    if TypeInfo(T) = TypeInfo(Int) then
    begin
      var
        Code: Integer;
      Val(R, pInt(@Result)^, Code);
    end
    else if TypeInfo(T) = TypeInfo(string) then
    begin
      Result := TValue(R).AsType<T>;
    end
    else if TypeInfo(T) = TypeInfo(float) then
    begin
      var
        Code: Integer;
      Val(R, pfloat(@Result)^, Code);
    end
    else if TypeInfo(T) = TypeInfo(double) then
    begin
      var
        Code: Integer;
      Val(R, pdouble(@Result)^, Code);
    end
    else if TypeInfo(T) = TypeInfo(bool) then
    begin
      if (R = '1') or (R = 'true') or (R = 'TRUE') then
        Result := TValue(True).AsType<T>
      else
        Result := TValue(False).AsType<T>;
    end;
  end;
end;

class operator TCommandLineParser.Implicit(const keys: String): TCommandLineParser;
begin
  Result := TCommandLineParser.Init(keys);
end;

class function TCommandLineParser.Init(const keys: String): TCommandLineParser;
begin
  Result.InternalInit(keys);
end;

function TCommandLineParser.Trim(const Text: String): String;
begin
  Result := Text;
  While (Length(Result) > 0) and ((Result[1] = #9) or (Result[1] = ' ')) do
    Delete(Result, 1, 1);
  While (Length(Result) > 0) and ((Result[Length(Result)] = #9) or (Result[Length(Result)] = ' ')) do
    Delete(Result, Length(Result), 1);
end;

procedure TCommandLineParser.InternalInit(const keys: String);
Var
  i, j, k: Integer;
  s: string;
  i1, j1, k1: Integer;
  s1, s2, s3: string;
  T: string;
  C: TArray<string>;
begin
  SetLength(CmdParam, 0);
  SetLength(Value, 0);
  SetLength(Info, 0);

  i := 1;
  While i <= Length(keys) do
  begin
    j := Pos('{', keys, i);
    if j > 0 then
    begin
      k := Pos('}', keys, j + 1);
      if k > 0 then
      begin
        s := Trim(Copy(keys, j + 1, k - j - 1));
        i1 := Pos('|', s);
        if i1 > 0 then
        begin
          s1 := Trim(Copy(s, 1, i1 - 1));
          j1 := Pos('|', s, i1 + 1);
          if j1 > 0 then
          begin
            s2 := Trim(Copy(s, i1 + 1, j1 - i1 - 1));
            s3 := Trim(Copy(s, j1 + 1, Length(s)));

            SetLength(C, 0);

            while Length(s1) > 0 do
            begin
              k1 := Pos(' ', s1);
              if k1 > 0 then
              begin
                T := Trim(Copy(s1, 1, k1 - 1));
                Delete(s1, 1, k1);
              end
              else
              begin
                T := s1;
                SetLength(s1, 0);
              end;
              C := C + [T];
            end;

            CmdParam := CmdParam + [C];
            Value := Value + [s2];
            Info := Info + [s3];
          end;
        end;
      end
      else
        Break;
    end
    else
      Break;
    i := k + 1;
  end;

  SetLength(CmdValue, Length(Value));

  ParseCommand;
end;

procedure TCommandLineParser.ParseCommand;
Var
  i, j: Integer;
  s: string;
begin
  i := 1;
  While i <= ParamCount do
  begin
    s := ParamStr(i);
    if s[1] <> '-' then
    begin
      if getIndexInput(j) then
        CmdValue[j] := s;
    end
    else
    begin
      while (Length(s) > 0) and (s[1] = '-') do
        Delete(s, 1, 1);
      if (Length(s) > 0) and getIndexByName(s, j) then
      begin
        if i < ParamCount then
          CmdValue[j] := ParamStr(i + 1);
        Inc(i);
      end;
    end;
    Inc(i);
  end;
end;

procedure TCommandLineParser.About(const Text: String);
begin
  FAbout := Text;
end;

function TCommandLineParser.has(const name: String): boolean;
var
  i: Integer;
begin
  Result := getIndexByName(Name, i);
end;

procedure TCommandLineParser.printMessage();
begin
  help;
end;

{$ENDIF OPENCV_CORE_UTILITY_H}
