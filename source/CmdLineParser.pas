unit CmdLineParser;

interface

Type
  TCommandLineParam = record
    switches: TArray<String>;
    Values: String;
    Note: String;
    index: Integer;
    isExists: Boolean;
    isOptional: Boolean;
  end;

  TCommandLineParser = record
  private
    Params: TArray<TCommandLineParam>;
    PosArg: TArray<TCommandLineParam>;
    fabout: string;
    function Cast<T>(const Value: String): T;
    function _TryHas(const Args: TArray<TCommandLineParam>; const key: String; Var index: Integer): Boolean;
    procedure ParseCmdLine; overload;
{$IFDEF DEBUG}
  public
{$ENDIF}
    procedure ParseCmdLine(const Cmd: TArray<String>); overload;
  public
    class operator Implicit(const a: String): TCommandLineParser;
  public
    constructor Create(const AKeys: String);
    procedure about(const a: String);
    procedure printMessage;
    function has(const key: String): Boolean;
    function TryHas(const key: String; Var index: Integer): Boolean;
    function TryHasPosArg(const key: String; Var index: Integer): Boolean;
    function get<T>(const key: String): T; overload;
    function getArrayOf<T>(const key: String): TArray<T>; overload;
    function get<T>(const key: String; const DefaultValue: T): T; overload;
    function TryGet<T>(const key: String; out Value: T): Boolean;
    function check: Boolean;
    procedure printErrors;
  end;

  CommandLineParser = TCommandLineParser;

implementation

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo;

{ TCommandLineParser }

procedure TCommandLineParser.about(const a: String);
begin
  fabout := a;
end;

function TCommandLineParser.Cast<T>(const Value: String): T;
Var
  R: TValue;
begin
  Var
    P: PTypeInfo := TypeInfo(T);
  if TValue(Value).TryCast(TypeInfo(T), R) then
    Result := R.AsType<T>
  else
  begin
    Var
      V: Variant := Value;
    Var
      tV: TValue;
    tV.From<Variant>(V);
    if tV.TryCast(TypeInfo(T), R) then
      Result := R.AsType<T>
    else
    begin
      Assert(False);
      {
        tkInteger
        tkChar
        tkFloat
        tkString
        tkWChar
        tkLString
        tkWString
        tkInt64
        tkUString
      }
    end;
  end;
end;

function TCommandLineParser.check: Boolean;
begin
  for Var i := 0 to High(Params) do
    if (not Params[i].isExists) and (not Params[i].isOptional) then
      Exit(False);
  for Var i := 0 to High(PosArg) do
    if (not PosArg[i].isExists) and (not PosArg[i].isOptional) then
      Exit(False);
  Result := True;
end;

constructor TCommandLineParser.Create(const AKeys: String);
begin
  Params := default (TArray<TCommandLineParam>);
  Var
    a: TArray<string> := AKeys.Split(['{', '}'], MaxInt, TStringSplitOptions.ExcludeEmpty);
  for Var i := 0 To High(a) do
  begin
    Var
      b: TArray<string> := a[i].Split(['|'], MaxInt);
    Assert(Length(b) = 3);
    Var
      P: TCommandLineParam := default (TCommandLineParam);
    P.switches := b[0].Split([' '], MaxInt, TStringSplitOptions.ExcludeEmpty);
    Assert(Length(P.switches) > 0);
    P.Values := b[1].Trim;
    P.isExists := Length(P.Values) > 0;
    P.isOptional := (Length(P.Values) = 0) and (P.switches[0][1] <> '@');
    P.index := -1;
    P.Note := b[2].Trim;
    if P.switches[0][1] = '@' then
      PosArg := PosArg + [P]
    else
      Params := Params + [P];
  end;
  ParseCmdLine;
end;

function TCommandLineParser.get<T>(const key: String): T;
begin
  Result := get<T>(key, default (T));
end;

function TCommandLineParser.get<T>(const key: String; const DefaultValue: T): T;
Var
  index: Integer;
begin
  if not TryGet<T>(key, Result) then
    Result := DefaultValue;
end;

function TCommandLineParser.TryGet<T>(const key: String; out Value: T): Boolean;
Var
  index: Integer;
begin
  Assert(key.Length > 0);
  if key[1] = '@' then
  begin
    Result := TryHasPosArg(Copy(key, 2, key.Length - 1), Index);
    if Result then
      Value := Cast<T>(PosArg[Index].Values); // TValue(PosArg[Index].Values).AsType<T>;
  end
  else
  begin
    Result := TryHas(key, Index);
    if Result then
      Value := Cast<T>(Params[Index].Values); // TValue(Params[Index].Values).AsType<T>;
  end;
end;

function TCommandLineParser.getArrayOf<T>(const key: String): TArray<T>;
Var
  index: Integer;
begin
  Result := default (TArray<T>);
  if TryHas(key, Index) then
  begin
    for var i := 0 to High(Params[Index].Values) do
      Result := Result + [TValue(Params[Index].Values[i]).AsType<T>];
  end;
end;

function TCommandLineParser.TryHas(const key: String; var index: Integer): Boolean;
begin
  Result := _TryHas(Params, key, Index);
end;

function TCommandLineParser.TryHasPosArg(const key: String; var index: Integer): Boolean;
begin
  Result := _TryHas(PosArg, key, Index);
end;

function TCommandLineParser._TryHas(const Args: TArray<TCommandLineParam>; const key: String; var index: Integer): Boolean;
begin
  for Var k := 0 to High(Args) do
    for Var m := 0 to High(Args[k].switches) do
      if (Args[k].switches[m] = key) and (Args[k].isExists) then
      begin
        Index := k;
        Exit(True);
      end;
  Result := False;
end;

function TCommandLineParser.has(const key: String): Boolean;
Var
  index: Integer;
begin
  Result := TryHas(key, Index);
end;

class operator TCommandLineParser.Implicit(const a: String): TCommandLineParser;
begin
  Result := TCommandLineParser.Create(a);
end;

procedure TCommandLineParser.ParseCmdLine;
begin
  if ParamCount > 0 then
  begin
    Var
      Cmd: TArray<String>;
    SetLength(Cmd, ParamCount);
    for Var i := 0 to High(Cmd) do
      Cmd[i] := ParamStr(i + 1);
    ParseCmdLine(Cmd);
  end;
end;

procedure TCommandLineParser.ParseCmdLine(const Cmd: TArray<String>);
label m1;
begin
  SetLength(PosArg, 0);
  for Var i: Integer := 0 to High(Cmd) do
  begin
    Var
      s: String := Cmd[i];
    if s[1] = '-' then
    begin
      while (Length(s) > 0) and (s[1] = '-') do
        Delete(s, 1, 1);
      Assert(s.Length > 0);
      Var
        e: Integer := Pos('=', s);
      Var
        ParamParam: String := default (String);
      if e <> 0 then
      begin
        ParamParam := Copy(s, e + 1, Length(s));
        Delete(s, e, Length(s));
      end;

      for Var k := 0 to High(Params) do
      begin
        for Var m := 0 to High(Params[k].switches) do
          if Params[k].switches[m] = s then
          begin
            Params[k].index := i;
            Params[k].Values := ParamParam.DeQuotedString.DeQuotedString('"');
            Params[k].isExists := True;
            goto m1;
          end;
      end;
    m1:
    end
    else
    begin
      SetLength(PosArg, Length(PosArg) + 1);
      PosArg[High(PosArg)].Values := s;
      PosArg[High(PosArg)].isExists := True;
    end;
  end;
end;

procedure TCommandLineParser.printErrors;
begin
  Var
    ErrPrinted: Boolean := False;
  for Var i := 0 to High(Params) do
    if (not Params[i].isExists) and (not Params[i].isOptional) then
    begin
      if not ErrPrinted then
      begin
        Writeln('ERRORS:');
        ErrPrinted := True;
      end;
      Writeln('Missing parameter: "', Params[i].switches[0], '"');
    end;
  for Var i := 0 to High(PosArg) do
    if (not PosArg[i].isExists) and (not PosArg[i].isOptional) then
    begin
      if not ErrPrinted then
      begin
        Writeln('ERRORS:');
        ErrPrinted := True;
      end;
      Writeln('Missing parameter: "', Copy(PosArg[i].switches[0], 2, PosArg[i].switches[0].Length - 1), '"');
    end;
end;

procedure TCommandLineParser.printMessage;
begin
  Writeln(fabout);
  Write('Usage: ' + ExtractFileName(ParamStr(0)) + ' [params]');
  for Var i := 0 to High(PosArg) do
    Write(' ', PosArg[i].switches[0].Replace('@', ''));
  Writeln;
  Writeln;
  for Var i := 0 to High(Params) do
  begin
    Write(#9);
    for Var j := 0 to High(Params[i].switches) do
    begin
      if j > 0 then
        Write(', ');
      if Params[i].switches[j].Length = 1 then
        write('-')
      else
        write('--');
      Write(Params[i].switches[j]);
    end;
    if Length(Params[i].Values) > 0 then
      Writeln(' (value: ', string.Join(' ', Params[i].Values), ')')
    else
      Writeln;
    Writeln(#9#9, Params[i].Note);
  end;
  // Writeln;
  for Var i := 0 to High(PosArg) do
  begin
    Writeln(#9, PosArg[i].switches[0].Replace('@', ''));
    Writeln(#9#9, PosArg[i].Note);
  end;
end;

end.
