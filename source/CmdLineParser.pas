unit CmdLineParser;

interface

Type
  TCommandLineParam = record
    switches: TArray<String>;
    Values: TArray<String>;
    Note: String;
    index: Integer;
    isExists: Boolean;
  end;

  TCommandLineParser = record
  private
    Params: TArray<TCommandLineParam>;
    PosArg: TArray<TCommandLineParam>;
    fabout: string;
    procedure ParseCmdLine;
    function _TryHas(const Args: TArray<TCommandLineParam>; const key: String; Var index: Integer): Boolean;
    function Cast<T>(const Value: String): T;
  public
    constructor Create(const AKeys: String);
    class operator Implicit(const a: String): TCommandLineParser;
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
    procedure printError;
  end;

  CommandLineParser = TCommandLineParser;

implementation

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Variants;

{ TCommandLineParser }

procedure TCommandLineParser.about(const a: String);
begin
  fabout := a;
end;

function TCommandLineParser.check: Boolean;
begin
  for Var i := 0 to High(Params) do
    if not Params[i].isExists then
      Exit(False);
  for Var i := 0 to High(PosArg) do
    if not PosArg[i].isExists then
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
      p: TCommandLineParam := default (TCommandLineParam);
    p.switches := b[0].Split([' '], MaxInt, TStringSplitOptions.ExcludeEmpty);
    Assert(Length(p.switches) > 0);
    p.Values := b[1].Split([' '], MaxInt, TStringSplitOptions.ExcludeEmpty);
    p.isExists := Length(p.Values) > 0;
    p.index := -1;
    p.Note := b[2].Trim;
    if p.switches[0][1] = '@' then
      PosArg := PosArg + [p]
    else
      Params := Params + [p];
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
    begin
      if PosArg[Index].index <> -1 then
        Value := Cast<T>(ParamStr(PosArg[Index].index + 1))
      else
        Value := Cast<T>(PosArg[Index].Values[0]);
    end;
  end
  else
  begin
    Result := TryHas(key, Index);
    if Result then
    begin
      if Params[Index].index <> -1 then
        Value := Cast<T>(ParamStr(Params[Index].index + 1))
      else
        Value := Cast<T>(Params[Index].Values[0]);
    end;
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
      Result := Result + [Cast<T>(Params[Index].Values[i])];
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
  Var
    i: Integer := 1;
  var
    j: Integer := 0;
  While i <= ParamCount do
  begin
    Var
      s: String := ParamStr(i);
    if s[1] = '-' then
    begin
      while (Length(s) > 0) and (s[1] = '-') do
        Delete(s, 1, 1);
      Assert(s.Length > 0);
      Var
        NeedBreak: Boolean := False;
      for Var k := 0 to High(Params) do
      begin
        for Var m := 0 to High(Params[k].switches) do
          if Params[k].switches[m] = s then
          begin
            Assert((ParamCount - i) >= Length(Params[k].Values));
            Params[k].index := i;
            Params[k].isExists := True;
            i := i+Length(Params[k].Values) + 1;
            NeedBreak := True;
            Break;
          end;
        if NeedBreak then
          Break;
      end;
    end
    else
    begin
      if j < Length(PosArg) then
      begin
        PosArg[j].Values := [s];
        PosArg[j].isExists := True;
        Inc(j);
      end;
      Inc(i);
    end;
  end;
end;

procedure TCommandLineParser.printError;
begin
  Var
    ErrPrinted: Boolean := False;
  for Var i := 0 to High(Params) do
    if not Params[i].isExists then
    begin
      if not ErrPrinted then
      begin
        Writeln('ERRORS:');
        ErrPrinted := True;
      end;
      Writeln('Missing parameter: "', Params[i].switches[0], '"');
    end;
  for Var i := 0 to High(PosArg) do
    if not PosArg[i].isExists then
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

function TCommandLineParser.Cast<T>(const Value: String): T;
begin
  case PTypeInfo(System.TypeInfo(T))^.Kind of
    tkInteger:
      TryStrToInt(Value, pInteger(@Result)^);
    tkFloat:
      begin
        case SizeOf(T) of
          SizeOf(Single):
            TryStrToFloat(Value, pSingle(@Result)^);
          SizeOf(Double):
            TryStrToFloat(Value, Double(pDouble(@Result)^));
{$IFDEF WIN32}
          SizeOf(Extended):
            TryStrToFloat(Value, Extended(pExtended(@Result)^));
{$ENDIF}
        end;
      end;
    tkString, tkUString:
      Result := TValue(Value).AsType<T>;
    tkInt64:
      TryStrToInt64(Value, pInt64(@Result)^);
  else
    Assert(False);
  end;
end;

end.
