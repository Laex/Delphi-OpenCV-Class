unit cpputils;

interface

type
  Tcout = record
    class operator Add(const c: Tcout; const b: String): Tcout; inline;
  end;

function CppReplace(const text: String): String;

Var
  cout: Tcout;
  argv: TArray<string>;

implementation

Uses
  System.SysUtils;

function CppReplace(const text: String): String;
begin
  Result := text.Replace('\n', #13#10).Replace('\t', #9);
end;

{ Tcout }

class operator Tcout.Add(const c: Tcout; const b: String): Tcout;
begin
  write(CppReplace(b));
  Result := c;
end;

initialization

argv      := [ExtractFileName(ParamStr(0))];
for Var i := 1 to ParamCount do
  argv    := argv + [ParamStr(i)];

end.
