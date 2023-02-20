program StdTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  cpp.utils;

Var
  s:CppString;
  r:String;
  v:vector<Int>;
begin
  //  CppString
  Writeln(s.length);
  Writeln(s.size);
  s.assign('sdfgsdfgsdfg');
  r:=s;
  Writeln(r);
  r:='1234234';
  s:=r;
  Writeln(s.length);
  Writeln(s.size);
  // Vector
end.
