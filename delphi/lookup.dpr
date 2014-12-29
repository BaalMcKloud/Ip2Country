program lookup;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Ip2Country in 'Ip2Country.pas',
  Lookups in 'Lookups.pas';

procedure Run;
var
  ipc: TIp2Country;
  i: Integer;
begin
  if ParamCount = 0 then begin
    WriteLn('lookup <ip> [<ip>...]');
    halt(1);
  end;

  ipc := TIp2Country.Create('..\data\ip2country.dat');
  try
    for i := 1 to ParamCount do
      WriteLn(ParamStr(i), ' = ', ipc.Lookup(ParamStr(i)));
  finally
    ipc.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
