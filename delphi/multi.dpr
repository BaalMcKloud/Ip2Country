program multi;

{$APPTYPE CONSOLE}

{
  multi.exe

  This program takes a number of IP addresses as indata, one for each line,
  and when the input is all gathered, it then sorts it, removes duplicates,
  and outputs a list of IP=COUNTRY on the standard output.
}

uses
  Windows,
  Classes,
  SysUtils,
  Ip2Country in 'Ip2Country.pas',
  Lookups in 'Lookups.pas';

function FindDB: string;
begin
  if FileExists('ip2country.dat') then
    Result := 'ip2country.dat'
  else if FileExists('..\data\ip2country.dat') then
    Result := '..\data\ip2country.dat'
  else
    raise Exception.Create('Unable to locate database "ip2country.dat"');
end;

procedure Run;

  procedure ReadInput(ip: TStringList);
  var
    s: string;
  begin
    // Read input from standard input until we reach EOF
    while not Eof(Input) do begin
      ReadLn(Input, s);
      if Trim(s) <> '' then
        ip.Add(Trim(s));
    end;
  end;

  procedure ProcessList(ip: TStringList);
  var
    i: integer;
  begin
    // Transform each row into a binary IP - this translates the dotted IP
    // address into a binary notation 
    i := 0;
    while i < ip.Count do begin
      try
        ip[i] := TIp2Country.a2bin(ip[i]);
        Inc(i);
      except
        on E: Exception do begin
          WriteLn(ErrOutput, E.Message);
          ip.Delete(i);
        end;
      end;
    end;

    // Sort the list so we can remove duplicates and optimize the lookup
    ip.Sort;

    // Remove any duplicates from the list
    i := 1;
    while i < ip.Count do
      if ip[i-1] = ip[i] then
        ip.Delete(i)
      else
        Inc(i);
  end;

var
  ipc: TIp2Country;
  ip: TStringList;
  i: Integer;
  t0: Cardinal;
begin
  ipc := TIp2Country.Create(FindDB);
  ip := TStringList.Create;
  try
    // Preload the database for faster access
    ipc.Preload;

    ReadInput(ip);
    ProcessList(ip);

    // Lookup all IP addresses and time the operation
    t0 := GetTickCount;
    for i := 0 to ip.Count - 1 do
      ip[i] := ipc.bin2a(ip[i]) + '=' + ipc.LookupBin(ip[i]);
    t0 := GetTickCount - t0;

    WriteLn(ip.Text);
    WriteLn(ErrOutput, ip.Count, ' lookups in ', t0, ' msec');
  finally
    ip.Free;
    ipc.Free;
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(ErrOutput, E.Message);
  end;
end.

