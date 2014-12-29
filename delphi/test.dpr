program test;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  Ip2Country in 'Ip2Country.pas',
  Lookups in 'Lookups.pas';

// Split a data string into a TStringList by record size.
procedure Split(Src: string; Len: integer; List: TStringList);
var
  i: integer;
  count: integer;
begin
  count := Length(Src) div Len;
  for i := 0 to count - 1 do
    List.Add(Copy(Src, i*Len + 1, Len));
end;

// Load a file into memory.
function LoadFile(Filename: string): string;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenRead);
  try
    SetLength(Result, fs.Size);
    fs.Read(PChar(Result)^, Length(Result));
  finally
    fs.Free;
  end;
end;

// Run a test on boundaries - typically 0.0.0.0, 255.255.255.255 etc to see
// if we trigger any lookup fails, indicating a logical problem with the lookup
// code.
procedure TestBoundaries(ipc: TIp2Country);
var
  i: integer;
  s: string;
begin
  Write('Verifying basic bounds functionality ... ');
  for i := 0 to 255 do begin
    s := IntToStr(i);
    ipc.lookup(s + '.0.0.0');
    ipc.lookup(s + '.' + s + '.0.0');
    ipc.lookup(s + '.' + s + '.' + s + '.0');
    ipc.lookup(s + '.' + s + '.' + s + '.' + s);
    ipc.lookup('0.' + s + '.' + s + '.' + s);
    ipc.lookup('0.0.' + s + '.' + s);
    ipc.lookup('0.0.0.' + s);
  end;
  WriteLn('ok');
end;

// Test random lookups just to see how we're doing.
procedure TestRandom(ipc: TIp2Country);
var
  t0: Cardinal;
  i: integer;
begin
  Write('Making 250,000 random lookups ... ');
  t0 := GetTickCount;
  for i := 1 to 250000 do
    ipc.lookup(IntToStr(Random(256)) + '.' + IntToStr(Random(256)) + '.' + IntToStr(Random(256)) + '.' + IntToStr(Random(256)));
  WriteLn(IntToStr(GetTickCount - t0) + ' ms');
end;

// Use the CSV data (actually the test.dat) file to look up, for each block, the
// low boundary, high boundary, and somewhere in between, to make sure that each
// IPv4 block is tested for bounds and internal consistency. And do this four
// times so we can time the result effectively.
procedure TestData(ipc: TIp2Country);
const
  iterations = 4;
var
  ips: TStringList;
  t0: Cardinal;
  i, iteration: integer;
  s, countryTest, countryLookup: string;
begin
  ips := TStringList.Create;
  try
    Split(LoadFile('..\data\test.dat'), 6, ips);
    Write('Verifying against generated test data ... ');
    t0 := GetTickCount;
    for iteration := 1 to iterations do
      for i := 0 to ips.Count - 1 do begin
        s := ips[i];
        countryTest := Copy(s, 5, 2);
        countryLookup := ipc.LookupBin(Copy(s, 1, 4));
        if countryTest <> countryLookup then
          WriteLn('Problem: ' + countryTest + ' <> ' + countryLookup + ' for ' + ipc.bin2a(Copy(s, 1, 4)));
      end;
    WriteLn(IntToStr(ips.Count * iterations) + ' tests in ' + IntToStr(GetTickCount - t0) + ' ms');
  finally
    ips.Free;
  end;
end;

procedure Run;
var
  ipc: TIp2Country;
begin
  // Run tests without preloading database - so we can test dynamic loading
  WriteLn('### Running dynamic load tests ###');
  ipc := TIp2Country.Create('..\data\ip2country.dat');
  try
    TestBoundaries(ipc);
    TestRandom(ipc);
    TestData(ipc);
  finally
    ipc.Free;
  end;
  WriteLn;

  // Run tests with preloading - just in case the preload method screwed
  // something up
  WriteLn('### Running preload tests ###');
  ipc := TIp2Country.Create('..\data\ip2country.dat');
  try
    ipc.Preload;
    TestBoundaries(ipc);
    TestRandom(ipc);
    TestData(ipc);
  finally
    ipc.Free;
  end;
  WriteLn;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  WriteLn('Press ENTER to exit');
  ReadLn;  
end.

