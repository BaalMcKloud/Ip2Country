unit Ip2Country;

interface

uses Windows, SysUtils, Classes, Contnrs, Lookups;

type
  TIp2Country = class
  private
    FIndex: AnsiString;
    FIndexCount: integer;
    FData: TIntToStrLookup;
    FDataLen: Cardinal;
    FDataOffset: Cardinal;
    FStream: TFileStream;
    FLastEntry: AnsiString;
  protected
    function FindRecord(Data, Search: AnsiString; RecSize: integer): AnsiString;
    procedure FindIndexPos(IP: AnsiString; var Pos, Len: Cardinal);
    function GetDataEntry(Pos, Len: Cardinal): AnsiString;
  public
    constructor Create(DataFile: AnsiString = '');
    destructor Destroy; override;
    class function a2bin(IP: AnsiString): AnsiString;
    class function bin2a(bin: AnsiString): AnsiString;
    function LookupBin(bin: AnsiString): AnsiString;
    function Lookup(IP: AnsiString): AnsiString;
    procedure Preload;
  end;

implementation

type
  PIndexRecord = ^TIndexRecord;
  TIndexRecord = record
    IP: Cardinal;
    Position: Cardinal;
    Length: Cardinal;
  end;

{ TIp2Country }

// Initialize and open the data file
constructor TIp2Country.Create(DataFile: AnsiString);
var
  buf: array[1..2] of Cardinal;
begin
  if DataFile = '' then
    DataFile := 'ip2country.dat';

  FData := TIntToStrLookup.Create;
  FStream := TFileStream.Create(DataFile, fmOpenRead);

  // Read the index and data length
  FStream.Read(PAnsiChar(@buf[1])^, 8);
  FDataOffset := buf[1] + 8;
  FDataLen := buf[2];

  // Read the entire index into memory
  SetLength(FIndex, buf[1]);
  FStream.Read(PAnsiChar(FIndex)^, Length(FIndex));
  FIndexCount := Length(FIndex) div 12;
end;

destructor TIp2Country.Destroy;
begin
  if FStream <> nil then
    FStream.Free;
  FData.Free;
  inherited;
end;

// Translate a dotted IP into a binary format
class function TIp2Country.a2bin(IP: AnsiString): AnsiString;
var
  i, c: integer;
  a: array[1..4] of AnsiString;
begin
  c := 1;
  for i := 1 to Length(IP) do
    if IP[i] = '.' then
      Inc(c)
    else if (IP[i] in ['0'..'9']) and (c <= 4) then
      a[c] := a[c] + IP[i]
    else
      raise Exception.Create('Invalid IP address: ' + IP);

  Result := Chr(StrToInt(a[1])) + Chr(StrToInt(a[2])) + Chr(StrToInt(a[3])) + Chr(StrToInt(a[4]));
end;

// Translate a binary IP into a dotted AnsiString
class function TIp2Country.bin2a(bin: AnsiString): AnsiString;
begin
  Result := IntToStr(Ord(bin[1])) + '.' + IntToStr(Ord(bin[2])) + '.' + IntToStr(Ord(bin[3])) + '.' + IntToStr(Ord(bin[4]));
end;

// Lookup the index for a particular IP address. Returns the position and length
// in the data file.
procedure TIp2Country.FindIndexPos(IP: AnsiString; var Pos, Len: Cardinal);
var
  index: AnsiString;
  p: PIndexRecord;
begin
  index := FindRecord(FIndex, IP, 12);
  p := PIndexRecord(PAnsiChar(index));

  Pos := p.Position;
  Len := p.Length;
end;

// Find a particular record. Uses a data AnsiString with records of a particular
// record size. Uses a binary search algorithm, where we maintain an interval
// and divide in half each attempt - yielding effectively O(log2 n) efficiency.
// Always tries to return an additional 4 bytes beyond the record size - this
// will be the IP address from the *next* block, which we can use as an upper
// bound to determine if we can cache the next lookup.
function TIp2Country.FindRecord(Data, Search: AnsiString; RecSize: integer): AnsiString;
var
  i, l, h, len, c: integer;
  s2: AnsiString;
  r: AnsiString;
begin
  l := 0;
  len := Length(Data) div RecSize;
  h := len - 1;

  while l <= h do begin
    i := (l + h) shr 1;
    Result := Copy(Data, i*RecSize + 1, RecSize + 4);

    r := Copy(Result, 1, 4);
    c := CompareStr(Search, r);

    if c = 0 then
      exit
    else if c > 0 then begin
      if i+1 >= len then
        exit;

      s2 := Copy(Data, (i+1)*RecSize + 1, RecSize + 4);
      c := CompareStr(Search, Copy(s2, 1, 4));

      if c = 0 then begin
        Result := s2;
        exit;
      end else if c < 0 then
        exit;

      l := i + 1;
    end else
      h := i - 1;
  end;

  raise Exception.Create('Binary find failure');
end;

// Get a particular data block - load it from disk if necessary; if it's
// already in memory, just return it
function TIp2Country.GetDataEntry(Pos, Len: Cardinal): AnsiString;
var
  s: AnsiString;
begin
  if not FData.Exists(Pos) then begin
    FStream.Seek(Pos + FDataOffset, soFromBeginning);
    SetLength(s, Len + 6);
    FStream.Read(PAnsiChar(s)^, Len + 6);

    FData.Add(Pos, s);
  end;

  Result := FData.Value(Pos);
end;

// Look up a dotted IP address and return the two-letter country code.
// If the IP address is in the same IP block as the last IP, we cache the
// result without doing a database lookup. So try to do lookups in sort order,
// this will improve access times.
function TIp2Country.Lookup(IP: AnsiString): AnsiString;
begin
  Result := LookupBin(a2bin(IP));
end;

// Look up a binary IP address (see a2bin) and return the two-letter country
// code.
function TIp2Country.LookupBin(bin: AnsiString): AnsiString;
var
  pos, len: Cardinal;
  data: AnsiString;
begin
  if (Length(FLastEntry) = 10) and (CompareStr(bin, Copy(FLastEntry, 1, 4)) >= 0)
      and (CompareStr(bin, Copy(FLastEntry, 7, 4)) < 0) then
  begin
    Result := Copy(FLastEntry, 5, 2);
    exit;
  end;

  FindIndexPos(bin, pos, len);
  data := GetDataEntry(pos, len);

  FLastEntry := FindRecord(data, bin, 6);
  Result := Copy(FLastEntry, 5, 2);
end;

// Preload the database into memory for fastest access.
procedure TIp2Country.Preload;
var
  data: AnsiString;
  pix: PIndexRecord;
  i: integer;
begin
  if FStream = nil then
    exit;

  FStream.Seek(FDataOffset, soFromBeginning);
  SetLength(data, FDataLen);
  FStream.Read(PAnsiChar(data)^, FDataLen);

  pix := PIndexRecord(PAnsiChar(FIndex));
  for i := 0 to FIndexCount - 1 do begin
    if not FData.Exists(pix.Position) then
      FData.Add(pix.Position, Copy(data, pix.Position + 1, pix.Length + 6));
    Inc(pix);
  end;

  FreeAndNil(FStream);
end;

initialization
  if TIp2Country.a2bin('194.218.238.1') <> #$C2#$DA#$EE#$01 then
    raise Exception.Create('a2bin test failed');
  if TIp2Country.bin2a(#$C2#$DA#$EE#$01) <> '194.218.238.1' then
    raise Exception.Create('bin2a test failed');
end.

