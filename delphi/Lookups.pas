unit Lookups;

interface

uses Classes, SysUtils;

type
    PLookupItem = ^TLookupItem;
    TLookupItem = record
        FID, FValue: pointer;
    end;

const
    MaxLookupListSize = MaxInt div SizeOf(TLookupItem) - 1;

type
    PLookupItemList = ^TLookupItemList;
    TLookupItemList = array[0..MaxLookupListSize] of TLookupItem;

    TCustomLookup = class
    protected
        FList: PLookupItemList;
        FCount: Integer;
        FCapacity: Integer;
        FInitialized: Boolean;
        procedure Add(ID, Value: pointer);
        function CompareItem(V1, V2: pointer): integer; virtual; abstract;
        procedure DeallocateList; virtual;
        function DumpLine(I: integer): AnsiString; virtual; abstract;
        procedure ExchangeItems(Index1, Index2: Integer);
        function Exists(ID: pointer): boolean;
        function Find(ID: pointer): integer;
        procedure Grow;
        function IsOrdered: boolean;
        procedure QuickSort(L, R: Integer);
        procedure SetInitialized(const Value: Boolean);
        procedure SetCapacity(NewCapacity: Integer);
        function Value(ID: pointer): pointer;
    public
        destructor Destroy; override;
        procedure Clear;
        procedure Dump(Strings: TStrings);
        property Count: integer read FCount;
        property Initialized: boolean read FInitialized write SetInitialized;
    end;

    TIntegerArray = array of integer;
    TStringArray = array of AnsiString;

    TCustomIntKeyLookup = class(TCustomLookup)
    protected
        function CompareItem(V1, V2: pointer): integer; override;
    public
        function Exists(ID: integer): boolean;
        function Low: integer;
        function High: integer;
        function GetKeys: TIntegerArray;
    end;

    TIntToIntLookup = class(TCustomIntKeyLookup)
    protected
        function DumpLine(I: integer): AnsiString; override;
    public
        procedure Add(ID, Value: integer);
        function Value(ID: integer): integer;
    end;

    TIntToStrLookup = class(TCustomIntKeyLookup)
    protected
        procedure DeallocateList; override;
        function DumpLine(I: integer): AnsiString; override;
    public
        procedure Add(ID: integer; Value: AnsiString);
        function Value(ID: integer): AnsiString;
    end;

    TIntToObjectLookup = class(TCustomIntKeyLookup)
    private
        FOwnsObjects: boolean;
    protected
        procedure DeallocateList; override;
        function DumpLine(I: integer): AnsiString; override;
    public
        procedure Add(ID: integer; Value: TObject);
        function Value(ID: integer): TObject;
        property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
    end;

    TCustomStrKeyLookup = class(TCustomLookup)
    protected
        function CompareItem(V1, V2: pointer): integer; override;
    public
        function Exists(ID: AnsiString): boolean;
        function GetKeys: TStringArray;
    end;

    TStrToIntLookup = class(TCustomStrKeyLookup)
    protected
        procedure DeallocateList; override;
        function DumpLine(I: integer): AnsiString; override;
    public
        procedure Add(ID: AnsiString; Value: integer);
        function Value(ID: AnsiString): integer;
    end;

    TStrToStrLookup = class(TCustomStrKeyLookup)
    protected
        procedure DeallocateList; override;
        function DumpLine(I: integer): AnsiString; override;
    public
        procedure Add(ID, Value: AnsiString);
        function Value(ID: AnsiString): AnsiString;
    end;

    TStrToObjectLookup = class(TCustomStrKeyLookup)
    private
        FOwnsObjects: boolean;
    protected
        procedure DeallocateList; override;
        function DumpLine(I: integer): AnsiString; override;
    public
        procedure Add(ID: AnsiString; Value: TObject);
        function Value(ID: AnsiString): TObject;
        property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
    end;

implementation

function CopyString(Value: AnsiString): PAnsiString;
begin
    New(Result);
    Result^ := Value;
end;

{ TCustomLookup }

procedure TCustomLookup.Add(ID, Value: pointer);
begin
    if FCount = FCapacity then
        Grow;

    with FList^[FCount] do begin
        FID := ID;
        FValue := Value;
    end;
    Inc(FCount);
    FInitialized := false;
end;

procedure TCustomLookup.Clear;
begin
    if FCount <> 0 then begin
        DeallocateList;
        FCount := 0;
        FInitialized := false;
        SetCapacity(0);
    end;
end;

procedure TCustomLookup.DeallocateList;
begin
end;

destructor TCustomLookup.Destroy;
begin
    Clear;
    inherited;
end;

procedure TCustomLookup.Dump(Strings: TStrings);
var
    i: Integer;
begin
    Strings.Clear;
    for i := 0 to FCount - 1 do
        Strings.Add(DumpLine(i));
end;

procedure TCustomLookup.ExchangeItems(Index1, Index2: Integer);
var
    temp: pointer;
    i1, i2: PLookupItem;
begin
    i1 := @FList^[Index1];
    i2 := @FList^[Index2];

    temp := i1^.FID;
    i1^.FID := i2^.FID;
    i2^.FID := temp;
    temp := i1^.FValue;
    i1^.FValue := i2^.FValue;
    i2^.FValue := temp;
end;

function TCustomLookup.Exists(ID: pointer): boolean;
begin
    Result := Find(ID) <> -1;
end;

function TCustomLookup.Find(ID: pointer): integer;
var
    L, H, I: Integer;
begin
    Initialized := true;

    Result := -1;
    if (FCount = 0) or (CompareItem(ID, FList^[0].FID) = -1) or (CompareItem(ID, FList^[FCount - 1].FID) = 1) then
        exit;

    L := 0;
    H := FCount - 1;
    while L <= H do begin
        I := (L + H) shr 1;
        case CompareItem(FList^[I].FID, ID) of
            -1: L := I + 1;
             1: H := I - 1;
            else begin
                Result := I;
                exit;
            end;
        end;
    end;
end;

procedure TCustomLookup.Grow;
var
    Delta: Integer;
begin
    if FCapacity > 64 then
        Delta := FCapacity div 4
    else
        Delta := 16;

    SetCapacity(FCapacity + Delta);
end;

function TCustomLookup.IsOrdered: boolean;
var
    i: integer;
    id, lastid: pointer;
begin
    Result := true;
    if FCount > 1 then begin
        lastid := FList^[0].FID;
        for i := 1 to FCount - 1 do begin
            id := FList^[i].FID;
            if CompareItem(id, lastid) = -1 then begin
                Result := false;
                exit;
            end;
            lastid := id;
        end;
    end;
end;

procedure TCustomLookup.QuickSort(L, R: Integer);

    function Compare(Index1, Index2: integer): integer;
    begin
        Result := CompareItem(FList^[Index1].FID, FList^[Index2].FID);
    end;

var
    I, J, P: Integer;
begin
    repeat
        I := L;
        J := R;
        P := (L + R) shr 1;
        repeat
            while Compare(I, P) < 0 do
                Inc(I);
            while Compare(J, P) > 0 do
                Dec(J);
            if I <= J then begin
                ExchangeItems(I, J);
                if P = I then
                    P := J
                else if P = J then
                    P := I;
                Inc(I);
                Dec(J);
            end;
        until I > J;
        if L < J then
            QuickSort(L, J);
        L := I;
    until I >= R;
end;

procedure TCustomLookup.SetCapacity(NewCapacity: Integer);
begin
    ReallocMem(FList, NewCapacity * SizeOf(TLookupItem));
    FCapacity := NewCapacity;
end;

procedure TCustomLookup.SetInitialized(const Value: Boolean);
begin
    if FInitialized <> Value then begin
        if Value and not IsOrdered then
            QuickSort(0, FCount - 1);

        FInitialized := Value;
    end;
end;

function TCustomLookup.Value(ID: pointer): pointer;
var
    index: integer;
begin
    index := Find(ID);
    if index = -1 then
        Result := nil
    else
        Result := FList^[index].FValue;
end;

{ TCustomIntKeyLookup }

function TCustomIntKeyLookup.CompareItem(V1, V2: pointer): integer;
begin
    if Integer(V1) < Integer(V2) then
        Result := -1
    else if Integer(V1) > Integer(V2) then
        Result := 1
    else
        Result := 0;
end;

function TCustomIntKeyLookup.Exists(ID: integer): boolean;
begin
    Result := inherited Exists(Ptr(ID));
end;

function TCustomIntKeyLookup.GetKeys: TIntegerArray;
var
    i: integer;
begin
    SetLength(Result, FCount);
    for i := 0 to FCount - 1 do
        Result[i] := integer(FList[i].FID);
end;

function TCustomIntKeyLookup.High: integer;
begin
    if Count = 0 then
        Result := 0
    else
        Result := Integer(FList[FCount-1].FID);
end;

function TCustomIntKeyLookup.Low: integer;
begin
    if Count = 0 then
        Result := 0
    else
        Result := Integer(FList[0].FID);
end;

{ TIntToIntLookup }

procedure TIntToIntLookup.Add(ID, Value: integer);
begin
    inherited Add(Ptr(ID), Ptr(Value));
end;

function TIntToIntLookup.DumpLine(I: integer): AnsiString;
begin
    with FList^[I] do
        Result := IntToStr(integer(FID)) + '=' + IntToStr(integer(FValue));
end;

function TIntToIntLookup.Value(ID: integer): integer;
begin
    Result := integer(inherited Value(Ptr(ID)));
end;

{ TIntToStrLookupList }

procedure TIntToStrLookup.Add(ID: integer; Value: AnsiString);
begin
    inherited Add(Ptr(ID), CopyString(Value));
end;

procedure TIntToStrLookup.DeallocateList;
var
    i: integer;
begin
    for i := 0 to FCount - 1 do
        Dispose(PAnsiString(FList^[i].FValue));
end;

function TIntToStrLookup.DumpLine(I: integer): AnsiString;
begin
    with FList^[I] do
        Result := IntToStr(integer(FID)) + '=' + PAnsiString(FValue)^;
end;

function TIntToStrLookup.Value(ID: integer): AnsiString;
begin
    Result := PAnsiString(inherited Value(Ptr(ID)))^;
end;

{ TIntToObjectLookup }

procedure TIntToObjectLookup.Add(ID: integer; Value: TObject);
begin
    inherited Add(Ptr(ID), Value);
end;

procedure TIntToObjectLookup.DeallocateList;
var
    i: integer;
begin
    if FOwnsObjects then
        for i := 0 to FCount - 1 do
            with FList^[i] do
                if FValue <> nil then
                    TObject(FValue).Free;
end;

function TIntToObjectLookup.DumpLine(I: integer): AnsiString;
begin
    with FList^[I] do
        Result := IntToStr(integer(FID)) + '=' + TObject(FValue).ClassName + '($' + IntToHex(integer(FValue), 8);
end;

function TIntToObjectLookup.Value(ID: integer): TObject;
begin
    Result := TObject(inherited Value(Ptr(ID)));
end;

{ TCustomStrKeyLookup }

function TCustomStrKeyLookup.CompareItem(V1, V2: pointer): integer;
begin
    Result := CompareStr(PAnsiString(V1)^, PAnsiString(V2)^);
end;

function TCustomStrKeyLookup.Exists(ID: AnsiString): boolean;
begin
    Result := inherited Exists(@ID);
end;

function TCustomStrKeyLookup.GetKeys: TStringArray;
var
    i: integer;
begin
    SetLength(Result, FCount);
    for i := 0 to FCount - 1 do
        Result[i] := PAnsiString(FList[i].FID)^;
end;

{ TStrToIntLookup }

procedure TStrToIntLookup.Add(ID: AnsiString; Value: integer);
begin
    inherited Add(CopyString(ID), Ptr(Value));
end;

procedure TStrToIntLookup.DeallocateList;
var
    i: integer;
begin
    for i := 0 to FCount - 1 do
        Dispose(PAnsiString(FList^[i].FID));
end;

function TStrToIntLookup.DumpLine(I: integer): AnsiString;
begin
    with FList^[I] do
        Result := PAnsiString(FID)^ + '=' + IntToStr(integer(FValue));
end;

function TStrToIntLookup.Value(ID: AnsiString): integer;
begin
    Result := integer(inherited Value(@ID));
end;

{ TStrToStrLookup }

procedure TStrToStrLookup.Add(ID, Value: AnsiString);
begin
    inherited Add(CopyString(ID), CopyString(Value));
end;

procedure TStrToStrLookup.DeallocateList;
var
    i: integer;
begin
    for i := 0 to FCount - 1 do begin
        Dispose(PAnsiString(FList^[i].FID));
        Dispose(PAnsiString(FList^[i].FValue));
    end;
end;

function TStrToStrLookup.DumpLine(I: integer): AnsiString;
begin
    with FList^[I] do
        Result := PAnsiString(FID)^ + '=' + PAnsiString(FValue)^;
end;

function TStrToStrLookup.Value(ID: AnsiString): AnsiString;
begin
    Result := PAnsiString(inherited Value(@ID))^;
end;

{ TStrToObjectLookup }

procedure TStrToObjectLookup.Add(ID: AnsiString; Value: TObject);
begin
    inherited Add(CopyString(ID), Value);
end;

procedure TStrToObjectLookup.DeallocateList;
var
    i: integer;
begin
    for i := 0 to FCount - 1 do
        with FList^[i] do begin
            Dispose(PAnsiString(FID));
            if FOwnsObjects and (FValue <> nil) then
                TObject(FValue).Free;
    end;
end;

function TStrToObjectLookup.DumpLine(I: integer): AnsiString;
begin
    with FList^[I] do
        Result := PAnsiString(FID)^ + '=' + TObject(FValue).ClassName + '($' + IntToHex(integer(FValue), 8);
end;

function TStrToObjectLookup.Value(ID: AnsiString): TObject;
begin
    Result := TObject(inherited Value(@ID));
end;

end.
