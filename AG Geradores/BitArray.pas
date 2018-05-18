unit BitArray;

interface

Type
  TBitArray = class
   private
    FintVal: LongWord;
    function bin2dec(value: string): LongWord;
    function Getitem(Index: Integer): LongWord;
    procedure SetItem(Index: Integer; const Value: LongWord);
   public
    function toBinString(size:integer;PartSz:Integer=0):string;
    procedure fromBinString(binString: string);
    property intVal: LongWord read FintVal write FintVal;
    constructor Create(intVal:LongWord);
    property Item[Index:Integer]: LongWord read Getitem write SetItem; default;
  end;


implementation

{ TBitArray }

constructor TBitArray.Create(intVal: LongWord);
begin
  Self.intVal:= intVal;
end;

function TBitArray.Getitem(Index: Integer): LongWord;
begin
 Result:= (intVal shr Index) and 1;
end;

procedure TBitArray.SetItem(Index: Integer; const Value: LongWord);
begin
if Value = 1 then
  intVal:= intVal or (Value shl Index)
else
  intVal:= intVal and not (Value shl Index);
end;

function TBitArray.toBinString(size:integer;PartSz:Integer=0): string;
var
  i: Integer;
begin
  Result:= '';
  for i := 0 to size-1 do
  begin
    Result:= chr(ord('0')+Item[i])+Result;
    if (PartSz > 0) and((i+1) mod PartSz = 0) then
     Result:= '.'+Result;
  end;
end;

procedure TBitArray.fromBinString(binString:string);
begin
   intVal:= bin2dec(binString);
end;

function TBitArray.bin2dec(value: string): LongWord;
var
  i, iValueSize: Integer;
begin
  Result := 0;
  iValueSize := Length(Value);
  for i := iValueSize downto 1 do
    if Value[i] = '1' then Result := Result + (1 shl (iValueSize - i));
end;

end.
