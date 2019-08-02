unit Firmata.Print;

interface

type
  TPrintBase = (puBin = 2, puOct = 8, puDec = 10, puHex = 16);

  TPrint = class
  private
    writeError: Integer;
    function PrintNumber(value: Cardinal; base: TPrintBase = puDec): Integer;
    function PrintFloat(value: Double; digits: byte = 3): Integer;
  public
    function write(value: AnsiString): Integer; overload; virtual;
    function write(value: AnsiChar): Integer; overload; virtual;
    function write(value: byte): Integer; overload; virtual;
    function write(value: array of byte; length: Integer): Integer;
      overload; virtual;
    function Print(value: AnsiString): Integer; overload;
    function Print(value: AnsiChar): Integer; overload;
    function Print(value: LongInt; base: TPrintBase = puDec): Integer; overload;
    function Print(value: Cardinal; base: TPrintBase = puDec): Integer;
      overload;
    function Print(value: Double; digits: byte): Integer; overload;
    function Println(value: AnsiString): Integer; overload;
    function Println(value: AnsiChar): Integer; overload;
    function Println(value: LongInt; base: TPrintBase = puDec)
      : Integer; overload;
    function Println(value: Cardinal; base: TPrintBase = puDec)
      : Integer; overload;
    function Println(value: Double; digits: byte): Integer; overload;
    function Println(): Integer; overload;
  end;

implementation

uses Math;

{ TPrint }

function TPrint.Print(value: AnsiString): Integer;
begin
  Result := write(value);
end;

function TPrint.Print(value: AnsiChar): Integer;
begin
  Result := write(value);
end;

function TPrint.Print(value: Cardinal; base: TPrintBase): Integer;
begin
  Result := PrintNumber(value, base);
end;

function TPrint.Print(value: LongInt; base: TPrintBase): Integer;
begin
  Result := 0;
  if base = puDec then
    if value < 0 then
    begin
      Result := Result + Print('-');
      Result := Result + PrintNumber(-value);
      exit;
    end;
  Result := PrintNumber(value, base);
end;

function TPrint.Print(value: Double; digits: byte): Integer;
begin
  Result := PrintFloat(value, digits);
end;

function TPrint.PrintFloat(value: Double; digits: byte): Integer;
var
  buf: string;
  rounding, remainder: Double;
  i: Integer;
  int_part, toPrint: Cardinal;

begin
  if IsNan(value) then
    exit(Print('nan'));
  if IsInfinite(value) then
  begin
    if (value < 0) then
      Print('-');
    exit(Print('inf'));
  end;

  Result := 0;
  if value < 0.0 then
  begin
    Result := Print('-');
    value := -value;
  end;

  rounding := 0.5;
  for i := 0 to digits - 1 do
  begin
    rounding := rounding / 10.0;
  end;
  value := value + rounding;
  int_part := Trunc(value);
  remainder := value - int_part;
  // Print(int_part);
  if digits > 0 then
    Result := Result + Print('.');
  while digits > 0 do
  begin
    remainder := remainder * 10.0;
    toPrint := Trunc(remainder);
    // Result := Result + Print(toPrint);
    remainder := remainder - toPrint;
    Dec(digits);
  end;
end;

function TPrint.Println(value: AnsiString): Integer;
begin

end;

function TPrint.Println(value: AnsiChar): Integer;
begin

end;

function TPrint.Println(value: LongInt; base: TPrintBase): Integer;
begin

end;

function TPrint.Println(value: Cardinal; base: TPrintBase): Integer;
begin

end;

function TPrint.Println(value: Double; digits: byte): Integer;
begin
  Result := Print(value, digits);
  Result := Result + Println();
end;

function TPrint.PrintNumber(value: Cardinal; base: TPrintBase): Integer;
var
  buf: AnsiString;
  c, b: byte;
begin
  buf := '';
  b := Ord(base);

  repeat
    c := value mod b;
    value := value div b;
    if c < 10 then
      buf := buf + AnsiChar(c + $30)
    else
      buf := buf + AnsiChar(c + $65);
  until value = 0;
  Result := write(buf);
end;

function TPrint.write(value: byte): Integer;
begin
  // For future override
  Result := 0;
end;

function TPrint.write(value: AnsiString): Integer;
begin
  Result := 0;
  while Result < length(value) do
  begin
    if write(value[Result]) = 0 then
      Break;
    Inc(Result);
  end;
end;

function TPrint.write(value: AnsiChar): Integer;
begin
  Result := write(Ord(value));
end;

function TPrint.write(value: array of byte; length: Integer): Integer;
begin
  Result := 0;
  while Result < length do
  begin
    if write(value[Result]) = 0 then
      Break;
    Inc(Result);
  end;
end;

function TPrint.Println: Integer;
begin
  Result := Print(#13#10);
end;

end.
