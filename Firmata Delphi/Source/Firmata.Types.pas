unit Firmata.Types;

interface

uses Firmata.Constants, System.Generics.Collections;

type
  TI2C_Register = TDictionary<Word, Word>;
  TI2C_memory = TDictionary<Word, TI2C_Register>;

  TModeResolution = array [0 .. 15] of byte;

  TPin = record
    mode: byte;
    analog_channel: byte;
    supported_modes: UInt64;
    resolution: TModeResolution;
    value: integer;
  end;

  TSerialBuffer = array [0 .. 4096] of byte;

  TSerialInfo = record
    Count: integer;
    IsOpended: Boolean;
    Data: TSerialBuffer;
    procedure Append(value: byte);
    procedure Clear;
    function ParseChar(const pos: integer): Char;
    function ToString(const Index: integer): string;
  end;

  TAnalogPins = array [0 .. 15] of byte;
  TPinInfos = array [0 .. 127] of TPin;

  TNotifySerialEvent = procedure(Sender: TObject; PortId: integer; msg: string)
    of object;

  TNotifySerialDataEvent = procedure(Sender: TObject; msg: string) of object;

  TVersion = record
    Major, Minor: byte;
  end;

  TPinMode = (pmInput = PIN_MODE_INPUT, pmOutput, pmAnalog, pmPWM, pmDevVo,
    pmShift, pmI2C, pmOneWire, pmStepper, pmEncoder, pmSerial, pmPullUp,
    pmIgnore = PIN_MODE_IGNORE);

  // pin digital state
  TPinState = (psLow = 0, psHigh = 1);

  TAnalogParse = record
    Cmd: byte;
    Buffer: array [0 .. 1] of byte;
    function Channel: byte;
    function value: integer;
  end;

  TDigitalParse = record
    Cmd: byte;
    Buffer: array [0 .. 1] of byte;
    Bit: array [0 .. 13] of byte;
    function Number: byte;
    function value: integer;
    procedure ExtractBits;
  end;

  TVersionParse = record
    Buffer: array [0 .. 1] of byte;
    function Major: byte;
    function Minor: byte;
  end;

implementation

{ TAnalogParse }

function TAnalogParse.Channel: byte;
begin
  Result := Cmd and $0F;
end;

function TAnalogParse.value: integer;
begin
  Result := Buffer[0] or (Buffer[1] shl 7);
end;

{ TDigitalParse }

procedure TDigitalParse.ExtractBits;
var
  val: integer;
  i: integer;
const
  Binary: array [Boolean] of byte = (0, 1);
begin
  val := value;
  for i := 0 to High(Bit) do
    Bit[i] := Binary[val and (1 shl i) > 0];
end;

function TDigitalParse.Number: byte;
begin
  Result := Cmd and $0F;
end;

function TDigitalParse.value: integer;
begin
  Result := Buffer[0] or (Buffer[1] shl 7);
end;

{ TVersionParse }

function TVersionParse.Major: byte;
begin
  Result := Buffer[0];
end;

function TVersionParse.Minor: byte;
begin
  Result := Buffer[1];
end;

{ TSerialInfo }

procedure TSerialInfo.Append(value: byte);
begin
  Data[Count] := value;
  Inc(Count);
end;

procedure TSerialInfo.Clear;
begin
  Count := 0;
end;

function TSerialInfo.ParseChar(const pos: integer): Char;
begin
  Result := Char((Data[pos] and $7F) or ((Data[pos + 1] and $7F) shl 7));
end;

function TSerialInfo.ToString(const Index: integer): string;
var
  idx: integer;
begin
  idx := Index;
  Result := '';
  while idx < Count do
  begin
    Result := Result + ParseChar(idx);
    Inc(idx, 2);
  end;
end;

end.
