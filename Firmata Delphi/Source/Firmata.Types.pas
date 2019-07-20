unit Firmata.Types;

interface

type
  TPin = record
    mode: byte;
    analog_channel: byte;
    supported_modes: UInt64;
    value: integer;
  end;

  TSerialBuffer = array [0 .. 4096] of byte;

  TSerialInfo = record
    Count: integer;
    IsOpended: Boolean;
    Buffer: TSerialBuffer;
    procedure Append(value: byte);
    procedure Clear;
  end;

  TAnalogPins = array [0 .. 15] of byte;
  TPinInfos = array [0 .. 127] of TPin;

  TCallback = procedure(message: array of byte);
  TCallbacks = array [0 .. 15] of TCallback;

  TVersion = record
    Major, Minor: byte;
  end;

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
  Buffer[Count] := value;
  Inc(Count);
end;

procedure TSerialInfo.Clear;
begin
  Count := 0;
end;

end.
