unit Firmata.Types;

interface

uses Firmata.Constants, System.Generics.Collections;

const
  MAX_BUFFER_SIZE = 64;

type
  TI2C_Register = TDictionary<Word, Word>;
  TI2C_memory = TDictionary<Word, TI2C_Register>;

  TModeResolution = array [0 .. 15] of byte;

  // all pin mode
  TPinMode = (pmInput = PIN_MODE_INPUT, pmOutput, pmAnalog, pmPWM, pmServo,
    pmShift, pmI2C, pmOneWire, pmStepper, pmEncoder, pmSerial, pmPullUp,
    pmIgnore);

  TPinModes = set of TPinMode;

const
  PIN_MODE_STR: array [TPinMode] of string = ('INPUT', 'OUTPUT', 'ANALOG',
    'PWM', 'SERVO', 'SHIFT', 'I2C', 'ONEWIRE', 'STEPPER', 'ENCODER', 'SERIAL',
    'PULL UP', 'IGNORE');

type

  TPin = record
    mode: TPinMode;
    analog_channel: byte;
    supported_modes: TPinModes;
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
  TAnalogPinsEnum = (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13,
    A14, A15);
  TAnalogPinsSet = set of TAnalogPinsEnum;

  TPinInfos = array [0 .. 127] of TPin;

  TNotifySerialEvent = procedure(Sender: TObject; PortId: integer; msg: string)
    of object;

  TNotifySerialDataEvent = procedure(Sender: TObject; msg: string) of object;

  TVersion = record
    Major, Minor: byte;
  end;

  // pin digital state
  TPinState = (psLow = 0, psHigh = 1);

  // Port ID for serial
  TSerialPortID = (spHardware0 = 0, spHardware1, spHardware2, spHardware3,
    spHardware4, spHardware5, spHardware6, spHardware7, spSoftware0,
    spSoftware1, spSoftware2, spSoftware3, spSoftware4, spSoftware5,
    spSoftware6, spSoftware7);

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

  TI2C_Mode = (imReadWrite, imReadOnce, imReadContinously, imStopReading);
  TI2C_AddressMode = (iam7bits, iam10bits);

  TI2C_RequestMode = record
    Adress: Word;
    autoRestart: Boolean;
    mode: TI2C_Mode;
    AddresMode: TI2C_AddressMode;
  end;

  TI2C_RequestParse = record
    Lsb, Msb: byte;
    procedure Pack(Req: TI2C_RequestMode);
    function UpPack: TI2C_RequestMode;
  end;

  TSerialCircularBuffer = record
  private
    Buffer: array [0 .. (MAX_BUFFER_SIZE - 1)] of byte;
    Head, Tail: byte;
    procedure Inc(var variable: byte);
  public
    procedure Initialize;
    procedure Flush;
    function Available: byte;
    function Read: byte;
    function Peek: byte;
    function Write(Data: byte): Boolean;
  end;

  // Notify events
  TDigitalChangeNotify = procedure(Sender: TObject; const PinNumber: integer;
    const PinValue: TPinState) of object;

  TAnalogChangeNotify = procedure(Sender: TObject;
    const AnalogNumber: TAnalogPinsEnum; const value: Word) of object;

  TBoardChangeNotify = procedure(Sender: TObject; const Pin: integer;
    Info: TPin) of object;

  TFirmwareNotify = procedure(Sender: TObject; const FirmwareName: string)
    of object;

  TPinStateChangeNotify = TBoardChangeNotify;

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

{ TI2C_RequestParse }

procedure TI2C_RequestParse.Pack(Req: TI2C_RequestMode);
begin
  Lsb := Req.Adress and $7F; // 7 bits  adress
  Msb := (Req.Adress shr 7) and $07; // More 3 bits adress(10bits mode)
  Msb := Msb or (ord(Req.mode) shl 3); // Read-write mode
  Msb := Msb or (ord(Req.AddresMode) shl 5); // 10bits adress mode
  Msb := Msb or (ord(Req.autoRestart) shl 6); // Auto Restart mode
end;

function TI2C_RequestParse.UpPack: TI2C_RequestMode;
begin
  Result.Adress := ((Msb and $07) shl 7) or Lsb;
  Result.mode := TI2C_Mode((Msb shr 3) and $03);
  Result.AddresMode := TI2C_AddressMode((Msb shr 5) and $01);
  Result.autoRestart := (Msb shr 6) and $01 > 0;
end;

{ TSerialCircularBuffer }

function TSerialCircularBuffer.Available: byte;
begin
  Result := ((MAX_BUFFER_SIZE + Head) - Tail) mod MAX_BUFFER_SIZE;
end;

procedure TSerialCircularBuffer.Flush;
begin
  Head := 0;
  Tail := 0;
end;

procedure TSerialCircularBuffer.Inc(var variable: byte);
begin
  variable := (variable + 1) mod MAX_BUFFER_SIZE;
end;

procedure TSerialCircularBuffer.Initialize;
begin
  Flush;
end;

function TSerialCircularBuffer.Peek: byte;
begin
  if Head = Tail then
    Result := $FF // Default error read 0xFF (255)
  else
    Result := Buffer[Tail];
end;

function TSerialCircularBuffer.Read: byte;
begin
  if Head <> Tail then
  begin
    Result := Buffer[Tail];
    Inc(Tail);
  end
  else
    Result := $FF; // Default error read 0xFF (255)
end;

function TSerialCircularBuffer.Write(Data: byte): Boolean;
begin
  if Available = MAX_BUFFER_SIZE - 1 then
    Exit(False); // overflow
  Buffer[Head] := Data;
  Inc(Head);
  Result := True;
end;

end.
