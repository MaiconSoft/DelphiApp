{ *******************************************************************************
  Adaptation of project of [Sottam](https://github.com/sottam/FirmataPascal)
  for Delphi compatibility.

  Maicon (jul/2019)
  ******************************************************************************* }
unit Firmata;

interface

uses
  System.SysUtils, System.Classes, Firmata.Constants, Firmata.Types, CPort,
  System.Generics.Collections, Vcl.ExtCtrls, Winapi.Windows, Vcl.Forms;

type
  TSerial = class;

  TFirmata = class(TComponent)
  private
    { Private declarations }
    FSerial: TComPort;
    I2CMemory: TI2C_memory;
    FFirmwareName: string;
    AnalogPins: TAnalogPins;
    PinsInfo: TPinInfos;
    Version: TVersion;
    Buffer: TSerialInfo;
    FReady: Boolean;
    FOnSerialReceve: TNotifySerialEvent;
    FOnSerialDataReceve: TNotifySerialDataEvent;
    FTimer: TTimer;
    FSerials: array [TSerialPortID] of TSerial;
    function GetFirmwareName: string;
    function SettingsFileName: string;
    procedure Process(value: Byte);
    procedure HandleRx(Sender: TObject; const Buffer; Count: Integer);
    procedure AskFirmware;
    procedure Write(Buf: array of Byte;
      IncludeHeadAndTail: Boolean = True); overload;
    procedure Write(Header, Buf: array of Byte;
      IncludeHeadAndTail: Boolean = True); overload;
    procedure Write(Buf: Byte; IncludeHeadAndTail: Boolean = True); overload;
    procedure WriteHead;
    procedure WriteTail;
    procedure InitPinInfo;
    procedure ReadAnalog(Cmd: Byte);
    procedure ReadDigital(Cmd: Byte);
    procedure ReadVersion;
    procedure ProcessEx;
    procedure ReportFirmware;
    procedure CapabilityResponse;
    procedure AnalogMappingResponse;
    procedure PinStateResponse;
    procedure I2cReply;
    procedure SerialMessage;
    procedure SerialData;
    procedure AskBoardCapabilities;
    procedure AskReportAnalog(index: Integer);
    procedure AskReportDigital(index: Integer);
    function digitalRead(pin: Byte): TPinState;
    function analogRead(pin: Byte): Word;
    function ReturnAnalogPinNumber(pin: Byte): Byte;
    procedure setPinMode(pin: Byte; mode: TPinMode);
    procedure digitalWrite(pin: Byte; value: TPinState);
    procedure analogWrite(pin: Byte; value: Word);
    procedure digitalReport(Port: Byte; enab: Boolean);
    procedure analogReport(pin: Byte; enab: Boolean);
    function GetDigital(pin: Byte): TPinState;
    procedure SetDigital(pin: Byte; const value: TPinState);
    function GetAnalog(pin: Byte): Word;
    procedure SetAnalog(pin: Byte; const value: Word);
    procedure PrintPinInfo(Info: TStrings);
    procedure SerialWrite(portID: TSerialPortID; Msg: AnsiString);
    procedure SerialRead(portID: TSerialPortID; maxBytesToRead: Integer = 0);
    procedure SerialStop(portID: TSerialPortID);
    procedure SerialFlush(portID: TSerialPortID);
    procedure SerialListen(portID: TSerialPortID);
    class procedure AvailableSerialPorts(Ports: TStrings); static;
    procedure SerialClose(portID: TSerialPortID);
    procedure SerialHWConfig(portID: TSerialPortID; baudRate: LongWord);
    procedure SerialSWConfig(portID: TSerialPortID; baudRate: LongWord;
      rx_pin, tx_pin: Byte);
    procedure I2cRequest(mode: TI2C_RequestMode; Data: array of Byte);
    class procedure ExpandArray(DataIn: array of Byte;
      var DataOut: array of Byte); static;
    procedure I2cConfig(delay: uint16; Data: array of Byte);
    class procedure ContractArray(DataIn: array of Byte;
      var DataOut: array of Byte); static;
    function GetPinMode(pin: Byte): TPinMode;
    procedure AskUpdate(Sender: TObject);
    function GetTimed: Boolean;
    procedure SetTimed(const value: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    function Start: Boolean; overload;
    function Start(Port: string): Boolean; overload;
    function Stop: Boolean;
    procedure SaveSettings;
    property FirmwareName: string read GetFirmwareName;
    property Ready: Boolean read FReady;
    property Digital[pin: Byte]: TPinState read GetDigital write SetDigital;
    property Analog[pin: Byte]: Word read GetAnalog write SetAnalog;
    property PinMode[pin: Byte]: TPinMode read GetPinMode write setPinMode;

  published
    { Published declarations }
    property TimedUpdate: Boolean read GetTimed write SetTimed;
    property SerialDriver: TComPort read FSerial;
    property OnSerialReceve: TNotifySerialEvent read FOnSerialReceve
      write FOnSerialReceve;
    property OnSerialDataReceve: TNotifySerialDataEvent read FOnSerialDataReceve
      write FOnSerialDataReceve;
  end;

  TLookAHead = (lhSkipAll, hlSkipNone, lhSkipWhiteSpace);

  TSerial = class
  private
    FPortID: TSerialPortID;
    FParent: TFirmata;
    FBuffer: TSerialCircularBuffer;
    FTimeout: Cardinal;
    function TimedPeek: Byte;
    function TimedRead: Byte;
    function peekNextDigit(lookahead: TLookAHead; detectDecimal: Boolean): Byte;
  public
    constructor Create(AOwner: TFirmata; portID: TSerialPortID);
    procedure Print(Msg: string);
    procedure Println(Msg: string);
    procedure Stop();
    procedure Listen();
    procedure Flush();
    function read(): Byte;
    function peek(): Byte;
    function available: Byte;
    function ParseInt(lookahead: TLookAHead; ignore: AnsiChar)
      : Integer; overload;
    function ParseInt(lookahead: TLookAHead; ignore: Byte): Integer; overload;
    function ParseInt(lookahead: TLookAHead): Integer; overload;
    function ParseInt(): Integer; overload;
    function ParseFloat: Double; overload;
    function ParseFloat(lookahead: TLookAHead): Double; overload;
    function ParseFloat(lookahead: TLookAHead; ignore: Byte): Double; overload;
    function ParseFloat(lookahead: TLookAHead; ignore: AnsiChar)
      : Double; overload;
    function ReadBytesUntil(terminator: AnsiChar; var Buffer: AnsiString;
      Length: Integer): Integer; overload;
    function ReadBytesUntil(terminator: Byte; var Buffer: array of Byte;
      Length: Integer): Integer; overload;
    function ReadBytes(var Buffer: AnsiString; Length: Integer)
      : Integer; overload;
    function ReadBytes(var Buffer: array of Byte; Length: Integer)
      : Integer; overload;
    function ReadString(terminator: AnsiChar): AnsiString; overload;
    function ReadString(terminator: Byte): AnsiString; overload;
    procedure baudRate(baud: LongWord); overload;
    procedure baudRate(baud: LongWord; RxPin, TXPin: Byte); overload;
    property portID: TSerialPortID read FPortID Write FPortID;
    property Timeout: Cardinal read FTimeout write FTimeout;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Firmata', [TFirmata]);
end;

{ TFirmata }

procedure TFirmata.AskReportAnalog(index: Integer);
begin
  Write(REPORT_ANALOG or index);
end;

procedure TFirmata.AskReportDigital(index: Integer);
begin
  Write(REPORT_DIGITAL or index);
end;

procedure TFirmata.AskBoardCapabilities();
var
  i: Integer;
begin
  write([ANALOG_MAPPING_QUERY]);
  write([CAPABILITY_QUERY]);
  for i := 0 to 15 do
  begin
    AskReportAnalog(i);
    AskReportDigital(i);
  end;
end;

procedure TFirmata.AskFirmware;
begin
  write([REPORT_FIRMWARE]);
end;

procedure TFirmata.AskUpdate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 127 do
  begin
    AskReportDigital(i);
    AskReportAnalog(i);
  end;
  for i := 0 to 15 do
    SerialRead(TSerialPortID(i));
end;

constructor TFirmata.Create(AOwner: TComponent);
var
  i: TSerialPortID;
begin
  inherited;
  Buffer.Clear;
  Buffer.IsOpended := false;
  FReady := false;
  FSerial := TComPort.Create(nil);
  I2CMemory := TI2C_memory.Create();
  with FSerial do
  begin
    DataBits := dbEight;
    Parity.Bits := prNone;
    StopBits := sbOneStopBit;
    FlowControl.FlowControl := fcNone;
    LoadSettings(stIniFile, SettingsFileName);
    OnRxBuf := HandleRx;
  end;
  FTimer := TTimer.Create(nil);
  with FTimer do
  begin
    Enabled := false;
    Interval := 500;
    OnTimer := AskUpdate;
  end;
  for i := spHardware0 to High(FSerials) do
    FSerials[i] := TSerial.Create(Self, i);
end;

destructor TFirmata.Destroy;
var
  key: Word;
begin
  FTimer.Free;
  Stop;
  SerialDriver.Free;
  for key in I2CMemory.Keys do
  begin
    I2CMemory[key].Free;
  end;
  I2CMemory.Free;
  inherited;
end;

procedure TFirmata.SaveSettings;
begin
  SerialDriver.StoreSettings(stIniFile, SettingsFileName);
end;

procedure TFirmata.SetTimed(const value: Boolean);
begin
  FTimer.Enabled := value;
end;

function TFirmata.SettingsFileName: string;
begin
  result := ChangeFileExt(ParamStr(0), '.ini')
end;

function TFirmata.GetAnalog(pin: Byte): Word;
begin
  result := analogRead(pin);
end;

function TFirmata.GetDigital(pin: Byte): TPinState;
begin
  result := digitalRead(pin);
end;

function TFirmata.GetFirmwareName: string;
begin
  result := FFirmwareName;
end;

function TFirmata.GetPinMode(pin: Byte): TPinMode;
begin
  result := TPinMode(PinsInfo[pin].mode);
end;

function TFirmata.GetTimed: Boolean;
begin
  result := FTimer.Enabled;
end;

procedure TFirmata.HandleRx(Sender: TObject; const Buffer; Count: Integer);
var
  Buf: Byte;
begin
  while SerialDriver.InputCount > 0 do
  begin
    SerialDriver.read(Buf, 1);
    Process(Buf);
  end;
end;

procedure TFirmata.InitPinInfo;
var
  i: Integer;

begin
  for i := 0 to High(PinsInfo) do
  begin
    with PinsInfo[i] do
    begin
      mode := MODE_NONE;
      analog_channel := i;
      supported_modes := 0;
      value := 0;
    end;
  end;

  for i := 0 to High(AnalogPins) do
  begin
    AnalogPins[i] := ANALOG_PIN_NONE;
  end;

end;

procedure TFirmata.ReadAnalog(Cmd: Byte);
var
  Parse: TAnalogParse;
  channel: Byte;
  value, i: Integer;
begin
  SerialDriver.read(Parse.Buffer, 2);
  Parse.Cmd := Cmd;
  channel := Parse.channel;
  value := Parse.value;
  for i := 0 to High(PinsInfo) do
  begin
    if PinsInfo[i].analog_channel = channel then
    begin
      PinsInfo[i].value := value;
      Break;
    end;
  end;
end;

procedure TFirmata.ReadDigital(Cmd: Byte);
var
  Parse: TDigitalParse;
  portNum, offset: Byte;
  value: Integer;
  i: Integer;
begin
  SerialDriver.read(Parse.Buffer, 2);
  Parse.Cmd := Cmd;
  portNum := Parse.Number;
  Parse.ExtractBits;
  offset := 8 * portNum;
  for i := 0 to 7 do
  begin
    with PinsInfo[offset + i] do
    begin
      if (mode = PIN_MODE_INPUT) or (mode = PIN_MODE_PULLUP) then
        value := Parse.Bit[i];
    end;
  end;
end;

procedure TFirmata.ReadVersion;
var
  Parse: TVersionParse;
begin
  SerialDriver.read(Parse.Buffer, 2);
  with Version do
  begin
    Major := Parse.Major;
    Minor := Parse.Minor;
  end;
end;

procedure TFirmata.ReportFirmware;
var
  Major, Minor: Byte;
begin
  Major := Buffer.Data[1];
  Minor := Buffer.Data[2];
  FFirmwareName := Format('%s - %d.%d', [Buffer.Tostring(3), Major, Minor]);
end;

procedure TFirmata.CapabilityResponse;
var
  idx, pin: Integer;
  isResolution: Boolean;
  mode: Byte;
begin
  idx := 1;
  mode := 0;
  isResolution := false;
  pin := 0;
  for pin := 0 to 127 do
  begin
    PinsInfo[pin].supported_modes := 0;
  end;

  while idx < Buffer.Count do
  begin
    if (Buffer.Data[idx] = CAPABILITY_PIN_SEPARATOR) then
    begin
      Inc(pin);
      isResolution := false;
    end;

    if (isResolution) then
    begin
      PinsInfo[pin].resolution[mode] := Buffer.Data[idx];
    end
    else
    begin
      mode := Buffer.Data[idx];
      PinsInfo[pin].supported_modes := PinsInfo[pin].supported_modes or
        (1 shl mode);
    end;

    isResolution := not isResolution;
    Inc(idx);
  end;
  FReady := True;
end;

procedure TFirmata.AnalogMappingResponse;
var
  value, i, aPin: Byte;
begin
  aPin := 0;
  for i := 0 to Buffer.Count - 2 do
  begin
    value := Buffer.Data[i + 1];
    PinsInfo[i].analog_channel := value;
    if value <> ANALOG_CHANNEL_NONE then
    begin
      AnalogPins[aPin] := i;
      Inc(aPin);
    end;
  end;
end;

procedure TFirmata.PinStateResponse;
var
  pin, i: Byte;
begin
  if Buffer.Count < 4 then
    exit;

  pin := Buffer.Data[1];
  PinsInfo[pin].mode := Buffer.Data[2];
  PinsInfo[pin].value := Buffer.Data[3];

  if (Buffer.Count > 4) then
  begin
    for i := 1 to Buffer.Count - 3 do
      PinsInfo[pin].value := (PinsInfo[pin].value) or
        (Buffer.Data[3 + i] shl (7 * i));
  end;
end;

procedure TFirmata.I2cReply;
var
  sAddres, Data, reg: Byte;
  idx: Integer;
begin
  idx := 0;
  sAddres := Buffer.Data[1] or (Buffer.Data[2] shl 7);
  if not I2CMemory.ContainsKey(sAddres) then
    I2CMemory.Add(sAddres, TI2C_Register.Create());

  while idx < Buffer.Count do
  begin
    reg := Buffer.Data[idx] or (Buffer.Data[idx + 1] shl 7);
    Data := Buffer.Data[idx + 2] or (Buffer.Data[idx + 3] shl 7);
    if I2CMemory[sAddres].ContainsKey(reg) then
      I2CMemory[sAddres][reg] := Data
    else
      I2CMemory[sAddres].Add(reg, Data);
    Inc(idx, 4);
  end;
end;

procedure TFirmata.SerialMessage;
var
  Msg: AnsiString;
  portID: Byte;
  idx: Integer;
  c: AnsiChar;
begin
  Msg := '';
  portID := Buffer.Data[1] and $0F;
  idx := 2;
  while idx < Buffer.Count do
  begin
    c := AnsiChar(Buffer.Data[idx] or (Buffer.Data[idx + 1] shl 7));
    Msg := Msg + c;
    Inc(idx, 2);
    FSerials[TSerialPortID(portID)].FBuffer.Write(Ord(c));
  end;
  if Assigned(FOnSerialReceve) then
    FOnSerialReceve(Self, portID, Msg);
end;

procedure TFirmata.SetAnalog(pin: Byte; const value: Word);
begin
  analogWrite(pin, value);
end;

procedure TFirmata.SetDigital(pin: Byte; const value: TPinState);
begin
  digitalWrite(pin, value);
end;

procedure TFirmata.SerialData;
var
  Msg: AnsiString;
  portID: Byte;
  idx: Integer;
  c: AnsiChar;
begin
  Msg := '';
  idx := 1;
  while idx < Buffer.Count do
  begin
    c := AnsiChar((Buffer.Data[idx] and $7F) or
      ((Buffer.Data[idx + 1] and $7F) shl 7));
    Msg := Msg + c;
    Inc(idx, 2);
  end;

  if Assigned(FOnSerialDataReceve) then
    FOnSerialDataReceve(Self, Msg);
end;

procedure TFirmata.ProcessEx;
begin
  case Buffer.Data[0] of
    REPORT_FIRMWARE:
      begin
        ReportFirmware;
        AskBoardCapabilities();
      end;
    CAPABILITY_RESPONSE:
      CapabilityResponse;
    ANALOG_MAPPING_RESPONSE:
      AnalogMappingResponse;
    PIN_STATE_RESPONSE:
      PinStateResponse;
    I2C_REPLY:
      I2cReply;
    SERIAL_MESSAGE:
      SerialMessage;
    STRING_DATA:
      SerialData;
  else
    // Do nothing for unknow messages
  end;
end;

procedure TFirmata.Process(value: Byte);
var
  Cmd: Byte;
begin
  if Buffer.IsOpended then
  begin
    if value = END_SYSEX then
    begin
      ProcessEx;
      Buffer.Clear;
      Buffer.IsOpended := false;
    end
    else
      Buffer.append(value);
  end
  else
  begin
    Cmd := value and $F0;
    case Cmd of
      ANALOG_MESSAGE:
        begin
          ReadAnalog(value);
          exit;
        end;
      DIGITAL_MESSAGE:
        begin
          ReadDigital(value);
          exit;
        end;
      REPORT_VERSION:
        begin
          ReadVersion;
          exit;
        end;
    end;

    case value of
      START_SYSEX:
        begin
          Buffer.IsOpended := True;
        end;
    end;
  end;
end;

function TFirmata.Start: Boolean;
begin
  result := True;
  try
    SerialDriver.Open();
  except
    result := false;
  end;
end;

function TFirmata.Start(Port: string): Boolean;
begin
  SerialDriver.Port := Port;
  result := Start;
end;

function TFirmata.Stop: Boolean;
begin
  result := True;
  if not SerialDriver.Connected then
    exit;
  try
    SerialDriver.Close();
  except
    result := false;
  end;
end;

procedure TFirmata.Write(Header, Buf: array of Byte;
  IncludeHeadAndTail: Boolean);
begin
  if IncludeHeadAndTail then
    WriteHead;
  SerialDriver.Write(Header, Length(Header));
  SerialDriver.Write(Buf, Length(Buf));
  if IncludeHeadAndTail then
    WriteTail;
end;

procedure TFirmata.WriteHead;
var
  b: Byte;
begin
  b := START_SYSEX;
  SerialDriver.Write(b, 1);
end;

procedure TFirmata.WriteTail;
var
  b: Byte;
begin
  b := END_SYSEX;
  SerialDriver.Write(b, 1);
end;

procedure TFirmata.Write(Buf: array of Byte; IncludeHeadAndTail: Boolean);

begin
  if IncludeHeadAndTail then
    WriteHead;
  SerialDriver.Write(Buf, Length(Buf));
  if IncludeHeadAndTail then
    WriteTail;
end;

procedure TFirmata.Write(Buf: Byte; IncludeHeadAndTail: Boolean = True);
begin
  if IncludeHeadAndTail then
    WriteHead;
  SerialDriver.Write(Buf, 1);
  if IncludeHeadAndTail then
    WriteTail;
end;

procedure TFirmata.setPinMode(pin: Byte; mode: TPinMode);
begin
  Write([SET_PIN_MODE, ReturnAnalogPinNumber(pin), Ord(mode)], false);
  PinsInfo[pin].mode := Ord(mode);
  PinsInfo[pin].value := 0;
end;

procedure TFirmata.digitalWrite(pin: Byte; value: TPinState);
begin
  Write([SET_DIGITAL_PIN_VALUE, ReturnAnalogPinNumber(pin), Ord(value)], false);
  PinsInfo[pin].value := Ord(value);
end;

function TFirmata.digitalRead(pin: Byte): TPinState;
begin
  result := TPinState(PinsInfo[pin].value);
end;

procedure TFirmata.digitalReport(Port: Byte; enab: Boolean);
begin
  Write([REPORT_DIGITAL OR Port, Ord(enab)], false);
end;

procedure TFirmata.analogReport(pin: Byte; enab: Boolean);
begin
  Write([REPORT_DIGITAL OR pin, Ord(enab)], false);
end;

function TFirmata.ReturnAnalogPinNumber(pin: Byte): Byte;
begin
  if pin >= $A0 then
  begin
    result := AnalogPins[(pin and $0F)];
  end
  else
    result := pin;
end;

procedure TFirmata.analogWrite(pin: Byte; value: Word);
var
  Buf: array [0 .. 2] of Byte;
begin
  Buf[0] := ANALOG_MESSAGE OR pin;
  Buf[1] := value AND $7F;
  Buf[2] := (value shr 7) AND $7F;

  Write(Buf, false);
  PinsInfo[pin].value := value;
end;

function TFirmata.analogRead(pin: Byte): Word;
begin
  pin := ReturnAnalogPinNumber(pin);
  analogRead := PinsInfo[pin].value;
end;

procedure TFirmata.PrintPinInfo(Info: TStrings);
var
  pin: TPin;
  idx: Integer;
begin
  idx := 0;
  for pin in PinsInfo do
  begin
    Info.Add(idx.Tostring + ':' + pin.analog_channel.Tostring);
    Inc(idx);
  end;
end;

procedure TFirmata.I2cRequest(mode: TI2C_RequestMode; Data: array of Byte);
var
  Buf: array of Byte;
  Parse: TI2C_RequestParse;
  i: Integer;
begin
  Parse.Pack(mode);
  SetLength(Buf, 2 * Length(Data));
  ExpandArray(Data, Buf);
  Write([I2C_REQUEST, Parse.Lsb, Parse.Msb], Buf);
end;

class procedure TFirmata.ExpandArray(DataIn: array of Byte;
  var DataOut: array of Byte);
var
  i: Integer;
begin
  // SetLength(DataOut, Length(DataIn) * 2);
  for i := 0 to High(DataIn) do
  begin
    DataOut[i * 2] := DataIn[i] and $7F;
    DataOut[(i * 2) + 1] := (DataIn[i] shl 7) and $7F;
  end;
end;

class procedure TFirmata.ContractArray(DataIn: array of Byte;
  var DataOut: array of Byte);
var
  i: Integer;
begin
  // SetLength(DataOut, Length(DataIn) div 2);
  for i := 0 to High(DataOut) do
  begin
    DataOut[i] := (DataIn[i * 2 + 1] shr 7) or (DataIn[i * 2] and $7F);
  end;
end;

procedure TFirmata.I2cConfig(delay: uint16; Data: array of Byte);
var
  Buf: array of Byte;
begin
  SetLength(Buf, 2 * Length(Data));
  ExpandArray(Data, Buf);
  write([I2C_CONFIG, (delay and $7F), ((delay shr 7) and $7F)], Buf);
end;

procedure TFirmata.SerialWrite(portID: TSerialPortID; Msg: AnsiString);
var
  slice: AnsiString;
  maxLength: Integer;
  Buf: array of Byte;
  i: Integer;
begin
  maxLength := (MAX_DATA_BYTES - 5) div 2;
  repeat
    if (Length(Msg) >= maxLength) then
    begin
      slice := Copy(Msg, 1, maxLength);
      Delete(Msg, 1, maxLength);
    end
    else
    begin
      slice := Msg;
      Msg := '';
    end;

    SetLength(Buf, Length(slice) * 2 + 2);
    Buf[0] := SERIAL_MESSAGE;
    Buf[1] := SERIAL_WRITE or Ord(portID);

    for i := 1 to Length(slice) do
    begin
      Buf[(i * 2)] := Word(slice[i]) and $7F;
      Buf[(i * 2) + 1] := (Word(slice[i]) shr 7) and $7F;
    end;
    Write(Buf);
  until Msg = '';
end;

procedure TFirmata.SerialRead(portID: TSerialPortID;
  maxBytesToRead: Integer = 0);
begin
  if maxBytesToRead > 0 then
    Write([SERIAL_MESSAGE, SERIAL_READ or Ord(portID), SERIAL_READ_MODE_CONT,
      (maxBytesToRead and $7F), ((maxBytesToRead shr 7) and $7F)])
  else
    Write([SERIAL_MESSAGE, SERIAL_READ or Ord(portID), SERIAL_READ_MODE_CONT]);
end;

procedure TFirmata.SerialStop(portID: TSerialPortID);
begin
  Write([SERIAL_MESSAGE, SERIAL_READ or Ord(portID), SERIAL_READ_MODE_STOP]);
end;

procedure TFirmata.SerialFlush(portID: TSerialPortID);
begin
  Write([SERIAL_MESSAGE, SERIAL_FLUSH or Ord(portID)]);
end;

procedure TFirmata.SerialListen(portID: TSerialPortID);
begin
  if Ord(portID) < 8 then
    exit;
  // listen only applies to software serial ports
  Write([SERIAL_MESSAGE, SERIAL_LISTEN or Ord(portID)]);
end;

procedure TFirmata.SerialClose(portID: TSerialPortID);
begin
  Write([SERIAL_MESSAGE, SERIAL_CLOSE or Ord(portID)]);
end;

procedure TFirmata.SerialHWConfig(portID: TSerialPortID; baudRate: LongWord);
var
  baud: array [0 .. 2] of Byte;
  i: Integer;
begin
  if Ord(portID) > 7 then
    exit; // PortID > 7 are software serial

  for i := 0 to 2 do
    baud[i] := ((baudRate shr (i * 7)) and $7F);
  Write([SERIAL_MESSAGE, SERIAL_CONFIG or Ord(portID), baud[0], baud[1],
    baud[2]]);
end;

procedure TFirmata.SerialSWConfig(portID: TSerialPortID; baudRate: LongWord;
  rx_pin: Byte; tx_pin: Byte);
var
  baud: array [0 .. 4] of Byte;
  i: Integer;
begin
  if Ord(portID) < 8 then
    exit; // PortID < 8 are hardware serial

  for i := 0 to 2 do
    baud[i] := ((baudRate shr (i * 7)) and $7F);
  baud[3] := rx_pin and $7F;
  baud[4] := tx_pin and $7F;
  Write([SERIAL_MESSAGE, SERIAL_CONFIG or Ord(portID), baud[0], baud[1],
    baud[2], baud[3], baud[4]]);
end;

class procedure TFirmata.AvailableSerialPorts(Ports: TStrings);
begin
  EnumComPorts(Ports);
end;

{ TSerial }

procedure TSerial.baudRate(baud: LongWord);
begin
  if Ord(FPortID) > 7 then
    raise Exception.Create('RX and TX pins must to be expecify for softSerial');
  FParent.SerialHWConfig(FPortID, baud);
end;

function TSerial.available: Byte;
begin
  result := FBuffer.available;
end;

procedure TSerial.baudRate(baud: LongWord; RxPin, TXPin: Byte);
begin
  if Ord(FPortID) > 7 then
    FParent.SerialSWConfig(FPortID, baud, RxPin, TXPin)
  else
    FParent.SerialHWConfig(FPortID, baud);
end;

constructor TSerial.Create(AOwner: TFirmata; portID: TSerialPortID);
begin
  FTimeout := 1000;
  FBuffer.Initialize;
  FParent := AOwner;
  FPortID := portID;
end;

procedure TSerial.Flush;
begin
  FParent.SerialFlush(FPortID);
end;

procedure TSerial.Listen;
begin
  FParent.SerialListen(FPortID);
end;

function TSerial.ParseFloat: Double;
begin
  result := ParseFloat(lhSkipAll, 0);
end;

function TSerial.ParseInt(lookahead: TLookAHead; ignore: AnsiChar): Integer;
begin
  result := ParseInt(lookahead, Ord(ignore));
end;

function TSerial.peekNextDigit(lookahead: TLookAHead;
  detectDecimal: Boolean): Byte;
var
  c: Byte;
begin
  result := $FF;
  while True do
  begin
    c := TimedPeek;
    if (c = $FF) or ((c >= $30) and (c <= $39)) or (detectDecimal and (c = $46))
    then
      exit(c);
    case lookahead of
      lhSkipAll:
        ;
      hlSkipNone:
        exit($FF);
      lhSkipWhiteSpace:
        case c of
          $09, $32, $10, $13:
            ;
        else
          exit($FF);
        end;
    end;
    read;
  end;
end;

function TSerial.TimedPeek(): Byte;
var
  Start: Cardinal;
  c: Byte;
begin
  result := $FF;
  Start := GetTickCount;
  repeat
    c := peek;
    if c <> $FF then
      exit(c);
  until ((GetTickCount - Start) >= FTimeout);
end;

function TSerial.TimedRead: Byte;
var
  Start: Cardinal;
  c: Byte;
begin
  result := $FF;
  Start := GetTickCount;
  repeat
    c := read;
    if c <> $FF then
      exit(c);
  until ((GetTickCount - Start) >= FTimeout);
end;

function TSerial.ParseInt(lookahead: TLookAHead; ignore: Byte): Integer;
var
  isNegative: Boolean;
  c: Byte;
begin
  isNegative := false;
  result := 0;
  c := peekNextDigit(lookahead, false);
  if c = $FF then
    exit(0); // Timeout event

  repeat
    if c <> ignore then
      case c of
        $95: // '-'
          isNegative := True;
        $30 .. $39: // '0'..'9'
          result := result * 10 + (c - $30);
      end;
    read;
    c := TimedPeek;
  until (not(c in [$30 .. $39]) and (c <> ignore));
  if isNegative then
    result := -result;
end;

function TSerial.ParseInt(lookahead: TLookAHead): Integer;
begin
  result := ParseInt(lookahead, 0);
end;

function TSerial.ParseFloat(lookahead: TLookAHead): Double;
begin
  result := ParseFloat(lookahead, 0);
end;

function TSerial.ParseFloat(lookahead: TLookAHead; ignore: Byte): Double;
var
  isNegative, isFraction: Boolean;
  c: Byte;
  fraction: Double;
begin
  isNegative := false;
  isFraction := false;
  fraction := 1.0;
  result := 0.0;
  c := peekNextDigit(lookahead, True);
  if c = $FF then
    exit(0.0); // Timeout event

  repeat
    if c <> ignore then
      case c of
        $95: // '-'
          isNegative := True;
        $46:
          isFraction := True;
        $30 .. $39: // '0'..'9'
          begin
            result := result * 10 + (c - $30);
            if isFraction then
              fraction := fraction * 0.1;
          end;
      end;
    read;
    c := TimedPeek;
  until (not(c in [$30 .. $39]) and (c <> ignore)) or
    ((c = $46) and isFraction);
  if isNegative then
    result := -result;
  if isFraction then
    result := result * fraction;
end;

function TSerial.ParseFloat(lookahead: TLookAHead; ignore: AnsiChar): Double;
begin
  result := ParseFloat(lookahead, Ord(ignore));
end;

function TSerial.ParseInt: Integer;
begin
  result := ParseInt(lhSkipAll, 0);
end;

function TSerial.peek: Byte;
begin
  result := FBuffer.peek;
end;

procedure TSerial.Print(Msg: string);
begin
  FParent.SerialWrite(FPortID, Msg);
end;

procedure TSerial.Println(Msg: string);
begin
  Print(Msg + #13);
end;

function TSerial.read: Byte;
begin
  result := FBuffer.read;
end;

function TSerial.ReadBytes(var Buffer: AnsiString; Length: Integer): Integer;
var
  Buf: array of Byte;
begin
  SetLength(Buf, Length);
  result := ReadBytes(Buf, Length);
  SetString(Buffer, PAnsiChar(@Buf[0]), result);
end;

function TSerial.ReadBytes(var Buffer: array of Byte; Length: Integer): Integer;
var
  c: Byte;
begin
  result := 0;
  while result < Length do
  begin
    c := TimedRead;
    if (c = $FF) then
      Break;
    Buffer[result] := c;
    result := result + 1;
  end;
end;

function TSerial.ReadBytesUntil(terminator: AnsiChar; var Buffer: AnsiString;
  Length: Integer): Integer;
var
  Buf: array of Byte;
begin
  SetLength(Buf, Length);
  result := ReadBytesUntil(Ord(terminator), Buf, Length);
  SetString(Buffer, PAnsiChar(@Buf[0]), result);
end;

function TSerial.ReadBytesUntil(terminator: Byte; var Buffer: array of Byte;
  Length: Integer): Integer;
var
  value: Byte;
begin
  result := 0;
  if Length < 0 then
    exit;
  while result < Length do
  begin
    value := TimedRead;
    if (value = $FF) or (value = terminator) then
      Break;
    Buffer[result] := value;
    result := result + 1;
  end;
end;

function TSerial.ReadString(terminator: Byte): AnsiString;
var
  c: Byte;
begin
  result := '';
  c := TimedRead;
  while c <> $FF do
  begin
    result := result + AnsiChar(c);
    c := TimedRead;
  end;
end;

function TSerial.ReadString(terminator: AnsiChar): AnsiString;
begin
  result := ReadString(Ord(terminator));
end;

procedure TSerial.Stop;
begin
  FParent.SerialStop(FPortID);
end;

end.
