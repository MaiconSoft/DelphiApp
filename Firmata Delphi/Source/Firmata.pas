{ *******************************************************************************
  Adaptation of project of [Sottam](https://github.com/sottam/FirmataPascal)
  for Delphi compatibility.

  Maicon (jul/2019)
  ******************************************************************************* }
unit Firmata;

interface

uses
  System.SysUtils, System.Classes, Firmata.Constants, Firmata.Types, CPort,
  System.Generics.Collections;

type
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
    function GetFirmwareName: string;
    function SettingsFileName: string;
    procedure Process(value: Byte);
    procedure HandleRx(Sender: TObject; const Buffer; Count: Integer);
    procedure AskFirmware;
    procedure Write(Buf: array of Byte;
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
  published
    { Published declarations }
    property Serial: TComPort read FSerial;
    property OnSerialReceve: TNotifySerialEvent read FOnSerialReceve
      write FOnSerialReceve;
    property OnSerialDataReceve: TNotifySerialDataEvent read FOnSerialDataReceve
      write FOnSerialDataReceve;
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

constructor TFirmata.Create(AOwner: TComponent);
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
end;

destructor TFirmata.Destroy;
var
  key: Word;
begin
  Stop;
  Serial.free;
  for key in I2CMemory.Keys do
  begin
    I2CMemory[key].free;
  end;
  I2CMemory.free;
  inherited;
end;

procedure TFirmata.SaveSettings;
begin
  Serial.StoreSettings(stIniFile, SettingsFileName);
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

end;

procedure TFirmata.HandleRx(Sender: TObject; const Buffer; Count: Integer);
var
  Buf: Byte;
begin
  while Serial.InputCount > 0 do
  begin
    Serial.read(Buf, 1);
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
  Serial.read(Parse.Buffer, 2);
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
  Serial.read(Parse.Buffer, 2);
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
  Serial.read(Parse.Buffer, 2);
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
  msg: string;
  portID: Byte;
  idx: Integer;
  c: Word;
begin
  msg := '';
  portID := Buffer.Data[1] and $0F;
  idx := 2;
  while idx < Buffer.Count do
  begin
    c := Buffer.Data[idx] or (Buffer.Data[idx + 1] shl 7);
    msg := msg + char(c);
    Inc(idx, 2);
  end;
  if Assigned(FOnSerialReceve) then
    FOnSerialReceve(Self, portID, msg);
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
  msg: string;
  portID: Byte;
  idx: Integer;
  c: Word;
begin
  msg := '';
  idx := 1;
  while idx < Buffer.Count do
  begin
    c := (Buffer.Data[idx] and $7F) or ((Buffer.Data[idx + 1] and $7F) shl 7);
    msg := msg + char(c);
    Inc(idx, 2);
  end;
  if Assigned(FOnSerialDataReceve) then
    FOnSerialDataReceve(Self, msg);
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
    Serial.Open();
  except
    result := false;
  end;
end;

function TFirmata.Start(Port: string): Boolean;
begin
  Serial.Port := Port;
  result := Start;
end;

function TFirmata.Stop: Boolean;
begin
  result := True;
  if not Serial.Connected then
    exit;
  try
    Serial.Close();
  except
    result := false;
  end;
end;

procedure TFirmata.WriteHead;
var
  b: Byte;
begin
  b := START_SYSEX;
  Serial.Write(b, 1);
end;

procedure TFirmata.WriteTail;
var
  b: Byte;
begin
  b := END_SYSEX;
  Serial.Write(b, 1);
end;

procedure TFirmata.Write(Buf: array of Byte; IncludeHeadAndTail: Boolean);

begin
  if IncludeHeadAndTail then
    WriteHead;
  Serial.Write(Buf, Length(Buf));
  if IncludeHeadAndTail then
    WriteTail;
end;

procedure TFirmata.Write(Buf: Byte; IncludeHeadAndTail: Boolean = True);

begin
  if IncludeHeadAndTail then
    WriteHead;
  Serial.Write(Buf, 1);
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
  result := PinsInfo[pin].value > 0;
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

end.
