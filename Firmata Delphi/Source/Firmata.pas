{ *******************************************************************************
  Adaptation of project of [Sottam](https://github.com/sottam/FirmataPascal)
  for Delphi compatibility.

  Maicon (jul/2019)
  ******************************************************************************* }
unit Firmata;

interface

uses
  System.SysUtils, System.Classes, Firmata.Constants, Firmata.Types, CPort,
  System.Generics.Collections, Vcl.ExtCtrls, Winapi.Windows, Vcl.Forms,
  LogUtils;

type
  TSerial = class;

  TFirmata = class(TComponent)
  private
    { Private declarations }
    FSerial: TComPort;
    FAnalogs: TAnalogPinsSet;
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
    FOnDigitalChange: TDigitalChangeNotify;
    FOnAnalogChange: TAnalogChangeNotify;
    FOnBeginBoardCapability: TNotifyEvent;
    FOnBoardChange: TBoardChangeNotify;
    FOnFirmware: TFirmwareNotify;
    FOnPinStateChange: TPinStateChangeNotify;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnFail: TNotifyEvent;
    FOnReady: TNotifyEvent;
    FPinSupported: TPinModes;
    function GetFirmwareName: string;
    function SettingsFileName: string;
    procedure Process(value: Byte);
    function WaitForBytes(const count: Integer;
      Timeout: Cardinal = 1000): Boolean;
    procedure HandleRx(Sender: TObject; count: Integer);
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
    function digitalRead(pin: Byte): TPinState;
    function analogRead(pin: Byte): Word;
    function ReturnAnalogPinNumber(pin: Byte): Byte;
    procedure setPinMode(pin: Byte; mode: TPinMode);
    procedure digitalWrite(pin: Byte; value: TPinState);
    procedure analogWrite(pin: Byte; value: Word);

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
    procedure AskFirmware;
    function GetTimed: Boolean;
    procedure SetTimed(const value: Boolean);
    procedure DoDigitalChange(pin: Integer);
    procedure DoAnalogChange(Number: Integer; value: Word);
    procedure DoBeginBoardCapabilities;
    procedure DoBoardChange(pin: Integer);
    procedure DoFirmware;
    procedure DoPinStateChange(pin: Integer);
    procedure DoStart;
    procedure DoStop;
    procedure DoFail;
    procedure DoReady;
    function GetPinSupported(pin: Byte): TPinModes;
    function GetPinRes(pin: Byte; mode: TPinMode): Byte;
    function PinNumberToAnalog(pin: Byte): Integer;
    function PinToInternal(pin: Byte): Byte;
    function PinToExternal(pin: Byte): Byte;
    function GetAnalogRelative(pin: Byte): double;
    procedure SetAnalogRelative(pin: Byte; const value: double);
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
    procedure digitalReport(Port: Byte; enab: Boolean);
    procedure analogReport(pin: Byte; enab: Boolean);
    property FirmwareName: string read GetFirmwareName;
    property Ready: Boolean read FReady;
    property Digital[pin: Byte]: TPinState read GetDigital write SetDigital;
    property Analog[pin: Byte]: Word read GetAnalog write SetAnalog;
    property AnalogRelative[pin: Byte]: double read GetAnalogRelative
      write SetAnalogRelative;
    property PinMode[pin: Byte]: TPinMode read GetPinMode write setPinMode;
    property PinSupported[pin: Byte]: TPinModes read GetPinSupported;
    property PinResolution[pin: Byte; mode: TPinMode]: Byte read GetPinRes;
  published
    { Published declarations }
    property TimedUpdate: Boolean read GetTimed write SetTimed;
    property SerialDriver: TComPort read FSerial;
    property OnSerialReceve: TNotifySerialEvent read FOnSerialReceve
      write FOnSerialReceve;
    property OnSerialDataReceve: TNotifySerialDataEvent read FOnSerialDataReceve
      write FOnSerialDataReceve;
    property OnDigitalChange: TDigitalChangeNotify read FOnDigitalChange
      write FOnDigitalChange;
    property Analogs: TAnalogPinsSet read FAnalogs write FAnalogs;
    property OnAnalogChange: TAnalogChangeNotify read FOnAnalogChange
      write FOnAnalogChange;
    property OnBeginBoardCapability: TNotifyEvent read FOnBeginBoardCapability
      write FOnBeginBoardCapability;
    property OnBoardChange: TBoardChangeNotify read FOnBoardChange
      write FOnBoardChange;
    property OnFirmware: TFirmwareNotify read FOnFirmware write FOnFirmware;
    property OnPinStateChange: TPinStateChangeNotify read FOnPinStateChange
      write FOnPinStateChange;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnFail: TNotifyEvent read FOnFail write FOnFail;
    property OnReady: TNotifyEvent read FOnReady write FOnReady;
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
    function ParseFloat: double; overload;
    function ParseFloat(lookahead: TLookAHead): double; overload;
    function ParseFloat(lookahead: TLookAHead; ignore: Byte): double; overload;
    function ParseFloat(lookahead: TLookAHead; ignore: AnsiChar)
      : double; overload;
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
function PinModeToString(mode: TPinMode): string;

implementation

procedure Register;
begin
  RegisterComponents('Firmata', [TFirmata]);
end;

function PinModeToString(mode: TPinMode): string;
begin
  result := PIN_MODE_STR[mode];
end;

{ TFirmata }

procedure TFirmata.AskReportAnalog(index: Integer);
begin
  Write(REPORT_ANALOG or index);
end;

procedure TFirmata.AskBoardCapabilities();
var
  i: Integer;
begin
  SavLog('call AskBoardCapabilities');
  write([ANALOG_MAPPING_QUERY]);
  write([CAPABILITY_QUERY]);
  for i := 0 to 15 do
  begin
    digitalReport(i, True);
    analogReport(i, True);
    setPinMode(ReturnAnalogPinNumber(i), pmAnalog);
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
  if not SerialDriver.Connected then
    Exit;
  for i := 0 to 127 do
  begin
    digitalReport(i, True);
    analogReport(i, True);
  end;
  for i := 0 to 15 do
    SerialRead(TSerialPortID(i));
end;

constructor TFirmata.Create(AOwner: TComponent);
var
  i: TSerialPortID;
begin
  inherited;
  FAnalogs := [];
  Buffer.Clear;
  Buffer.IsOpended := false;
  FReady := false;
  FSerial := TComPort.Create(nil);
  I2CMemory := TI2C_memory.Create();
  with FSerial do
  begin
    baudRate := br57600;
    DataBits := dbEight;
    Parity.Bits := prNone;
    StopBits := sbOneStopBit;
    FlowControl.FlowControl := fcNone;
    LoadSettings(stIniFile, SettingsFileName);
    TriggersOnRxChar := True;
    OnRxChar := HandleRx;
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

function TFirmata.GetAnalogRelative(pin: Byte): double;
var
  max: Integer;
begin
  max := (1 shl GetPinRes(pin, pmAnalog)) - 1;
  result := 100 * analogRead(pin) / max;
end;

function TFirmata.GetDigital(pin: Byte): TPinState;
begin
  pin := PinToInternal(pin);
  result := digitalRead(pin);
end;

function TFirmata.GetFirmwareName: string;
begin
  result := FFirmwareName;
end;

function TFirmata.GetPinMode(pin: Byte): TPinMode;
begin
  if pin >= $A0 then
    pin := ReturnAnalogPinNumber(pin and $0F);

  result := TPinMode(PinsInfo[pin].mode);
end;

function TFirmata.GetPinRes(pin: Byte; mode: TPinMode): Byte;
begin
  if pin >= $A0 then
    pin := ReturnAnalogPinNumber(pin and $0F);
  result := PinsInfo[pin].resolution[Ord(mode)];
end;

function TFirmata.GetPinSupported(pin: Byte): TPinModes;
begin
  result := PinsInfo[pin].supported_modes;
end;

function TFirmata.GetTimed: Boolean;
begin
  result := FTimer.Enabled;
end;

procedure TFirmata.HandleRx(Sender: TObject; count: Integer);
var
  Buf: Byte;
  i: Integer;
begin
  // SetLength(Buf, 1);
  // Move(Buffer, Buf[0], 1);

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
      mode := pmIgnore;
      analog_channel := i;
      supported_modes := [];
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
  WaitForBytes(2);
  SerialDriver.read(Parse.Buffer, 2);
  Parse.Cmd := Cmd;
  channel := Parse.channel;
  value := Parse.value;
  SavLog('analog channel ' + channel.ToString);
  SavLog('analog value ' + value.ToString);

  for i := 0 to High(PinsInfo) do
  begin
    if PinsInfo[i].analog_channel = channel then
    begin
      SavLog('analog to pin ' + i.ToString);
      if PinsInfo[i].value <> value then
      begin
        PinsInfo[i].value := value;

        if TAnalogPinsEnum(channel) in Analogs then
          DoAnalogChange(channel, value);
      end;
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
  Buffer: array [0 .. 1] of Byte;
begin
  WaitForBytes(2);
  SerialDriver.read(Parse.Buffer, 2);
  Parse.Cmd := Cmd;
  portNum := Parse.Number;
  SavLog('portNum = ' + portNum.ToString);
  Parse.ExtractBits;
  offset := 8 * portNum;

  for i := 0 to 7 do
  begin
    with PinsInfo[offset + i] do
    begin
      if (mode = pmInput) or (mode = pmPullUp) then
      begin
        // if value <> Parse.Bit[i] then
        // begin
        value := Parse.Bit[i];
        DoDigitalChange(offset + i);
        // SavLog('value[' + (offset + i).ToString + '] ' + value.ToString);
        // end;
      end;
    end;
  end;
end;

procedure TFirmata.ReadVersion;
var
  Parse: TVersionParse;
begin
  SavLog('Version report');
  WaitForBytes(2);
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
  FFirmwareName := Format('%s - %d.%d', [Buffer.ToString(3), Major, Minor]);
  DoFirmware;
end;

procedure TFirmata.CapabilityResponse;
var
  idx, pin, Analog: Integer;
  mode: Byte;
begin
  idx := 1;
  mode := 0;
  pin := 0;
  Analog := 0;
  for pin := 0 to 127 do
  begin
    PinsInfo[pin].supported_modes := [];
  end;

  DoBeginBoardCapabilities;
  pin := 0;
  while idx < Buffer.count do
  begin
    if (Buffer.Data[idx] = CAPABILITY_PIN_SEPARATOR) then
    begin
      if pmAnalog in PinsInfo[pin].supported_modes then
      begin
        DoBoardChange($A0 + Analog);
        inc(Analog);
      end
      else
        DoBoardChange(pin);
      inc(pin);
    end
    else
    begin
      mode := Buffer.Data[idx];
      Include(PinsInfo[pin].supported_modes, TPinMode(mode));
      inc(idx);
      PinsInfo[pin].resolution[mode] := Buffer.Data[idx];
    end;
    inc(idx);
  end;
  FReady := True;
  DoReady;
end;

procedure TFirmata.AnalogMappingResponse;
var
  value, i, aPin: Byte;
begin
  aPin := 0;
  for i := 0 to Buffer.count - 2 do
  begin
    value := Buffer.Data[i + 1];
    PinsInfo[i].analog_channel := value;
    if value <> ANALOG_CHANNEL_NONE then
    begin
      AnalogPins[aPin] := i;
      inc(aPin);
    end;
  end;
end;

procedure TFirmata.PinStateResponse;
var
  pin, i: Byte;
begin
  SavLog('pin response');
  if Buffer.count < 4 then
    Exit;

  pin := Buffer.Data[1];
  if Buffer.Data[2] = PIN_MODE_IGNORE then
    PinsInfo[pin].mode := pmIgnore
  else
    PinsInfo[pin].mode := TPinMode(Buffer.Data[2]);

  PinsInfo[pin].value := Buffer.Data[3];

  if (Buffer.count > 4) then
  begin
    for i := 1 to Buffer.count - 3 do
      PinsInfo[pin].value := (PinsInfo[pin].value) or
        (Buffer.Data[3 + i] shl (7 * i));
  end;
  SavLog('Mode ' + PinModeToString(PinsInfo[pin].mode));
  SavLog('Pin ' + pin.ToString);
  DoPinStateChange(pin);
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

  while idx < Buffer.count do
  begin
    reg := Buffer.Data[idx] or (Buffer.Data[idx + 1] shl 7);
    Data := Buffer.Data[idx + 2] or (Buffer.Data[idx + 3] shl 7);
    if I2CMemory[sAddres].ContainsKey(reg) then
      I2CMemory[sAddres][reg] := Data
    else
      I2CMemory[sAddres].Add(reg, Data);
    inc(idx, 4);
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
  while idx < Buffer.count do
  begin
    c := AnsiChar(Buffer.Data[idx] or (Buffer.Data[idx + 1] shl 7));
    Msg := Msg + c;
    inc(idx, 2);
    FSerials[TSerialPortID(portID)].FBuffer.Write(Ord(c));
  end;
  if Assigned(FOnSerialReceve) then
    FOnSerialReceve(Self, portID, Msg);
end;

procedure TFirmata.SetAnalog(pin: Byte; const value: Word);
begin
  analogWrite(pin, value);
end;

procedure TFirmata.SetAnalogRelative(pin: Byte; const value: double);
var
  max: Integer;
begin
  max := (1 shl GetPinRes(pin, pmPWM)) - 1;
  if max = 0 then
    Exit;
  analogWrite(pin, max * value / 100);
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
  while idx < Buffer.count do
  begin
    c := AnsiChar((Buffer.Data[idx] and $7F) or
      ((Buffer.Data[idx + 1] and $7F) shl 7));
    Msg := Msg + c;
    inc(idx, 2);
  end;

  if Assigned(FOnSerialDataReceve) then
    FOnSerialDataReceve(Self, Msg);
end;

procedure TFirmata.ProcessEx;
begin
  SavLog('ProcessEx: 0x' + Buffer.Data[0].ToHexString);
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
  if value > $7F then
    SavLog('-------');
  SavLog('RECEVE 0x' + value.ToHexString);
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
          Exit;
        end;
      DIGITAL_MESSAGE:
        begin
          ReadDigital(value);
          Exit;
        end;
      REPORT_VERSION:
        begin
          ReadVersion;
          Exit;
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
    DoStart;
    AskFirmware;
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
    Exit;
  try
    SerialDriver.Close();
    DoStop;
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

function TFirmata.WaitForBytes(const count: Integer;
  Timeout: Cardinal = 1000): Boolean;
var
  startTime: Cardinal;
begin
  if not SerialDriver.Connected then
    Exit(false);

  startTime := GetTickCount;
  while SerialDriver.InputCount < count do
  begin
    Application.ProcessMessages;
    if (GetTickCount - startTime) > Timeout then
      Exit(false);
  end;
  result := True;
end;

procedure TFirmata.Write(Buf: Byte; IncludeHeadAndTail: Boolean = True);
begin
  if IncludeHeadAndTail then
    WriteHead;
  SerialDriver.Write(Buf, 1);
  if IncludeHeadAndTail then
    WriteTail;
end;

function TFirmata.PinToInternal(pin: Byte): Byte;
begin
  if pin >= $A0 then
    result := ReturnAnalogPinNumber(pin and $0F)
  else
    result := pin;
end;

function TFirmata.PinToExternal(pin: Byte): Byte;
begin
  if pmAnalog in PinsInfo[pin].supported_modes then
    result := $A0 + PinNumberToAnalog(pin)
  else
    result := pin;
end;

procedure TFirmata.setPinMode(pin: Byte; mode: TPinMode);
begin
  pin := PinToInternal(pin);

  Write([SET_PIN_MODE, pin, Ord(mode)], false);
  Write([PIN_STATE_QUERY, pin]);

  analogReport(pin, mode = pmAnalog);
  digitalReport(pin div 8, (mode = pmInput) or (mode = pmPullUp));
end;

procedure TFirmata.digitalWrite(pin: Byte; value: TPinState);
begin
  pin := PinToInternal(pin);

  Write([SET_DIGITAL_PIN_VALUE, pin, Ord(value)], false);
  PinsInfo[pin].value := Ord(value);
end;

procedure TFirmata.DoAnalogChange(Number: Integer; value: Word);
begin
  if Assigned(OnAnalogChange) then
    OnAnalogChange(Self, TAnalogPinsEnum(Number), value);
end;

procedure TFirmata.DoBeginBoardCapabilities;
begin
  if Assigned(FOnBeginBoardCapability) then
    FOnBeginBoardCapability(Self);
end;

procedure TFirmata.DoBoardChange(pin: Integer);
begin
  if Assigned(FOnBoardChange) then
    FOnBoardChange(Self, pin, PinsInfo[pin]);
end;

procedure TFirmata.DoDigitalChange(pin: Integer);
var
  value: TPinState;
begin
  value := TPinState(PinsInfo[pin].value);

  pin := PinToExternal(pin);

  if Assigned(FOnDigitalChange) then
    FOnDigitalChange(Self, pin, value);
end;

procedure TFirmata.DoFail;
begin
  if Assigned(FOnFail) then
    FOnFail(Self);
end;

procedure TFirmata.DoFirmware;
begin
  if Assigned(FOnFirmware) then
    FOnFirmware(Self, FFirmwareName);
end;

procedure TFirmata.DoPinStateChange(pin: Integer);
var
  Info: TPin;
begin
  Info := PinsInfo[pin];
  pin := PinToExternal(pin);

  if Assigned(FOnPinStateChange) then
    FOnPinStateChange(Self, pin, Info);
end;

procedure TFirmata.DoReady;
begin
  if Assigned(FOnReady) then
    FOnReady(Self);
end;

procedure TFirmata.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TFirmata.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
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
  Write([REPORT_ANALOG OR pin, Ord(enab)], false);
end;

function TFirmata.ReturnAnalogPinNumber(pin: Byte): Byte;
begin
  result := AnalogPins[(pin and $0F)];
end;

function TFirmata.PinNumberToAnalog(pin: Byte): Integer;
var
  i: Integer;
begin
  for i := 0 to High(AnalogPins) do
    if AnalogPins[i] = pin then
      Exit(i);
  result := -1;
end;

procedure TFirmata.analogWrite(pin: Byte; value: Word);
var
  Buf: array [0 .. 2] of Byte;
begin
  pin := PinToInternal(pin);
  if not(pmPWM in PinsInfo[pin].supported_modes) then
    Exit;

  if (pmPWM <> PinsInfo[pin].mode) then
    setPinMode(pin, pmPWM);

  Buf[0] := ANALOG_MESSAGE OR pin;
  Buf[1] := value AND $7F;
  Buf[2] := (value shr 7) AND $7F;

  Write(Buf, false);
  PinsInfo[pin].value := value;
end;

function TFirmata.analogRead(pin: Byte): Word;
begin
  pin := ReturnAnalogPinNumber(pin);
  result := PinsInfo[pin].value;
end;

procedure TFirmata.PrintPinInfo(Info: TStrings);
var
  pin: TPin;
  idx: Integer;
begin
  idx := 0;
  for pin in PinsInfo do
  begin
    Info.Add(idx.ToString + ':' + pin.analog_channel.ToString);
    inc(idx);
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
    Exit;
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
    Exit; // PortID > 7 are software serial

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
    Exit; // PortID < 8 are hardware serial

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

function TSerial.ParseFloat: double;
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
      Exit(c);
    case lookahead of
      lhSkipAll:
        ;
      hlSkipNone:
        Exit($FF);
      lhSkipWhiteSpace:
        case c of
          $09, $32, $10, $13:
            ;
        else
          Exit($FF);
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
      Exit(c);
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
      Exit(c);
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
    Exit(0);
  // Timeout event

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

function TSerial.ParseFloat(lookahead: TLookAHead): double;
begin
  result := ParseFloat(lookahead, 0);
end;

function TSerial.ParseFloat(lookahead: TLookAHead; ignore: Byte): double;
var
  isNegative, isFraction: Boolean;
  c: Byte;
  fraction: double;
begin
  isNegative := false;
  isFraction := false;
  fraction := 1.0;
  result := 0.0;
  c := peekNextDigit(lookahead, True);
  if c = $FF then
    Exit(0.0); // Timeout event

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

function TSerial.ParseFloat(lookahead: TLookAHead; ignore: AnsiChar): double;
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
    Exit;
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
