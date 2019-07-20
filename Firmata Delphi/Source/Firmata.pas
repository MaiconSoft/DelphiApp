{ *******************************************************************************
  Adaptation of project of [Sottam](https://github.com/sottam/FirmataPascal)
  for Delphi compatibility.

  Maicon (jul/2019)
  ******************************************************************************* }
unit Firmata;

interface

uses
  System.SysUtils, System.Classes, Firmata.Constants, Firmata.Types, CPort;

type
  TFirmata = class(TComponent)
  private
    { Private declarations }
    FSerial: TComPort;
    FFirmwareName: string;
    AnalogPins: TAnalogPins;
    PinsInfo: TPinInfos;
    SerialCalbacks: TCallbacks;
    Version: TVersion;
    Buffer: TSerialInfo;
    function GetFirmwareName: string;
    function SettingsFileName: string;
    procedure Process(value: Byte);
    procedure HandleRx(Sender: TObject; const Buffer; Count: Integer);
    procedure AskFirmware;
    procedure Write(Buf: array of Byte; IncludeHeadAndTail: Boolean = True);
    procedure WriteHead;
    procedure WriteTail;
    procedure InitPinInfo;
    procedure ReadAnalog(Cmd: Byte);
    procedure ReadDigital(Cmd: Byte);
    procedure ReadVersion;
    procedure ProcessEx;

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
  published
    { Published declarations }
    property Serial: TComPort read FSerial;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Firmata', [TFirmata]);
end;

{ TFirmata }

procedure TFirmata.AskFirmware;
begin
  write([REPORT_FIRMWARE]);
end;

constructor TFirmata.Create(AOwner: TComponent);
begin
  inherited;
  Buffer.Clear;
  Buffer.IsOpended := false;
  FSerial := TComPort.Create(nil);
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
begin
  Stop;
  Serial.free;
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


procedure TFirmata.ProcessEx;
begin
  case Buffer[0] of
    REPORT_FIRMWARE:
      ;
    CAPABILITY_RESPONSE:
      ;
    ANALOG_MAPPING_RESPONSE:
      ;
    PIN_STATE_RESPONSE:
      ;
    I2C_REPLY:
      ;
    SERIAL_MESSAGE:
      ;
    STRING_DATA:
      ;
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
      // process buffer
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
    Exit;
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

end.
