unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Firmata, Vcl.ExtCtrls, CPort,
  Vcl.StdCtrls, Firmata.Types;

type
  TForm1 = class(TForm)
    tmr1: TTimer;
    fmtArduino: TFirmata;
    cpSerial: TComPort;
    btn1: TButton;
    lbl1: TLabel;
    btn2: TButton;
    btn3: TButton;
    mmo1: TMemo;
    procedure btn1Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure fmtArduinoAnalogChange(Sender: TObject;
      const AnalogNumber: TAnalogPinsEnum; const value: Word);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  fmtArduino.SerialDriver.Port := 'COM9';
  fmtArduino.SerialDriver.BaudRate := br57600;
  fmtArduino.Start('COM9');
  // fmtArduino.SerialDriver.TryConect;
  // fmtArduino.AskFirmware;
  // fmtArduino.TimedUpdate := false;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  fmtArduino.AskFirmware();
  // fmtArduino.Analog[0]

end;

procedure TForm1.btn3Click(Sender: TObject);
begin
   fmtArduino.Stop;
  // fmtArduino.Analog
end;

procedure TForm1.fmtArduinoAnalogChange(Sender: TObject;
  const AnalogNumber: TAnalogPinsEnum; const value: Word);
begin
  if AnalogNumber = A0 then
    mmo1.Lines.Add(value.ToString);
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  // lbl1.Caption := Ord(fmtArduino.Digital[7]).ToString;
  lbl1.Caption := fmtArduino.Analog[$0].ToString;
  Caption := fmtArduino.FirmwareName;
end;

end.
