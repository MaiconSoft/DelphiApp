object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 322
  ClientWidth = 630
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 216
    Top = 160
    Width = 16
    Height = 13
    Caption = 'lbl1'
  end
  object btn1: TButton
    Left = 240
    Top = 72
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 384
    Top = 72
    Width = 75
    Height = 25
    Caption = 'btn2'
    TabOrder = 1
    OnClick = btn2Click
  end
  object btn3: TButton
    Left = 384
    Top = 155
    Width = 75
    Height = 25
    Caption = 'btn3'
    TabOrder = 2
    OnClick = btn3Click
  end
  object mmo1: TMemo
    Left = 8
    Top = 232
    Width = 561
    Height = 89
    Lines.Strings = (
      'mmo1')
    TabOrder = 3
  end
  object tmr1: TTimer
    Enabled = False
    OnTimer = tmr1Timer
    Left = 528
    Top = 72
  end
  object fmtArduino: TFirmata
    TimedUpdate = False
    Analogs = [A0]
    OnAnalogChange = fmtArduinoAnalogChange
    Left = 80
    Top = 32
  end
  object cpSerial: TComPort
    BaudRate = br9600
    Port = 'COM1'
    Parity.Bits = prNone
    StopBits = sbOneStopBit
    DataBits = dbEight
    Events = [evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS, evDSR, evError, evRLSD, evRx80Full]
    FlowControl.OutCTSFlow = False
    FlowControl.OutDSRFlow = False
    FlowControl.ControlDTR = dtrDisable
    FlowControl.ControlRTS = rtsDisable
    FlowControl.XonXoffOut = False
    FlowControl.XonXoffIn = False
    StoredProps = [spBasic]
    TriggersOnRxChar = True
    Left = 32
    Top = 32
  end
end
