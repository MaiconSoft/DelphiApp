unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Firmata, Vcl.ExtCtrls, CPort,
  Vcl.StdCtrls, Firmata.Types, CPortCtl, Vcl.ComCtrls, Vcl.CheckLst,
  System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TForm1 = class(TForm)
    fmtArduino: TFirmata;
    cpSerial: TComPort;
    grp1: TGroupBox;
    lbl1: TLabel;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    grp2: TGroupBox;
    cmb1: TComComboBox;
    pgc1: TPageControl;
    tsDigital: TTabSheet;
    tsAnalog: TTabSheet;
    lblA0: TLabel;
    pbA0: TProgressBar;
    lblA1: TLabel;
    pbA1: TProgressBar;
    pbA2: TProgressBar;
    pbA3: TProgressBar;
    lblA2: TLabel;
    lblA3: TLabel;
    lblA4: TLabel;
    pbA4: TProgressBar;
    lblA5: TLabel;
    pbA5: TProgressBar;
    lblA6: TLabel;
    pbA6: TProgressBar;
    lblA7: TLabel;
    pbA7: TProgressBar;
    pbA12: TProgressBar;
    pbA13: TProgressBar;
    lblA12: TLabel;
    lblA13: TLabel;
    pbA14: TProgressBar;
    lblA14: TLabel;
    lblA15: TLabel;
    pbA15: TProgressBar;
    pbA11: TProgressBar;
    pbA10: TProgressBar;
    pbA9: TProgressBar;
    pbA8: TProgressBar;
    lblA8: TLabel;
    lblA9: TLabel;
    lblA10: TLabel;
    lblA11: TLabel;
    lvDigitais: TListView;
    ilDigital: TImageList;
    pmMode: TPopupMenu;
    chkAnalogEnable: TCheckBox;
    btn4: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure fmtArduinoAnalogChange(Sender: TObject;
      const AnalogNumber: TAnalogPinsEnum; const value: Word);
    procedure fmtArduinoBoardChange(Sender: TObject; const Pin: Integer;
      Info: TPin);
    procedure fmtArduinoBeginBoardCapability(Sender: TObject);
    procedure fmtArduinoDigitalChange(Sender: TObject; const PinNumber: Integer;
      const PinValue: TPinState);
    procedure pmModePopup(Sender: TObject);
    procedure fmtArduinoPinStateChange(Sender: TObject; const Pin: Integer;
      Info: TPin);
    procedure chkAnalogEnableClick(Sender: TObject);
    procedure lvDigitaisCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    function IndexOfCaption(lv: TListView; Caption: string): Integer;
    procedure ModeChangeClick(Sender: TObject);
    procedure ModeOutputClick(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses CommCtrl, Themes;

const
  StatusColumnIndex = 2;

procedure DrawStatus(DC: HDC; R: TRect; State: TCustomDrawState; Font: TFont;
  const Txt: String; Progress: Single);
var
  TxtRect: TRect;
  S: String;
  Details: TThemedElementDetails;
  SaveBrush: HBRUSH;
  SavePen: HPEN;
  TxtFont: TFont;
  SaveFont: HFONT;
  SaveTextColor: COLORREF;
begin
  FillRect(DC, R, 0);
  InflateRect(R, -1, -1);
  TxtRect := R;
  S := Txt;
  if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tpBar);
    ThemeServices.DrawElement(DC, Details, R, nil);
    InflateRect(R, -2, -2);
    R.Right := R.Left + Trunc((R.Right - R.Left) * Progress);
    Details := ThemeServices.GetElementDetails(tpChunk);
    ThemeServices.DrawElement(DC, Details, R, nil);
  end
  else
  begin
    SavePen := SelectObject(DC, CreatePen(PS_NULL, 0, 0));
    SaveBrush := SelectObject(DC, CreateSolidBrush($00EBEBEB));
    Inc(R.Right);
    Inc(R.Bottom);
    RoundRect(DC, R.Left, R.Top, R.Right, R.Bottom, 3, 3);
    R.Right := R.Left + Trunc((R.Right - R.Left) * Progress);
    DeleteObject(SelectObject(DC, CreateSolidBrush($00FFC184)));
    RoundRect(DC, R.Left, R.Top, R.Right, R.Bottom, 3, 3);
    if R.Right > R.Left + 3 then
      Rectangle(DC, R.Right - 3, R.Top, R.Right, R.Bottom);
    DeleteObject(SelectObject(DC, SaveBrush));
    DeleteObject(SelectObject(DC, SavePen));
  end;
  TxtFont := TFont.Create;
  try
    TxtFont.Assign(Font);
    TxtFont.Height := TxtRect.Bottom - TxtRect.Top;
    TxtFont.Color := clGrayText;
    SetBkMode(DC, TRANSPARENT);
    SaveFont := SelectObject(DC, TxtFont.Handle);
    SaveTextColor := SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));
    DrawText(DC, PChar(S), -1, TxtRect, DT_SINGLELINE or DT_CENTER or
      DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
    SetBkMode(DC, TRANSPARENT);
  finally
    DeleteObject(SelectObject(DC, SaveFont));
    SetTextColor(DC, SaveTextColor);
    TxtFont.Free;
  end;
end;

procedure TForm1.lvDigitaisCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  ListView: TListView absolute Sender;
  R: TRect;
  value, max: Integer;
begin
  DefaultDraw := SubItem <> StatusColumnIndex;
  if DefaultDraw then
    Exit;
  DefaultDraw := not TryStrToInt(Item.SubItems[1], value);
  if DefaultDraw then
    Exit;
  DefaultDraw := not TryStrToInt(Item.SubItems[2], max);

  if not DefaultDraw then
  begin
    ListView_GetSubItemRect(ListView.Handle, Item.Index, SubItem,
      LVIR_BOUNDS, @R);
    DrawStatus(ListView.Canvas.Handle, R, State, ListView.Font, value.ToString,
      value / max);
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  fmtArduino.Start(cmb1.text);
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  fmtArduino.Stop;
end;

procedure TForm1.chkAnalogEnableClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 15 do
    fmtArduino.analogReport(i, chkAnalogEnable.Checked);
end;

procedure TForm1.fmtArduinoAnalogChange(Sender: TObject;
  const AnalogNumber: TAnalogPinsEnum; const value: Word);
var
  channel: Integer;
  ProgressBar: TProgressBar;
  pLabel: TLabel;
  Index, resolution: Integer;
begin
  channel := Ord(AnalogNumber);
  resolution := fmtArduino.PinResolution[$A0 + channel, pmAnalog];
  ProgressBar := FindComponent('pbA' + channel.ToString) as TProgressBar;
  ProgressBar.Position := value;
  pLabel := FindComponent('lblA' + channel.ToString) as TLabel;
  pLabel.Caption := Format('A%d(%d)', [channel, value]);

  if fmtArduino.PinMode[$A0 + channel] <> pmAnalog then
    Exit;

  index := IndexOfCaption(lvDigitais, 'A' + channel.ToString);

  if index = -1 then
    Exit;

  lvDigitais.Items[index].ImageIndex := -1;
  lvDigitais.Items[index].SubItems[1] := value.ToString;
  lvDigitais.Items[index].SubItems[2] := ((1 shl resolution) - 1).ToString;
end;

procedure TForm1.fmtArduinoBeginBoardCapability(Sender: TObject);
begin
  lvDigitais.Items.Clear;
end;

function TForm1.IndexOfCaption(lv: TListView; Caption: string): Integer;
var
  i: Integer;
begin
  for i := 0 to lv.Items.Count - 1 do
  begin
    if SameText(lv.Items[i].Caption, Caption) then
      Exit(i)
  end;
  Result := -1;
end;

procedure TForm1.ModeOutputClick(Sender: TObject);
var
  State, Index: Integer;
begin
  index := lvDigitais.Selected.Index;
  State := (Sender as TMenuItem).Tag;
  fmtArduino.Digital[index] := TPinState(State);
end;

procedure TForm1.ModeChangeClick(Sender: TObject);
var
  mode, Index: Integer;
begin
  index := lvDigitais.Selected.Index;
  mode := (Sender as TMenuItem).Tag;
  fmtArduino.PinMode[index] := TPinMode(mode);
end;

procedure TForm1.pmModePopup(Sender: TObject);
var
  Index: Integer;

  supports: TPinModes;
  mode: TPinMode;

  function NewMenuItem(aCaption: string; aTag: Integer; aOnClick: TNotifyEvent)
    : TMenuItem;
  begin
    Result := TMenuItem.Create(nil);
    with Result do
    begin
      Caption := aCaption;
      OnClick := aOnClick;
      Tag := aTag;
    end;
  end;

  procedure AddItemMenu(aCaption: string; mode: TPinMode);
  var
    m, sm: TMenuItem;
  begin
    m := NewMenuItem(aCaption, Ord(mode), ModeChangeClick);
    if mode = pmOutput then
    begin
      sm := NewMenuItem('LOW', 0, ModeOutputClick);
      m.Add(sm);
      sm := NewMenuItem('HIGH', 1, ModeOutputClick);
      m.Add(sm);
    end;
    pmMode.Items.Add(m);
  end;

begin
  index := lvDigitais.Selected.Index;
  supports := fmtArduino.PinSupported[index];

  pmMode.Items.Clear;

  for mode := pmInput to pmPullUp do
  begin
    if mode in supports then
    begin
      AddItemMenu(PinModeToString(mode), mode);
    end;
  end;
end;

procedure TForm1.fmtArduinoBoardChange(Sender: TObject; const Pin: Integer;
  Info: TPin);
begin
  with lvDigitais.Items.Add do
  begin
    if Pin >= $A0 then
      Caption := 'A' + (Pin and $0F).ToString
    else
      Caption := 'D' + Pin.ToString;
    SubItems.Add(PinModeToString(Info.mode));
    SubItems.Add('');
    SubItems.Add('');
    ImageIndex := 0;
  end;
end;

procedure TForm1.fmtArduinoDigitalChange(Sender: TObject;
  const PinNumber: Integer; const PinValue: TPinState);
var
  Index: Integer;
  Caption: string;
begin
  if PinNumber >= $A0 then
    Caption := 'A' + (PinNumber and $0F).ToString
  else
    Caption := 'D' + PinNumber.ToString;

  index := IndexOfCaption(lvDigitais, Caption);

  if index = -1 then
    Exit;

  lvDigitais.Items[index].ImageIndex := Ord(PinValue);
  lvDigitais.Items[index].SubItems[1] := Ord(PinValue).ToString;
  lvDigitais.Items[index].SubItems[2] := '1';
end;

procedure TForm1.fmtArduinoPinStateChange(Sender: TObject; const Pin: Integer;
  Info: TPin);
var
  Index: Integer;
  Caption: string;
begin
  if Pin >= $A0 then
    Caption := 'A' + (Pin and $0F).ToString
  else
    Caption := 'D' + Pin.ToString;

  index := IndexOfCaption(lvDigitais, Caption);

  lvDigitais.Items[index].SubItems[0] := PinModeToString(Info.mode);
end;

end.
