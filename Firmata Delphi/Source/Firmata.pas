unit Firmata;

interface

uses
  System.SysUtils, System.Classes, Firmata.Constants, Firmata.Types, CPort;

type
  TFirmata = class(TComponent)
  private
    FPort: TComPort;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property Port: TComPort read FPort write FPort;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Firmata', [TFirmata]);
end;

end.
