program AG_Geradores;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  BitArray in 'BitArray.pas',
  ProblemGerator in 'ProblemGerator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
