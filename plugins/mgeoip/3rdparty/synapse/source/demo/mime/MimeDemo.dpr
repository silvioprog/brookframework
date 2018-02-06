program MimeDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MIMEmess in 'MIMEmess.pas',
  MIMEpart in 'MIMEpart.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
