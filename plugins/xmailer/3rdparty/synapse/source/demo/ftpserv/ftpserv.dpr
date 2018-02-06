program ftpserv;

uses
  Forms,
  main in 'main.pas' {Form1},
  ftpthrd in 'ftpthrd.pas',
  ftpmain in 'ftpmain.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
