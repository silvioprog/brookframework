program httpserv;

uses
  Forms,
  main in 'main.pas' {Form1},
  http in 'http.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
