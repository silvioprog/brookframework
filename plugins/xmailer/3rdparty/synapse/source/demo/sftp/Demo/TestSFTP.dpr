program TestSFTP;

uses
  Forms,
  Main in 'Main.pas' {TestSFTPForm},
  SimpleSFTP in '..\SimpleSFTP.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestSFTPForm, TestSFTPForm);
  Application.Run;
end.
