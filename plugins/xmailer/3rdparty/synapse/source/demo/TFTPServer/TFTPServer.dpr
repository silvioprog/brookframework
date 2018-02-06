program TFTPServer;

uses
  Forms,
  MainBox in 'MainBox.pas' {MainForm},
  TFTPDaemonThread in 'TFTPDaemonThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
