program BrookHTTPServer_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  BrookHTTPServer_frMain in 'BrookHTTPServer_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
