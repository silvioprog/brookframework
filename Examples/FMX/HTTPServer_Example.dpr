program HTTPServer_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  HTTPServer_frMain in 'HTTPServer_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
