program BrookHTTPAuth_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  BrookHTTPAuth_frMain in 'BrookHTTPAuth_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
