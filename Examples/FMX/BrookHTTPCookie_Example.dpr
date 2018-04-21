program BrookHTTPCookie_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  BrookHTTPCookie_frMain in 'BrookHTTPCookie_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
