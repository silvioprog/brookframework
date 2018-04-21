program HTTPCookie_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  HTTPCookie_frMain in 'HTTPCookie_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
