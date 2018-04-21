program HTTPAuth_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  HTTPAuth_frMain in 'HTTPAuth_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
