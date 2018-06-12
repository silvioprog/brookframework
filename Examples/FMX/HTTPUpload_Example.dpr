program HTTPUpload_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  HTTPUpload_frMain in 'HTTPUpload_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
