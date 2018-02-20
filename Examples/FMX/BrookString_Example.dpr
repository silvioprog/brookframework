program BrookString_Example;

uses
  System.StartUpCopy,
  FMX.Forms,
  BrookString_frMain in 'BrookString_frMain.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.