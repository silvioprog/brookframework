program BrookString_Example;

{$MODE DELPHI}

uses
{$IFDEF UNIX}
 {$IFDEF UseCThreads}
  CThreads,
 {$ENDIF}
{$ENDIF}
  Interfaces,
  Forms,
  BrookString_frMain;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
