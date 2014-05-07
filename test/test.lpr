program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testbrookaction, testbrookapplication,
  testbrookconfigurator;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

