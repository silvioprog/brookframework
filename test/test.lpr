program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testbrookaction, testbrookapplication,
  testbrookconfigurator, testbrookhttpclient, testbrookhttputils,
  testbrookmiddleware, testbrookrouter, testbrooksession, testbrookutils,
  testbrooklogger;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

