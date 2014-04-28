program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testbrookaction;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

