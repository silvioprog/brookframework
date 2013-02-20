program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testbrookaction, testbrookapplication,
  testbrookdatabase, testbrookquery, testbrooktable, testbrookdbaction,
  testbrookrouter, testbrookconfigurator, testbrookdbutils, testbrookhttputils,
  testbrookutils, testbrooksession, testbrookrestactions, testbrookhttpclient;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

