program console;

{$mode objfpc}{$H+}

uses
  heaptrc, Classes, consoletestrunner, alltestsrt;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  end;

begin
  with TMyTestRunner.Create(nil) do
  try
    Application.Initialize;
    Application.Run;
  finally
    Application.Free;
  end;
end.
