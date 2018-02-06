{$MODE DELPHI}

Program testping;

uses
 pingsend, sysutils;

var
  ping:TPingSend;
begin
  ping:=TPingSend.Create;
  try
    ping.ping(ParamStr(1));
    Writeln (IntTostr(ping.pingtime));  
  finally
    ping.Free;
  end;
end.

