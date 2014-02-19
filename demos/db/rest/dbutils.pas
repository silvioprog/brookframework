unit dbutils;

{$mode objfpc}{$H+}

interface

uses
  sysutils, pqconnection;

function Connection: TPQConnection;

implementation

var
  _Connection: TPQConnection;

function Connection: TPQConnection;
begin
  if not Assigned(_Connection) then
  begin
    _Connection := TPQConnection.Create(nil);
    _Connection.HostName := '127.0.0.1';
    _Connection.DatabaseName := 'postgres';
    _Connection.UserName := 'postgres';
    _Connection.Password := 'postgres';
  end;
  Result := _Connection;
end;

finalization
  FreeAndNil(_Connection);

end.

