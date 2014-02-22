unit dbutils;

{$mode objfpc}{$H+}

interface

uses
  dOPF, dSQLdbBroker, pqconnection, sysutils;

var
  Connection: TdConnection;

implementation

initialization
  Connection := TdConnection.Create(nil, TdSQLdbBroker);
  Connection.Logger.Active := True;
  Connection.Logger.Filter := [ltSQL];
  Connection.Logger.FileName := 'OUTPUT.LOG';
  Connection.Driver := 'postgresql';
  Connection.Host := '127.0.0.1';
  Connection.Database := 'postgres';
  Connection.User := 'postgres';
  Connection.Password := 'postgres';
  Connection.Connect;

finalization
  FreeAndNil(Connection);

end.

