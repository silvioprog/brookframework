unit dbutils;

{$mode objfpc}{$H+}

interface

uses
  dOpf, dSQLdbBroker, PQConnection;

type
  Tcon = TdSQLdbConnector;

  Tqry = TdSQLdbQuery;

var
  con: TdSQLdbConnector;

implementation

initialization
  con := TdSQLdbConnector.Create(nil);
  con.Logger.Active := True;
  con.Logger.Filter := [ltSQL];
  con.Logger.FileName := 'OUTPUT.LOG';
  con.Driver := 'postgresql';
  con.Host := '127.0.0.1';
  con.Database := 'postgres';
  con.User := 'postgres';
  con.Password := 'postgres';
  con.Connect;

finalization
  con.Free;

end.

