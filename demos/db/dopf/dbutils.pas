unit dbutils;

{$mode objfpc}{$H+}

interface

uses
  dOPF, dSQLdbBroker, pqconnection;

type
  Tcon = specialize TdConnection<TdSQLdbConnectionBroker, TdLogger>;

  Tqry = specialize TdQuery<TdSQLdbQueryBroker, Tcon>;

var
  con: Tcon;

implementation

initialization
  con := Tcon.Create(nil);
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

