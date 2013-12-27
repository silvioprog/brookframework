unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookSQLdbBroker, SQLite3Conn, BrookUtils;

implementation

initialization
  BrookSettings.Configuration := 'C:\repository\git\brookframework\demos\db\db.cfg';
  BrookSettings.Page404 := '{ "error": "Not found." }';
  BrookSettings.Page500 := '{ "error": "@error" }';

end.
