unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookGhSQLdbBroker, SQLite3Conn, BrookUtils;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';

end.
