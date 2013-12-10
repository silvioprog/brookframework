unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils, BrookSQLdbBroker, SQLite3Conn;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';

end.
