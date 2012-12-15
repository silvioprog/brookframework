unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils, BrookHTTPConsts, BrookSQLdbBroker,
  SQLite3Conn;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404 := '404.html';
  BrookSettings.Page500 := '500.html';

end.
