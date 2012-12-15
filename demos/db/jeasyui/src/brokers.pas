unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookSQLdbBroker, BrookUtils, BrookHTTPConsts,
  SQLite3Conn;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.ContentType := BROOK_HTTP_CONTENT_TYPE_APP_JSON;
  BrookSettings.Page404 := '{ "msg": "404 - Page not found." }';
  BrookSettings.Page500 := '{ "msg": "500 - Internal server error: %s" }';

end.
