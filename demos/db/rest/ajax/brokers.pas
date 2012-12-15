unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils, BrookHTTPConsts, BrookSQLdbBroker, SQLite3Conn;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.ContentType := BROOK_HTTP_CONTENT_TYPE_APP_JSON;
  BrookSettings.Page404 := '{ "error": "Page not found." }';
  BrookSettings.Page500 := '{ "error": "%s" }';

end.
