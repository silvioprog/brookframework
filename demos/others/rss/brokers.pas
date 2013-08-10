unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookHTTPConsts, BrookSQLdbBroker, SQLite3Conn, BrookUtils;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.ContentType := BROOK_HTTP_CONTENT_TYPE_APP_RDF_XML;

end.
