unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookHTTPConsts, BrookSQLdbBroker, SQLite3Conn, BrookUtils;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.ContentType := BROOK_HTTP_CONTENT_TYPE_APP_RDF_XML;
  BrookSettings.Page404 :=
    '<html><head><title>Page not found</title></head><body>' +
    '<h1>404 - Page not found</h1></body></html>';
  BrookSettings.Page500 :=
    '<html><head><title>Internal server error</title></head><body>' +
    '<h1>500 - Internal server error</h1>' +
    '<p style="color: red;" >Error: %s</p></body></html>';

end.
