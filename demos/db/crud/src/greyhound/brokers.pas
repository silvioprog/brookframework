unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookGhSQLdbBroker, SQLite3Conn, BrookUtils;

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.Page404 :=
    '<html><head><title>Page not found</title></head><body>' +
    '<h1>404 - Page not found</h1></body></html>';
  BrookSettings.Page500 :=
    '<html><head><title>Internal server error</title></head><body>' +
    '<h1>500 - Internal server error</h1>' +
    '<p style="color: red;" >Error: @error</p></body></html>';

end.
