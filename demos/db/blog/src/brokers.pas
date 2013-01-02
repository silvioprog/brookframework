unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils, BrookHTTPConsts, BrookSQLdbBroker,
  SQLite3Conn;

const
  PUBLIC_HTML = '/your/public/html/directory/';

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404 := PUBLIC_HTML + '404.html';
  BrookSettings.Page500 := PUBLIC_HTML + '500.html';

end.
