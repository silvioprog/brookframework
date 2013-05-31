unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLHTTPAppBroker, BrookUtils, BrookHTTPConsts, BrookSQLdbBroker,
  SQLite3Conn, BrookApplication;

const
  PUBLIC_HTML = '/media/Sources/lazarus-fpc/git/brookframework/demos/db/blog/html/';

implementation

initialization
  TBrookHTTPApplication(BrookApp.Instance).Port := 2015;
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404 := PUBLIC_HTML + '404.html';
  BrookSettings.Page500 := PUBLIC_HTML + '500.html';
  BrookSettings.RootUrl := 'http://localhost:2015';
end.
