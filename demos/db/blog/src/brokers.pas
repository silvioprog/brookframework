unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookApplication, BrookFCLFCGIBroker, BrookUtils, BrookHTTPConsts, BrookSQLdbBroker,
  SQLite3Conn;

const
  publichtml = '/media/Sources/lazarus-fpc/git/brookframework/demos/db/blog/html/';

implementation

initialization
  TBrookFCGIApplication(BrookApp.Instance).Port := 2000;
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404 := publichtml + '404.html';
  BrookSettings.Page500 := publichtml + '500.html';

end.
