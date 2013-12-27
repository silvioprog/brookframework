unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils, BrookHTTPConsts, BrookSQLdbBroker,
  sqlite3conn, sysutils;

const
  PUBLIC_HTML = 'C:\repository\git\brookframework\demos\db\blog\';

implementation

initialization
  BrookSettings.Configuration := 'C:\repository\git\brookframework\demos\db\db.cfg';
  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404File := PUBLIC_HTML + '404.html';
  BrookSettings.Page500File := PUBLIC_HTML + '500.html';
  BrookSettings.RootUrl := '/cgi-bin/cgi1.bf';

end.
