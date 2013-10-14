unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookUtils, BrookHTTPConsts, BrookSQLdbBroker, SQLite3Conn,
  BrookApplication;

const
  PUBLIC_HTML ={$IFDEF MSWINDOWS}'C:\websrv\htdocs\'{$ENDIF}'/var/www/'{$ELSE};

implementation

initialization
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404File := PUBLIC_HTML + '404.html';
  BrookSettings.Page500File := PUBLIC_HTML + '500.html';
  BrookSettings.RootUrl := 'http://localhost/cgi-bin/blog';

end.
