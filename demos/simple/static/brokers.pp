unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLHTTPAppBroker, BrookHTTPConsts, BrookUtils, Classes, SysUtils;

implementation

uses
  BrookStaticFileBroker, BrookApplication;

var
  PublicHTMLDir: String;
initialization
  PublicHTMLDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404 := PublicHTMLDir + '404.html';
  BrookSettings.Page500 := PublicHTMLDir + '500.html';

  RegisterDirectory('/css/',PublicHTMLDir + 'css');
  RegisterDirectory('/js/',PublicHTMLDir + 'js');
  RegisterDirectory('/img/',PublicHTMLDir + 'img');

  with BrookApp.Instance as TBrookHTTPApplication do begin
    Port := 8000;
  end;

end.
