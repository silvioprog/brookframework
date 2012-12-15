// Please see: http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.4

unit test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookHTTPConsts, HTTPDefs, SysUtils;

type
  TMyAction = class(TBrookAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

implementation

procedure TMyAction.Request(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.SetCustomHeader(BROOK_HTTP_HEADER_CACHE_CONTROL,
    BROOK_HTTP_CACHE_CONTROL_PUBLIC + ', ' +
    BROOK_HTTP_CACHE_CONTROL_MAX_AGE + '10');
  Write('<!DOCTYPE HTML>');
  Write('<html lang="en-US">');
  Write('<head>');
  Write('	<meta charset="UTF-8">');
  Write('	<title>CacheControl</title>');
  Write('</head>');
  Write('<body>');
  Write(FormatDateTime('hh:nn:ss', Now));
  Write('</body>');
  Write('</html>');
end;

initialization
  TMyAction.Register('*');

end.
