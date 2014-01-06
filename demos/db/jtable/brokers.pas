unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLCGIBroker, BrookException, BrookUtils, BrookSQLdbBroker,
  BrookHTTPConsts, PQConnection, HTTPDefs, fpjson, sysutils;

implementation

procedure OnError(AResponse: TResponse; AException: Exception; var AHandled: Boolean);
begin
  AHandled := True;
  AResponse.Code := BROOK_HTTP_STATUS_CODE_OK;
  AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_OK;
  if AException is EBrookHTTP404 then
    AResponse.Content := '{ "Result": "ERROR", "Message": "Not found." }'
  else
    AResponse.Content := '{ "Result": "ERROR", "Message": "' +
      StringToJSONString(AException.Message) + '" }';
end;

initialization
  BrookSettings.OnError := @OnError;
  BrookSettings.Configuration := 'db.cfg';
  BrookSettings.ContentType := BROOK_HTTP_CONTENT_TYPE_APP_JSON;

end.
