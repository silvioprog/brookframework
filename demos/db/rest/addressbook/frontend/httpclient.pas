unit HttpClient;

{$mode objfpc}{$H+}

interface

uses
  FPHTTPClient, FPJSON, JSONParser, SysUtils, Classes;

type
  THttpResult = record
    Code: Integer;
    Text: string;
  end;

  TRequestMethod = (rmGet, rmHead, rmOptions, rmPost, rmPut, rmDelete);

function HttpRequest(const AUrl: string; AResponse: TJSONData;
  const AMethod: TRequestMethod = rmGet): THttpResult;
function HttpRequest(AData: TJSONData; const AUrl: string;
  const AMethod: TRequestMethod = rmPost;
  const AContentType: string = 'application/json'): THttpResult;

implementation

function HttpRequest(const AUrl: string; AResponse: TJSONData;
  const AMethod: TRequestMethod): THttpResult;
var
  VMethod: string;
  VParser: TJSONParser;
  VHttp: TFPHTTPClient;
  VData: TMemoryStream;
begin
  VHttp := TFPHTTPClient.Create(nil);
  VData := TMemoryStream.Create;
  try
    case AMethod of
      rmGet: VMethod := 'GET';
      rmHead: VMethod := 'HEAD';
      rmOptions: VMethod := 'OPTIONS';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    VHttp.HTTPMethod(VMethod, AUrl, VData, []);
    Result.Code := VHttp.ResponseStatusCode;
    Result.Text := VHttp.ResponseStatusText;
    if Assigned(AResponse) and (VData.Size > 0) then
    begin
      VData.Position := 0;
      VParser := TJSONParser.Create(VData);
      try
        FreeAndNil(AResponse);
        AResponse := VParser.Parse;
      finally
        VParser.Free;
      end;
    end;
  finally
    VData.Free;
    VHttp.Free;
  end;
end;

function HttpRequest(AData: TJSONData; const AUrl: string;
  const AMethod: TRequestMethod; const AContentType: string): THttpResult;
var
  VMethod: string;
  VHttp: TFPHTTPClient;
  VParser: TJSONParser;
  VData: TMemoryStream;
  VJSON: TJSONStringType;
begin
  VHttp := TFPHTTPClient.Create(nil);
  VData := TMemoryStream.Create;
  try
    case AMethod of
      rmPost: VMethod := 'POST';
      rmPut: VMethod := 'PUT';
      rmDelete: VMethod := 'DELETE';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    if Assigned(AData) then
    begin
      VHttp.RequestBody := TMemoryStream.Create;
      VJSON := AData.AsJSON;
      VHttp.RequestBody.Write(Pointer(VJSON)^, Length(VJSON));
      VHttp.RequestBody.Position := 0;
    end;
    VHttp.AddHeader('Content-Type', AContentType);
    VHttp.HTTPMethod(VMethod, AUrl, VData, []);
    Result.Code := VHttp.ResponseStatusCode;
    Result.Text := VHttp.ResponseStatusText;
    if VData.Size > 0 then
    begin
      VData.Position := 0;
      VParser := TJSONParser.Create(VData);
      try
        FreeAndNil(AData);
        AData := VParser.Parse;
      finally
        VParser.Free;
      end;
    end;
  finally
    VHttp.RequestBody.Free;
    VHttp.RequestBody := nil;
    VData.Free;
    VHttp.Free;
  end;
end;

end.
