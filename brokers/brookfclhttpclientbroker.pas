(*
  Brook FCL HTTP Client Broker unit.

  Copyright (C) 2013 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookFCLHTTPClientBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookHTTPClient, BrookConsts, FPHTTPClient, Classes, SysUtils;

type
  TBrookFPHTTPClientDef = class(TBrookHTTPDef)
  private
    FHttp: TFPHTTPClient;
    FContents: TStrings;
    FDocument: TMemoryStream;
    FMethod: string;
    FUrl: string;
    class procedure InternalRequest(AHttp: TFPHTTPClient; AResponse: TStream;
      const AMethod, AUrl: string);
  protected
    function GetClient: TObject; override;
    function GetContents: TStrings; override;
    function GetCookies: TStrings; override;
    function GetDocument: TStream; override;
    function GetHeaders: TStrings; override;
    function GetContentType: string; override;
    procedure SetContentType(AValue: string); override;
    function GetStatusCode: Integer; override;
    function GetReasonPhrase: string; override;
    function GetMethod: string; override;
    function GetUrl: string; override;
    procedure SetMethod(AValue: string); override;
    procedure SetUrl(AValue: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetLibrary: string; override;
    class function Get(const AUrl: string; AResponse: TStream): Boolean; override;
    class function Post(const AUrl: string; AResponse: TStream): Boolean; override;
    class function Put(const AUrl: string; AResponse: TStream): Boolean; override;
    class function Delete(const AUrl: string; AResponse: TStream): Boolean; override;
    class function Options(const AUrl: string; AResponse: TStream): Boolean; override;
    class function Head(const AUrl: string; AHeaders: TStrings): Boolean; override;
    class function PostForm(const AUrl, AFormData: string; AResponse: TStream): Boolean; override;
    class function PostForm(const AUrl: string; AFormData, AResponse: TStream): Boolean; override;
    class function PostFile(const AUrl, AFieldName, AFileName: string;
      AResponse: TStream): Boolean; override;
    class function PostFile(const AUrl, AFieldName, AFileName: string;
      AFile, AResponse: TStream): Boolean; override;
    procedure AddHeader(const AName, AValue: string); override;
    function Request: Boolean; override;
  end;

implementation

constructor TBrookFPHTTPClientDef.Create;
begin
  FHttp := TFPHTTPClient.Create(nil);
  FContents := TStringList.Create;
  FDocument := TMemoryStream.Create;
  FHttp.AddHeader('User-Agent', 'Brook framework and FCL-Web.');
  FMethod := 'GET';
end;

destructor TBrookFPHTTPClientDef.Destroy;
begin
  FContents.Free;
  FDocument.Free;
  FHttp.Free;
  inherited Destroy;
end;

function TBrookFPHTTPClientDef.GetClient: TObject;
begin
  Result := FHttp;
end;

function TBrookFPHTTPClientDef.GetContents: TStrings;
begin
  FDocument.Seek(0, 0);
  FContents.LoadFromStream(FDocument);
  Result := FContents;
end;

function TBrookFPHTTPClientDef.GetCookies: TStrings;
begin
  Result := FHttp.Cookies;
end;

function TBrookFPHTTPClientDef.GetDocument: TStream;
begin
  Result := FDocument;
end;

function TBrookFPHTTPClientDef.GetHeaders: TStrings;
begin
  Result := FHttp.ResponseHeaders;
end;

function TBrookFPHTTPClientDef.GetContentType: string;
begin
  Result := FHttp.GetHeader('Content-Type');
end;

procedure TBrookFPHTTPClientDef.SetContentType(AValue: string);
begin
  FHttp.AddHeader('Content-Type', AValue);
end;

function TBrookFPHTTPClientDef.GetStatusCode: Integer;
begin
  Result := FHttp.ResponseStatusCode;
end;

function TBrookFPHTTPClientDef.GetReasonPhrase: string;
begin
  Result := FHttp.ResponseStatusText;
end;

function TBrookFPHTTPClientDef.GetMethod: string;
begin
  Result := FMethod;
end;

function TBrookFPHTTPClientDef.GetUrl: string;
begin
  Result := FUrl;
end;

procedure TBrookFPHTTPClientDef.SetMethod(AValue: string);
begin
  FMethod := AValue;
end;

procedure TBrookFPHTTPClientDef.SetUrl(AValue: string);
begin
  FUrl := AValue;
end;

class procedure TBrookFPHTTPClientDef.InternalRequest(AHttp: TFPHTTPClient;
  AResponse: TStream; const AMethod, AUrl: string);
begin
  if Assigned(AResponse) then
    AHttp.HTTPMethod(AMethod, AUrl, AResponse, [])
  else
  begin
    AResponse := TMemoryStream.Create;
    try
      AHttp.HTTPMethod(AMethod, AUrl, AResponse, []);
    finally
      FreeAndNil(AResponse);
    end;
  end;
end;

class function TBrookFPHTTPClientDef.GetLibrary: string;
begin
  Result := 'FCLWeb';
end;

class function TBrookFPHTTPClientDef.Get(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TFPHTTPClient;
begin
  VHttp := TFPHTTPClient.Create(nil);
  try
    VHttp.RequestHeaders.Add('Connection: Close');
    InternalRequest(VHttp, AResponse, 'GET', AUrl);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.Post(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TFPHTTPClient;
begin
  VHttp := TFPHTTPClient.Create(nil);
  try
    VHttp.RequestHeaders.Add('Connection: Close');
    InternalRequest(VHttp, AResponse, 'POST', AUrl);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.Put(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TFPHTTPClient;
begin
  VHttp := TFPHTTPClient.Create(nil);
  try
    VHttp.RequestHeaders.Add('Connection: Close');
    InternalRequest(VHttp, AResponse, 'PUT', AUrl);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.Delete(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TFPHTTPClient;
begin
  VHttp := TFPHTTPClient.Create(nil);
  try
    VHttp.RequestHeaders.Add('Connection: Close');
    InternalRequest(VHttp, AResponse, 'DELETE', AUrl);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.Options(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: TFPHTTPClient;
begin
  VHttp := TFPHTTPClient.Create(nil);
  try
    VHttp.RequestHeaders.Add('Connection: Close');
    InternalRequest(VHttp, AResponse, 'OPTIONS', AUrl);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.Head(const AUrl: string;
  AHeaders: TStrings): Boolean;
var
  VHttp: TFPHTTPClient;
begin
  VHttp := TFPHTTPClient.Create(nil);
  try
    VHttp.RequestHeaders.Add('Connection: Close');
    VHttp.HTTPMethod('HEAD', AUrl, nil, []);
    AHeaders.Assign(VHttp.ResponseHeaders);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.PostForm(const AUrl: string; AFormData,
  AResponse: TStream): Boolean;
var
  VHttp: TFPHTTPClient;
begin
  VHttp := TFPHTTPClient.Create(nil);
  try
    VHttp.RequestBody := AFormData;
    VHttp.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    VHttp.RequestHeaders.Add('Connection: Close');
    InternalRequest(VHttp, AResponse, 'POST', AUrl);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.PostForm(const AUrl, AFormData: string;
  AResponse: TStream): Boolean;
var
  VFormData: TStringStream;
begin
  VFormData := TStringStream.Create(AFormData);
  try
    Result := PostForm(AUrl, VFormData, AResponse);
  finally
    VFormData.Free;
    VFormData := nil;
  end;
end;

class function TBrookFPHTTPClientDef.PostFile(const AUrl, AFieldName,
  AFileName: string; AFile, AResponse: TStream): Boolean;
var
  S, VSep: string;
  VData: TMemoryStream;
  VHttp: TFPHTTPClient;
begin
  VData := TMemoryStream.Create;
  VHttp := TFPHTTPClient.Create(nil);
  try
    VSep := Format('%.8x_multipart_boundary', [Random($FFFFFF)]);
    S := '--' + VSep + CRLF;
    S := S + Format('Content-Disposition: form-data; name="%s"; filename="%s"' +
      CRLF, [AFieldName, ExtractFileName(AFileName)]);
    S := S + 'Content-Type: application/octet-string' + CRLF + CRLF;
    VData.Write(Pointer(S)^, Length(S));
    VData.CopyFrom(AFile, 0);
    S := CRLF + '--' + VSep + '--' + CRLF;
    VData.Write(Pointer(S)^, Length(S));
    VHttp.AddHeader('Content-Type', 'multipart/form-data; boundary=' + VSep);
    VHttp.RequestHeaders.Add('Connection: Close');
    VData.Seek(0, 0);
    VHttp.RequestBody := VData;
    VHttp.RequestHeaders.Add('Connection: Close');
    InternalRequest(VHttp, AResponse, 'POST', AUrl);
    Result := VHttp.ResponseStatusCode = 200;
  finally
    VData.Free;
    VHttp.RequestBody := nil;
    VHttp.Free;
  end;
end;

class function TBrookFPHTTPClientDef.PostFile(const AUrl, AFieldName,
  AFileName: string; AResponse: TStream): Boolean;
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := PostFile(AUrl, AFieldName, AFileName, VFile, AResponse);
  finally
    VFile.Free;
  end;
end;

procedure TBrookFPHTTPClientDef.AddHeader(const AName, AValue: string);
begin
  FHttp.AddHeader(AName, AValue);
end;

function TBrookFPHTTPClientDef.Request: Boolean;
begin
  try
    if FHttp.ResponseHeaders.Count > 0 then
      FHttp.RequestHeaders.AddStrings(FHttp.ResponseHeaders);
    if FDocument.Size > 0 then
    begin
      FHttp.RequestBody := TMemoryStream.Create;
      FHttp.RequestBody.CopyFrom(FDocument, 0);
      FHttp.RequestBody.Seek(0, 0);
      FDocument.Clear;
    end;
    FHttp.RequestHeaders.Add('Connection: Close');
    FHttp.HTTPMethod(FMethod, FUrl, FDocument, []);
    Result := FHttp.ResponseStatusCode = 200;
  finally
    FHttp.RequestBody.Free;
    FHttp.RequestBody := nil;
  end;
end;

initialization
  TBrookFPHTTPClientDef.Register;

end.

