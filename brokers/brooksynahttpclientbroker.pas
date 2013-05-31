(*
  Brook Synapse HTTP Client Broker unit.

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

unit BrookSynaHTTPClientBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookHTTPClient, BrookConsts, HTTPSend, Classes, SysUtils;

type
  TBrookSynaHTTPSendDef = class(TBrookHTTPDef)
  private
    FHttp: THTTPSend;
    FContents: TStrings;
    FMethod: string;
    FUrl: string;
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

constructor TBrookSynaHTTPSendDef.Create;
begin
  FHttp := THTTPSend.Create;
  FContents := TStringList.Create;
  FHttp.UserAgent := 'Brook and Synapse framework.';
  FMethod := 'GET';
end;

destructor TBrookSynaHTTPSendDef.Destroy;
begin
  FContents.Free;
  FHttp.Free;
  inherited Destroy;
end;

function TBrookSynaHTTPSendDef.GetClient: TObject;
begin
  Result := FHttp;
end;

function TBrookSynaHTTPSendDef.GetContents: TStrings;
begin
  FHttp.Document.Seek(0, 0);
  FContents.LoadFromStream(FHttp.Document);
  Result := FContents;
end;

function TBrookSynaHTTPSendDef.GetCookies: TStrings;
begin
  Result := FHttp.Cookies;
end;

function TBrookSynaHTTPSendDef.GetDocument: TStream;
begin
  Result := FHttp.Document;
end;

function TBrookSynaHTTPSendDef.GetHeaders: TStrings;
begin
  Result := FHttp.Headers;
end;

function TBrookSynaHTTPSendDef.GetContentType: string;
begin
  Result := FHttp.MimeType;
end;

procedure TBrookSynaHTTPSendDef.SetContentType(AValue: string);
begin
  FHttp.MimeType := AValue;
end;

function TBrookSynaHTTPSendDef.GetStatusCode: Integer;
begin
  Result := FHttp.ResultCode;
end;

function TBrookSynaHTTPSendDef.GetReasonPhrase: string;
begin
  Result := FHttp.ResultString;
end;

function TBrookSynaHTTPSendDef.GetMethod: string;
begin
  Result := FMethod;
end;

function TBrookSynaHTTPSendDef.GetUrl: string;
begin
  Result := FUrl;
end;

procedure TBrookSynaHTTPSendDef.SetMethod(AValue: string);
begin
  FMethod := AValue;
end;

procedure TBrookSynaHTTPSendDef.SetUrl(AValue: string);
begin
  FUrl := AValue;
end;

class function TBrookSynaHTTPSendDef.GetLibrary: string;
begin
  Result := 'Synapse';
end;

class function TBrookSynaHTTPSendDef.Get(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VHttp.HTTPMethod('GET', AUrl);
    if Assigned(AResponse) then
      AResponse.CopyFrom(VHttp.Document, 0);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.Post(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VHttp.HTTPMethod('POST', AUrl);
    if Assigned(AResponse) then
      AResponse.CopyFrom(VHttp.Document, 0);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.Put(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VHttp.HTTPMethod('PUT', AUrl);
    if Assigned(AResponse) then
      AResponse.CopyFrom(VHttp.Document, 0);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.Delete(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VHttp.HTTPMethod('DELETE', AUrl);
    if Assigned(AResponse) then
      AResponse.CopyFrom(VHttp.Document, 0);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.Options(const AUrl: string;
  AResponse: TStream): Boolean;
var
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VHttp.HTTPMethod('OPTIONS', AUrl);
    if Assigned(AResponse) then
      AResponse.CopyFrom(VHttp.Document, 0);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.Head(const AUrl: string;
  AHeaders: TStrings): Boolean;
var
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VHttp.HTTPMethod('HEAD', AUrl);
    if Assigned(AHeaders) then
      AHeaders.Assign(VHttp.Headers);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.PostForm(const AUrl, AFormData: string;
  AResponse: TStream): Boolean;
var
  VFormData: TStringStream;
begin
  VFormData := TStringStream.Create(AFormData);
  try
    Result := PostForm(AUrl, VFormData, AResponse);
  finally
    VFormData.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.PostForm(const AUrl: string; AFormData,
  AResponse: TStream): Boolean;
var
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VHttp.Document.CopyFrom(AFormData, 0);
    VHttp.MimeType := 'application/x-www-form-urlencoded';
    VHttp.HTTPMethod('POST', AUrl);
    if Assigned(AResponse) then
      AResponse.CopyFrom(VHttp.Document, 0);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

class function TBrookSynaHTTPSendDef.PostFile(const AUrl, AFieldName,
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

class function TBrookSynaHTTPSendDef.PostFile(const AUrl, AFieldName,
  AFileName: string; AFile, AResponse: TStream): Boolean;
var
  S, VSep: string;
  VHttp: THTTPSend;
begin
  VHttp := THTTPSend.Create;
  try
    VSep := Format('%.8x_multipart_boundary', [Random($FFFFFF)]);
    S := '--' + VSep + CRLF;
    S := S + Format('Content-Disposition: form-data; name="%s"; filename="%s"' +
      CRLF, [AFieldName, ExtractFileName(AFileName)]);
    S := S + 'Content-Type: application/octet-string' + CRLF + CRLF;
    VHttp.Document.Write(Pointer(S)^, Length(S));
    VHttp.Document.CopyFrom(AFile, 0);
    S := CRLF + '--' + VSep + '--' + CRLF;
    VHttp.Document.Write(Pointer(S)^, Length(S));
    VHttp.MimeType := 'multipart/form-data; boundary=' + VSep;
    VHttp.HTTPMethod('POST', AUrl);
    if Assigned(AResponse) then
      AResponse.CopyFrom(VHttp.Document, 0);
    Result := VHttp.ResultCode = 200;
  finally
    VHttp.Free;
  end;
end;

procedure TBrookSynaHTTPSendDef.AddHeader(const AName, AValue: string);
begin
  FHttp.Headers.Values[AName] := AValue;
end;

function TBrookSynaHTTPSendDef.Request: Boolean;
begin
  Result := FHttp.HTTPMethod(FMethod, FUrl);
end;

initialization
  TBrookSynaHTTPSendDef.Register;

end.

