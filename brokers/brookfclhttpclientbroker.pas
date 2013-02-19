(*
  Brook FCL HTTP Client Broker unit.

  Copyright (C) 2012 Silvio Clecio.

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
  BrookHTTPClient, FPHTTPClient, Classes;

type
  TBrookFPHTTPClientDef = class(TBrookHTTPDef)
  private
    FHttp: TFPHTTPClient;
    FContents: TStrings;
    FDocument: TMemoryStream;
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
  FHttp.Cookies.Free;
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

class function TBrookFPHTTPClientDef.GetLibrary: string;
begin
  Result := 'FCLWeb';
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

