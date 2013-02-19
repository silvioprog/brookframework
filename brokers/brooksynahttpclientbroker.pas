(*
  Brook Synapse HTTP Client Broker unit.

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

unit BrookSynaHTTPClientBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookHTTPClient, HTTPSend, Classes;

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

