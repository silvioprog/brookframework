(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– an ideal Pascal microframework to develop cross-platform HTTP servers.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
 *)

{ Use this unit ONLY if you have a legacy application in Brook 3 and wants to
  run it in Brook 4. }

unit BrookTardigradeBroker;

{$MODE DELPHI}

interface

uses
  SysUtils,
  Classes,
  HTTPDefs,
  HTTPProtocol,
  CustApp,
  BrookLibraryLoader,
  BrookUtility,
  BrookStringMap,
  BrookHTTPUploads,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPServer,
  BrookApplication,
  BrookRouter;

type

  { THTTPRequest }

  THTTPRequest = class(TRequest)
  private
    FHandle: TBrookHTTPRequest;
  protected
    procedure FetchHeaders(AMap: TBrookStringMap; ADest: TStrings); virtual;
    procedure FetchFields(AMap: TBrookStringMap; ADest: TStrings); virtual;
    procedure FetchParams(AMap: TBrookStringMap; ADest: TStrings); virtual;
    procedure FetchCookies(AMap: TBrookStringMap; ADest: TStrings); virtual;
    procedure FetchUploads(AUploads: TBrookHTTPUploads); virtual;
    property Handle: TBrookHTTPRequest read FHandle;
  public
    constructor Create(AHandle: TBrookHTTPRequest); reintroduce; virtual;
  end;

  { THTTPResponse }

  THTTPResponse = class(TResponse)
  private
    FHandle: TBrookHTTPResponse;
  protected
    procedure DoSendHeaders(AHeaders: TStrings); override;
    procedure DoSendContent; override;
    property Handle: TBrookHTTPResponse read FHandle;
  public
    constructor Create(AHandle: TBrookHTTPResponse;
      ARequest: TRequest); reintroduce; virtual;
  end;

  { THTTPServer }

  THTTPServer = class(TBrookHTTPServer)
  protected
    function CreateHTTPRequest(AHandle: TBrookHTTPRequest): THTTPRequest; virtual;
    function CreateHTTPResponse(AHandle: TBrookHTTPResponse;
      ARequest: THTTPRequest): THTTPResponse; virtual;
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  public
    procedure Initialize; virtual;
  end;

  { TApplication }

  TApplication = class(TCustomApplication, IBrookApplication)
  private
    FLibraryLoader: TBrookLibraryLoader;
    FServer: THTTPServer;
    function GetTerminated: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Run; virtual;
    function Instance: TObject;
    property LibraryLoader: TBrookLibraryLoader read FLibraryLoader;
    property Server: THTTPServer read FServer;
  end;

var
  Application: TApplication = nil;

implementation

{ THTTPRequest }

constructor THTTPRequest.Create(AHandle: TBrookHTTPRequest);
begin
  inherited Create;
  FHandle := AHandle;
  FetchHeaders(AHandle.Headers, CustomHeaders);
  FetchFields(AHandle.Fields, ContentFields);
  FetchParams(AHandle.Params, QueryFields);
  FetchCookies(AHandle.Cookies, CookieFields);
  if AHandle.IsUploading then
    FetchUploads(AHandle.Uploads);
  HttpVersion := FHandle.Version;
  Method := FHandle.Method;
  PathInfo := FHandle.Path;
  UserAgent := FHandle.UserAgent;
  Referer := FHandle.Referer;
end;

procedure THTTPRequest.FetchHeaders(AMap: TBrookStringMap; ADest: TStrings);
var
  P: TBrookStringPair;
  H: THeader;
begin
  for P in AMap do
  begin
    H := HeaderType(P.Name);
    if H <> hhUnknown then
      SetHeader(H, P.Value)
    else
      ADest.AddPair(P.Name, P.Value);
  end;
end;

procedure THTTPRequest.FetchFields(AMap: TBrookStringMap; ADest: TStrings);
var
  P: TBrookStringPair;
begin
  for P in AMap do
    ADest.AddPair(P.Name, P.Value);
end;

procedure THTTPRequest.FetchParams(AMap: TBrookStringMap; ADest: TStrings);
var
  P: TBrookStringPair;
begin
  for P in AMap do
    ADest.AddPair(P.Name, P.Value);
end;

procedure THTTPRequest.FetchCookies(AMap: TBrookStringMap; ADest: TStrings);
var
  P: TBrookStringPair;
begin
  for P in AMap do
    ADest.AddPair(P.Name, P.Value);
end;

procedure THTTPRequest.FetchUploads(AUploads: TBrookHTTPUploads);
var
  U: TBrookHTTPUpload;
  F: TUploadedFile;
  E: string;
begin
  for U in AUploads do
  begin
    if not U.Save(True, E) then
      raise EFCreateError.Create(E);
    F := TUploadedFile.Create(Files);
    F.FieldName := U.Field;
    F.FileName := U.Name;
    F.ContentType := U.Mime;
    F.LocalFileName := ConcatPaths([U.Directory, U.Name]);
    F.Size := U.Size;
  end;
end;

{ THTTPResponse }

constructor THTTPResponse.Create(AHandle: TBrookHTTPResponse; ARequest: TRequest);
begin
  inherited Create(ARequest);
  FHandle := AHandle;
  Contents.LineBreak := '';
end;

procedure THTTPResponse.DoSendHeaders(AHeaders: TStrings);
var
  NVS: Char;
  I: Integer;
  N, V: string;
begin
  NVS := AHeaders.NameValueSeparator;
  try
    AHeaders.NameValueSeparator := ':';
    FHandle.Headers.Clear;
    for I := 0 to Pred(AHeaders.Count) do
    begin
      AHeaders.GetNameValue(I, N, V);
      FHandle.Headers.Add(N, V.Trim);
    end;
    FHandle.Headers.Remove('Content-Length');
  finally
    AHeaders.NameValueSeparator := NVS;
  end;
end;

procedure THTTPResponse.DoSendContent;
begin
  FHandle.Send(Contents.Text, ContentType, Code);
end;

{ THTTPServer }

function THTTPServer.CreateHTTPRequest(AHandle: TBrookHTTPRequest): THTTPRequest;
begin
  Result := THTTPRequest.Create(AHandle);
end;

function THTTPServer.CreateHTTPResponse(AHandle: TBrookHTTPResponse;
  ARequest: THTTPRequest): THTTPResponse;
begin
  Result := THTTPResponse.Create(AHandle, ARequest);
end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
var
  VReq: THTTPRequest;
  VRes: THTTPResponse;
begin
  VReq := CreateHTTPRequest(ARequest);
  VRes := CreateHTTPResponse(AResponse, VReq);
  try
    TBrookRouter.Service.Route(VReq, VRes);
    VRes.SendContent;
  finally
    VReq.Free;
    VRes.Free;
  end;
end;

procedure THTTPServer.Initialize;
begin
  if TRequest.DefaultRequestUploadDir = '' then
    TRequest.DefaultRequestUploadDir := Sagui.TmpDir;
  UploadsDir := TRequest.DefaultRequestUploadDir;
end;

{ TApplication }

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLibraryLoader := TBrookLibraryLoader.Create(nil);
  FServer := THTTPServer.Create(nil);
end;

destructor TApplication.Destroy;
begin
  FServer.Free;
  FLibraryLoader.Free;
  inherited Destroy;
end;

procedure TApplication.Initialize;
begin
  inherited Initialize;
  FLibraryLoader.Open;
  FServer.Initialize;
end;

procedure TApplication.Run;
begin
  FServer.Open;
  inherited Run;
end;

function TApplication.GetTerminated: Boolean;
begin
  Result := Terminated;
end;

function TApplication.Instance: TObject;
begin
  Result := FServer;
end;

initialization
  Application := TApplication.Create(nil);
  BrookRegisterApp(Application);

finalization
  BrookUnregisterApp;
  FreeAndNil(Application);

end.
