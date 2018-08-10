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

unit BrookHTTPRequest;

{$I Brook.inc}

interface

uses
  SysUtils,
  Marshalling,
  libsagui,
  BrookUtils,
  BrookHandledClasses,
  BrookString,
  BrookStringMap,
  BrookHTTPUploads;

type
  TBrookHTTPRequest = class(TBrookHandledPersistent)
  private
    FUploads: TBrookHTTPUploads;
    FHeaders: TBrookStringMap;
    FCookies: TBrookStringMap;
    FParams: TBrookStringMap;
    FFields: TBrookStringMap;
    FPayload: TBrookString;
    FVersion: string;
    FMethod: string;
    FPath: string;
    FUploading: Boolean;
    FTLSSession: Pointer;
    FHandle: Psg_httpreq;
    function GetPaths: TArray<string>;
  protected
    function CreateUploads(AHandle: Pointer): TBrookHTTPUploads; virtual;
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateCookies(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateParams(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateFields(AHandle: Pointer): TBrookStringMap; virtual;
    function CreatePayload(AHandle: Pointer): TBrookString; virtual;
    function GetHandle: Pointer; override;
    function GetUserData: Pointer; virtual;
    procedure SetUserData(AValue: Pointer); virtual;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    function IsPost: Boolean; virtual;
    property Headers: TBrookStringMap read FHeaders;
    property Cookies: TBrookStringMap read FCookies;
    property Params: TBrookStringMap read FParams;
    property Fields: TBrookStringMap read FFields;
    property Payload: TBrookString read FPayload;
    property Version: string read FVersion;
    property Method: string read FMethod;
    property Path: string read FPath;
    property Paths: TArray<string> read GetPaths;
    property Uploading: Boolean read FUploading;
    property Uploads: TBrookHTTPUploads read FUploads;
    property TLSSession: Pointer read FTLSSession;
    property UserData: Pointer read GetUserData write SetUserData;
  end;

implementation

constructor TBrookHTTPRequest.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FUploads := CreateUploads(sg_httpreq_uploads(AHandle));
  FHeaders := CreateHeaders(sg_httpreq_headers(FHandle));
  FCookies := CreateCookies(sg_httpreq_cookies(FHandle));
  FParams := CreateParams(sg_httpreq_params(FHandle));
  FFields := CreateFields(sg_httpreq_fields(FHandle));
  FPayload := CreatePayload(sg_httpreq_payload(FHandle));
  FVersion := TMarshal.ToString(sg_httpreq_version(FHandle));
  FMethod := TMarshal.ToString(sg_httpreq_method(FHandle));
  FPath := TMarshal.ToString(sg_httpreq_path(FHandle));
  FUploading := sg_httpreq_uploading(FHandle);
  if Assigned(sg_httpreq_tls_session) then
    FTLSSession := sg_httpreq_tls_session(FHandle);
end;

destructor TBrookHTTPRequest.Destroy;
begin
  FUploads.Free;
  FHeaders.Free;
  FCookies.Free;
  FParams.Free;
  FFields.Free;
  FPayload.Free;
  inherited Destroy;
end;

function TBrookHTTPRequest.CreateUploads(AHandle: Pointer): TBrookHTTPUploads;
begin
  Result := TBrookHTTPUploads.Create(AHandle);
end;

function TBrookHTTPRequest.CreateHeaders(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreateCookies(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreateParams(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreateFields(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPRequest.CreatePayload(AHandle: Pointer): TBrookString;
begin
  Result := TBrookString.Create(AHandle);
end;

function TBrookHTTPRequest.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookHTTPRequest.GetPaths: TArray<string>;
begin
  Result := Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);
end;

function TBrookHTTPRequest.IsPost: Boolean;
begin
  Result := BrookIsPost(FMethod);
end;

procedure TBrookHTTPRequest.SetUserData(AValue: Pointer);
begin
  SgCheckLibrary;
  SgCheckLastError(sg_httpreq_set_user_data(FHandle, AValue));
end;

function TBrookHTTPRequest.GetUserData: Pointer;
begin
  SgCheckLibrary;
  Result := sg_httpreq_user_data(FHandle);
end;

end.
