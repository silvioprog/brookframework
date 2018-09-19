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

unit BrookHTTPServer;

{$I Brook.inc}

interface

uses
  SysUtils,
  Classes,
  Marshalling,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui,
  BrookUtils,
  BrookHandledClasses,
  BrookHTTPExtra,
  BrookHTTPAuthentication,
  BrookHTTPRequest,
  BrookHTTPResponse;

resourcestring
  SBrookActiveServer = 'Active server.';
  SBrookCannotCreateServerHandle = 'Cannot create server handle.';
  SBrookTLSNotAvailable = 'TLS is not available.';
  SBrookEmptyPrivateKey = 'Private key cannot be empty.';
  SBrookEmptyCertificate = 'Certificate cannot be empty.';

type
  EBrookHTTPServerSecurity = class(Exception);

  TBrookCustomHTTPServerSecurity = class(TPersistent)
  private
    FActive: Boolean;
    FPrivateKey: string;
    FPrivatePassword: string;
    FCertificate: string;
    FTrust: string;
    FDHParams: string;
    function IsActive: Boolean;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Clear; virtual;
    procedure Validate; inline;
  public
    property Active: Boolean read FActive write FActive stored IsActive;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    property PrivatePassword: string read FPrivatePassword
      write FPrivatePassword;
    property Certificate: string read FCertificate write FCertificate;
    property Trust: string read FTrust write FTrust;
    property DHParams: string read FDHParams write FDHParams;
  end;

  TBrookHTTPServerSecurity = class(TBrookCustomHTTPServerSecurity)
  published
    property Active;
    property PrivateKey;
    property PrivatePassword;
    property Certificate;
    property Trust;
    property DHParams;
  end;

  TBrookHTTPAuthenticateEvent = function(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse): Boolean of object;

  TBrookHTTPAuthenticateErrorEvent = procedure(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse; AException: Exception) of object;

  TBrookHTTPRequestEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRequestErrorEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
    AException: Exception) of object;

  EBrookHTTPServer = class(Exception);

  TBrookCustomHTTPServer = class(TBrookHandledComponent)
  private
    FHandle: Psg_httpsrv;
    FAuthenticated: Boolean;
    FConnectionLimit: Cardinal;
    FConnectionTimeout: Cardinal;
    FNoFavicon: Boolean;
    FPayloadLimit: NativeUInt;
    FUploadsLimit: UInt64;
    FActive: Boolean;
    FPort: UInt16;
    FPostBufferSize: NativeUInt;
    FThreaded: Boolean;
    FStreamedActive: Boolean;
    FStreamedAuthenticated: Boolean;
    FThreadPoolSize: Cardinal;
    FUploadsDir: string;
    FSecurity: TBrookCustomHTTPServerSecurity;
    FOnAuthenticate: TBrookHTTPAuthenticateEvent;
    FOnAuthenticateError: TBrookHTTPAuthenticateErrorEvent;
    FOnRequest: TBrookHTTPRequestEvent;
    FOnRequestError: TBrookHTTPRequestErrorEvent;
    FOnError: TBrookErrorEvent;
    function GetConnectionLimit: Cardinal;
    function GetConnectionTimeout: Cardinal;
    function GetPayloadLimit: NativeUInt;
    function GetUploadsLimit: UInt64;
    function GetPort: UInt16;
    function GetPostBufferSize: NativeUInt;
    function GetThreaded: Boolean;
    function GetThreadPoolSize: Cardinal;
    function GetUploadsDir: string;
    function IsActive: Boolean;
    function IsAuthenticated: Boolean;
    function IsConnectionLimit: Boolean;
    function IsConnectionTimeout: Boolean;
    function IsNoFavicon: Boolean;
    function IsPayloadLimit: Boolean;
    function IsUploadsLimit: Boolean;
    function IsPort: Boolean;
    function IsPostBufferSize: Boolean;
    function IsThreaded: Boolean;
    function IsThreadPoolSize: Boolean;
    function IsUploadsDir: Boolean;
    procedure SetAuthenticated(AValue: Boolean);
    procedure SetConnectionLimit(AValue: Cardinal);
    procedure SetConnectionTimeout(AValue: Cardinal);
    procedure SetPayloadLimit(AValue: NativeUInt);
    procedure SetSecurity(AValue: TBrookCustomHTTPServerSecurity);
    procedure SetUploadsLimit(AValue: UInt64);
    procedure SetPort(AValue: UInt16);
    procedure SetPostBufferSize(AValue: NativeUInt);
    procedure SetThreaded(AValue: Boolean);
    procedure SetThreadPoolSize(AValue: Cardinal);
    procedure SetUploadsDir(const AValue: string);
    procedure InternalCreateServerHandle; inline;
    procedure InternalFreeServerHandle; inline;
    procedure InternalCheckServerOption(Aret: cint); inline;
  protected
    class function DoAuthenticationCallback(Acls: Pcvoid; Aauth: Psg_httpauth;
      Areq: Psg_httpreq; Ares: Psg_httpres): cbool; cdecl; static;
    class procedure DoRequestCallback(Acls: Pcvoid; Areq: Psg_httpreq;
      Ares: Psg_httpres); cdecl; static;
    class procedure DoErrorCallback(Acls: Pcvoid;
      const Aerr: Pcchar); cdecl; static;
    function CreateAuthentication(
      AHandle: Pointer): TBrookHTTPAuthentication; virtual;
    function CreateSecurity: TBrookCustomHTTPServerSecurity; virtual;
    function CreateRequest(AHandle: Pointer): TBrookHTTPRequest; virtual;
    function CreateResponse(AHandle: Pointer): TBrookHTTPResponse; virtual;
    function CreateError(const AMessage: string): Exception; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoError(ASender: TObject; AException: Exception); virtual;
    function DoAuthenticate(ASender: TObject;
      AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse): Boolean; virtual;
    procedure DoAuthenticateError(ASender: TObject;
      AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse; AException: Exception); virtual;
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure DoRequestError(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse; AException: Exception); virtual;
    function HandleAuthenticate(AAuthentication: TBrookHTTPAuthentication;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): Boolean; virtual;
    procedure HandleAuthenticateError(AAuthentication: TBrookHTTPAuthentication;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      AException: Exception); virtual;
    procedure HandleRequest(ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure HandleRequestError(ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse; AException: Exception); virtual;
    procedure CheckInactive; inline;
    procedure SetActive(AValue: Boolean); virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
  public
    property Active: Boolean read FActive write SetActive stored IsActive;
    property Authenticated: Boolean read FAuthenticated write SetAuthenticated
      stored IsAuthenticated;
    property Port: UInt16 read GetPort write SetPort stored IsPort;
    property Threaded: Boolean read GetThreaded write SetThreaded
      stored IsThreaded;
    property UploadsDir: string read GetUploadsDir write SetUploadsDir
      stored IsUploadsDir;
    property PostBufferSize: NativeUInt read GetPostBufferSize
      write SetPostBufferSize stored IsPostBufferSize;
    property PayloadLimit: NativeUInt read GetPayloadLimit
      write SetPayloadLimit stored IsPayloadLimit;
    property UploadsLimit: UInt64 read GetUploadsLimit write SetUploadsLimit
      stored IsUploadsLimit;
    property ThreadPoolSize: Cardinal read GetThreadPoolSize
      write SetThreadPoolSize stored IsThreadPoolSize;
    property ConnectionTimeout: Cardinal read GetConnectionTimeout
      write SetConnectionTimeout stored IsConnectionTimeout;
    property ConnectionLimit: Cardinal read GetConnectionLimit
      write SetConnectionLimit stored IsConnectionLimit;
    property NoFavicon: Boolean read FNoFavicon write FNoFavicon
      stored IsNoFavicon;
    property Security: TBrookCustomHTTPServerSecurity read FSecurity
      write SetSecurity;
    property OnAuthenticate: TBrookHTTPAuthenticateEvent read FOnAuthenticate
      write FOnAuthenticate;
    property OnAuthenticateError: TBrookHTTPAuthenticateErrorEvent
      read FOnAuthenticateError write FOnAuthenticateError;
    property OnRequest: TBrookHTTPRequestEvent read FOnRequest write FOnRequest;
    property OnRequestError: TBrookHTTPRequestErrorEvent read FOnRequestError
      write FOnRequestError;
    property OnError: TBrookErrorEvent read FOnError write FOnError;
  end;

  TBrookHTTPServer = class(TBrookCustomHTTPServer)
  published
    property Active;
    property Authenticated;
    property Port default 0;
    property Threaded default False;
    property UploadsDir;
    property PostBufferSize default BROOK_POST_BUFFER_SIZE;
    property PayloadLimit default BROOK_PAYLOAD_LIMIT;
    property UploadsLimit default BROOK_UPLOADS_LIMIT;
    property ThreadPoolSize default 0;
    property ConnectionTimeout default 0;
    property ConnectionLimit default 0;
    property NoFavicon default False;
    property Security;
    property OnAuthenticate;
    property OnAuthenticateError;
    property OnRequest;
    property OnRequestError;
    property OnError;
  end;

implementation

{ TBrookCustomHTTPServerSecurity }

procedure TBrookCustomHTTPServerSecurity.Assign(ASource: TPersistent);
var
  VSource: TBrookCustomHTTPServerSecurity;
begin
  if ASource is TBrookCustomHTTPServerSecurity then
  begin
    VSource := ASource as TBrookCustomHTTPServerSecurity;
    FPrivateKey := VSource.FPrivateKey;
    FPrivatePassword := VSource.FPrivatePassword;
    FCertificate := VSource.FCertificate;
    FTrust := VSource.FTrust;
    FDHParams := VSource.FDHParams;
  end
  else
    inherited Assign(ASource);
end;

function TBrookCustomHTTPServerSecurity.IsActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookCustomHTTPServerSecurity.Validate;
begin
  if FPrivateKey.IsEmpty then
    raise EBrookHTTPServerSecurity.CreateRes(@SBrookEmptyPrivateKey);
  if FCertificate.IsEmpty then
    raise EBrookHTTPServerSecurity.CreateRes(@SBrookEmptyCertificate);
end;

procedure TBrookCustomHTTPServerSecurity.Clear;
begin
  FActive := False;
  FPrivateKey := '';
  FPrivatePassword := '';
  FCertificate := '';
  FTrust := '';
  FDHParams := '';
end;

{ TBrookCustomHTTPServer }

constructor TBrookCustomHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecurity := CreateSecurity;
  FPostBufferSize := BROOK_POST_BUFFER_SIZE;
  FPayloadLimit := BROOK_PAYLOAD_LIMIT;
  FUploadsLimit := BROOK_UPLOADS_LIMIT;
end;

destructor TBrookCustomHTTPServer.Destroy;
begin
  FSecurity.Free;
  try
    SetActive(False);
  finally
    inherited Destroy;
  end;
end;

procedure TBrookCustomHTTPServer.InternalCreateServerHandle;
var
  VACb: sg_httpauth_cb;
begin
  if FAuthenticated then
    VACb := {$IFNDEF VER3_0}@{$ENDIF}DoAuthenticationCallback
  else
    VACb := nil;
  FHandle := sg_httpsrv_new2(VACb, Self,
{$IFNDEF VER3_0}@{$ENDIF}DoRequestCallback, Self,
{$IFNDEF VER3_0}@{$ENDIF}DoErrorCallback, Self);
  if not Assigned(FHandle) then
    raise EInvalidPointer.CreateRes(@SBrookCannotCreateServerHandle);
end;

procedure TBrookCustomHTTPServer.InternalFreeServerHandle;
begin
  { sg_httpsrv_shutdown() is called internally by sg_httpsrv_free(). }
  sg_httpsrv_free(FHandle);
  FHandle := nil;
end;

procedure TBrookCustomHTTPServer.InternalCheckServerOption(Aret: cint);
begin
  if Aret <> 0 then
  begin
    InternalFreeServerHandle;
    SgLib.CheckLastError(Aret);
  end;
end;

function TBrookCustomHTTPServer.CreateAuthentication(
  AHandle: Pointer): TBrookHTTPAuthentication;
begin
  Result := TBrookHTTPAuthentication.Create(AHandle);
end;

function TBrookCustomHTTPServer.CreateSecurity: TBrookCustomHTTPServerSecurity;
begin
  Result := TBrookHTTPServerSecurity.Create;
end;

function TBrookCustomHTTPServer.CreateRequest(
  AHandle: Pointer): TBrookHTTPRequest;
begin
  Result := TBrookHTTPRequest.Create(AHandle);
end;

function TBrookCustomHTTPServer.CreateResponse(
  AHandle: Pointer): TBrookHTTPResponse;
begin
  Result := TBrookHTTPResponse.Create(AHandle);
end;

function TBrookCustomHTTPServer.CreateError(const AMessage: string): Exception;
begin
  Result := EBrookHTTPServer.Create(AMessage);
end;

class function TBrookCustomHTTPServer.DoAuthenticationCallback(Acls: Pcvoid;
  Aauth: Psg_httpauth; Areq: Psg_httpreq; Ares: Psg_httpres): cbool;
var
  VSrv: TBrookCustomHTTPServer;
  VAuth: TBrookHTTPAuthentication;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VSrv := Acls;
  VReq := VSrv.CreateRequest(Areq);
  VRes := VSrv.CreateResponse(Ares);
  try
    if VSrv.FNoFavicon and VReq.IsFavicon then
      Exit(True);
    VAuth := VSrv.CreateAuthentication(Aauth);
    try
      Result := VSrv.HandleAuthenticate(VAuth, VReq, VRes);
    finally
      VAuth.Free;
    end;
  finally
    VRes.Free;
    VReq.Free;
  end;
end;

class procedure TBrookCustomHTTPServer.DoRequestCallback(Acls: Pcvoid;
  Areq: Psg_httpreq; Ares: Psg_httpres);
var
  VSrv: TBrookCustomHTTPServer;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VSrv := Acls;
  VReq := VSrv.CreateRequest(Areq);
  VRes := VSrv.CreateResponse(Ares);
  try
    if VSrv.FNoFavicon and VReq.IsFavicon then
      VRes.SendEmpty
    else
      VSrv.HandleRequest(VReq, VRes);
  finally
    VRes.Free;
    VReq.Free;
  end;
end;

class procedure TBrookCustomHTTPServer.DoErrorCallback(Acls: Pcvoid;
  const Aerr: Pcchar);
var
  VSrv: TBrookCustomHTTPServer;
  VExcept: Exception;
begin
  VSrv := Acls;
  VExcept := VSrv.CreateError(TMarshal.ToString(Aerr));
  try
    VSrv.DoError(VSrv, VExcept);
  finally
    VExcept.Free;
  end;
end;

procedure TBrookCustomHTTPServer.CheckInactive;
begin
  if (not (csLoading in ComponentState)) and Active then
    raise EInvalidOpException.CreateRes(@SBrookActiveServer);
end;

procedure TBrookCustomHTTPServer.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedAuthenticated then
      SetAuthenticated(True);
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

function TBrookCustomHTTPServer.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookCustomHTTPServer.DoError(ASender: TObject;
  AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(ASender, AException)
  else
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(AException)
    else
      ShowException(AException, Pointer(AException));
end;

function TBrookCustomHTTPServer.DoAuthenticate(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse): Boolean;
begin
  Result := Assigned(FOnAuthenticate) and
    FOnAuthenticate(ASender, AAuthentication, ARequest, AResponse);
end;

procedure TBrookCustomHTTPServer.DoAuthenticateError(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; AException: Exception);
begin
  if Assigned(FOnAuthenticateError) then
    FOnAuthenticateError(ASender, AAuthentication, ARequest, AResponse,
      AException)
  else
    HandleRequestError(ARequest, AResponse, AException);
end;

procedure TBrookCustomHTTPServer.DoRequest(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(ASender, ARequest, AResponse)
  else
    AResponse.SendEmpty;
end;

procedure TBrookCustomHTTPServer.DoRequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  if Assigned(FOnRequestError) then
    FOnRequestError(ASender, ARequest, AResponse, AException)
  else
    AResponse.Send(AException.Message, BROOK_CONTENT_TYPE, 500);
end;

function TBrookCustomHTTPServer.HandleAuthenticate(
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse): Boolean;
begin
  try
    Result := DoAuthenticate(Self, AAuthentication, ARequest, AResponse);
  except
    on E: Exception do
    begin
      Result := False;
      HandleAuthenticateError(AAuthentication, ARequest, AResponse, E);
    end;
  end;
end;

procedure TBrookCustomHTTPServer.HandleAuthenticateError(
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; AException: Exception);
begin
  AResponse.Clear;
  try
    DoAuthenticateError(Self, AAuthentication, ARequest, AResponse, AException);
  except
    on E: Exception do
      AResponse.Send(E.Message, BROOK_CONTENT_TYPE, 500);
  end;
end;

procedure TBrookCustomHTTPServer.HandleRequest(ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  try
    DoRequest(Self, ARequest, AResponse);
  except
    on E: Exception do
      HandleRequestError(ARequest, AResponse, E);
  end;
end;

procedure TBrookCustomHTTPServer.HandleRequestError(
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  AResponse.Clear;
  try
    DoRequestError(Self, ARequest, AResponse, AException);
  except
    on E: Exception do
      AResponse.Send(E.Message, BROOK_CONTENT_TYPE, 500);
  end;
end;

procedure TBrookCustomHTTPServer.SetPort(AValue: UInt16);
begin
  if not FStreamedActive then
    CheckInactive;
  FPort := AValue;
end;

procedure TBrookCustomHTTPServer.SetPostBufferSize(AValue: NativeUInt);
begin
  if not FStreamedActive then
    CheckInactive;
  FPostBufferSize := AValue;
end;

procedure TBrookCustomHTTPServer.SetConnectionLimit(AValue: Cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FConnectionLimit := AValue;
end;

procedure TBrookCustomHTTPServer.SetConnectionTimeout(AValue: Cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FConnectionTimeout := AValue;
end;

procedure TBrookCustomHTTPServer.SetPayloadLimit(AValue: NativeUInt);
begin
  if not FStreamedActive then
    CheckInactive;
  FPayloadLimit := AValue;
end;

procedure TBrookCustomHTTPServer.SetSecurity(
  AValue: TBrookCustomHTTPServerSecurity);
begin
  if FSecurity = AValue then
    Exit;
  if Assigned(AValue) then
    FSecurity.Assign(AValue)
  else
    FSecurity.Clear;
end;

procedure TBrookCustomHTTPServer.SetUploadsLimit(AValue: UInt64);
begin
  if not FStreamedActive then
    CheckInactive;
  FUploadsLimit := AValue;
end;

procedure TBrookCustomHTTPServer.SetThreaded(AValue: Boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  FThreaded := AValue;
  if FThreaded then
    System.IsMultiThread := True;
end;

procedure TBrookCustomHTTPServer.SetThreadPoolSize(AValue: Cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FThreadPoolSize := AValue;
  if FThreadPoolSize > 0 then
    System.IsMultiThread := True;
end;

procedure TBrookCustomHTTPServer.SetUploadsDir(const AValue: string);
begin
  if not FStreamedActive then
    CheckInactive;
  FUploadsDir := AValue;
end;

function TBrookCustomHTTPServer.IsConnectionLimit: Boolean;
begin
  Result := FConnectionLimit > 0;
end;

function TBrookCustomHTTPServer.IsConnectionTimeout: Boolean;
begin
  Result := FConnectionTimeout > 0;
end;

function TBrookCustomHTTPServer.IsNoFavicon: Boolean;
begin
  Result := FNoFavicon;
end;

function TBrookCustomHTTPServer.IsPayloadLimit: Boolean;
begin
  Result := FPayloadLimit <> BROOK_PAYLOAD_LIMIT;
end;

function TBrookCustomHTTPServer.IsUploadsLimit: Boolean;
begin
  Result := FUploadsLimit <> BROOK_UPLOADS_LIMIT;
end;

function TBrookCustomHTTPServer.IsActive: Boolean;
begin
  Result := FActive;
end;

function TBrookCustomHTTPServer.GetPort: UInt16;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FPort := sg_httpsrv_port(FHandle);
  end;
  Result := FPort;
end;

function TBrookCustomHTTPServer.GetThreaded: Boolean;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FThreaded := sg_httpsrv_is_threaded(FHandle);
  end;
  Result := FThreaded;
end;

function TBrookCustomHTTPServer.GetUploadsDir: string;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FUploadsDir := TMarshal.ToString(sg_httpsrv_upld_dir(FHandle));
  end;
  Result := FUploadsDir;
end;

function TBrookCustomHTTPServer.GetPostBufferSize: NativeUInt;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FPostBufferSize := sg_httpsrv_post_buf_size(FHandle);
  end;
  Result := FPostBufferSize;
end;

function TBrookCustomHTTPServer.GetPayloadLimit: NativeUInt;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FPayloadLimit := sg_httpsrv_payld_limit(FHandle);
  end;
  Result := FPayloadLimit;
end;

function TBrookCustomHTTPServer.GetUploadsLimit: UInt64;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FUploadsLimit := sg_httpsrv_uplds_limit(FHandle);
  end;
  Result := FUploadsLimit;
end;

function TBrookCustomHTTPServer.GetThreadPoolSize: Cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FThreadPoolSize := sg_httpsrv_thr_pool_size(FHandle);
  end;
  Result := FThreadPoolSize;
end;

function TBrookCustomHTTPServer.GetConnectionTimeout: Cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FConnectionTimeout := sg_httpsrv_con_timeout(FHandle);
  end;
  Result := FConnectionTimeout;
end;

function TBrookCustomHTTPServer.GetConnectionLimit: Cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FConnectionLimit := sg_httpsrv_con_limit(FHandle);
  end;
  Result := FConnectionLimit;
end;

function TBrookCustomHTTPServer.IsAuthenticated: Boolean;
begin
  Result := FAuthenticated;
end;

function TBrookCustomHTTPServer.IsPort: Boolean;
begin
  Result := FPort <> 0;
end;

function TBrookCustomHTTPServer.IsPostBufferSize: Boolean;
begin
  Result := FPostBufferSize <> BROOK_POST_BUFFER_SIZE;
end;

function TBrookCustomHTTPServer.IsThreaded: Boolean;
begin
  Result := FThreaded;
end;

function TBrookCustomHTTPServer.IsThreadPoolSize: Boolean;
begin
  Result := FThreadPoolSize > 0;
end;

function TBrookCustomHTTPServer.IsUploadsDir: Boolean;
begin
  Result := not FUploadsDir.IsEmpty;
end;

procedure TBrookCustomHTTPServer.SetAuthenticated(AValue: Boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  if AValue = FAuthenticated then
    Exit;
  if AValue and (csReading in ComponentState) then
    FStreamedAuthenticated := True;
  FAuthenticated := AValue;
end;

procedure TBrookCustomHTTPServer.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
      SgLib.Check;
    FActive := AValue;
  end
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        DoOpen;
    end
    else
      DoClose;
end;

procedure TBrookCustomHTTPServer.DoOpen;
var
  M: TMarshaller;
begin
  if Assigned(FHandle) then
    Exit;
  SgLib.Check;
  InternalCreateServerHandle;
  if not FUploadsDir.IsEmpty then
    InternalCheckServerOption(sg_httpsrv_set_upld_dir(FHandle,
      M.ToCString(FUploadsDir)));
  if FPostBufferSize > 0 then
    InternalCheckServerOption(sg_httpsrv_set_post_buf_size(FHandle,
      FPostBufferSize));
  if FPayloadLimit > 0 then
    InternalCheckServerOption(sg_httpsrv_set_payld_limit(FHandle,
      FPayloadLimit));
  if FUploadsLimit > 0 then
    InternalCheckServerOption(sg_httpsrv_set_uplds_limit(FHandle,
      FUploadsLimit));
  if FThreadPoolSize > 0 then
    InternalCheckServerOption(sg_httpsrv_set_thr_pool_size(FHandle,
      FThreadPoolSize));
  if FConnectionTimeout > 0 then
    InternalCheckServerOption(sg_httpsrv_set_con_timeout(FHandle,
      FConnectionTimeout));
  if FConnectionLimit > 0 then
    InternalCheckServerOption(sg_httpsrv_set_con_limit(FHandle,
      FConnectionLimit));
  if FSecurity.Active then
  begin
    FSecurity.Validate;
    if not Assigned(sg_httpsrv_tls_listen2) then
      raise ENotSupportedException.CreateRes(@SBrookTLSNotAvailable);
    FActive := sg_httpsrv_tls_listen2(FHandle,
      M.ToCNullableString(FSecurity.PrivateKey),
      M.ToCNullableString(FSecurity.PrivatePassword),
      M.ToCNullableString(FSecurity.Certificate),
      M.ToCNullableString(FSecurity.Trust),
      M.ToCNullableString(FSecurity.DHParams), FPort, FThreaded);
  end
  else
    FActive := sg_httpsrv_listen(FHandle, FPort, FThreaded);
  if not FActive then
    InternalFreeServerHandle;
end;

procedure TBrookCustomHTTPServer.DoClose;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  InternalFreeServerHandle;
  FActive := False;
end;

procedure TBrookCustomHTTPServer.Open;
begin
  SetActive(True);
end;

procedure TBrookCustomHTTPServer.Close;
begin
  SetActive(False);
end;

end.
