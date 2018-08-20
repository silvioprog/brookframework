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
  libsagui,
  BrookHandledClasses,
  BrookHTTPExtra,
  BrookHTTPAuthentication,
  BrookHTTPRequest,
  BrookHTTPResponse;

const
  BROOK_POST_BUFFER_SIZE = {$IFDEF CPUARM}1024{~1Kb}{$ELSE}4096{~4kB}{$ENDIF};
  BROOK_PAYLOAD_LIMIT = {$IFDEF CPUARM}1048576{~1MB}{$ELSE}4194304{~4MB}{$ENDIF};
  BROOK_UPLOADS_LIMIT = {$IFDEF CPUARM}16777216{~16MB}{$ELSE}67108864{~64MB}{$ENDIF};

resourcestring
  SBrookOpNotAllowedActiveServer =
    'Operation is not allowed while the server is active.';
  SBrookCannotCreateHTTPServerHandle = 'Cannot create HTTP server handle.';
  SBrookTLSNotAvailable = 'TLS is not available.';
  SBrookEmptyPrivateKey = 'Private key cannot be empty.';
  SBrookEmptyCertificate = 'Certificate cannot be empty.';

type
  TBrookHTTPErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

  TBrookHTTPAuthenticationEvent = function(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse): Boolean of object;

  TBrookHTTPAuthenticationErrorEvent = procedure(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse; AException: Exception) of object;

  TBrookHTTPRequestEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRequestErrorEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
    AException: Exception) of object;

  EBrookHTTPServer = class(Exception);

  EBrookOpNotAllowedActiveServer = class(Exception);

  EBrookHTTPServerSecurity = class(Exception);

  TBrookHTTPServerSecurity = class(TPersistent)
  private
    FActive: Boolean;
    FPrivateKey: string;
    FPrivatePassword: string;
    FCertificate: string;
    FTrust: string;
    FDHParams: string;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Clear; virtual;
    procedure Validate; inline;
  published
    property Active: Boolean read FActive write FActive;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    property PrivatePassword: string read FPrivatePassword
      write FPrivatePassword;
    property Certificate: string read FCertificate write FCertificate;
    property Trust: string read FTrust write FTrust;
    property DHParams: string read FDHParams write FDHParams;
  end;

  TBrookHTTPServer = class(TBrookHandledComponent)
  private
    FHandle: Psg_httpsrv;
    FAuthenticated: Boolean;
    FCatchOSErrors: Boolean;
    FConnectionLimit: Cardinal;
    FConnectionTimeout: Cardinal;
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
    FSecurity: TBrookHTTPServerSecurity;
    FOnAuthenticate: TBrookHTTPAuthenticationEvent;
    FOnAuthenticateError: TBrookHTTPAuthenticationErrorEvent;
    FOnRequest: TBrookHTTPRequestEvent;
    FOnRequestError: TBrookHTTPRequestErrorEvent;
    FOnError: TBrookHTTPErrorEvent;
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
    function IsCatchOSErrors: Boolean;
    function IsConnectionLimit: Boolean;
    function IsConnectionTimeout: Boolean;
    function IsPayloadLimit: Boolean;
    function IsUploadsLimit: Boolean;
    function IsPort: Boolean;
    function IsPostBufferSize: Boolean;
    function IsThreaded: Boolean;
    function IsThreadPoolSize: Boolean;
    function IsUploadsDir: Boolean;
    procedure SetAuthenticated(AValue: Boolean);
    procedure SetCatchOSErrors(AValue: Boolean);
    procedure SetConnectionLimit(AValue: Cardinal);
    procedure SetConnectionTimeout(AValue: Cardinal);
    procedure SetPayloadLimit(AValue: NativeUInt);
    procedure SetSecurity(AValue: TBrookHTTPServerSecurity);
    procedure SetUploadsLimit(AValue: UInt64);
    procedure SetPort(AValue: UInt16);
    procedure SetPostBufferSize(AValue: NativeUInt);
    procedure SetThreaded(AValue: Boolean);
    procedure SetThreadPoolSize(AValue: Cardinal);
    procedure SetUploadsDir(const AValue: string);
    procedure InternalCreateServerHandle;
    procedure InternalFreeServerHandle; inline;
    procedure InternalCheckServerOption(Aopt: cint); inline;
  protected
    class function DoAuthenticationCallback(Acls: Pcvoid; Aauth: Psg_httpauth;
      Areq: Psg_httpreq; Ares: Psg_httpres): cbool; cdecl; static;
    class procedure DoRequestCallback(Acls: Pcvoid; Areq: Psg_httpreq;
      Ares: Psg_httpres); cdecl; static;
    class procedure DoErrorCallback(Acls: Pcvoid;
      const Aerr: Pcchar); cdecl; static;
    function CreateAuthentication(
      AHandle: Pointer): TBrookHTTPAuthentication; virtual;
    function CreateSecurity: TBrookHTTPServerSecurity; virtual;
    function CreateRequest(AHandle: Pointer): TBrookHTTPRequest; virtual;
    function CreateResponse(AHandle: Pointer): TBrookHTTPResponse; virtual;
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
    procedure CheckInactive; inline;
    procedure SetActive(AValue: Boolean); virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
  published
    property Active: Boolean read FActive write SetActive stored IsActive;
    property Authenticated: Boolean read FAuthenticated write SetAuthenticated
      stored IsAuthenticated;
    property Port: UInt16 read GetPort write SetPort stored IsPort
      default 0;
    property Threaded: Boolean read GetThreaded write SetThreaded
      stored IsThreaded default False;
    property CatchOSErrors: Boolean read FCatchOSErrors write SetCatchOSErrors
      stored IsCatchOSErrors default True;
    property UploadsDir: string read GetUploadsDir write SetUploadsDir
      stored IsUploadsDir;
    property PostBufferSize: NativeUInt read GetPostBufferSize
      write SetPostBufferSize stored IsPostBufferSize
      default BROOK_POST_BUFFER_SIZE;
    property PayloadLimit: NativeUInt read GetPayloadLimit
      write SetPayloadLimit stored IsPayloadLimit
      default BROOK_PAYLOAD_LIMIT;
    property UploadsLimit: UInt64 read GetUploadsLimit write SetUploadsLimit
      stored IsUploadsLimit default BROOK_UPLOADS_LIMIT;
    property ThreadPoolSize: Cardinal read GetThreadPoolSize
      write SetThreadPoolSize stored IsThreadPoolSize default 0;
    property ConnectionTimeout: Cardinal read GetConnectionTimeout
      write SetConnectionTimeout stored IsConnectionTimeout default 0;
    property ConnectionLimit: Cardinal read GetConnectionLimit
      write SetConnectionLimit stored IsConnectionLimit default 0;
    property Security: TBrookHTTPServerSecurity read FSecurity write SetSecurity;
    property OnAuthenticate: TBrookHTTPAuthenticationEvent read FOnAuthenticate
      write FOnAuthenticate;
    property OnAuthenticateError: TBrookHTTPAuthenticationErrorEvent
      read FOnAuthenticateError write FOnAuthenticateError;
    property OnRequest: TBrookHTTPRequestEvent read FOnRequest write FOnRequest;
    property OnRequestError: TBrookHTTPRequestErrorEvent read FOnRequestError
      write FOnRequestError;
    property OnError: TBrookHTTPErrorEvent read FOnError write FOnError;
  end;

implementation

{ TBrookHTTPServerSecurity }

procedure TBrookHTTPServerSecurity.Assign(ASource: TPersistent);
var
  VSource: TBrookHTTPServerSecurity;
begin
  if ASsigned(ASource) and (ASource is TBrookHTTPServerSecurity) then
  begin
    VSource := ASource as TBrookHTTPServerSecurity;
    FPrivateKey := VSource.FPrivateKey;
    FPrivatePassword := VSource.FPrivatePassword;
    FCertificate := VSource.FCertificate;
    FTrust := VSource.FTrust;
    FDHParams := VSource.FDHParams;
  end
  else
    inherited Assign(ASource);
end;

procedure TBrookHTTPServerSecurity.Validate;
begin
  if FPrivateKey.IsEmpty then
    raise EBrookHTTPServerSecurity.CreateRes(@SBrookEmptyPrivateKey);
  if FCertificate.IsEmpty then
    raise EBrookHTTPServerSecurity.CreateRes(@SBrookEmptyCertificate);
end;

procedure TBrookHTTPServerSecurity.Clear;
begin
  FActive := False;
  FPrivateKey := '';
  FPrivatePassword := '';
  FCertificate := '';
  FTrust := '';
  FDHParams := '';
end;

{ TBrookHTTPServer }

constructor TBrookHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecurity := CreateSecurity;
  FCatchOSErrors := True;
  FPostBufferSize := BROOK_POST_BUFFER_SIZE;
  FPayloadLimit := BROOK_PAYLOAD_LIMIT;
  FUploadsLimit := BROOK_UPLOADS_LIMIT;
end;

destructor TBrookHTTPServer.Destroy;
begin
  FSecurity.Free;
  try
    SetActive(False);
  finally
    inherited Destroy;
  end;
end;

procedure TBrookHTTPServer.InternalCreateServerHandle;
var
  ACB: sg_httpauth_cb;
begin
  if FAuthenticated then
    ACB := {$IFNDEF VER3_0}@{$ENDIF}DoAuthenticationCallback
  else
    ACB := nil;
  FHandle := sg_httpsrv_new2(ACB, Self,
{$IFNDEF VER3_0}@{$ENDIF}DoRequestCallback, Self,
{$IFNDEF VER3_0}@{$ENDIF}DoErrorCallback, Self);
  if not Assigned(FHandle) then
    raise EInvalidPointer.CreateRes(@SBrookCannotCreateHTTPServerHandle);
end;

procedure TBrookHTTPServer.InternalFreeServerHandle;
begin
  { sg_httpsrv_shutdown() is called internally by sg_httpsrv_free(). }
  sg_httpsrv_free(FHandle);
  FHandle := nil;
end;

procedure TBrookHTTPServer.InternalCheckServerOption(Aopt: cint);
begin
  if Aopt <> 0 then
  begin
    InternalFreeServerHandle;
    SgCheckLastError(Aopt);
  end;
end;

function TBrookHTTPServer.CreateAuthentication(
  AHandle: Pointer): TBrookHTTPAuthentication;
begin
  Result := TBrookHTTPAuthentication.Create(AHandle);
end;

function TBrookHTTPServer.CreateSecurity: TBrookHTTPServerSecurity;
begin
  Result := TBrookHTTPServerSecurity.Create;
end;

function TBrookHTTPServer.CreateRequest(AHandle: Pointer): TBrookHTTPRequest;
begin
  Result := TBrookHTTPRequest.Create(AHandle);
end;

function TBrookHTTPServer.CreateResponse(AHandle: Pointer): TBrookHTTPResponse;
begin
  Result := TBrookHTTPResponse.Create(AHandle);
end;

class function TBrookHTTPServer.DoAuthenticationCallback(Acls: Pcvoid;
  Aauth: Psg_httpauth; Areq: Psg_httpreq; Ares: Psg_httpres): cbool;
var
  VSrv: TBrookHTTPServer absolute Acls;
  VAuth: TBrookHTTPAuthentication;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VAuth := TBrookHTTPAuthentication.Create(Aauth);
  VReq := TBrookHTTPRequest.Create(Areq);
  VRes := TBrookHTTPResponse.Create(Ares);
  try
    try
      Result := VSrv.DoAuthenticate(VSrv, VAuth, VReq, VRes);
    except
      on E: EOSError do
      begin
        Result := False;
        if VSrv.CatchOSErrors then
          VSrv.DoAuthenticateError(VSrv, VAuth, VReq, VRes, E)
        else
          VSrv.DoError(VSrv, E);
      end;
      on E: Exception do
      begin
        Result := False;
        VSrv.DoAuthenticateError(VSrv, VAuth, VReq, VRes, E);
      end;
    end;
  finally
    VRes.Free;
    VReq.Free;
    VAuth.Free;
  end;
end;

class procedure TBrookHTTPServer.DoRequestCallback(Acls: Pcvoid;
  Areq: Psg_httpreq; Ares: Psg_httpres);
var
  VSrv: TBrookHTTPServer absolute Acls;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VReq := VSrv.CreateRequest(Areq);
  VRes := VSrv.CreateResponse(Ares);
  try
    try
      VSrv.DoRequest(VSrv, VReq, VRes);
    except
      on E: EOSError do
        if VSrv.CatchOSErrors then
          VSrv.DoRequestError(VSrv, VReq, VRes, E)
        else
          VSrv.DoError(VSrv, E);
      on E: Exception do
        VSrv.DoRequestError(VSrv, VReq, VRes, E);
    end;
  finally
    VRes.Free;
    VReq.Free;
  end;
end;

class procedure TBrookHTTPServer.DoErrorCallback(Acls: Pcvoid;
  const Aerr: Pcchar);
var
  VServer: TBrookHTTPServer absolute Acls;
  VException: EBrookHTTPServer;
begin
  VException := EBrookHTTPServer.Create(TMarshal.ToString(Aerr));
  try
    VServer.DoError(VServer, VException);
  finally
    VException.Free;
  end;
end;

procedure TBrookHTTPServer.Loaded;
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

function TBrookHTTPServer.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPServer.DoError(ASender: TObject; AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(ASender, AException);
end;

function TBrookHTTPServer.DoAuthenticate(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse): Boolean;
begin
  Result := Assigned(FOnAuthenticate) and
    FOnAuthenticate(ASender, AAuthentication, ARequest, AResponse);
end;

procedure TBrookHTTPServer.DoAuthenticateError(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; AException: Exception);
begin
  if Assigned(FOnAuthenticateError) then
    FOnAuthenticateError(ASender, AAuthentication, ARequest, AResponse,
      AException)
  else
    AAuthentication.Deny(AException.Message, BROOK_CONTENT_TYPE);
end;

procedure TBrookHTTPServer.DoRequest(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(ASender, ARequest, AResponse)
  else
    AResponse.Send('Empty response', BROOK_CONTENT_TYPE, 200);
end;

procedure TBrookHTTPServer.DoRequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  if Assigned(FOnRequestError) then
    FOnRequestError(ASender, ARequest, AResponse, AException)
  else
    AResponse.Send(AException.Message, BROOK_CONTENT_TYPE, 500);
end;

procedure TBrookHTTPServer.CheckInactive;
begin
  if not (csLoading in ComponentState) and Active then
    raise EBrookOpNotAllowedActiveServer.CreateRes(
      @SBrookOpNotAllowedActiveServer);
end;

procedure TBrookHTTPServer.SetPort(AValue: UInt16);
begin
  if not FStreamedActive then
    CheckInactive;
  FPort := AValue;
end;

procedure TBrookHTTPServer.SetPostBufferSize(AValue: NativeUInt);
begin
  if not FStreamedActive then
    CheckInactive;
  FPostBufferSize := AValue;
end;

procedure TBrookHTTPServer.SetCatchOSErrors(AValue: Boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  FCatchOSErrors := AValue;
end;

procedure TBrookHTTPServer.SetConnectionLimit(AValue: Cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FConnectionLimit := AValue;
end;

procedure TBrookHTTPServer.SetConnectionTimeout(AValue: Cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FConnectionTimeout := AValue;
end;

procedure TBrookHTTPServer.SetPayloadLimit(AValue: NativeUInt);
begin
  if not FStreamedActive then
    CheckInactive;
  FPayloadLimit := AValue;
end;

procedure TBrookHTTPServer.SetSecurity(AValue: TBrookHTTPServerSecurity);
begin
  if FSecurity = AValue then
    Exit;
  if Assigned(AValue) then
    FSecurity.Assign(AValue)
  else
    FSecurity.Clear;
end;

procedure TBrookHTTPServer.SetUploadsLimit(AValue: UInt64);
begin
  if not FStreamedActive then
    CheckInactive;
  FUploadsLimit := AValue;
end;

procedure TBrookHTTPServer.SetThreaded(AValue: Boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  FThreaded := AValue;
end;

procedure TBrookHTTPServer.SetThreadPoolSize(AValue: Cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FThreadPoolSize := AValue;
end;

procedure TBrookHTTPServer.SetUploadsDir(const AValue: string);
begin
  if not FStreamedActive then
    CheckInactive;
  FUploadsDir := AValue;
end;

function TBrookHTTPServer.IsCatchOSErrors: Boolean;
begin
  Result := not FCatchOSErrors;
end;

function TBrookHTTPServer.IsConnectionLimit: Boolean;
begin
  Result := FConnectionLimit > 0;
end;

function TBrookHTTPServer.IsConnectionTimeout: Boolean;
begin
  Result := FConnectionTimeout > 0;
end;

function TBrookHTTPServer.IsPayloadLimit: Boolean;
begin
  Result := FPayloadLimit > 0;
end;

function TBrookHTTPServer.IsUploadsLimit: Boolean;
begin
  Result := FUploadsLimit > 0;
end;

function TBrookHTTPServer.IsActive: Boolean;
begin
  Result := FActive;
end;

function TBrookHTTPServer.GetPort: UInt16;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FPort := sg_httpsrv_port(FHandle);
  end;
  Result := FPort;
end;

function TBrookHTTPServer.GetThreaded: Boolean;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FThreaded := sg_httpsrv_is_threaded(FHandle);
  end;
  Result := FThreaded;
end;

function TBrookHTTPServer.GetUploadsDir: string;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FUploadsDir := TMarshal.ToString(sg_httpsrv_upld_dir(FHandle));
  end;
  Result := FUploadsDir;
end;

function TBrookHTTPServer.GetPostBufferSize: NativeUInt;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FPostBufferSize := sg_httpsrv_post_buf_size(FHandle);
  end;
  Result := FPostBufferSize;
end;

function TBrookHTTPServer.GetPayloadLimit: NativeUInt;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FPayloadLimit := sg_httpsrv_payld_limit(FHandle);
  end;
  Result := FPayloadLimit;
end;

function TBrookHTTPServer.GetUploadsLimit: UInt64;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FUploadsLimit := sg_httpsrv_uplds_limit(FHandle);
  end;
  Result := FUploadsLimit;
end;

function TBrookHTTPServer.GetThreadPoolSize: Cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FThreadPoolSize := sg_httpsrv_thr_pool_size(FHandle);
  end;
  Result := FThreadPoolSize;
end;

function TBrookHTTPServer.GetConnectionTimeout: Cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FConnectionTimeout := sg_httpsrv_con_timeout(FHandle);
  end;
  Result := FConnectionTimeout;
end;

function TBrookHTTPServer.GetConnectionLimit: Cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgCheckLibrary;
    FConnectionLimit := sg_httpsrv_con_limit(FHandle);
  end;
  Result := FConnectionLimit;
end;

function TBrookHTTPServer.IsAuthenticated: Boolean;
begin
  Result := FAuthenticated;
end;

function TBrookHTTPServer.IsPort: Boolean;
begin
  Result := FPort <> 0;
end;

function TBrookHTTPServer.IsPostBufferSize: Boolean;
begin
  Result := FPostBufferSize > 0;
end;

function TBrookHTTPServer.IsThreaded: Boolean;
begin
  Result := FThreaded;
end;

function TBrookHTTPServer.IsThreadPoolSize: Boolean;
begin
  Result := FThreadPoolSize > 0;
end;

function TBrookHTTPServer.IsUploadsDir: Boolean;
begin
  Result := not FUploadsDir.IsEmpty;
end;

procedure TBrookHTTPServer.SetAuthenticated(AValue: Boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  if AValue = FAuthenticated then
    Exit;
  if AValue and (csReading in ComponentState) then
    FStreamedAuthenticated := True;
  FAuthenticated := AValue;
end;

procedure TBrookHTTPServer.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
      SgCheckLibrary;
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

procedure TBrookHTTPServer.DoOpen;
var
  M: TMarshaller;
begin
  if Assigned(FHandle) then
    Exit;
  SgCheckLibrary;
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
      M.ToCNullable(FSecurity.PrivateKey),
      M.ToCNullable(FSecurity.PrivatePassword),
      M.ToCNullable(FSecurity.Certificate),
      M.ToCNullable(FSecurity.Trust),
      M.ToCNullable(FSecurity.DHParams), FPort, FThreaded);
  end
  else
    FActive := sg_httpsrv_listen(FHandle, FPort, FThreaded);
  if not FActive then
    InternalFreeServerHandle;
end;

procedure TBrookHTTPServer.DoClose;
begin
  if not Assigned(FHandle) then
    Exit;
  SgCheckLibrary;
  InternalFreeServerHandle;
  FActive := False;
end;

procedure TBrookHTTPServer.Open;
begin
  SetActive(True);
end;

procedure TBrookHTTPServer.Close;
begin
  SetActive(False);
end;

end.
