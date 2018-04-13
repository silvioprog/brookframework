unit BrookHTTPServer;

{$I Brook.inc}

interface

uses
  SysUtils,
  Classes,
  Marshalling,
  libbrook,
  BrookHandledClasses,
  BrookHTTPExtra,
  BrookHTTPAuthentication,
  BrookHTTPRequest,
  BrookHTTPResponse;

resourcestring
  SBrookOpNotAllowedActiveServer =
    'Operation is not allowed while the server is active';
  SBrookCannotCreateHTTPServerHandler = 'Cannot create HTTP server handler';
  SBrookInvalidHTTPServerPort = 'Invalid HTTP server port: %d';

type
  TBrookHTTPErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

  TBrookHTTPAuthenticationEvent = function(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication): Boolean of object;

  TBrookHTTPAuthenticationErrorEvent = procedure(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication; AException: Exception) of object;

  TBrookHTTPRequestEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRequestErrorEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
    AException: Exception) of object;

  EBrookHTTPServerError = class(Exception);

  EBrookOpNotAllowedActiveServer = class(Exception);

  TBrookHTTPServer = class(TBrookHandledComponent)
  private
    FAuthenticated: Boolean;
    FCatchOSErrors: Boolean;
    FOnAuthenticate: TBrookHTTPAuthenticationEvent;
    FOnAuthenticateError: TBrookHTTPAuthenticationErrorEvent;
    FOnRequest: TBrookHTTPRequestEvent;
    FOnError: TBrookHTTPErrorEvent;
    FActive: Boolean;
    FOnRequestError: TBrookHTTPRequestErrorEvent;
    FPort: UInt16;
    FThreaded: Boolean;
    FStreamedActive: Boolean;
    FStreamedAuthenticated: Boolean;
    Fsrv: Pbk_httpsrv;
    function IsActive: Boolean;
    function IsAuthenticated: Boolean;
    function IsCatchOSErrors: Boolean;
    function IsPort: Boolean;
    function IsThreaded: Boolean;
    procedure SetAuthenticated(AValue: Boolean);
    procedure SetCatchOSErrors(AValue: Boolean);
    procedure SetPort(AValue: UInt16);
    procedure SetThreaded(AValue: Boolean);
  protected
    class function DoAuthenticationCallback(Acls: Pcvoid;
      Aauth: Pbk_httpauth): cbool; cdecl; static;
    class procedure DoRequestCallback(Acls: Pcvoid; Areq: Pbk_httpreq;
      Ares: Pbk_httpres); cdecl; static;
    class procedure DoErrorCallback(Acls: Pcvoid;
      const Aerr: Pcchar); cdecl; static;
    function CreateAuthentication(
      AHandle: Pointer): TBrookHTTPAuthentication; virtual;
    function CreateRequest(AHandle: Pointer): TBrookHTTPRequest; virtual;
    function CreateResponse(AHandle: Pointer): TBrookHTTPResponse; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoError(ASender: TObject; AException: Exception); virtual;
    function DoAuthenticate(ASender: TObject;
      AAuthentication: TBrookHTTPAuthentication): Boolean; virtual;
    procedure DoAuthenticateError(ASender: TObject;
      AAuthentication: TBrookHTTPAuthentication; AException: Exception); virtual;
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure DoRequestError(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse; AException: Exception); virtual;
    procedure CheckInactive; inline;
    procedure SetActive(AValue: Boolean); virtual;
    procedure InternalStart; virtual;
    procedure InternalStop; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Active: Boolean read FActive write SetActive stored IsActive;
    property Authenticated: Boolean read FAuthenticated write SetAuthenticated
      stored IsAuthenticated;
    property Port: UInt16 read FPort write SetPort stored IsPort
      default BROOK_PORT;
    property Threaded: Boolean read FThreaded write SetThreaded
      stored IsThreaded default False;
    property CatchOSErrors: Boolean read FCatchOSErrors write SetCatchOSErrors
      stored IsCatchOSErrors default True;
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

constructor TBrookHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort := BROOK_PORT;
  FCatchOSErrors := True;
end;

destructor TBrookHTTPServer.Destroy;
begin
  try
    SetActive(False);
  finally
    inherited Destroy;
  end;
end;

function TBrookHTTPServer.CreateAuthentication(
  AHandle: Pointer): TBrookHTTPAuthentication;
begin
  Result := TBrookHTTPAuthentication.Create(AHandle);
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
  Aauth: Pbk_httpauth): cbool;
var
  VSrv: TBrookHTTPServer absolute Acls;
  VAuth: TBrookHTTPAuthentication;
begin
  VAuth := TBrookHTTPAuthentication.Create(Aauth);
  try
    try
      Result := VSrv.DoAuthenticate(VSrv, VAuth);
    except
      on E: EOSError do
      begin
        Result := False;
        if VSrv.CatchOSErrors then
          VSrv.DoAuthenticateError(VSrv, VAuth, E)
        else
          VSrv.DoError(VSrv, E);
      end;
      on E: Exception do
      begin
        Result := False;
        VSrv.DoAuthenticateError(VSrv, VAuth, E);
      end;
    end;
  finally
    VAuth.Free;
  end;
end;

class procedure TBrookHTTPServer.DoRequestCallback(Acls: Pcvoid;
  Areq: Pbk_httpreq; Ares: Pbk_httpres);
var
  VSrv: TBrookHTTPServer absolute Acls;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VReq := VSrv.CreateRequest(Areq);
  try
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
    end;
  finally
    VReq.Free;
  end;
end;

class procedure TBrookHTTPServer.DoErrorCallback(Acls: Pcvoid;
  const Aerr: Pcchar);
var
  VServer: TBrookHTTPServer absolute Acls;
  VException: EBrookHTTPServerError;
begin
  VException := EBrookHTTPServerError.Create(TMarshal.ToString(Aerr));
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
  Result := Fsrv;
end;

procedure TBrookHTTPServer.DoError(ASender: TObject; AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(ASender, AException);
end;

function TBrookHTTPServer.DoAuthenticate(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication): Boolean;
begin
  Result := Assigned(FOnAuthenticate) and
    FOnAuthenticate(ASender, AAuthentication);
end;

procedure TBrookHTTPServer.DoAuthenticateError(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; AException: Exception);
begin
  if Assigned(FOnAuthenticateError) then
    FOnAuthenticateError(ASender, AAuthentication, AException)
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

procedure TBrookHTTPServer.SetCatchOSErrors(AValue: Boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  FCatchOSErrors := AValue;
end;

procedure TBrookHTTPServer.SetThreaded(AValue: Boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  FThreaded := AValue;
end;

function TBrookHTTPServer.IsCatchOSErrors: Boolean;
begin
  Result := not FCatchOSErrors;
end;

function TBrookHTTPServer.IsActive: Boolean;
begin
  Result := FActive;
end;

function TBrookHTTPServer.IsAuthenticated: Boolean;
begin
  Result := FAuthenticated;
end;

function TBrookHTTPServer.IsPort: Boolean;
begin
  Result := FPort <> BROOK_PORT;
end;

function TBrookHTTPServer.IsThreaded: Boolean;
begin
  Result := FThreaded;
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
      BkCheckLibrary;
    FActive := AValue;
  end
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        InternalStart;
    end
    else
      InternalStop;
end;

procedure TBrookHTTPServer.InternalStart;
var
  VAuthCb: bk_httpauth_cb;
begin
  if Assigned(Fsrv) then
    Exit;
  BkCheckLibrary;
  if FAuthenticated then
    VAuthCb := {$IFNDEF VER3_0}@{$ENDIF}DoAuthenticationCallback
  else
    VAuthCb := nil;
  Fsrv := bk_httpsrv_new2(VAuthCb, Self,
{$IFNDEF VER3_0}@{$ENDIF}DoRequestCallback, Self,
{$IFNDEF VER3_0}@{$ENDIF}DoErrorCallback, Self);
  if not Assigned(Fsrv) then
    raise EInvalidPointer.CreateRes(@SBrookCannotCreateHTTPServerHandler);
  if FPort <= 0 then
  begin
    bk_httpsrv_free(Fsrv);
    Fsrv := nil;
    raise EInvalidOperation.CreateResFmt(
      @SBrookInvalidHTTPServerPort, [FPort]);
  end;
  FActive := bk_httpsrv_start(Fsrv, FPort, FThreaded) = 0;
  if FActive then
    Exit;
  bk_httpsrv_free(Fsrv);
  Fsrv := nil;
end;

procedure TBrookHTTPServer.InternalStop;
begin
  if not Assigned(Fsrv) then
    Exit;
  BkCheckLibrary;
  bk_httpsrv_free(Fsrv);
  Fsrv := nil;
  FActive := False;
end;

procedure TBrookHTTPServer.Start;
begin
  SetActive(True);
end;

procedure TBrookHTTPServer.Stop;
begin
  SetActive(False);
end;

end.
