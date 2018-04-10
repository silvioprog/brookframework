unit BrookHTTPServer;

{$I Brook.inc}

interface

uses
  RtlConsts,
  SysUtils,
  Classes,
  Platform,
  Marshalling,
  libbrook,
  BrookHandledClasses,
  BrookString,
  BrookStringMap;

const
  BROOK_BLOCK_SIZE = 4096;
  BROOK_CONTENT_TYPE = 'text/plain; charset=utf-8';

resourcestring
  SBrookOpNotAllowedActiveServer =
    'Operation is not allowed while the server is active';
  SBrookCannotCreateHTTPServerHandler = 'Cannot create HTTP server handler';
  SBrookInvalidHTTPServerPort = 'Invalid HTTP server port: %d';
  SBrookInvalidHTTPStatus = 'Invalid status code: %d';

type
  TBrookHTTPAuthentication = class;

  TBrookHTTPServer = class;

  TBrookHTTPRequest = class;

  TBrookHTTPResponse = class;

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

  EBrookHTTPServerError = class(Exception);

  EBrookOpNotAllowedActiveServer = class(Exception);

  TBrookHTTPAuthentication = class(TBrookHandledPersistent)
  private
    Fauth: Pbk_httpauth;
    FPassword: string;
    FRealm: string;
    FUserName: string;
    procedure SetRealm(const AValue: string);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    procedure Cancel; virtual;
    property Realm: string read FRealm write SetRealm;
    property UserName: string read FUserName;
    property Password: string read FPassword;
  end;

  TBrookHTTPRequest = class(TBrookHandledPersistent)
  private
    Freq: Pbk_httpreq;
    function GetMethod: string;
    function GetPath: string;
    function GetUserData: Pointer;
    function GetVersion: string;
    procedure SetUserData(AValue: Pointer);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    property Version: string read GetVersion;
    property Method: string read GetMethod;
    property Path: string read GetPath;
    property UserData: Pointer read GetUserData write SetUserData;
  end;

  TBrookHTTPResponse = class(TBrookHandledPersistent)
  private
    FHeaders: TBrookStringMap;
    Fres: Pbk_httpres;
  protected
    class function DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t; Abuf: Pcchar;
      Asize: csize_t): cssize_t; cdecl; static;
    class procedure DoStreamFree(Acls: Pcvoid); cdecl; static;
    class procedure CheckStatus(AStatus: Word); static; inline;
    class procedure CheckStream(AStream: TStream); static; inline;
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    function Send(const AValue, AContentType: string;
      AStatus: Word): Boolean; overload; virtual;
    function Send(const AFmt: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word): Boolean; overload; virtual;
    function Send(ABuffer: Pointer; ASize: NativeUInt;
      const AContentType: string; AStatus: Word): Boolean; overload; virtual;
    function Send(const ABytes: TBytes; ASize: NativeUInt;
      const AContentType: string; AStatus: Word): Boolean; overload; virtual;
    function Send(AString: TBrookString; const AContentType: string;
      AStatus: Word): Boolean; overload; virtual;
    function TrySendFile(ABlockSite: NativeUInt; const AFileName: TFileName;
      ARendered: Boolean; AStatus: Word;
      out AFailed: Boolean): Boolean; overload; virtual;
    function SendFile(ABlockSite: NativeUInt; const AFileName: TFileName;
      ARendered: Boolean; AStatus: Word): Boolean; overload; virtual;
    function SendFile(const AFileName: TFileName): Boolean; overload; virtual;
    function SendStream(AStream: TStream; AStatus: Word): Boolean; virtual;
    function SendData(AStream: TStream; AStatus: Word): Boolean; virtual;
    property Headers: TBrookStringMap read FHeaders;
  end;

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
    class function DoAuthenticationCallback(Acls: Pcvoid; Aauth: Pbk_httpauth;
      Areq: Pbk_httpreq; Ares: Pbk_httpreq): cbool; cdecl; static;
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
    property Port: UInt16 read FPort write SetPort stored IsPort default 8080;
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

{ TBrookHTTPAuthentication }

constructor TBrookHTTPAuthentication.Create(AHandle: Pointer);
begin
  inherited Create;
  Fauth := AHandle;
  FUserName := TMarshal.ToString(bk_httpauth_usr(Fauth));
  FPassword := TMarshal.ToString(bk_httpauth_pwd(Fauth));
end;

function TBrookHTTPAuthentication.GetHandle: Pointer;
begin
  Result := Fauth;
end;

procedure TBrookHTTPAuthentication.SetRealm(const AValue: string);
var
  M: TMarshaller;
begin
  if AValue = FRealm then
    Exit;
  BkCheckLibrary;
  FRealm := AValue;
  CheckOSError(-bk_httpauth_setrealm(Fauth, M.ToCString(FRealm)));
end;

procedure TBrookHTTPAuthentication.Cancel;
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpauth_cancel(Fauth));
end;

{ TBrookHTTPRequest }

constructor TBrookHTTPRequest.Create(AHandle: Pointer);
begin
  inherited Create;
  Freq := AHandle;
end;

function TBrookHTTPRequest.GetHandle: Pointer;
begin
  Result := Freq;
end;

function TBrookHTTPRequest.GetVersion: string;
begin
  BkCheckLibrary;
  Result := TMarshal.ToString(bk_httpreq_version(Freq));
end;

function TBrookHTTPRequest.GetMethod: string;
begin
  BkCheckLibrary;
  Result := TMarshal.ToString(bk_httpreq_method(Freq));
end;

function TBrookHTTPRequest.GetPath: string;
begin
  BkCheckLibrary;
  Result := TMarshal.ToString(bk_httpreq_path(Freq));
end;

function TBrookHTTPRequest.GetUserData: Pointer;
begin
  BkCheckLibrary;
  Result := bk_httpreq_userdata(Freq);
end;

procedure TBrookHTTPRequest.SetUserData(AValue: Pointer);
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpreq_setuserdata(Freq, AValue));
end;

{ TBrookHTTPResponse }

constructor TBrookHTTPResponse.Create(AHandle: Pointer);
begin
  inherited Create;
  Fres := AHandle;
  FHeaders := CreateHeaders(bk_httpres_headers(Fres));
end;

destructor TBrookHTTPResponse.Destroy;
begin
  FHeaders.Free;
  inherited Destroy;
end;

function TBrookHTTPResponse.GetHandle: Pointer;
begin
  Result := Fres;
end;

class procedure TBrookHTTPResponse.CheckStatus(AStatus: Word);
begin
  if (AStatus < 100) or (AStatus > 599) then
    raise EArgumentException.CreateResFmt(@SBrookInvalidHTTPStatus, [AStatus]);
end;

class procedure TBrookHTTPResponse.CheckStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AStream']);
end;

function TBrookHTTPResponse.CreateHeaders(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

{$IFDEF FPC}
 {$PUSH}{$WARN 5024 OFF}
{$ENDIF}
class function TBrookHTTPResponse.DoStreamRead(Acls: Pcvoid;
  Aoffset: cuint64_t; Abuf: Pcchar; Asize: csize_t): cssize_t;
begin
  Result := TStream(Acls).Read(Abuf^, Asize);
  if Result = 0 then
    Exit(bk_httpread_end(False));
  if Result = -1 then
    Result := bk_httpread_end(True);
end;
{$IFDEF FPC}
 {$POP}
{$ENDIF}

class procedure TBrookHTTPResponse.DoStreamFree(Acls: Pcvoid);
begin
  TStream(Acls).Free;
end;

function TBrookHTTPResponse.Send(const AValue, AContentType: string;
  AStatus: Word): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_send(Fres, M.ToCString(AValue),
    M.ToCString(AContentType), AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    CheckOSError(R);
end;

function TBrookHTTPResponse.Send(const AFmt: string;
  const AArgs: array of const; const AContentType: string;
  AStatus: Word): Boolean;
begin
  Result := Send(Format(AFmt, AArgs), AContentType, AStatus);
end;

function TBrookHTTPResponse.Send(ABuffer: Pointer; ASize: NativeUInt;
  const AContentType: string; AStatus: Word): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendbinary(Fres, ABuffer, ASize,
    M.ToCString(AContentType), AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    CheckOSError(R);
end;

function TBrookHTTPResponse.Send(const ABytes: TBytes; ASize: NativeUInt;
  const AContentType: string; AStatus: Word): Boolean;
begin
  Result := Send(@ABytes[0], ASize, AContentType, AStatus);
end;

function TBrookHTTPResponse.Send(AString: TBrookString;
  const AContentType: string; AStatus: Word): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendstr(Fres, AString.Handle,
    M.ToCString(AContentType), AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    CheckOSError(R);
end;

function TBrookHTTPResponse.TrySendFile(ABlockSite: NativeUInt;
  const AFileName: TFileName; ARendered: Boolean; AStatus: Word;
  out AFailed: Boolean): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendfile(Fres, ABlockSite, M.ToCString(AFileName),
    ARendered, AStatus);
  Result := R = 0;
  if not Result then
  begin
    AFailed := R = ENOENT;
    if (not AFailed) and (R <> EALREADY) then
      CheckOSError(R);
  end;
end;

function TBrookHTTPResponse.SendFile(ABlockSite: NativeUInt;
  const AFileName: TFileName; ARendered: Boolean; AStatus: Word): Boolean;
begin
  if not TrySendFile(ABlockSite, AFileName, ARendered, AStatus, Result) then
    raise EFileNotFoundException.CreateResFmt(@SFOpenError, [AFileName]);
end;

function TBrookHTTPResponse.SendFile(const AFileName: TFileName): Boolean;
begin
  Result := SendFile(4096, AFileName, False, 200);
end;

function TBrookHTTPResponse.SendStream(AStream: TStream;
  AStatus: Word): Boolean;
var
  R: cint;
begin
  CheckStream(AStream);
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendstream(Fres, AStream.Size, BROOK_BLOCK_SIZE,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamRead, AStream,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamFree, AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    CheckOSError(R);
end;

function TBrookHTTPResponse.SendData(AStream: TStream; AStatus: Word): Boolean;
var
  R: cint;
begin
  CheckStream(AStream);
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_senddata(Fres, BROOK_BLOCK_SIZE,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamRead, AStream,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamFree, AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    CheckOSError(R);
end;

{ TBrookHTTPServer }

constructor TBrookHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort := 8080;
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
  Aauth: Pbk_httpauth; Areq: Pbk_httpreq; Ares: Pbk_httpreq): cbool;
var
  VSrv: TBrookHTTPServer absolute Acls;
  VAuth: TBrookHTTPAuthentication;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VAuth := TBrookHTTPAuthentication.Create(Aauth);
  try
    VReq := VSrv.CreateRequest(Areq);
    try
      VRes := VSrv.CreateResponse(Ares);
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
      end;
    finally
      VReq.Free;
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
    AResponse.Send(AException.Message, BROOK_CONTENT_TYPE, 500);
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
  Result := FPort <> 8080;
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
