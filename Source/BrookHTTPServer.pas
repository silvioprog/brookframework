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
  BrookString;

const
  BROOK_CONTENT_TYPE = 'text/plain; charset=utf-8';

resourcestring
  SBrookOpNotAllowedActiveServer =
    'Operation is not allowed while the server is active';
  SBrookCannotCreateHTTPServerHandler = 'Cannot create HTTP server handler';
  SBrookInvalidHTTPServerPort = 'Invalid HTTP server port: %d';

type
  TBrookHTTPServer = class;

  TBrookHTTPRequest = class;

  TBrookHTTPResponse = class;

  TBrookHTTPErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

  TBrookHTTPRequestEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRequestErrorEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
    AException: Exception) of object;

  EBrookHTTPServerError = class(Exception);

  EBrookOpNotAllowedActiveServer = class(Exception);

  TBrookHTTPRequest = class(TBrookHandledPersistent)
  private
    Freq: Pbk_httpreq;
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
  end;

  TBrookHTTPResponse = class(TBrookHandledPersistent)
  private
    Fres: Pbk_httpres;
  protected
    class function DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t; Abuf: Pcchar;
      Asize: csize_t): cssize_t; cdecl; static;
    class procedure DoStreamFree(Acls: Pcvoid); cdecl; static;
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
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
    //procedure SendData(); virtual;
  end;

  TBrookHTTPServer = class(TBrookHandledComponent)
  private
    FCatchOSErrors: Boolean;
    FOnRequest: TBrookHTTPRequestEvent;
    FOnError: TBrookHTTPErrorEvent;
    FActive: Boolean;
    FOnRequestError: TBrookHTTPRequestErrorEvent;
    FPort: UInt16;
    FThreaded: Boolean;
    FStreamedActive: Boolean;
    Fsrv: Pbk_httpsrv;
    function IsActive: Boolean;
    function IsCatchOSErrors: Boolean;
    function IsPort: Boolean;
    function IsThreaded: Boolean;
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
    function CreateRequest(AHandle: Pointer): TBrookHTTPRequest; virtual;
    function CreateResponse(AHandle: Pointer): TBrookHTTPResponse; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoError(ASender: TObject; AException: Exception); virtual;
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
    property Active: Boolean read FActive write SetActive stored IsActive
      default False;
    property Port: UInt16 read FPort write SetPort stored IsPort default 8080;
    property Threaded: Boolean read FThreaded write SetThreaded
      stored IsThreaded default False;
    property CatchOSErrors: Boolean read FCatchOSErrors write SetCatchOSErrors
      stored IsCatchOSErrors default True;
    property OnRequest: TBrookHTTPRequestEvent read FOnRequest write FOnRequest;
    property OnRequestError: TBrookHTTPRequestErrorEvent read FOnRequestError
      write FOnRequestError;
    property OnError: TBrookHTTPErrorEvent read FOnError write FOnError;
  end;

implementation

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

{ TBrookHTTPResponse }

constructor TBrookHTTPResponse.Create(AHandle: Pointer);
begin
  inherited Create;
  Fres := AHandle;
end;

function TBrookHTTPResponse.GetHandle: Pointer;
begin
  Result := Fres;
end;

{$IFDEF FPC}
 {$PUSH}{$WARN 5024 OFF}
{$ENDIF}
class function TBrookHTTPResponse.DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t;
  Abuf: Pcchar; Asize: csize_t): cssize_t;
begin
  Result := TStream(Acls).Read(Abuf^, Asize);
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
  if not Assigned(AStream) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AStream']);
  BkCheckLibrary;
  R := -bk_httpres_sendstream(Fres, AStream.Size, 32768,
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
begin
  { TODO: implement authentication. }
  Result := True;
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
  if FStreamedActive then
    Exit;
  CheckInactive;
  FPort := AValue;
end;

procedure TBrookHTTPServer.SetCatchOSErrors(AValue: Boolean);
begin
  if FStreamedActive then
    Exit;
  CheckInactive;
  FCatchOSErrors := AValue;
end;

procedure TBrookHTTPServer.SetThreaded(AValue: Boolean);
begin
  if FStreamedActive then
    Exit;
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

function TBrookHTTPServer.IsPort: Boolean;
begin
  Result := FPort <> 8080;
end;

function TBrookHTTPServer.IsThreaded: Boolean;
begin
  Result := FThreaded;
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
begin
  if Assigned(Fsrv) then
    Exit;
  BkCheckLibrary;
  Fsrv := bk_httpsrv_new2(
{$IFNDEF VER3_0}@{$ENDIF}DoAuthenticationCallback, Self,
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
