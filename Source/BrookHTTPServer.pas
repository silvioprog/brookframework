unit BrookHTTPServer;

{$I Brook.inc}

interface

uses
  SysUtils,
  Classes,
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
    const AError: string) of object;

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
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    procedure Send(const AValue, AContentType: string;
      AStatus: Word); overload; virtual;
    procedure Send(const AFmt: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word); overload; virtual;
    procedure Send(ABuffer: Pointer; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); overload; virtual;
    procedure Send(const ABytes: TBytes; ASize: NativeUInt;
      const AContentType: string; AStatus: Word); overload; virtual;
    procedure Send(AString: TBrookString; const AContentType: string;
      AStatus: Word); overload; virtual;
    procedure SendFile(const AFileName: TFileName; ARendered: Boolean); virtual;
  end;

  TBrookHTTPServer = class(TBrookHandledComponent)
  private
    FOnRequest: TBrookHTTPRequestEvent;
    FOnError: TBrookHTTPErrorEvent;
    FActive: Boolean;
    FOnRequestError: TBrookHTTPRequestErrorEvent;
    FPort: UInt16;
    FThreaded: Boolean;
    FStreamedActive: Boolean;
    Fsrv: Pbk_httpsrv;
    procedure SetPort(AValue: UInt16);
    procedure SetThreaded(AValue: Boolean);
  protected
    function CreateRequest(AHandle: Pointer): TBrookHTTPRequest; virtual;
    function CreateResponse(AHandle: Pointer): TBrookHTTPResponse; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoError(ASender: TObject; const AError: string); virtual;
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
    property Active: Boolean read FActive write SetActive default False;
    property Port: UInt16 read FPort write SetPort default 8080;
    property Threaded: Boolean read FThreaded write SetThreaded;
    property OnRequest: TBrookHTTPRequestEvent read FOnRequest write FOnRequest;
    property OnRequestError: TBrookHTTPRequestErrorEvent read FOnRequestError
      write FOnRequestError;
    property OnError: TBrookHTTPErrorEvent read FOnError write FOnError;
  end;

implementation

procedure BrookHTTPServerRequestCallback(Acls: Pcvoid; Areq: Pbk_httpreq;
  Ares: Pbk_httpres); cdecl;
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

procedure BrookHTTPServerErrorCallback(Acls: Pcvoid; const Aerr: Pcchar); cdecl;
var
  VServer: TBrookHTTPServer absolute Acls;
begin
  VServer.DoError(VServer, TMarshal.ToString(Aerr));
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

{ TBrookHTTPResponse }

constructor TBrookHTTPResponse.Create(AHandle: Pointer);
begin
  inherited Create;
  Fres := AHandle;
end;

procedure TBrookHTTPResponse.Send(const AValue, AContentType: string;
  AStatus: Word);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpres_send(Fres, M.ToCString(AValue),
    M.ToCString(AContentType), AStatus));
end;

procedure TBrookHTTPResponse.Send(const AFmt: string;
  const AArgs: array of const; const AContentType: string; AStatus: Word);
begin
  Send(Format(AFmt, AArgs), AContentType, AStatus);
end;

procedure TBrookHTTPResponse.Send(ABuffer: Pointer; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpres_sendbinary(Fres, ABuffer, ASize,
    M.ToCString(AContentType), AStatus));
end;

procedure TBrookHTTPResponse.Send(const ABytes: TBytes; ASize: NativeUInt;
  const AContentType: string; AStatus: Word);
begin
  Send(@ABytes[0], ASize, AContentType, AStatus);
end;

procedure TBrookHTTPResponse.Send(AString: TBrookString;
  const AContentType: string; AStatus: Word);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpres_sendstr(Fres, AString.Handle,
    M.ToCString(AContentType), AStatus));
end;

procedure TBrookHTTPResponse.SendFile(const AFileName: TFileName;
  ARendered: Boolean);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpres_sendfile(Fres, M.ToCString(AFileName), ARendered));
end;

function TBrookHTTPResponse.GetHandle: Pointer;
begin
  Result := Fres;
end;

{ TBrookHTTPServer }

constructor TBrookHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPort := 8080;
end;

destructor TBrookHTTPServer.Destroy;
begin
  SetActive(False);
  inherited Destroy;
end;

function TBrookHTTPServer.CreateRequest(AHandle: Pointer): TBrookHTTPRequest;
begin
  Result := TBrookHTTPRequest.Create(AHandle);
end;

function TBrookHTTPServer.CreateResponse(AHandle: Pointer): TBrookHTTPResponse;
begin
  Result := TBrookHTTPResponse.Create(AHandle);
end;

procedure TBrookHTTPServer.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr)
    else
      raise;
  end;
end;

function TBrookHTTPServer.GetHandle: Pointer;
begin
  Result := Fsrv;
end;

procedure TBrookHTTPServer.DoError(ASender: TObject; const AError: string);
begin
  if Assigned(FOnError) then
    FOnError(ASender, AError);
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

procedure TBrookHTTPServer.SetThreaded(AValue: Boolean);
begin
  if FStreamedActive then
    Exit;
  CheckInactive;
  FThreaded := AValue;
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
  R: cint;
begin
  if Assigned(Fsrv) then
    Exit;
  BkCheckLibrary;
  Fsrv := bk_httpsrv_new2(BrookHTTPServerRequestCallback, Self,
    BrookHTTPServerErrorCallback, Self);
  if not Assigned(Fsrv) then
    raise EInvalidPointer.CreateRes(@SBrookCannotCreateHTTPServerHandler);
  if FPort <= 0 then
  begin
    bk_httpsrv_free(Fsrv);
    Fsrv := nil;
    raise EInvalidOperation.CreateResFmt(
      @SBrookInvalidHTTPServerPort, [FPort]);
  end;
  R := bk_httpsrv_start(Fsrv, FPort, FThreaded);
  if R < 0 then
    CheckOSError(-R);
  FActive := R = 0;
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
