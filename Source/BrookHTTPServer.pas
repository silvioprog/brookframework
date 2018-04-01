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
    FBody: TBrookString;
    FContentType: string;
    Fres: Pbk_httpres;
    FStatus: Word;
    procedure SetContentType(const AValue: string);
    procedure SetStatus(AValue: Word);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    property Status: Word read FStatus write SetStatus;
    property ContentType: string read FContentType write SetContentType;
    property Body: TBrookString read FBody;
  end;

  TBrookHTTPServer = class(TBrookHandledComponent)
  private
    FOnRequest: TBrookHTTPRequestEvent;
    FOnError: TBrookHTTPErrorEvent;
    FActive: Boolean;
    FOnRequestError: TBrookHTTPRequestErrorEvent;
    FPort: Word;
    FThreaded: Boolean;
    FStreamedActive: Boolean;
    Fsrv: Pbk_httpsrv;
    procedure SetPort(AValue: Word);
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
    property Port: Word read FPort write SetPort default 8080;
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
  FBody := TBrookString.Create(bk_httpres_body(Fres));
  FStatus := 200;
  FContentType := 'text/html';
end;

destructor TBrookHTTPResponse.Destroy;
begin
  FBody.Free;
  inherited Destroy;
end;

procedure TBrookHTTPResponse.SetStatus(AValue: Word);
begin
  BkCheckLibrary;
  FStatus := AValue;
  CheckOSError(bk_httpres_status(Fres, AValue));
end;

procedure TBrookHTTPResponse.SetContentType(const AValue: string);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  FContentType := AValue;
  CheckOSError(bk_httpres_type(Fres, M.ToCString(AValue)));
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
    FOnRequest(ASender, ARequest, AResponse);
end;

procedure TBrookHTTPServer.DoRequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
  AException: Exception);
begin
  if Assigned(FOnRequestError) then
    FOnRequestError(ASender, ARequest, AResponse, AException)
  else
  begin
    AResponse.Body.Text := AException.Message;
    AResponse.Status := 500;
  end;
end;

procedure TBrookHTTPServer.CheckInactive;
begin
  if not (csLoading in ComponentState) and Active then
    raise EBrookOpNotAllowedActiveServer.CreateRes(
      @SBrookOpNotAllowedActiveServer);
end;

procedure TBrookHTTPServer.SetPort(AValue: Word);
begin
  CheckInactive;
  FPort := AValue;
end;

procedure TBrookHTTPServer.SetThreaded(AValue: Boolean);
begin
  CheckInactive;
  FThreaded := AValue;
end;

procedure TBrookHTTPServer.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
    BkCheckLibrary
  else
    if AValue then
    begin
      if csReading in ComponentState then
      begin
        FStreamedActive := True;
        Exit;
      end
      else
        InternalStart;
    end
    else
      InternalStop;
  FActive := AValue;
end;

procedure TBrookHTTPServer.InternalStart;
begin
  if Assigned(Fsrv) then
    Exit;
  BkCheckLibrary;
  Fsrv := bk_httpsrv_new2(BrookHTTPServerRequestCallback, Self,
    BrookHTTPServerErrorCallback, Self);
  if not Assigned(Fsrv) then
    raise EInvalidPointer.CreateRes(@SBrookCannotCreateHTTPServerHandler);
  if FPort <= 0 then
    raise EInvalidOperation.CreateResFmt(@SBrookInvalidHTTPServerPort, [FPort]);
  CheckOSError(bk_httpsrv_start(Fsrv, FPort, FThreaded));
end;

procedure TBrookHTTPServer.InternalStop;
begin
  if not Assigned(Fsrv) then
    Exit;
  BkCheckLibrary;
  bk_httpsrv_free(Fsrv);
  Fsrv := nil;
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
