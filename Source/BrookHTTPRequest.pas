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
{$IFDEF BROOK_HAS_HTTPS_SUPPORT}
    FTLSSession: Pointer;
{$ENDIF}
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
{$IFDEF BROOK_HAS_HTTPS_SUPPORT}
    property TLSSession: Pointer read FTLSSession;
{$ENDIF}
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
{$IFDEF BROOK_HAS_HTTPS_SUPPORT}
  FTLSSession := sg_httpreq_tls_session(FHandle);
{$ENDIF}
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
