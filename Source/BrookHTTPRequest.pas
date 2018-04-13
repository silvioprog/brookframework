unit BrookHTTPRequest;

{$I Brook.inc}

interface

uses
  SysUtils,
  Marshalling,
  libbrook,
  BrookHandledClasses,
  BrookString,
  BrookStringMap;

type
  TBrookHTTPRequest = class(TBrookHandledPersistent)
  private
    FCookies: TBrookStringMap;
    FHeaders: TBrookStringMap;
    FIsPost: Boolean;
    FMethod: string;
    FParams: TBrookStringMap;
    FPath: string;
    FPayload: TBrookString;
    FHandle: Pbk_httpreq;
    FUserData: Pointer;
    FVersion: string;
    procedure SetUserData(AValue: Pointer);
  protected
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateCookies(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateParams(AHandle: Pointer): TBrookStringMap; virtual;
    function CreatePayload(AHandle: Pointer): TBrookString; virtual;
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    property Headers: TBrookStringMap read FHeaders;
    property Cookies: TBrookStringMap read FCookies;
    property Params: TBrookStringMap read FParams;
    { TODO: Fields }
    property Payload: TBrookString read FPayload;
    property Version: string read FVersion;
    property Method: string read FMethod;
    property Path: string read FPath;
    property IsPost: Boolean read FIsPost;
    property UserData: Pointer read FUserData write SetUserData;
  end;

implementation

constructor TBrookHTTPRequest.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FHeaders := CreateHeaders(bk_httpreq_headers(FHandle));
  FCookies := CreateCookies(bk_httpreq_cookies(FHandle));
  FParams := CreateParams(bk_httpreq_params(FHandle));
  FPayload := CreatePayload(bk_httpreq_payload(FHandle));
  FVersion := TMarshal.ToString(bk_httpreq_version(FHandle));
  FMethod := TMarshal.ToString(bk_httpreq_method(FHandle));
  FPath := TMarshal.ToString(bk_httpreq_path(FHandle));
  FIsPost := bk_httpreq_ispost(FHandle);
  FUserData := bk_httpreq_userdata(FHandle);
end;

destructor TBrookHTTPRequest.Destroy;
begin
  FParams.Free;
  FCookies.Free;
  FHeaders.Free;
  inherited Destroy;
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

function TBrookHTTPRequest.CreatePayload(AHandle: Pointer): TBrookString;
begin
  Result := TBrookString.Create(AHandle);
end;

function TBrookHTTPRequest.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPRequest.SetUserData(AValue: Pointer);
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpreq_setuserdata(FHandle, AValue));
end;

end.
