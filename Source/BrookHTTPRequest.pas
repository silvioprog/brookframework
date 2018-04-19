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
    FHeaders: TBrookStringMap;
    FCookies: TBrookStringMap;
    FParams: TBrookStringMap;
    FFields: TBrookStringMap;
    FPayload: TBrookString;
    FVersion: string;
    FMethod: string;
    FPath: string;
    FIsPost: Boolean;
    FUserData: Pointer;
    FHandle: Pbk_httpreq;
    function GetPaths: TArray<string>;
    procedure SetUserData(AValue: Pointer);
  protected
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateCookies(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateParams(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateFields(AHandle: Pointer): TBrookStringMap; virtual;
    function CreatePayload(AHandle: Pointer): TBrookString; virtual;
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    property Headers: TBrookStringMap read FHeaders;
    property Cookies: TBrookStringMap read FCookies;
    property Params: TBrookStringMap read FParams;
    property Fields: TBrookStringMap read FFields;
    property Payload: TBrookString read FPayload;
    property Version: string read FVersion;
    property Method: string read FMethod;
    property Path: string read FPath;
    property Paths: TArray<string> read GetPaths;
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
  FFields := CreateFields(bk_httpreq_fields(FHandle));
  FPayload := CreatePayload(bk_httpreq_payload(FHandle));
  FVersion := TMarshal.ToString(bk_httpreq_version(FHandle));
  FMethod := TMarshal.ToString(bk_httpreq_method(FHandle));
  FPath := TMarshal.ToString(bk_httpreq_path(FHandle));
  FIsPost := bk_httpreq_ispost(FHandle);
  FUserData := bk_httpreq_userdata(FHandle);
end;

destructor TBrookHTTPRequest.Destroy;
begin
  FHeaders.Free;
  FCookies.Free;
  FParams.Free;
  FFields.Free;
  FPayload.Free;
  inherited Destroy;
end;

function TBrookHTTPRequest.GetHandle: Pointer;
begin
  Result := FHandle;
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

function TBrookHTTPRequest.GetPaths: TArray<string>;
begin
  Result := Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);
end;

procedure TBrookHTTPRequest.SetUserData(AValue: Pointer);
begin
  BkCheckLibrary;
  BkCheckLastError(-bk_httpreq_setuserdata(FHandle, AValue));
end;

end.
