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
    Freq: Pbk_httpreq;
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
  Freq := AHandle;
  FHeaders := CreateHeaders(bk_httpreq_headers(Freq));
  FCookies := CreateCookies(bk_httpreq_cookies(Freq));
  FParams := CreateParams(bk_httpreq_params(Freq));
  FPayload := CreatePayload(bk_httpreq_payload(Freq));
  FVersion := TMarshal.ToString(bk_httpreq_version(Freq));
  FMethod := TMarshal.ToString(bk_httpreq_method(Freq));
  FPath := TMarshal.ToString(bk_httpreq_path(Freq));
  FIsPost := bk_httpreq_ispost(Freq);
  FUserData := bk_httpreq_userdata(Freq);
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
  Result := Freq;
end;

procedure TBrookHTTPRequest.SetUserData(AValue: Pointer);
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpreq_setuserdata(Freq, AValue));
end;

end.
