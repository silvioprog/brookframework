unit BrookHTTPRequest;

{$I Brook.inc}

interface

uses
  SysUtils,
  Marshalling,
  libbrook,
  BrookHandledClasses,
  BrookStringMap;

type
  TBrookHTTPRequest = class(TBrookHandledPersistent)
  private
    FCookies: TBrookStringMap;
    FHeaders: TBrookStringMap;
    FParams: TBrookStringMap;
    Freq: Pbk_httpreq;
    function GetMethod: string;
    function GetPath: string;
    function GetUserData: Pointer;
    function GetVersion: string;
    procedure SetUserData(AValue: Pointer);
  protected
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateCookies(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateParams(AHandle: Pointer): TBrookStringMap; virtual;
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    property Headers: TBrookStringMap read FHeaders;
    property Cookies: TBrookStringMap read FCookies;
    property Params: TBrookStringMap read FParams;
    { TODO: Fields }
    property Version: string read GetVersion;
    property Method: string read GetMethod;
    property Path: string read GetPath;
    property UserData: Pointer read GetUserData write SetUserData;
  end;

implementation

constructor TBrookHTTPRequest.Create(AHandle: Pointer);
begin
  inherited Create;
  Freq := AHandle;
  FHeaders := CreateHeaders(bk_httpreq_headers(Freq));
  FCookies := CreateCookies(bk_httpreq_cookies(Freq));
  FParams := CreateParams(bk_httpreq_params(Freq));
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

end.
