unit BrookHTTPAuthentication;

{$I Brook.inc}

interface

uses
  SysUtils,
  Platform,
  Marshalling,
  libbrook,
  BrookHandledClasses,
  BrookStringMap;

type
  TBrookHTTPAuthentication = class(TBrookHandledPersistent)
  private
    FHeaders: TBrookStringMap;
    FCookies: TBrookStringMap;
    FParams: TBrookStringMap;
    FVersion: string;
    FMethod: string;
    FPath: string;
    FUserName: string;
    FPassword: string;
    FHandle: Pbk_httpauth;
    function GetRealm: string;
    function GetUserData: Pointer;
    procedure SetRealm(const AValue: string);
    procedure SetUserData(AValue: Pointer);
    function GetPaths: TArray<string>;
  protected
    function GetHandle: Pointer; override;
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateCookies(AHandle: Pointer): TBrookStringMap; virtual;
    function CreateParams(AHandle: Pointer): TBrookStringMap; virtual;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    function SetCookie(const AName, AValue: string): Boolean; virtual;
    function Deny(const AJustification,
      AContentType: string): Boolean; overload; virtual;
    function Deny(const AFmt: string; const AArgs: array of const;
      const AContentType: string): Boolean; overload; virtual;
    procedure Cancel; virtual;
    property Headers: TBrookStringMap read FHeaders;
    property Cookies: TBrookStringMap read FCookies;
    property Params: TBrookStringMap read FParams;
    property Version: string read FVersion;
    property Method: string read FMethod;
    property Path: string read FPath;
    property Paths: TArray<string> read GetPaths;
    property Realm: string read GetRealm write SetRealm;
    property UserName: string read FUserName;
    property Password: string read FPassword;
    property UserData: Pointer read GetUserData write SetUserData;
  end;

implementation

constructor TBrookHTTPAuthentication.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FHeaders := CreateHeaders(bk_httpauth_headers(FHandle));
  FCookies := CreateCookies(bk_httpauth_cookies(FHandle));
  FParams := CreateParams(bk_httpauth_params(FHandle));
  FVersion := TMarshal.ToString(bk_httpauth_version(FHandle));
  FMethod := TMarshal.ToString(bk_httpauth_method(FHandle));
  FPath := TMarshal.ToString(bk_httpauth_path(FHandle));
  FUserName := TMarshal.ToString(bk_httpauth_usr(FHandle));
  FPassword := TMarshal.ToString(bk_httpauth_pwd(FHandle));
end;

destructor TBrookHTTPAuthentication.Destroy;
begin
  FHeaders.Free;
  FCookies.Free;
  FParams.Free;
  inherited Destroy;
end;

function TBrookHTTPAuthentication.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookHTTPAuthentication.CreateHeaders(
  AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPAuthentication.CreateCookies(
  AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPAuthentication.CreateParams(
  AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

function TBrookHTTPAuthentication.SetCookie(const AName,
  AValue: string): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  BkCheckLibrary;
  R := -bk_httpauth_set_cookie(FHandle, M.ToCString(AName), M.ToCString(AValue));
  Result := R = 0;
  if (not Result) and (R <> EINVAL) then
    BkCheckLastError(R);
end;

function TBrookHTTPAuthentication.GetPaths: TArray<string>;
begin
  Result := Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);
end;

procedure TBrookHTTPAuthentication.SetRealm(const AValue: string);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  BkCheckLastError(-bk_httpauth_set_realm(FHandle, M.ToCString(AValue)));
end;

procedure TBrookHTTPAuthentication.SetUserData(AValue: Pointer);
begin
  BkCheckLibrary;
  BkCheckLastError(-bk_httpauth_set_user_data(FHandle, AValue));
end;

function TBrookHTTPAuthentication.GetRealm: string;
begin
  BkCheckLibrary;
  Result := TMarshal.ToString(bk_httpauth_realm(FHandle));
end;

function TBrookHTTPAuthentication.GetUserData: Pointer;
begin
  BkCheckLibrary;
  Result := bk_httpauth_user_data(FHandle);
end;

function TBrookHTTPAuthentication.Deny(const AJustification,
  AContentType: string): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  BkCheckLibrary;
  R := -bk_httpauth_deny(FHandle, M.ToCString(AJustification),
    M.ToCString(AContentType));
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    BkCheckLastError(R);
end;

function TBrookHTTPAuthentication.Deny(const AFmt: string;
  const AArgs: array of const; const AContentType: string): Boolean;
begin
  Result := Deny(Format(AFmt, AArgs), AContentType);
end;

procedure TBrookHTTPAuthentication.Cancel;
begin
  BkCheckLibrary;
  BkCheckLastError(-bk_httpauth_cancel(FHandle));
end;

end.
