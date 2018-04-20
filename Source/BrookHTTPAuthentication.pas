unit BrookHTTPAuthentication;

{$I Brook.inc}

interface

uses
  SysUtils,
  Platform,
  Marshalling,
  libbrook,
  BrookHandledClasses;

type
  TBrookHTTPAuthentication = class(TBrookHandledPersistent)
  private
    FHandle: Pbk_httpauth;
    FVersion: string;
    FMethod: string;
    FPath: string;
    FUserName: string;
    FPassword: string;
    function GetRealm: string;
    procedure SetRealm(const AValue: string);
    function GetPaths: TArray<string>;
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    function Deny(const AJustification,
      AContentType: string): Boolean; overload; virtual;
    function Deny(const AFmt: string; const AArgs: array of const;
      const AContentType: string): Boolean; overload; virtual;
    procedure Cancel; virtual;
    property Version: string read FVersion;
    property Method: string read FMethod;
    property Path: string read FPath;
    property Paths: TArray<string> read GetPaths;
    property Realm: string read GetRealm write SetRealm;
    property UserName: string read FUserName;
    property Password: string read FPassword;
  end;

implementation

constructor TBrookHTTPAuthentication.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FVersion := TMarshal.ToString(bk_httpauth_version(FHandle));
  FMethod := TMarshal.ToString(bk_httpauth_method(FHandle));
  FPath := TMarshal.ToString(bk_httpauth_path(FHandle));
  FUserName := TMarshal.ToString(bk_httpauth_usr(FHandle));
  FPassword := TMarshal.ToString(bk_httpauth_pwd(FHandle));
end;

function TBrookHTTPAuthentication.GetHandle: Pointer;
begin
  Result := FHandle;
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

function TBrookHTTPAuthentication.GetRealm: string;
begin
  BkCheckLibrary;
  Result := TMarshal.ToString(bk_httpauth_realm(FHandle));
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
