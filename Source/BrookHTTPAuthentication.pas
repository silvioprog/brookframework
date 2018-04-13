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
    FRealm: string;
    FUserName: string;
    FPassword: string;
    procedure SetRealm(const AValue: string);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    function Deny(const AJustification,
      AContentType: string): Boolean; overload; virtual;
    function Deny(const AFmt: string; const AArgs: array of const;
      const AContentType: string): Boolean; overload; virtual;
    procedure Cancel; virtual;
    property Realm: string read FRealm write SetRealm;
    property UserName: string read FUserName;
    property Password: string read FPassword;
  end;

implementation

constructor TBrookHTTPAuthentication.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FUserName := TMarshal.ToString(bk_httpauth_usr(FHandle));
  FPassword := TMarshal.ToString(bk_httpauth_pwd(FHandle));
end;

function TBrookHTTPAuthentication.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPAuthentication.SetRealm(const AValue: string);
var
  M: TMarshaller;
begin
  if AValue = FRealm then
    Exit;
  BkCheckLibrary;
  FRealm := AValue;
  CheckOSError(-bk_httpauth_setrealm(FHandle, M.ToCString(FRealm)));
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
    CheckOSError(R);
end;

function TBrookHTTPAuthentication.Deny(const AFmt: string;
  const AArgs: array of const; const AContentType: string): Boolean;
begin
  Result := Deny(Format(AFmt, AArgs), AContentType);
end;

procedure TBrookHTTPAuthentication.Cancel;
begin
  BkCheckLibrary;
  CheckOSError(-bk_httpauth_cancel(FHandle));
end;

end.
