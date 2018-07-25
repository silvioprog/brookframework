unit BrookHTTPAuthentication;

{$I Brook.inc}

interface

uses
  SysUtils,
  Marshalling,
  libsagui,
  BrookHandledClasses;

type
  TBrookHTTPAuthentication = class(TBrookHandledPersistent)
  private
    FUserName: string;
    FPassword: string;
    FHandle: Psg_httpauth;
    function GetRealm: string;
    procedure SetRealm(const AValue: string);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    procedure Deny(const AJustification, AContentType: string); overload; virtual;
    procedure Deny(const AFmt: string; const AArgs: array of const;
      const AContentType: string); overload; virtual;
    procedure Cancel; virtual;
    property Realm: string read GetRealm write SetRealm;
    property UserName: string read FUserName;
    property Password: string read FPassword;
  end;

implementation

constructor TBrookHTTPAuthentication.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FUserName := TMarshal.ToString(sg_httpauth_usr(FHandle));
  FPassword := TMarshal.ToString(sg_httpauth_pwd(FHandle));
end;

function TBrookHTTPAuthentication.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookHTTPAuthentication.SetRealm(const AValue: string);
var
  M: TMarshaller;
begin
  SgCheckLibrary;
  SgCheckLastError(-sg_httpauth_set_realm(FHandle, M.ToCString(AValue)));
end;

function TBrookHTTPAuthentication.GetRealm: string;
begin
  SgCheckLibrary;
  Result := TMarshal.ToString(sg_httpauth_realm(FHandle));
end;

procedure TBrookHTTPAuthentication.Deny(const AJustification,
  AContentType: string);
var
  M: TMarshaller;
begin
  SgCheckLibrary;
  SgCheckLastError(-sg_httpauth_deny(FHandle, M.ToCString(AJustification),
    M.ToCString(AContentType)));
end;

procedure TBrookHTTPAuthentication.Deny(const AFmt: string;
  const AArgs: array of const; const AContentType: string);
begin
  Deny(Format(AFmt, AArgs), AContentType);
end;

procedure TBrookHTTPAuthentication.Cancel;
begin
  SgCheckLibrary;
  SgCheckLastError(-sg_httpauth_cancel(FHandle));
end;

end.
