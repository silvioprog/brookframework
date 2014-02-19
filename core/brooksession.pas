(*
  Brook Session unit.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/brookframework

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookSession;

{$i brook.inc}

interface

uses
  BrookClasses, BrookHttpDefs, BrookUtils, BrookException, BrookConsts, Classes,
  SysUtils, HTTPDefs, DateUtils;

type
  { Handles exceptions for @link(TBrookSession). }
  EBrookSession = class(EBrook);

  { Is a metaclass for @link(TBrookSession) class. }
  TBrookSessionClass = class of TBrookSession;

  { Defines features to the section handling. }
  TBrookSession = class(TBrookObject)
  private
    FCookieDomain: string;
    FCookieName: string;
    FCookiePath: string;
    FCookieSecure: Boolean;
    FFields: TStrings;
    FDirectory: string;
    FFileName: TFileName;
    FFilePrefix: ShortString;
    FHttpOnly: Boolean;
    FSID: string;
    FStarted: Boolean;
    FTimeOut: Integer;
  protected
    function IsStarted: Boolean;
    procedure CheckSID(ARequest: TBrookRequest); virtual;
    procedure CheckFileName; virtual;
    procedure CheckCookie(AResponse: TBrookResponse); virtual;
    procedure Load; virtual;
    procedure Save; virtual;
  public
    { Creates an instance of a @link(TBrookSession) class. }
    constructor Create; virtual;
    { Frees an instance of @link(TBrookSession) class. }
    destructor Destroy; override;
    { Gets an object coming from the session data. }
    procedure GetFields(AObject: TObject);
    { Returns @code(True) if the session has expired.}
    function IsExpired: Boolean; virtual;
    { Creates an ID for the session. }
    function GenerateID: string; virtual;
    { Starts the session. }
    procedure Start(ARequest: TBrookRequest); virtual;
    { Terminates the session. }
    procedure Finish(AResponse: TBrookResponse); virtual;
    { Expires the session. }
    procedure Expire(ARequest: TBrookRequest;
      AResponse: TBrookResponse); virtual;
    { Checks if a name exists in fields. }
    function Exists(const AName: string): Boolean;
    { Set the session cookie name. }
    property CookieName: string read FCookieName write FCookieName;
    { Set the session cookie domain. }
    property CookieDomain: string read FCookieDomain write FCookieDomain;
    { Set the session cookie path. }
    property CookiePath: string read FCookiePath write FCookiePath;
    { Set the session cookie secure. }
    property CookieSecure: Boolean read FCookieSecure write FCookieSecure;
    { The session fields. }
    property Fields: TStrings read FFields;
    { Set the name of session directory. }
    property Directory: string read FDirectory write FDirectory;
    { Returns @code(True) if the session has expired.}
    property Expired: Boolean read IsExpired;
    { Get or set the session ID. }
    property SID: string read FSID write FSID;
    { Checks if the session has started. }
    property Started: Boolean read IsStarted;
    { The session file name. }
    property FileName: TFileName read FFileName write FFileName;
    { The session file prefix. }
    property FilePrefix: ShortString read FFilePrefix write FFilePrefix;
    { The remaining seconds for the session finish. }
    property TimeOut: Integer read FTimeOut write FTimeOut;
    { Informs if the session cookie is accessible only by HTTP requests,
      if @code(True), the JavaScript access is not allowed. }
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
  end;

implementation

constructor TBrookSession.Create;
begin
  inherited Create;
  FFields := TStringList.Create;
  FCookieName := BROOK_SESS_ID;
  FFilePrefix := BROOK_SESS_PREFIX;
  FDirectory := GetTempDir(False);
  if FDirectory = ES then
    FDirectory := ExtractFilePath(ParamStr(0));
  FTimeOut := BROOK_SESS_DEFAULT_TIMEOUT;
  FHttpOnly := True;
end;

destructor TBrookSession.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TBrookSession.GetFields(AObject: TObject);
begin
  BrookStringsToObject(FFields, AObject);
end;

function TBrookSession.IsExpired: Boolean;
begin
  Result := FTimeOut <> 0;
  if not Result then
    Exit;
  if FileExists(FFileName) then
  begin
    if FTimeOut > 0 then
      Result := IncSecond(BrookFileDate(FFileName), FTimeOut) < Now
    else
      Result := False;
  end
  else
    Result := True;
end;

function TBrookSession.GenerateID: string;
var
  VGuid: TGuid;
begin
  CreateGUID(VGuid);
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32, BROOK_UUID_MASK, [VGuid.D1, VGuid.D2, VGuid.D3,
    VGuid.D4[0], VGuid.D4[1], VGuid.D4[2], VGuid.D4[3], VGuid.D4[4],
    VGuid.D4[5], VGuid.D4[6], VGuid.D4[7]]);
end;

function TBrookSession.IsStarted: Boolean;
begin
  Result := FStarted;
end;

procedure TBrookSession.CheckSID(ARequest: TBrookRequest);
begin
  if FSID = ES then
    FSID := ARequest.CookieFields.Values[FCookieName];
  if FSID = ES then
    FSID := GenerateID;
end;

procedure TBrookSession.CheckFileName;
begin
  FFileName := IncludeTrailingPathDelimiter(FDirectory) + FFilePrefix + FSID;
end;

procedure TBrookSession.CheckCookie(AResponse: TBrookResponse);
var
  VCookie: TCookie;
begin
  VCookie := AResponse.Cookies.FindCookie(FCookieName);
  if not Assigned(VCookie) then
  begin
    VCookie := AResponse.Cookies.Add;
    VCookie.Name := FCookieName;
    VCookie.Domain := FCookieDomain;
    VCookie.Path := FCookiePath;
    VCookie.Secure := FCookieSecure;
    VCookie.HttpOnly := FHttpOnly;
  end;
  VCookie.Value := SID;
end;

procedure TBrookSession.Load;
begin
  if IsExpired then
    Exit;
  if FileExists(FFileName) then
    FFields.LoadFromFile(FFileName);
end;

procedure TBrookSession.Save;
begin
  if FFileName <> ES then
    FFields.SaveToFile(FFileName);
end;

procedure TBrookSession.Start(ARequest: TBrookRequest);
begin
  if FStarted then
    Exit;
  CheckSID(ARequest);
  CheckFileName;
  FStarted := True;
  Load;
end;

procedure TBrookSession.Finish(AResponse: TBrookResponse);
begin
  if not FStarted then
    Exit;
  CheckCookie(AResponse);
  Save;
  FStarted := False;
end;

procedure TBrookSession.Expire(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
var
  VCookie: TCookie;
begin
  if IsExpired or not FStarted then
    Exit;
  FSID := ARequest.CookieFields.Values[FCookieName];
  if FSID = ES then
    Exit;
  CheckFileName;
  DeleteFile(FFileName);
  VCookie := AResponse.Cookies.Add;
  VCookie.Name := FCookieName;
  VCookie.Expire;
  FFields.Clear;
end;

function TBrookSession.Exists(const AName: string): Boolean;
begin
  Result := FFields.IndexOfName(AName) <> -1;
end;

end.
