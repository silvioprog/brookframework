(*
  Brook Session unit.

  Copyright (C) 2012 Silvio Clecio.

  http://brookframework.org

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
  BrookClasses, BrookUtils, BrookException, BrookConsts, FPJSON, JSONParser,
  Classes, SysUtils, HTTPDefs, DateUtils;

type
  { Handles exceptions for @link(TBrookSession). }
  EBrookSession = class(EBrook);

  { Is a metaclass for @link(TBrookSession) class. }
  TBrookSessionClass = class of TBrookSession;

  { Defines features to the section handling. }
  TBrookSession = class(TBrookObject)
  private
    FCookieName: string;
    FData: TMemoryStream;
    FDirectory: string;
    FFields: TJSONObject;
    FFileName: TFileName;
    FFilePrefix: ShortString;
    FHttpOnly: Boolean;
    FSID: string;
    FStarted: Boolean;
    FTimeOut: Integer;
  protected
    function CreateFields: TJSONObject; virtual;
    function IsStarted: Boolean;
    procedure CheckSID(ARequest: TRequest); virtual;
    procedure CheckFileName; virtual;
    procedure CheckCookie(AResponse: TResponse); virtual;
    procedure Load; virtual;
    procedure Save; virtual;
    property Data: TMemoryStream read FData;
  public
    { Creates an instance of a @link(TBrookSession) class. }
    constructor Create; virtual;
    { Frees an instance of @link(TBrookSession) class. }
    destructor Destroy; override;
    { Returns @code(True) if the session has expired.}
    function IsExpired: Boolean; virtual;
    { Creates an ID for the session. }
    function GenerateID: string; virtual;
    { Starts the session. }
    procedure Start(ARequest: TRequest); virtual;
    { Terminates the session. }
    procedure Finish(AResponse: TResponse); virtual;
    { Expires the session. }
    procedure Expire(ARequest: TRequest; AResponse: TResponse); virtual;
    { Deletes the session files. }
    procedure DeleteFiles;
    { Checks if a name exists in fields. }
    function Exists(const AName: string): Boolean;
    { Deletes the session files except for the current session file. }
    procedure DeleteOldFiles(const ABeforeOf: TDateTime);
    { Set the session cookie name. }
    property CookieName: string read FCookieName write FCookieName;
    { Set the name of directory session. }
    property Directory: string read FDirectory write FDirectory;
    { Returns @code(True) if the session has expired.}
    property Expired: Boolean read IsExpired;
    { Get the ID session. }
    property SID: string read FSID;
    { Checks if the session has started. }
    property Started: Boolean read IsStarted;
    { The session file name. }
    property FileName: TFileName read FFileName write FFileName;
    { The session file prefix. }
    property FilePrefix: ShortString read FFilePrefix write FFilePrefix;
    { The fields session. }
    property Fields: TJSONObject read FFields;
    { The remaining seconds for the session end. }
    property TimeOut: Integer read FTimeOut write FTimeOut;
    { Informs if the session cookie is accessible only by HTTP requests,
      if @code(True), the JavaScript access is not allowed. }
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
  end;

implementation

constructor TBrookSession.Create;
begin
  FData := TMemoryStream.Create;
  FFields := CreateFields;
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
  FreeAndNil(FFields);
  FreeAndNil(FData);
  inherited Destroy;
end;

function TBrookSession.CreateFields: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function TBrookSession.IsExpired: Boolean;
begin
  Result := FTimeOut <> 0;
  if Result then
    Result := FileExists(FFileName) and
      (IncSecond(BrookFileDate(FFileName), FTimeOut) < Now);
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

procedure TBrookSession.CheckSID(ARequest: TRequest);
begin
  FSID := ARequest.CookieFields.Values[FCookieName];
  if FSID = ES then
    FSID := GenerateID;
end;

procedure TBrookSession.CheckFileName;
begin
  FFileName := IncludeTrailingPathDelimiter(FDirectory) + FFilePrefix + FSID;
end;

procedure TBrookSession.CheckCookie(AResponse: TResponse);
var
  VCookie: TCookie;
begin
  VCookie := AResponse.Cookies.FindCookie(FCookieName);
  if not Assigned(VCookie) then
  begin
    VCookie := AResponse.Cookies.Add;
    VCookie.Name := FCookieName;
    VCookie.HttpOnly := FHttpOnly;
  end;
  VCookie.Value := SID;
end;

procedure TBrookSession.Load;
var
  I: Integer;
  VParser: TJSONParser;
  VJSON: TJSONObject = nil;
begin
  if IsExpired then
    Exit;
  if FileExists(FFileName) then
    FData.LoadFromFile(FFileName)
  else
    Exit;
  FData.Position := 0;
  VParser := TJSONParser.Create(FData);
  try
    FFields.Clear;
    VJSON := VParser.Parse as TJSONObject;
    if Assigned(VJSON) then
      for I := 0 to Pred(VJSON.Count) do
        FFields.Add(VJSON.Names[I], VJSON.Items[I].Clone);
  finally
    VJSON.Free;
    VParser.Free;
  end;
end;

procedure TBrookSession.Save;
var
  VData: TJSONStringType;
begin
  VData := FFields.AsJSON;
  FData.Clear;
  FData.Write(Pointer(VData)^, Length(VData));
  if FFileName <> ES then
    FData.SaveToFile(FFileName);
end;

procedure TBrookSession.Start(ARequest: TRequest);
begin
  if FStarted then
    Exit;
  CheckSID(ARequest);
  CheckFileName;
  FStarted := True;
  Load;
end;

procedure TBrookSession.Finish(AResponse: TResponse);
begin
  if not FStarted then
    Exit;
  CheckCookie(AResponse);
  Save;
  FStarted := False;
  FFields.Clear;
end;

procedure TBrookSession.Expire(ARequest: TRequest; AResponse: TResponse);
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

procedure TBrookSession.DeleteFiles;
begin
  CheckFileName;
  BrookDeleteFiles(FDirectory, NullDate, ES, BROOK_SESS_PREFIX);
end;

function TBrookSession.Exists(const AName: string): Boolean;
begin
  Result := FFields.IndexOfName(AName, True) <> -1;
end;

procedure TBrookSession.DeleteOldFiles(const ABeforeOf: TDateTime);
begin
  CheckFileName;
  BrookDeleteFiles(FDirectory, ABeforeOf, FFileName, BROOK_SESS_PREFIX);
end;

end.
