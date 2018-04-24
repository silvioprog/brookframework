unit BrookHTTPResponse;

{$I Brook.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes,
  Platform,
  Marshalling,
  libbrook,
  BrookHandledClasses,
  BrookString,
  BrookStringMap,
  BrookHTTPExtra,
  BrookHTTPUploads;

resourcestring
  SBrookInvalidHTTPStatus = 'Invalid status code: %d';

type
  TBrookHTTPResponse = class(TBrookHandledPersistent)
  private
    FHeaders: TBrookStringMap;
    FHandle: Pbk_httpres;
    FUploads: TBrookHTTPUploads;
  protected
    class function DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t; Abuf: Pcchar;
      Asize: csize_t): cssize_t; cdecl; static;
    class procedure DoStreamFree(Acls: Pcvoid); cdecl; static;
    class procedure CheckStatus(AStatus: Word); static; inline;
    class procedure CheckStream(AStream: TStream); static; inline;
    function CreateHeaders(AHandle: Pointer): TBrookStringMap; virtual;
    function GetHandle: Pointer; override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    function SetCookie(const AName, AValue: string): Boolean; virtual;
    function Send(const AValue, AContentType: string;
      AStatus: Word): Boolean; overload; virtual;
    function Send(const AFmt: string; const AArgs: array of const;
      const AContentType: string; AStatus: Word): Boolean; overload; virtual;
    function Send(ABuffer: Pointer; ASize: NativeUInt;
      const AContentType: string; AStatus: Word): Boolean; overload; virtual;
    function Send(const ABytes: TBytes; ASize: NativeUInt;
      const AContentType: string; AStatus: Word): Boolean; overload; virtual;
    function Send(AString: TBrookString; const AContentType: string;
      AStatus: Word): Boolean; overload; virtual;
    function TrySendFile(ABlockSize: NativeUInt; AMaxSize: UInt64;
      const AFileName: TFileName; ARendered: Boolean; AStatus: Word;
      out AFailed: Boolean): Boolean; overload; virtual;
    function SendFile(ABlockSize: NativeUInt; AMaxSize: UInt64;
      const AFileName: TFileName; ARendered: Boolean;
      AStatus: Word): Boolean; overload; virtual;
    function SendFile(const AFileName: TFileName): Boolean; overload; virtual;
    function SendStream(AStream: TStream; AStatus: Word): Boolean; virtual;
    function SendData(AStream: TStream; AStatus: Word): Boolean; virtual;
    property Headers: TBrookStringMap read FHeaders;
    property Uploads: TBrookHTTPUploads read FUploads;
  end;

implementation

constructor TBrookHTTPResponse.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FHeaders := CreateHeaders(bk_httpres_headers(FHandle));
end;

destructor TBrookHTTPResponse.Destroy;
begin
  FHeaders.Free;
  inherited Destroy;
end;

function TBrookHTTPResponse.GetHandle: Pointer;
begin
  Result := FHandle;
end;

class procedure TBrookHTTPResponse.CheckStatus(AStatus: Word);
begin
  if (AStatus < 100) or (AStatus > 599) then
    raise EArgumentException.CreateResFmt(@SBrookInvalidHTTPStatus, [AStatus]);
end;

class procedure TBrookHTTPResponse.CheckStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AStream']);
end;

function TBrookHTTPResponse.CreateHeaders(AHandle: Pointer): TBrookStringMap;
begin
  Result := TBrookStringMap.Create(AHandle);
  Result.ClearOnDestroy := False;
end;

{$IFDEF FPC}
 {$PUSH}{$WARN 5024 OFF}
{$ENDIF}
class function TBrookHTTPResponse.DoStreamRead(Acls: Pcvoid;
  Aoffset: cuint64_t; Abuf: Pcchar; Asize: csize_t): cssize_t; cdecl;
begin
  Result := TStream(Acls).Read(Abuf^, Asize);
  if Result = 0 then
    Exit(bk_httpread_end(False));
  if Result = -1 then
    Result := bk_httpread_end(True);
end;
{$IFDEF FPC}
 {$POP}
{$ENDIF}

class procedure TBrookHTTPResponse.DoStreamFree(Acls: Pcvoid); cdecl;
begin
  TStream(Acls).Free;
end;

function TBrookHTTPResponse.SetCookie(const AName, AValue: string): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  BkCheckLibrary;
  R := -bk_httpres_set_cookie(FHandle, M.ToCString(AName), M.ToCString(AValue));
  Result := R = 0;
  if (not Result) and (R <> EINVAL) then
    BkCheckLastError(R);
end;

function TBrookHTTPResponse.Send(const AValue, AContentType: string;
  AStatus: Word): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_send(FHandle, M.ToCString(AValue),
    M.ToCString(AContentType), AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    BkCheckLastError(R);
end;

function TBrookHTTPResponse.Send(const AFmt: string;
  const AArgs: array of const; const AContentType: string;
  AStatus: Word): Boolean;
begin
  Result := Send(Format(AFmt, AArgs), AContentType, AStatus);
end;

function TBrookHTTPResponse.Send(ABuffer: Pointer; ASize: NativeUInt;
  const AContentType: string; AStatus: Word): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendbinary(FHandle, ABuffer, ASize,
    M.ToCString(AContentType), AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    BkCheckLastError(R);
end;

function TBrookHTTPResponse.Send(const ABytes: TBytes; ASize: NativeUInt;
  const AContentType: string; AStatus: Word): Boolean;
begin
  Result := Send(@ABytes[0], ASize, AContentType, AStatus);
end;

function TBrookHTTPResponse.Send(AString: TBrookString;
  const AContentType: string; AStatus: Word): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendstr(FHandle, AString.Handle,
    M.ToCString(AContentType), AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    BkCheckLastError(R);
end;

function TBrookHTTPResponse.TrySendFile(ABlockSize: NativeUInt;
  AMaxSize: UInt64; const AFileName: TFileName; ARendered: Boolean;
  AStatus: Word; out AFailed: Boolean): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendfile(FHandle, ABlockSize, AMaxSize, M.ToCString(AFileName),
    ARendered, AStatus);
  Result := R = 0;
  if not Result then
  begin
    AFailed := R = ENOENT;
    if (not AFailed) and (R <> EALREADY) then
      BkCheckLastError(R);
  end;
end;

function TBrookHTTPResponse.SendFile(ABlockSize: NativeUInt; AMaxSize: UInt64;
  const AFileName: TFileName; ARendered: Boolean; AStatus: Word): Boolean;
begin
  if not TrySendFile(ABlockSize, AMaxSize, AFileName, ARendered,
    AStatus, Result) then
    raise EFileNotFoundException.CreateResFmt(@SFOpenError, [AFileName]);
end;

function TBrookHTTPResponse.SendFile(const AFileName: TFileName): Boolean;
begin
  Result := SendFile(BROOK_BLOCK_SIZE, 0, AFileName, False, 200);
end;

function TBrookHTTPResponse.SendStream(AStream: TStream;
  AStatus: Word): Boolean;
var
  R: cint;
begin
  CheckStream(AStream);
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_sendstream(FHandle, AStream.Size, BROOK_BLOCK_SIZE,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamRead, AStream,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamFree, AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    BkCheckLastError(R);
end;

function TBrookHTTPResponse.SendData(AStream: TStream; AStatus: Word): Boolean;
var
  R: cint;
begin
  CheckStream(AStream);
  CheckStatus(AStatus);
  BkCheckLibrary;
  R := -bk_httpres_senddata(FHandle, BROOK_BLOCK_SIZE,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamRead, AStream,
{$IFNDEF VER3_0}@{$ENDIF}DoStreamFree, AStatus);
  Result := R = 0;
  if (not Result) and (R <> EALREADY) then
    BkCheckLastError(R);
end;

end.
