(*
  Brook HTTP Utils unit.

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

unit BrookHTTPUtils;

{$i brook.inc}

interface

uses
  BrookHTTPConsts, BrookConsts, HTTPDefs, SysUtils;

type
  { Defines a set to represent the AcceptEncoding HTTP header. }
  TBrookAcceptEncodingSet = set of (aeDeflate, aeGzip, aeSdch, aeXGzip);

{ Converts TDateTime to a GMT format. }
function BrookDateTimeToGMT(const ADateTime: TDateTime): string;
{ Compare two URLs ignoring a possible final slash. }
function BrookSameURL(AURL1, AURL2: string): Boolean;
{ Returns the reason phrase corresponding to a status code. }
function BrookStatusCodeToReasonPhrase(const AStatusCode: Word): string;
{ Returns the status code corresponding to a reason phrase. }
function BrookReasonPhraseToStatusCode(const AReasonPhrase: string): Word;
{ Returns a set of HTTP AceptEnconding header. }
function BrookGetAcceptEncodingSet(
  const AAcceptEncoding: ShortString): TBrookAcceptEncodingSet;
{ Returns a string of HTTP AcceptEnconding. }
function BrookGetAcceptEncoding(
  const AAcceptEncoding: TBrookAcceptEncodingSet): string;
{ Returns a MIME type by file extension. }
function BrookMimeTypeFromFileExt(const AValue: string): string;
{ Returns a MIME type by file name. }
function BrookMimeTypeFromFileName(const AValue: string): string;
{ Returns a file extension by MIME type. }
function BrookFileExtFromMimeType(const AValue: string): string;
{ Extracts the file name of a URL. }
function BrookExtractUrlFileName(const AUrl: string): string;
{ Extracts the file name of a URL and escapes it. }
function BrookExtractUrlFileName(const AUrl: string;
  const AEscapeQueryString: Boolean): string;

implementation

function BrookDateTimeToGMT(const ADateTime: TDateTime): string;
var
  VYear, VMonth, VDay, VHour, VMinute, VSecond, M: Word;
begin
  DecodeDate(ADateTime, VYear, VMonth, VDay);
  DecodeTime(ADateTime, VHour, VMinute, VSecond, M);
  Result := Format(BROOK_GMT_FRMT, [HTTPDays[DayOfWeek(ADateTime)], VDay,
    HTTPMonths[VMonth], VYear, VHour, VMinute, VSecond]);
end;

function BrookSameURL(AURL1, AURL2: string): Boolean;
begin
  AURL1 := IncludeHTTPPathDelimiter(AURL1);
  AURL2 := IncludeHTTPPathDelimiter(AURL2);
  Result := CompareText(AURL1, AURL2) = 0;
end;

function BrookStatusCodeToReasonPhrase(const AStatusCode: Word): string;
begin
  case AStatusCode of
    BROOK_HTTP_STATUS_CODE_CONTINUE:
      Result := BROOK_HTTP_REASON_PHRASE_CONTINUE;
    BROOK_HTTP_STATUS_CODE_SWITCHING_PROTOCOLS:
      Result := BROOK_HTTP_REASON_PHRASE_SWITCHING_PROTOCOLS;
    BROOK_HTTP_STATUS_CODE_OK:
      Result := BROOK_HTTP_REASON_PHRASE_OK;
    BROOK_HTTP_STATUS_CODE_CREATED:
      Result := BROOK_HTTP_REASON_PHRASE_CREATED;
    BROOK_HTTP_STATUS_CODE_ACCEPTED:
      Result := BROOK_HTTP_REASON_PHRASE_CREATED;
    BROOK_HTTP_STATUS_CODE_NON_AUTHORITATIVE_INFORMATION:
      Result := BROOK_HTTP_REASON_PHRASE_NON_AUTHORITATIVE_INFORMATION;
    BROOK_HTTP_STATUS_CODE_NO_CONTENT:
      Result := BROOK_HTTP_REASON_PHRASE_NO_CONTENT;
    BROOK_HTTP_STATUS_CODE_RESET_CONTENT:
      Result := BROOK_HTTP_REASON_PHRASE_RESET_CONTENT;
    BROOK_HTTP_STATUS_CODE_PARTIAL_CONTENT:
      Result := BROOK_HTTP_REASON_PHRASE_PARTIAL_CONTENT;
    BROOK_HTTP_STATUS_CODE_MULTIPLE_CHOICES:
      Result := BROOK_HTTP_REASON_PHRASE_MULTIPLE_CHOICES;
    BROOK_HTTP_STATUS_CODE_MOVED_PERMANENTLY:
      Result := BROOK_HTTP_REASON_PHRASE_MOVED_PERMANENTLY;
    BROOK_HTTP_STATUS_CODE_FOUND:
      Result := BROOK_HTTP_REASON_PHRASE_FOUND;
    BROOK_HTTP_STATUS_CODE_SEE_OTHER:
      Result := BROOK_HTTP_REASON_PHRASE_SEE_OTHER;
    BROOK_HTTP_STATUS_CODE_NOT_MODIFIED:
      Result := BROOK_HTTP_REASON_PHRASE_NOT_MODIFIED;
    BROOK_HTTP_STATUS_CODE_USE_PROXY:
      Result := BROOK_HTTP_REASON_PHRASE_USE_PROXY;
    BROOK_HTTP_STATUS_CODE_TEMPORARY_REDIRECT:
      Result := BROOK_HTTP_REASON_PHRASE_TEMPORARY_REDIRECT;
    BROOK_HTTP_STATUS_CODE_BAD_REQUEST:
      Result := BROOK_HTTP_REASON_PHRASE_BAD_REQUEST;
    BROOK_HTTP_STATUS_CODE_UNAUTHORIZED:
      Result := BROOK_HTTP_REASON_PHRASE_UNAUTHORIZED;
    BROOK_HTTP_STATUS_CODE_PAYMENT_REQUIRED:
      Result := BROOK_HTTP_REASON_PHRASE_PAYMENT_REQUIRED;
    BROOK_HTTP_STATUS_CODE_FORBIDDEN:
      Result := BROOK_HTTP_REASON_PHRASE_FORBIDDEN;
    BROOK_HTTP_STATUS_CODE_NOT_FOUND:
      Result := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
    BROOK_HTTP_STATUS_CODE_METHOD_NOT_ALLOWED:
      Result := BROOK_HTTP_REASON_PHRASE_METHOD_NOT_ALLOWED;
    BROOK_HTTP_STATUS_CODE_NOT_ACCEPTABLE:
      Result := BROOK_HTTP_REASON_PHRASE_NOT_ACCEPTABLE;
    BROOK_HTTP_STATUS_CODE_PROXY_AUTHENTICATION_REQUIRED:
      Result := BROOK_HTTP_REASON_PHRASE_PROXY_AUTHENTICATION_REQUIRED;
    BROOK_HTTP_STATUS_CODE_REQUEST_TIMEOUT:
      Result := BROOK_HTTP_REASON_PHRASE_REQUEST_TIMEOUT;
    BROOK_HTTP_STATUS_CODE_CONFLICT:
      Result := BROOK_HTTP_REASON_PHRASE_CONFLICT;
    BROOK_HTTP_STATUS_CODE_GONE:
      Result := BROOK_HTTP_REASON_PHRASE_GONE;
    BROOK_HTTP_STATUS_CODE_LENGTH_REQUIRED:
      Result := BROOK_HTTP_REASON_PHRASE_LENGTH_REQUIRED;
    BROOK_HTTP_STATUS_CODE_PRECONDITION_FAILED:
      Result := BROOK_HTTP_REASON_PHRASE_PRECONDITION_FAILED;
    BROOK_HTTP_STATUS_CODE_REQUEST_ENTITY_TOO_LARGE:
      Result := BROOK_HTTP_REASON_PHRASE_REQUEST_ENTITY_TOO_LARGE;
    BROOK_HTTP_STATUS_CODE_REQUEST_URI_TOO_LONG:
      Result := BROOK_HTTP_REASON_PHRASE_REQUEST_URI_TOO_LONG;
    BROOK_HTTP_STATUS_CODE_UNSUPPORTED_MEDIA_TYPE:
      Result := BROOK_HTTP_REASON_PHRASE_UNSUPPORTED_MEDIA_TYPE;
    BROOK_HTTP_STATUS_CODE_REQUESTED_RANGE_NOT_SATISFIABLE:
      Result := BROOK_HTTP_REASON_PHRASE_REQUESTED_RANGE_NOT_SATISFIABLE;
    BROOK_HTTP_STATUS_CODE_EXPECTATION_FAILED:
      Result := BROOK_HTTP_REASON_PHRASE_EXPECTATION_FAILED;
    BROOK_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR:
      Result := BROOK_HTTP_REASON_PHRASE_INTERNAL_SERVER_ERROR;
    BROOK_HTTP_STATUS_CODE_NOT_IMPLEMENTED:
      Result := BROOK_HTTP_REASON_PHRASE_NOT_IMPLEMENTED;
    BROOK_HTTP_STATUS_CODE_BAD_GATEWAY:
      Result := BROOK_HTTP_REASON_PHRASE_BAD_GATEWAY;
    BROOK_HTTP_STATUS_CODE_SERVICE_UNAVAILABLE:
      Result := BROOK_HTTP_REASON_PHRASE_SERVICE_UNAVAILABLE;
    BROOK_HTTP_STATUS_CODE_GATEWAY_TIMEOUT:
      Result := BROOK_HTTP_REASON_PHRASE_GATEWAY_TIMEOUT;
    BROOK_HTTP_STATUS_CODE_HTTP_VERSION_NOT_SUPPORTED:
      Result := BROOK_HTTP_REASON_PHRASE_HTTP_VERSION_NOT_SUPPORTED;
  end;
end;

function BrookReasonPhraseToStatusCode(const AReasonPhrase: string): Word;
begin
  case AReasonPhrase of
    BROOK_HTTP_REASON_PHRASE_CONTINUE:
      Result := BROOK_HTTP_STATUS_CODE_CONTINUE;
    BROOK_HTTP_REASON_PHRASE_SWITCHING_PROTOCOLS:
      Result := BROOK_HTTP_STATUS_CODE_SWITCHING_PROTOCOLS;
    BROOK_HTTP_REASON_PHRASE_OK:
      Result := BROOK_HTTP_STATUS_CODE_OK;
    BROOK_HTTP_REASON_PHRASE_CREATED:
      Result := BROOK_HTTP_STATUS_CODE_CREATED;
    BROOK_HTTP_REASON_PHRASE_ACCEPTED:
      Result := BROOK_HTTP_STATUS_CODE_CREATED;
    BROOK_HTTP_REASON_PHRASE_NON_AUTHORITATIVE_INFORMATION:
      Result := BROOK_HTTP_STATUS_CODE_NON_AUTHORITATIVE_INFORMATION;
    BROOK_HTTP_REASON_PHRASE_NO_CONTENT:
      Result := BROOK_HTTP_STATUS_CODE_NO_CONTENT;
    BROOK_HTTP_REASON_PHRASE_RESET_CONTENT:
      Result := BROOK_HTTP_STATUS_CODE_RESET_CONTENT;
    BROOK_HTTP_REASON_PHRASE_PARTIAL_CONTENT:
      Result := BROOK_HTTP_STATUS_CODE_PARTIAL_CONTENT;
    BROOK_HTTP_REASON_PHRASE_MULTIPLE_CHOICES:
      Result := BROOK_HTTP_STATUS_CODE_MULTIPLE_CHOICES;
    BROOK_HTTP_REASON_PHRASE_MOVED_PERMANENTLY:
      Result := BROOK_HTTP_STATUS_CODE_MOVED_PERMANENTLY;
    BROOK_HTTP_REASON_PHRASE_FOUND:
      Result := BROOK_HTTP_STATUS_CODE_FOUND;
    BROOK_HTTP_REASON_PHRASE_SEE_OTHER:
      Result := BROOK_HTTP_STATUS_CODE_SEE_OTHER;
    BROOK_HTTP_REASON_PHRASE_NOT_MODIFIED:
      Result := BROOK_HTTP_STATUS_CODE_NOT_MODIFIED;
    BROOK_HTTP_REASON_PHRASE_USE_PROXY:
      Result := BROOK_HTTP_STATUS_CODE_USE_PROXY;
    BROOK_HTTP_REASON_PHRASE_TEMPORARY_REDIRECT:
      Result := BROOK_HTTP_STATUS_CODE_TEMPORARY_REDIRECT;
    BROOK_HTTP_REASON_PHRASE_BAD_REQUEST:
      Result := BROOK_HTTP_STATUS_CODE_BAD_REQUEST;
    BROOK_HTTP_REASON_PHRASE_UNAUTHORIZED:
      Result := BROOK_HTTP_STATUS_CODE_UNAUTHORIZED;
    BROOK_HTTP_REASON_PHRASE_PAYMENT_REQUIRED:
      Result := BROOK_HTTP_STATUS_CODE_PAYMENT_REQUIRED;
    BROOK_HTTP_REASON_PHRASE_FORBIDDEN:
      Result := BROOK_HTTP_STATUS_CODE_FORBIDDEN;
    BROOK_HTTP_REASON_PHRASE_NOT_FOUND:
      Result := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
    BROOK_HTTP_REASON_PHRASE_METHOD_NOT_ALLOWED:
      Result := BROOK_HTTP_STATUS_CODE_METHOD_NOT_ALLOWED;
    BROOK_HTTP_REASON_PHRASE_NOT_ACCEPTABLE:
      Result := BROOK_HTTP_STATUS_CODE_NOT_ACCEPTABLE;
    BROOK_HTTP_REASON_PHRASE_PROXY_AUTHENTICATION_REQUIRED:
      Result := BROOK_HTTP_STATUS_CODE_PROXY_AUTHENTICATION_REQUIRED;
    BROOK_HTTP_REASON_PHRASE_REQUEST_TIMEOUT:
      Result := BROOK_HTTP_STATUS_CODE_REQUEST_TIMEOUT;
    BROOK_HTTP_REASON_PHRASE_CONFLICT:
      Result := BROOK_HTTP_STATUS_CODE_CONFLICT;
    BROOK_HTTP_REASON_PHRASE_GONE:
      Result := BROOK_HTTP_STATUS_CODE_GONE;
    BROOK_HTTP_REASON_PHRASE_LENGTH_REQUIRED:
      Result := BROOK_HTTP_STATUS_CODE_LENGTH_REQUIRED;
    BROOK_HTTP_REASON_PHRASE_PRECONDITION_FAILED:
      Result := BROOK_HTTP_STATUS_CODE_PRECONDITION_FAILED;
    BROOK_HTTP_REASON_PHRASE_REQUEST_ENTITY_TOO_LARGE:
      Result := BROOK_HTTP_STATUS_CODE_REQUEST_ENTITY_TOO_LARGE;
    BROOK_HTTP_REASON_PHRASE_REQUEST_URI_TOO_LONG:
      Result := BROOK_HTTP_STATUS_CODE_REQUEST_URI_TOO_LONG;
    BROOK_HTTP_REASON_PHRASE_UNSUPPORTED_MEDIA_TYPE:
      Result := BROOK_HTTP_STATUS_CODE_UNSUPPORTED_MEDIA_TYPE;
    BROOK_HTTP_REASON_PHRASE_REQUESTED_RANGE_NOT_SATISFIABLE:
      Result := BROOK_HTTP_STATUS_CODE_REQUESTED_RANGE_NOT_SATISFIABLE;
    BROOK_HTTP_REASON_PHRASE_EXPECTATION_FAILED:
      Result := BROOK_HTTP_STATUS_CODE_EXPECTATION_FAILED;
    BROOK_HTTP_REASON_PHRASE_INTERNAL_SERVER_ERROR:
      Result := BROOK_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
    BROOK_HTTP_REASON_PHRASE_NOT_IMPLEMENTED:
      Result := BROOK_HTTP_STATUS_CODE_NOT_IMPLEMENTED;
    BROOK_HTTP_REASON_PHRASE_BAD_GATEWAY:
      Result := BROOK_HTTP_STATUS_CODE_BAD_GATEWAY;
    BROOK_HTTP_REASON_PHRASE_SERVICE_UNAVAILABLE:
      Result := BROOK_HTTP_STATUS_CODE_SERVICE_UNAVAILABLE;
    BROOK_HTTP_REASON_PHRASE_GATEWAY_TIMEOUT:
      Result := BROOK_HTTP_STATUS_CODE_GATEWAY_TIMEOUT;
    BROOK_HTTP_REASON_PHRASE_HTTP_VERSION_NOT_SUPPORTED:
      Result := BROOK_HTTP_STATUS_CODE_HTTP_VERSION_NOT_SUPPORTED;
  end;
end;

function BrookGetAcceptEncodingSet(
  const AAcceptEncoding: ShortString): TBrookAcceptEncodingSet;
var
  S: ShortString;
begin
  Result := [];
  S := LowerCase(AAcceptEncoding);
  if Pos(BROOK_HTTP_CONTENT_ENCODING_GZIP, S) <> 0 then
    Include(Result, aeGzip);
  if Pos(BROOK_HTTP_CONTENT_ENCODING_DEFLATE, S) <> 0 then
    Include(Result, aeDeflate);
  if Pos(BROOK_HTTP_CONTENT_ENCODING_X_GZIP, S) <> 0 then
    Include(Result, aeXGzip);
  if Pos(BROOK_HTTP_CONTENT_ENCODING_SDCH, S) <> 0 then
    Include(Result, aeSdch);
end;

function BrookGetAcceptEncoding(
  const AAcceptEncoding: TBrookAcceptEncodingSet): string;
begin
  Result := ES;
  if aeDeflate in AAcceptEncoding then
    Result += 'deflate,';
  if aeGzip in AAcceptEncoding then
    Result += 'gzip,';
  if aeSdch in AAcceptEncoding then
    Result += 'sdch,';
  if aeXGzip in AAcceptEncoding then
    Result += 'xgzip,';
  SetLength(Result, Length(Result) - 1);
end;

function BrookMimeTypeFromFileExt(const AValue: string): string;
var
  I: Integer;
begin
  for I := 0 to BROOK_MAX_MIME_TYPE do
    if SameText(BROOK_MIME_TYPE[I, 2], AValue) then
    begin
      Result := BROOK_MIME_TYPE[I, 1];
      Exit;
    end;
  Result := BROOK_HTTP_CONTENT_TYPE_APP_OCTET_STREAM;
end;

function BrookMimeTypeFromFileName(const AValue: string): string;
begin
  Result := BrookMimeTypeFromFileExt(ExtractFileExt(AValue));
end;

function BrookFileExtFromMimeType(const AValue: string): string;
var
  I: Integer;
begin
  for I := 0 to BROOK_MAX_MIME_TYPE do
    if SameText(BROOK_MIME_TYPE[I, 1], AValue) then
    begin
      Result := BROOK_MIME_TYPE[I, 2];
      Exit;
    end;
  Result := BROOK_HTTP_CONTENT_TYPE_APP_OCTET_STREAM;
end;

function BrookExtractUrlFileName(const AUrl: string): string;
var
  I: Integer;
begin
  Result := ES;
  I := Length(AUrl);
  repeat
    Result := AUrl[I] + Result;
    Dec(I);
  until (AUrl[I] = US) or (I = 0);
end;

function BrookExtractUrlFileName(const AUrl: string;
  const AEscapeQueryString: Boolean): string;
var
  I: Integer = -1;
begin
  Result := ES;
  if AEscapeQueryString then
    I := Pred(Pos(QU, AUrl));
  if I < 0 then
    I := Length(AUrl);
  repeat
    Result := AUrl[I] + Result;
    Dec(I);
  until (AUrl[I] = US) or (I = 0);
end;

end.
