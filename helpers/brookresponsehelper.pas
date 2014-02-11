(*
  Brook Response Helper unit.

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

{$WARNING This unit will be discontinued!}

unit BrookResponseHelper;

{$i brook.inc}

interface

uses
  BrookHttpDefs, BrookMessages, BrookConsts, BrookHTTPConsts, BrookHTTPUtils,
  BrookException, Classes, SysUtils;

type
  { Handles exceptions for @link(TBrookResponseHelper). }
  EBrookResponseHelper = class(EBrook);

  { Adds features to @code(TBrookResponse) class. }
  TBrookResponseHelper = class helper for TBrookResponse
  private
    function GetHeader(AName: string): string;
    function GetStatus: Integer;
    procedure SetHeader(AName: string; AValue: string);
    procedure SetStatus(AValue: Integer);
  public
    { Is this a empty response? }
    function IsEmpty: Boolean;
    { Is this an informational response? }
    function IsInformational: Boolean;
    { Is this a 200 OK response? }
    function IsOK: Boolean;
    { Is this a 2xx successful response? }
    function IsSuccessful: Boolean;
    { Is this a 3xx redirection response? }
    function IsRedirection: Boolean;
    { Is this a specific redirect response? (301, 302, 303, 307) }
    function IsRedirect: Boolean;
    { Is this a forbidden response? }
    function IsForbidden: Boolean;
    { Is this a 404 not found response? }
    function IsNotFound: Boolean;
    { Is this a client error response? }
    function IsClientError: Boolean;
    { Is this a server error response? }
    function IsServerError: Boolean;
    { Defines a content disposition for a file. }
    procedure AddContentDisposition(const AContentType: ShortString;
      const AFileName: TFileName = ES;
      const ADispositionType: ShortString = BROOK_HTTP_CONTENT_DISPOSITION_ATTACHMENT;
      const AContentDescription: ShortString = ES;
      const AModificationDate: TDateTime = NullDate);
    { Get or set a HTTP header. }
    property Header[AName: string]: string read GetHeader write SetHeader;
    { Get or set a HTTP status code. }
    property Status: Integer read GetStatus write SetStatus;
  end;

implementation

function TBrookResponseHelper.IsEmpty: Boolean;
begin
  Result := (Code = 201) or (Code = 204) or (Code = 304);
end;

function TBrookResponseHelper.IsInformational: Boolean;
begin
  Result := (Code >= 100) and (Code < 200);
end;

function TBrookResponseHelper.IsOK: Boolean;
begin
  Result := Code = 200;
end;

function TBrookResponseHelper.IsSuccessful: Boolean;
begin
  Result := (Code >= 200) and (Code < 300);
end;

function TBrookResponseHelper.IsRedirection: Boolean;
begin
  Result := (Code >= 300) and (Code < 400);
end;

function TBrookResponseHelper.IsRedirect: Boolean;
begin
  Result := (Code = 301) or (Code = 302) or (Code = 303) or (Code = 307);
end;

function TBrookResponseHelper.IsForbidden: Boolean;
begin
  Result := Code = 403;
end;

function TBrookResponseHelper.IsNotFound: Boolean;
begin
  Result := Code = 404;
end;

function TBrookResponseHelper.IsClientError: Boolean;
begin
  Result := (Code >= 400) and (Code < 500);
end;

function TBrookResponseHelper.IsServerError: Boolean;
begin
  Result := (Code >= 500) and (Code < 600);
end;

procedure TBrookResponseHelper.AddContentDisposition(
  const AContentType: ShortString; const AFileName: TFileName;
  const ADispositionType: ShortString; const AContentDescription: ShortString;
  const AModificationDate: TDateTime);
var
  VHeaders: string;
begin
  if not FileExists(AFileName) then
    EBrookResponseHelper.Create(SBrookFileNotFoundError);
  ContentType := ES;
  VHeaders := 'Content-Type' + CO + SP + AContentType + CRLF +
    BROOK_HTTP_HEADER_CONTENT_DISPOSITION + CO + SP + ADispositionType;
  if AFileName <> ES then
  begin
    VHeaders += '; filename=' + DQ + ExtractFileName(AFileName) + DQ;
    if AModificationDate <> NullDate then
      VHeaders += '; modification-date=' + DQ +
        BrookDateTimeToGMT(AModificationDate) + DQ;
    if AContentDescription <> ES then
      VHeaders += CRLF + BROOK_HTTP_HEADER_CONTENT_DESCRIPTION +
        AContentDescription;
  end;
  CustomHeaders.NameValueSeparator := CO;
  CustomHeaders.Add(VHeaders);
  if FileExists(AFileName) then
  begin
    ContentStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      SendContent;
    finally
      ContentStream.Free;
    end;
  end;
end;

function TBrookResponseHelper.GetStatus: Integer;
begin
  Result := Code;
end;

function TBrookResponseHelper.GetHeader(AName: string): string;
begin
  Result := GetCustomHeader(AName);
end;

procedure TBrookResponseHelper.SetHeader(AName: string; AValue: string);
begin
  SetCustomHeader(AName, AValue);
end;

procedure TBrookResponseHelper.SetStatus(AValue: Integer);
begin
  Code := AValue;
  CodeText := BrookStatusCodeToReasonPhrase(AValue);
end;

end.
