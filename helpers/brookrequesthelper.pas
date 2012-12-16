(*
  Brook Request Helper unit.

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

unit BrookRequestHelper;

{$i brook.inc}

interface

uses
  BrookUtils, BrookConsts, BrookHTTPConsts, HttpDefs, SysUtils, Classes,
  CustWeb;

type
  { Adds features to @code(TRequest) class. }
  TBrookRequestHelper = class helper for TRequest
  public
    { Set the URL scheme (HTTP or HTTPs) in @code(GetFullURL) function. }
    class procedure SetScheme(const AScheme: ShortString);
    { Get the current URL scheme (HTTP or HTTPs). }
    class function GetScheme: ShortString;
    { Get the path level passing the respective index. Exemple:

      @code(BrookGetPathLavel('/a/b/c/', 1)) = b. }
    function GetPath(const AIndex: Integer): string;
    { Checks if the request was done under the GET method. }
    function IsGet: Boolean;
    { Checks if the request was done under the POST method. }
    function IsPost: Boolean;
    { Checks if the request was done under the PUT method. }
    function IsPut: Boolean;
    { Checks if the request was done under the DELETE method. }
    function IsDelete: Boolean;
    { Checks if the request was done under the HEAD method. }
    function IsHead: Boolean;
    { Checks if the request was done under the OPTIONS method. }
    function IsOptions: Boolean;
    { Checks if the request was done by means of Ajax. }
    function IsAjax: Boolean;
    { Checks if the request was done by means of Xhr. }
    function IsXhr: Boolean;
    { Checks if the request was done by means of a HTML form. }
    function IsForm: Boolean;
    { Checks if the request was done by means of a HTML data form. }
    function IsFormData: Boolean;
    { Get X-HTTP-Method-Override header }
    function OverriddenMethod: ShortString;
    { Get the URL and the port separated by a ":", exemple: http://localhost:80 }
    function GetHostWithPort: string;
    { Get the current path. }
    function GetPath: string;
    { Get the full URL, schema included. }
    function GetFullURL: string;
    { Get the full URL, schema and script name included. }
    function GetURLWithScriptName: string;
    { Get the IP of the requester. }
    function GetIP: ShortString;
    { Get the server path where HTML documents can be placed. }
    function GetDocumentRoot: string;
    { Get a environment variable given its name. }
    function Variable(const AName: string): string;
    { Get a list with all environment variables. }
    procedure Variables(out AValues: TStrings);
    { Get the current path, script name and pathinfo included. }
    property Path[const AIndex: Integer]: string read GetPath;
  end;

implementation

var
  _BrookDefaultScheme: ShortString = 'http';

class procedure TBrookRequestHelper.SetScheme(const AScheme: ShortString);
begin
  _BrookDefaultScheme := AScheme;
end;

class function TBrookRequestHelper.GetScheme: ShortString;
begin
  Result := _BrookDefaultScheme;
end;

function TBrookRequestHelper.GetPath(const AIndex: Integer): string;
begin
  Result := BrookGetPathLevel(PathInfo, AIndex);
end;

function TBrookRequestHelper.IsGet: Boolean;
begin
  Result := Method = BROOK_HTTP_REQUEST_METHOD_GET;
end;

function TBrookRequestHelper.IsPost: Boolean;
begin
  Result := Method = BROOK_HTTP_REQUEST_METHOD_POST;
end;

function TBrookRequestHelper.IsPut: Boolean;
begin
  Result := Method = BROOK_HTTP_REQUEST_METHOD_PUT;
end;

function TBrookRequestHelper.IsDelete: Boolean;
begin
  Result := Method = BROOK_HTTP_REQUEST_METHOD_DELETE;
end;

function TBrookRequestHelper.IsHead: Boolean;
begin
  Result := Method = BROOK_HTTP_REQUEST_METHOD_HEAD;
end;

function TBrookRequestHelper.IsOptions: Boolean;
begin
  Result := Method = BROOK_HTTP_REQUEST_METHOD_OPTIONS;
end;

function TBrookRequestHelper.IsAjax: Boolean;
begin
  Result := (ContentFields.Values['isajax'] <> ES) or
    (GetEnvironmentVariable(BROOK_CLT_ENV_HTTP_X_REQUESTED_WITH) = 'XMLHttpRequest');
end;

function TBrookRequestHelper.IsXhr: Boolean;
begin
  Result := IsAjax;
end;

function TBrookRequestHelper.IsForm: Boolean;
begin
  Result := (IsPost or IsPut) and
    (ContentType = BROOK_HTTP_CONTENT_TYPE_APP_X_WWW_FORM_URLENCODED);
end;

function TBrookRequestHelper.IsFormData: Boolean;
begin
  Result := (IsPost or IsPut) and
    (ContentType = BROOK_HTTP_CONTENT_TYPE_MULTIPART_FORM_DATA);
end;

function TBrookRequestHelper.OverriddenMethod: ShortString;
begin
  Result := ContentFields.Values[BROOK_OVERRIDDEN_METHOD];
  if Result = ES then
    Result := GetEnvironmentVariable('X-HTTP-Method-Override');
end;

function TBrookRequestHelper.GetHostWithPort: string;
begin
  Result := Host + CO + IntToStr(ServerPort);
end;

function TBrookRequestHelper.GetPath: string;
begin
  Result := ScriptName + PathInfo;
end;

function TBrookRequestHelper.GetFullURL: string;
var
  S: string;
begin
  S := GetScheme;
  Result := S + '://' + Host;
  if ((S = 'https') and (ServerPort <> 443 )) or
    ((S = 'http') and (ServerPort <> 80 )) then
    Result += CO + IntToStr(ServerPort);
end;

function TBrookRequestHelper.GetURLWithScriptName: string;
begin
  Result := GetFullURL + ScriptName;
end;

function TBrookRequestHelper.GetIP: ShortString;
begin
  Result := GetEnvironmentVariable(BROOK_SRV_ENV_X_FORWARDED_FOR);
  if Result = ES then
  begin
    Result := GetEnvironmentVariable('CLIENT_IP');
    if Result = ES then
      Result := GetEnvironmentVariable(BROOK_SRV_ENV_REMOTE_ADDR);
  end;
end;

function TBrookRequestHelper.GetDocumentRoot: string;
{$IFDEF MSWINDOWS}
var
  P: PChar;
{$ENDIF}
begin
  Result := GetEnvironmentVariable(BROOK_SRV_ENV_DOCUMENT_ROOT);
{$IFDEF MSWINDOWS}
  if not Assigned(Pointer(Result)) then
    Exit;
  P := PChar(Result);
  while P^ <> NU do
  begin
    if P^ = US then
      P^ := DS;
    Inc(P);
  end;
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result);
end;

function TBrookRequestHelper.Variable(const AName: string): string;
begin
  Result := GetEnvironmentVariable(AName);
end;

procedure TBrookRequestHelper.Variables(out AValues: TStrings);
var
 I: Integer;
begin
  AValues := TStringList.Create;
  AValues.Clear;
  for I := 1 to CGIVarCount do
    AValues.Add(CGIVarNames[I] + EQ + GetEnvironmentVariable(CGIVarNames[I]));
end;

end.
