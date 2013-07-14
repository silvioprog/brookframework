(*
  Brook FCL CGI Broker unit.

  Copyright (C) 2013 Silvio Clecio.

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

unit BrookFCLCGIBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, BrookApplication, BrookException, BrookMessages, BrookConsts,
  BrookHTTPConsts, BrookRouter, BrookUtils, HTTPDefs, CustWeb, CustCGI, FPJSON,
  JSONParser, Classes, SysUtils, StrUtils;

type
  TBrookCGIApplication = class;

  { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookCGIApplication;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
  end;

  { TBrookCGIApplication }

  TBrookCGIApplication = class(TCustomCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

  { TBrookCGIRequest }

  TBrookCGIRequest = class(TCGIRequest)
  protected
    procedure DeleteTempUploadedFiles; override;
    function GetTempUploadFileName(
      const {%H-}AName, AFileName: string; {%H-}ASize: Int64): string; override;
    function RequestUploadDir: string; override;
    procedure InitRequestVars; override;
    procedure HandleUnknownEncoding(
      const AContentType: string; AStream: TStream); override;
  end;

  { TBrookCGIResponse }

  TBrookCGIResponse = class(TCGIResponse)
  protected
    procedure CollectHeaders(AHeaders: TStrings); override;
  end;

  { TBrookCGIHandler }

  TBrookCGIHandler = class(TCGIHandler)
  protected
    function CreateRequest: TCGIRequest; override;
    function CreateResponse(AOutput: TStream): TCGIResponse; override;
    function FormatContentType: string;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure ShowRequestException(R: TResponse; E: Exception); override;
  end;

implementation

{ TBrookApplication }

constructor TBrookApplication.Create;
begin
  FApp := TBrookCGIApplication.Create(nil);
  FApp.Initialize;
end;

destructor TBrookApplication.Destroy;
begin
  FApp.Free;
  inherited Destroy;
end;

function TBrookApplication.Instance: TObject;
begin
  Result := FApp;
end;

procedure TBrookApplication.Run;
begin
  FApp.Run;
end;

{ TBrookCGIApplication }

function TBrookCGIApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookCGIHandler.Create(Self);
end;

{ TBrookCGIRequest }

procedure TBrookCGIRequest.DeleteTempUploadedFiles;
begin
  if BrookSettings.DeleteUploadedFiles then
    inherited;
end;

function TBrookCGIRequest.GetTempUploadFileName(
  const AName, AFileName: string; ASize: Int64): string;
begin
  if BrookSettings.KeepUploadedNames then
    Result := inherited GetTempUploadFileName(AName, AFileName, ASize)
  else
    Result := RequestUploadDir + AFileName;
end;

function TBrookCGIRequest.RequestUploadDir: string;
begin
  Result := BrookSettings.DirectoryForUploads;
  if Result = '' then
    Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TBrookCGIRequest.InitRequestVars;
var
  VMethod: ShortString;
begin
{$IFDEF BROOK_DEBUG}
  try
{$ENDIF}
    VMethod := Method;
    if VMethod = ES then
      raise Exception.Create(SBrookNoRequestMethodError);
    case VMethod of
      BROOK_HTTP_REQUEST_METHOD_DELETE, BROOK_HTTP_REQUEST_METHOD_PUT,
        BROOK_HTTP_REQUEST_METHOD_PATCH:
        begin
          InitPostVars;
          if HandleGetOnPost then
            InitGetVars;
        end;
    else
      inherited;
    end;
{$IFDEF BROOK_DEBUG}
  except
    on E: Exception do
    begin
      WriteLn('Content-Type: text/plain');
      WriteLn;
      WriteLn('Catastrophic error:');
      raise;
    end;
  end;
{$ENDIF}
end;

procedure TBrookCGIRequest.HandleUnknownEncoding(const AContentType: string;
  AStream: TStream);

  procedure ProcessJSONObject(AJSON: TJSONObject);
  var
    I: Integer;
  begin
    for I := 0 to Pred(AJSON.Count) do
      ContentFields.Add(AJSON.Names[I] + EQ + AJSON.Items[I].AsString);
  end;

  procedure ProcessJSONArray(AJSON: TJSONArray);
  var
    I: Integer;
  begin
    for I := 0 to Pred(AJSON.Count) do
      if AJSON[I].JSONType = jtObject then
        ProcessJSONObject(AJSON.Objects[I])
      else
        raise Exception.CreateFmt('%s: Unsupported JSON format.', [ClassName]);
  end;

var
  VJSON: TJSONData;
  VParser: TJSONParser;
begin
  if Copy(AContentType, 1, Length(BROOK_HTTP_CONTENT_TYPE_APP_JSON)) =
    BROOK_HTTP_CONTENT_TYPE_APP_JSON then
    if BrookSettings.AcceptsJSONContent then
    begin
      AStream.Position := 0;
      VParser := TJSONParser.Create(AStream);
      try
        VJSON := VParser.Parse;
        case VJSON.JSONType of
          jtArray: ProcessJSONArray(TJSONArray(VJSON));
          jtObject: ProcessJSONObject(TJSONObject(VJSON));
        else
          raise Exception.CreateFmt('%s: Unsupported JSON format.', [ClassName]);
        end;
      finally
        VParser.Free;
      end;
    end
    else
      ProcessURLEncoded(AStream, ContentFields)
  else
    inherited HandleUnknownEncoding(AContentType, AStream);
end;

{ TBrookCGIResponse }

procedure TBrookCGIResponse.CollectHeaders(AHeaders: TStrings);
begin
  AHeaders.Add(BROOK_HTTP_HEADER_X_POWERED_BY + HS +
    'Brook framework and FCL-Web.');
  inherited CollectHeaders(AHeaders);
end;

{ TBrookCGIHandler }

function TBrookCGIHandler.CreateRequest: TCGIRequest;
begin
  Result := TBrookCGIRequest.CreateCGI(Self);
  if ApplicationURL = ES then
    ApplicationURL := TBrookRouter.RootUrl;
end;

function TBrookCGIHandler.CreateResponse(AOutput: TStream): TCGIResponse;
begin
  Result := TBrookCGIResponse.CreateCGI(Self, AOutput);
end;

function TBrookCGIHandler.FormatContentType: string;
begin
  if BrookSettings.Charset <> ES then
    Result := BrookSettings.ContentType + BROOK_HTTP_HEADER_CHARSET +
      BrookSettings.Charset
  else
    Result := BrookSettings.ContentType;
end;

procedure TBrookCGIHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.ContentType := FormatContentType;
  if BrookSettings.Language <> BROOK_DEFAULT_LANGUAGE then
    TBrookMessages.Service.SetLanguage(BrookSettings.Language);
  try
    TBrookRouter.Service.Route(ARequest, AResponse);
    TBrookCGIRequest(ARequest).DeleteTempUploadedFiles;
  except
    on E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;

procedure TBrookCGIHandler.ShowRequestException(R: TResponse; E: Exception);
var
  VStr: TStrings;
  VHandled: Boolean = False;
begin
  if R.ContentSent then
    Exit;
  if Assigned(BrookSettings.OnError) then
  begin
    BrookSettings.OnError(R, E, VHandled);
    if VHandled then
      Exit;
  end;
  if Assigned(OnShowRequestException) then
  begin
    OnShowRequestException(R, E, VHandled);
    if VHandled then
      Exit;
  end;
  if RedirectOnError and not R.HeadersSent then
  begin
    R.SendRedirect(Format(RedirectOnErrorURL, [HTTPEncode(E.Message)]));
    R.SendContent;
    Exit;
  end;
  if (BrookSettings.Page404 <> ES) and (E is EBrookHTTP404) and
    (not R.HeadersSent) then
  begin
    R.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
    R.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
    R.ContentType := FormatContentType;
    if FileExists(BrookSettings.Page404) then
      R.Contents.LoadFromFile(BrookSettings.Page404)
    else
      R.Content := BrookSettings.Page404;
    R.Content := Format(R.Content, [ApplicationURL]);
    R.SendContent;
    Exit;
  end;
  if (BrookSettings.Page500 <> ES) and (not R.HeadersSent) then
  begin
    R.Code := BROOK_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
    R.CodeText := 'Application error ' + E.ClassName;
    R.ContentType := FormatContentType;
    if FileExists(BrookSettings.Page500) then
    begin
      R.Contents.LoadFromFile(BrookSettings.Page500);
      R.Content := StringsReplace(R.Content, ['@error', '@trace'],
        [E.Message, BrookDumpStack], [rfIgnoreCase, rfReplaceAll]);
    end
    else
      if BrookSettings.ContentType = BROOK_HTTP_CONTENT_TYPE_APP_JSON then
        R.Content := StringsReplace(BrookSettings.Page500, ['@error', '@trace'],
          [StringToJSONString(E.Message), StringToJSONString(BrookDumpStack(LE))],
          [rfIgnoreCase, rfReplaceAll])
      else
        R.Content := StringsReplace(BrookSettings.Page500, ['@error', '@trace'],
          [E.Message, BrookDumpStack], [rfIgnoreCase, rfReplaceAll]);
    R.SendContent;
    Exit;
  end;
  if (R.ContentType = BROOK_HTTP_CONTENT_TYPE_TEXT_HTML) or
    (BrookSettings.ContentType = BROOK_HTTP_CONTENT_TYPE_TEXT_HTML) then
  begin
    VStr := TStringList.Create;
    try
      ExceptionToHTML(VStr, E, Title, Email, Administrator);
      R.Content := VStr.Text;
      R.SendContent;
    finally
      VStr.Free;
    end;
  end;
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.
