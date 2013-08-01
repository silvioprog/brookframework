(*
  Brook FCL HTTPApp Broker unit.

  Copyright (C) 2013 Mario Ray Mahardhika.

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

unit BrookFCLHTTPAppBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, BrookApplication, BrookException, BrookMessages, BrookConsts,
  BrookHTTPConsts, BrookRouter, BrookUtils, HTTPDefs, CustWeb, CustHTTPApp,
  FPJSON, Classes, SysUtils, StrUtils;

type
  TBrookHTTPApplication = class;

  { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookHTTPApplication;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
  end;

  { TBrookHTTPApplication }

  TBrookHTTPApplication = class(TCustomHTTPApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

  { TBrookHTTPServerHandler }

  TBrookHTTPServerHandler = class(TFPHTTPServerHandler)
  protected
    function FormatContentType: string;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure ShowRequestException(R: TResponse; E: Exception); override;
  end;

implementation

{ TBrookApplication }

constructor TBrookApplication.Create;
begin
  FApp := TBrookHTTPApplication.Create(nil);
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

{ TBrookHTTPApplication }

function TBrookHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookHTTPServerHandler.Create(Self);
end;

{ TBrookHTTPServerHandler }

function TBrookHTTPServerHandler.FormatContentType: string;
begin
  if BrookSettings.Charset <> ES then
    Result := BrookSettings.ContentType + BROOK_HTTP_HEADER_CHARSET +
      BrookSettings.Charset
  else
    Result := BrookSettings.ContentType;
end;

procedure TBrookHTTPServerHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  try
    AResponse.ContentType := FormatContentType;
    if BrookSettings.Language <> BROOK_DEFAULT_LANGUAGE then
      TBrookMessages.Service.SetLanguage(BrookSettings.Language);
    TBrookRouter.Service.Route(ARequest, AResponse);
  except
    on E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;

procedure TBrookHTTPServerHandler.ShowRequestException(R: TResponse; E: Exception);
var
  VStr: TStrings;
  VHandled: Boolean = False;

  procedure HandleHTTP404;
  begin
    if not R.HeadersSent then begin
      R.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
      R.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
      R.ContentType := FormatContentType;

      if FileExists(BrookSettings.Page404File) then
        R.Contents.LoadFromFile(BrookSettings.Page404File)
      else if BrookSettings.Page404 <> ES then
        R.Content := BrookSettings.Page404;

      R.Content := Format(R.Content, [BrookSettings.RootUrl]);
      R.SendContent;
      VHandled := true;
    end;
  end;

  procedure HandleHTTP500;
  var
    ExceptionMessage,StackDumpString: TJSONStringType;
  begin
    if not R.HeadersSent then begin
      R.Code := BROOK_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
      R.CodeText := 'Application error ' + E.ClassName;
      R.ContentType := FormatContentType;

      if FileExists(BrookSettings.Page500File) then begin
        R.Contents.LoadFromFile(BrookSettings.Page500File);
        R.Content := StringsReplace(R.Content, ['@error', '@trace'],
          [E.Message, BrookDumpStack], [rfIgnoreCase, rfReplaceAll]);
      end else if BrookSettings.Page500 <> ES then begin
        if BrookSettings.ContentType = BROOK_HTTP_CONTENT_TYPE_APP_JSON then begin
          ExceptionMessage := StringToJSONString(E.Message);
          StackDumpString  := StringToJSONString(BrookDumpStack(LE));
        end else begin
          ExceptionMessage := E.Message;
          StackDumpString  := BrookDumpStack;
        end;
        R.Content := StringsReplace(BrookSettings.Page500, ['@error', '@trace'],
          [ExceptionMessage, StackDumpString], [rfIgnoreCase, rfReplaceAll]);
      end;

      R.SendContent;
      VHandled := true;
    end;
  end;

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
  case E.ClassName of
    'EBrookHTTP404': HandleHTTP404;
    'EBrookHTTP500': HandleHTTP500;
  end;
  if VHandled then
    Exit;
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
