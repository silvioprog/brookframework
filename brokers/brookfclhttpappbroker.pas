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
  VHandled: Boolean = False;

  procedure HandleHTTP404;
  begin
    if not R.HeadersSent then begin
      R.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
      R.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
      R.ContentType := FormatContentType;
    end;

    if (BrookSettings.Page404File <> ES) and FileExists(BrookSettings.Page404File) then
      R.Contents.LoadFromFile(BrookSettings.Page404File)
    else
      R.Content := BrookSettings.Page404;

    R.Content := StringsReplace(R.Content, ['@root', '@path'],
      [BrookSettings.RootUrl, E.Message], [rfIgnoreCase, rfReplaceAll]);

    R.SendContent;
    VHandled := true;
  end;

  procedure HandleHTTP500;
  var
    ExceptionMessage,StackDumpString: TJSONStringType;
  begin
    if not R.HeadersSent then begin
      R.Code := BROOK_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
      R.CodeText := BROOK_HTTP_REASON_PHRASE_INTERNAL_SERVER_ERROR;
      R.ContentType := FormatContentType;
    end;

    if (BrookSettings.Page500File <> ES) and FileExists(BrookSettings.Page500File) then begin
      R.Contents.LoadFromFile(BrookSettings.Page500File);
      R.Content := StringsReplace(R.Content, ['@error'],
        [E.Message], [rfIgnoreCase, rfReplaceAll]);
      if Pos('@trace',LowerCase(R.Content))>0 then
        R.Content := StringsReplace(R.Content, ['@trace'],
          [BrookDumpStack], [rfIgnoreCase, rfReplaceAll]); // DumpStack is slow and not thread safe
    end else begin
      R.Content := BrookSettings.Page500;
      StackDumpString := '';
      if BrookSettings.ContentType = BROOK_HTTP_CONTENT_TYPE_APP_JSON then begin
        ExceptionMessage := StringToJSONString(E.Message);
        if Pos('@trace',LowerCase(R.Content))>0 then
          StackDumpString  := StringToJSONString(BrookDumpStack(LF));
      end else begin
        ExceptionMessage := E.Message;
        if Pos('@trace',LowerCase(R.Content))>0 then
           StackDumpString  := BrookDumpStack;
      end;
      R.Content := StringsReplace(BrookSettings.Page500, ['@error', '@trace'],
        [ExceptionMessage, StackDumpString], [rfIgnoreCase, rfReplaceAll]);
    end;

    R.SendContent;
    VHandled := true;
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
  if E is EBrookHTTP404 then begin
    HandleHTTP404;
  end else begin
    HandleHTTP500;
  end
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.
