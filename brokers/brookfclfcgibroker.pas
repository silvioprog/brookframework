(*
  Brook FCL FastCGI Broker unit.

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

unit BrookFCLFCGIBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, BrookApplication, BrookMessages, BrookConsts, BrookHTTPConsts,
  BrookRouter, BrookUtils, BrookHTTPDefsBroker, HTTPDefs, CustWeb, CustFCGI,
  Classes, SysUtils;

type
  TBrookFCGIApplication = class;

  { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookFCGIApplication;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
  end;

  { TBrookFCGIApplication }

  TBrookFCGIApplication = class(TCustomFCGIApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

  { TBrookFCGIRequest }

  TBrookFCGIRequest = class(TFCGIRequest)
  protected
    procedure DeleteTempUploadedFiles; override;
    function GetTempUploadFileName(
      const {%H-}AName, AFileName: string; {%H-}ASize: Int64): string; override;
    function RequestUploadDir: string; override;
    procedure InitRequestVars; override;
    procedure HandleUnknownEncoding(
      const AContentType: string; AStream: TStream); override;
  end;

  { TBrookFCGIResponse }

  TBrookFCGIResponse = class(TFCGIResponse)
  protected
    procedure CollectHeaders(AHeaders: TStrings); override;
  end;

  { TBrookFCGIHandler }

  TBrookFCGIHandler = class(TFCGIHandler)
  protected
    function CreateRequest: TFCGIRequest; override;
    function CreateResponse(ARequest: TFCGIRequest): TFCGIResponse; override;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure ShowRequestException(R: TResponse; E: Exception); override;
  end;

implementation

{ TBrookApplication }

constructor TBrookApplication.Create;
begin
  FApp := TBrookFCGIApplication.Create(nil);
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
  if BrookSettings.Port <> 0 then
    FApp.Port := BrookSettings.Port;
  FApp.Run;
end;

{ TBrookFCGIApplication }

function TBrookFCGIApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookFCGIHandler.Create(Self);
end;

{ TBrookFCGIRequest }

procedure TBrookFCGIRequest.DeleteTempUploadedFiles;
begin
  if BrookSettings.DeleteUploadedFiles then
    inherited;
end;

function TBrookFCGIRequest.GetTempUploadFileName(
  const AName, AFileName: string; ASize: Int64): string;
begin
  if BrookSettings.KeepUploadedNames then
    Result := RequestUploadDir + AFileName
  else
    Result := inherited GetTempUploadFileName(AName, AFileName, ASize);
end;

function TBrookFCGIRequest.RequestUploadDir: string;
begin
  Result := BrookSettings.DirectoryForUploads;
  if Result = '' then
    Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TBrookFCGIRequest.InitRequestVars;
var
  VMethod: ShortString;
begin
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
end;

procedure TBrookFCGIRequest.HandleUnknownEncoding(const AContentType: string;
  AStream: TStream);
begin
  if not BrookHandleUnknownEncoding(Self, AContentType, AStream) then
    inherited HandleUnknownEncoding(AContentType, AStream);
end;

{ TBrookFCGIResponse }

procedure TBrookFCGIResponse.CollectHeaders(AHeaders: TStrings);
begin
  AHeaders.Add(BROOK_HTTP_HEADER_X_POWERED_BY + HS +
    'Brook framework and FCL-Web.');
  inherited CollectHeaders(AHeaders);
end;

{ TBrookFCGIHandler }

function TBrookFCGIHandler.CreateRequest: TFCGIRequest;
begin
  Result := TBrookFCGIRequest.Create;
  if ApplicationURL = ES then
    ApplicationURL := TBrookRouter.RootUrl;
end;

function TBrookFCGIHandler.CreateResponse(ARequest: TFCGIRequest): TFCGIResponse;
begin
  Result := TBrookFCGIResponse.Create(ARequest);
end;

procedure TBrookFCGIHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.ContentType := BrookFormatContentType;
  if BrookSettings.Language <> BROOK_DEFAULT_LANGUAGE then
    TBrookMessages.Service.SetLanguage(BrookSettings.Language);
  try
    TBrookRouter.Service.Route(ARequest, AResponse);
    TBrookFCGIRequest(ARequest).DeleteTempUploadedFiles;
  except
    on E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;

procedure TBrookFCGIHandler.ShowRequestException(R: TResponse; E: Exception);
begin
  BrookShowRequestException(Self, R, E);
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.
