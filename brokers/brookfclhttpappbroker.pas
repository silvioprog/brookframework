(*
  Brook framework, FCL HTTPApp Broker

  Copyright (C) 2014 Mario Ray Mahardhika

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookFCLHttpAppBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, BrookApplication, BrookRouter, BrookUtils, BrookConsts,
  BrookHttpConsts, BrookHttpDefsBroker, BrookMessages, HttpDefs, CustWeb,
  CustHttpApp, FPHttpServer, Classes, SysUtils;

type
  TBrookHttpApplication = class;

  { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookHttpApplication;
    function GetTerminated: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
    procedure Terminate;
    property Terminated: Boolean read GetTerminated;
  end;

  { TBrookHttpApplication }

  TBrookHttpApplication = class(TCustomHttpApplication)
  protected
    function InitializeWebHandler: TWebHandler; override;
  end;

  { TBrookHttpConnectionRequest }

  TBrookHttpConnectionRequest = class(TFPHttpConnectionRequest)
  protected
    procedure DeleteTempUploadedFiles; override;
    function GetTempUploadFileName(
      const {%H-}AName, AFileName: string; {%H-}ASize: Int64): string; override;
    function RequestUploadDir: string; override;
    procedure InitRequestVars; override;
    procedure HandleUnknownEncoding(
      const AContentType: string; AStream: TStream); override;
  end;

  { TBrookHttpConnectionResponse }

  TBrookHttpConnectionResponse = class(TFPHttpConnectionResponse)
  protected
    procedure CollectHeaders(AHeaders: TStrings); override;
  end;

 { TBrookEmbeddedHttpServer }

  TBrookEmbeddedHttpServer = class(TEmbeddedHttpServer)
  protected
    function CreateRequest: TFPHttpConnectionRequest; override;
    function CreateResponse(
      ARequest: TFPHttpConnectionRequest): TFPHttpConnectionResponse; override;
  end;

  { TBrookHttpServerHandler }

  TBrookHttpServerHandler = class(TFPHttpServerHandler)
  protected
    function CreateServer: TEmbeddedHttpServer; override;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure ShowRequestException(R: TResponse; E: Exception); override;
  end;


implementation

{ TBrookApplication }

function TBrookApplication.GetTerminated: Boolean;
begin
  Result := FApp.Terminated;
end;

constructor TBrookApplication.Create;
begin
  FApp := TBrookHttpApplication.Create(nil);
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

procedure TBrookApplication.Terminate;
begin
  FApp.Terminate;
end;

{ TBrookHttpApplication }

function TBrookHttpApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookHttpServerHandler.Create(Self);
end;

{ TBrookHttpConnectionRequest }

procedure TBrookHttpConnectionRequest.DeleteTempUploadedFiles;
begin
  if BrookSettings.DeleteUploadedFiles then
    inherited;
end;

function TBrookHttpConnectionRequest.GetTempUploadFileName(const AName,
  AFileName: string; ASize: Int64): string;
begin
  if BrookSettings.KeepUploadedNames then
    Result := RequestUploadDir + AFileName
  else
    Result := inherited GetTempUploadFileName(AName, AFileName, ASize);
end;

function TBrookHttpConnectionRequest.RequestUploadDir: string;
begin
  Result := BrookSettings.DirectoryForUploads;
  if Result = '' then
    Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TBrookHttpConnectionRequest.InitRequestVars;
var
  VMethod: string;
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

procedure TBrookHttpConnectionRequest.HandleUnknownEncoding(
  const AContentType: string; AStream: TStream);
begin
  if not BrookHandleUnknownEncoding(Self, AContentType, AStream) then
    inherited HandleUnknownEncoding(AContentType, AStream);
end;

{ TBrookHttpConnectionResponse }

procedure TBrookHttpConnectionResponse.CollectHeaders(AHeaders: TStrings);
begin
  AHeaders.Add(BROOK_HTTP_HEADER_X_POWERED_BY + HS +
    'Brook framework and FCL-Web.');
  inherited CollectHeaders(AHeaders);
end;

{ TBrookEmbeddedHttpServer }

function TBrookEmbeddedHttpServer.CreateRequest: TFPHttpConnectionRequest;
begin
  Result := TBrookHttpConnectionRequest.Create;
end;

function TBrookEmbeddedHttpServer.CreateResponse(
  ARequest: TFPHttpConnectionRequest): TFPHttpConnectionResponse;
begin
  Result := TBrookHttpConnectionResponse.Create(ARequest);
end;

{ TBrookHttpServerHandler }

function TBrookHttpServerHandler.CreateServer: TEmbeddedHttpServer;
begin
  Result := TBrookEmbeddedHttpServer.Create(Self);
end;

procedure TBrookHttpServerHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  AResponse.ContentType := BrookFormatContentType;
  try
    TBrookRouter.Service.Route(ARequest, AResponse);
    TBrookHttpConnectionRequest(ARequest).DeleteTempUploadedFiles;
  except
    on E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;

procedure TBrookHttpServerHandler.ShowRequestException(R: TResponse; E: Exception);
begin
  BrookShowRequestException(Self, R, E);
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.
