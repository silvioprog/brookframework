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
  BrookHTTPConsts, BrookRouter, BrookUtils, BrookHTTPDefsBroker, HttpDefs,
  CustWeb, CustHTTPApp, FPHTTPServer, Classes, SysUtils, StrUtils;

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

  { TBrookHTTPConnectionRequest }

  TBrookHTTPConnectionRequest = class(TFPHTTPConnectionRequest)
  protected
    procedure HandleUnknownEncoding(
      const AContentType: string; AStream: TStream); override;
  end;

  { TBrookHTTPConnectionResponse }

  TBrookHTTPConnectionResponse = class(TFPHTTPConnectionResponse)
  end;

 { TBrookEmbeddedHttpServer }

  TBrookEmbeddedHttpServer = class(TEmbeddedHttpServer)
  protected
    function CreateRequest: TFPHTTPConnectionRequest; override;
    function CreateResponse(ARequest: TFPHTTPConnectionRequest): TFPHTTPConnectionResponse; override;
  end;

  { TBrookHTTPServerHandler }

  TBrookHTTPServerHandler = class(TFPHTTPServerHandler)
  protected
    function CreateServer: TEmbeddedHttpServer; override;
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
  if BrookSettings.Port <> 0 then
    FApp.Port := BrookSettings.Port;
  FApp.Run;
end;

{ TBrookHTTPApplication }

function TBrookHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookHTTPServerHandler.Create(Self);
end;

{ TBrookHTTPConnectionRequest }

procedure TBrookHTTPConnectionRequest.HandleUnknownEncoding(const AContentType: string;
  AStream: TStream);
begin
  if not BrookHandleUnknownEncoding(Self, AContentType, AStream) then begin
    inherited HandleUnknownEncoding(AContentType, AStream);
  end;
end;

{ TBrookEmbeddedHttpServer }

function TBrookEmbeddedHttpServer.CreateRequest: TFPHTTPConnectionRequest;
begin
  Result := TBrookHTTPConnectionRequest.Create;
end;

function TBrookEmbeddedHttpServer.CreateResponse(ARequest: TFPHTTPConnectionRequest): TFPHTTPConnectionResponse;
begin
  Result := TBrookHTTPConnectionResponse.Create(ARequest);
end;

{ TBrookHTTPServerHandler }

function TBrookHTTPServerHandler.CreateServer: TEmbeddedHttpServer;
begin
  Result:=TBrookEmbeddedHttpServer.Create(Self);
end;

procedure TBrookHTTPServerHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  try
    AResponse.ContentType := BrookFormatContentType;
    if BrookSettings.Language <> BROOK_DEFAULT_LANGUAGE then
      TBrookMessages.Service.SetLanguage(BrookSettings.Language);
    TBrookRouter.Service.Route(ARequest, AResponse);
  except
    on E: Exception do
      ShowRequestException(AResponse, E);
  end;
end;

procedure TBrookHTTPServerHandler.ShowRequestException(R: TResponse; E: Exception);
begin
  BrookShowRequestException(Self, R, E);
end;

initialization
  BrookRegisterApp(TBrookApplication.Create);

end.
