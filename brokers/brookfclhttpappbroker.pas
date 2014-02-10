(*
  Brook FCL HTTPApp Broker unit.

  Copyright (C) 2013 Mario Ray Mahardhika.

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

unit BrookFCLHTTPAppBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookClasses, BrookApplication, BrookRouter, BrookUtils, BrookHTTPDefsBroker,
  HttpDefs, CustWeb, CustHTTPApp, FPHTTPServer, Classes, SysUtils;

type
  TBrookHTTPApplication = class;

  { TBrookApplication }

  TBrookApplication = class(TBrookInterfacedObject, IBrookApplication)
  private
    FApp: TBrookHTTPApplication;
    function GetTerminated: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Instance: TObject;
    procedure Run;
    procedure Terminate;
    property Terminated: Boolean read GetTerminated;
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
    function CreateResponse(
      ARequest: TFPHTTPConnectionRequest): TFPHTTPConnectionResponse; override;
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

function TBrookApplication.GetTerminated: Boolean;
begin
  Result := FApp.Terminated;
end;

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

procedure TBrookApplication.Terminate;
begin
  FApp.Terminate;
end;

{ TBrookHTTPApplication }

function TBrookHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result := TBrookHTTPServerHandler.Create(Self);
end;

{ TBrookHTTPConnectionRequest }

procedure TBrookHTTPConnectionRequest.HandleUnknownEncoding(
  const AContentType: string; AStream: TStream);
begin
  if not BrookHandleUnknownEncoding(Self, AContentType, AStream) then
    inherited HandleUnknownEncoding(AContentType, AStream);
end;

{ TBrookEmbeddedHttpServer }

function TBrookEmbeddedHttpServer.CreateRequest: TFPHTTPConnectionRequest;
begin
  Result := TBrookHTTPConnectionRequest.Create;
end;

function TBrookEmbeddedHttpServer.CreateResponse(
  ARequest: TFPHTTPConnectionRequest): TFPHTTPConnectionResponse;
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
