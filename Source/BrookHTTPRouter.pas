(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– an ideal Pascal microframework to develop cross-platform HTTP servers.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
 *)

unit BrookHTTPRouter;

{$I Brook.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes,
  BrookHTTPExtra,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookPathRouter;

resourcestring
  SBrookRequestMethodNotAllowed = 'Request method not allowed: %s';
  SBrookRequestNoMethodDefined = 'No method(s) defined';

type
  TBrookCustomHTTPRoute = class;

  TBrookHTTPRouteRequestEvent = procedure(ASender: TObject;
    ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRouteErrorEvent = procedure(ASender: TObject;
    ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse; AException: Exception) of object;

  TBrookHTTPRouteRequestMethodEvent = function(ASender: TObject;
    ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse): Boolean of object;

  TBrookHTTPRouteRequestMethod = (rmUnknown, rmGET, rmPOST, rmPUT, rmDELETE,
    rmPATCH, rmOPTIONS, rmHEAD);

  TBrookHTTPRouteRequestMethods = set of TBrookHTTPRouteRequestMethod;

  TBrookHTTPRouteRequestMethodHelper = record helper for TBrookHTTPRouteRequestMethod
  public const
    METHODS: array[TBrookHTTPRouteRequestMethod] of string = ('Unknown',
      'GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'OPTIONS', 'HEAD');
  public
    function ToString: string; inline;
    function FromString(
      const AMethod: string): TBrookHTTPRouteRequestMethod; inline;
  end;

  TBrookCustomHTTPRoute = class(TBrookCustomPathRoute)
  public const
    DefaultReqMethods = [rmGET, rmPOST];
  private
    FMethods: TBrookHTTPRouteRequestMethods;
    FOnRequestMethod: TBrookHTTPRouteRequestMethodEvent;
    FOnRequest: TBrookHTTPRouteRequestEvent;
    FOnError: TBrookHTTPRouteErrorEvent;
    function IsMethods: Boolean;
  protected
    procedure DoMatch(ARoute: TBrookCustomPathRoute); override;
    function DoRequestMethod(ASender: TObject; ARoute: TBrookCustomHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): Boolean; virtual;
    procedure DoRequest(ASender: TObject; ARoute: TBrookCustomHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure DoRoute(ASender: TObject; ARoute: TBrookCustomHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
    procedure DoError(ASender: TObject; ARoute: TBrookCustomHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      AException: Exception);
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
    property Methods: TBrookHTTPRouteRequestMethods read FMethods write FMethods
      stored IsMethods;
    property OnRequestMethod: TBrookHTTPRouteRequestMethodEvent
      read FOnRequestMethod write FOnRequestMethod;
    property OnRequest: TBrookHTTPRouteRequestEvent read FOnRequest
      write FOnRequest;
    property OnError: TBrookHTTPRouteErrorEvent read FOnError write FOnError;
  end;

  TBrookHTTPRoute = class(TBrookCustomHTTPRoute)
  published
    property Methods default TBrookHTTPRoute.DefaultReqMethods;
    property Pattern;
    property Path;
    property OnMath;
    property OnRequestMethod;
    property OnRequest;
    property OnError;
  end;

  TBrookHTTPRoutes = class(TBrookPathRoutes)
  public
    class function GetRouterClass: TBrookCustomPathRouteClass; override;
    function Add: TBrookCustomHTTPRoute; reintroduce; virtual;
  end;

  TBrookHTTPRouterHolder = record
    Request: TBrookHTTPRequest;
    Response: TBrookHTTPResponse;
    Sender: TObject;
  end;

  TBrookHTTPRouterRequestEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  TBrookCustomHTTPRouter = class(TBrookCustomPathRouter)
  private
    FOnNotFound: TBrookHTTPRouterRequestEvent;
    FOnRoute: TBrookHTTPRouterRequestEvent;
    function GetRoutes: TBrookHTTPRoutes;
    procedure SetRoutes(AValue: TBrookHTTPRoutes);
  protected
    function CreateRoutes: TBrookPathRoutes; override;
    procedure DoRoute(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse);
    procedure DoNotFound(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse);
  public
    procedure Route(ASender: TObject; const APath: string;
      ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); reintroduce; virtual;
    property Routes: TBrookHTTPRoutes read GetRoutes write SetRoutes;
    property OnRoute: TBrookHTTPRouterRequestEvent read FOnRoute write FOnRoute;
    property OnNotFound: TBrookHTTPRouterRequestEvent read FOnNotFound
      write FOnNotFound;
  end;

  TBrookHTTPRouter = class(TBrookCustomHTTPRouter)
  published
    property Active;
    property Routes;
    property OnRoute;
    property OnNotFound;
  end;

  { TODO: TBrookRESTRouter }

implementation

{ TBrookHTTPRouteRequestMethodHelper }

function TBrookHTTPRouteRequestMethodHelper.ToString: string;
begin
  Result := METHODS[Self];
end;

function TBrookHTTPRouteRequestMethodHelper.FromString(
  const AMethod: string): TBrookHTTPRouteRequestMethod;
var
  M: string;
  I: TBrookHTTPRouteRequestMethod;
begin
  M := AMethod.ToUpper;
  for I := Low(METHODS) to High(METHODS) do
    if SameStr(M, METHODS[I]) then
      Exit(I);
  Result := rmUnknown;
end;

{ TBrookCustomHTTPRoute }

constructor TBrookCustomHTTPRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMethods := DefaultReqMethods;
end;

procedure TBrookCustomHTTPRoute.Assign(ASource: TPersistent);
begin
  inherited Assign(ASource);
  if ASource is TBrookCustomHTTPRoute then
    FMethods := (ASource as TBrookCustomHTTPRoute).FMethods;
end;

procedure TBrookCustomHTTPRoute.DoMatch(ARoute: TBrookCustomPathRoute);
var
  VHolder: TBrookHTTPRouterHolder;
begin
  inherited DoMatch(ARoute);
  VHolder := TBrookHTTPRouterHolder(ARoute.UserData^);
  DoRoute(VHolder.Sender, TBrookHTTPRoute(ARoute), VHolder.Request,
    VHolder.Response);
end;

function TBrookCustomHTTPRoute.DoRequestMethod(ASender: TObject;
  ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse): Boolean;
begin
  if Assigned(FOnRequestMethod) then
    Result := FOnRequestMethod(ASender, ARoute, ARequest, AResponse)
  else
    Result := (FMethods = []) or
      (rmUnknown.FromString(ARequest.Method) in FMethods);
end;

procedure TBrookCustomHTTPRoute.DoRequest(ASender: TObject;
  ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(ASender, ARoute, ARequest, AResponse)
  else
    AResponse.SendEmpty;
end;

procedure TBrookCustomHTTPRoute.DoRoute(ASender: TObject;
  ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  try
    if not (FMethods = [rmUnknown]) then
      if DoRequestMethod(ASender, ARoute, ARequest, AResponse) then
        DoRequest(ASender, ARoute, ARequest, AResponse)
      else
        AResponse.Send(LoadResString(@SBrookRequestMethodNotAllowed),
          [ARequest.Method], BROOK_CONTENT_TYPE, 405)
    else
      AResponse.Send(LoadResString(@SBrookRequestNoMethodDefined),
        BROOK_CONTENT_TYPE, 500);
  except
    on E: Exception do
      DoError(ASender, ARoute, ARequest, AResponse, E);
  end;
end;

procedure TBrookCustomHTTPRoute.DoError(ASender: TObject;
  ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(ASender, ARoute, ARequest, AResponse, AException)
  else
    AResponse.Send(AException.Message, BROOK_CONTENT_TYPE, 500);
end;

function TBrookCustomHTTPRoute.IsMethods: Boolean;
begin
  Result := FMethods <> DefaultReqMethods;
end;

{ TBrookHTTPRoutes }

class function TBrookHTTPRoutes.GetRouterClass: TBrookCustomPathRouteClass;
begin
  Result := TBrookHTTPRoute;
end;

function TBrookHTTPRoutes.Add: TBrookCustomHTTPRoute;
begin
  Result := TBrookHTTPRoute(inherited Add);
end;

{ TBrookCustomHTTPRouter }

function TBrookCustomHTTPRouter.CreateRoutes: TBrookPathRoutes;
begin
  Result := TBrookHTTPRoutes.Create(Self);
end;

function TBrookCustomHTTPRouter.GetRoutes: TBrookHTTPRoutes;
begin
  Result := TBrookHTTPRoutes(inherited Routes);
end;

procedure TBrookCustomHTTPRouter.SetRoutes(AValue: TBrookHTTPRoutes);
begin
  inherited Routes := AValue;
end;

procedure TBrookCustomHTTPRouter.DoRoute(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRoute) then
    FOnRoute(ASender, ARequest, AResponse);
end;

procedure TBrookCustomHTTPRouter.DoNotFound(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnNotFound) then
    FOnNotFound(ASender, ARequest, AResponse);
end;

procedure TBrookCustomHTTPRouter.Route(ASender: TObject; const APath: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  VHolder: TBrookHTTPRouterHolder;
begin
  if not Assigned(ARequest) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['ARequest']);
  if not Assigned(AResponse) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AResponse']);
  VHolder.Request := ARequest;
  VHolder.Response := AResponse;
  VHolder.Sender := ASender;
  if inherited Route(APath, @VHolder) then
    DoRoute(ASender, ARequest, AResponse)
  else
    DoNotFound(ASender, ARequest, AResponse);
end;

end.
