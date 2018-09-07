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
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookRouter;

type
  TBrookCustomHTTPRoute = class;

  TBrookHTTPRouteRequestEvent = procedure(ASender: TObject;
    ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  TBrookCustomHTTPRoute = class(TBrookCustomRoute)
  private
    FOnRequest: TBrookHTTPRouteRequestEvent;
  protected
    procedure DoMatch(ARoute: TBrookCustomRoute); override;
    procedure DoRequest(ASender: TObject; ARoute: TBrookCustomHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
  public
    property OnRequest: TBrookHTTPRouteRequestEvent read FOnRequest
      write FOnRequest;
  end;

  TBrookHTTPRoute = class(TBrookCustomHTTPRoute)
  published
    property Pattern;
    property Path;
    property OnCreate;
    property OnDestroy;
    property OnMath;
    property OnRequest;
  end;

  TBrookHTTPRoutes = class(TBrookRoutes)
  public
    class function GetRouterClass: TBrookCustomRouteClass; override;
    function Add: TBrookCustomHTTPRoute; reintroduce; virtual;
  end;

  TBrookHTTPRouterHolder = record
    Request: TBrookHTTPRequest;
    Response: TBrookHTTPResponse;
    Sender: TObject;
  end;

  TBrookCustomHTTPRouter = class(TBrookCustomRouter)
  protected
    function CreateRoutes: TBrookRoutes; override;
  public
    function Route(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse): Boolean; reintroduce; virtual;
  end;

  TBrookHTTPRouter = class(TBrookCustomHTTPRouter)
  published
    property Active;
    property EntryPoint;
    property Routes;
  end;

implementation

{ TBrookCustomHTTPRoute }

procedure TBrookCustomHTTPRoute.DoMatch(ARoute: TBrookCustomRoute);
var
  VHolder: TBrookHTTPRouterHolder;
begin
  VHolder := TBrookHTTPRouterHolder(ARoute.UserData^);
  try
    inherited DoMatch(ARoute);
  finally
    DoRequest(VHolder.Sender, TBrookCustomHTTPRoute(ARoute), VHolder.Request,
      VHolder.Response);
  end;
end;

procedure TBrookCustomHTTPRoute.DoRequest(ASender: TObject;
  ARoute: TBrookCustomHTTPRoute; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(ASender, ARoute, ARequest, AResponse);
end;

{ TBrookHTTPRoutes }

class function TBrookHTTPRoutes.GetRouterClass: TBrookCustomRouteClass;
begin
  Result := TBrookHTTPRoute;
end;

function TBrookHTTPRoutes.Add: TBrookCustomHTTPRoute;
begin
  Result := TBrookHTTPRoute(inherited Add);
end;

{ TBrookCustomHTTPRouter }

function TBrookCustomHTTPRouter.CreateRoutes: TBrookRoutes;
begin
  Result := TBrookHTTPRoutes.Create(Self);
end;

function TBrookCustomHTTPRouter.Route(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): Boolean;
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
  Result := inherited Route(ARequest.Path, @VHolder);
end;

end.
