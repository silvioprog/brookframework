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
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookRouter;

type
  TBrookHTTPRoute = class;

  TBrookHTTPRouteRequestEvent = procedure(ASender: TObject;
    ARoute: TBrookHTTPRoute; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  TBrookHTTPRoute = class(TBrookRoute)
  private
    FOnRequest: TBrookHTTPRouteRequestEvent;
  protected
    procedure DoMatch(ARoute: TBrookRoute); override;
  published
    property OnRequest: TBrookHTTPRouteRequestEvent read FOnRequest
      write FOnRequest;
  end;

  TBrookHTTPRoutes = class(TBrookRoutes)
  public
    class function GetRouterClass: TBrookRouteClass; override;
  end;

  TBrookHTTPRouterHolder = record
    Request: TBrookHTTPRequest;
    Response: TBrookHTTPResponse;
    Sender: TObject;
  end;

  TBrookHTTPRouter = class(TBrookRouter)
  protected
    function CreateRoutes: TBrookRoutes; override;
  public
    function Route(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse): Boolean; reintroduce; virtual;
  end;

implementation

{ TBrookHTTPRoute }

procedure TBrookHTTPRoute.DoMatch(ARoute: TBrookRoute);
var
  VHolder: TBrookHTTPRouterHolder;
begin
  VHolder := TBrookHTTPRouterHolder(ARoute.UserData^);
  inherited DoMatch(ARoute);
  if not Assigned(FOnRequest) then
    Exit;
  FOnRequest(VHolder.Sender, TBrookHTTPRoute(ARoute), VHolder.Request,
    VHolder.Response);
end;

{ TBrookHTTPRoutes }

class function TBrookHTTPRoutes.GetRouterClass: TBrookRouteClass;
begin
  Result := TBrookHTTPRoute;
end;

{ TBrookHTTPRouter }

function TBrookHTTPRouter.CreateRoutes: TBrookRoutes;
begin
  Result := TBrookHTTPRoutes.Create(Self);
end;

function TBrookHTTPRouter.Route(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse): Boolean;
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
