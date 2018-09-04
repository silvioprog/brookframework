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
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

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
  private
    function GetItem(AIndex: Integer): TBrookHTTPRoute;
    procedure SetItem(AIndex: Integer; AValue: TBrookHTTPRoute);
  public
    constructor Create(AOwner: TPersistent); override;
    function Add: TBrookHTTPRoute; reintroduce; virtual;
  published
    property Items[AIndex: Integer]: TBrookHTTPRoute read GetItem
      write SetItem; default;
  end;

  TBrookHTTPRouterHolder = record
    Request: TBrookHTTPRequest;
    Response: TBrookHTTPResponse;
  end;

  TBrookHTTPRouter = class(TBrookRouter)
  protected
    function CreateRoutes: TBrookRoutes; override;
  protected
    function Route(ARequest: TBrookHTTPRequest;
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
  if Assigned(FOnRequest) then
    FOnRequest(Self, VHolder.Request, VHolder.Response);
end;

{ TBrookHTTPRoutes }

constructor TBrookHTTPRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TBrookHTTPRoute);
end;

function TBrookHTTPRoutes.Add: TBrookHTTPRoute;
begin
  Result := TBrookHTTPRoute(inherited Add);
end;

function TBrookHTTPRoutes.GetItem(AIndex: Integer): TBrookHTTPRoute;
begin
  Result := TBrookHTTPRoute(inherited Items[AIndex]);
end;

procedure TBrookHTTPRoutes.SetItem(AIndex: Integer; AValue: TBrookHTTPRoute);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TBrookHTTPRouter }

function TBrookHTTPRouter.CreateRoutes: TBrookRoutes;
begin
  Result := TBrookHTTPRoutes.Create(Self);
end;

function TBrookHTTPRouter.Route(ARequest: TBrookHTTPRequest;
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
  Result := inherited Route(ARequest.Path, @VHolder);
end;

end.
