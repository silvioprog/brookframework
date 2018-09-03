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

unit BrookRouter;

{$I Brook.inc}

interface

uses
  Classes,
  libsagui,
  BrookHandledClasses,
  BrookRoutes;

type
  TBrookRouter = class;

  TBrookRouterCreatePanelClassEvent = procedure(ASender: TBrookRouter;
    var ARouteClass: TBrookRouteClass) of object;

  TBrookRouter = class(TBrookHandledComponent)
  private
    FRoutes: TBrookRoutes;
    FOnCreateRouteClass: TBrookRouterCreatePanelClassEvent;
    procedure SetRoutes(AValue: TBrookRoutes);
  protected
    function CreateRoutes: TBrookRoutes; virtual;
    function GetRouteClass: TBrookRouteClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Routes: TBrookRoutes read FRoutes write SetRoutes;
    property OnCreateRouteClass: TBrookRouterCreatePanelClassEvent read
      FOnCreateRouteClass write FOnCreateRouteClass;
  end;

implementation

constructor TBrookRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := CreateRoutes;
end;

function TBrookRouter.CreateRoutes: TBrookRoutes;
begin
  Result := TBrookRoutes.Create(Self);
end;

function TBrookRouter.GetRouteClass: TBrookRouteClass;
begin
  Result := TBrookRoute;
end;

procedure TBrookRouter.SetRoutes(AValue: TBrookRoutes);
begin
  FRoutes.Assign(AValue);
end;

end.
