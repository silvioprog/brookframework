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

unit BrookRoutes;

{$I Brook.inc}

interface

uses
  Classes,
  Marshalling,
  libsagui,
  BrookHandledClasses;

type
  TBrookRoute = class;

  TBrookRouteMatchEvent = procedure(ARoute: TBrookRoute) of object;

  TBrookRoute = class(TBrookHandleCollectionItem)
  private
    FOnMath: TBrookRouteMatchEvent;
    FPattern: string;
  protected
    class procedure DoRouteCallback(Acls: Pcvoid;
      Aroute: Psg_route); cdecl; static;
  published
    property Pattern: string read FPattern write FPattern;
    property OnMath: TBrookRouteMatchEvent read FOnMath write FOnMath;
  end;

  TBrookRouteClass = class of TBrookRoute;

  TBrookRoutesEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookRoute;
    property Current: TBrookRoute read GetCurrent;
  end;

  TBrookRoutes = class(TBrookHandleOwnedCollection)
  private
    FHandle: Psg_route;
    function GetItem(AIndex: Integer): TBrookRoute;
    procedure SetItem(AIndex: Integer; AValue: TBrookRoute);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    function GetEnumerator: TBrookRoutesEnumerator;
    procedure Prepare; virtual;
    function Add: TBrookRoute; virtual;
    property Items[AIndex: Integer]: TBrookRoute read GetItem
      write SetItem; default;
    procedure Clear; virtual;
  end;

implementation

class procedure TBrookRoute.DoRouteCallback(Acls: Pcvoid; Aroute: Psg_route);
var
  RT: TBrookRoute absolute Acls;
begin
  if Assigned(RT.OnMath) then
    RT.OnMath(RT);
end;

{ TBrookRoutesEnumerator }

function TBrookRoutesEnumerator.GetCurrent: TBrookRoute;
begin
  Result := TBrookRoute(inherited GetCurrent);
end;

{ TBrookRoutes }

constructor TBrookRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TBrookRoute);
end;

function TBrookRoutes.GetEnumerator: TBrookRoutesEnumerator;
begin
  Result := TBrookRoutesEnumerator.Create(Self);
end;

function TBrookRoutes.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookRoutes.Prepare;
var
  M: TMarshaller;
  RT: TBrookRoute;
begin
  SgCheckLibrary;
  SgCheckLastError(sg_routes_clear(@FHandle));
  for RT in Self do
    SgCheckLastError(sg_routes_add(@FHandle, M.ToCNullable(RT.Pattern),
{$IFNDEF VER3_0}@{$ENDIF}RT.DoRouteCallback, RT));
end;

function TBrookRoutes.Add: TBrookRoute;
begin
  Result := TBrookRoute(inherited Add);
end;

function TBrookRoutes.GetItem(AIndex: Integer): TBrookRoute;
begin
  Result := TBrookRoute(inherited Items[AIndex]);
end;

procedure TBrookRoutes.SetItem(AIndex: Integer; AValue: TBrookRoute);
begin
  inherited SetItem(AIndex, TCollectionItem(AValue));
end;

procedure TBrookRoutes.Clear;
begin
  inherited Clear;
  SgCheckLibrary;
  SgCheckLastError(sg_routes_clear(@FHandle));
end;

end.
