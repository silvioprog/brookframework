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
  SysUtils,
  Classes,
  Platform,
  Marshalling,
  libsagui,
  BrookHandledClasses;

resourcestring
  SBrookEmptyPattern = '%s: pattern cannot be empty.';
  SBrookRouteAlreadyExists = '%s: route ''%s'' already exists.';

type
  TBrookRoute = class;

  TBrookRouteMatchEvent = procedure(ARoute: TBrookRoute) of object;

  EBrookRoute = class(Exception);

  TBrookRoute = class(TBrookHandleCollectionItem)
  private
    FOnMath: TBrookRouteMatchEvent;
    FPattern: string;
  protected
    class procedure DoRouteCallback(Acls: Pcvoid;
      Aroute: Psg_route); cdecl; static;
  public
    procedure Validate; inline;
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

  EBrookRoutes = class(Exception);

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

procedure TBrookRoute.Validate;
begin
  if FPattern.IsEmpty then
    raise EBrookRoute.CreateResFmt(@SBrookEmptyPattern, [GetNamePath]);
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
const
  BUF_LEN = 256;
var
  RT: TBrookRoute;
  M: TMarshaller;
  P: MarshaledAString;
  R: cint;
begin
  SgCheckLibrary;
  SgCheckLastError(sg_routes_clear(@FHandle));
  GetMem(P, BUF_LEN);
  try
    for RT in Self do
    begin
      RT.Validate;
      FillChar(P^, BUF_LEN, 0);
      R := sg_routes_add2(@FHandle, M.ToCNullable(RT.Pattern), P, BUF_LEN,
{$IFNDEF VER3_0}@{$ENDIF}RT.DoRouteCallback, RT);
      if R = EALREADY then
        raise EBrookRoutes.CreateResFmt(@SBrookRouteAlreadyExists,
          [RT.GetNamePath, RT.Pattern]);
      if R <> 0 then
        raise EBrookRoutes.Create(TMarshal.ToString(P));
    end;
  finally
    FreeMem(P, BUF_LEN);
  end;
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
