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

program httprouter;

{$MODE DELPHI}
{$WARN 5024 OFF}

uses
  SysUtils,
  Classes,
  BrookLibraryLoader,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookHTTPRouter,
  BrookHTTPServer;

type

  { TRouter }

  TRouter = class(TBrookHTTPRouter)
  protected
    procedure DoNotFound(ASender: TObject; const ARoute: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
  end;

  { THTTPServer }

  THTTPServer = class(TBrookHTTPServer)
  private
    FRouter: TRouter;
    procedure RouteDownloads(ASender: TObject; ARoute: TBrookHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure RouteHome(ASender: TObject; ARoute: TBrookHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
    procedure RoutePage(ASender: TObject; ARoute: TBrookHTTPRoute;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddRoute(const APattern: string;
      ARequestEvent: TBrookHTTPRouteRequestEvent);
    property Router: TRouter read FRouter;
  end;

{ TRouter }

procedure TRouter.DoNotFound(ASender: TObject; const ARoute: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('Page not found', 'text/plain', 404);
end;

{ THTTPServer }

constructor THTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRouter := TRouter.Create(Self);
  AddRoute('/home', RouteHome);
  AddRoute('/download/(?P<file>[a-z]+)', RouteDownloads);
  AddRoute('/page/([0-9]+)', RoutePage);
  FRouter.Active := True;
end;

procedure THTTPServer.AddRoute(const APattern: string;
  ARequestEvent: TBrookHTTPRouteRequestEvent);
var
  RT: TBrookHTTPRoute;
begin
  RT := Router.Routes.Add;
  RT.Pattern := APattern;
  RT.OnRequest := ARequestEvent;
end;

procedure THTTPServer.RouteHome(ASender: TObject; ARoute: TBrookHTTPRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('Home page', 'text/plain', 200);
end;

procedure THTTPServer.RouteDownloads(ASender: TObject; ARoute: TBrookHTTPRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('Downloaded file: %s',
    [ARoute.Variables['file']], 'text/plain', 200);
end;

procedure THTTPServer.RoutePage(ASender: TObject; ARoute: TBrookHTTPRoute;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('Page number: %d', [ARoute.Segments[0].ToInteger],
    'text/plain', 200);
end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  Router.Route(ASender, ARequest, AResponse);
end;

begin
  with THTTPServer.Create(nil) do
  try
    Open;
    if not Active then
      Exit;
    WriteLn('Server running at http://localhost:', Port);
    ReadLn;
  finally
    Free;
  end;
end.
