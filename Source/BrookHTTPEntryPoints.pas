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

unit BrookHTTPEntryPoints;

{$I Brook.inc}

interface

uses
  RTLConsts,
  SysUtils,
  BrookUtils,
  BrookHTTPExtra,
  BrookHTTPRequest,
  BrookHTTPResponse,
  BrookEntryPoints,
  BrookHTTPRouter;

resourcestring
  SBrookEntryPointNotFound = 'Entry-point not found: %s';
  SBrookRouterNotAssigned = 'Router not assigned for entry-point ''%s''.';

type
  TBrookHTTPEntryPointsNotFoundEvent = procedure(ASender: TObject;
    const AEntryPoint, APath: string; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse) of object;

  TBrookCustomHTTPEntryPoints = class(TBrookCustomEntryPoints)
  private
    FOnNotFound: TBrookHTTPEntryPointsNotFoundEvent;
  protected
    procedure HandleRoute(ASender: TObject; const AEntryPoint, APath: string;
      ARouter: TBrookCustomHTTPRouter; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure DoNotFound(ASender: TObject; const AEntryPoint, APath: string;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); virtual;
  public
    procedure Go(ASender: TObject; const APath: string;
      ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
    procedure Go(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); overload; virtual;
    property OnNotFound: TBrookHTTPEntryPointsNotFoundEvent read FOnNotFound
      write FOnNotFound;
  end;

  TBrookHTTPEntryPoints = class(TBrookCustomHTTPEntryPoints)
  published
    property Active;
    property List;
    property OnNotFound;
  end;

implementation

procedure TBrookCustomHTTPEntryPoints.HandleRoute(ASender: TObject;
  const AEntryPoint, APath: string; ARouter: TBrookCustomHTTPRouter;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if Assigned(ARouter) then
    ARouter.Route(ASender, APath, ARequest, AResponse)
  else
    AResponse.Send(LoadResString(@SBrookRouterNotAssigned), [AEntryPoint],
      BROOK_CONTENT_TYPE, 500);
end;

procedure TBrookCustomHTTPEntryPoints.DoNotFound(ASender: TObject;
  const AEntryPoint, APath: string; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnNotFound) then
    FOnNotFound(ASender, AEntryPoint, APath, ARequest, AResponse)
  else
    AResponse.Send(LoadResString(@SBrookEntryPointNotFound), [AEntryPoint],
      BROOK_CONTENT_TYPE, 404);
end;

procedure TBrookCustomHTTPEntryPoints.Go(ASender: TObject; const APath: string;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
  VRouter: TBrookCustomHTTPRouter;
  VEntryPoint, VPath: string;
begin
  VEntryPoint := BrookFixEntryPoint(APath);
  VPath := APath.SubString(VEntryPoint.Length);
  if inherited Find(VEntryPoint, VRouter) then
    HandleRoute(ASender, VEntryPoint, VPath, VRouter, ARequest, AResponse)
  else
    DoNotFound(ASender, VEntryPoint, VPath, ARequest, AResponse);
end;

procedure TBrookCustomHTTPEntryPoints.Go(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
  if not Assigned(ARequest) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['ARequest']);
  Go(ASender, ARequest.Path, ARequest, AResponse);
end;

end.
