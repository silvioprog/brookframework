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
  SysUtils,
  Classes,
  Platform,
  Marshalling,
  libsagui,
  BrookUtils,
  BrookHandledClasses,
  BrookRoutes;

resourcestring
  SBrookOpNotAllowedInactiveRouter =
    'Operation is not allowed while the router is inactive.';
  SBrookCannotCreateRouterHandle = 'Cannot create router handle.';

type
  TBrookRouter = class;

  TBrookRouterCreatePanelClassEvent = procedure(ASender: TBrookRouter;
    var ARouteClass: TBrookRouteClass) of object;

  EBrookRouter = class(Exception);

  EBrookOpNotAllowedInactiveRouter = class(Exception);

  TBrookRouter = class(TBrookHandledComponent)
  private
    FRoutes: TBrookRoutes;
    FHandle: Psg_router;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FOnCreateRouteClass: TBrookRouterCreatePanelClassEvent;
    FOnError: TBrookErrorEvent;
    function IsActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetRoutes(AValue: TBrookRoutes);
  protected
    function CreateRoutes: TBrookRoutes; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoError(ASender: TObject; AException: Exception); virtual;
    function GetRouteClass: TBrookRouteClass; virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckActive; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function Route(const APath: string; AUserData: Pointer): Boolean; virtual;
  published
    property Active: Boolean read FActive write SetActive stored IsActive;
    property Routes: TBrookRoutes read FRoutes write SetRoutes;
    property OnCreateRouteClass: TBrookRouterCreatePanelClassEvent read
      FOnCreateRouteClass write FOnCreateRouteClass;
    property OnError: TBrookErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TBrookRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := CreateRoutes;
end;

destructor TBrookRouter.Destroy;
begin
  FRoutes.Free;
  try
    SetActive(False);
  finally
    inherited Destroy;
  end;
end;

function TBrookRouter.CreateRoutes: TBrookRoutes;
begin
  Result := TBrookRoutes.Create(Self);
end;

procedure TBrookRouter.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

function TBrookRouter.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookRouter.DoError(ASender: TObject; AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(ASender, AException);
end;

function TBrookRouter.GetRouteClass: TBrookRouteClass;
begin
  Result := TBrookRoute;
end;

procedure TBrookRouter.SetRoutes(AValue: TBrookRoutes);
begin
  FRoutes.Assign(AValue);
end;

function TBrookRouter.IsActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookRouter.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
      SgCheckLibrary;
    FActive := AValue;
  end
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        DoOpen;
    end
    else
      DoClose;
end;

procedure TBrookRouter.DoOpen;
begin
  if Assigned(FHandle) then
    Exit;
  FRoutes.Prepare;
  SgCheckLibrary;
  FHandle := sg_router_new(FRoutes.Handle);
  FActive := Assigned(FHandle);
  if not FActive then
    raise EInvalidPointer.CreateRes(@SBrookCannotCreateRouterHandle);
end;

procedure TBrookRouter.DoClose;
begin
  if not Assigned(FHandle) then
    Exit;
  SgCheckLibrary;
  sg_router_free(FHandle);
  FHandle := nil;
  FActive := False;
end;

procedure TBrookRouter.Open;
begin
  SetActive(True);
end;

procedure TBrookRouter.Close;
begin
  SetActive(False);
end;

procedure TBrookRouter.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EBrookOpNotAllowedInactiveRouter.CreateRes(
      @SBrookOpNotAllowedInactiveRouter);
end;

function TBrookRouter.Route(const APath: string; AUserData: Pointer): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  CheckActive;
  SgCheckLibrary;
  R := sg_router_dispatch(FHandle, M.ToCNullable(APath), AUserData);
  Result := R = 0;
  if (not Result) and (R <> ENOENT) then
    SgCheckLastError(R);
end;

end.
