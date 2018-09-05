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
  BrookStringMap;

resourcestring
  SBrookOpNotAllowedInactiveRouter =
    'Operation is not allowed while the router is inactive.';
  SBrookCannotCreateRouterHandle = 'Cannot create router handle.';
  SBrookEmptyPattern = '%s: pattern cannot be empty.';
  SBrookRouteAlreadyExists = '%s: route ''%s'' already exists.';
  SBrookEmptyPath = 'Path cannot be empty.';

type
  TBrookRoute = class;

  TBrookRoutes = class;

  TBrookRouteClass = class of TBrookRoute;

  TBrookRouter = class;

  TBrookRouterCreatePanelClassEvent = procedure(ASender: TBrookRouter;
    var ARouteClass: TBrookRouteClass) of object;

  TBrookRouteMatchEvent = procedure(ARoute: TBrookRoute) of object;

  EBrookRoute = class(Exception);

  EBrookRoutes = class(Exception);

  EBrookRouter = class(Exception);

  EBrookOpNotAllowedInactiveRouter = class(Exception);

  TBrookRoute = class(TBrookHandleCollectionItem)
  private
    FVariables: TBrookStringMap;
    FSegments: TArray<string>;
    FOnMath: TBrookRouteMatchEvent;
    FPattern: string;
    FHandle: Psg_route;
    Fvars: Psg_strmap;
    function GetPattern: string;
    function GetPath: string;
    function GetPatternRaw: string;
    function GetSegments: TArray<string>;
    function GetVariables: TBrookStringMap;
    function GetRegexHandle: Pointer;
    function GetUserData: Pointer;
    procedure SetPattern(const AValue: string);
  protected
    class procedure DoRouteCallback(Acls: Pcvoid;
      Aroute: Psg_route); cdecl; static;
    class function DoGetSegmentsCallback(Acls: Pcvoid;
      const Asegment: Pcchar): cint; cdecl; static;
    class function DoGetVarsCallback(Acls: Pcvoid;
      const Aname: Pcchar; const Aval: Pcchar): cint; cdecl; static;
    function GetHandle: Pointer; override;
    procedure DoMatch(ARoute: TBrookRoute); virtual;
    function MakePattern: string; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Validate; inline;
    property Owner: TPersistent read GetOwner;
    property PatternRaw: string read GetPatternRaw;
    property Segments: TArray<string> read GetSegments;
    property Variables: TBrookStringMap read GetVariables;
    property RegexHandle: Pointer read GetRegexHandle;
    property UserData: Pointer read GetUserData;
  published
    property Pattern: string read GetPattern write SetPattern;
    property Path: string read GetPath;
    property OnMath: TBrookRouteMatchEvent read FOnMath write FOnMath;
  end;

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
    class function GetRouterClass: TBrookRouteClass; virtual;
    function GetEnumerator: TBrookRoutesEnumerator;
    procedure Prepare; virtual;
    function Add: TBrookRoute; virtual;
    function IndexOf(const APattern: string): Integer; virtual;
    function Find(const APattern: string): TBrookRoute; virtual;
    property Items[AIndex: Integer]: TBrookRoute read GetItem
      write SetItem; default;
    procedure Clear; virtual;
  end;

  TBrookRouter = class(TBrookHandledComponent)
  private
    FEntryPoint: string;
    FRoutes: TBrookRoutes;
    FHandle: Psg_router;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FOnCreateRouteClass: TBrookRouterCreatePanelClassEvent;
    function IsActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetEntryPoint(const AValue: string);
    procedure SetRoutes(AValue: TBrookRoutes);
  protected
    function CreateRoutes: TBrookRoutes; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
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
    property EntryPoint: string read FEntryPoint write SetEntryPoint;
    property Routes: TBrookRoutes read FRoutes write SetRoutes;
    property OnCreateRouteClass: TBrookRouterCreatePanelClassEvent read
      FOnCreateRouteClass write FOnCreateRouteClass;
  end;

implementation

{ TBrookRoute }

constructor TBrookRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVariables := TBrookStringMap.Create(@Fvars);
  SetPattern(MakePattern);
end;

destructor TBrookRoute.Destroy;
begin
  FVariables.ClearOnDestroy := False;
  FVariables.Free;
  inherited Destroy;
end;

class procedure TBrookRoute.DoRouteCallback(Acls: Pcvoid; Aroute: Psg_route);
var
  RT: TBrookRoute absolute Acls;
begin
  RT.FHandle := Aroute;
  RT.DoMatch(RT);
end;

class function TBrookRoute.DoGetSegmentsCallback(Acls: Pcvoid;
  const Asegment: Pcchar): cint;
var
  VSegments: ^TArray<string> absolute Acls;
begin
{$IFDEF VER3_0}
  SetLength(VSegments^, Succ(Length(VSegments^)));
  VSegments^[High(VSegments^)] := TMarshal.ToString(Asegment);
{$ELSE}
  VSegments^ := VSegments^ + [TMarshal.ToString(Asegment)];
{$ENDIF}
  Result := 0;
end;

class function TBrookRoute.DoGetVarsCallback(Acls: Pcvoid; const Aname: Pcchar;
  const Aval: Pcchar): cint;
begin
  TBrookStringMap(Acls).Add(TMarshal.ToString(Aname), TMarshal.ToString(Aval));
  Result := 0;
end;

function TBrookRoute.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookRoute.GetSegments: TArray<string>;
begin
  if (not Assigned(FHandle)) or (Length(FSegments) > 0) then
    Exit(FSegments);
  SgCheckLibrary;
  SgCheckLastError(sg_route_get_segments(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoGetSegmentsCallback, @FSegments));
  Result := FSegments;
end;

function TBrookRoute.GetVariables: TBrookStringMap;
begin
  Result := FVariables;
  if (not Assigned(FHandle)) or (not FVariables.IsEmpty) then
    Exit;
  SgCheckLastError(sg_route_get_vars(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoGetVarsCallback, FVariables));
end;

function TBrookRoute.GetRegexHandle: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  Result := sg_route_handle(FHandle);
end;

function TBrookRoute.GetPatternRaw: string;
begin
  if not Assigned(FHandle) then
  begin
    if FPattern.IsEmpty then
      Exit('');
    Exit(Concat('^', FPattern, '$'));
  end;
  SgCheckLibrary;
  Result := TMarshal.ToString(sg_route_pattern_raw(FHandle));
end;

function TBrookRoute.GetPattern: string;
var
  P: Pcchar;
begin
  if not Assigned(FHandle) then
    Exit(FPattern);
  SgCheckLibrary;
  P := sg_route_pattern(FHandle);
  try
    Result := TMarshal.ToString(P);
  finally
    sg_free(P);
  end;
end;

procedure TBrookRoute.SetPattern(const AValue: string);
var
  RT: TBrookRoute;
begin
  if AValue = FPattern then
    Exit;
  FPattern := BrookFixPath(AValue);
  if (not Assigned(Owner)) or (not (Owner is TBrookRoutes)) then
    Exit;
  RT := TBrookRoutes(Owner).Find(FPattern);
  if Assigned(RT) and (RT <> Self) then
    raise EBrookRoutes.CreateResFmt(@SBrookRouteAlreadyExists,
      [GetNamePath, FPattern]);
end;

function TBrookRoute.GetPath: string;
begin
  if not Assigned(FHandle) then
    Exit('');
  SgCheckLibrary;
  Result := TMarshal.ToString(sg_route_path(FHandle));
end;

function TBrookRoute.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  Result := sg_route_user_data(FHandle);
end;

procedure TBrookRoute.DoMatch(ARoute: TBrookRoute);
begin
  if Assigned(FOnMath) then
    FOnMath(ARoute);
end;

function TBrookRoute.MakePattern: string;
begin
  Result := Concat('/', string(ClassName).SubString(6).ToLower, Index.ToString);
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
  inherited Create(AOwner, GetRouterClass);
end;

class function TBrookRoutes.GetRouterClass: TBrookRouteClass;
begin
  Result := TBrookRoute;
end;

function TBrookRoutes.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookRoutes.GetEnumerator: TBrookRoutesEnumerator;
begin
  Result := TBrookRoutesEnumerator.Create(Self);
end;

procedure TBrookRoutes.Prepare;
const
  BUF_LEN = 256;
var
  RT: TBrookRoute;
  H: Psg_route;
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
      R := sg_routes_add2(@FHandle, @H, M.ToCNullable(RT.Pattern), P, BUF_LEN,
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

function TBrookRoutes.IndexOf(const APattern: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Pattern, APattern) then
      Exit;
  Result := -1;
end;

function TBrookRoutes.Find(const APattern: string): TBrookRoute;
var
  RT: TBrookRoute;
begin
  for RT in Self do
    if SameText(RT.Pattern, APattern) then
      Exit(RT);
  Result := nil;
end;

function TBrookRoutes.GetItem(AIndex: Integer): TBrookRoute;
begin
  Result := TBrookRoute(inherited Items[AIndex]);
end;

procedure TBrookRoutes.SetItem(AIndex: Integer; AValue: TBrookRoute);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TBrookRoutes.Clear;
begin
  inherited Clear;
  SgCheckLibrary;
  SgCheckLastError(sg_routes_clear(@FHandle));
end;

{ TBrookRouter }

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

procedure TBrookRouter.SetEntryPoint(const AValue: string);
begin
  if FEntryPoint = AValue then
    Exit;
  FEntryPoint := AValue;
  if not FEntryPoint.IsEmpty then
    FEntryPoint := BrookFixPath(FEntryPoint);
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
  P: string;
  R: cint;
begin
  if APath.IsEmpty then
    raise EArgumentException.CreateRes(@SBrookEmptyPath);
  CheckActive;
  SgCheckLibrary;
  P := BrookFixPath(APath);
  if not FEntryPoint.IsEmpty then
    P := Concat(FEntryPoint, P);
  R := sg_router_dispatch(FHandle, M.ToCNullable(P), AUserData);
  Result := R = 0;
  if (not Result) and (R <> ENOENT) then
    SgCheckLastError(R);
end;

end.
