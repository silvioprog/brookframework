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
  SBrookNoRoutesDefined = 'No routes defined.';
  SBrookEmptyPattern = '%s: pattern cannot be empty.';
  SBrookRouteAlreadyExists = '%s: route ''%s'' already exists.';
  SBrookEmptyPath = 'Path cannot be empty.';

type
  TBrookCustomRoute = class;

  TBrookCustomRouteClass = class of TBrookCustomRoute;

  TBrookRoutes = class;

  TBrookCustomRouter = class;

  TBrookRouterCreatePanelClassEvent = procedure(ASender: TBrookCustomRouter;
    var ARouteClass: TBrookCustomRouteClass) of object;

  TBrookRouteMatchEvent = procedure(ARoute: TBrookCustomRoute) of object;

  EBrookRoute = class(Exception);

  EBrookRoutes = class(Exception);

  EBrookRouter = class(Exception);

  EBrookOpNotAllowedInactiveRouter = class(Exception);

  TBrookCustomRoute = class(TBrookHandleCollectionItem)
  private
    FRoutes: TBrookRoutes;
    FVariables: TBrookStringMap;
    FOnMath: TBrookRouteMatchEvent;
    FPattern: string;
    FHandle: Psg_route;
    Fvars: Psg_strmap;
    function GetSegments: TArray<string>; inline;
    function GetPattern: string;
    function GetPath: string;
    function GetPatternRaw: string;
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
    procedure DoMatch(ARoute: TBrookCustomRoute); virtual;
    property Routes: TBrookRoutes read FRoutes;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Validate; inline;
    property RegexHandle: Pointer read GetRegexHandle;
    property Segments: TArray<string> read GetSegments;
    property Variables: TBrookStringMap read GetVariables;
    property Pattern: string read GetPattern write SetPattern;
    property PatternRaw: string read GetPatternRaw;
    property Path: string read GetPath;
    property UserData: Pointer read GetUserData;
    property OnMath: TBrookRouteMatchEvent read FOnMath write FOnMath;
  end;

  TBrookRoute = class(TBrookCustomRoute)
  published
    property Pattern;
    property Path;
    property OnMath;
  end;

  TBrookRoutesEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookCustomRoute;
    property Current: TBrookCustomRoute read GetCurrent;
  end;

  TBrookRoutes = class(TBrookHandleOwnedCollection)
  private
    FHandle: Psg_route;
    function GetItem(AIndex: Integer): TBrookCustomRoute;
    procedure SetItem(AIndex: Integer; AValue: TBrookCustomRoute);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    class function GetRouterClass: TBrookCustomRouteClass; virtual;
    function GetEnumerator: TBrookRoutesEnumerator;
    function MakePattern: string; virtual;
    procedure Prepare; virtual;
    function Add: TBrookCustomRoute; virtual;
    function First: TBrookCustomRoute; virtual;
    function Last: TBrookCustomRoute; virtual;
    function IndexOf(const APattern: string): Integer; virtual;
    function Find(const APattern: string): TBrookCustomRoute; virtual;
    function Remove(const APattern: string): Boolean; virtual;
    procedure Clear; virtual;
    property Items[AIndex: Integer]: TBrookCustomRoute read GetItem
      write SetItem; default;
  end;

  TBrookCustomRouter = class(TBrookHandledComponent)
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
    property Active: Boolean read FActive write SetActive stored IsActive;
    property EntryPoint: string read FEntryPoint write SetEntryPoint;
    property Routes: TBrookRoutes read FRoutes write SetRoutes;
    property OnCreateRouteClass: TBrookRouterCreatePanelClassEvent read
      FOnCreateRouteClass write FOnCreateRouteClass;
  end;

  TBrookRouter = class(TBrookCustomRouter)
  published
    property Active;
    property EntryPoint;
    property Routes;
    property OnCreateRouteClass;
  end;

implementation

{ TBrookCustomRoute }

constructor TBrookCustomRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVariables := TBrookStringMap.Create(@Fvars);
  if Assigned(ACollection) and (ACollection is TBrookRoutes) then
  begin
    FRoutes := TBrookRoutes(ACollection);
    FPattern := FRoutes.MakePattern;
  end
  else
    SetPattern('/');
end;

destructor TBrookCustomRoute.Destroy;
begin
  FVariables.ClearOnDestroy := False;
  FVariables.Free;
  inherited Destroy;
end;

class procedure TBrookCustomRoute.DoRouteCallback(Acls: Pcvoid;
  Aroute: Psg_route);
var
  RT: TBrookCustomRoute;
begin
  RT := Acls;
  RT.FHandle := Aroute;
  RT.DoMatch(RT);
end;

class function TBrookCustomRoute.DoGetSegmentsCallback(Acls: Pcvoid;
  const Asegment: Pcchar): cint;
var
  VSegments: ^TArray<string>;
begin
  VSegments := Acls;
{$IFDEF VER3_0}
  SetLength(VSegments^, Succ(Length(VSegments^)));
  VSegments^[High(VSegments^)] := TMarshal.ToString(Asegment);
{$ELSE}
  VSegments^ := VSegments^ + [TMarshal.ToString(Asegment)];
{$ENDIF}
  Result := 0;
end;

class function TBrookCustomRoute.DoGetVarsCallback(Acls: Pcvoid;
  const Aname: Pcchar; const Aval: Pcchar): cint;
begin
  TBrookStringMap(Acls).Add(TMarshal.ToString(Aname), TMarshal.ToString(Aval));
  Result := 0;
end;

function TBrookCustomRoute.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookCustomRoute.GetSegments: TArray<string>;
begin
  Result := nil;
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  SgCheckLastError(sg_route_get_segments(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoGetSegmentsCallback, @Result));
end;

function TBrookCustomRoute.GetVariables: TBrookStringMap;
begin
  Result := FVariables;
  FVariables.Clear;
  if not Assigned(FHandle) then
    Exit;
  SgCheckLastError(sg_route_get_vars(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoGetVarsCallback, FVariables));
end;

function TBrookCustomRoute.GetRegexHandle: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  Result := sg_route_handle(FHandle);
end;

function TBrookCustomRoute.GetPatternRaw: string;
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

function TBrookCustomRoute.GetPattern: string;
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

procedure TBrookCustomRoute.SetPattern(const AValue: string);
var
  RT: TBrookCustomRoute;
begin
  if AValue = FPattern then
    Exit;
  FPattern := BrookFixPath(AValue);
  if not Assigned(FRoutes) then
    Exit;
  RT := FRoutes.Find(FPattern);
  if Assigned(RT) and (RT <> Self) then
    raise EBrookRoutes.CreateResFmt(@SBrookRouteAlreadyExists,
      [GetNamePath, FPattern]);
end;

function TBrookCustomRoute.GetPath: string;
begin
  if not Assigned(FHandle) then
    Exit('');
  SgCheckLibrary;
  Result := TMarshal.ToString(sg_route_path(FHandle));
end;

function TBrookCustomRoute.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  Result := sg_route_user_data(FHandle);
end;

procedure TBrookCustomRoute.DoMatch(ARoute: TBrookCustomRoute);
begin
  if Assigned(FOnMath) then
    FOnMath(ARoute);
end;

procedure TBrookCustomRoute.Validate;
begin
  if FPattern.IsEmpty then
    raise EBrookRoute.CreateResFmt(@SBrookEmptyPattern, [GetNamePath]);
end;

{ TBrookRoutesEnumerator }

function TBrookRoutesEnumerator.GetCurrent: TBrookCustomRoute;
begin
  Result := TBrookCustomRoute(inherited GetCurrent);
end;

{ TBrookRoutes }

constructor TBrookRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetRouterClass);
end;

class function TBrookRoutes.GetRouterClass: TBrookCustomRouteClass;
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

function TBrookRoutes.MakePattern: string;
var
  VIndex: Integer;
begin
  VIndex := 1;
  repeat
    Result := Concat('/', string(ClassName).SubString(6).ToLower,
      VIndex.ToString);
    Inc(VIndex);
  until IndexOf(Result) < 0;
end;

procedure TBrookRoutes.Prepare;
var
  RT: TBrookCustomRoute;
  H: Psg_route;
  M: TMarshaller;
  P: array[0..255] of cchar;
  S: string;
  R: cint;
begin
  if Count = 0 then
    raise EBrookRoutes.CreateRes(@SBrookNoRoutesDefined);
  SgCheckLibrary;
  SgCheckLastError(sg_routes_clear(@FHandle));
  for RT in Self do
  begin
    RT.Validate;
    P[0] := 0;
    R := sg_routes_add2(@FHandle, @H, M.ToCNullable(RT.Pattern), @P[0], SizeOf(P),
{$IFNDEF VER3_0}@{$ENDIF}RT.DoRouteCallback, RT);
    if R = 0 then
      Continue;
    if R = EALREADY then
      raise EBrookRoutes.CreateResFmt(@SBrookRouteAlreadyExists,
        [RT.GetNamePath, RT.Pattern]);
    S := TMarshal.ToString(@P[0]).TrimRight;
    if S.IsEmpty then
      S := BrookStrError(R);
    raise EBrookRoutes.Create(S);
  end;
end;

function TBrookRoutes.Add: TBrookCustomRoute;
begin
  Result := TBrookRoute(inherited Add);
end;

function TBrookRoutes.First: TBrookCustomRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(0);
end;

function TBrookRoutes.Last: TBrookCustomRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(Pred(Count));
end;

function TBrookRoutes.IndexOf(const APattern: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Pattern, APattern) then
      Exit;
  Result := -1;
end;

function TBrookRoutes.Find(const APattern: string): TBrookCustomRoute;
var
  RT: TBrookCustomRoute;
begin
  for RT in Self do
    if SameText(RT.Pattern, APattern) then
      Exit(RT);
  Result := nil;
end;

function TBrookRoutes.Remove(const APattern: string): Boolean;
var
  I: Integer;
begin
  I := IndexOf(APattern);
  Result := I > -1;
  if Result then
    inherited Delete(I);
end;

function TBrookRoutes.GetItem(AIndex: Integer): TBrookCustomRoute;
begin
  Result := TBrookCustomRoute(inherited GetItem(AIndex));
end;

procedure TBrookRoutes.SetItem(AIndex: Integer; AValue: TBrookCustomRoute);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TBrookRoutes.Clear;
begin
  inherited Clear;
  SgCheckLibrary;
  SgCheckLastError(sg_routes_clear(@FHandle));
end;

{ TBrookCustomRouter }

constructor TBrookCustomRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := CreateRoutes;
end;

destructor TBrookCustomRouter.Destroy;
begin
  FRoutes.Free;
  try
    SetActive(False);
  finally
    inherited Destroy;
  end;
end;

function TBrookCustomRouter.CreateRoutes: TBrookRoutes;
begin
  Result := TBrookRoutes.Create(Self);
end;

procedure TBrookCustomRouter.Loaded;
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

function TBrookCustomRouter.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookCustomRouter.SetRoutes(AValue: TBrookRoutes);
begin
  FRoutes.Assign(AValue);
end;

function TBrookCustomRouter.IsActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookCustomRouter.SetActive(AValue: Boolean);
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

procedure TBrookCustomRouter.SetEntryPoint(const AValue: string);
begin
  if FEntryPoint = AValue then
    Exit;
  FEntryPoint := AValue;
  if not FEntryPoint.IsEmpty then
    FEntryPoint := BrookFixPath(FEntryPoint);
end;

procedure TBrookCustomRouter.DoOpen;
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

procedure TBrookCustomRouter.DoClose;
begin
  if not Assigned(FHandle) then
    Exit;
  SgCheckLibrary;
  sg_router_free(FHandle);
  FHandle := nil;
  FActive := False;
end;

procedure TBrookCustomRouter.Open;
begin
  SetActive(True);
end;

procedure TBrookCustomRouter.Close;
begin
  SetActive(False);
end;

procedure TBrookCustomRouter.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EBrookOpNotAllowedInactiveRouter.CreateRes(
      @SBrookOpNotAllowedInactiveRouter);
end;

function TBrookCustomRouter.Route(const APath: string;
  AUserData: Pointer): Boolean;
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
