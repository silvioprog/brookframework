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

unit BrookPathRouter;

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
  SBrookRouteAlreadyExists = '%s: pattern ''%s'' already exists in ''%s''.';
  SBrookEmptyPath = 'Path cannot be empty.';

type
  TBrookCustomPathRoute = class;

  TBrookCustomPathRouteClass = class of TBrookCustomPathRoute;

  TBrookPathRoutes = class;

  TBrookCustomPathRouter = class;

  TBrookPathRouteMatchEvent = procedure(
    ARoute: TBrookCustomPathRoute) of object;

  EBrookRoute = class(Exception);

  EBrookRoutes = class(Exception);

  EBrookRouter = class(Exception);

  EBrookOpNotAllowedInactiveRouter = class(Exception);

  TBrookCustomPathRoute = class(TBrookHandleCollectionItem)
  private
    FRoutes: TBrookPathRoutes;
    FVariables: TBrookStringMap;
    FHandle: Psg_route;
    Fvars: Psg_strmap;
    FPattern: string;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnMath: TBrookPathRouteMatchEvent;
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
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DoMatch(ARoute: TBrookCustomPathRoute); virtual;
    property Routes: TBrookPathRoutes read FRoutes;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Validate; inline;
    property RegexHandle: Pointer read GetRegexHandle;
    property Segments: TArray<string> read GetSegments;
    property Variables: TBrookStringMap read GetVariables;
    property Pattern: string read GetPattern write SetPattern;
    property PatternRaw: string read GetPatternRaw;
    property Path: string read GetPath;
    property UserData: Pointer read GetUserData;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnMath: TBrookPathRouteMatchEvent read FOnMath write FOnMath;
  end;

  TBrookPathRoute = class(TBrookCustomPathRoute)
  published
    property Pattern;
    property Path;
    property OnCreate;
    property OnDestroy;
    property OnMath;
  end;

  TBrookPathRoutesEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookPathRoute;
    property Current: TBrookPathRoute read GetCurrent;
  end;

  TBrookPathRoutes = class(TBrookHandleOwnedCollection)
  private
    FHandle: Psg_route;
    function GetItem(AIndex: Integer): TBrookCustomPathRoute;
    procedure SetItem(AIndex: Integer; AValue: TBrookCustomPathRoute);
  protected
    function GetHandle: Pointer; override;
    class function GetRoutePattern(
      ARoute: TBrookCustomPathRoute): string; virtual;
  public
    constructor Create(AOwner: TPersistent); virtual;
    class function GetRouterClass: TBrookCustomPathRouteClass; virtual;
    function GetEnumerator: TBrookPathRoutesEnumerator;
    procedure Assign(ASource: TPersistent); override;
    function MakePattern: string; virtual;
    procedure Prepare; virtual;
    function Add: TBrookCustomPathRoute; virtual;
    function First: TBrookCustomPathRoute; virtual;
    function Last: TBrookCustomPathRoute; virtual;
    function IndexOf(const APattern: string): Integer; virtual;
    function Find(const APattern: string): TBrookCustomPathRoute; virtual;
    function Remove(const APattern: string): Boolean; virtual;
    procedure Clear; virtual;
    property Items[AIndex: Integer]: TBrookCustomPathRoute read GetItem
      write SetItem; default;
  end;

  TBrookCustomPathRouter = class(TBrookHandledComponent)
  private
    FRoutes: TBrookPathRoutes;
    FHandle: Psg_router;
    FActive: Boolean;
    FStreamedActive: Boolean;
    function IsActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetRoutes(AValue: TBrookPathRoutes);
  protected
    function CreateRoutes: TBrookPathRoutes; virtual;
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
    property Routes: TBrookPathRoutes read FRoutes write SetRoutes;
  end;

  TBrookPathRouter = class(TBrookCustomPathRouter)
  published
    property Active;
    property Routes;
  end;

implementation

{ TBrookCustomPathRoute }

constructor TBrookCustomPathRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVariables := TBrookStringMap.Create(@Fvars);
  if Assigned(ACollection) and (ACollection is TBrookPathRoutes) then
  begin
    FRoutes := TBrookPathRoutes(ACollection);
    FPattern := FRoutes.MakePattern;
  end
  else
    SetPattern('/');
end;

destructor TBrookCustomPathRoute.Destroy;
begin
  FVariables.ClearOnDestroy := False;
  FVariables.Free;
  inherited Destroy;
  DoDestroy;
end;

procedure TBrookCustomPathRoute.AfterConstruction;
begin
  inherited AfterConstruction;
  DoCreate;
end;

procedure TBrookCustomPathRoute.Assign(ASource: TPersistent);
begin
  if ASource is TBrookCustomPathRoute then
    FPattern := (ASource as TBrookCustomPathRoute).FPattern
  else
    inherited Assign(ASource);
end;

class procedure TBrookCustomPathRoute.DoRouteCallback(Acls: Pcvoid;
  Aroute: Psg_route);
var
  RT: TBrookCustomPathRoute;
begin
  RT := Acls;
  RT.FHandle := Aroute;
  RT.DoMatch(RT);
end;

class function TBrookCustomPathRoute.DoGetSegmentsCallback(Acls: Pcvoid;
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

class function TBrookCustomPathRoute.DoGetVarsCallback(Acls: Pcvoid;
  const Aname: Pcchar; const Aval: Pcchar): cint;
begin
  TBrookStringMap(Acls).Add(TMarshal.ToString(Aname), TMarshal.ToString(Aval));
  Result := 0;
end;

function TBrookCustomPathRoute.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookCustomPathRoute.GetSegments: TArray<string>;
begin
  Result := nil;
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  SgCheckLastError(sg_route_get_segments(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoGetSegmentsCallback, @Result));
end;

function TBrookCustomPathRoute.GetVariables: TBrookStringMap;
begin
  Result := FVariables;
  FVariables.Clear;
  if not Assigned(FHandle) then
    Exit;
  SgCheckLastError(sg_route_get_vars(FHandle,
{$IFNDEF VER3_0}@{$ENDIF}DoGetVarsCallback, FVariables));
end;

function TBrookCustomPathRoute.GetRegexHandle: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  Result := sg_route_handle(FHandle);
end;

function TBrookCustomPathRoute.GetPatternRaw: string;
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

function TBrookCustomPathRoute.GetPattern: string;
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

function TBrookCustomPathRoute.GetPath: string;
begin
  if not Assigned(FHandle) then
    Exit('');
  SgCheckLibrary;
  Result := TMarshal.ToString(sg_route_path(FHandle));
end;

function TBrookCustomPathRoute.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(nil);
  SgCheckLibrary;
  Result := sg_route_user_data(FHandle);
end;

procedure TBrookCustomPathRoute.DoCreate;
begin
  if Assigned(FOnCreate) then
    FOnCreate(Self);
end;

procedure TBrookCustomPathRoute.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
end;

procedure TBrookCustomPathRoute.DoMatch(ARoute: TBrookCustomPathRoute);
begin
  if Assigned(FOnMath) then
    FOnMath(ARoute);
end;

procedure TBrookCustomPathRoute.SetPattern(const AValue: string);
var
  RT: TBrookCustomPathRoute;
begin
  if AValue = FPattern then
    Exit;
  FPattern := BrookFixPath(AValue);
  if not Assigned(FRoutes) then
    Exit;
  RT := FRoutes.Find(FPattern);
  if Assigned(RT) and (RT <> Self) then
    raise EBrookRoutes.CreateResFmt(@SBrookRouteAlreadyExists,
      [GetNamePath, FPattern, RT.GetNamePath]);
end;

procedure TBrookCustomPathRoute.Validate;
begin
  if FPattern.IsEmpty then
    raise EBrookRoute.CreateResFmt(@SBrookEmptyPattern, [GetNamePath]);
end;

{ TBrookPathRoutesEnumerator }

function TBrookPathRoutesEnumerator.GetCurrent: TBrookPathRoute;
begin
  Result := TBrookPathRoute(inherited GetCurrent);
end;

{ TBrookPathRoutes }

constructor TBrookPathRoutes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetRouterClass);
end;

class function TBrookPathRoutes.GetRouterClass: TBrookCustomPathRouteClass;
begin
  Result := TBrookPathRoute;
end;

function TBrookPathRoutes.GetHandle: Pointer;
begin
  Result := FHandle;
end;

class function TBrookPathRoutes.GetRoutePattern(
  ARoute: TBrookCustomPathRoute): string;
begin
  Result := ARoute.FPattern;
end;

function TBrookPathRoutes.GetEnumerator: TBrookPathRoutesEnumerator;
begin
  Result := TBrookPathRoutesEnumerator.Create(Self);
end;

procedure TBrookPathRoutes.Assign(ASource: TPersistent);
var
  R: TBrookCustomPathRoute;
begin
  if ASource is TBrookPathRoutes then
  begin
    Clear;
    for R in (ASource as TBrookPathRoutes) do
      Add.Assign(R);
  end
  else
    inherited Assign(ASource);
end;

function TBrookPathRoutes.MakePattern: string;
var
  VIndex: Integer;
begin
  VIndex := 1;
  repeat
    Result := Concat('/path', VIndex.ToString);
    Inc(VIndex);
  until IndexOf(Result) < 0;
end;

procedure TBrookPathRoutes.Prepare;
var
  RT: TBrookCustomPathRoute;
  H: Psg_route;
  M: TMarshaller;
  P: array[0..SG_ERR_SIZE-1] of cchar;
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
    R := sg_routes_add2(@FHandle, @H, M.ToCNullable(GetRoutePattern(RT)), @P[0],
      SG_ERR_SIZE, {$IFNDEF VER3_0}@{$ENDIF}RT.DoRouteCallback, RT);
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

function TBrookPathRoutes.Add: TBrookCustomPathRoute;
begin
  Result := TBrookPathRoute(inherited Add);
end;

function TBrookPathRoutes.First: TBrookCustomPathRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(0);
end;

function TBrookPathRoutes.Last: TBrookCustomPathRoute;
begin
  if Count = 0 then
    Exit(nil);
  Result := GetItem(Pred(Count));
end;

function TBrookPathRoutes.IndexOf(const APattern: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Pattern, APattern) then
      Exit;
  Result := -1;
end;

function TBrookPathRoutes.Find(const APattern: string): TBrookCustomPathRoute;
var
  RT: TBrookCustomPathRoute;
begin
  for RT in Self do
    if SameText(RT.Pattern, APattern) then
      Exit(RT);
  Result := nil;
end;

function TBrookPathRoutes.Remove(const APattern: string): Boolean;
var
  I: Integer;
begin
  I := IndexOf(APattern);
  Result := I > -1;
  if Result then
    inherited Delete(I);
end;

function TBrookPathRoutes.GetItem(AIndex: Integer): TBrookCustomPathRoute;
begin
  Result := TBrookCustomPathRoute(inherited GetItem(AIndex));
end;

procedure TBrookPathRoutes.SetItem(AIndex: Integer;
  AValue: TBrookCustomPathRoute);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TBrookPathRoutes.Clear;
begin
  inherited Clear;
  SgCheckLibrary;
  SgCheckLastError(sg_routes_clear(@FHandle));
end;

{ TBrookCustomPathRouter }

constructor TBrookCustomPathRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoutes := CreateRoutes;
end;

destructor TBrookCustomPathRouter.Destroy;
begin
  FRoutes.Free;
  try
    SetActive(False);
  finally
    inherited Destroy;
  end;
end;

function TBrookCustomPathRouter.CreateRoutes: TBrookPathRoutes;
begin
  Result := TBrookPathRoutes.Create(Self);
end;

procedure TBrookCustomPathRouter.Loaded;
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

function TBrookCustomPathRouter.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookCustomPathRouter.SetRoutes(AValue: TBrookPathRoutes);
begin
  if AValue = FRoutes then
    Exit;
  if Assigned(AValue) then
    FRoutes.Assign(AValue)
  else
    FRoutes.Clear;
end;

function TBrookCustomPathRouter.IsActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookCustomPathRouter.SetActive(AValue: Boolean);
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

procedure TBrookCustomPathRouter.DoOpen;
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

procedure TBrookCustomPathRouter.DoClose;
begin
  if not Assigned(FHandle) then
    Exit;
  SgCheckLibrary;
  sg_router_free(FHandle);
  FHandle := nil;
  FActive := False;
end;

procedure TBrookCustomPathRouter.Open;
begin
  SetActive(True);
end;

procedure TBrookCustomPathRouter.Close;
begin
  SetActive(False);
end;

procedure TBrookCustomPathRouter.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EBrookOpNotAllowedInactiveRouter.CreateRes(
      @SBrookOpNotAllowedInactiveRouter);
end;

function TBrookCustomPathRouter.Route(const APath: string;
  AUserData: Pointer): Boolean;
var
  M: TMarshaller;
  R: cint;
begin
  if APath.IsEmpty then
    raise EArgumentException.CreateRes(@SBrookEmptyPath);
  CheckActive;
  SgCheckLibrary;
  R := sg_router_dispatch(FHandle, M.ToCNullable(BrookFixPath(APath)),
    AUserData);
  Result := R = 0;
  if (not Result) and (R <> ENOENT) then
    SgCheckLastError(R);
end;

end.
