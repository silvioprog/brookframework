(*
  Brook Router unit.

  Copyright (C) 2013 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookRouter;

{$i brook.inc}

interface

uses
  BrookClasses, BrookException, BrookAction, BrookUtils, BrookConsts,
  BrookMessages, BrookHTTPConsts, HTTPDefs, FPJSON, Classes, SysUtils, StrUtils;

type
  { Handles exceptions for @link(TBrookRoutes). }
  EBrookRoutes = class(EBrook);

  { Handles exceptions for @link(TBrookRouter). }
  EBrookRouter = class(EBrook);

  { Is a metaclass for @link(TBrookRoutes) class. }
  TBrookRoutesClass = class of TBrookRoutes;

  { Is a metaclass for @link(TBrookRouter) class. }
  TBrookRouterClass = class of TBrookRouter;

  { Defines a route item. }
  TBrookRoute = record
    { Specifies the class of the action to be called. }
    ActionClass: TBrookActionClass;
    { Checks if the action is default. }
    Default: Boolean;
    { Specifies a HTTP request method of the action to be called. }
    Method: TBrookRequestMethod;
    { Specifies the patter of the action to be called. }
    Pattern: string;
  end;
  { Defines a pointer to the route item.}
  PBrookRoute = ^TBrookRoute;

  { Defines a list of routes. }
  TBrookRoutes = class(TBrookObject)
  private
    FList: TFPList;
    function GetItems(const AIndex: Integer): PBrookRoute;
    procedure SetItems(const AIndex: Integer; const AValue: PBrookRoute);
  public
    { Creates an instance of a @link(TBrookRoutes) class. }
    constructor Create;
    { Frees an instance of @link(TBrookRoutes) class. }
    destructor Destroy; override;
    { Returns the number of registered routes. }
    function Count: Integer;
    { Adds a route item. }
    function Add(AActionClass: TBrookActionClass; const APattern: string;
      const AMethod: TBrookRequestMethod; const ADefault: Boolean): Integer;
    { Get the default action class. }
    procedure GetDefaultActionClass(out AClass: TBrookActionClass;
      out AIndex: Integer);
    { Get the registered pattern of a class. }
    function PatternByActionClass(AClass: TBrookActionClass): string;
    { Get the action class from a patter. }
    function ActionClassByPattern(const APattern: string): TBrookActionClass;
    { Get an action class from its class name. }
    function ActionClassByClassName(const AName: string): TBrookActionClass;
    { The list of routes. }
    property Items[const AIndex: Integer]: PBrookRoute read GetItems
      write SetItems; default;
    { The router pointers list. }
    property List: TFPList read FList;
  end;

  { Provides features for the route handling. }
  TBrookRouter = class(TBrookObject)
  private
    FRoutes: TBrookRoutes;
  protected
    class function CreateRoutes: TBrookRoutes; virtual;
    class procedure FreeRoutes(ARoutes: TBrookRoutes); virtual;
    class function DoCreateAction(
      out AActionClass: TBrookActionClass): TBrookAction; virtual;
    class procedure DoFreeAction(AAction: TBrookAction); virtual;
    class procedure DoExecuteAction(AAction: TBrookAction; ARequest: TRequest;
      AResponse: TResponse; ANames, AValues: TBrookArrayOfString); virtual;
  public
    { Creates an instance of a @link(TBrookRouter) class. }
    constructor Create; virtual;
    { Frees an instance of @link(TBrookRouter) class. }
    destructor Destroy; override;
    { Return the service class provided by this class. }
    class function GetServiceClass: TBrookRouterClass;
    { Registers the service provided by this class. }
    class procedure RegisterService;
    { Unregisters the service  provided by this class. }
    class procedure UnregisterService;
    { Return a instance of this class. }
    class function Service: TBrookRouter;
    { Return the root URL. }
    class function RootUrl: string;
    { Return the root URL passing @code(TRequest) as param. }
    class function RootUrl(ARequest: TRequest): string;
    { Sends the HTTP "NotAllowed" status code to the response. }
    class procedure MethodNotAllowed(AResponse: TResponse);
    { Creates an URL for an action informing an array of parameters. Exemple:

      @longCode(
      procedure TMyAction.Get;
      begin
        // When calling with http://localhost/cgi-bin/cgi1/foo/myvalue
        // the output will be /cgi-bin/cgi1/foo/myvalue
        Write(UrlFor(TMyAction, ['myvalue']));
      end;

      initialization
        TMyAction.Register('/foo/:myvar');) }
    function UrlFor(AActionClass: TBrookActionClass;
      const AParams: array of string): string; overload;
    { Creates an URL for an action passing an array of parameters however
      informing the class name as string }
    function UrlFor(AClassName: string;
      const AParams: array of string): string; overload;
    { Creates an URL for an action passing a JSON data. }
    function UrlFor(AActionClass: TBrookActionClass;
      const AParams: TJSONData): string; overload;
    { Creates an URL for an action passing a JSON data however informing the
      class name as string. }
    function UrlFor(AClassName: string;
      const AParams: TJSONData): string; overload;
    { Adds an slash to the end of the URL if does not exist. }
    function Canonicalize(ARequest: TRequest; AResponse: TResponse): Boolean;
    { Checks if the given parameters match with a registered route. }
    function MatchPattern(APattern, APathInfo: string; out ARedirect: Boolean;
      out ANames, AValues: TBrookArrayOfString): Boolean; virtual;
    { Runs the route processing. }
    procedure Route(ARequest: TRequest; AResponse: TResponse); virtual;
    { List of available routes. }
    property Routes: TBrookRoutes read FRoutes write FRoutes;
  end;

implementation

var
  _BrookRouterService: TBrookRouter = nil;
  _BrookRouterServiceClass: TBrookRouterClass = nil;

{ TBrookRoutes }

constructor TBrookRoutes.Create;
begin
  FList := TFPList.Create;
end;

destructor TBrookRoutes.Destroy;
var
  P: PBrookRoute;
begin
  for P in FList do
    Dispose(P);
  FList.Free;
  inherited Destroy;
end;

function TBrookRoutes.Count: Integer;
begin
  Result := FList.Count;
end;

function TBrookRoutes.GetItems(const AIndex: Integer): PBrookRoute;
begin
  Result := FList.Items[AIndex];
end;

procedure TBrookRoutes.SetItems(const AIndex: Integer; const AValue: PBrookRoute);
begin
  FList.Items[AIndex] := AValue;
end;

function TBrookRoutes.Add(AActionClass: TBrookActionClass;
  const APattern: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean): Integer;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
  begin
    if BrookSettings.Mapped then
    begin
      if (PRoute^.ActionClass = AActionClass) and
        (PRoute^.Pattern = APattern) and (PRoute^.Method = AMethod) then
        raise EBrookRoutes.CreateFmt(Self, SBrookActionAlreadyRegisteredError,
          [AActionClass]);
      if (PRoute^.Pattern = APattern) and (PRoute^.Method = AMethod) then
        raise EBrookRoutes.CreateFmt(Self, SBrookPatternAlreadyRegisteredError,
          [APattern]);
    end
    else
    begin
      if (PRoute^.ActionClass = AActionClass) and
        (PRoute^.Pattern = APattern) then
        raise EBrookRoutes.CreateFmt(Self, SBrookActionAlreadyRegisteredError,
          [AActionClass]);
      if PRoute^.Pattern = APattern then
        raise EBrookRoutes.CreateFmt(Self, SBrookPatternAlreadyRegisteredError,
          [APattern]);
    end;
    if ADefault and PRoute^.Default and (PRoute^.ActionClass <> AActionClass) then
      raise EBrookRoutes.Create(Self, SBrookDefaultActionAlreadyRegisteredError);
  end;
  New(PRoute);
  PRoute^.ActionClass := AActionClass;
  PRoute^.Default := ADefault;
  PRoute^.Method := AMethod;
  PRoute^.Pattern := APattern;
  Result := FList.Add(PRoute);
end;

procedure TBrookRoutes.GetDefaultActionClass(out AClass: TBrookActionClass;
  out AIndex: Integer);
var
  I: Integer;
  PRoute: PBrookRoute;
begin
  for I := 0 to Pred(FList.Count) do
  begin
    PRoute := FList[I];
    if PRoute^.Default then
    begin
      AIndex := I;
      AClass := PRoute^.ActionClass;
      Exit;
    end;
  end;
  AIndex := -1;
  AClass := nil;
end;

function TBrookRoutes.PatternByActionClass(AClass: TBrookActionClass): string;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
    if PRoute^.ActionClass = AClass then
    begin
      Result := PRoute^.Pattern;
      Exit;
    end;
  Result := ES;
end;

function TBrookRoutes.ActionClassByPattern(
  const APattern: string): TBrookActionClass;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
    if PRoute^.Pattern = APattern then
    begin
      Result := PRoute^.ActionClass;
      Exit;
    end;
  Result := nil;
end;

function TBrookRoutes.ActionClassByClassName(
  const AName: string): TBrookActionClass;
var
  PRoute: PBrookRoute;
begin
  for PRoute in FList do
    if SameText(PRoute^.ActionClass.ClassName, AName) then
    begin
      Result := PRoute^.ActionClass;
      Exit;
    end;
  Result := nil;
end;

{ TBrookRouter }

constructor TBrookRouter.Create;
begin
  FRoutes := CreateRoutes;
end;

destructor TBrookRouter.Destroy;
begin
  FreeRoutes(FRoutes);
  inherited Destroy;
end;

class function TBrookRouter.GetServiceClass: TBrookRouterClass;
begin
  Result := _BrookRouterServiceClass;
end;

class function TBrookRouter.CreateRoutes: TBrookRoutes;
begin
  Result := TBrookRoutes.Create;
end;

class procedure TBrookRouter.FreeRoutes(ARoutes: TBrookRoutes);
begin
  FreeAndNil(ARoutes);
end;

class function TBrookRouter.DoCreateAction(
  out AActionClass: TBrookActionClass): TBrookAction;
begin
  Result := AActionClass.Create;
end;

class procedure TBrookRouter.DoFreeAction(AAction: TBrookAction);
begin
  AAction.Free;
end;

class procedure TBrookRouter.DoExecuteAction(AAction: TBrookAction;
  ARequest: TRequest; AResponse: TResponse; ANames, AValues: TBrookArrayOfString);
begin
  AAction.FillFields(ARequest);
  AAction.FillParams(ARequest);
  AAction.FillValues(ANames, AValues);
  AAction.DoRequest(ARequest, AResponse);
end;

class procedure TBrookRouter.RegisterService;
begin
  if Assigned(_BrookRouterServiceClass) then
    raise EBrookRouter.Create(Self, SBrookRouterAlreadyRegisteredError);
  _BrookRouterServiceClass := Self;
end;

class procedure TBrookRouter.UnregisterService;
begin
  FreeAndNil(_BrookRouterService);
  _BrookRouterServiceClass := nil;
end;

class function TBrookRouter.Service: TBrookRouter;
begin
  if not Assigned(_BrookRouterService) then
  begin
    if not Assigned(_BrookRouterServiceClass) then
      raise EBrookRouter.Create(Self, SBrookNoRouterRegisteredError);
    _BrookRouterService := _BrookRouterServiceClass.Create;
  end;
  Result := _BrookRouterService;
end;

class function TBrookRouter.RootUrl: string;
begin
  if BrookSettings.RootUrl = ES then
    Result := GetEnvironmentVariable(BROOK_SRV_ENV_SCRIPT_NAME)
  else
    Result := BrookSettings.RootUrl;
end;

class function TBrookRouter.RootUrl(ARequest: TRequest): string;
begin
  if BrookSettings.RootUrl = ES then
    Result := ARequest.ScriptName
  else
    Result := BrookSettings.RootUrl;
end;

class procedure TBrookRouter.MethodNotAllowed(AResponse: TResponse);
begin
  AResponse.Code := BROOK_HTTP_STATUS_CODE_METHOD_NOT_ALLOWED;
  AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_METHOD_NOT_ALLOWED;
  AResponse.Contents.Add(SBrookMethodNotAllowedError);
end;

function TBrookRouter.UrlFor(AActionClass: TBrookActionClass;
  const AParams: array of string): string;
var
  S, VVal: string;
  I, B, E: Integer;
begin
  Result := ES;
  S := FRoutes.PatternByActionClass(AActionClass);
  if Length(S) = 0 then
    Exit;
  if S[1] = AK then
    Delete(S, 1, 1);
  for I := 0 to High(AParams) do
  begin
    VVal := HTTPEncode(AParams[I]);
    B := Pos(CO, S);
    if B = 0 then
      B := Pos(AK,S);
    if B <> 0 then
    begin
      E := PosEx(US, S, B);
      if E <> 0 then
      begin
        Delete(S, B, E - B);
        Insert(VVal, S, B);
      end
      else
      begin
        Delete(S, B, MaxInt);
        Insert(VVal, S, MaxInt);
      end;
    end;
  end;
  Result := BrookExcludeHTTPPathDelimiter(TBrookRouter.RootUrl) + S;
end;

function TBrookRouter.UrlFor(AClassName: string;
  const AParams: array of string): string;
begin
  Result := UrlFor(FRoutes.ActionClassByClassName(AClassName), AParams);
end;

function TBrookRouter.UrlFor(AActionClass: TBrookActionClass;
  const AParams: TJSONData): string;
var
  S, VVal: string;
  I, B, E: Integer;
begin
  Result := ES;
  if (AParams.JSONType <> jtArray) and (AParams.JSONType <> jtObject) then
    raise EBrookRouter.CreateFmt(Self, SBrookIncompatibleTypesError,
      [AParams.ClassName, 'TJSONArray or TJSONObject']);
  S := FRoutes.PatternByActionClass(AActionClass);
  if Length(S) = 0 then
    Exit;
  if S[1] = AK then
    Delete(S, 1, 1);
  for I := 0 to Pred(AParams.Count) do
  begin
    case AParams.JSONType of
      jtArray: VVal := HTTPEncode(TJSONArray(AParams).Strings[I]);
      jtObject: VVal := HTTPEncode(TJSONObject(AParams).Items[I].AsString);
    end;
    B := Pos(CO, S);
    if B = 0 then
      B := Pos(AK,S);
    if B <> 0 then
    begin
      E := PosEx(US, S, B);
      if E <> 0 then
      begin
        Delete(S, B, E - B);
        Insert(VVal, S, B);
      end
      else
      begin
        Delete(S, B, MaxInt);
        Insert(VVal, S, MaxInt);
      end;
    end;
  end;
  Result := BrookExcludeHTTPPathDelimiter(TBrookRouter.RootUrl) + S;
end;

function TBrookRouter.UrlFor(AClassName: string;
  const AParams: TJSONData): string;
begin
  Result := UrlFor(FRoutes.ActionClassByClassName(AClassName), AParams);
end;

function TBrookRouter.Canonicalize(ARequest: TRequest; AResponse: TResponse): Boolean;
var
  L: LongInt;
  VURL, VQueryStr: string;
begin
  VQueryStr := ARequest.QueryString;
  if VQueryStr <> ES then
    VQueryStr := QU + VQueryStr;
  VURL := TBrookRouter.RootUrl(ARequest) + ARequest.PathInfo;
  L := Length(VURL);
  Result := ((L > 0) and (VURL[L] <> US)) or (VURL = ES);
  if Result then
    AResponse.SendRedirect(LowerCase(VURL) + US + VQueryStr);
end;

function TBrookRouter.MatchPattern(APattern, APathInfo: string; out
  ARedirect: Boolean; out ANames, AValues: TBrookArrayOfString): Boolean;
var
  VCount: Integer;
  VPt, VPa, VRPt, VRPa: string;
  VPtDelim, VPaDelim, VIsVar, VIsSuf: Boolean;
begin
  Result := False;
  ARedirect := False;
  Delete(APattern, Pos(QU, APattern), MaxInt);
  Delete(APathInfo, Pos(QU, APathInfo), MaxInt);
  if (APattern = '*/') or ((APathInfo = ES) and (APattern = US)) then
  begin
    Result := True;
    ARedirect := True;
    Exit;
  end;
  if ((APathInfo = ES) and (APattern = ES)) or
    ((APathInfo = US) and (APattern = US)) or (APattern = AK) then
  begin
    Result := True;
    Exit;
  end;
  if (APathInfo = US) and (APattern = ES) then
    Exit;
  VPa := ES;
  VPt := ES;
  VRPa := ES;
  VRPt := ES;
  VPaDelim := False;
  VPtDelim := False;
  VCount := 1;
  while True do
  begin
    BrookExtractPathLevels(APattern, VRPt, VPt, VPtDelim);
    BrookExtractPathLevels(APathInfo, VRPa, VPa, VPaDelim);
    if VPa = ES then
      if VPt <> ES then
      begin
        if Pos(AK, VPt) <> 1 then begin
          Result := False;
          Exit;
        end;
      end
      else
        Break;
    if VPt = AK + AK then
      Exit;
    if VPt = AK then
      Continue;
    VIsVar := Pos(CO, VPt) = 1;
    VIsSuf := Pos(AK, VPt) = 1;
    if VIsVar then
      Result := not (((VPa = ES) and (VPt <> ES)) or ((VPa <> ES) and (VPt = ES)))
    else
    if VIsSuf then
      Result := True
    else
      Result := VPa = VPt;
    if not Result then
    begin
      ARedirect := False;
      Exit;
    end;
    if not VIsVar and not VIsSuf then
      Continue;
    SetLength(ANames, VCount);
    SetLength(AValues, VCount);
    if VIsVar then begin
      ANames[VCount - 1] := Copy(VPt, 2, MaxInt);
      AValues[VCount - 1] := VPa;
      Inc(VCount);
    end else begin
      ANames[VCount - 1] := Copy(VPt, 2, MaxInt);
      AValues[VCount - 1] := Copy(APathInfo, 1+Length(VRPa)-Length(VPa)+1, MaxInt);
      Exit;
    end;
  end;
  if VPtDelim then
    ARedirect := not VPaDelim
  else
    Result := VPaDelim = VPtDelim;
end;

procedure TBrookRouter.Route(ARequest: TRequest; AResponse: TResponse);
var
  I, C: Integer;
  PRoute: PBrookRoute;
  VNames, VValues: TBrookArrayOfString;
  VAct: TBrookAction;
  VActClass: TBrookActionClass = nil;
  VDefaultActClass: TBrookActionClass = nil;
  VRedirect, VMatchMethod, VMatchPattern: Boolean;
begin
  C := FRoutes.List.Count;
  if C = 0 then
    raise EBrookRouter.Create(Self, SBrookNoRouteRegisteredError);
  FRoutes.GetDefaultActionClass(VDefaultActClass, I);
  if I > -1 then
    FRoutes.List.Move(I, C - 1);
  if BrookSettings.Mapped then
  begin
    VMatchMethod := False;
    VMatchPattern := False;
    for PRoute in FRoutes.List do
      if MatchPattern(PRoute^.Pattern, ARequest.PathInfo, VRedirect,
        VNames, VValues) then
      begin
        if VRedirect and Canonicalize(ARequest, AResponse) then
          Exit;
        VMatchPattern := True;
        if not BrookMatchMethod(PRoute^.Method, ARequest.Method) then
          Continue;
        VMatchMethod := True;
        VActClass := PRoute^.ActionClass;
        if PRoute^.Method <> rmAll then
          Break;
      end;
    if VMatchPattern then
    begin
      if VMatchMethod then
      begin
        if not Assigned(VActClass) then
          if Assigned(VDefaultActClass) then
            VActClass := VDefaultActClass;
      end
      else
      begin
        TBrookRouter.MethodNotAllowed(AResponse);
        Exit;
      end;
    end
    else
      raise EBrookHTTP404.Create(ARequest.PathInfo);
  end
  else
  begin
    for PRoute in FRoutes.List do
      if MatchPattern(PRoute^.Pattern, ARequest.PathInfo, VRedirect,
        VNames, VValues) then
      begin
        if VRedirect and Canonicalize(ARequest, AResponse) then
          Exit;
        VActClass := PRoute^.ActionClass;
        Break;
      end;
    if not Assigned(VActClass) then
      if Assigned(VDefaultActClass) then
        VActClass := VDefaultActClass
      else
        raise EBrookHTTP404.Create(ARequest.PathInfo);
  end;
  VAct := DoCreateAction(VActClass);
  try
    DoExecuteAction(VAct, ARequest, AResponse, VNames, VValues);
  finally
    DoFreeAction(VAct);
  end;
end;

initialization
  TBrookRouter.RegisterService;

finalization
  TBrookRouter.UnregisterService;

end.
