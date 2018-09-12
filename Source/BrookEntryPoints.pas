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

unit BrookEntryPoints;

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
  SBrookCannotCreateEntryPointsHandle = 'Cannot create entry-points handle.';
  SBrookInactiveEntryPoints = 'Inactive entry-points.';
  SBrookNoEntryPointsDefined = 'No entry-points defined.';
  SBrookEntryPointAlreadyExists =
    '%s: entry-point ''%s'' already exists in ''%s''.';
  SBrookEmptyEntryPointName = '%s: entry-point cannot be empty.';
  SBrookEmptyEntryPointsPath = 'Entry-points path cannot be empty.';

type
  TBrookEntryPointList = class;

  EBrookEntryPoint = class(Exception);

  EBrookEntryPointList = class(Exception);

  TBrookCustomEntryPoint = class(TBrookHandleCollectionItem)
  private
    FList: TBrookEntryPointList;
    FHandle: Psg_entrypoint;
    FName: string;
    FUserData: Pointer;
    function GetName: string;
    function GetTarget: TComponent;
    function GetUserData: Pointer;
    procedure SetName(const AValue: string);
    procedure SetTarget(AValue: TComponent);
  protected
    function GetHandle: Pointer; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
    procedure Validate; inline;
    property Name: string read GetName write SetName;
    property UserData: Pointer read GetUserData write FUserData;
    property Target: TComponent read GetTarget write SetTarget;
  end;

  TBrookCustomEntryPointClass = class of TBrookCustomEntryPoint;

  TBrookEntryPoint = class(TBrookCustomEntryPoint)
  published
    property Name;
    property Target;
  end;

  TBrookEntryPointListEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBrookCustomEntryPoint;
    property Current: TBrookCustomEntryPoint read GetCurrent;
  end;

  TBrookEntryPointList = class(TBrookHandleOwnedCollection)
  private
    FHandle: Psg_entrypoints;
    function GetItem(AIndex: Integer): TBrookCustomEntryPoint;
    procedure SetItem(AIndex: Integer; AValue: TBrookCustomEntryPoint);
  protected
    function GetHandle: Pointer; override;
    class function GetNameLabel: string; virtual;
    class function GetEntryPointName(
      AEntryPoint: TBrookCustomEntryPoint): string; virtual;
  public
    constructor Create(AOwner: TPersistent); virtual;
    class function GetEntryPointClass: TBrookCustomEntryPointClass; virtual;
    procedure Assign(ASource: TPersistent); override;
    function GetEnumerator: TBrookEntryPointListEnumerator;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function IsPrepared: Boolean; virtual;
    function NewName: string; virtual;
    function Add: TBrookCustomEntryPoint; virtual;
    function IndexOf(const AName: string): Integer; virtual;
    function Find(const AName: string): TBrookCustomEntryPoint; virtual;
    procedure Clear; virtual;
    property Items[AIndex: Integer]: TBrookCustomEntryPoint read GetItem
      write SetItem; default;
  end;

  TBrookEntryPointsFoundEvent = procedure(
    AEntryPoint: TBrookCustomEntryPoint) of object;

  TBrookCustomEntryPoints = class(TBrookHandledComponent)
  private
    FActive: Boolean;
    FList: TBrookEntryPointList;
    FStreamedActive: Boolean;
    function IsActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetList(AValue: TBrookEntryPointList);
  protected
    function CreateList: TBrookEntryPointList; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckActive; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Find(const APath: string;
      out AUserData: Pointer): Boolean; virtual;
    function FindTarget<T: TComponent>(const APath: string;
      out AUserData: T): Boolean;
    procedure Open;
    procedure Close;
    property Active: Boolean read FActive write SetActive stored IsActive;
    property List: TBrookEntryPointList read FList write SetList;
  end;

  TBrookEntryPoints = class(TBrookCustomEntryPoints)
  published
    property Active;
    property List;
  end;

implementation

{ TBrookCustomEntryPoint }

constructor TBrookCustomEntryPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  if Assigned(ACollection) and (ACollection is TBrookEntryPointList) then
  begin
    FList := ACollection as TBrookEntryPointList;
    FName := FList.NewName;
  end
  else
    SetName('/');
end;

function TBrookCustomEntryPoint.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookCustomEntryPoint.Assign(ASource: TPersistent);
begin
  if ASource is TBrookCustomEntryPoint then
    FName := (ASource as TBrookCustomEntryPoint).FName
  else
    inherited Assign(ASource);
end;

procedure TBrookCustomEntryPoint.Validate;
begin
  if FName.IsEmpty then
    raise EBrookEntryPoint.CreateResFmt(@SBrookEmptyEntryPointName,
      [GetNamePath]);
end;

function TBrookCustomEntryPoint.GetName: string;
var
  P: Pcchar;
begin
  if not Assigned(FHandle) then
    Exit(FName);
  SgLib.Check;
  P := sg_entrypoint_name(FHandle);
  try
    Result := TMarshal.ToString(P);
  finally
    sg_free(P);
  end;
end;

function TBrookCustomEntryPoint.GetTarget: TComponent;
begin
  Result := GetUserData;
end;

function TBrookCustomEntryPoint.GetUserData: Pointer;
begin
  if not Assigned(FHandle) then
    Exit(FUserData);
  SgLib.Check;
  Result := sg_entrypoint_user_data(FHandle);
end;

procedure TBrookCustomEntryPoint.SetName(const AValue: string);
var
  EP: TBrookCustomEntryPoint;
  PS: TArray<string>;
  NN: string;
begin
  if (AValue = FName) or (not Assigned(FList)) then
    Exit;
  PS := AValue.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  NN := '/';
  if Length(PS) > 0 then
    NN := Concat(NN, PS[0]);
  EP := FList.Find(NN);
  if Assigned(EP) and (EP <> Self) then
    raise EBrookEntryPoint.CreateResFmt(@SBrookEntryPointAlreadyExists,
      [GetNamePath, NN, EP.GetNamePath]);
  FName := NN;
end;

procedure TBrookCustomEntryPoint.SetTarget(AValue: TComponent);
begin
  FUserData := AValue;
end;

{ TBrookEntryPointListEnumerator }

function TBrookEntryPointListEnumerator.GetCurrent: TBrookCustomEntryPoint;
begin
  Result := TBrookCustomEntryPoint(inherited GetCurrent);
end;

{ TBrookEntryPointList }

constructor TBrookEntryPointList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetEntryPointClass);
end;

class function TBrookEntryPointList.GetEntryPointClass: TBrookCustomEntryPointClass;
begin
  Result := TBrookEntryPoint;
end;

procedure TBrookEntryPointList.Assign(ASource: TPersistent);
var
  EP: TBrookCustomEntryPoint;
begin
  if ASource is TBrookEntryPointList then
  begin
    Clear;
    for EP in (ASource as TBrookEntryPointList) do
      Add.Assign(EP);
  end
  else
    inherited Assign(ASource);
end;

function TBrookEntryPointList.GetEnumerator: TBrookEntryPointListEnumerator;
begin
  Result := TBrookEntryPointListEnumerator.Create(Self);
end;

procedure TBrookEntryPointList.Prepare;
var
  EP: TBrookCustomEntryPoint;
  M: TMarshaller;
  R: cint;
begin
  if Assigned(FHandle) then
    Exit;
  if Count = 0 then
    raise EBrookEntryPointList.CreateRes(@SBrookNoEntryPointsDefined);
  SgLib.Check;
  FHandle := sg_entrypoints_new;
  if not Assigned(FHandle) then
    raise EInvalidPointer.CreateRes(@SBrookCannotCreateEntryPointsHandle);
  SgLib.CheckLastError(sg_entrypoints_clear(FHandle));
  for EP in Self do
  begin
    EP.Validate;
    R := sg_entrypoints_add(FHandle, M.ToCString(GetEntryPointName(EP)),
      EP.FUserData);
    if R = 0 then
      Continue;
    if R = EALREADY then
      raise EBrookEntryPointList.CreateResFmt(@SBrookEntryPointAlreadyExists,
        [EP.GetNamePath, EP.Name]);
    SgLib.CheckLastError(R);
  end;
end;

procedure TBrookEntryPointList.Unprepare;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  sg_entrypoints_free(FHandle);
  FHandle := nil;
end;

function TBrookEntryPointList.IsPrepared: Boolean;
begin
  Result := Assigned(FHandle);
end;

function TBrookEntryPointList.GetItem(AIndex: Integer): TBrookCustomEntryPoint;
begin
  Result := TBrookCustomEntryPoint(inherited GetItem(AIndex));
end;

procedure TBrookEntryPointList.SetItem(AIndex: Integer;
  AValue: TBrookCustomEntryPoint);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBrookEntryPointList.GetHandle: Pointer;
begin
  Result := FHandle;
end;

class function TBrookEntryPointList.GetNameLabel: string;
begin
  Result := '/api';
end;

class function TBrookEntryPointList.GetEntryPointName(
  AEntryPoint: TBrookCustomEntryPoint): string;
begin
  Result := AEntryPoint.FName;
end;

function TBrookEntryPointList.NewName: string;
var
  VIndex: Integer;
begin
  VIndex := 1;
  repeat
    Result := Concat(GetNameLabel, VIndex.ToString);
    Inc(VIndex);
  until IndexOf(Result) < 0;
end;

function TBrookEntryPointList.Add: TBrookCustomEntryPoint;
begin
  Result := TBrookEntryPoint(inherited Add);
end;

function TBrookEntryPointList.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Name, AName) then
      Exit;
  Result := -1;
end;

function TBrookEntryPointList.Find(const AName: string): TBrookCustomEntryPoint;
var
  EP: TBrookCustomEntryPoint;
begin
  for EP in Self do
    if SameText(EP.Name, AName) then
      Exit(EP);
  Result := nil;
end;

procedure TBrookEntryPointList.Clear;
begin
  inherited Clear;
  SgLib.Check;
  SgLib.CheckLastError(sg_entrypoints_clear(FHandle));
end;

{ TBrookCustomEntryPoints }

constructor TBrookCustomEntryPoints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := CreateList;
end;

destructor TBrookCustomEntryPoints.Destroy;
begin
  try
    SetActive(False);
  finally
    inherited Destroy;
    FList.Free;
  end;
end;

function TBrookCustomEntryPoints.CreateList: TBrookEntryPointList;
begin
  Result := TBrookEntryPointList.Create(Self);
end;

procedure TBrookCustomEntryPoints.Loaded;
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

function TBrookCustomEntryPoints.GetHandle: Pointer;
begin
  Result := FList.FHandle;
end;

procedure TBrookCustomEntryPoints.DoOpen;
begin
  FList.Prepare;
  FActive := FList.IsPrepared;
end;

procedure TBrookCustomEntryPoints.DoClose;
begin
  FList.Unprepare;
  FActive := False;
end;

procedure TBrookCustomEntryPoints.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EInvalidOpException.CreateRes(@SBrookInactiveEntryPoints);
end;

procedure TBrookCustomEntryPoints.SetList(AValue: TBrookEntryPointList);
begin
  if AValue = FList then
    Exit;
  if Assigned(AValue) then
    FList.Assign(AValue)
  else
    FList.Clear;
end;

function TBrookCustomEntryPoints.IsActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookCustomEntryPoints.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
      SgLib.Check;
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

procedure TBrookCustomEntryPoints.Open;
begin
  SetActive(True);
end;

procedure TBrookCustomEntryPoints.Close;
begin
  SetActive(False);
end;

function TBrookCustomEntryPoints.Find(const APath: string;
  out AUserData: Pointer): Boolean;
var
  M: TMarshaller;
  R: cint;
  EP: Psg_entrypoint;
begin
  if APath.IsEmpty then
    raise EArgumentException.CreateRes(@SBrookEmptyEntryPointsPath);
  CheckActive;
  SgLib.Check;
  R := sg_entrypoints_find(FList.FHandle, @EP, M.ToCString(APath));
  Result := R = 0;
  if Result then
    AUserData := sg_entrypoint_user_data(EP)
  else
    if (R <> ENOENT) then
      SgLib.CheckLastError(R);
end;

function TBrookCustomEntryPoints.FindTarget<T>(const APath: string;
  out AUserData: T): Boolean;
begin
  Result := Find(APath, Pointer(AUserData));
end;

end.
