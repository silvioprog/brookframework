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
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui,
  BrookUtils,
  BrookHandledClasses;

resourcestring
  SBrookEntryPointListUnprepared = 'Entry-point list not prepared.';
  SBrookInactiveEntryPoints = 'Inactive entry-points.';
  SBrookNoEntryPointsDefined = 'No entry-points defined.';
  SBrookEntryPointAlreadyExists =
    '%s: entry-point ''%s'' already exists in ''%s''.';
  SBrookEmptyEntryPointName = '%s: entry-point cannot be empty.';

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
    function GetUserData: Pointer;
    procedure SetName(const AValue: string);
  protected
    function GetHandle: Pointer; override;
    function GetTarget: TComponent; virtual;
    procedure SetTarget(AValue: TComponent); virtual;
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
    procedure InternalAdd(AEntryPoint: TBrookCustomEntryPoint);
  protected
    class function GetEntryPointLabel: string; virtual;
    class function GetEntryPointName(
      AEntryPoint: TBrookCustomEntryPoint): string; virtual;
    function GetHandle: Pointer; override;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    procedure CheckPrepared; inline;
  public
    constructor Create(AOwner: TPersistent); virtual;
    class function GetEntryPointClass: TBrookCustomEntryPointClass; virtual;
    procedure Assign(ASource: TPersistent); override;
    function GetEnumerator: TBrookEntryPointListEnumerator;
    function IsPrepared: Boolean; virtual;
    function NewName: string; virtual;
    function Add: TBrookCustomEntryPoint; virtual;
    function Remove(const AName: string): Boolean; virtual;
    function IndexOf(const AName: string): Integer; virtual;
    function FindInList(const AName: string): TBrookCustomEntryPoint; virtual;
    function Find(const APath: string; out AUserData): Boolean; virtual;
    procedure Clear; virtual;
    property Items[AIndex: Integer]: TBrookCustomEntryPoint read GetItem
      write SetItem; default;
  end;

  TBrookCustomEntryPoints = class(TBrookHandledComponent)
  private
    FActive: Boolean;
    FList: TBrookEntryPointList;
    FStreamedActive: Boolean;
    function GetItem(AIndex: Integer): TBrookCustomEntryPoint;
    function IsActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetItem(AIndex: Integer; AValue: TBrookCustomEntryPoint);
    procedure SetList(AValue: TBrookEntryPointList);
  protected
    function CreateList: TBrookEntryPointList; virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckItems; inline;
    procedure CheckActive; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function GetEnumerator: TBrookEntryPointListEnumerator;
    function Add: TBrookCustomEntryPoint; inline;
    procedure Remove(const AName: string); inline;
    procedure Clear; inline;
    function Find(const APath: string; out ATarget): Boolean; virtual;
    procedure Open;
    procedure Close;
    property Active: Boolean read FActive write SetActive stored IsActive;
    property List: TBrookEntryPointList read FList write SetList;
    property Items[AIndex: Integer]: TBrookCustomEntryPoint read GetItem
      write SetItem; default;
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
  NN: string;
begin
  if (AValue = FName) or (not Assigned(FList)) then
    Exit;
  NN := BrookFixEntryPoint(AValue);
  EP := FList.FindInList(NN);
  if Assigned(EP) and (EP <> Self) then
    raise EBrookEntryPoint.CreateResFmt(@SBrookEntryPointAlreadyExists,
      [GetNamePath, NN, EP.GetNamePath]);
  FName := NN;
  if Assigned(FList.FHandle) then
  begin
    SgLib.Check;
    FList.InternalAdd(Self);
  end;
end;

procedure TBrookCustomEntryPoint.SetTarget(AValue: TComponent);
var
  M: TMarshaller;
  EP: Psg_entrypoint;
begin
  FUserData := AValue;
  if not Assigned(FList.FHandle) then
    Exit;
  SgLib.Check;
  if sg_entrypoints_find2(FList.FHandle, @EP, M.ToCString(FName)) = 0 then
    SgLib.CheckLastError(sg_entrypoint_set_user_data(EP, FUserData));
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

class function TBrookEntryPointList.GetEntryPointLabel: string;
begin
  Result := '/api';
end;

class function TBrookEntryPointList.GetEntryPointName(
  AEntryPoint: TBrookCustomEntryPoint): string;
begin
  Result := AEntryPoint.FName;
end;

function TBrookEntryPointList.GetHandle: Pointer;
begin
  Result := FHandle;
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
begin
  if Assigned(FHandle) or (Count = 0) then
    Exit;
  SgLib.Check;
  FHandle := sg_entrypoints_new;
  SgLib.CheckLastError(sg_entrypoints_clear(FHandle));
  for EP in Self do
  begin
    EP.Validate;
    InternalAdd(EP);
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

procedure TBrookEntryPointList.CheckPrepared;
begin
  if not Assigned(FHandle) then
    raise EInvalidPointer.CreateRes(@SBrookEntryPointListUnprepared);
end;

procedure TBrookEntryPointList.InternalAdd(AEntryPoint: TBrookCustomEntryPoint);
var
  M: TMarshaller;
  R: cint;
begin
  R := sg_entrypoints_add(FHandle, M.ToCString(GetEntryPointName(AEntryPoint)),
    AEntryPoint.FUserData);
  if R = 0 then
    Exit;
  if R = EALREADY then
    raise EBrookEntryPointList.CreateResFmt(@SBrookEntryPointAlreadyExists,
      [AEntryPoint.GetNamePath, AEntryPoint.Name]);
  SgLib.CheckLastError(R);
end;

function TBrookEntryPointList.NewName: string;
var
  I: Integer;
begin
  I := 1;
  repeat
    Result := Concat(GetEntryPointLabel, I.ToString);
    Inc(I);
  until IndexOf(Result) < 0;
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

function TBrookEntryPointList.Add: TBrookCustomEntryPoint;
begin
  Result := TBrookEntryPoint(inherited Add);
end;

function TBrookEntryPointList.Remove(const AName: string): Boolean;
var
  M: TMarshaller;
  I: Integer;
begin
  I := IndexOf(AName);
  Result := I > -1;
  if Result then
  begin
    if Assigned(FHandle) then
      SgLib.CheckLastError(sg_entrypoints_rm2(FHandle, M.ToCString(AName)));
    inherited Delete(I);
  end;
end;

function TBrookEntryPointList.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Pred(Count) do
    if SameText(GetItem(Result).Name, AName) then
      Exit;
  Result := -1;
end;

function TBrookEntryPointList.FindInList(
  const AName: string): TBrookCustomEntryPoint;
var
  EP: TBrookCustomEntryPoint;
begin
  for EP in Self do
    if SameText(EP.Name, AName) then
      Exit(EP);
  Result := nil;
end;

function TBrookEntryPointList.Find(const APath: string; out AUserData): Boolean;
var
  M: TMarshaller;
  R: cint;
  EP: Psg_entrypoint;
begin
  CheckPrepared;
  SgLib.Check;
  R := sg_entrypoints_find(FHandle, @EP, M.ToCString(APath));
  Result := R = 0;
  if Result then
    Pointer(AUserData) := sg_entrypoint_user_data(EP)
  else
    if (R <> ENOENT) then
      SgLib.CheckLastError(R);
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

procedure TBrookCustomEntryPoints.CheckItems;
begin
  if FList.Count = 0 then
    raise EBrookEntryPointList.CreateRes(@SBrookNoEntryPointsDefined);
end;

procedure TBrookCustomEntryPoints.CheckActive;
begin
  if (not (csLoading in ComponentState)) and (not Active) then
    raise EInvalidOpException.CreateRes(@SBrookInactiveEntryPoints);
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

procedure TBrookCustomEntryPoints.Assign(ASource: TPersistent);
begin
  if ASource is TBrookCustomEntryPoints then
    FList.Assign((ASource as TBrookCustomEntryPoints).FList)
  else
    inherited Assign(ASource);
end;

function TBrookCustomEntryPoints.GetEnumerator: TBrookEntryPointListEnumerator;
begin
  Result := TBrookEntryPointListEnumerator.Create(FList);
end;

function TBrookCustomEntryPoints.Add: TBrookCustomEntryPoint;
begin
  Result := FList.Add;
end;

procedure TBrookCustomEntryPoints.Remove(const AName: string);
begin
  FList.Remove(AName);
end;

procedure TBrookCustomEntryPoints.Clear;
begin
  FList.Clear;
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

function TBrookCustomEntryPoints.GetItem(AIndex: Integer): TBrookCustomEntryPoint;
begin
  Result := FList.GetItem(AIndex);
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

procedure TBrookCustomEntryPoints.SetItem(AIndex: Integer;
  AValue: TBrookCustomEntryPoint);
begin
  FList.SetItem(AIndex, AValue);
end;

procedure TBrookCustomEntryPoints.Open;
begin
  SetActive(True);
end;

procedure TBrookCustomEntryPoints.Close;
begin
  SetActive(False);
end;

function TBrookCustomEntryPoints.Find(const APath: string; out ATarget): Boolean;
begin
  CheckItems;
  CheckActive;
  Result := FList.Find(APath, ATarget);
end;

end.
