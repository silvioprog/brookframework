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

{ Integrates Brook in Delphi or Lazarus IDE. }

unit BrookIDEIntegration;

{$I Brook.inc}

interface

uses
  SysUtils,
  Classes,
  TypInfo,
  Dialogs,
{$IFDEF LCL}
  PropEdits,
  ComponentEditors,
{$ELSE}
  DesignIntf,
  DesignEditors,
  ColnEdit,
{$ENDIF}
  libsagui;

resourcestring
  SBrookSelectLibraryTitle = 'Select library ...';
  SBrookSharedLibraryFilter = 'Shared libraries (%s)|%s|All files (*.*)|*.*';
  SBrookRoutesEditor = 'Routes editor ...';
  SBrookEntryPointsEditor = 'Entry-points editor ...';

type

  { TBrookLibraryNamePropertyEditor }

  TBrookLibraryNamePropertyEditor = class(
{$IFDEF LCL}TFileNamePropertyEditor{$ELSE}TStringProperty{$ENDIF})
  public
{$IFDEF LCL}
    function GetVerbCount: Integer; override;
    function GetVerb(AIndex: Integer): string; override;
    procedure ExecuteVerb(AIndex: Integer); override;
{$ENDIF}
    function GetFilter: string;{$IFDEF LCL}override{$ELSE}virtual{$ENDIF};
    function GetDialogTitle: string;{$IFDEF LCL}override{$ELSE}virtual{$ENDIF};
{$IFNDEF LCL}
    function CreateFileDialog: TOpenDialog; virtual;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetDialogOptions: TOpenOptions; virtual;
    function GetInitialDirectory: string; virtual;
    procedure SetFileName(const AFileName: string); virtual;
{$ENDIF}
  end;

  { TBrookHTTPRouteRequestMethodsPropertyEditor }

  TBrookHTTPRouteRequestMethodsPropertyEditor = class(TSetProperty)
  public
    procedure GetProperties(AProc:
{$IFDEF LCL}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF}); override;
  end;

  { TBrookLibraryNameComponentEditor }

  TBrookLibraryNameComponentEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    function GetVerb(AIndex: Integer): string; override;
{$IFNDEF LCL}
    function GetVerbCount: Integer; override;
{$ENDIF}
    procedure ExecuteVerb(AIndex: Integer); override;
  end;

  { TBrookOnRequestComponentEditor }

  TBrookOnRequestComponentEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const AProperty:
{$IFDEF LCL}TPropertyEditor{$ELSE}IProperty{$ENDIF};
      var AContinue: Boolean); override;
  end;

  { TBrookPathRouterComponentEditor }

  TBrookPathRouterComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TBrookEntryPointsComponentEditor }

  TBrookEntryPointsComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$R BrookFramework40Icons.res}

procedure Register;

implementation

uses
  BrookLibraryLoader,
  BrookPathRouter,
  BrookEntryPoints,
  BrookHTTPRouter,
  BrookHTTPServer;

{$IFNDEF LCL}

type
  TLocalSetElementProperty = class(TSetElementProperty)
  public
    constructor Create(AParent: TPropertyEditor; AElement: Integer); reintroduce;
  end;

constructor TLocalSetElementProperty.Create(AParent: TPropertyEditor;
  AElement: Integer);
begin
  inherited Create(AParent, AElement);
end;

function BrookHTTPRouteRequestMethodsPropertyMapper(AObj: TPersistent;
  APropInfo: PPropInfo): TPropertyEditorClass;
begin
  if Assigned(AObj) and (AObj is TBrookCustomHTTPRoute) and
    SameText(APropInfo.NameFld.ToString, 'Methods') then
    Exit(TBrookHTTPRouteRequestMethodsPropertyEditor);
  Result := nil;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Brook', [
    TBrookLibraryLoader,
    TBrookPathRouter,
    TBrookEntryPoints,
    TBrookHTTPRouter,
    TBrookHTTPServer
  ]);
  RegisterPropertyEditor(TypeInfo(TFileName), TBrookLibraryLoader,
    'LibraryName', TBrookLibraryNamePropertyEditor);
{$IFDEF LCL}
  RegisterPropertyEditor(TypeInfo(string), TBrookHTTPServerSecurity,
    'PrivatePassword', TPasswordStringPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBrookHTTPRouteRequestMethods), nil, '',
    TBrookHTTPRouteRequestMethodsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TBrookHTTPServer, 'UploadsDir',
    TDirectoryPropertyEditor);
{$ELSE}
  RegisterPropertyMapper(BrookHTTPRouteRequestMethodsPropertyMapper);
{$ENDIF}
  RegisterComponentEditor(TBrookLibraryLoader, TBrookLibraryNameComponentEditor);
  RegisterComponentEditor(TBrookPathRouter, TBrookPathRouterComponentEditor);
  RegisterComponentEditor(TBrookEntryPoints, TBrookEntryPointsComponentEditor);
  RegisterComponentEditor(TBrookHTTPRouter, TBrookPathRouterComponentEditor);
  RegisterComponentEditor(TBrookHTTPServer, TBrookOnRequestComponentEditor);
end;

{$IFDEF LCL}

{ TBrookLibraryNamePropertyEditor }

function TBrookLibraryNamePropertyEditor.GetVerbCount: Integer;
begin
  Result := Succ(inherited GetVerbCount);
end;

function TBrookLibraryNamePropertyEditor.GetVerb(
  AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := LoadResString(@SBrookSelectLibraryTitle)
  else
    Result := inherited GetVerb(AIndex);
end;

procedure TBrookLibraryNamePropertyEditor.ExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited ExecuteVerb(AIndex);
end;

{$ENDIF}

function TBrookLibraryNamePropertyEditor.GetFilter: string;
var
  VSharedSuffix: string;
begin
  VSharedSuffix := Concat('*.', SharedSuffix);
  Result := Format(LoadResString(@SBrookSharedLibraryFilter), [VSharedSuffix,
{$IFDEF LINUX}Concat({$ENDIF}VSharedSuffix
{$IFDEF LINUX}, ';', VSharedSuffix, '.*'){$ENDIF}]);
end;

function TBrookLibraryNamePropertyEditor.GetDialogTitle: string;
begin
  Result := LoadResString(@SBrookSelectLibraryTitle);
end;

{$IFNDEF LCL}

function TBrookLibraryNamePropertyEditor.CreateFileDialog: TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
end;

procedure TBrookLibraryNamePropertyEditor.Edit;
begin
  with CreateFileDialog do
  try
    Filter := GetFilter;
    Options := GetDialogOptions;
    FileName := GetStrValue;
    InitialDir := GetInitialDirectory;
    Title := GetDialogTitle;
    if Execute then
      SetFileName(FileName);
  finally
    Free;
  end;
end;

function TBrookLibraryNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TBrookLibraryNamePropertyEditor.GetDialogOptions: TOpenOptions;
begin
  Result := [ofEnableSizing];
end;

function TBrookLibraryNamePropertyEditor.GetInitialDirectory: string;
begin
  Result := '';
end;

procedure TBrookLibraryNamePropertyEditor.SetFileName(
  const AFileName: string);
begin
  SetStrValue(AFileName);
end;

{$ENDIF}

{ TBrookHTTPRouteRequestMethodsPropertyEditor }

procedure TBrookHTTPRouteRequestMethodsPropertyEditor.GetProperties(
  AProc:{$IFDEF LCL}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF});
var
  M: TBrookHTTPRouteRequestMethod;
{$IFNDEF LCL}
  P: IProperty;
{$ENDIF}
begin
  for M := Succ(Low(TBrookHTTPRouteRequestMethod)) to
    High(TBrookHTTPRouteRequestMethod) do
{$IFDEF LCL}
    AProc(TSetElementProperty.Create(Self, Ord(M)));
{$ELSE}
  begin
    P := TLocalSetElementProperty.Create(Self, Ord(M));
    AProc(P);
    P := nil;
  end;
{$ENDIF}
end;

{ TBrookLibraryNameComponentEditor }

procedure TBrookLibraryNameComponentEditor.Edit;
var
  VDialog: TOpenDialog;
  VLibraryLoader: TBrookCustomLibraryLoader;
  VPropertyEditor: TBrookLibraryNamePropertyEditor;
begin
  VLibraryLoader := Component as TBrookCustomLibraryLoader;
  if not Assigned(VLibraryLoader) then
    Exit;
  VPropertyEditor := TBrookLibraryNamePropertyEditor.Create(nil, 0);
  VDialog := VPropertyEditor.CreateFileDialog;
  try
    VDialog.Filter := VPropertyEditor.GetFilter;
    VDialog.Options := VPropertyEditor.GetDialogOptions;
    VDialog.InitialDir := VPropertyEditor.GetInitialDirectory;
    VDialog.Title := VPropertyEditor.GetDialogTitle;
    VDialog.FileName := VLibraryLoader.LibraryName;
    if VDialog.Execute then
    begin
      VLibraryLoader.LibraryName := VDialog.FileName;
      VLibraryLoader.Open;
      Designer.Modified;
    end;
  finally
    VPropertyEditor.Free;
  end;
end;

function TBrookLibraryNameComponentEditor.GetVerb(
  AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := LoadResString(@SBrookSelectLibraryTitle)
  else
    Result := inherited GetVerb(AIndex);
end;

{$IFNDEF LCL}

function TBrookLibraryNameComponentEditor.GetVerbCount: Integer;
begin
  Result := Succ(inherited GetVerbCount);
end;

{$ENDIF}

procedure TBrookLibraryNameComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited ExecuteVerb(AIndex);
end;

{ TBrookOnRequestComponentEditor }

procedure TBrookOnRequestComponentEditor.EditProperty(const AProperty:
{$IFDEF LCL}TPropertyEditor{$ELSE}IProperty{$ENDIF}; var AContinue: Boolean);
begin
  if SameText(AProperty.GetName, 'OnRequest') then
    inherited EditProperty(AProperty, AContinue);
end;

{ TBrookPathRouterComponentEditor }

procedure TBrookPathRouterComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VRouter: TBrookCustomPathRouter;
begin
  if AIndex <> 0 then
    Exit;
  VRouter := GetComponent as TBrookCustomPathRouter;
{$IFDEF LCL}
  EditCollection(
{$ELSE}
  ShowCollectionEditor(Designer,
{$ENDIF}
    VRouter, VRouter.Routes, 'Routes');
end;

function TBrookPathRouterComponentEditor.GetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Exit(LoadResString(@SBrookRoutesEditor));
  Result := '';
end;

function TBrookPathRouterComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBrookEntryPointsComponentEditor }

procedure TBrookEntryPointsComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VEntryPoints: TBrookCustomEntryPoints;
begin
  if AIndex <> 0 then
    Exit;
  VEntryPoints := GetComponent as TBrookCustomEntryPoints;
{$IFDEF LCL}
  EditCollection(
{$ELSE}
  ShowCollectionEditor(Designer,
{$ENDIF}
    VEntryPoints, VEntryPoints.List, 'List');
end;

function TBrookEntryPointsComponentEditor.GetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Exit(LoadResString(@SBrookEntryPointsEditor));
  Result := '';
end;

function TBrookEntryPointsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
