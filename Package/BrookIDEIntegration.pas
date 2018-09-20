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

{ Integrates Brook to Delphi or Lazarus IDE. }

unit BrookIDEIntegration;

{$I Brook.inc}

{$IFDEF FPC}
 {$WARN 5024 OFF}
{$ENDIF}

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

{$R BrookFramework40Icons.res}

procedure Register;

implementation

uses
  BrookLibraryLoader,
  BrookHTTPEntryPoints,
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
  if Assigned(AObj) and (AObj is TBrookHTTPRoute) and
    SameText(APropInfo.NameFld.ToString, 'Methods') then
    Exit(TBrookHTTPRouteRequestMethodsPropertyEditor);
  Result := nil;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Brook', [
    TBrookLibraryLoader,
    TBrookHTTPEntryPoints,
    TBrookHTTPRouter,
    TBrookHTTPServer
  ]);
  RegisterPropertyEditor(TypeInfo(TFileName), TBrookLibraryLoader,
    'LibraryName', TBrookLibraryNamePropertyEditor);
{$IFDEF LCL}
 {$IFNDEF VER3_0_0}
  RegisterPropertyEditor(TypeInfo(string), TBrookHTTPServerSecurity,
    'PrivatePassword', TPasswordStringPropertyEditor);
 {$ENDIF}
  RegisterPropertyEditor(TypeInfo(TBrookHTTPRouteRequestMethods), nil, '',
    TBrookHTTPRouteRequestMethodsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TBrookHTTPServer, 'UploadsDir',
    TDirectoryPropertyEditor);
{$ELSE}
  RegisterPropertyMapper(BrookHTTPRouteRequestMethodsPropertyMapper);
{$ENDIF}
  RegisterComponentEditor(TBrookLibraryLoader, TBrookLibraryNameComponentEditor);
  RegisterComponentEditor(TBrookHTTPServer, TBrookOnRequestComponentEditor);
end;

{$IFDEF LCL}

{ TBrookLibraryNamePropertyEditor }

function TBrookLibraryNamePropertyEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TBrookLibraryNamePropertyEditor.GetVerb(
  AIndex: Integer): string;
begin
  Result := LoadResString(@SBrookSelectLibraryTitle);
end;

procedure TBrookLibraryNamePropertyEditor.ExecuteVerb(AIndex: Integer);
begin
  Edit;
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
  VLibraryLoader: TBrookLibraryLoader;
  VPropertyEditor: TBrookLibraryNamePropertyEditor;
begin
  VLibraryLoader := Component as TBrookLibraryLoader;
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
      Designer.Modified;
    end;
  finally
    VPropertyEditor.Free;
  end;
end;

function TBrookLibraryNameComponentEditor.GetVerb(
  AIndex: Integer): string;
begin
  Result := LoadResString(@SBrookSelectLibraryTitle);
end;

{$IFNDEF LCL}

function TBrookLibraryNameComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$ENDIF}

procedure TBrookLibraryNameComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  Edit;
end;

{ TBrookOnRequestComponentEditor }

procedure TBrookOnRequestComponentEditor.EditProperty(const AProperty:
{$IFDEF LCL}TPropertyEditor{$ELSE}IProperty{$ENDIF}; var AContinue: Boolean);
begin
  if SameText(AProperty.GetName, 'OnRequest') then
    inherited EditProperty(AProperty, AContinue);
end;

end.
