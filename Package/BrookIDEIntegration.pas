(*    _____   _____    _____   _____   _   __
 *   |  _  \ |  _  \  /  _  \ /  _  \ | | / /
 *   | |_) | | |_) |  | | | | | | | | | |/ /
 *   |  _ <  |  _ <   | | | | | | | | |   (
 *   | |_) | | | \ \  | |_| | | |_| | | |\ \
 *   |_____/ |_|  \_\ \_____/ \_____/ |_| \_\
 *
 *   –– a small library which helps you write quickly REST APIs.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook library.  If not, see <http://www.gnu.org/licenses/>.
 *)

{ Integrates Brook in Delphi or Lazarus IDE. }

unit BrookIDEIntegration;

{$I Brook.inc}

interface

uses
  SysUtils,
  Classes,
  Dialogs,
{$IFDEF LCL}
  PropEdits,
  ComponentEditors
{$ELSE}
  DesignIntf,
  DesignEditors
{$ENDIF},
  libbrook;

resourcestring
  SBrookSelectLibraryTitle = 'Select library';
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
  BrookHTTPServer;

procedure Register;
begin
  RegisterComponents('Brook', [
    TBrookLibraryLoader,
    TBrookHTTPServer
  ]);
  RegisterPropertyEditor(TypeInfo(TFileName), TBrookLibraryLoader,
    'LibraryName', TBrookLibraryNamePropertyEditor);
  RegisterComponentEditor(TBrookLibraryLoader,
    TBrookLibraryNameComponentEditor);
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

{ TBrookLibraryNameComponentEditor }

procedure TBrookLibraryNameComponentEditor.Edit;
var
  VDialog: TOpenDialog;
  VLibraryLoader: TBrookLibraryLoader;
  VPropertyEditor: TBrookLibraryNamePropertyEditor;
begin
  VLibraryLoader := Component as TBrookLibraryLoader;
  if not Assigned(VLibraryLoader) then Exit;
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

end.
