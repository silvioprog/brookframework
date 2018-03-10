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

  { TBrookLibraryLibraryNamePropertyEditor }

  TBrookLibraryLibraryNamePropertyEditor = class(
{$IFDEF LCL}TFileNamePropertyEditor{$ELSE}TStringProperty{$ENDIF})
  public
{$IFDEF LCL}
    function GetVerbCount: Integer; override;
    function GetVerb(AIndex: Integer): string; override;
    procedure ExecuteVerb(AIndex: Integer); override;
{$ENDIF}
    function GetFilter: string; {$IFDEF LCL}override{$ELSE}virtual{$ENDIF};
    function GetDialogTitle: string; {$IFDEF LCL}override{$ELSE}virtual{$ENDIF};
{$IFNDEF LCL}
    function CreateFileDialog: TOpenDialog; virtual;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetDialogOptions: TOpenOptions; virtual;
    function GetInitialDirectory: string; virtual;
    procedure SetFileName(const AFileName: string); virtual;
{$ENDIF}
  end;

  { TBrookLibraryLibraryNameComponentEditor }

  TBrookLibraryLibraryNameComponentEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    function GetVerb(AIndex: Integer): string; override;
{$IFNDEF LCL}
    function GetVerbCount: Integer; override;
{$ENDIF}
    procedure ExecuteVerb(AIndex: Integer); override;
  end;

{$R BrookFramework40Icons.res}

procedure Register;

implementation

uses
  BrookLibraryLoader;

procedure Register;
begin
  RegisterComponents('Brook', [
    TBrookLibraryLoader
  ]);
  RegisterPropertyEditor(TypeInfo(TFileName), TBrookLibraryLoader,
    'LibraryName', TBrookLibraryLibraryNamePropertyEditor);
  RegisterComponentEditor(TBrookLibraryLoader,
    TBrookLibraryLibraryNameComponentEditor);
end;

{$IFDEF LCL}

{ TBrookLibraryLibraryNamePropertyEditor }

function TBrookLibraryLibraryNamePropertyEditor.GetVerbCount: Integer;
begin
  Result := Succ(inherited GetVerbCount);
end;

function TBrookLibraryLibraryNamePropertyEditor.GetVerb(
  AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := LoadResString(@SBrookSelectLibraryTitle)
  else
    Result := inherited GetVerb(AIndex);
end;

procedure TBrookLibraryLibraryNamePropertyEditor.ExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited ExecuteVerb(AIndex);
end;

{$ENDIF}

function TBrookLibraryLibraryNamePropertyEditor.GetFilter: string;
var
  VSharedSuffix: string;
begin
  VSharedSuffix := Concat('*.', SharedSuffix);
  Result := Format(LoadResString(@SBrookSharedLibraryFilter), [VSharedSuffix,
{$IFDEF LINUX}Concat({$ENDIF}VSharedSuffix
{$IFDEF LINUX}, ';', VSharedSuffix, '.*'){$ENDIF}]);
end;

function TBrookLibraryLibraryNamePropertyEditor.GetDialogTitle: string;
begin
  Result := LoadResString(@SBrookSelectLibraryTitle);
end;

{$IFNDEF LCL}

function TBrookLibraryLibraryNamePropertyEditor.CreateFileDialog: TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
end;

procedure TBrookLibraryLibraryNamePropertyEditor.Edit;
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

function TBrookLibraryLibraryNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TBrookLibraryLibraryNamePropertyEditor.GetDialogOptions: TOpenOptions;
begin
  Result := [ofEnableSizing];
end;

function TBrookLibraryLibraryNamePropertyEditor.GetInitialDirectory: string;
begin
  Result := '';
end;

procedure TBrookLibraryLibraryNamePropertyEditor.SetFileName(
  const AFileName: string);
begin
  SetStrValue(AFileName);
end;

{$ENDIF}

{ TBrookLibraryLibraryNameComponentEditor }

procedure TBrookLibraryLibraryNameComponentEditor.Edit;
var
  VDialog: TOpenDialog;
  VLibraryLoader: TBrookLibraryLoader;
  VPropertyEditor: TBrookLibraryLibraryNamePropertyEditor;
begin
  VLibraryLoader := Component as TBrookLibraryLoader;
  if not Assigned(VLibraryLoader) then Exit;
  VPropertyEditor := TBrookLibraryLibraryNamePropertyEditor.Create(nil, 0);
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

function TBrookLibraryLibraryNameComponentEditor.GetVerb(
  AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := LoadResString(@SBrookSelectLibraryTitle)
  else
    Result := inherited GetVerb(AIndex);
end;

{$IFNDEF LCL}

function TBrookLibraryLibraryNameComponentEditor.GetVerbCount: Integer;
begin
  Result := Succ(inherited GetVerbCount);
end;

{$ENDIF}

procedure TBrookLibraryLibraryNameComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    Edit
  else
    inherited ExecuteVerb(AIndex);
end;

end.
