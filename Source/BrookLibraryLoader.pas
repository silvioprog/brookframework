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

{ Dynamic library loader. }

unit BrookLibraryLoader;

{$I Brook.inc}

interface

uses
  SysUtils,
  Classes,
  libsagui,
  BrookHandledClasses,
  BrookUtils;

resourcestring
  { Indicates not allowed operation when the library is loaded. }
  SBrookOpNotAllowedLoadedLib =
    'Operation is not allowed while the library is loaded.';

type
  { Raised when an operation is not allowed while the library is loaded. }
  EBrookOpNotAllowedLoadedLib = class(Exception);

  { Class for dynamic library loading. }
  TBrookLibraryLoader = class(TBrookHandledComponent)
  private
    FEnabled: Boolean;
    FVersion: string;
    FHandle: TLibHandle;
    FLibraryName: TFileName;
    FStreamedLoad: Boolean;
    function IsEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure SetLibraryName(const AValue: TFileName);
  protected
    procedure Loaded; override;
    procedure CheckDisabled; inline;
    function GetHandle: Pointer; override;
  public
    { Loads the library dynamically. }
    procedure Load; virtual;
    { Unloads the library dynamically. }
    procedure Unload; virtual;
    { @exclude }
    procedure DefineProperties(AFiler: TFiler); override;
  published
    { Loads/Unloads the library dynamically. }
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabled;
    { Specifies the library to be loaded dynamically. }
    property LibraryName: TFileName read FLibraryName write SetLibraryName;
    { Version of the loaded library. }
    property Version: string read FVersion stored False;
  end;

implementation

procedure TBrookLibraryLoader.Loaded;
begin
  inherited Loaded;
  if FEnabled then
    Load;
end;

procedure TBrookLibraryLoader.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  if FEnabled and not FStreamedLoad then
  begin
    FStreamedLoad := True;
    Load;
  end;
end;

function TBrookLibraryLoader.GetHandle: Pointer;
begin
  Result := @FHandle;
end;

procedure TBrookLibraryLoader.CheckDisabled;
begin
  if not (csLoading in ComponentState) and Enabled then
    raise EBrookOpNotAllowedLoadedLib.CreateRes(@SBrookOpNotAllowedLoadedLib);
end;

procedure TBrookLibraryLoader.SetEnabled(AValue: Boolean);
begin
  if AValue = FEnabled then
    Exit;
  if csLoading in ComponentState then
    FEnabled := AValue
  else
    if AValue then
      Load
    else
      Unload;
end;

function TBrookLibraryLoader.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TBrookLibraryLoader.SetLibraryName(const AValue: TFileName);
begin
  if AValue = FLibraryName then
    Exit;
  CheckDisabled;
  FLibraryName := AValue;
end;

procedure TBrookLibraryLoader.Load;
begin
  FHandle := SgLoadLibrary(FLibraryName);
  FEnabled := FHandle <> NilHandle;
  if FEnabled then
    FVersion := BrookVersionStr
  else
    FVersion := '';
end;

procedure TBrookLibraryLoader.Unload;
begin
  FHandle := SgUnloadLibrary;
  FEnabled := FHandle <> NilHandle;
  if not FEnabled then
    FVersion := '';
end;

end.
