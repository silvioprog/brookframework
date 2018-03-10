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

{ Dynamic library loader. }

unit BrookLibraryLoader;

{$I Brook.inc}

interface

uses
  SysUtils,
  Classes,
  libbrook,
  BrookHandledClasses,
  BrookUtils;

resourcestring
  { Indicates not allowed operation when the library is loaded. }
  SBrookOpNotAllowedLoadedLib =
    'Operation is not allowed while the library is loaded';

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
    procedure CheckDisabled; inline;
    procedure SetEnabled(AValue: Boolean);
    procedure SetLibraryName(const AValue: TFileName);
  protected
    procedure Loaded; override;
    function GetHandle: Pointer; override;
  public
    { Loads the library dynamically. }
    procedure Load; virtual;
    { Unloads the library dynamically. }
    procedure Unload; virtual;
  published
    { Loads/Unloads the library dynamically. }
    property Enabled: Boolean read FEnabled write SetEnabled;
    { Specifies the library to be loaded dynamically. }
    property LibraryName: TFileName read FLibraryName write SetLibraryName;
    { Version of the loaded library. }
    property Version: string read FVersion stored False;
  end;

implementation

procedure TBrookLibraryLoader.Loaded;
begin
  inherited Loaded;
  if FEnabled and (FLibraryName <> '') then
    Load;
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

procedure TBrookLibraryLoader.SetLibraryName(const AValue: TFileName);
begin
  if AValue = FLibraryName then
    Exit;
  CheckDisabled;
  FLibraryName := AValue;
end;

procedure TBrookLibraryLoader.Load;
begin
  FHandle := BkLoadLibrary(FLibraryName);
  FEnabled := FHandle <> NilHandle;
  if FEnabled then
    FVersion := BrookVersionStr
  else
    FVersion := '';
end;

procedure TBrookLibraryLoader.Unload;
begin
  FHandle := BkUnloadLibrary;
  FEnabled := FHandle <> NilHandle;
  if not FEnabled then
    FVersion := '';
end;

end.
