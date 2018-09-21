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
  { Indicates not allowed operation when the library loader is loaded. }
  SBrookActiveLibLoader = 'Active library loader.';

type
  { Class for dynamic library loading. }
  TBrookLibraryLoader = class(TBrookHandledComponent)
  private
    FActive: Boolean;
    FVersion: string;
    FHandle: TLibHandle;
    FLibraryName: TFileName;
    FStreamedActive: Boolean;
    function IsActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetLibraryName(const AValue: TFileName);
  protected
    procedure Loaded; override;
    procedure CheckInactive; inline;
    function GetHandle: Pointer; override;
  public
    { Loads the library dynamically.
      @return(@True if the library is succesfully loaded.) }
    class function Load(const ALibraryName: TFileName): Boolean;
    { Loads the library dynamically. }
    procedure Open; virtual;
    { Unloads the library dynamically. }
    procedure Close; virtual;
    { @exclude }
    procedure DefineProperties(AFiler: TFiler); override;
  published
    { Loads/Unloads the library dynamically. }
    property Active: Boolean read FActive write SetActive stored IsActive;
    { Specifies the library to be loaded dynamically. }
    property LibraryName: TFileName read FLibraryName write SetLibraryName;
    { Version of the loaded library. }
    property Version: string read FVersion stored False;
  end;

implementation

procedure TBrookLibraryLoader.CheckInactive;
begin
  if not (csLoading in ComponentState) and Active then
    raise EInvalidOpException.CreateRes(@SBrookActiveLibLoader);
end;

procedure TBrookLibraryLoader.Loaded;
begin
  inherited Loaded;
  if FActive then
    Open;
end;

procedure TBrookLibraryLoader.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  if FActive and not FStreamedActive then
  begin
    FStreamedActive := True;
    Open;
  end;
end;

function TBrookLibraryLoader.GetHandle: Pointer;
begin
  Result := @FHandle;
end;

class function TBrookLibraryLoader.Load(const ALibraryName: TFileName): Boolean;
begin
  Result := SgLib.Load(ALibraryName) <> NilHandle;
end;

procedure TBrookLibraryLoader.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csLoading in ComponentState then
    FActive := AValue
  else
    if AValue then
      Open
    else
      Close;
end;

function TBrookLibraryLoader.IsActive: Boolean;
begin
  Result := FActive;
end;

procedure TBrookLibraryLoader.SetLibraryName(const AValue: TFileName);
begin
  if AValue = FLibraryName then
    Exit;
  CheckInactive;
  FLibraryName := AValue;
end;

procedure TBrookLibraryLoader.Open;
begin
  FHandle := SgLib.Load(FLibraryName);
  FActive := FHandle <> NilHandle;
  if FActive then
    FVersion := BrookVersionStr
  else
    FVersion := '';
end;

procedure TBrookLibraryLoader.Close;
begin
  FHandle := SgLib.Unload;
  FActive := FHandle <> NilHandle;
  if not FActive then
    FVersion := '';
end;

end.
