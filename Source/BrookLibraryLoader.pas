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

type
  { Class for dynamic library loading. }
  TBrookLibraryLoader = class(TBrookHandledComponent)
  private
    FLibLoaded: Boolean;
    FFileName: TFileName;
    FHandle: TLibHandle;
    FVersion: string;
    function IsFileNameStored: Boolean;
    procedure SetFileName(const AValue: TFileName);
  protected
    function GetHandle: Pointer; override;
    procedure DoLoad; virtual;
  public
    { Creates an instance of @link(TBrookLibraryLoader). }
    constructor Create(AOwner: TComponent); override;
    procedure DefineProperties(AFiler: TFiler); override;
  published
    { Specifies the file name of the library to be loaded dynamically. }
    property FileName: TFileName read FFileName write SetFileName
      stored IsFileNameStored;
    { Version of the loaded library. }
    property Version: string read FVersion stored False;
  end;

implementation

constructor TBrookLibraryLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName := BK_LIB_NAME;
end;

procedure TBrookLibraryLoader.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  DoLoad;
end;

function TBrookLibraryLoader.IsFileNameStored: Boolean;
begin
  Result := FFileName <> BK_LIB_NAME;
end;

procedure TBrookLibraryLoader.SetFileName(const AValue: TFileName);
begin
  if FFileName = AValue then
    Exit;
  FLibLoaded := False;
  FFileName := AValue;
  DoLoad;
end;

function TBrookLibraryLoader.GetHandle: Pointer;
begin
  Result := @FHandle;
end;

procedure TBrookLibraryLoader.DoLoad;
begin
  if FLibLoaded then
    Exit;
  BkUnloadLibrary;
  FHandle := NilHandle;
  FLibLoaded := False;
  if (FFileName = '') or (not FileExists(FFileName)) then
    Exit;
  FHandle := BkLoadLibrary(FFileName);
  FLibLoaded := FHandle <> NilHandle;
  if FLibLoaded then
    FVersion := BrookVersionStr
  else
    FVersion := '';
end;

end.
