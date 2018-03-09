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
    FFileName: TFileName;
    FHandle: TLibHandle;
    FVersion: string;
    procedure SetFileName(const AValue: TFileName);
  protected
    function GetHandle: Pointer; override;
    procedure DoLoad; virtual;
  public
    { Creates an instance of @link(TBrookLibraryLoader). }
    constructor Create(AOwner: TComponent); override;
    { Returns @true when the library was successfully loaded. }
    function IsLoaded: Boolean;
  published
    { Specifies the file name of the library to be loaded dynamically. }
    property FileName: TFileName read FFileName write SetFileName;
    { Version of the loaded library. }
    property Version: string read FVersion stored False;
  end;

implementation

constructor TBrookLibraryLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetFileName(BK_LIB_NAME);
end;

function TBrookLibraryLoader.GetHandle: Pointer;
begin
  Result := @FHandle;
end;

procedure TBrookLibraryLoader.SetFileName(const AValue: TFileName);
begin
  if AValue = FFileName then
    Exit;
  FFileName := AValue;
  FHandle := BkUnloadLibrary;
  DoLoad;
end;

function TBrookLibraryLoader.IsLoaded: Boolean;
begin
  Result := FHandle <> NilHandle;
end;

procedure TBrookLibraryLoader.DoLoad;
begin
  if FHandle <> NilHandle then
    Exit;
  FHandle := BkLoadLibrary(FFileName);
  if FHandle <> NilHandle then
    FVersion := BrookVersionStr
  else
    FVersion := '';
end;

end.
