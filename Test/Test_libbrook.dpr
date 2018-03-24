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

program Test_libbrook;

{$I Tests.inc}

uses
  SysUtils,
  libbrook;

procedure Test_LoadLibrary;
begin
  BkUnloadLibrary;
  Assert(BkLoadLibrary('') = NilHandle);
  Assert(BkLoadLibrary('abc') = NilHandle);
  Assert(BkLoadLibrary(BK_LIB_NAME) <> NilHandle);
end;

procedure Test_UnloadLibrary;
begin
  Assert(BkLoadLibrary(BK_LIB_NAME) <> NilHandle);
  Assert(BkUnloadLibrary = NilHandle);
end;

procedure Test_CheckLibrary;
var
  OK: Boolean;
begin
  OK := False;
  try
    BkUnloadLibrary;
    BkCheckLibrary;
  except
    on E: Exception do
      OK := (E.ClassType = EBkLibraryNotLoaded) and (E.Message =
        Format(SBkLibraryNotLoaded, [BK_LIB_NAME]));
  end;
  Assert(OK);
end;

begin
  Test_LoadLibrary;
  Test_UnloadLibrary;
  Test_CheckLibrary;
end.