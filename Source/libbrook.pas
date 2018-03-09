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

{ Cross-platform low-level Pascal binding for the Brook library. }

unit libbrook;

{$I libbrook.inc}

interface

uses
  SysUtils,
  StrUtils,
{$IFDEF FPC}
 {$IFDEF UNIX}
  BaseUnix,
 {$ENDIF}
  DynLibs,
{$ELSE}
 {$IF DEFINED(MSWINDOWS)}
  Winapi.Windows
 {$ELSEIF DEFINED(POSIX)}
  Posix.Dlfcn,
  Posix.SysTypes
 {$ENDIF},
{$ENDIF}
  SyncObjs;

const
{$IFDEF FPC}
 {$IFDEF VER3_0}
  NilHandle = DynLibs.NilHandle;
 {$ENDIF}
{$ELSE}
  SharedSuffix =
 {$IF DEFINED(MSWINDOWS)}
    'dll'
 {$ELSEIF DEFINED(MACOS)}
    'dylib'
 {$ELSE}
    'so'
 {$ENDIF};
  NilHandle = HMODULE(0);
{$ENDIF}

  BK_LIB_NAME = Concat(
{$IFDEF MSWINDOWS}
    'libbrook-0'
{$ELSE}
    'libbrook'
{$ENDIF}, '.', SharedSuffix);

resourcestring
  SBkLibraryNotLoaded = 'Brook library ''%s'' not loaded';

type
  Pcchar = MarshaledAString;
  cint = Integer;
  cuint = Cardinal;
  csize_t =
{$IFDEF FPC}
 {$IFDEF UNIX}
    BaseUnix
 {$ELSE}
    System
 {$ENDIF}
{$ELSE}
 {$IFDEF POSIX}
    Posix.SysTypes
 {$ELSE}
    Winapi.Windows
 {$ENDIF}
{$ENDIF}.size_t;
  Pcsize_t = ^csize_t;
  Pcvoid = Pointer;
  cva_list = Pointer;

{$IFDEF FPC}
 {$PACKRECORDS C}
 {$IFDEF VER3_0}
  TLibHandle = DynLibs.TLibHandle;
 {$ENDIF}
{$ELSE}
  TLibHandle = HMODULE;
{$ENDIF}

  EBkLibraryNotLoaded = class(EFileNotFoundException);

  Pbk_str = ^bk_str;
  bk_str = record
  end;

  PPbk_strmap = ^Pbk_strmap;
  Pbk_strmap = ^bk_strmap;
  bk_strmap = record
  end;

  bk_strmap_iter_cb = function(cls: Pcvoid; const name: Pcchar;
    name_len: csize_t; const val: Pcchar; val_len: csize_t): cint; cdecl;

var
  bk_version: function: cuint; cdecl;
  bk_version_str: function: Pcchar; cdecl;
  bk_alloc: function(size: csize_t): Pcvoid; cdecl;
  bk_free: procedure(ptr: Pcvoid); cdecl;

  bk_str_new: function: Pbk_str; cdecl;
  bk_str_free: procedure(str: Pbk_str); cdecl;
  bk_str_write: function(str: Pbk_str; const val: Pcchar;
    len: csize_t): cint; cdecl;
  bk_str_read: function(str: Pbk_str; val: Pcchar; len: Pcsize_t): cint; cdecl;
  bk_str_printf_va: function(str: Pbk_str; const fmt: Pcchar;
    ap: cva_list): cint; cdecl;
  bk_str_printf: function(str: Pbk_str; const fmt: Pcchar): cint; cdecl varargs;
  bk_str_content: function(str: Pbk_str): Pcchar; cdecl;
  bk_str_length: function(str: Pbk_str; len: Pcsize_t): cint; cdecl;
  bk_str_clear: function(str: Pbk_str): cint; cdecl;

  bk_strmap_add: function(map: PPbk_strmap; const name: Pcchar;
    name_len: csize_t; const val: Pcchar; val_len: csize_t): cint; cdecl;
  bk_strmap_find: function(map: Pbk_strmap; const name: Pcchar;
    name_len: csize_t; val: Pcchar; val_len: Pcsize_t): cint; cdecl;
  bk_strmap_iter: function(map: Pbk_strmap; iter_cb: bk_strmap_iter_cb;
    iter_cls: Pcvoid): cint; cdecl;
  bk_strmap_cleanup: procedure(map: PPbk_strmap); cdecl;

{$IFDEF VER3_0}
procedure CheckOSError(LastError: Integer); platform; inline;
{$ENDIF}

function BkLoadLibrary(const AFileName: TFileName): TLibHandle;
function BkUnloadLibrary: TLibHandle;
procedure BkCheckLibrary;

implementation

{$IFDEF VER3_0}
procedure CheckOSError(LastError: Integer);
begin
  if LastError <> 0 then
    RaiseLastOSError(LastError);
end;
{$ENDIF}

var
  GBkLock: TCriticalSection = nil;
  GBkLibHandle: TLibHandle = NilHandle;
  GBkLastLibName: TFileName = BK_LIB_NAME;

function BkLoadLibrary(const AFileName: TFileName): TLibHandle;
begin
  GBkLock.Acquire;
  try
    if (GBkLibHandle <> NilHandle) or (AFileName = '') then
      Exit(GBkLibHandle);
    GBkLibHandle := SafeLoadLibrary(AFileName);
    if GBkLibHandle = NilHandle then
      Exit(NilHandle);
    { TODO: check the library version }
    GBkLastLibName := AFileName;

    bk_version := GetProcAddress(GBkLibHandle, 'bk_version');
    bk_version_str := GetProcAddress(GBkLibHandle, 'bk_version_str');
    bk_alloc := GetProcAddress(GBkLibHandle, 'bk_alloc');
    bk_free := GetProcAddress(GBkLibHandle, 'bk_free');

    bk_str_new := GetProcAddress(GBkLibHandle, 'bk_str_new');
    bk_str_free := GetProcAddress(GBkLibHandle, 'bk_str_free');
    bk_str_write := GetProcAddress(GBkLibHandle, 'bk_str_write');
    bk_str_read := GetProcAddress(GBkLibHandle, 'bk_str_read');
    bk_str_printf_va := GetProcAddress(GBkLibHandle, 'bk_str_printf_va');
    bk_str_printf := GetProcAddress(GBkLibHandle, 'bk_str_printf');
    bk_str_content := GetProcAddress(GBkLibHandle, 'bk_str_content');
    bk_str_length := GetProcAddress(GBkLibHandle, 'bk_str_length');
    bk_str_clear := GetProcAddress(GBkLibHandle, 'bk_str_clear');

    bk_strmap_add := GetProcAddress(GBkLibHandle, 'bk_strmap_add');
    bk_strmap_find := GetProcAddress(GBkLibHandle, 'bk_strmap_find');
    bk_strmap_iter := GetProcAddress(GBkLibHandle, 'bk_strmap_iter');
    bk_strmap_cleanup := GetProcAddress(GBkLibHandle, 'bk_strmap_cleanup');

    Result := GBkLibHandle;
  finally
    GBkLock.Release;
  end;
end;

function BkUnloadLibrary: TLibHandle;
begin
  GBkLock.Acquire;
  try
    if GBkLibHandle = NilHandle then
      Exit(NilHandle);
    if not FreeLibrary(GBkLibHandle) then
      Exit(GBkLibHandle);
    GBkLibHandle := NilHandle;
    GBkLastLibName := '';

    bk_version := nil;
    bk_version_str := nil;
    bk_alloc := nil;
    bk_free := nil;

    bk_str_new := nil;
    bk_str_free := nil;
    bk_str_write := nil;
    bk_str_read := nil;
    bk_str_printf_va := nil;
    bk_str_printf := nil;
    bk_str_content := nil;
    bk_str_length := nil;
    bk_str_clear := nil;

    bk_strmap_add := nil;
    bk_strmap_find := nil;
    bk_strmap_iter := nil;
    bk_strmap_cleanup := nil;

    Result := GBkLibHandle;
  finally
    GBkLock.Release;
  end;
end;

procedure BkCheckLibrary;
begin
  if GBkLibHandle = NilHandle then
    raise EBkLibraryNotLoaded.CreateResFmt(@SBkLibraryNotLoaded,
      [IfThen(GBkLastLibName = '', BK_LIB_NAME, GBkLastLibName)]);
end;

initialization
  GBkLock := TCriticalSection.Create;
  BkLoadLibrary(BK_LIB_NAME);

finalization
  BkUnloadLibrary;
  FreeAndNil(GBkLock);

end.
