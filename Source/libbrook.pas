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
 {$IF DEFINED(UNIX)}
  BaseUnix,
 {$ELSEIF DEFINED(MSWINDOWS)}
  Windows,
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
  NilHandle = HMODULE(0);
{$ENDIF}

{$IF (NOT DEFINED(FPC)) OR DEFINED(VER3_0)}
  SharedSuffix =
 {$IF DEFINED(MSWINDOWS)}
    'dll'
 {$ELSEIF DEFINED(MACOS)}
    'dylib'
 {$ELSE}
    'so'
 {$ENDIF};
{$ENDIF}

  BK_LIB_NAME = Concat(
{$IFDEF MSWINDOWS}
    'libbrook-0'
{$ELSE}
    'libbrook'
{$ENDIF}, '.', SharedSuffix);

resourcestring
  SBkLibraryNotLoaded = 'Library ''%s'' not loaded';
{$IFDEF MSWINDOWS}
  SBkInvalidLibrary = 'Invalid library ''%s''';
{$ENDIF}

type
  Pcchar = MarshaledAString;
{$IF DEFINED(FPC) AND DEFINED(UNIX)}
  cbool = BaseUnix.cbool;
  cushort = BaseUnix.cushort;
  cint = BaseUnix.cint;
  cuint = BaseUnix.cuint;
  Pcuint = BaseUnix.pcuint;
{$ELSE}
  cbool = Boolean;
  cushort = Word;
  cint = Integer;
  cuint = Cardinal;
  Pcuint = ^cuint;
{$ENDIF}
{$IFDEF FPC}
 {$IFDEF MSWINDOWS}
  csize_t = System.size_t;
 {$ENDIF}
{$ELSE}
  csize_t =
 {$IFDEF POSIX}
    Posix.SysTypes
 {$ELSE}
    Winapi.Windows
 {$ENDIF}.size_t;
  Pcsize_t = ^csize_t;
{$ENDIF}
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

var
  bk_version: function: cuint; cdecl;
  bk_version_str: function: Pcchar; cdecl;
  bk_alloc: function(size: csize_t): Pcvoid; cdecl;
  bk_free: procedure(ptr: Pcvoid); cdecl;

type
  Pbk_str = ^bk_str;
  bk_str = record
  end;

var
  bk_str_new: function: Pbk_str; cdecl;
  bk_str_free: procedure(str: Pbk_str); cdecl;
  bk_str_strcpy: function(str: Pbk_str; const val: Pcchar;
    len: csize_t): cint; cdecl;
  bk_str_printf_va: function(str: Pbk_str; const fmt: Pcchar;
    ap: cva_list): cint; cdecl;
  bk_str_printf: function(str: Pbk_str; const fmt: Pcchar): cint; cdecl varargs;
  bk_str_content: function(str: Pbk_str): Pcchar; cdecl;
  bk_str_length: function(str: Pbk_str): csize_t; cdecl;
  bk_str_clear: function(str: Pbk_str): cint; cdecl;

type
  PPbk_strmap = ^Pbk_strmap;
  Pbk_strmap = ^bk_strmap;
  bk_strmap = record
  end;

  bk_strmap_iter_cb = function(cls: Pcvoid; pair: Pbk_strmap): cint; cdecl;

  bk_strmap_sort_cb = function(cls: Pcvoid; pair_a: Pbk_strmap;
    pair_b: Pbk_strmap): cint; cdecl;

var
  bk_strmap_name: function(pair: Pbk_strmap): Pcchar; cdecl;
  bk_strmap_val: function(pair: Pbk_strmap): Pcchar; cdecl;
  bk_strmap_add: function(map: PPbk_strmap; const name: Pcchar;
    const val: Pcchar): cint; cdecl;
  bk_strmap_set: function(map: PPbk_strmap; const name: Pcchar;
    const val: Pcchar): cint; cdecl;
  bk_strmap_find: function(map: Pbk_strmap; const name: Pcchar;
    pair: PPbk_strmap): cint; cdecl;
  bk_strmap_rm: function(map: PPbk_strmap; const name: Pcchar): cint; cdecl;
  bk_strmap_iter: function(map: Pbk_strmap; cb: bk_strmap_iter_cb;
    cls: Pcvoid): cint; cdecl;
  bk_strmap_sort: function(map: PPbk_strmap; cb: bk_strmap_sort_cb;
    cls: Pcvoid): cint; cdecl;
  bk_strmap_count: function(map: Pbk_strmap): cuint; cdecl;
  bk_strmap_next: function(next: PPbk_strmap): cint; cdecl;
  bk_strmap_cleanup: procedure(map: PPbk_strmap); cdecl;

type
  Pbk_httpreq = ^bk_httpreq;
  bk_httpreq = record
  end;

  Pbk_httpres = ^bk_httpres;
  bk_httpres = record
  end;

  Pbk_httpsrv = ^bk_httpsrv;
  bk_httpsrv = record
  end;

  bk_httperr_cb = procedure(cls: Pcvoid; const err: Pcchar); cdecl;

  bk_httpreq_cb = procedure(cls: Pcvoid; req: Pbk_httpreq;
    res: Pbk_httpres); cdecl;

var
  bk_httpsrv_new2: function(req_cb: bk_httpreq_cb; req_cls: Pcvoid;
    err_cb: bk_httperr_cb; err_cls: Pcvoid): Pbk_httpsrv; cdecl;

  bk_httpsrv_new: function(cb: bk_httpreq_cb; cls: Pcvoid): Pbk_httpsrv; cdecl;

  bk_httpsrv_free: procedure(srv: Pbk_httpsrv); cdecl;

  bk_httpsrv_start: function(srv: Pbk_httpsrv; port: cushort;
    threaded: cbool): cint; cdecl;

  bk_httpsrv_stop: function(srv: Pbk_httpsrv): cint; cdecl;

  bk_httpres_send: function(res: Pbk_httpres; const val: Pcchar;
    const content_type: Pcchar; status: cuint): cint; cdecl;

  bk_httpres_sendbinary: function(res: Pbk_httpres; buf: Pcvoid; size: size_t;
    const content_type: Pcchar; status: cuint): cint; cdecl;

  bk_httpres_sendstr: function(res: Pbk_httpres; str: Pbk_str;
    const content_type: Pcchar; status: cuint): cint; cdecl;

  bk_httpres_sendfile: function(res: Pbk_httpres; const filename: Pcchar;
    rendered: cbool): cint; cdecl;

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
{$IFDEF MSWINDOWS}
    begin
      if GetLastError = ERROR_BAD_EXE_FORMAT then
        raise EBkLibraryNotLoaded.CreateResFmt(@SBkInvalidLibrary, [AFileName]);
{$ENDIF}
      Exit(NilHandle);
{$IFDEF MSWINDOWS}
    end;
{$ENDIF}
    { TODO: check the library version }
    GBkLastLibName := AFileName;

    bk_version := GetProcAddress(GBkLibHandle, 'bk_version');
    bk_version_str := GetProcAddress(GBkLibHandle, 'bk_version_str');
    bk_alloc := GetProcAddress(GBkLibHandle, 'bk_alloc');
    bk_free := GetProcAddress(GBkLibHandle, 'bk_free');

    bk_str_new := GetProcAddress(GBkLibHandle, 'bk_str_new');
    bk_str_free := GetProcAddress(GBkLibHandle, 'bk_str_free');
    bk_str_strcpy := GetProcAddress(GBkLibHandle, 'bk_str_strcpy');
    bk_str_printf_va := GetProcAddress(GBkLibHandle, 'bk_str_printf_va');
    bk_str_printf := GetProcAddress(GBkLibHandle, 'bk_str_printf');
    bk_str_content := GetProcAddress(GBkLibHandle, 'bk_str_content');
    bk_str_length := GetProcAddress(GBkLibHandle, 'bk_str_length');
    bk_str_clear := GetProcAddress(GBkLibHandle, 'bk_str_clear');

    bk_strmap_name := GetProcAddress(GBkLibHandle, 'bk_strmap_name');
    bk_strmap_val := GetProcAddress(GBkLibHandle, 'bk_strmap_val');
    bk_strmap_add := GetProcAddress(GBkLibHandle, 'bk_strmap_add');
    bk_strmap_set := GetProcAddress(GBkLibHandle, 'bk_strmap_set');
    bk_strmap_find := GetProcAddress(GBkLibHandle, 'bk_strmap_find');
    bk_strmap_rm := GetProcAddress(GBkLibHandle, 'bk_strmap_rm');
    bk_strmap_iter := GetProcAddress(GBkLibHandle, 'bk_strmap_iter');
    bk_strmap_sort := GetProcAddress(GBkLibHandle, 'bk_strmap_sort');
    bk_strmap_count := GetProcAddress(GBkLibHandle, 'bk_strmap_count');
    bk_strmap_next := GetProcAddress(GBkLibHandle, 'bk_strmap_next');
    bk_strmap_cleanup := GetProcAddress(GBkLibHandle, 'bk_strmap_cleanup');

    bk_httpsrv_new2 := GetProcAddress(GBkLibHandle, 'bk_httpsrv_new2');
    bk_httpsrv_new := GetProcAddress(GBkLibHandle, 'bk_httpsrv_new');
    bk_httpsrv_free := GetProcAddress(GBkLibHandle, 'bk_httpsrv_free');
    bk_httpsrv_start := GetProcAddress(GBkLibHandle, 'bk_httpsrv_start');
    bk_httpsrv_stop := GetProcAddress(GBkLibHandle, 'bk_httpsrv_stop');
    bk_httpres_send := GetProcAddress(GBkLibHandle, 'bk_httpres_send');
    bk_httpres_sendbinary := GetProcAddress(GBkLibHandle, 'bk_httpres_sendbinary');
    bk_httpres_sendstr := GetProcAddress(GBkLibHandle, 'bk_httpres_sendstr');
    bk_httpres_sendfile := GetProcAddress(GBkLibHandle, 'bk_httpres_sendfile');

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
    bk_str_strcpy := nil;
    bk_str_printf_va := nil;
    bk_str_printf := nil;
    bk_str_content := nil;
    bk_str_length := nil;
    bk_str_clear := nil;

    bk_strmap_name := nil;
    bk_strmap_val := nil;
    bk_strmap_add := nil;
    bk_strmap_set := nil;
    bk_strmap_find := nil;
    bk_strmap_rm := nil;
    bk_strmap_iter := nil;
    bk_strmap_sort := nil;
    bk_strmap_count := nil;
    bk_strmap_next := nil;
    bk_strmap_cleanup := nil;

    bk_httpsrv_new2 := nil;
    bk_httpsrv_new := nil;
    bk_httpsrv_free := nil;
    bk_httpsrv_start := nil;
    bk_httpsrv_stop := nil;
    bk_httpres_send := nil;
    bk_httpres_sendbinary := nil;
    bk_httpres_sendstr := nil;
    bk_httpres_sendfile := nil;

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
