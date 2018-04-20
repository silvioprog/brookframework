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
  UnixType,
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
{$IF DEFINED(MSWINDOWS)}
  cbool = {$IFNDEF FPC}Winapi.{$ENDIF}Windows.BOOL;
  cuint16_t = UInt16;
  cint = {$IFNDEF FPC}Winapi.{$ENDIF}Windows.LONG;
  cuint = {$IFNDEF FPC}Winapi.{$ENDIF}Windows.UINT;
  cuint64_t = {$IFNDEF FPC}Winapi.{$ENDIF}Windows.ULONG64;
  csize_t = {$IFDEF FPC}System{$ELSE}Winapi.Windows{$ENDIF}.SIZE_T;
  cssize_t = {$IFDEF FPC}NativeInt{$ELSE}Winapi.Windows.SSIZE_T{$ENDIF};
  ctime_t = NativeUInt;
{$ELSEIF DEFINED(POSIX)}
  cbool = LongBool;
  cuint16_t = UInt16;
  cint = Integer;
  cuint = Cardinal;
  cuint64_t = UInt64;
  csize_t = Posix.SysTypes.size_t;
  cssize_t = Posix.SysTypes.ssize_t;
  ctime_t = Posix.SysTypes.time_t;
{$ELSEIF DEFINED(UNIX)}
  cbool = UnixType.cbool;
  cuint16_t = UnixType.cuint16;
  cint = UnixType.cint;
  cuint = UnixType.cuint;
  cuint64_t = UnixType.cuint64;
  csize_t = UnixType.size_t;
  cssize_t = UnixType.ssize_t;
  ctime_t = UnixType.time_t;
{$ELSE}
  cbool = LongBool;
  cuint16_t = UInt16;
  cint = Integer;
  cuint = Cardinal;
  cuint64_t = UInt64;
  csize_t = NativeUInt;
  cssize_t = NativeInt;
  ctime_t = NativeUInt;
{$ENDIF}
  Pcvoid = Pointer;
  PPcvoid = PPointer;
  cenum = cint;
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

type
  bk_err_cb = procedure(cls: Pcvoid; const err: Pcchar); cdecl;

  bk_write_cb = function(handle: Pcvoid; offset: cuint64_t; const buf: Pcchar;
    size: csize_t): csize_t; cdecl;

  bk_read_cb = function(handle: Pcvoid; offset: cuint64_t; buf: Pcchar;
    size: csize_t): cssize_t; cdecl;

  bk_free_cb = procedure(handle: Pcvoid); cdecl;

  bk_save_cb = function(handle: Pcvoid; overwritten: cbool): cint; cdecl;

  bk_save_as_cb = function(handle: Pcvoid; const path: Pcchar;
    overwritten: cbool): cint; cdecl;

var
  bk_version: function: cuint; cdecl;
  bk_version_str: function: Pcchar; cdecl;
  bk_alloc: function(size: csize_t): Pcvoid; cdecl;
  bk_free: procedure(ptr: Pcvoid); cdecl;
  bk_strerror: function(errnum: cint; buf: Pcchar; len: csize_t): Pcchar; cdecl;
  bk_tmpdir: function: Pcchar; cdecl;

type
  Pbk_str = ^bk_str;
  bk_str = record
  end;

var
  bk_str_new: function: Pbk_str; cdecl;
  bk_str_free: procedure(str: Pbk_str); cdecl;
  bk_str_write: function(str: Pbk_str; const val: Pcchar;
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
  bk_strmap_get: function(map: Pbk_strmap; const name: Pcchar): Pcchar; cdecl;
  bk_strmap_rm: function(map: PPbk_strmap; const name: Pcchar): cint; cdecl;
  bk_strmap_iter: function(map: Pbk_strmap; cb: bk_strmap_iter_cb;
    cls: Pcvoid): cint; cdecl;
  bk_strmap_sort: function(map: PPbk_strmap; cb: bk_strmap_sort_cb;
    cls: Pcvoid): cint; cdecl;
  bk_strmap_count: function(map: Pbk_strmap): cuint; cdecl;
  bk_strmap_next: function(next: PPbk_strmap): cint; cdecl;
  bk_strmap_cleanup: procedure(map: PPbk_strmap); cdecl;

type
  Pbk_httpauth = ^bk_httpauth;
  bk_httpauth = record
  end;

  PPbk_httpupld = ^Pbk_httpupld;
  Pbk_httpupld = ^bk_httpupld;
  bk_httpupld = record
  end;

  Pbk_httpreq = ^bk_httpreq;
  bk_httpreq = record
  end;

  Pbk_httpres = ^bk_httpres;
  bk_httpres = record
  end;

  Pbk_httpsrv = ^bk_httpsrv;
  bk_httpsrv = record
  end;

type
  bk_httpauth_cb = function(cls: Pcvoid; auth: Pbk_httpauth): cbool; cdecl;

  bk_httpupld_cb = function(cls: Pcvoid; handle: PPcvoid; const dir: Pcchar;
    const field: Pcchar; const name: Pcchar; const mime: Pcchar;
    const encoding: Pcchar): cint; cdecl;

  bk_httpreq_cb = procedure(cls: Pcvoid; req: Pbk_httpreq;
    res: Pbk_httpres); cdecl;

var
  bk_httpauth_version: function(auth: Pbk_httpauth): Pcchar; cdecl;
  bk_httpauth_method: function(auth: Pbk_httpauth): Pcchar; cdecl;
  bk_httpauth_path: function(auth: Pbk_httpauth): Pcchar; cdecl;

  bk_httpauth_set_realm: function(auth: Pbk_httpauth;
    const realm: Pcchar): cint; cdecl;
  bk_httpauth_realm: function(auth: Pbk_httpauth): pcchar; cdecl;
  bk_httpauth_deny: function(auth: Pbk_httpauth; const justification: Pcchar;
    const content_type: Pcchar): cint; cdecl;
  bk_httpauth_cancel: function(auth: Pbk_httpauth): cint; cdecl;
  bk_httpauth_usr: function(auth: Pbk_httpauth): Pcchar; cdecl;
  bk_httpauth_pwd: function(auth: Pbk_httpauth): Pcchar; cdecl;

  bk_httpuplds_next: function(uplds: Pbk_httpupld;
    upld: PPbk_httpupld): cint; cdecl;
  bk_httpupld_save: function(upld: Pbk_httpupld;
    overwritten: cbool): cint; cdecl;
  bk_httpupld_save_as: function(upld: Pbk_httpupld; const path: Pcchar;
    overwritten: cbool): cint; cdecl;

  bk_httpreq_headers: function(req: Pbk_httpreq): PPbk_strmap; cdecl;
  bk_httpreq_cookies: function(req: Pbk_httpreq): PPbk_strmap; cdecl;
  bk_httpreq_params: function(req: Pbk_httpreq): PPbk_strmap; cdecl;
  bk_httpreq_fields: function(req: Pbk_httpreq): PPbk_strmap; cdecl;
  bk_httpreq_version: function(req: Pbk_httpreq): Pcchar; cdecl;
  bk_httpreq_method: function(req: Pbk_httpreq): Pcchar; cdecl;
  bk_httpreq_path: function(req: Pbk_httpreq): Pcchar; cdecl;
  bk_httpreq_is_post: function(req: Pbk_httpreq): cbool; cdecl;
  bk_httpreq_payload: function(req: Pbk_httpreq): Pbk_str; cdecl;
  bk_httpreq_uploads: function(req: Pbk_httpreq): PPbk_httpupld; cdecl;
  bk_httpreq_set_userdata: function(req: Pbk_httpreq;
    data: Pcvoid): cint; cdecl;
  bk_httpreq_userdata: function(req: Pbk_httpreq): Pcvoid; cdecl;

  bk_httpres_headers: function(res: Pbk_httpres): PPbk_strmap; cdecl;
  bk_httpres_set_cookie: function(res: Pbk_httpres; const name: Pcchar;
    const val: Pcchar): cint; cdecl;
  bk_httpres_printf_va: function(res: Pbk_httpres; const content_type: Pcchar;
    status: cuint; const fmt: Pcchar; ap: cva_list): cint; cdecl;
  bk_httpres_printf: function(res: Pbk_httpres; const content_type: Pcchar;
    status: cuint; const fmt: Pcchar): cint; cdecl varargs;
  bk_httpres_send: function(res: Pbk_httpres; const val: Pcchar;
    const content_type: Pcchar; status: cuint): cint; cdecl;
  bk_httpres_sendbinary: function(res: Pbk_httpres; buf: Pcvoid; size: size_t;
    const content_type: Pcchar; status: cuint): cint; cdecl;
  bk_httpres_sendstr: function(res: Pbk_httpres; str: Pbk_str;
    const content_type: Pcchar; status: cuint): cint; cdecl;
  bk_httpres_sendfile: function(res: Pbk_httpres; block_size: csize_t;
    max_size: cuint64_t; const filename: Pcchar; rendered: cbool;
    status: cuint): cint; cdecl;
  bk_httpres_sendstream: function(res: Pbk_httpres; size: cuint64_t;
    block_size: csize_t; read_cb: bk_read_cb; handle: Pcvoid;
    flush_cb: bk_free_cb; status: cuint): cint; cdecl;
  bk_httpres_senddata: function(res: Pbk_httpres; block_size: csize_t;
    read_cb: bk_read_cb; handle: Pcvoid; free_cb: bk_free_cb;
    status: cuint): cint; cdecl;

  bk_httpsrv_new2: function(auth_cb: bk_httpauth_cb; auth_cls: Pcvoid;
    req_cb: bk_httpreq_cb; req_cls: Pcvoid; err_cb: bk_err_cb;
    err_cls: Pcvoid): Pbk_httpsrv; cdecl;
  bk_httpsrv_new: function(cb: bk_httpreq_cb; cls: Pcvoid): Pbk_httpsrv; cdecl;
  bk_httpsrv_free: procedure(srv: Pbk_httpsrv); cdecl;
  bk_httpsrv_listen: function(srv: Pbk_httpsrv; port: cuint16_t;
    threaded: cbool): cint; cdecl;
  bk_httpsrv_shutdown: function(srv: Pbk_httpsrv): cint; cdecl;
  bk_httpsrv_port: function(srv: Pbk_httpsrv): cuint16_t; cdecl;
  bk_httpsrv_threaded: function(srv: Pbk_httpsrv): cbool; cdecl;
  bk_httpsrv_set_upld_cbs: function(srv: Pbk_httpsrv; cb: bk_httpupld_cb;
    cls: Pcvoid; write_cb: bk_write_cb; free_cb: bk_free_cb;
    save_cb: bk_save_cb; save_as_cb: bk_save_as_cb): cint; cdecl;
  bk_httpsrv_set_upld_dir: function(srv: Pbk_httpsrv;
    const dir: Pcchar): cint; cdecl;
  bk_httpsrv_set_post_buf_size: function(srv: Pbk_httpsrv;
    size: csize_t): cint; cdecl;
  bk_httpsrv_set_max_payld_size: function(srv: Pbk_httpsrv;
    size: csize_t): cint; cdecl;
  bk_httpsrv_set_max_uplds_size: function(srv: Pbk_httpsrv;
    size: cuint64_t): cint; cdecl;
  bk_httpsrv_set_thr_pool_size: function(srv: Pbk_httpsrv;
    size: cuint): cint; cdecl;
  bk_httpsrv_set_con_timeout: function(srv: Pbk_httpsrv;
    timeout: cuint): cint; cdecl;
  bk_httpsrv_set_con_limit: function(srv: Pbk_httpsrv;
    limit: cuint): cint; cdecl;

  bk_httpread_end: function(err: cbool): cssize_t; cdecl;

{ TODO: procedure BkAddUnloadLibraryProc }
function BkLoadLibrary(const AFileName: TFileName): TLibHandle;
function BkUnloadLibrary: TLibHandle;
procedure BkCheckLibrary;
procedure BkCheckLastError(ALastError: Integer); inline;

implementation

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
      if GetLastError = ERROR_BAD_EXE_FORMAT then
      begin
        MessageBox(0, PChar(Format(SBkInvalidLibrary, [AFileName])), nil,
          MB_OK or MB_ICONERROR);
        Halt;
      end;
{$ELSE}
      Exit(NilHandle);
{$ENDIF}
    { TODO: check the library version }
    GBkLastLibName := AFileName;

    bk_version := GetProcAddress(GBkLibHandle, 'bk_version');
    bk_version_str := GetProcAddress(GBkLibHandle, 'bk_version_str');
    bk_alloc := GetProcAddress(GBkLibHandle, 'bk_alloc');
    bk_free := GetProcAddress(GBkLibHandle, 'bk_free');
    bk_strerror := GetProcAddress(GBkLibHandle, 'bk_strerror');
    bk_tmpdir := GetProcAddress(GBkLibHandle, 'bk_tmpdir');

    bk_str_new := GetProcAddress(GBkLibHandle, 'bk_str_new');
    bk_str_free := GetProcAddress(GBkLibHandle, 'bk_str_free');
    bk_str_write := GetProcAddress(GBkLibHandle, 'bk_str_write');
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
    bk_strmap_get := GetProcAddress(GBkLibHandle, 'bk_strmap_get');
    bk_strmap_rm := GetProcAddress(GBkLibHandle, 'bk_strmap_rm');
    bk_strmap_iter := GetProcAddress(GBkLibHandle, 'bk_strmap_iter');
    bk_strmap_sort := GetProcAddress(GBkLibHandle, 'bk_strmap_sort');
    bk_strmap_count := GetProcAddress(GBkLibHandle, 'bk_strmap_count');
    bk_strmap_next := GetProcAddress(GBkLibHandle, 'bk_strmap_next');
    bk_strmap_cleanup := GetProcAddress(GBkLibHandle, 'bk_strmap_cleanup');

    bk_httpauth_version := GetProcAddress(GBkLibHandle, 'bk_httpauth_version');
    bk_httpauth_method := GetProcAddress(GBkLibHandle, 'bk_httpauth_method');
    bk_httpauth_path := GetProcAddress(GBkLibHandle, 'bk_httpauth_path');
    bk_httpauth_set_realm := GetProcAddress(GBkLibHandle, 'bk_httpauth_set_realm');
    bk_httpauth_realm := GetProcAddress(GBkLibHandle, 'bk_httpauth_realm');
    bk_httpauth_deny := GetProcAddress(GBkLibHandle, 'bk_httpauth_deny');
    bk_httpauth_cancel := GetProcAddress(GBkLibHandle, 'bk_httpauth_cancel');
    bk_httpauth_usr := GetProcAddress(GBkLibHandle, 'bk_httpauth_usr');
    bk_httpauth_pwd := GetProcAddress(GBkLibHandle, 'bk_httpauth_pwd');

    bk_httpuplds_next := GetProcAddress(GBkLibHandle, 'bk_httpuplds_next');
    bk_httpupld_save := GetProcAddress(GBkLibHandle, 'bk_httpupld_save');
    bk_httpupld_save_as := GetProcAddress(GBkLibHandle, 'bk_httpupld_save_as');

    bk_httpreq_headers := GetProcAddress(GBkLibHandle, 'bk_httpreq_headers');
    bk_httpreq_cookies := GetProcAddress(GBkLibHandle, 'bk_httpreq_cookies');
    bk_httpreq_params := GetProcAddress(GBkLibHandle, 'bk_httpreq_params');
    bk_httpreq_fields := GetProcAddress(GBkLibHandle, 'bk_httpreq_fields');
    bk_httpreq_version := GetProcAddress(GBkLibHandle, 'bk_httpreq_version');
    bk_httpreq_method := GetProcAddress(GBkLibHandle, 'bk_httpreq_method');
    bk_httpreq_path := GetProcAddress(GBkLibHandle, 'bk_httpreq_path');
    bk_httpreq_is_post := GetProcAddress(GBkLibHandle, 'bk_httpreq_is_post');
    bk_httpreq_payload := GetProcAddress(GBkLibHandle, 'bk_httpreq_payload');
    bk_httpreq_uploads := GetProcAddress(GBkLibHandle, 'bk_httpreq_uploads');
    bk_httpreq_set_userdata := GetProcAddress(GBkLibHandle, 'bk_httpreq_set_userdata');
    bk_httpreq_userdata := GetProcAddress(GBkLibHandle, 'bk_httpreq_userdata');

    bk_httpres_headers := GetProcAddress(GBkLibHandle, 'bk_httpres_headers');
    bk_httpres_set_cookie := GetProcAddress(GBkLibHandle, 'bk_httpres_set_cookie');
    bk_httpres_printf_va := GetProcAddress(GBkLibHandle, 'bk_httpres_printf_va');
    bk_httpres_printf := GetProcAddress(GBkLibHandle, 'bk_httpres_printf');
    bk_httpres_send := GetProcAddress(GBkLibHandle, 'bk_httpres_send');
    bk_httpres_sendbinary := GetProcAddress(GBkLibHandle, 'bk_httpres_sendbinary');
    bk_httpres_sendstr := GetProcAddress(GBkLibHandle, 'bk_httpres_sendstr');
    bk_httpres_sendfile := GetProcAddress(GBkLibHandle, 'bk_httpres_sendfile');
    bk_httpres_sendstream := GetProcAddress(GBkLibHandle, 'bk_httpres_sendstream');
    bk_httpres_senddata := GetProcAddress(GBkLibHandle, 'bk_httpres_senddata');

    bk_httpsrv_new2 := GetProcAddress(GBkLibHandle, 'bk_httpsrv_new2');
    bk_httpsrv_new := GetProcAddress(GBkLibHandle, 'bk_httpsrv_new');
    bk_httpsrv_free := GetProcAddress(GBkLibHandle, 'bk_httpsrv_free');
    bk_httpsrv_listen := GetProcAddress(GBkLibHandle, 'bk_httpsrv_listen');
    bk_httpsrv_shutdown := GetProcAddress(GBkLibHandle, 'bk_httpsrv_shutdown');
    bk_httpsrv_port := GetProcAddress(GBkLibHandle, 'bk_httpsrv_port');
    bk_httpsrv_threaded := GetProcAddress(GBkLibHandle, 'bk_httpsrv_threaded');
    bk_httpsrv_set_upld_cbs := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_upld_cbs');
    bk_httpsrv_set_upld_dir := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_upld_dir');
    bk_httpsrv_set_post_buf_size := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_post_buf_size');
    bk_httpsrv_set_max_payld_size := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_max_payld_size');
    bk_httpsrv_set_max_uplds_size := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_max_uplds_size');
    bk_httpsrv_set_thr_pool_size := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_thr_pool_size');
    bk_httpsrv_set_con_timeout := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_con_timeout');
    bk_httpsrv_set_con_limit := GetProcAddress(GBkLibHandle, 'bk_httpsrv_set_con_limit');

    bk_httpread_end := GetProcAddress(GBkLibHandle, 'bk_httpread_end');

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
    bk_strerror := nil;
    bk_tmpdir := nil;

    bk_str_new := nil;
    bk_str_free := nil;
    bk_str_write := nil;
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
    bk_strmap_get := nil;
    bk_strmap_rm := nil;
    bk_strmap_iter := nil;
    bk_strmap_sort := nil;
    bk_strmap_count := nil;
    bk_strmap_next := nil;
    bk_strmap_cleanup := nil;

    bk_httpauth_version := nil;
    bk_httpauth_method := nil;
    bk_httpauth_path := nil;
    bk_httpauth_set_realm := nil;
    bk_httpauth_realm := nil;
    bk_httpauth_deny := nil;
    bk_httpauth_cancel := nil;
    bk_httpauth_usr := nil;
    bk_httpauth_pwd := nil;

    bk_httpuplds_next := nil;
    bk_httpupld_save := nil;
    bk_httpupld_save_as := nil;

    bk_httpreq_headers := nil;
    bk_httpreq_cookies := nil;
    bk_httpreq_params := nil;
    bk_httpreq_fields := nil;
    bk_httpreq_version := nil;
    bk_httpreq_method := nil;
    bk_httpreq_path := nil;
    bk_httpreq_is_post := nil;
    bk_httpreq_payload := nil;
    bk_httpreq_uploads := nil;
    bk_httpreq_set_userdata := nil;
    bk_httpreq_userdata := nil;

    bk_httpres_headers := nil;
    bk_httpres_set_cookie := nil;
    bk_httpres_printf_va := nil;
    bk_httpres_printf := nil;
    bk_httpres_send := nil;
    bk_httpres_sendbinary := nil;
    bk_httpres_sendstr := nil;
    bk_httpres_sendfile := nil;
    bk_httpres_sendstream := nil;
    bk_httpres_senddata := nil;

    bk_httpsrv_new2 := nil;
    bk_httpsrv_new := nil;
    bk_httpsrv_free := nil;
    bk_httpsrv_listen := nil;
    bk_httpsrv_shutdown := nil;
    bk_httpsrv_port := nil;
    bk_httpsrv_threaded := nil;
    bk_httpsrv_set_upld_cbs := nil;
    bk_httpsrv_set_upld_dir := nil;
    bk_httpsrv_set_post_buf_size := nil;
    bk_httpsrv_set_max_payld_size := nil;
    bk_httpsrv_set_max_uplds_size := nil;
    bk_httpsrv_set_thr_pool_size := nil;
    bk_httpsrv_set_con_timeout := nil;
    bk_httpsrv_set_con_limit := nil;

    bk_httpread_end := nil;

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

procedure BkCheckLastError(ALastError: Integer);
const
  BUF_LEN = 255;
var
  B: TBytes;
begin
  if (ALastError = 0) or (not Assigned(bk_strerror)) then
    Exit;
  SetLength(B, BUF_LEN);
  bk_strerror(ALastError, @B[0], BUF_LEN + SizeOf(Byte));
  raise EOSError.Create(
{$IFDEF FPC}string({$ENDIF}TEncoding.UTF8.GetString(B, 0, BUF_LEN)){$IFDEF FPC}){$ENDIF};
end;

initialization
  GBkLock := TCriticalSection.Create;
  BkLoadLibrary(BK_LIB_NAME);

finalization
  BkUnloadLibrary;
  FreeAndNil(GBkLock);

end.
