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
  SysUtils
{$IFDEF FPC}
 {$IFDEF UNIX}
  , BaseUnix
 {$ENDIF}
{$ELSE}
 {$IF DEFINED(MSWINDOWS)}
  , Winapi.Windows
 {$ELSEIF DEFINED(POSIX)}
  , Posix.SysTypes
 {$ENDIF}
{$ENDIF};

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

{$IFDEF VER3_0}
procedure CheckOSError(LastError: Integer); inline;
{$ENDIF}

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
  bk_str_write: function(str: Pbk_str; const val: Pcchar;
    len: csize_t): cint; cdecl;
  bk_str_read: function(str: Pbk_str; val: Pcchar; len: Pcsize_t): cint; cdecl;
  bk_str_printf_va: function(str: Pbk_str; const fmt: Pcchar;
    ap: cva_list): cint; cdecl;
  bk_str_printf: function(str: Pbk_str; const fmt: Pcchar): cint; cdecl varargs;
  bk_str_content: function(str: Pbk_str): Pcchar; cdecl;
  bk_str_length: function(str: Pbk_str; len: Pcsize_t): cint; cdecl;
  bk_str_clear: function(str: Pbk_str): cint; cdecl;

type
  PPbk_strmap = ^Pbk_strmap;
  Pbk_strmap = ^bk_strmap;
  bk_strmap = record
  end;

  bk_strmap_iter_cb = function(cls: Pcvoid; const name: Pcchar;
    name_len: csize_t; const val: Pcchar; val_len: csize_t): cint; cdecl;

var
  bk_strmap_add: function(map: PPbk_strmap; const name: Pcchar;
    name_len: csize_t; const val: Pcchar; val_len: csize_t): cint; cdecl;
  bk_strmap_find: function(map: Pbk_strmap; const name: Pcchar;
    name_len: csize_t; val: Pcchar; val_len: Pcsize_t): cint; cdecl;
  bk_strmap_iter: function(map: Pbk_strmap; iter_cb: bk_strmap_iter_cb;
    iter_cls: Pcvoid): cint; cdecl;
  bk_strmap_cleanup: procedure(map: PPbk_strmap); cdecl;

implementation

{$IFDEF VER3_0}
procedure CheckOSError(LastError: Integer);
begin
  if LastError <> 0 then
    RaiseLastOSError(LastError);
end;
{$ENDIF}

end.
