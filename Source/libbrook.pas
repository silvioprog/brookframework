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

{$IF DEFINED(FPC) OR DEFINED(POSIX)}
 {$DEFINE BK_GNUCC_BUILT} // uses Brook library built in any GNU C compiler
{$ELSEIF DEFINED(MSWINDOWS)}
 {$DEFINE BK_MSVC_BUILT} // uses Brook library built in any GNU C compiler
{$ELSE}
 {$MESSAGE FATAL 'Unknown environment'}
{$ENDIF}

interface

{$IF DEFINED(BK_GNUCC_BUILT)}
uses
  SysUtils
 {$IF DEFINED(UNIX)}
  , BaseUnix
 {$ELSEIF DEFINED(POSIX)}
  , Posix.SysTypes
 {$ENDIF};
{$ELSEIF DEFINED(BK_MSVC_BUILT)}
uses
  System.Win.Crtl,
  Winapi.Windows;
{$ENDIF}

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

const
  BK_PU = {$IFDEF BK_MSVC_BUILT}'_'{$ELSE}''{$ENDIF};

{$IFDEF BK_MSVC_BUILT}
procedure _exit; cdecl; external msvcrt name 'exit';
{$ENDIF}

{$IFDEF VER3_0}
procedure CheckOSError(LastError: Integer); inline;
{$ENDIF}

{ Utility }

function bk_version: cuint; cdecl;
  external name Concat(BK_PU, 'bk_version');

function bk_version_str: Pcchar; cdecl;
  external name Concat(BK_PU, 'bk_version_str');

function bk_alloc(size: csize_t): Pcvoid; cdecl;
  external name Concat(BK_PU, 'bk_alloc');

procedure bk_free(ptr: Pcvoid); cdecl;
  external name Concat(BK_PU, 'bk_free');

{ String }

type
  Pbk_str = ^bk_str;
  bk_str = record
  end;

function bk_str_new: Pbk_str; cdecl;
  external name Concat(BK_PU, 'bk_str_new');

procedure bk_str_free(str: Pbk_str); cdecl;
  external name Concat(BK_PU, 'bk_str_free');

function bk_str_write(str: Pbk_str; const val: Pcchar;
  len: csize_t): cint; cdecl;
  external name Concat(BK_PU, 'bk_str_write');

function bk_str_read(str: Pbk_str; val: Pcchar; len: Pcsize_t): cint; cdecl;
  external name Concat(BK_PU, 'bk_str_read');

function bk_str_printf_va(str: Pbk_str; const fmt: Pcchar;
  ap: cva_list): cint; cdecl;
  external name Concat(BK_PU, 'bk_str_printf_va');

function bk_str_printf(str: Pbk_str; const fmt: Pcchar): cint; cdecl; varargs;
  external name Concat(BK_PU, 'bk_str_printf');

function bk_str_content(str: Pbk_str): Pcchar; cdecl;
  external name Concat(BK_PU, 'bk_str_content');

function bk_str_length(str: Pbk_str; len: Pcsize_t): cint; cdecl;
  external name Concat(BK_PU, 'bk_str_length');

function bk_str_clear(str: Pbk_str): cint; cdecl;
  external name Concat(BK_PU, 'bk_str_clear');

type
  PPbk_strmap = ^Pbk_strmap;
  Pbk_strmap = ^bk_strmap;
  bk_strmap = record
  end;

  bk_strmap_iter_cb = function(cls: Pcvoid; const name: Pcchar;
    name_len: csize_t; const val: Pcchar; val_len: csize_t): cint; cdecl;

function bk_strmap_add(map: PPbk_strmap; const name: Pcchar; name_len: csize_t;
  const val: Pcchar; val_len: csize_t): cint; cdecl;
  external name Concat(BK_PU, 'bk_strmap_add');

function bk_strmap_find(map: Pbk_strmap; const name: Pcchar; name_len: csize_t;
  val: Pcchar; val_len: Pcsize_t): cint; cdecl;
  external name Concat(BK_PU, 'bk_strmap_find');

function bk_strmap_iter(map: Pbk_strmap; iter_cb: bk_strmap_iter_cb;
  iter_cls: Pcvoid): cint; cdecl;
  external name Concat(BK_PU, 'bk_strmap_iter');

procedure bk_strmap_cleanup(map: PPbk_strmap); cdecl;
  external name Concat(BK_PU, 'bk_strmap_cleanup');

implementation

{$IFDEF VER3_0}
procedure CheckOSError(LastError: Integer);
begin
  if LastError <> 0 then
    RaiseLastOSError(LastError);
end;
{$ENDIF}

{$IF DEFINED(BK_MSVC_BUILT)}
 {$LINK bk_str.obj}
 {$LINK bk_utils.obj}
{$ELSEIF DEFINED(BK_GNUCC_BUILT)}
 {$LINKLIB libbrook.a}
 {$IFDEF UNIX}
  {$LINKLIB c}
 {$ENDIF}
 {$IFDEF MSWINDOWS}
  {$LINKLiB libmingwex.a}
  {$LINKLIB libmsvcrt.a}
 {$ENDIF}
{$ENDIF}

end.