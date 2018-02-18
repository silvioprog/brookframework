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

unit libbrook;

{$I libbrook.inc}

interface

{$IF DEFINED(FPC) AND DEFINED(UNIX)}
uses
  BaseUnix;
{$ELSE}
uses
 {$IFDEF MSWINDOWS}
  System.Win.Crtl,
  Winapi.Windows
 {$ELSEIF POSIX}
  Posix.SysTypes
 {$ENDIF};
{$ENDIF}

type
  Pcchar = MarshaledAString;
  cuint = Cardinal;
  csize =
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
  Pcvoid = Pointer;

const
  BK_LIB_NAME = {$IFDEF BK_LIB_EXTERNAL}{ TODO: use LIB_NAME }{$ELSE}''{$ENDIF};
  BK_PU = {$IF (NOT DEFINED(FPC)) AND DEFINED(WIN32)}'_'{$ELSE}''{$ENDIF};

{$IF (NOT DEFINED(FPC)) AND DEFINED(WIN32)}
procedure _exit; cdecl; external msvcrt name 'exit';
{$ENDIF}

function bk_version: cuint; cdecl; external BK_LIB_NAME name Concat(BK_PU, 'bk_version');

function bk_version_str: Pcchar; cdecl; external BK_LIB_NAME name Concat(BK_PU, 'bk_version_str');

function bk_alloc: Pcvoid; cdecl; external BK_LIB_NAME name Concat(BK_PU, 'bk_alloc');

procedure bk_free(ptr: Pcvoid); cdecl; external BK_LIB_NAME name Concat(BK_PU, 'bk_free');

implementation

{$IFDEF FPC}
 {$LINKLIB brook}
{$ELSE}
 {$LINK bk_str.obj}
 {$LINK bk_utils.obj}
{$ENDIF}

end.