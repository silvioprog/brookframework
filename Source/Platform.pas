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

{ Platform symbols. }

unit Platform;

{$I libbrook.inc}

interface

uses
{$IF DEFINED(MSWINDOWS)}
  Windows
{$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
  BaseUnix
{$ELSEIF DEFINED(POSIX)}
  Posix.Errno
{$ENDIF};

const
{$IF DEFINED(FPC) AND DEFINED(UNIX)}
  EINVAL = ESysEINVAL;
  EALREADY = ESysEALREADY;
  ENOENT = ESysENOENT;
{$ELSEIF DEFINED(POSIX)}
  EINVAL = Posix.Errno.EINVAL;
  EALREADY = Posix.Errno.EALREADY;
  ENOENT = Posix.Errno.ENOENT;
{$ELSEIF DEFINED(MSWINDOWS)}
  EINVAL = 22;
  EALREADY = 103;
  ENOENT = ERROR_FILE_NOT_FOUND;
{$ENDIF}

implementation

end.
