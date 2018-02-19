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

{ All base types supporting a library handle. }

unit BrookHandledClasses;

{$I Brook.inc}

interface

uses
  Classes;

type
  { Main RTTI handled type. }
  TBrookHandledPersistent = class abstract(TPersistent)
  protected
    function GetHandle: Pointer; virtual; abstract;
  public
    { Handle of a loaded library. }
    property Handle: Pointer read GetHandle;
  end;

  { Main RTTI handled component. }
  TBrookHandledComponent = class abstract(TComponent)
  protected
    function GetHandle: Pointer; virtual; abstract;
  public
    { Handle of a loaded library. }
    property Handle: Pointer read GetHandle;
  end;

implementation

end.

