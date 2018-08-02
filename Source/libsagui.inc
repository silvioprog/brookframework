(*                         _
 *   ___  __ _  __ _ _   _(_)
 *  / __|/ _` |/ _` | | | | |
 *  \__ \ (_| | (_| | |_| | |
 *  |___/\__,_|\__, |\__,_|_|
 *             |___/
 *
 *   –– a smart C library which helps you write quickly REST APIs.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Sagui library.
 *
 * Sagui library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sagui library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Sagui library.  If not, see <http://www.gnu.org/licenses/>.
 *)

{$IFDEF FPC}
 {$MODE DELPHI}
 {$IFDEF VER3_0}
  {$PUSH}{$MACRO ON}
  {$DEFINE MarshaledAString := PAnsiChar}
  {$POP}
 {$ENDIF}
 {$IFNDEF VER3_0} //{$IF FPC_FULLVERSION >= 30101} commented to fix "[dcc32 Error] E2026 Constant expression expected"
  {$WARN 6058 OFF} // Note: Call to subroutine "%s" marked as inline is not inlined
 {$ENDIF}
{$ENDIF}
{$DEFINE SG_HTTPS_SUPPORT}
