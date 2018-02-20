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

{ Useful types for marshalling arguments. }

unit Marshalling;

{$I libbrook.inc}

interface

uses
  SysUtils,
  libbrook;

type

  { TMarshal* }

{$IFDEF FPC}
  TMarshal = record
{$ELSE}
  TMarshalHelper = class helper for TMarshal
{$ENDIF}
  public
    class function ToBytes(const S: Pcchar;
      const Z: csize): TBytes; static; inline;
    class function ToString(const S: Pcchar): string; static; inline;
  end;

  { TMarshaller* }

{$IFDEF FPC}
  TMarshaller = record
{$ELSE}
  TMarshallerHelper = record helper for TMarshaller
{$ENDIF}
  public
    function ToCString(const S: string): Pcchar; inline;
  end;

implementation

{ TMarshal* }

class function {$IFDEF FPC}TMarshal{$ELSE}TMarshalHelper{$ENDIF}.ToBytes(
  const S: Pcchar; const Z: csize): TBytes;
begin
  if (not Assigned(S)) or (Z = 0) then
    Exit(nil);
  SetLength(Result, Z);
  System.Move(S^, Result[0], Z);
end;

class function {$IFDEF FPC}TMarshal{$ELSE}TMarshalHelper{$ENDIF}.ToString(
  const S: Pcchar): string;
{$IFDEF FPC}
var
  B: Pcchar;
{$ENDIF}
begin
  if not Assigned(S) then
    Exit('');
{$IFDEF FPC}
  B := Pcchar(@S[0]);
  SetString(Result, B, Length(B));
  SetCodePage(RawByteString(Result), CP_UTF8, False);
{$ELSE}
  Result := TMarshal.ReadStringAsUtf8(TPtrWrapper.Create(S));
{$ENDIF}
end;

{ TMarshaller* }

function {$IFDEF FPC}TMarshaller{$ELSE}TMarshallerHelper{$ENDIF}.ToCString(
  const S: string): Pcchar;
begin
  Result :=
{$IFDEF FPC}
    Pcchar(S)
{$ELSE}
    AsAnsi(S, CP_UTF8).ToPointer
{$ENDIF};
end;

end.