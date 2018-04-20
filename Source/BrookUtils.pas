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

{ Utility functions of the library. }

unit BrookUtils;

{$I Brook.inc}

interface

uses
  SysUtils,
  libbrook,
  Marshalling;

{
  Returns the library version number.
  @return(Library version packed into a single integer.)
}
function BrookVersion: Cardinal;

{
  Returns the library version number as string.
  @return(Library version packed into a static string.)
}
function BrookVersionStr: string;

{
  Allocates a new memory space and zero-initialize it.

  @param(ASize[in] Memory size to be allocated.)

  @return(Pointer of the allocated zero-initialized memory.

    @bold(Returns values:)

    @definitionList(
      @itemLabel(@code(nil))
      @item(When size is @code(0) or no memory space.)
    )
  )
}
function BrookAlloc(ASize: NativeUInt): Pointer;

{
  Frees a memory space previous allocated by @link(BrookAlloc).
  @param(APtr[in] Pointer of the memory to be freed.)
}
procedure BrookFree(APtr: Pointer);

{ experimental: it will be documented and tested as soon as it is accepted as better API. }
function BrookStrError(AErrorNum: Integer; ALength: Integer): string; overload;

function BrookStrError(AErrorNum: Integer): string; overload;

{ experimental: it will be documented and tested as soon as it is accepted as better API. }
function BrookTmpDir: string;

{ experimental: it will be documented and tested as soon as it is accepted as better API. }
procedure BrookHTime(ATime: Int64; ABuffer: TBytes; ALength: NativeUInt;
  AGMT: Boolean); overload;

{ experimental: it will be documented and tested as soon as it is accepted as better API. }
function BrookHTime(ATime: Int64; AGMT: Boolean): string; overload;

implementation

function BrookVersion: Cardinal;
begin
  BkCheckLibrary;
  Result := bk_version;
end;

function BrookVersionStr: string;
begin
  BkCheckLibrary;
  Result := TMarshal.ToString(bk_version_str);
end;

function BrookAlloc(ASize: NativeUInt): Pointer;
begin
  BkCheckLibrary;
  Result := bk_alloc(ASize);
end;

procedure BrookFree(APtr: Pointer);
begin
  BkCheckLibrary;
  bk_free(APtr);
end;

function BrookStrError(AErrorNum: Integer; ALength: Integer): string;
var
  B: TBytes;
begin
  BkCheckLibrary;
  SetLength(B, ALength);
  Result := TMarshal.ToString(bk_strerror(AErrorNum, @B[0], ALength), ALength);
end;

function BrookStrError(AErrorNum: Integer): string;
begin
  Result := BrookStrError(AErrorNum, 256);
end;

function BrookTmpDir: string;
begin
  BkCheckLibrary;
  Result := TMarshal.ToString(bk_tmpdir);
end;

procedure BrookHTime(ATime: Int64; ABuffer: TBytes; ALength: NativeUInt;
  AGMT: Boolean);
begin
  BkCheckLibrary;
  BkCheckLastError(-bk_htime(ATime, @ABuffer[0], ALength, AGMT));
end;

function BrookHTime(ATime: Int64; AGMT: Boolean): string;
const
  BUF_LEN = 29;
var
  B: TBytes;
begin
  SetLength(B, BUF_LEN);
  BrookHTime(ATime, B, BUF_LEN + SizeOf(Byte), AGMT);
  SetLength(Result, BUF_LEN);
  Move(B[0], Result[1], BUF_LEN);
end;

end.
