(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– an ideal Pascal microframework to develop cross-platform HTTP servers.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook framework is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook framework.  If not, see <http://www.gnu.org/licenses/>.
 *)

{ Utility functions of the library. }

unit BrookUtils;

{$I Brook.inc}

interface

uses
  SysUtils,
  libsagui,
  Marshalling;

type
  TBrookErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

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

{ experimental }
function BrookStrError(AErrorNum: Integer): string;

{ experimental }
function BrookIsPost(const AMethod: string): Boolean;

{ experimental }
function BrookTmpDir: string;

{ experimental }
function BrookFixPath(const APath: string): string; inline;

implementation

function BrookVersion: Cardinal;
begin
  SgCheckLibrary;
  Result := sg_version;
end;

function BrookVersionStr: string;
begin
  SgCheckLibrary;
  Result := TMarshal.ToString(sg_version_str);
end;

function BrookAlloc(ASize: NativeUInt): Pointer;
begin
  SgCheckLibrary;
  Result := sg_alloc(ASize);
end;

procedure BrookFree(APtr: Pointer);
begin
  SgCheckLibrary;
  sg_free(APtr);
end;

function BrookStrError(AErrorNum: Integer): string;
var
  P: array[0..255] of cchar;
begin
  SgCheckLibrary;
  sg_strerror(AErrorNum, @P[0], Length(P));
  Result := TMarshal.ToString(@P[0]);
end;

function BrookIsPost(const AMethod: string): Boolean;
var
  M: TMarshaller;
begin
  SgCheckLibrary;
  Result := sg_is_post(M.ToCString(AMethod));
end;

function BrookTmpDir: string;
var
  S: Pcchar;
begin
  SgCheckLibrary;
  S := sg_tmpdir;
  try
    Result := TMarshal.ToString(S);
  finally
    sg_free(S);
  end;
end;

function BrookFixPath(const APath: string): string;
begin
  Result := APath;
  if not APath.StartsWith('/') then
    Result := Concat('/', Result);
  if (Length('/') > SizeOf(Char)) and Result.EndsWith('/') then
    SetLength(Result, Length(Result) - Length('/'));
end;

end.
