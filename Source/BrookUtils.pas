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
  Marshalling,
{$IFDEF VER3_0_0}
  FPC300Fixes,
{$ENDIF}
  libsagui;

type
  TBrookErrorEvent = procedure(ASender: TObject;
    AException: Exception) of object;

{
  Returns the library version number.
  @return(Library version packed into a single integer.)
}
function BrookVersion: Cardinal; overload;

{ experimental }
function BrookVersion(out AMajor, AMinor: Byte;
  out APatch: SmallInt): Cardinal; overload;

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
function BrookExtractEntryPoint(const APath: string): string;

{ experimental }
function BrookTmpDir: string;

{ experimental }
function BrookFixPath(const APath: string): string; inline;

{ experimental }
function BrookFixEntryPoint(const APath: string): string;

implementation

function BrookVersion: Cardinal;
begin
  SgLib.Check;
  Result := sg_version;
end;

function BrookVersion(out AMajor, AMinor: Byte; out APatch: SmallInt): Cardinal;
begin
  SgLib.Check;
  Result := sg_version;
  AMajor := (Result shr 16) and $FF;
  AMinor := (Result shr 8) and $FF;
  APatch := Result and $FF;
end;

function BrookVersionStr: string;
begin
  SgLib.Check;
  Result := TMarshal.ToString(sg_version_str);
end;

function BrookAlloc(ASize: NativeUInt): Pointer;
begin
  SgLib.Check;
  Result := sg_alloc(ASize);
end;

procedure BrookFree(APtr: Pointer);
begin
  SgLib.Check;
  sg_free(APtr);
end;

function BrookStrError(AErrorNum: Integer): string;
var
  P: array[0..SG_ERR_SIZE-1] of cchar;
begin
  SgLib.Check;
  sg_strerror(AErrorNum, @P[0], SG_ERR_SIZE);
  Result := TMarshal.ToString(@P[0]);
end;

function BrookIsPost(const AMethod: string): Boolean;
var
  M: TMarshaller;
begin
  SgLib.Check;
  Result := sg_is_post(M.ToCString(AMethod));
end;

function BrookExtractEntryPoint(const APath: string): string;
var
  M: TMarshaller;
  S: Pcchar;
begin
  SgLib.Check;
  S := sg_extract_entrypoint(M.ToCString(APath));
  try
    Result := TMarshal.ToString(S);
  finally
    sg_free(S);
  end;
end;

function BrookTmpDir: string;
var
  S: Pcchar;
begin
  SgLib.Check;
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

function BrookFixEntryPoint(const APath: string): string;
var
  PS: TArray<string>;
begin
  PS := APath.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  Result := '/';
  if Length(PS) > 0 then
    Result := Concat(Result, PS[0]);
end;

end.
