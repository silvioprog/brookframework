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

{ String type used to represent a HTML body, POST payload and more. }

unit BrookString;

{$I Brook.inc}

interface

uses
  SysUtils,
  libbrook,
  Marshalling,
  BrookHandledClasses;

{ TODO: TBrookString.Assign() }
{ TODO: TBrookString.Read() }
{ TODO: TBrookString.Format() }

type
  { String class and its related methods. }
  TBrookString = class(TBrookHandledPersistent)
  private
    Fstr: Pbk_str;
    FOwnsHandle: Boolean;
    function GetContent: TBytes;
    function GetLength: NativeUInt;
  protected
    function GetHandle: Pointer; override;
    function GetOwnsHandle: Boolean; override;
    procedure SetOwnsHandle(AValue: Boolean); override;
  public
    { Creates an instance of TBrookString. }
    constructor Create(AHandle: Pointer); virtual;
    { Frees an instance of TBrookString. }
    destructor Destroy; override;
    { Writes a string buffer to the string handle. All strings previously
      written are kept. }
    function Write(const AValue: TBytes;
      ALength: NativeUInt): NativeUInt; virtual;
    { Reads a string buffer from the string handle. }
    function Read(AValue: TBytes; ALength: NativeUInt): NativeUInt; virtual;
    { Cleans all the content present in the string handle. }
    procedure Clear; virtual;
    { Gets the content buffer from the string handle. }
    property Content: TBytes read GetContent;
    { Gets the content length from the string handle. }
    property Length: NativeUInt read GetLength;
  end;

implementation

constructor TBrookString.Create(AHandle: Pointer);
begin
  inherited Create;
  FOwnsHandle := not Assigned(AHandle);
  if FOwnsHandle then
    Fstr := bk_str_new;
end;

destructor TBrookString.Destroy;
begin
  if FOwnsHandle then
    bk_str_free(Fstr);
  inherited Destroy;
end;

function TBrookString.GetHandle: Pointer;
begin
  Result := Fstr;
end;

function TBrookString.GetOwnsHandle: Boolean;
begin
  Result := FOwnsHandle;
end;

procedure TBrookString.SetOwnsHandle(AValue: Boolean);
begin
  FOwnsHandle := AValue;
end;

function TBrookString.Write(const AValue: TBytes;
  ALength: NativeUInt): NativeUInt;
begin
  CheckHandle;
  Result := ALength;
  bk_str_write(Fstr, @AValue[0], Result);
  { TODO: check the function result. }
end;

function TBrookString.Read(AValue: TBytes; ALength: NativeUInt): NativeUInt;
begin
  CheckHandle;
  Result := ALength;
  bk_str_read(Fstr, @AValue[0], @ALength);
  { TODO: check the function result. }
end;

procedure TBrookString.Clear;
begin
  CheckHandle;
  bk_str_clear(Fstr);
end;

function TBrookString.GetLength: NativeUInt;
begin
  CheckHandle;
  bk_str_length(Fstr, @Result);
end;

function TBrookString.GetContent: TBytes;
begin
  Result := TMarshal.ToBytes(bk_str_content(Fstr), GetLength);
end;

end.