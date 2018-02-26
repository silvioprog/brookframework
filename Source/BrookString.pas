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
  RTLConsts,
  SysUtils,
  Classes,
  libbrook,
  Marshalling,
  BrookHandledClasses;

{ TODO: TBrookString.Assign() }

type
  { String class and its related methods. }
  TBrookString = class(TBrookHandledPersistent)
  private
    Fstr: Pbk_str;
    FOwnsHandle: Boolean;
    function GetContent: TBytes;
    function GetLength: NativeUInt;
    procedure SetText(const AValue: string);
  protected
    class procedure CheckEncoding(AEncoding: TEncoding); static; inline;
    function GetHandle: Pointer; override;
    function GetOwnsHandle: Boolean; override;
    procedure SetOwnsHandle(AValue: Boolean); override;
  public
    { Creates an instance of @link(TBrookString). }
    constructor Create(AHandle: Pointer); virtual;
    { Frees an instance of @link(TBrookString). }
    destructor Destroy; override;
    { Writes a string buffer to the string handle. All strings previously
      written are kept.

      @param(AValue String buffer to be written.)
      @param(ALength Length of the string buffer being written.)

      @returns(Length of the written string buffer.) }
    function WriteBytes(const AValue: TBytes;
      ALength: NativeUInt): NativeUInt; virtual;
    { Reads a string buffer from the string handle.

      @param(AValue Array of byte to store the read string buffer.)
      @param(ALength Length of array to store the string buffer being read.)

      @returns(Length of the read string buffer.) }
    function ReadBytes(AValue: TBytes; ALength: NativeUInt): NativeUInt; virtual;
    { Writes a string to the string handle. All strings previously written are
      kept.

      @param(AValue String to be written.)
      @param(AEncoding Determines the encoding of the string being read.) }
    procedure Write(const AValue: string;
      AEncoding: TEncoding); overload; virtual;
    { Writes a string to the string handle. All strings previously written are
      kept.

      @param(AValue String to be written.) }
    procedure Write(const AValue: string); overload; virtual;
    { Reads a string from the string handle.

      @param(AEncoding Determines the encoding of the string being read.)

      @returns(String read from the string handler.) }
    function Read(AEncoding: TEncoding): string; overload; virtual;
    { Reads a string from the string handle.

      @returns(String read from the string handler.) }
    function Read: string; overload; virtual;
    { Cleans all the content present in the string handle. }
    procedure Clear; virtual;
    { Gets the content buffer from the string handle. }
    property Content: TBytes read GetContent;
    { Gets the content length from the string handle. }
    property Length: NativeUInt read GetLength;
    { Gets or sets a string to the string handle. }
    property Text: string read Read write SetText;
  end;

implementation

constructor TBrookString.Create(AHandle: Pointer);
begin
  inherited Create;
  FOwnsHandle := not Assigned(AHandle);
  if FOwnsHandle then
    Fstr := bk_str_new
  else
    Fstr := AHandle;
end;

destructor TBrookString.Destroy;
begin
  if FOwnsHandle then
  begin
    bk_str_free(Fstr);
    Fstr := nil;
  end;
  inherited Destroy;
end;

class procedure TBrookString.CheckEncoding(AEncoding: TEncoding);
begin
  if not Assigned(AEncoding) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AEncoding']);
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

function TBrookString.WriteBytes(const AValue: TBytes;
  ALength: NativeUInt): NativeUInt;
begin
  CheckHandle;
  Result := ALength;
  CheckOSError(bk_str_write(Fstr, @AValue[0], Result));
end;

function TBrookString.ReadBytes(AValue: TBytes; ALength: NativeUInt): NativeUInt;
begin
  CheckHandle;
  CheckOSError(bk_str_read(Fstr, @AValue[0], @ALength));
  Result := ALength;
end;

{$IFDEF FPC}{$PUSH}{$WARN 4104 OFF}{$ENDIF}
procedure TBrookString.Write(const AValue: string; AEncoding: TEncoding);
var
  VBytes: TBytes;
begin
  CheckHandle;
  CheckEncoding(AEncoding);
  VBytes := AEncoding.GetBytes(AValue);
  WriteBytes(VBytes, System.Length(VBytes));
end;
{$IFDEF FPC}{$POP}{$ENDIF}

procedure TBrookString.Write(const AValue: string);
begin
  Write(AValue, TEncoding.UTF8);
end;

{$IFDEF FPC}{$PUSH}{$WARN 4105 OFF}{$ENDIF}
function TBrookString.Read(AEncoding: TEncoding): string;
var
  VBytes: TBytes;
  VLength: NativeUInt;
begin
  CheckEncoding(AEncoding);
  VLength := GetLength;
  if VLength = 0 then
    Exit('');
  SetLength(VBytes, VLength);
  ReadBytes(VBytes, VLength);
  Result := AEncoding.GetString(VBytes, 0, VLength);
end;
{$IFDEF FPC}{$POP}{$ENDIF}

function TBrookString.Read: string;
begin
  Result := Read(TEncoding.UTF8);
end;

procedure TBrookString.Clear;
begin
  CheckHandle;
  CheckOSError(bk_str_clear(Fstr));
end;

function TBrookString.GetLength: NativeUInt;
begin
  CheckHandle;
  CheckOSError(bk_str_length(Fstr, @Result));
end;

procedure TBrookString.SetText(const AValue: string);
begin
  Clear;
  Write(AValue);
end;

function TBrookString.GetContent: TBytes;
begin
  Result := TMarshal.ToBytes(bk_str_content(Fstr), GetLength);
end;

end.