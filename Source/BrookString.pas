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
    function GetLength: NativeInt;
    procedure SetText(const AValue: string);
    function GetText: string;
  protected
    class procedure CheckEncoding(AEncoding: TEncoding); static; inline;
    procedure SetHandle(AHandle: Pointer); override;
    function GetHandle: Pointer; override;
    function GetOwnsHandle: Boolean; override;
    procedure SetOwnsHandle(AValue: Boolean); override;
  public
    { Creates an instance of @link(TBrookString).

      @param(AHandle[in] String handle.) }
    constructor Create(AHandle: Pointer); virtual;
    { Frees an instance of @link(TBrookString). }
    destructor Destroy; override;
    { Copies a string buffer to the string handle. All strings previously
      copied are kept.

      @param(ASource[in] String buffer source to be copied.)
      @param(ALength[in] Length of the string buffer being copied.)

      @returns(Length of the written string buffer.) }
    function CopyBytes(const ASource: TBytes;
      ALength: NativeUInt): NativeUInt; virtual;
    { Copies a string to the string handle. All strings previously copied are
      kept.

      @param(ASource[in] String to be copied.)
      @param(AEncoding[in] Determines the encoding of the string being copied.) }
    procedure Copy(const ASource: string;
      AEncoding: TEncoding); overload; virtual;
    { Copies a string to the string handle. All strings previously copied are
      kept.

      @param(ASource[in] String to be copied.) }
    procedure Copy(const ASource: string); overload; virtual;
    { Cleans all the content present in the string handle. }
    procedure Clear; virtual;
    { Gets the content buffer from the string handle. }
    property Content: TBytes read GetContent;
    { Gets the content length from the string handle. }
    property Length: NativeInt read GetLength;
    { Gets or sets a string to the string handle. }
    property Text: string read GetText write SetText;
  end;

implementation

constructor TBrookString.Create(AHandle: Pointer);
begin
  inherited Create;
  FOwnsHandle := not Assigned(AHandle);
  if FOwnsHandle then
  begin
    BkCheckLibrary;
    Fstr := bk_str_new;
  end
  else
    Fstr := AHandle;
end;

destructor TBrookString.Destroy;
begin
  try
    if FOwnsHandle then
    begin
      BkCheckLibrary;
      bk_str_free(Fstr);
      Fstr := nil;
    end;
  finally
    inherited Destroy;
  end;
end;

class procedure TBrookString.CheckEncoding(AEncoding: TEncoding);
begin
  if not Assigned(AEncoding) then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AEncoding']);
end;

procedure TBrookString.SetHandle(AHandle: Pointer);
begin
  if AHandle = Fstr then
    Exit;
  Clear;
  Fstr := AHandle;
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

function TBrookString.CopyBytes(const ASource: TBytes;
  ALength: NativeUInt): NativeUInt;
begin
  BkCheckLibrary;
  Result := ALength;
  CheckOSError(bk_str_strcpy(Fstr, @ASource[0], Result));
end;

procedure TBrookString.Copy(const ASource: string; AEncoding: TEncoding);
var
  VBytes: TBytes;
begin
  CheckEncoding(AEncoding);
  VBytes := AEncoding.GetBytes(
{$IFDEF FPC}UnicodeString({$ENDIF}ASource{$IFDEF FPC}){$ENDIF});
  CopyBytes(VBytes, System.Length(VBytes));
end;

procedure TBrookString.Copy(const ASource: string);
begin
  Copy(ASource, TEncoding.UTF8);
end;

procedure TBrookString.Clear;
begin
  BkCheckLibrary;
  CheckOSError(bk_str_clear(Fstr));
end;

function TBrookString.GetLength: NativeInt;
begin
  BkCheckLibrary;
  Result := bk_str_length(Fstr);
  if Result < 0 then
    CheckOSError(Result);
end;

procedure TBrookString.SetText(const AValue: string);
begin
  Clear;
  Self.Copy(AValue);
end;

function TBrookString.GetText: string;
begin
  Result :=
{$IFDEF FPC}string({$ENDIF}TEncoding.UTF8.GetString(GetContent){$IFDEF FPC}){$ENDIF};
end;

function TBrookString.GetContent: TBytes;
begin
  BkCheckLibrary;
  Result := TMarshal.ToBytes(bk_str_content(Fstr), GetLength);
end;

end.
