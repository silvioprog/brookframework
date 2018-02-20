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

{ This example show a basic usage of TBrookString. }

unit BrookString_frMain;

{$MODE DELPHI}
{$WARN 4105 OFF}

interface

uses
  SysUtils,
  StdCtrls,
  Forms,
  Dialogs,
  BrookString;

type
  TfrMain = class(TForm)
    btAddNow: TButton;
    btShowContent: TButton;
    btClear: TButton;
    Label1: TLabel;
    procedure btAddNowClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btShowContentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FString: TBrookString;
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

procedure TfrMain.FormCreate(Sender: TObject);
begin
  FString := TBrookString.Create(nil);
end;

procedure TfrMain.FormDestroy(Sender: TObject);
begin
  FString.Free;
end;

procedure TfrMain.btAddNowClick(Sender: TObject);
var
  VBytes: TBytes;
begin
  VBytes := BytesOf(FormatDateTime(Concat('hh:nn:ss.zzz', sLineBreak), Now));
  FString.Write(VBytes, Length(VBytes));
end;

procedure TfrMain.btShowContentClick(Sender: TObject);
begin
  ShowMessage(Concat('All clicks:', sLineBreak, sLineBreak,
    StringOf(FString.Content)));
end;

procedure TfrMain.btClearClick(Sender: TObject);
begin
  FString.Clear;
end;

end.