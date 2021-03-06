(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 *  –– microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook framework.
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

program stringbuffer;

{$MODE DELPHI}
{$WARN 5024 OFF}

uses
  SysUtils,
  Classes,
  BrookLibraryLoader,
  BrookString;

const
  fn = 'test.txt';
var
  sb: TBrookString;
  f: TBytesStream;
begin
  if not TBrookLibraryLoader.Load(TBrookLibraryLoader.LIB_NAME) then
  begin
    WriteLn(ErrOutput, 'Library not loaded.');
    Halt(1);
  end;
  sb := TBrookString.Create(nil);
  try
    sb.Write('abc');
    sb.WriteBytes(TBytes.Create(49, 50, 51), 3);
    WriteLn('Text: ', sb.Text);
    f := TBytesStream.Create(sb.Content);
    try
      f.SaveToFile(fn);
      WriteLn('File saved: ', fn);
    finally
      f.Free;
    end;
{$IFDEF MSWINDOWS}
    ReadLn;
{$ENDIF}
  finally
    sb.Free;
  end;
end.
