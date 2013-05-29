(*
  Brook Static File Broker unit.

  Copyright (C) 2013 Mario Ray Mahardhika.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookStaticFileBroker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ This is the only thing that user may know from this unit. }
procedure BrookRegisterDirectory(ARequestPath, ADirectory: string);

implementation

uses
  StrUtils, ghashmap, fpmimetypes, HTTPDefs, BrookAction;

resourcestring
  SEmptyRequestPathErrMsg = 'Request path may not be empty.';
  SDirectoryNotExistErrMsg = 'Directory not exists: %s.';

type

  { TStringHash }

  TStringHash = class
    class function Hash(S: string; N: Integer): Integer;
  end;

  TRequestDirectoryMap = specialize THashmap<string, string, TStringHash>;

  { TStaticFileAction }

  TStaticFileAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

var
  RequestDirectoryMap: TRequestDirectoryMap;

{ TStringHash }

class function TStringHash.Hash(S: String; N: Integer): Integer;
var
  C: Char;
begin
  Result := 0;
  for C in LowerCase(S) do
    Inc(Result, Ord(C));
  Result := Result mod N;
end;

{ TStaticFileAction }

procedure TStaticFileAction.Get;
var
  VLastSlashPos: Integer;
  VPathInfo, VFilePath, VBuffer, VContentType: string;
begin
  VPathInfo := GetRequest.PathInfo;
  VLastSlashPos := RPos('/', VPathInfo);
  System.Delete(VPathInfo, VLastSlashPos + 1, Length(VPathInfo) - VLastSlashPos);
  VFilePath := RequestDirectoryMap[VPathInfo] + Values['file'].AsString;
  if FileExists(VFilePath) then
  begin
    VContentType := MimeTypes.GetMimeType(ExtractFileExt(VFilePath));
    if VContentType = '' then
      VContentType := 'application/octet-stream';
    GetResponse.ContentType := VContentType;
    with TFileStream.Create(VFilePath, fmOpenRead) do
      try
        SetLength(VBuffer, Size);
        Read(VBuffer[1], Size);
        Self.Write(VBuffer);
      finally
        Free;
      end;
  end;
end;

procedure BrookRegisterDirectory(ARequestPath, ADirectory: string);
begin
  if Length(ARequestPath) = 0 then
    raise Exception.Create(SEmptyRequestPathErrMsg);
  if not DirectoryExists(ADirectory) then
    raise Exception.CreateFmt(SDirectoryNotExistErrMsg, [ADirectory]);
  // add required slashes
  if ARequestPath[1] <> '/' then
    ARequestPath := '/' + ARequestPath;
  if ARequestPath[Length(ARequestPath)] <> '/' then
    ARequestPath := ARequestPath + '/';
  RequestDirectoryMap[ARequestPath] := IncludeTrailingPathDelimiter(ADirectory);
  TStaticFileAction.Register(ARequestPath + ':file');
end;

initialization
  RequestDirectoryMap := TRequestDirectoryMap.Create;

finalization
  RequestDirectoryMap.Free;

end.
