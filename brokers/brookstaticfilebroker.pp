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

// This is the only thing that user may know from this unit
procedure RegisterDirectory(ARequestPath,ADirectory: String);

implementation

uses
  StrUtils, ghashmap, fpmimetypes, HTTPDefs, BrookAction;

resourcestring
  EmptyRequestPathErrMsg = 'Request path may not be empty';
  RequestPathAlreadyRegisteredErrMsg = 'Request path "%s" already registered';
  DirectoryNotExistErrMsg = 'Directory not exists: "%s"';

type

  { TStringHash }

  TStringHash = class
    class function hash(s: String; n: Integer): Integer;
  end;

  TRequestDirectoryMap = specialize THashmap<String,String,TStringHash>;

  { TStaticFileAction }

  TStaticFileAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

var
  RequestDirectoryMap: TRequestDirectoryMap;

{ TStringHash }

class function TStringHash.hash(s: String; n: Integer): Integer;
var
  c: Char;
begin
  Result := 0;
  for c in LowerCase(s) do
    Inc(Result,Ord(c));
  Result := Result mod n;
end;

{ TStaticFileAction }

procedure TStaticFileAction.Get;
var
  LastSlashPos: Integer;
  PathInfo,FilePath,Buffer: String;
  ContentType: String;
begin
  PathInfo := GetRequest.PathInfo;
  LastSlashPos := RPos('/',PathInfo);
  System.Delete(PathInfo,LastSlashPos + 1,Length(PathInfo) - LastSlashPos);

  FilePath := RequestDirectoryMap[PathInfo] + Values['file'].AsString;
  if FileExists(FilePath) then begin
    ContentType := MimeTypes.GetMimeType(ExtractFileExt(FilePath));
    if ContentType = '' then
      ContentType := 'application/octet-stream';
    GetResponse.ContentType := ContentType;
    with TFileStream.Create(FilePath,fmOpenRead) do
      try
        SetLength(Buffer,Size);
        Read(Buffer[1],Size);
        Self.Write(Buffer);
      finally
        Free;
      end;
  end;
end;

procedure RegisterDirectory(ARequestPath,ADirectory: String);
begin
  if Length(ARequestPath) = 0 then raise Exception.Create(EmptyRequestPathErrMsg);
  if not DirectoryExists(ADirectory) then raise Exception.CreateFmt(DirectoryNotExistErrMsg,[ADirectory]);

  // add required slashes
  if ARequestPath[1] <> '/' then ARequestPath := '/' + ARequestPath;
  if ARequestPath[Length(ARequestPath)] <> '/' then ARequestPath := ARequestPath + '/';

  if RequestDirectoryMap.Contains(ARequestPath) then raise Exception.CreateFmt(RequestPathAlreadyRegisteredErrMsg,[ARequestPath]);
  RequestDirectoryMap[ARequestPath] := IncludeTrailingPathDelimiter(ADirectory);
  TStaticFileAction.Register(ARequestPath + ':file');
end;

initialization
  RequestDirectoryMap := TRequestDirectoryMap.Create;

finalization
  RequestDirectoryMap.Free;

end.
