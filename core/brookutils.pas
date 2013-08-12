(*
  Brook Utils unit.

  Copyright (C) 2013 Silvio Clecio.

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

unit BrookUtils;

{$i brook.inc}

interface

uses
  BrookException, BrookMessages, BrookConsts, BrookHTTPConsts, HTTPDefs, FPJSON,
  CustWeb, SysUtils;

type
  { Defines a array of strings. }
  TBrookArrayOfString = array of string;

  { Defines an enumerator to represent the HTTP request methods. }
  TBrookRequestMethod = (
    rmUnknown, rmAll, rmGet, rmPost, rmPut, rmDelete, rmHead, rmOptions, rmTrace
  );

  { Stores the Brook configurations. }
  TBrookSettings = record
    { Enables the mapping of HTTP request methods. }
    Mapped: Boolean;
    { Set the default application Charset. }
    Charset: ShortString;
    { Set the default application Content-Type. }
    ContentType: ShortString;
    { Set the default application Language. }
    Language: ShortString;
    { Set the 404 HTML page. The string will be sent as is. }
    Page404: string;
    { Set the 404 HTML page file. The file content will be sent.
      This has higher precedence than @code(TBrookSettings.Page404)
      so when both are set, this will be processed first and only
      if the file is not found or cannot be read the system will
      fallback to @code(TBrookSettings.Page404) }
    Page404File: string;
    { Set the 500 HTML page. The string will be sent as is. }
    Page500: string;
    { Set the 500 HTML page file. The file content will be sent.
      This has higher precedence than @code(TBrookSettings.Page500)
      so when both are set, this will be processed first and only
      if the file is not found or cannot be read the system will
      fallback to @code(TBrookSettings.Page500) }
    Page500File: string;
    { Set the default directory for uploads. }
    DirectoryForUploads: string;
    { Defines if the temporary uploaded files will be deleted. }
    DeleteUploadedFiles: Boolean;
    { Keeps the original name of the uploaded files. }
    KeepUploadedNames: Boolean;
    { Defines if the application allows JSON formated requests. }
    AcceptsJSONContent: Boolean;
    { Set a configuration for the application or for its object members. }
    Configuration: string;
    { Set the default root URL. This is used by methods such as
      @code(TBrookAction.UrlFor), @code(TBrookActionHelper.LinkTo),
      @code(TBrookActionHelper.ButtonTo) etc. By default, Brook assumes
      @code(SCRIPT_NAME) as root URL. }
    RootUrl: string;
    { Set the default application port. }
    Port: Word;
    { Handles the application exceptions. }
    OnError: TOnShowRequestException;
  end;

var
  { Global variable to store Brook settings. }
  BrookSettings: TBrookSettings = (
    Mapped: False;
    Charset: BROOK_HTTP_CHARSET_UTF_8;
    ContentType: BROOK_HTTP_CONTENT_TYPE_TEXT_HTML;
    Language: BROOK_DEFAULT_LANGUAGE;
    Page404: BROOK_HTTP_RESPONSE_TEMPLATE_NOT_FOUND;
    Page404File: ES;
    Page500: BROOK_HTTP_RESPONSE_TEMPLATE_INTERNAL_SERVER_ERROR;
    Page500File: ES;
    DirectoryForUploads: ES;
    DeleteUploadedFiles: False;
    KeepUploadedNames: True;
    AcceptsJSONContent: False;
    Configuration: ES;
    RootUrl: ES;
    Port: 0;
    OnError: nil;
  );

{ Check whether a string starts with a given character. }
function BrookStartsChar(Ch: char; S: string): boolean;
{ Check whether a string ends with a given character. }
function BrookEndsChar(Ch: char; S: string): boolean;
{ Get the path level passing the respective index. Exemple:

  @code(BrookGetPathLavel('/a/b/c/', 1)) = b. }
function BrookGetPathLevel(const APath: string; const AIndex: SizeInt = 0;
  const ADelimiter: Char = US): string;
{ Get the path from the level correspondent to the index to the last level.
  Exemple:

  @code(BrookGetPathLevels('/a/b/c/', 1)) = b/c/. }
function BrookGetPathLevels(const APath: string; const AIndex: SizeInt = 0;
  const ADelimiter: Char = US): string;
{ Checks if a string is equivalent a enumerator representing a HTTP request
  method. }
function BrookMatchMethod(const ABrookMethod: TBrookRequestMethod;
  const AMethod: string): Boolean;
{ Deletes files according to their creation dates and file names. }
procedure BrookDeleteFiles(APath: string; const ABeforeOf: TDateTime;
  const ASkippedFile: TFileName = ES; const AContains: string = ES);
{ Get the datetime of a file. }
function BrookFileDate(const AFileName: TFileName): TDateTime;
{ Set the datetime of a file. }
function BrookFileSetDate(const AFileName: TFileName;
  const ADateTime: TDateTime): LongInt;
{ Copy the content of a JSON Object to another. }
procedure BrookJSONCopy(ASrc, ADest: TJSONObject);
{ Writes a backtrace of the current exception. }
function BrookDumpStack(const AEOL: ShortString = BR): string;
{ Ensures URL ends with delimiter. }
function BrookExcludeHTTPPathDelimiter(const AUrl: string): string;

implementation

function BrookStartsChar(Ch: char; S: string): boolean;
begin
  Result := (Length(S)>0) and (S[1]=Ch);
end;

function BrookEndsChar(Ch: char; S: string): boolean;
begin
  Result := (Length(S)>0) and (S[Length(S)]=Ch);
end;

function BrookGetPathLevel(const APath: string; const AIndex: SizeInt;
  const ADelimiter: Char): string;
var
  C, L: SizeInt;
  VSrc, VDest: PChar;
begin
  SetLength(Result, Length(APath));
  VSrc := PChar(APath);
  VDest := PChar(Result);
  C := Succ(AIndex);
  L := 0;
  while (VSrc^ <> NU) and (VSrc^ <> QU) do
  begin
    if (VSrc^ = ADelimiter) and (C <> 0) then
      Dec(C)
    else
      if C = 0 then
      begin
        if VSrc^ = ADelimiter then
          Break;
        VDest^ := VSrc^;
        Inc(VDest);
        Inc(L);
      end;
    Inc(VSrc);
  end;
  SetLength(Result, L);
end;

function BrookGetPathLevels(const APath: string; const AIndex: SizeInt;
  const ADelimiter: Char): string;
var
  C, L: Integer;
  VSrc, VDest: PChar;
begin
  SetLength(Result, Length(APath));
  VSrc := PChar(APath);
  VDest := PChar(Result);
  C := Succ(AIndex);
  L := 0;
  while (VSrc^ <> NU) and (VSrc^ <> QU) do
  begin
    if (VSrc^ = ADelimiter) and (C <> 0) then
      Dec(C)
    else
      if C = 0 then
      begin
        VDest^ := VSrc^;
        Inc(VDest);
        Inc(L);
      end;
    Inc(VSrc);
  end;
  SetLength(Result, L);
end;

function BrookMatchMethod(const ABrookMethod: TBrookRequestMethod;
  const AMethod: string): Boolean;
begin
  case ABrookMethod of
    rmAll: Result := True;
    rmGet: Result := AMethod = BROOK_HTTP_REQUEST_METHOD_GET;
    rmHead: Result := AMethod = BROOK_HTTP_REQUEST_METHOD_HEAD;
    rmOptions: Result := AMethod = BROOK_HTTP_REQUEST_METHOD_OPTIONS;
    rmPost: Result := AMethod = BROOK_HTTP_REQUEST_METHOD_POST;
    rmPut: Result := AMethod = BROOK_HTTP_REQUEST_METHOD_PUT;
    rmDelete: Result := AMethod = BROOK_HTTP_REQUEST_METHOD_DELETE;
  else
    Result := False;
  end;
end;

procedure BrookDeleteFiles(APath: string; const ABeforeOf: TDateTime;
  const ASkippedFile: TFileName; const AContains: string);

  function IsOldFile(const FN: TFileName; const DT: TDateTime): Boolean;
  begin
    if DT = NullDate then
      Result := True
    else
      Result := BrookFileDate(FN) < DT;
  end;

var
  VResult: Integer;
  VFileName: TFileName;
  VSearchRec: TSearchRec;
begin
  APath := IncludeTrailingPathDelimiter(APath);
  VResult := FindFirst(APath + AK, faArchive, VSearchRec);
  try
    if AContains = ES then
      while VResult = 0 do
      begin
        VFileName := APath + VSearchRec.Name;
        if (VFileName <> ASkippedFile) and (VSearchRec.Name <> '..') and
          (VSearchRec.Name <> DT) and IsOldFile(VFileName, ABeforeOf) then
          DeleteFile(VFileName);
        VResult := FindNext(VSearchRec);
      end
    else
      while VResult = 0 do
      begin
        VFileName := APath + VSearchRec.Name;
        if (VFileName <> ASkippedFile) and (VSearchRec.Name <> '..') and
          (Pos(AContains, VSearchRec.Name) <> 0) and (VSearchRec.Name <> DT) and
          IsOldFile(VFileName, ABeforeOf) then
          DeleteFile(VFileName);
        VResult := FindNext(VSearchRec);
      end;
  finally
    FindClose(VSearchRec);
  end;
end;

function BrookFileDate(const AFileName: TFileName): TDateTime;
begin
  if not FileExists(AFileName) then
    raise EBrook.CreateFmt('BrookFileDate',
      SBrookFileNotFoundError, [AFileName]);
  Result := FileDateToDateTime(FileAge(AFileName));
end;

function BrookFileSetDate(const AFileName: TFileName;
  const ADateTime: TDateTime): LongInt;
begin
  if not FileExists(AFileName) then
    raise EBrook.CreateFmt('BrookFileSetDate',
      SBrookFileNotFoundError, [AFileName]);
  Result := FileSetDate(AFileName, DateTimeToFileDate(ADateTime));
end;

procedure BrookJSONCopy(ASrc, ADest: TJSONObject);
var
  I: Integer;
begin
  if not Assigned(ASrc) then
    raise EBrook.CreateFmt('BrookJSONCopy', SBrookNilParamError, ['ASrc']);
  if not Assigned(ADest) then
    raise EBrook.CreateFmt('BrookJSONCopy', SBrookNilParamError, ['ADest']);
  for I := 0 to Pred(ASrc.Count) do
    ADest.Add(ASrc.Names[I], ASrc.Items[I].Clone);
end;

function BrookDumpStack(const AEOL: ShortString): string;
var
  I: Integer;
begin
  Result := BackTraceStrFunc(ExceptAddr) + AEOL;
  for I := 0 to Pred(ExceptFrameCount) do
    Result += BackTraceStrFunc(ExceptFrames[I]) + AEOL;
end;

function BrookExcludeHTTPPathDelimiter(const AUrl: string): string;
var
  L: SizeInt;
begin
  Result := AUrl;
  L := Length(Result);
  if (L > 0) and (Result[L] = US) then
    Delete(Result, L, 1);
end;

end.
