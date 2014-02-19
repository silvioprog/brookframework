(*
  Brook framework, FCL HttpDefs Broker

  Copyright (C) 2014 Yuriy Pilgun

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookHttpDefsBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookConsts, BrookHttpConsts, BrookException, BrookUtils, HttpDefs, CustWeb,
  Classes, SysUtils, StrUtils;

function BrookFormatContentType: string;
function BrookHandleUnknownEncoding(ARequest: TRequest;
  const AContentType: string; AStream: TStream): Boolean;
procedure BrookShowRequestException(AHandler: TWebHandler;
  R: TResponse; E: Exception);

implementation

uses
  FPJSON, JSONParser;

function BrookFormatContentType: string;
begin
  if BrookSettings.Charset <> ES then
    Result := BrookSettings.ContentType + BROOK_HTTP_HEADER_CHARSET +
      BrookSettings.Charset
  else
    Result := BrookSettings.ContentType;
end;

function BrookHandleUnknownEncoding(ARequest: TRequest;
  const AContentType: string; AStream: TStream): Boolean;

  procedure ProcessJSONObject(AJSON: TJSONObject);
  var
    I: integer;
  begin
    for I := 0 to Pred(AJSON.Count) do
      ARequest.ContentFields.Add(AJSON.Names[I] + EQ + AJSON.Items[I].AsString);
  end;

  procedure ProcessJSONArray(AJSON: TJSONArray);
  var
    I: integer;
  begin
    for I := 0 to Pred(AJSON.Count) do
      if AJSON[I].JSONType = jtObject then
        ProcessJSONObject(AJSON.Objects[I])
      else
        raise Exception.CreateFmt('%s: Unsupported JSON format.',
          [ARequest.ClassName]);
  end;

var
  VJSON: TJSONData;
  VParser: TJSONParser;
begin
  Result := False;
  if Copy(AContentType, 1, Length(BROOK_HTTP_CONTENT_TYPE_APP_JSON)) =
    BROOK_HTTP_CONTENT_TYPE_APP_JSON then
  begin
    if BrookSettings.AcceptsJSONContent then
    begin
      AStream.Position := 0;
      VParser := TJSONParser.Create(AStream);
      try
        VJSON := VParser.Parse;
        case VJSON.JSONType of
          jtArray: ProcessJSONArray(TJSONArray(VJSON));
          jtObject: ProcessJSONObject(TJSONObject(VJSON));
        else
          raise Exception.CreateFmt('%s: Unsupported JSON format.',
            [ARequest.ClassName]);
        end;
      finally
        VJSON.Free;
        VParser.Free;
      end;
      Result := True;
    end;
  end;
end;

procedure BrookShowRequestException(AHandler: TWebHandler;
  R: TResponse; E: Exception);
var
  VHandled: Boolean = False;

  procedure HandleHttp404;
  begin
    if not R.HeadersSent then
    begin
      R.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
      R.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
      R.ContentType := BrookFormatContentType;
    end;
    if (BrookSettings.Page404File <> ES) and
      FileExists(BrookSettings.Page404File) then
      R.Contents.LoadFromFile(BrookSettings.Page404File)
    else
      R.Content := BrookSettings.Page404;
    R.Content := StringsReplace(R.Content, ['@root', '@path'],
      [BrookSettings.RootUrl, E.Message], [rfIgnoreCase, rfReplaceAll]);
    R.SendContent;
    VHandled := True;
  end;

  procedure HandleHttp500;
  var
    VExceptionMessage, VStackDumpString: TJSONStringType;
  begin
    if not R.HeadersSent then
    begin
      R.Code := BROOK_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
      R.CodeText := BROOK_HTTP_REASON_PHRASE_INTERNAL_SERVER_ERROR;
      R.ContentType := BrookFormatContentType;
    end;
    if (BrookSettings.Page500File <> ES) and
      FileExists(BrookSettings.Page500File) then
    begin
      R.Contents.LoadFromFile(BrookSettings.Page500File);
      R.Content := StringsReplace(R.Content, ['@error'], [E.Message],
        [rfIgnoreCase, rfReplaceAll]);
      if Pos('@trace', LowerCase(R.Content)) > 0 then
        R.Content := StringsReplace(R.Content, ['@trace'],
          [BrookDumpStack], [rfIgnoreCase, rfReplaceAll]);
      // DumpStack is slow and not thread safe
    end
    else
    begin
      R.Content := BrookSettings.Page500;
      VStackDumpString := '';
      if BrookSettings.ContentType = BROOK_HTTP_CONTENT_TYPE_APP_JSON then
      begin
        VExceptionMessage := StringToJSONString(E.Message);
        if Pos('@trace', LowerCase(R.Content)) > 0 then
          VStackDumpString := StringToJSONString(BrookDumpStack(LF));
      end
      else
      begin
        VExceptionMessage := E.Message;
        if Pos('@trace', LowerCase(R.Content)) > 0 then
          VStackDumpString := BrookDumpStack;
      end;
      R.Content := StringsReplace(BrookSettings.Page500, ['@error', '@trace'],
        [VExceptionMessage, VStackDumpString], [rfIgnoreCase, rfReplaceAll]);
    end;
    R.SendContent;
    VHandled := True;
  end;

begin
  if R.ContentSent then
    Exit;
  if Assigned(BrookSettings.OnError) then
  begin
    BrookSettings.OnError(R, E, VHandled);
    if VHandled then
      Exit;
  end;
  if Assigned(AHandler.OnShowRequestException) then
  begin
    AHandler.OnShowRequestException(R, E, VHandled);
    if VHandled then
      Exit;
  end;
  if AHandler.RedirectOnError and not R.HeadersSent then
  begin
    R.SendRedirect(Format(AHandler.RedirectOnErrorURL, [HttpEncode(E.Message)]));
    R.SendContent;
    Exit;
  end;
  if E is EBrookHttp404 then
    HandleHttp404
  else
    HandleHttp500;
end;

end.
