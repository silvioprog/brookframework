unit testbrooksession;

{$mode objfpc}{$H+}

interface

uses
  BrookSession, BrookConsts, BrookUtils, fpcunit, testregistry, HTTPDefs,
  sysutils, Classes;

type
  TTestBrookSession = class(TTestCase)
  published
    procedure TestIsExpired;
    procedure TestGenerateID;
    procedure TestExpire;
    procedure TestFields;
    procedure TestDeleteFiles;
    procedure TestDeleteOldFiles;
  end;

var
  TEST_COOKIE: string = '';
  TEST_SESS_PATH: string = '';

implementation

procedure TTestBrookSession.TestIsExpired;
var
  VReq: TRequest;
  VRes: TResponse;
  VCookie: string;
  VSess: TBrookSession;
begin
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  VSess := TBrookSession.Create;
  try
    VSess.Directory := TEST_SESS_PATH;
    VSess.Start(VReq);
    VSess.Finish(VRes);
    VCookie := VRes.Cookies.CookieByName(BROOK_SESS_ID).Value;
    if VCookie <> '' then
      VReq.CookieFields.Values[BROOK_SESS_ID] := VCookie;
    VSess.Start(VReq);
    BrookFileSetDate(VSess.FileName, EncodeDate(2000, 1, 1));
    AssertEquals(True, VSess.IsExpired);
    VSess.DeleteFiles;
  finally
    VRes.Free;
    VReq.Free;
    VSess.Free;
  end;
end;

procedure TTestBrookSession.TestGenerateID;
var
  VReq: TRequest;
  VRes: TResponse;
  VSess: TBrookSession;
begin
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  VSess := TBrookSession.Create;
  try
    VSess.Directory := TEST_SESS_PATH;
    VSess.Start(VReq);
    AssertEquals(32, Length(VSess.GenerateID));
    VSess.Finish(VRes);
  finally
    VRes.Free;
    VReq.Free;
    VSess.Free;
  end;
end;

procedure TTestBrookSession.TestExpire;
var
  VReq: TRequest;
  VRes: TResponse;
  VSess: TBrookSession;
begin
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  VSess := TBrookSession.Create;
  try
    VSess.Directory := TEST_SESS_PATH;
    if TEST_COOKIE <> '' then
      VReq.CookieFields.Values[BROOK_SESS_ID] := TEST_COOKIE;
    VSess.Start(VReq);
    if VSess.Fields.Count = 0 then
    begin
      VSess.Fields.Add('field1', 'abc');
      VSess.Fields.Add('field2', 123);
      VSess.Fields.Add('field3', 1.23);
      VSess.Fields.Add('field4', True);
    end;
    VSess.Finish(VRes);
    TEST_COOKIE := VRes.Cookies.CookieByName(BROOK_SESS_ID).Value;
    if TEST_COOKIE <> '' then
      VReq.CookieFields.Values[BROOK_SESS_ID] := TEST_COOKIE;
    VSess.Start(VReq);
    AssertEquals('abc', VSess.Fields['field1'].AsString);
    AssertEquals(123, VSess.Fields['field2'].AsInteger);
    AssertEquals(1.23, VSess.Fields['field3'].AsFloat);
    AssertEquals(True, VSess.Fields['field4'].AsBoolean);
    VSess.Expire(VReq, VRes);
    VSess.Finish(VRes);
    if VRes.Cookies.CookieByName(BROOK_SESS_ID).Expires =
      EncodeDate(1970, 1, 1) then
      VRes.Cookies.Clear;
    VSess.Start(VReq);
    AssertEquals('{}', VSess.Fields.AsJSON);
    VSess.Finish(VRes);
  finally
    VRes.Free;
    VReq.Free;
    VSess.Free;
  end;
end;

procedure TTestBrookSession.TestFields;
var
  VReq: TRequest;
  VRes: TResponse;
  VSess: TBrookSession;
begin
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  VSess := TBrookSession.Create;
  try
    VSess.Directory := TEST_SESS_PATH;
    if TEST_COOKIE <> '' then
      VReq.CookieFields.Values[BROOK_SESS_ID] := TEST_COOKIE;
    VSess.Start(VReq);
    if VSess.Fields.Count = 0 then
    begin
      VSess.Fields.Add('field1', 'abc');
      VSess.Fields.Add('field2', 123);
      VSess.Fields.Add('field3', 1.23);
      VSess.Fields.Add('field4', True);
    end;
    VSess.Finish(VRes);
    TEST_COOKIE := VRes.Cookies.CookieByName(BROOK_SESS_ID).Value;
    if TEST_COOKIE <> '' then
      VReq.CookieFields.Values[BROOK_SESS_ID] := TEST_COOKIE;
    VSess.Start(VReq);
    AssertEquals('abc', VSess.Fields['field1'].AsString);
    AssertEquals(123, VSess.Fields['field2'].AsInteger);
    AssertEquals(1.23, VSess.Fields['field3'].AsFloat);
    AssertEquals(True, VSess.Fields['field4'].AsBoolean);
    VSess.Finish(VRes);
  finally
    VRes.Free;
    VReq.Free;
    VSess.Free;
  end;
end;

procedure TTestBrookSession.TestDeleteFiles;
var
  I: Integer;
  VReq: TRequest;
  VRes: TResponse;
  VFileName: string;
  VFiles: TStringList;
  VSess: TBrookSession;
begin
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  VSess := TBrookSession.Create;
  VFiles := TStringList.Create;
  try
    VSess.Directory := TEST_SESS_PATH;
    for I := 1 to 10 do
    begin
      VFileName := TEST_SESS_PATH + BROOK_SESS_PREFIX + VSess.GenerateID;
      VFiles.Add(VFileName);
      VFiles.SaveToFile(VFileName);
    end;
    VSess.DeleteFiles;
    for I := 0 to Pred(VFiles.Count) do
      AssertEquals(False, FileExists(VFiles[I]));
  finally
    VFiles.Free;
    VRes.Free;
    VReq.Free;
    VSess.Free;
  end;
end;

procedure TTestBrookSession.TestDeleteOldFiles;
var
  I: Integer;
  VReq: TRequest;
  VRes: TResponse;
  VFileName: string;
  VFiles: TStringList;
  VSess: TBrookSession;
begin
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  VSess := TBrookSession.Create;
  VFiles := TStringList.Create;
  try
    VSess.Directory := TEST_SESS_PATH;
    for I := 1 to 10 do
    begin
      VFileName := TEST_SESS_PATH + BROOK_SESS_PREFIX + VSess.GenerateID;
      VFiles.Add(VFileName);
      VFiles.SaveToFile(VFileName);
      BrookFileSetDate(VFileName, Now - 1);
    end;
    VSess.DeleteOldFiles(Now - 1);
    for I := 0 to Pred(VFiles.Count) do
      AssertEquals(False, FileExists(VFiles[I]));
  finally
    VFiles.Free;
    VRes.Free;
    VReq.Free;
    VSess.Free;
  end;
end;

initialization
  TEST_SESS_PATH := ExtractFilePath(ParamStr(0));
  RegisterTest(TTestBrookSession);

finalization
  BrookDeleteFiles(TEST_SESS_PATH, NullDate, '', BROOK_SESS_PREFIX);

end.

