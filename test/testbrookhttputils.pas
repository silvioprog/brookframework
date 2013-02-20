unit testbrookhttputils;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLHTTPClientBroker, BrookHTTPUtils, BrookUtils, BrookHTTPClient,
  fpcunit, testregistry, fpjson,dialogs,sysutils;

type
  TTestBrookHttpUtils = class(TTestCase)
  published
    procedure TestDateTimeToGMT;
    procedure TestSameURL;
    procedure TestStatusCodeToReasonPhrase;
    procedure TestReasonPhraseToStatusCode;
    procedure TestGetAcceptEncodingSet;
    procedure TestGetAcceptEncoding;
    procedure TestMimeTypeFromFileExt;
    procedure TestTypeFromFileName;
    procedure TestFileExtFromMimeType;
    procedure TestExtractUrlFileName;
    procedure RequestMethodToStr;
    procedure StrToRequestMethod;
    procedure HttpRequest;
  end;

const
  TEST_DATETIME = 41233 + 0.6161111111;
  URL = 'http://brookframework.org/demos/db/rest/addressbook/backend/addressbook.fbf/contacts/1';

implementation

procedure TTestBrookHttpUtils.TestDateTimeToGMT;
begin
  AssertEquals('Tue, 20 Nov 2012 14:47:12 GMT',
    BrookDateTimeToGMT(TEST_DATETIME));
end;

procedure TTestBrookHttpUtils.TestSameURL;
begin
  AssertEquals(True, BrookSameURL('http://localhost', 'http://localhost/'));
end;

procedure TTestBrookHttpUtils.TestStatusCodeToReasonPhrase;
begin
  AssertEquals('OK', BrookStatusCodeToReasonPhrase(200));
end;

procedure TTestBrookHttpUtils.TestReasonPhraseToStatusCode;
begin
  AssertEquals(200, BrookReasonPhraseToStatusCode('OK'));
end;

procedure TTestBrookHttpUtils.TestGetAcceptEncodingSet;
begin
  AssertEquals('deflate,gzip', BrookGetAcceptEncoding([aeDeflate, aeGzip]));
end;

procedure TTestBrookHttpUtils.TestGetAcceptEncoding;
begin
  AssertEquals(True, BrookGetAcceptEncodingSet('deflate,gzip') =
    [aeDeflate, aeGzip]);
end;

procedure TTestBrookHttpUtils.TestMimeTypeFromFileExt;
begin
  AssertEquals('text/plain', BrookMimeTypeFromFileExt('.txt'));
end;

procedure TTestBrookHttpUtils.TestTypeFromFileName;
begin
  AssertEquals('text/plain', BrookMimeTypeFromFileName('/foo/file.txt'));
end;

procedure TTestBrookHttpUtils.TestFileExtFromMimeType;
begin
  AssertEquals('.txt', BrookFileExtFromMimeType('text/plain'));
end;

procedure TTestBrookHttpUtils.TestExtractUrlFileName;
begin
  AssertEquals('dummy.txt',
    BrookExtractUrlFileName('http://localhost/dummy.txt'));
  AssertEquals('dummy.txt',
    BrookExtractUrlFileName('http://localhost/dummy.txt?foo=bah', True));
end;

procedure TTestBrookHttpUtils.RequestMethodToStr;
begin
  AssertEquals('GET', BrookRequestMethodToStr(rmGet));
  AssertEquals('POST', BrookRequestMethodToStr(rmPost));
  AssertEquals('PUT', BrookRequestMethodToStr(rmPut));
  AssertEquals('DELETE', BrookRequestMethodToStr(rmDelete));
  AssertEquals('HEAD', BrookRequestMethodToStr(rmHead));
  AssertEquals('OPTIONS', BrookRequestMethodToStr(rmOptions));
  AssertEquals('TRACE', BrookRequestMethodToStr(rmTrace));
end;

procedure TTestBrookHttpUtils.StrToRequestMethod;
begin
  AssertEquals(True, BrookStrToRequestMethod('GET') = rmGet);
  AssertEquals(True, BrookStrToRequestMethod('POST') = rmPost);
  AssertEquals(True, BrookStrToRequestMethod('PUT') = rmPut);
  AssertEquals(True, BrookStrToRequestMethod('DELETE') = rmDelete);
  AssertEquals(True, BrookStrToRequestMethod('HEAD') = rmHead);
  AssertEquals(True, BrookStrToRequestMethod('OPTIONS') = rmOptions);
  AssertEquals(True, BrookStrToRequestMethod('TRACE') = rmTrace);
end;

procedure TTestBrookHttpUtils.HttpRequest;
var
  VContacts: TJSONArray;
  VResult: TBrookHTTPResult;
begin
  VContacts := TJSONArray.Create([TJSONObject.Create(['name', 'Foo'])]);
  try
    VResult := BrookHttpRequest(VContacts, URL, rmPut);
    AssertEquals(204, VResult.StatusCode);
    VResult := BrookHttpRequest(URL);
    AssertEquals('{ "id" : 1, "name" : "Foo" }', Trim(VResult.Content));
    AssertEquals('OK', VResult.ReasonPhrase);
    AssertEquals(200, VResult.StatusCode);
  finally
    VContacts.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookHttpUtils);

end.

