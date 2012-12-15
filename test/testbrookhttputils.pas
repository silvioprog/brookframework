unit testbrookhttputils;

{$mode objfpc}{$H+}

interface

uses
  BrookHTTPUtils, fpcunit, testregistry;

type
  TTestBrookHttpUtils = class(TTestCase)
  published
    procedure TestDateTimeToGMT;
    procedure TestSameURL;
    procedure TestStatusCodeToReasonPhrase;
    procedure TestReasonPhraseToStatusCode;
    procedure TestGetAcceptEncodingSet;
    procedure TestGetAcceptEncoding;
  end;

const
  TEST_DATETIME = 41233 + 0.6161111111;

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

initialization
  RegisterTest(TTestBrookHttpUtils);

end.

