unit testbrookrouter;

{$mode objfpc}{$H+}

interface

uses
  BrookRouter, BrookAction, BrookUtils, BrookHTTPConsts, fpcunit, testregistry,
  HTTPDefs, fpjson, sysutils;

type
  TAction1 = class(TBrookAction)
  end;

  TAction2 = class(TAction1)
  end;

  TAction3 = class(TAction1)
  end;

  TAction4 = class(TAction1)
  end;

  TRequest1 = class(TRequest)
  private
    FQueryString: string;
  public
    function GetFieldValue(AIndex: Integer): string; override;
    procedure SetFieldValue(AIndex: Integer; AValue: string); override;
  end;

  TRouter = class(TBrookRouter)
  private
    class var ActionClassName: string;
  protected
    class function DoCreateAction(out AActionClass: TBrookActionClass
       ): TBrookAction; override;
  end;

  TTestBrookRoutes = class(TTestCase)
  private
    procedure ClearRoutes(R: TBrookRoutes);
  published
    procedure TestAdd;
    procedure TestGetDefaultActionClass;
    procedure TestPatternByActionClass;
    procedure TestActionClassByPattern;
    procedure TestActionClassByClassName;
  end;

  TTestBrookRouter = class(TTestCase)
  private
    procedure ClearRoutes(R: TBrookRoutes);
  published
    procedure TestUrlFor;
    procedure TestCanonicalize;
    procedure TestMatchPattern;
    procedure TestRoute;
  end;

implementation

{ TRequest1 }

function TRequest1.GetFieldValue(AIndex: Integer): string;
begin
  case AIndex of
    33{QUERY_STRING}: Result := FQueryString;
  else
    Result := inherited GetFieldValue(AIndex);
  end;
end;

procedure TRequest1.SetFieldValue(AIndex: Integer; AValue: string);
begin
  case AIndex of
    33{QUERY_STRING}: FQueryString := AValue;
  else
    inherited SetFieldValue(AIndex, AValue);
  end;
end;

{ TRouter }

class function TRouter.DoCreateAction(out AActionClass: TBrookActionClass
  ): TBrookAction;
begin
  Result := inherited DoCreateAction(AActionClass);
  ActionClassName := AActionClass.ClassName;
end;

{ TTestBrookRoutes }

procedure TTestBrookRoutes.ClearRoutes(R: TBrookRoutes);
var
  P: PBrookRoute;
begin
  for P in R.List do
    Dispose(P);
  R.List.Clear;
end;

procedure TTestBrookRoutes.TestAdd;
var
  VRoutes: TBrookRoutes;
begin
  VRoutes := TBrookRouter.Service.Routes;
  ClearRoutes(VRoutes);
  VRoutes.Add(TAction1, '/action1', rmGet, True);
  VRoutes.Add(TAction2, '/action2', rmGet, False);
  AssertEquals(2, VRoutes.Count);
end;

procedure TTestBrookRoutes.TestGetDefaultActionClass;
var
  VIndex: Integer = -1;
  VRoutes: TBrookRoutes;
  VActClass: TBrookActionClass = nil;
begin
  VRoutes := TBrookRouter.Service.Routes;
  ClearRoutes(VRoutes);
  VRoutes.Add(TAction1, '/action1', rmGet, False);
  VRoutes.Add(TAction2, '/action2', rmGet, True);
  VRoutes.GetDefaultActionClass(VActClass, VIndex);
  AssertEquals('TAction2 - 1', VActClass.ClassName + ' - ' + IntToStr(VIndex));
end;

procedure TTestBrookRoutes.TestPatternByActionClass;
var
  VRoutes: TBrookRoutes;
begin
  VRoutes := TBrookRouter.Service.Routes;
  ClearRoutes(VRoutes);
  VRoutes.Add(TAction1, '/action1', rmGet, True);
  VRoutes.Add(TAction2, '/action2', rmGet, False);
  AssertEquals('/action2', VRoutes.PatternByActionClass(TAction2));
end;

procedure TTestBrookRoutes.TestActionClassByPattern;
var
  VRoutes: TBrookRoutes;
begin
  VRoutes := TBrookRouter.Service.Routes;
  ClearRoutes(VRoutes);
  VRoutes.Add(TAction1, '/action1', rmGet, True);
  VRoutes.Add(TAction2, '/action2', rmGet, False);
  AssertEquals(TAction2, VRoutes.ActionClassByPattern('/action2'));
end;

procedure TTestBrookRoutes.TestActionClassByClassName;
var
  VRoutes: TBrookRoutes;
begin
  VRoutes := TBrookRouter.Service.Routes;
  ClearRoutes(VRoutes);
  VRoutes.Add(TAction1, '/action1', rmGet, True);
  VRoutes.Add(TAction2, '/action2', rmGet, False);
  AssertEquals(TAction2, VRoutes.ActionClassByClassName('TAction2'));
end;

{ TTestBrookRouter }

procedure TTestBrookRouter.ClearRoutes(R: TBrookRoutes);
var
  P: PBrookRoute;
begin
  for P in R.List do
    Dispose(P);
  R.List.Clear;
end;

procedure TTestBrookRouter.TestUrlFor;
var
  VArray: TJSONArray;
  VObject: TJSONObject;
  VRouter: TBrookRouter;
begin
  VRouter := TBrookRouter.Service;
  ClearRoutes(VRouter.Routes);
  TAction1.Register('/action1');
  TAction2.Register('/action2/:myvar1/:myvar2');
  AssertEquals('/action1', VRouter.UrlFor(TAction1, []));
  AssertEquals('/action1', VRouter.UrlFor('TAction1', []));
  AssertEquals('/action2/abc/123', VRouter.UrlFor(TAction2, ['abc', '123']));
  AssertEquals('/action2/abc/123', VRouter.UrlFor('TAction2', ['abc', '123']));
  VArray := TJSONArray.Create(['abc', 123]);
  try
    AssertEquals('/action2/abc/123', VRouter.UrlFor(TAction2, VArray));
    AssertEquals('/action2/abc/123', VRouter.UrlFor('TAction2', VArray));
  finally
    VArray.Free;
  end;
  VObject := TJSONObject.Create(['item1', 'abc', 'item2', 123]);
  try
    AssertEquals('/action2/abc/123', VRouter.UrlFor(TAction2, VObject));
    AssertEquals('/action2/abc/123', VRouter.UrlFor('TAction2', VObject));
  finally
    VObject.Free;
  end;
end;

procedure TTestBrookRouter.TestCanonicalize;
var
  VReq: TRequest1;
  VRes: TResponse;
  VRouter: TBrookRouter;
begin
  VRouter := TBrookRouter.Service;
  VReq := TRequest1.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    VReq.PathInfo := '/action';
    VRouter.Canonicalize(VReq, VRes);
    AssertEquals(BROOK_HTTP_STATUS_CODE_TEMPORARY_REDIRECT, VRes.Code);
    AssertEquals(BROOK_HTTP_REASON_PHRASE_TEMPORARY_REDIRECT, VRes.CodeText);
    AssertEquals('/action/', VRes.Location);
    VReq.PathInfo := '/action/';
    VRouter.Canonicalize(VReq, VRes);
    AssertEquals(BROOK_HTTP_STATUS_CODE_TEMPORARY_REDIRECT, VRes.Code);
    AssertEquals(BROOK_HTTP_REASON_PHRASE_TEMPORARY_REDIRECT, VRes.CodeText);
    AssertEquals('/action/', VRes.Location);
    VReq.PathInfo := '/action';
    VReq.QueryString := 'q=abc123';
    VRouter.Canonicalize(VReq, VRes);
    AssertEquals(BROOK_HTTP_STATUS_CODE_TEMPORARY_REDIRECT, VRes.Code);
    AssertEquals(BROOK_HTTP_REASON_PHRASE_TEMPORARY_REDIRECT, VRes.CodeText);
    AssertEquals('/action/?q=abc123', VRes.Location);
    VReq.PathInfo := '/action/';
    VReq.QueryString := 'q=abc123';
    VRouter.Canonicalize(VReq, VRes);
    AssertEquals(BROOK_HTTP_STATUS_CODE_TEMPORARY_REDIRECT, VRes.Code);
    AssertEquals(BROOK_HTTP_REASON_PHRASE_TEMPORARY_REDIRECT, VRes.CodeText);
    AssertEquals('/action/?q=abc123', VRes.Location);
  finally
    VReq.Free;
    VRes.Free;
  end;
end;

procedure TTestBrookRouter.TestMatchPattern;
var
  VRouter: TBrookRouter;
  VMatch, VRedirect: Boolean;
  VNames, VValues: TBrookArrayOfString;
begin
  VRouter := TBrookRouter.Service;
  AssertEquals(False, VRouter.MatchPattern('/action', '/action/', VRedirect,
    VNames, VValues));
  VMatch := VRouter.MatchPattern('/action', '/action', VRedirect,
    VNames, VValues);
  AssertEquals(True, VMatch);
  AssertEquals(False, VRedirect);
  AssertEquals(-1, High(VNames));
  AssertEquals(-1, High(VValues));
  VMatch := VRouter.MatchPattern('/action/', '/action', VRedirect,
    VNames, VValues);
  AssertEquals(True, VMatch);
  AssertEquals(True, VRedirect);
  VMatch := VRouter.MatchPattern('/action/:myvar1/:myvar2', '/action/abc/123',
    VRedirect, VNames, VValues);
  AssertEquals('myvar1', VNames[0]);
  AssertEquals('myvar2', VNames[1]);
  AssertEquals('abc', VValues[0]);
  AssertEquals('123', VValues[1]);
  VMatch := VRouter.MatchPattern('/action/:myvar1/:myvar2/',
    '/action/abc/123/?q=abc123', VRedirect, VNames, VValues);
  AssertEquals('myvar1', VNames[0]);
  AssertEquals('myvar2', VNames[1]);
  AssertEquals('abc', VValues[0]);
  AssertEquals('123', VValues[1]);
end;

procedure TTestBrookRouter.TestRoute;
var
  VReq: TRequest1;
  VRes: TResponse;
  VRouter: TRouter;
begin
  VReq := TRequest1.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    BrookSettings.Mapped := False;
    ClearRoutes(TBrookRouter.Service.Routes);
    TBrookRouter.UnregisterService;
    TRouter.RegisterService;
    VRouter := TRouter.Service as TRouter;
    TAction1.Register('/action1');
    TAction2.Register('/action2');
    TAction3.Register('/action3/*/foo');
    TAction4.Register('/action4/**/foo');
    VReq.PathInfo := '/action2';
    VRouter.Route(VReq, VRes);
    AssertEquals('TAction2', VRouter.ActionClassName);
    VReq.PathInfo := '/action3/name/foo';
    VRouter.Route(VReq, VRes);
    AssertEquals('TAction3', VRouter.ActionClassName);
    VReq.PathInfo := '/action4/name';
    VRouter.Route(VReq, VRes);
    AssertEquals('TAction4', VRouter.ActionClassName);
  finally
    VReq.Free;
    VRes.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookRoutes);
  RegisterTest(TTestBrookRouter);

end.

