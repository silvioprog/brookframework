unit testbrookrestactions;

{$mode objfpc}{$H+}

interface

uses
  BrookRESTActions, BrookSQLdbBroker, BrookDataBase, BrookUtils, fpcunit,
  testregistry, fpjson, sqlite3conn, HTTPDefs, sysutils;

type
  TPersonOptions = class(TBrookOptionsAction)
  end;

  TPersonRetrieve = class(TBrookRetrieveAction)
  end;

  TPersonShow = class(TBrookShowAction)
  end;

  TPersonCreate = class(TBrookCreateAction)
  end;

  TPersonUpdate = class(TBrookUpdateAction)
  end;

  TPersonDestroy = class(TBrookDestroyAction)
  end;

  { TTestRESTAction }

  TTestRESTAction = class(TTestCase)
  published
    procedure TestOptions;
    procedure TestRetrieve;
    procedure TestShow;
    procedure TestCreate;
    procedure TestUpdate;
    procedure TestDestroy;
  end;

implementation

{ TTestRESTAction }

procedure TTestRESTAction.TestOptions;
var
  VReq: TRequest;
  VRes: TResponse;
  VAct: TPersonOptions;
begin
  BrookSettings.Configuration :=
    'library=sqldb;database=testdb2.sqlite3;driver=sqlite3';
  TBrookDataBases.Service.FreeCurrent;
  VAct := TPersonOptions.Create;
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    VReq.Method := 'OPTIONS';
    VReq.PathInfo := '/person';
    VAct.DoRequest(VReq, VRes);
    AssertEquals(200, VRes.Code);
  finally
    VRes.Free;
    VReq.Free;
    VAct.Free;
  end;
end;

procedure TTestRESTAction.TestRetrieve;
var
  VReq: TRequest;
  VRes: TResponse;
  VAct: TPersonRetrieve;
begin
  VAct := TPersonRetrieve.Create;
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    VReq.Method := 'GET';
    VReq.PathInfo := '/person';
    VAct.DoRequest(VReq, VRes);
    AssertEquals(200, VRes.Code);
  finally
    VRes.Free;
    VReq.Free;
    VAct.Free;
  end;
end;

procedure TTestRESTAction.TestShow;
var
  VReq: TRequest;
  VRes: TResponse;
  VAct: TPersonShow;
  VJSON: TJSONObject;
begin
  VAct := TPersonShow.Create;
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    if not VAct.Table.Open.Locate('id', 1) then
    begin
      VJSON := TJSONObject.Create(['id', 1, 'name', 'abc']);
      try
        VAct.Table.Insert(VJSON).Apply;
      finally
        VJSON.Free;
      end;
    end;
    VReq.Method := 'GET';
    VReq.PathInfo := '/person/1';
    VAct.Values.Add('id', 1);
    VAct.DoRequest(VReq, VRes);
    AssertEquals(200, VRes.Code);
  finally
    VRes.Free;
    VReq.Free;
    VAct.Free;
  end;
end;

procedure TTestRESTAction.TestCreate;
var
  VGuid: TGuid;
  VReq: TRequest;
  VRes: TResponse;
  VAct: TPersonCreate;
begin
  VAct := TPersonCreate.Create;
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    VReq.Method := 'POST';
    VReq.PathInfo := '/person';
    CreateGUID(VGuid);
    VAct.Fields.Add('name', Copy(GUIDToString(VGuid), 2, 30));
    VAct.DoRequest(VReq, VRes);
    AssertEquals(201, VRes.Code);
  finally
    VRes.Free;
    VReq.Free;
    VAct.Free;
  end;
end;

procedure TTestRESTAction.TestUpdate;
var
  VGuid: TGuid;
  VReq: TRequest;
  VRes: TResponse;
  VAct: TPersonUpdate;
  VJSON: TJSONObject;
begin
  VAct := TPersonUpdate.Create;
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    if not VAct.Table.Open.Locate('id', 1) then
    begin
      VJSON := TJSONObject.Create(['id', 1, 'name', 'abc']);
      try
        VAct.Table.Insert(VJSON).Apply;
      finally
        VJSON.Free;
      end;
    end;
    VReq.Method := 'PUT';
    VReq.PathInfo := '/person/1';
    CreateGUID(VGuid);
    VAct.Values.Add('id', 1);
    VAct.Fields.Add('id', 1);
    VAct.Fields.Add('name', Copy(GUIDToString(VGuid), 2, 30));
    VAct.DoRequest(VReq, VRes);
    AssertEquals(204, VRes.Code);
  finally
    VRes.Free;
    VReq.Free;
    VAct.Free;
  end;
end;

procedure TTestRESTAction.TestDestroy;
var
  VReq: TRequest;
  VRes: TResponse;
  VAct: TPersonDestroy;
  VJSON: TJSONObject;
begin
  VAct := TPersonDestroy.Create;
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  try
    if not VAct.Table.Open.Locate('id', 1) then
    begin
      VJSON := TJSONObject.Create(['id', 1, 'name', 'abc']);
      try
        VAct.Table.Insert(VJSON).Apply;
      finally
        VJSON.Free;
      end;
    end;
    VReq.Method := 'DELETE';
    VReq.PathInfo := '/person/1';
    VAct.Values.Add('id', 1);
    VAct.DoRequest(VReq, VRes);
    AssertEquals(204, VRes.Code);
  finally
    VRes.Free;
    VReq.Free;
    VAct.Free;
  end;
end;

initialization
  RegisterTest(TTestRESTAction);
  TPersonOptions.Register('person', '/person', rmOptions);
  TPersonRetrieve.Register('person', '/person', rmGet);
  TPersonShow.Register('person', '/person/:id', rmGet);
  TPersonCreate.Register('person', '/person', rmPost);
  TPersonUpdate.Register('person', '/person/:id', rmPut);
  TPersonDestroy.Register('person', '/person/:id', rmDelete);

end.

