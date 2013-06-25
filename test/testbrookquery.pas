unit testbrookquery;

{$mode objfpc}{$H+}

interface

uses
  BrookQuery, BrookDataBase, BrookDBConsts, sqlite3conn, sqldb, db, fpjson,
  fpcunit, testregistry, sysutils, Classes;

type

  { TQueryBroker }

  TQueryBroker = class(TBrookQuery)
  private
    FQuery: TSQLQuery;
  protected
    procedure SetDataBase({%H-}AValue: TBrookDataBase); override;
    procedure SetDataSource({%H-}AValue: TDataSource); override;
    function GetDataSource: TDataSource; override;
    function GetFields: TFields; override;
    function GetSQL: TStrings; override;
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
    function GetDataBase: TBrookDataBase; override;
  public
    constructor Init({%H-}ADataBase: TBrookDataBase); override;
    function FieldDef({%H-}const AName: string): TFieldDef; override;
    function Execute: TBrookQuery; override;
    function Apply({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
    function ApplyUpdates: TBrookQuery; override;
    function Commit({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
    function Undo({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
    function CancelUpdates: TBrookQuery; override;
    function Rollback({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
    function Field(const AName: string): TField; override;
    function Param(const AName: string): TParam; override;
    function RowsAffected: TRowsCount; override;
  end;

  TTestBrookQuery = class(TTestCase)
  published
    procedure TestGetJSONType;
    procedure TestJSONToFields;
    procedure TestFieldsToJSON;
    procedure TestDataSetToJSON;
    procedure TestJSONToParams;
    procedure TestFieldDefsToSchema1;
    procedure TestFieldDefsToSchema2;
    procedure TestGetJSONAttributes;
    procedure TestLocate;
    procedure TestGetRows;
    procedure TestGetRow;
    procedure TestSetRows;
    procedure TestSetRow;
    procedure TestBind;
    procedure TestAppend;
    procedure TestInsert;
    procedure TestEdit;
    procedure TestDelete;
  end;

implementation

const
  TEST_DATETIME = 41233 + 0.6161111111;
  TEST_SQL_SELECT =
    'select field1, field2, field3, field4, field5 from testtable';
  TEST_SQL_PARAM =
    'select field1, field2, field3, field4, field5 from testtable ' +
    'where field1 = :field1 and field2 = :field2 and field3 = :field3 and ' +
    'field4 = :field4 and field5 = :field5';

var
  TestQuery: TSQLQuery;
  TestDatabase: TSQLite3Connection;

procedure InitQuery;
begin
  TestQuery := TSQLQuery.Create(nil);
  TestQuery.DataBase := TestDatabase;
  TestQuery.Transaction := TestDatabase.Transaction;
end;

procedure InitDatabase;
begin
  TestDatabase := TSQLite3Connection.Create(nil);
  TestDatabase.Transaction := TSQLTransaction.Create(TestDatabase);
  TestDatabase.Transaction.DataBase := TestDatabase;
  TestDatabase.DatabaseName := 'testdb.sqlite3';
end;

procedure DoneQuery;
begin
  FreeAndNil(TestQuery);
end;

procedure DoneDatabase;
begin
  FreeAndNil(TestDatabase);
end;

procedure SetSQL(const S: string);
begin
  TestQuery.Close;
  TestQuery.SQL.Text := S;
end;

procedure InitJSONObject(out J: TJSONObject);
begin
  J := TJSONObject.Create(['field1', 123, 'field2', 'abc', 'field3', True,
    'field4', TEST_DATETIME, 'field5', 1.23]);
end;

procedure InitFields(F: TFields);
begin
  F.FieldByName('field1').AsInteger := 123;
  F.FieldByName('field2').AsString := 'abc';
  F.FieldByName('field3').AsBoolean := True;
  F.FieldByName('field4').AsDateTime := TEST_DATETIME;
  F.FieldByName('field5').AsFloat := 1.23;
end;

{ TQueryBroker }

constructor TQueryBroker.Init(ADataBase: TBrookDataBase);
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := TestDataBase;
  FQuery.Transaction := TestDatabase.Transaction;
end;

function TQueryBroker.FieldDef(const AName: string): TFieldDef;
begin
  Result := nil;
end;

function TQueryBroker.Execute: TBrookQuery;
begin
  Result := Self;
end;

function TQueryBroker.Apply(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
end;

function TQueryBroker.ApplyUpdates: TBrookQuery;
begin
  Result := Self;
end;

function TQueryBroker.Commit(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
end;

function TQueryBroker.Undo(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
end;

function TQueryBroker.CancelUpdates: TBrookQuery;
begin
  Result := Self;
end;

function TQueryBroker.Rollback(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
end;

function TQueryBroker.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AName);
end;

function TQueryBroker.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AName);
end;

function TQueryBroker.RowsAffected: TRowsCount;
begin
  Result := -1;
end;

procedure TQueryBroker.SetDataBase(AValue: TBrookDataBase);
begin
end;

procedure TQueryBroker.SetDataSource(AValue: TDataSource);
begin
end;

function TQueryBroker.GetDataSource: TDataSource;
begin
  Result := nil;
end;

function TQueryBroker.GetFields: TFields;
begin
  Result := nil;
end;

function TQueryBroker.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TQueryBroker.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TQueryBroker.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TQueryBroker.GetDataBase: TBrookDataBase;
begin
  Result := nil;
end;

{ TTestBrookQuery }

procedure TTestBrookQuery.TestGetJSONType;
var
  VField: TField;
begin
  VField := TField.Create(nil);
  AssertEquals(BROOK_FT_NULL, TBrookQuery.GetJSONType(VField));
  FreeAndNil(VField);
  VField := TStringField.Create(nil);
  AssertEquals(BROOK_FT_STRING, TBrookQuery.GetJSONType(VField));
  FreeAndNil(VField);
  VField := TBooleanField.Create(nil);
  AssertEquals(BROOK_FT_BOOLEAN, TBrookQuery.GetJSONType(VField));
  FreeAndNil(VField);
  VField := TDateTimeField.Create(nil);
  AssertEquals(BROOK_FT_DATE, TBrookQuery.GetJSONType(VField));
  FreeAndNil(VField);
  VField := TFloatField.Create(nil);
  AssertEquals(BROOK_FT_FLOAT, TBrookQuery.GetJSONType(VField));
  FreeAndNil(VField);
  VField := TLongintField.Create(nil);
  AssertEquals(BROOK_FT_INT, TBrookQuery.GetJSONType(VField));
  FreeAndNil(VField);
end;

procedure TTestBrookQuery.TestJSONToFields;
var
  VJSON: TJSONObject;
begin
  try
    InitJSONObject(VJSON);
    SetSQL(TEST_SQL_SELECT);
    TestQuery.Open;
    TestQuery.Edit;
    TBrookQuery.JSONToFields(VJSON, TestQuery.Fields, False);
    AssertEquals(123, TestQuery.FieldByName('field1').AsInteger);
    AssertEquals('abc', TestQuery.FieldByName('field2').AsString);
    AssertEquals(True, TestQuery.FieldByName('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, TestQuery.FieldByName('field4').AsDateTime);
    AssertEquals(1.23, TestQuery.FieldByName('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
  end;
end;

procedure TTestBrookQuery.TestFieldsToJSON;
var
  VJSON: TJSONObject;
begin
  VJSON := TJSONObject.Create;
  try
    SetSQL(TEST_SQL_SELECT);
    TestQuery.Open;
    TestQuery.Edit;
    InitFields(TestQuery.Fields);
    TBrookQuery.FieldsToJSON(TestQuery.Fields, VJSON, False);
    AssertEquals(123, VJSON['field1'].AsInteger);
    AssertEquals('abc', VJSON['field2'].AsString);
    AssertEquals(True, VJSON['field3'].AsBoolean);
    AssertEquals(TEST_DATETIME, VJSON['field4'].AsFloat);
    AssertEquals(1.23, VJSON['field5'].AsFloat);
  finally
    VJSON.Free;
  end;
end;

procedure TTestBrookQuery.TestDataSetToJSON;
var
  VJSON: TJSONArray;
begin
  VJSON := TJSONArray.Create;
  try
    SetSQL(TEST_SQL_SELECT);
    TestQuery.Open;
    TestQuery.Insert;
    InitFields(TestQuery.Fields);
    TestQuery.Insert;
    InitFields(TestQuery.Fields);
    TBrookQuery.DataSetToJSON(TestQuery, VJSON, False);
    AssertEquals(2, VJSON.Count);
    AssertEquals(123, VJSON.Objects[0]['field1'].AsInteger);
    AssertEquals('abc', VJSON.Objects[0]['field2'].AsString);
    AssertEquals(True, VJSON.Objects[0]['field3'].AsBoolean);
    AssertEquals(TEST_DATETIME, VJSON.Objects[0]['field4'].AsFloat);
    AssertEquals(1.23, VJSON.Objects[0]['field5'].AsFloat);
  finally
    VJSON.Free;
  end;
end;

procedure TTestBrookQuery.TestJSONToParams;
var
  VJSON: TJSONObject;
begin
  try
    InitJSONObject(VJSON);
    SetSQL(TEST_SQL_PARAM);
    TBrookQuery.JSONToParams(VJSON, TestQuery.Params, TestQuery.FieldDefs,
      False, False);
    AssertEquals(123, TestQuery.ParamByName('field1').AsInteger);
    AssertEquals('abc', TestQuery.ParamByName('field2').AsString);
    AssertEquals(True, TestQuery.ParamByName('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, TestQuery.ParamByName('field4').AsDateTime);
    AssertEquals(1.23, TestQuery.ParamByName('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
  end;
end;

procedure TTestBrookQuery.TestFieldDefsToSchema1;
var
  S: string;
begin
  TestQuery.Open;
  TBrookQuery.FieldDefsToSchema(TestQuery.FieldDefs, S);
  AssertEquals(S, '[{ "name": "field1", "type": "int" }, { "name": "field2", ' +
    '"type": "string", "maxlen": 50 }, { "name": "field3", "type": "boolean" }, ' +
    '{ "name": "field4", "type": "date" }, { "name": "field5", "type": "float" }]');
end;

procedure TTestBrookQuery.TestFieldDefsToSchema2;
var
  S: TJSONObject;
begin
  S := TJSONObject.Create;
  try
    TestQuery.Open;
    TBrookQuery.FieldDefsToSchema(TestQuery.FieldDefs, S);
    AssertEquals(S.AsJSON, '{ "fields" : [{ "name" : "field1", "type" : "int" }, ' +
      '{ "name" : "field2", "type" : "string", "maxlen" : 50 }, ' +
      '{ "name" : "field3", "type" : "boolean" }, { "name" : "field4", "type" : "date" }, ' +
      '{ "name" : "field5", "type" : "float" }] }');
  finally
    S.Free;
  end;
end;

procedure TTestBrookQuery.TestGetJSONAttributes;
var
  VNames: string;
  VValues: Variant;
  VJSON: TJSONObject;
begin
  try
    InitJSONObject(VJSON);
    TBrookQuery.GetJSONAttributes(VJSON, VNames, VValues);
    AssertEquals('field1;field2;field3;field4;field5', VNames);
    AssertEquals(123, VValues[0]);
    AssertEquals('abc', VValues[1]);
    AssertEquals(True, VValues[2]);
    AssertEquals(TEST_DATETIME, VValues[3]);
    AssertEquals(1.23, VValues[4]);
  finally
    FreeAndNil(VJSON);
  end;
end;

procedure TTestBrookQuery.TestLocate;
var
  VJSON: TJSONObject;
begin
  VJSON := TJSONObject.Create(['field2', 'test1']);
  try
    SetSQL(TEST_SQL_SELECT);
    TestQuery.Open;
    TestQuery.Insert;
    InitFields(TestQuery.Fields);
    TestQuery.Insert;
    InitFields(TestQuery.Fields);
    TestQuery.FieldByName('field2').AsString := 'test1';
    TestQuery.Insert;
    InitFields(TestQuery.Fields);
    TestQuery.FieldByName('field2').AsString := 'test2';
    AssertEquals(True, TBrookQuery.Locate(TestQuery, VJSON, []));
  finally
    VJSON.Free;
  end;
end;

procedure TTestBrookQuery.TestGetRows;
var
  VQuery: TQueryBroker;
  VJSON: TJSONArray;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.DataSet.Append;
    InitFields(VQuery.DataSet.Fields);
    VQuery.DataSet.FieldByName('field2').AsString := 'test1';
    VQuery.DataSet.Append;
    InitFields(VQuery.DataSet.Fields);
    VQuery.DataSet.FieldByName('field2').AsString := 'test2';
    VQuery.GetRows(VJSON);
    AssertEquals(2, VJSON.Count);
    AssertEquals(123, VJSON.Objects[0]['field1'].AsInteger);
    AssertEquals('test1', VJSON.Objects[0]['field2'].AsString);
    AssertEquals(True, VJSON.Objects[0]['field3'].AsBoolean);
    AssertEquals(TEST_DATETIME, VJSON.Objects[0]['field4'].AsFloat);
    AssertEquals(1.23, VJSON.Objects[0]['field5'].AsFloat);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestGetRow;
var
  VQuery: TQueryBroker;
  VJSON: TJSONObject;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.DataSet.Open;
    VQuery.DataSet.Insert;
    InitFields(VQuery.DataSet.Fields);
    VQuery.DataSet.Post;
    VQuery.GetRow(VJSON);
    AssertEquals(5, VJSON.Count);
    AssertEquals(123, VJSON['field1'].AsInteger);
    AssertEquals('abc', VJSON['field2'].AsString);
    AssertEquals(True, VJSON['field3'].AsBoolean);
    AssertEquals(TEST_DATETIME, VJSON['field4'].AsFloat);
    AssertEquals(1.23, VJSON['field5'].AsFloat);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestSetRows;
var
  VQuery: TQueryBroker;
  VJSON: TJSONArray;
  VItem1, VItem2: TJSONObject;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    InitJSONObject(VItem1);
    InitJSONObject(VItem2);
    VJSON := TJSONArray.Create([VItem1, VItem2]);
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.SetRows(VJSON);
    AssertEquals(2, VQuery.DataSet.RecordCount);
    AssertEquals(123, VQuery.DataSet.FieldByName('field1').AsInteger);
    AssertEquals('abc', VQuery.DataSet.FieldByName('field2').AsString);
    AssertEquals(True, VQuery.DataSet.FieldByName('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VQuery.DataSet.FieldByName('field4').AsDateTime);
    AssertEquals(1.23, VQuery.DataSet.FieldByName('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestSetRow;
var
  VQuery: TQueryBroker;
  VJSON: TJSONObject;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    InitJSONObject(VJSON);
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.DataSet.Insert;
    VQuery.SetRow(VJSON);
    VQuery.Post;
    AssertEquals(1, VQuery.DataSet.RecordCount);
    AssertEquals(123, VQuery.DataSet.FieldByName('field1').AsInteger);
    AssertEquals('abc', VQuery.DataSet.FieldByName('field2').AsString);
    AssertEquals(True, VQuery.DataSet.FieldByName('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VQuery.DataSet.FieldByName('field4').AsDateTime);
    AssertEquals(1.23, VQuery.DataSet.FieldByName('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestBind;
var
  VJSON: TJSONObject;
  VQuery: TQueryBroker;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    InitJSONObject(VJSON);
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.Close;
    VQuery.SQL.Text := TEST_SQL_PARAM;
    VQuery.Bind(VJSON);
    AssertEquals(123, VQuery.Param('field1').AsInteger);
    AssertEquals('abc', VQuery.Param('field2').AsString);
    AssertEquals(True, VQuery.Param('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VQuery.Param('field4').AsDateTime);
    AssertEquals(1.23, VQuery.Param('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestAppend;
var
  VJSON: TJSONObject;
  VQuery: TQueryBroker;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    InitJSONObject(VJSON);
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.Append(VJSON);
    VQuery.Post;
    AssertEquals(1, VQuery.Count);
    AssertEquals(123, VQuery.Field('field1').AsInteger);
    AssertEquals('abc', VQuery.Field('field2').AsString);
    AssertEquals(True, VQuery.Field('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VQuery.Field('field4').AsDateTime);
    AssertEquals(1.23, VQuery.Field('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestInsert;
var
  VJSON: TJSONObject;
  VQuery: TQueryBroker;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    InitJSONObject(VJSON);
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.Insert(VJSON);
    VQuery.Post;
    AssertEquals(1, VQuery.Count);
    AssertEquals(123, VQuery.Field('field1').AsInteger);
    AssertEquals('abc', VQuery.Field('field2').AsString);
    AssertEquals(True, VQuery.Field('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VQuery.Field('field4').AsDateTime);
    AssertEquals(1.23, VQuery.Field('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestEdit;
var
  VQuery: TQueryBroker;
  VJSON1, VJSON2: TJSONObject;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    InitJSONObject(VJSON1);
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.Insert(VJSON1);
    VQuery.Post;
    InitJSONObject(VJSON2);
    VJSON2['field1'].AsInteger := 1234;
    VJSON2['field2'].AsString := 'abcd';
    VJSON2['field3'].AsBoolean := False;
    VJSON2['field4'].AsFloat := TEST_DATETIME + 1;
    VJSON2['field5'].AsFloat := 1.234;
    VQuery.Edit(VJSON2);
    AssertEquals(1, VQuery.Count);
    AssertEquals(1234, VQuery.Field('field1').AsInteger);
    AssertEquals('abcd', VQuery.Field('field2').AsString);
    AssertEquals(False, VQuery.Field('field3').AsBoolean);
    AssertEquals(TEST_DATETIME + 1, VQuery.Field('field4').AsDateTime);
    AssertEquals(1.234, VQuery.Field('field5').AsFloat);
  finally
    FreeAndNil(VJSON1);
    FreeAndNil(VJSON2);
    VQuery.Free;
  end;
end;

procedure TTestBrookQuery.TestDelete;
var
  VJSON: TJSONObject;
  VQuery: TQueryBroker;
begin
  VQuery := TQueryBroker.Init(nil);
  try
    InitJSONObject(VJSON);
    VQuery.SQL.Text := TEST_SQL_SELECT;
    VQuery.Open;
    VQuery.Insert(VJSON);
    VQuery.Post;
    VQuery.Delete;
    AssertEquals(0, VQuery.Count);
  finally
    FreeAndNil(VJSON);
    VQuery.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookQuery);
  InitDatabase;
  InitQuery;

finalization
  DoneQuery;
  DoneDatabase;

end.

