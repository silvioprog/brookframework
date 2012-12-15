unit testbrooktable;

{$mode objfpc}{$H+}

interface

uses
  BrookTable, BrookQuery, BrookDataBase, BrookDBConsts, sqlite3conn, sqldb, db,
  fpjson, fpcunit, testregistry, sysutils, Classes;

type
  TQueryBroker = class(TBrookQuery)
  private
    FQuery: TSQLQuery;
  protected
    function GetSQL: TStrings; override;
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
  public
    constructor Init(ADataBase: TBrookDataBase); override;
    function Field(const AName: string): TField; override;
    function Param(const AName: string): TParam; override;
    function ApplyUpdates: TBrookQuery; override;
  end;

  TTable = class(TBrookTable)
  public
    constructor Create(ADataBase: TBrookDataBase;
      const ATableName: string = ''); override;
    destructor Destroy; override;
    property Query;
  end;

  TTestBrookTable = class(TTestCase)
  published
    procedure TestGetRows;
    procedure TestGetRow;
    procedure TestSetRows;
    procedure TestSetRow;
    procedure TestBind;
    procedure TestAppend;
    procedure TestInsert;
    procedure TestEdit;
    procedure TestDelete;
    procedure TestLocate;
    procedure TestFind;
    procedure TestGet;
    procedure TestConditions;
    procedure TestPrepare;
    procedure TestUnprepare;
    procedure TestSelect;
    procedure TestWhere;
    procedure TestOrderBy;
    procedure TestEmpty;
    procedure TestColumns;
  end;

implementation

const
  TEST_DATETIME = 41233 + 0.6161111111;
  TEST_SQL_PARAM =
    'select field1, field2, field3, field4, field5 from testtable ' +
    'where field1 = :field1 and field2 = :field2 and field3 = :field3 and ' +
    'field4 = :field4 and field5 = :field5';

var
  TestDatabase: TSQLite3Connection;

procedure InitDatabase;
begin
  TestDatabase := TSQLite3Connection.Create(nil);
  TestDatabase.Transaction := TSQLTransaction.Create(TestDatabase);
  TestDatabase.Transaction.DataBase := TestDatabase;
  TestDatabase.DatabaseName := 'testdb.sqlite3';
end;

procedure DoneDatabase;
begin
  FreeAndNil(TestDatabase);
end;

procedure InitJSONObject(out J: TJSONObject);
begin
  J := TJSONObject.Create(['field1', 123, 'field2', 'abc', 'field3', True,
    'field4', TEST_DATETIME, 'field5', 1.23]);
end;

{ TQueryBroker }

constructor TQueryBroker.Init(ADataBase: TBrookDataBase);
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := TestDataBase;
  FQuery.Transaction := TestDatabase.Transaction;
end;

function TQueryBroker.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AName);
end;

function TQueryBroker.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AName);
end;

function TQueryBroker.ApplyUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.ApplyUpdates;
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

{ TTable }

constructor TTable.Create(ADataBase: TBrookDataBase; const ATableName: string);
begin
  Query := TQueryBroker.Init(nil);
  Name := 'testtable';
end;

destructor TTable.Destroy;
begin
  Query.Free;
  inherited Destroy;
end;

{ TTestBrookTable }

procedure TTestBrookTable.TestGetRows;
var
  VTable: TTable;
  VJSON: TJSONArray;
  VItem1, VItem2 : TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VItem1);
    VTable.Append(VItem1);
    InitJSONObject(VItem2);
    VTable.Append(VItem2);
    VTable.GetRows(VJSON);
    AssertEquals(2, VJSON.Count);
    AssertEquals(123, VJSON.Objects[0]['field1'].AsInteger);
    AssertEquals('abc', VJSON.Objects[0]['field2'].AsString);
    AssertEquals(True, VJSON.Objects[0]['field3'].AsBoolean);
    AssertEquals(TEST_DATETIME, VJSON.Objects[0]['field4'].AsFloat);
    AssertEquals(1.23, VJSON.Objects[0]['field5'].AsFloat);
  finally
    FreeAndNil(VItem1);
    FreeAndNil(VItem2);
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestGetRow;
var
  VTable: TTable;
  VItem, VJSON: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VItem);
    VTable.Insert(VItem);
    VTable.Post;
    VTable.GetRow(VJSON);
    AssertEquals(5, VJSON.Count);
    AssertEquals(123, VJSON['field1'].AsInteger);
    AssertEquals('abc', VJSON['field2'].AsString);
    AssertEquals(True, VJSON['field3'].AsBoolean);
    AssertEquals(TEST_DATETIME, VJSON['field4'].AsFloat);
    AssertEquals(1.23, VJSON['field5'].AsFloat);
  finally
    FreeAndNil(VItem);
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestSetRows;
var
  VTable: TTable;
  VJSON: TJSONArray;
  VItem1, VItem2: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  try
    VTable.Open;
    while not VTable.Empty do
      VTable.Delete;
    InitJSONObject(VItem1);
    InitJSONObject(VItem2);
    VJSON := TJSONArray.Create([VItem1, VItem2]);
    VTable.Open;
    VTable.SetRows(VJSON);
    AssertEquals(2, VTable.DataSet.RecordCount);
    AssertEquals(123, VTable.DataSet.FieldByName('field1').AsInteger);
    AssertEquals('abc', VTable.DataSet.FieldByName('field2').AsString);
    AssertEquals(True, VTable.DataSet.FieldByName('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VTable.DataSet.FieldByName('field4').AsDateTime);
    AssertEquals(1.23, VTable.DataSet.FieldByName('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestSetRow;
var
  VTable: TTable;
  VItem, VJSON: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VJSON);
    InitJSONObject(VItem);
    VTable.Insert(VItem);
    VTable.SetRow(VJSON);
    VTable.Post;
    AssertEquals(1, VTable.DataSet.RecordCount);
    AssertEquals(123, VTable.DataSet.FieldByName('field1').AsInteger);
    AssertEquals('abc', VTable.DataSet.FieldByName('field2').AsString);
    AssertEquals(True, VTable.DataSet.FieldByName('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VTable.DataSet.FieldByName('field4').AsDateTime);
    AssertEquals(1.23, VTable.DataSet.FieldByName('field5').AsFloat);
  finally
    FreeAndNil(VItem);
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestBind;
var
  VTable: TTable;
  VJSON: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VJSON);
    VTable.Open;
    VTable.Close;
    VTable.Select(TEST_SQL_PARAM).Prepare;
    VTable.Bind(VJSON);
    AssertEquals(123, VTable.Param('field1').AsInteger);
    AssertEquals('abc', VTable.Param('field2').AsString);
    AssertEquals(True, VTable.Param('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VTable.Param('field4').AsDateTime);
    AssertEquals(1.23, VTable.Param('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestAppend;
var
  VJSON: TJSONObject;
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VJSON);
    VTable.Append(VJSON);
    VTable.Post;
    AssertEquals(1, VTable.Count);
    AssertEquals(123, VTable.Field('field1').AsInteger);
    AssertEquals('abc', VTable.Field('field2').AsString);
    AssertEquals(True, VTable.Field('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VTable.Field('field4').AsDateTime);
    AssertEquals(1.23, VTable.Field('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestInsert;
var
  VJSON: TJSONObject;
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VJSON);
    VTable.Insert(VJSON);
    VTable.Post;
    AssertEquals(1, VTable.Count);
    AssertEquals(123, VTable.Field('field1').AsInteger);
    AssertEquals('abc', VTable.Field('field2').AsString);
    AssertEquals(True, VTable.Field('field3').AsBoolean);
    AssertEquals(TEST_DATETIME, VTable.Field('field4').AsDateTime);
    AssertEquals(1.23, VTable.Field('field5').AsFloat);
  finally
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestEdit;
var
  VTable: TTable;
  VJSON1, VJSON2: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VJSON1);
    VTable.Insert(VJSON1);
    VTable.Post;
    InitJSONObject(VJSON2);
    VJSON2['field1'].AsInteger := 1234;
    VJSON2['field2'].AsString := 'abcd';
    VJSON2['field3'].AsBoolean := False;
    VJSON2['field4'].AsFloat := TEST_DATETIME + 1;
    VJSON2['field5'].AsFloat := 1.234;
    VTable.Edit(VJSON2);
    AssertEquals(1, VTable.Count);
    AssertEquals(1234, VTable.Field('field1').AsInteger);
    AssertEquals('abcd', VTable.Field('field2').AsString);
    AssertEquals(False, VTable.Field('field3').AsBoolean);
    AssertEquals(TEST_DATETIME + 1, VTable.Field('field4').AsDateTime);
    AssertEquals(1.234, VTable.Field('field5').AsFloat);
  finally
    FreeAndNil(VJSON1);
    FreeAndNil(VJSON2);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestDelete;
var
  VJSON: TJSONObject;
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VJSON);
    VTable.Insert(VJSON);
    VTable.Post;
    VTable.Delete;
    AssertEquals(0, VTable.Count);
  finally
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestLocate;
var
  VTable: TTable;
  VItem1, VItem2, VJSON: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  VJSON := TJSONObject.Create(['field2', 'test1']);
  try
    InitJSONObject(VItem1);
    VItem1['field2'].AsString := 'test1';
    VTable.Insert(VItem1);
    InitJSONObject(VItem2);
    VItem1['field2'].AsString := 'test2';
    VTable.Insert(VItem2);
    AssertEquals(True, VTable.Locate(VJSON));
  finally
    FreeAndNil(VItem1);
    FreeAndNil(VItem2);
    VJSON.Free;
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestFind;
var
  VTable: TTable;
  VItem1, VItem2, VJSON: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  VJSON := TJSONObject.Create(['field2', 'test1']);
  try
    VTable.Open;
    while not VTable.Empty do
      VTable.Delete;
    InitJSONObject(VItem1);
    VItem1['field1'].AsInteger := 1;
    VItem1['field2'].AsString := 'test1';
    VTable.Insert(VItem1);
    InitJSONObject(VItem2);
    VItem1['field1'].AsInteger := 2;
    VItem1['field2'].AsString := 'test2';
    VTable.Insert(VItem2);
    VTable.ApplyUpdates;
    AssertEquals('test1', VTable.Find(VJSON).Field('field2').AsString);
  finally
    FreeAndNil(VItem1);
    FreeAndNil(VItem2);
    VJSON.Free;
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestGet;
var
  VTable: TTable;
  VItem1, VItem2, VJSON: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  VJSON := TJSONObject.Create(['field2', 'test1']);
  try
    VTable.Open;
    while not VTable.Empty do
      VTable.Delete;
    InitJSONObject(VItem1);
    VItem1['field1'].AsInteger := 1;
    VItem1['field2'].AsString := 'test1';
    VTable.Insert(VItem1);
    InitJSONObject(VItem2);
    VItem2['field1'].AsInteger := 2;
    VItem2['field2'].AsString := 'test2';
    VTable.Insert(VItem2);
    VTable.ApplyUpdates;
    AssertEquals('test2', VTable.Get(2).Field('field2').AsString);
  finally
    FreeAndNil(VItem1);
    FreeAndNil(VItem2);
    VJSON.Free;
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestConditions;
var
  VTable: TTable;
  VJSON: TJSONObject;
begin
  VTable := TTable.Create(nil, '');
  try
    InitJSONObject(VJSON);
    VTable.Conditions(VJSON).Prepare;
    AssertEquals(5, VTable.Params.Count);
  finally
    FreeAndNil(VJSON);
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestPrepare;
var
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    VTable.Select('*').Prepare;
    AssertEquals(True, VTable.Prepared);
    AssertEquals('select * from testtable', Trim(VTable.Query.SQL.Text));
  finally
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestUnprepare;
var
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    VTable.Select('*').Prepare;
    VTable.Unprepare;
    AssertEquals(False, VTable.Prepared);
    AssertEquals('', VTable.Query.SQL.Text);
  finally
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestSelect;
var
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    VTable.Select('field1').Open;
    AssertEquals('select field1 from testtable', Trim(VTable.Query.SQL.Text));
  finally
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestWhere;
var
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    VTable.Select('field1').Where('field1 > 0').Open;
    AssertEquals(True, Pos('where field1 > 0', VTable.Query.SQL.Text) <> 0);
  finally
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestOrderBy;
begin

end;

procedure TTestBrookTable.TestEmpty;
var
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    VTable.Select('field1').OrderBy('field1').Open;
    AssertEquals(True, Pos('order by field1', VTable.Query.SQL.Text) <> 0);
  finally
    VTable.Free;
  end;
end;

procedure TTestBrookTable.TestColumns;
var
  VTable: TTable;
begin
  VTable := TTable.Create(nil, '');
  try
    VTable.Open;
    AssertEquals(True, Assigned(VTable['field1']));
  finally
    VTable.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookTable);
  InitDatabase;
  BROOK_DEFAULT_KEY_NAME := 'field1';

finalization
  DoneDatabase;

end.

