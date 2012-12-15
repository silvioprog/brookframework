unit testbrookdbutils;

{$mode objfpc}{$H+}

interface

uses
  BrookDBUtils, fpcunit, testregistry, sqldb, db, sysutils;

type
  TTestBrookDBUtils = class(TTestCase)
  published
    procedure TestBrookDataSetToHTMLTable;
    procedure TestBrookDataSetToHTML5Table;
    procedure TestBrookFieldDefsToHTMLForm;
  end;

const
  HTML_TABLE =
    '<table border="1" >'+#10+
    '	<tr>'+#10+
    '		<td>field1</td>'+#10+
    '		<td>field2</td>'+#10+
    '		<td>field3</td>'+#10+
    '		<td>field4</td>'+#10+
    '		<td>field5</td>'+#10+
    '	</tr>'+#10+
    '</table>';
  HTML5_TABLE =
    '<table border="1" >'+#10+
    '	<thead>'+#10+
    '		<tr>'+#10+
    '				<td>field1</td>'+#10+
    '				<td>field2</td>'+#10+
    '				<td>field3</td>'+#10+
    '				<td>field4</td>'+#10+
    '				<td>field5</td>'+#10+
    '		</tr>'+#10+
    '	</thead>'+#10+
    '	<tbody>'+#10+
    '	</tbody>'+#10+
    '</table>';
  HTML_FORM =
    '<form action="#" method="post" >'+#10+
    '	<label for="field1">'+#10+
    '		Field1 <input type="text" name="field1" id="field1" />'+#10+
    '	</label>'+#10+
    '	<label for="field2">'+#10+
    '		Field2 <input type="text" name="field2" id="field2" maxlength="50" />'+#10+
    '	</label>'+#10+
    '	<label for="field3">'+#10+
    '		Field3 <input type="checkbox" name="field3" id="field3" />'+#10+
    '	</label>'+#10+
    '	<label for="field4">'+#10+
    '		Field4 <input type="text" name="field4" id="field4" />'+#10+
    '	</label>'+#10+
    '	<label for="field5">'+#10+
    '		Field5 <input type="text" name="field5" id="field5" maxlength="2" />'+#10+
    '	</label>'+#10+
    '	<input type="submit" class="btn" />'+#10+
    '</form>';

implementation

var
  _db: TSQLConnector;
  _q: TSQLQuery;

function q: TDataSet;
begin
  Result := _q;
  if _q.Active then
    Exit;
  _db.Transaction.DataBase := _db;
  _q.DataBase := _db;
  _q.Transaction := _db.Transaction;
  _db.ConnectorType := 'SQLite3';
  _db.DatabaseName := 'testdb.sqlite3';
  _q.SQL.Text := 'select * from testtable';
  _q.Open;
end;

procedure TTestBrookDBUtils.TestBrookDataSetToHTMLTable;
begin
  AssertEquals(HTML_TABLE, Trim(BrookDataSetToHTMLTable(q, [])));
end;

procedure TTestBrookDBUtils.TestBrookDataSetToHTML5Table;
begin
  AssertEquals(HTML5_TABLE, Trim(BrookDataSetToHTML5Table(q, [])));
end;

procedure TTestBrookDBUtils.TestBrookFieldDefsToHTMLForm;
begin
  AssertEquals(HTML_FORM, Trim(BrookFieldDefsToHTMLForm(q.FieldDefs)));
end;

initialization
  RegisterTest(TTestBrookDBUtils);
  _db := TSQLConnector.Create(nil);
  _db.Transaction := TSQLTransaction.Create(_db);
  _q := TSQLQuery.Create(_db);

finalization
  FreeAndNil(_db);

end.

