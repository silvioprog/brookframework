(*
  Brook Table unit.

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

unit BrookTable;

{$i brook.inc}

interface

uses
  BrookClasses, BrookDataBase, BrookQuery, BrookException, BrookMessages,
  BrookConsts, BrookDBConsts, DB, SQLdb, FPJSON, SysUtils;

type
  { Handles exceptions for @link(TBrookTable). }
  EBrookTable = class(EBrook);

  { Is a metaclass for @link(TBrookTable) class. }
  TBrookTableClass = class of TBrookTable;

  { Handles database tables. }
  TBrookTable = class(TBrookObject)
  private
    FConditionsStr: string;
    FFieldsStr: string;
    FWhereStr: string;
    FOrderByStr: string;
    FPrepared: Boolean;
    FQuery: TBrookQuery;
    FName: string;
    function GetActive: Boolean;
    function GetAsJSON: TJSONStringType;
    function GetBOF: Boolean;
    function GetColumns(const AName: string): TField;
    function GetDataBase: TBrookDataBase;
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetDateAsString: Boolean;
    function GetEOF: Boolean;
    function GetFieldDefs: TFieldDefs;
    function GetFields: TFields;
    function GetModified: Boolean;
    function GetParams: TParams;
    function GetRow: TJSONObject;
    function GetRows: TJSONArray;
    function GetState: TDataSetState;
    procedure SetActive(AValue: Boolean);
    procedure SetAsJSON(AValue: TJSONStringType);
    procedure SetDataBase(AValue: TBrookDataBase);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetDateAsString(AValue: Boolean);
  protected
    procedure CheckTableName;
    procedure CheckJSONParam(AJSON: TJSONData);
    function IsPrepared: Boolean;
    property ConditionsStr: string read FConditionsStr write FConditionsStr;
    property FieldsStr: string read FFieldsStr write FFieldsStr;
    property WhereStr: string read FWhereStr write FWhereStr;
    property OrderByStr: string read FOrderByStr write FOrderByStr;
    property Query: TBrookQuery read FQuery write FQuery;
  public
    { Creates an instance of a @link(TBrookTable) class. }
    constructor Create(ADataBase: TBrookDataBase;
      const ATableName: string = ES); virtual;
    { Creates an instance of a @link(TBrookTable) class. }
    constructor Create(const ATableName: string = ES); reintroduce;
    { Frees an instance of @link(TBrookTable) class. }
    destructor Destroy; override;
    { Receives an JSON object with parameters and creates a "SQL where"
      condition. }
    class procedure GetConditions(AJSON: TJSONObject; out AConditions: string);
    { Returns a JSON object with the table columns. }
    procedure GetSchema(out ASchema: TJSONObject); overload;
    { Returns a JSON string with the table columns. }
    function GetSchema: TJSONStringType; overload;
    { Creates fielddefs from a JSON Object. }
    function CreateFields(AJSON: TJSONObject): TBrookTable;
    { Get all the rows of a query in a JSON array. }
    function GetRows(out AJSON: TJSONArray): TBrookTable;
    { Get the current register in a JSON object. }
    function GetRow(out AJSON: TJSONObject): TBrookTable;
    { Inserts registers from a JSON array. }
    function SetRows(AJSON: TJSONArray): TBrookTable;
    { Inserts one register from a JSON object. }
    function SetRow(AJSON: TJSONObject): TBrookTable;
    { Binds a JSON object to the parameters. }
    function Bind(AJSON: TJSONObject): TBrookTable;
    { Applies all the changes stored in the buffer. }
    function ApplyUpdates: TBrookTable;
    { Cancels all the updates stored in the buffer. }
    function CancelUpdates: TBrookTable;
    { Applies the updates stored in the buffer and commits the transaction. }
    function Apply(const ARetaining: Boolean = False): TBrookTable;
    { Undoes the updates stored in the buffer and rollbacks the transaction. }
    function Undo(const ARetaining: Boolean = False): TBrookTable;
    { Commits the transaction. }
    function Commit(const ARetaining: Boolean = False): TBrookTable;
    { Rollbacks the transaction. }
    function Rollback(const ARetaining: Boolean = False): TBrookTable;
    { Adds a JSON object to the end of registers. }
    function Append(AJSON: TJSONObject): TBrookTable;
    { Inserts a JSON object in the current position. }
    function Insert(AJSON: TJSONObject): TBrookTable;
    { Edits the current register by means of a JSON object. }
    function Edit(AJSON: TJSONObject): TBrookTable;
    { Cancels editions in the table. }
    function Cancel: TBrookTable;
    { Deletes the current register. }
    function Delete: TBrookTable;
    { Opens the table. }
    function Open: TBrookTable;
    { Closes the table. }
    function Close: TBrookTable;
    { Refreshes the table. }
    function Refresh: TBrookTable;
    { Goes to the table first register. }
    function First: TBrookTable;
    { Goes to the table previous register. }
    function Prior: TBrookTable;
    { Goes to the table next register. }
    function Next: TBrookTable;
    { Goes to the table last register. }
    function Last: TBrookTable;
    { Clears the SQL statements and close the table. }
    function Clear: TBrookTable;
    { Clears the SQL statements, close the table and clears the fielddefs. }
    function Reset: TBrookTable;
    { Applies editions to the query. }
    function Post: TBrookTable;
    { Locates a register by means of JSON object. }
    function Locate(AJSON: TJSONObject;
      const AOptions: TLocateOptions = []): Boolean; overload;
    { Locates a register passing a key and a value. }
    function Locate(const AKeyFields: string; const AKeyValues: Variant;
      const AOptions: TLocateOptions = []): Boolean; overload;
    { Finds a register by means of JSON object. }
    function Find(AJSON: TJSONObject;
      const AOptions: TLocateOptions = []): TBrookTable;
    { Gets a register passing a key value. }
    function Get(const AKeyValue: Variant): TBrookTable; overload;
    { Gets a register passing a key and a value. }
    function Get(const AKeyName: string;
      const AKeyValue: Variant): TBrookTable; overload;
    { Get a table field. }
    function Field(const AName: string): TField;
    { Get a table parameter. }
    function Param(const AName: string): TParam;
    { Get a table fielddef. }
    function FieldDef(const AName: string): TFieldDef;
    { Receives an JSON object with parameters and creates a "SQL where"
      condition. }
    function Conditions(AJSON: TJSONObject): TBrookTable;
    { Get the number of registers. }
    function Count: Int64;
    { Get the position of the current register. }
    function Position: Int64;
    { Get the number of changed registers.}
    function RowsAffected: TRowsCount;
    { Composes a SQL statement. }
    function Prepare: TBrookTable;
    { Clears a SQL statement preparation. }
    function Unprepare: TBrookTable;
    { Composes a "SELECT" statement. }
    function Select(const AFields: string): TBrookTable;
    { Composes a "WHERE" statement. }
    function Where(const AConditions: string): TBrookTable; overload;
    { Composes a parametrized "WHERE" statement. }
    function Where(const AConditions: string;
      const AArgs: array of const): TBrookTable; overload;
    { Composes a "ORDER BY" statement. }
    function OrderBy(const AFields: string): TBrookTable;
    { Checks if table is empty. }
    function Empty: Boolean;
    { Creates a bookmark. }
    function GetBookmark: TBookmark;
    { Goes to a bookmark. }
    procedure GotoBookmark(ABookmark: TBookmark);
    { Defines the table name. }
    property Name: string read FName write FName;
    { Checks if the cursor is at the first register. }
    property BOF: Boolean read GetBOF;
    { Checks if the cursor is at the last register. }
    property EOF: Boolean read GetEOF;
    { Returns or creates a bookmark. }
    property Bookmark: TBookmark read GetBookmark write GotoBookmark;
    { Returns @code(True) if the content of the table has changed. }
    property Modified: Boolean read GetModified;
    { Get the current table state. }
    property State: TDataSetState read GetState;
    { Checks if the table is active. }
    property Active: Boolean read GetActive write SetActive;
    { Get the dataset of the table. }
    property DataSet: TDataSet read GetDataSet;
    { Get the fieldefs of the table. }
    property FieldDefs: TFieldDefs read GetFieldDefs;
    { Get or set a master table datasource. }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    { Get or set a table database. }
    property DataBase: TBrookDataBase read GetDataBase write SetDataBase;
    { Enables the date saving as string. }
    property DateAsString: Boolean read GetDateAsString write SetDateAsString;
    { Returns a JSON string with the table registers. }
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    { Returns a JSON array with the table registers. }
    property Rows: TJSONArray read GetRows;
    { Returns the current register in a JSON object. }
    property Row: TJSONObject read GetRow;
    { The list of table columns. }
    property Columns[const AName: string]: TField read GetColumns; default;
    { Checks if a statement preparation is set. }
    property Prepared: Boolean read IsPrepared;
    { Are the table fields. }
    property Fields: TFields read GetFields;
    { Are the table parameters. }
    property Params: TParams read GetParams;
  end;

implementation

constructor TBrookTable.Create(ADataBase: TBrookDataBase;
  const ATableName: string);
begin
  FQuery := TBrookQuery.BrokerClass.Create(ADataBase);
  if Assigned(ADataBase) then
    ADataBase.AddObject(Self);
  FName := ATableName;
end;

constructor TBrookTable.Create(const ATableName: string);
begin
  if Assigned(TBrookDataBases.Service.Current) then
    Create(TBrookDataBases.Service.Current, ATableName)
  else
    Create(TBrookDataBase.Create, ATableName);
end;

destructor TBrookTable.Destroy;
begin
  if Assigned(DataBase) then
    DataBase.RemoveObject(Self);
  FQuery.Free;
  inherited Destroy;
end;

class procedure TBrookTable.GetConditions(AJSON: TJSONObject; out
  AConditions: string);
var
  I: Integer;
  VName: string;
begin
  AConditions := ES;
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    AConditions += VName + BROOK_SQL_EQ_PARAM_TOKEN + VName + SP +
      BROOK_SQL_AND_TOKEN + SP;
  end;
  SetLength(AConditions, Length(AConditions) -
    Length(SP + BROOK_SQL_AND_TOKEN + SP));
end;

procedure TBrookTable.GetSchema(out ASchema: TJSONObject);
begin
  FQuery.GetSchema(ASchema);
end;

function TBrookTable.GetSchema: TJSONStringType;
begin
  Result := FQuery.GetSchema;
end;

function TBrookTable.GetAsJSON: TJSONStringType;
begin
  Result := FQuery.AsJSON;
end;

function TBrookTable.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

function TBrookTable.GetBOF: Boolean;
begin
  Result := DataSet.BOF;
end;

function TBrookTable.GetColumns(const AName: string): TField;
begin
  Result := FQuery.Field(AName);
end;

function TBrookTable.GetDataBase: TBrookDataBase;
begin
  Result := FQuery.DataBase;
end;

function TBrookTable.GetDataSet: TDataSet;
begin
  Result := FQuery.DataSet;
end;

function TBrookTable.GetDataSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TBrookTable.GetDateAsString: Boolean;
begin
  Result := FQuery.DateAsString;
end;

function TBrookTable.GetEOF: Boolean;
begin
  Result := DataSet.EOF;
end;

function TBrookTable.GetFieldDefs: TFieldDefs;
begin
  Result := DataSet.FieldDefs;
end;

function TBrookTable.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TBrookTable.GetModified: Boolean;
begin
  Result := DataSet.Modified;
end;

function TBrookTable.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TBrookTable.GetRow: TJSONObject;
begin
  Result := FQuery.Row;
end;

function TBrookTable.GetRows: TJSONArray;
begin
  Result := FQuery.Rows;
end;

function TBrookTable.GetState: TDataSetState;
begin
  Result := DataSet.State;
end;

procedure TBrookTable.SetActive(AValue: Boolean);
begin
  DataSet.Active := AValue;
end;

procedure TBrookTable.SetAsJSON(AValue: TJSONStringType);
begin
  FQuery.AsJSON := AValue;
end;

procedure TBrookTable.SetDataBase(AValue: TBrookDataBase);
begin
  FQuery.DataBase := AValue;
end;

procedure TBrookTable.SetDataSource(AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

procedure TBrookTable.SetDateAsString(AValue: Boolean);
begin
  FQuery.DateAsString := AValue;
end;

procedure TBrookTable.CheckTableName;
begin
  if FName = ES then
    raise EBrookTable.Create(Self, SBrookEmptyTableNameError);
end;

function TBrookTable.CreateFields(AJSON: TJSONObject): TBrookTable;
var
  I: Integer;
  VFields: string;
begin
  Result := Self;
  CheckTableName;
  CheckJSONParam(AJSON);
  VFields := ES;
  for I := 0 to Pred(AJSON.Count) do
    VFields += AJSON.Names[I] + CS;
  SetLength(VFields, Length(VFields) - 1);
  FQuery.Close;
  FQuery.SQL.Text := BROOK_SQL_SELECT_TOKEN + SP + VFields + SP +
    BROOK_SQL_FROM_TOKEN + SP + FName + SP + BROOK_SQL_NOTHING_WHERE_TOKEN;
  FQuery.Open;
end;

procedure TBrookTable.CheckJSONParam(AJSON: TJSONData);
begin
  if not Assigned(AJSON) then
    raise EBrookQuery.Create(Self, SBrookNilJSONParamError);
end;

function TBrookTable.GetRows(out AJSON: TJSONArray): TBrookTable;
begin
  Result := Self;
  FQuery.GetRows(AJSON);
end;

function TBrookTable.GetRow(out AJSON: TJSONObject): TBrookTable;
begin
  Result := Self;
  FQuery.GetRow(AJSON);
end;

function TBrookTable.SetRows(AJSON: TJSONArray): TBrookTable;
begin
  Result := Self;
  FQuery.SetRows(AJSON);
end;

function TBrookTable.SetRow(AJSON: TJSONObject): TBrookTable;
begin
  Result := Self;
  FQuery.SetRow(AJSON);
end;

function TBrookTable.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

function TBrookTable.Bind(AJSON: TJSONObject): TBrookTable;
begin
  Result := Self;
  FQuery.Bind(AJSON);
end;

function TBrookTable.ApplyUpdates: TBrookTable;
begin
  Result := Self;
  FQuery.ApplyUpdates;
end;

function TBrookTable.CancelUpdates: TBrookTable;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TBrookTable.Apply(const ARetaining: Boolean): TBrookTable;
begin
  Result := Self;
  FQuery.Apply(ARetaining);
end;

function TBrookTable.Undo(const ARetaining: Boolean): TBrookTable;
begin
  Result := Self;
  FQuery.Undo(ARetaining);
end;

function TBrookTable.Commit(const ARetaining: Boolean): TBrookTable;
begin
  Result := Self;
  FQuery.Commit(ARetaining);
end;

function TBrookTable.Rollback(const ARetaining: Boolean): TBrookTable;
begin
  Result := Self;
  FQuery.Rollback(ARetaining);
end;

function TBrookTable.Append(AJSON: TJSONObject): TBrookTable;
begin
  Result := Self;
  if FQuery.DataSet.FieldDefs.Count = 0 then
    CreateFields(AJSON);
  FQuery.Append(AJSON);
end;

function TBrookTable.Insert(AJSON: TJSONObject): TBrookTable;
begin
  Result := Self;
  if FQuery.DataSet.FieldDefs.Count = 0 then
    CreateFields(AJSON);
  FQuery.Insert(AJSON);
end;

function TBrookTable.Edit(AJSON: TJSONObject): TBrookTable;
begin
  Result := Self;
  FQuery.Edit(AJSON);
end;

function TBrookTable.Cancel: TBrookTable;
begin
  Result := Self;
  FQuery.Cancel;
end;

function TBrookTable.Delete: TBrookTable;
begin
  Result := Self;
  FQuery.Delete;
end;

function TBrookTable.Open: TBrookTable;
begin
  Result := Self;
  if not FPrepared then
    Prepare;
  FQuery.Open;
end;

function TBrookTable.Close: TBrookTable;
begin
  Result := Self;
  FQuery.Close;
end;

function TBrookTable.Refresh: TBrookTable;
begin
  Result := Self;
  FQuery.Refresh;
end;

function TBrookTable.First: TBrookTable;
begin
  Result := Self;
  FQuery.First;
end;

function TBrookTable.Prior: TBrookTable;
begin
  Result := Self;
  FQuery.Prior;
end;

function TBrookTable.Next: TBrookTable;
begin
  Result := Self;
  FQuery.Next;
end;

function TBrookTable.Last: TBrookTable;
begin
  Result := Self;
  FQuery.Last;
end;

function TBrookTable.Clear: TBrookTable;
begin
  Result := Self;
  FConditionsStr := ES;
  FFieldsStr := ES;
  FWhereStr := ES;
  FOrderByStr := ES;
  FPrepared := False;
  FQuery.Close;
  FQuery.SQL.Clear;
end;

function TBrookTable.Reset: TBrookTable;
begin
  Result := Clear;
  DataSet.FieldDefs.Clear;
end;

function TBrookTable.Post: TBrookTable;
begin
  Result := Self;
  FQuery.Post;
end;

function TBrookTable.Locate(AJSON: TJSONObject;
  const AOptions: TLocateOptions): Boolean;
begin
  Result := FQuery.Locate(AJSON, AOptions);
end;

function TBrookTable.Locate(const AKeyFields: string;
  const AKeyValues: Variant; const AOptions: TLocateOptions): Boolean;
begin
  Result := FQuery.Locate(AKeyFields, AKeyValues, AOptions);
end;

function TBrookTable.Find(AJSON: TJSONObject;
  const AOptions: TLocateOptions): TBrookTable;
begin
  Result := Open;
  FQuery.Locate(AJSON, AOptions);
end;

function TBrookTable.Get(const AKeyValue: Variant): TBrookTable;
begin
  Result := Open;
  FQuery.Locate(BROOK_DEFAULT_KEY_NAME, AKeyValue);
end;

function TBrookTable.Get(const AKeyName: string;
  const AKeyValue: Variant): TBrookTable;
begin
  Result := Open;
  FQuery.Locate(AKeyName, AKeyValue);
end;

function TBrookTable.Field(const AName: string): TField;
begin
  Result := FQuery.Field(AName);
end;

function TBrookTable.Param(const AName: string): TParam;
begin
  Result := FQuery.Param(AName);
end;

function TBrookTable.FieldDef(const AName: string): TFieldDef;
begin
  Result := FQuery.FieldDef(AName);
end;

function TBrookTable.Conditions(AJSON: TJSONObject): TBrookTable;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  GetConditions(AJSON, FConditionsStr);
end;

function TBrookTable.Count: Int64;
begin
  Result := FQuery.Count;
end;

function TBrookTable.Position: Int64;
begin
  Result := FQuery.Position;
end;

function TBrookTable.RowsAffected: TRowsCount;
begin
  Result := FQuery.RowsAffected;
end;

function TBrookTable.Prepare: TBrookTable;
var
  VIsWhere: Boolean;
begin
  Result := Self;
  CheckTableName;
  if FFieldsStr = ES then
    FFieldsStr := AK;
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add(BROOK_SQL_SELECT_TOKEN + SP + FFieldsStr + SP +
    BROOK_SQL_FROM_TOKEN + SP + FName);
  VIsWhere := FWhereStr <> ES;
  if VIsWhere then
    FQuery.SQL.Add(BROOK_SQL_WHERE_TOKEN + SP + FWhereStr);
  if FConditionsStr <> ES then
    if VIsWhere then
      FQuery.SQL.Add(BROOK_SQL_AND_TOKEN + SP + FConditionsStr)
    else
      FQuery.SQL.Add(BROOK_SQL_WHERE_TOKEN + SP + FConditionsStr);
  if FOrderByStr <> ES then
    FQuery.SQL.Add(BROOK_SQL_ORDER_BY_TOKEN + SP + FOrderByStr);
  FPrepared := True;
end;

function TBrookTable.Unprepare: TBrookTable;
begin
  Result := Self;
  FPrepared := False;
  FConditionsStr := ES;
  FFieldsStr := ES;
  FWhereStr := ES;
  FOrderByStr := ES;
  FQuery.SQL.Clear;
end;

function TBrookTable.Select(const AFields: string): TBrookTable;
begin
  Result := Self;
  FFieldsStr := AFields;
end;

function TBrookTable.Where(const AConditions: string): TBrookTable;
begin
  Result := Self;
  FWhereStr := AConditions;
end;

function TBrookTable.Where(const AConditions: string;
  const AArgs: array of const): TBrookTable;
begin
  Result := Self;
  FWhereStr := Format(AConditions, AArgs);
end;

function TBrookTable.OrderBy(const AFields: string): TBrookTable;
begin
  Result := Self;
  FOrderByStr := AFields;
end;

function TBrookTable.Empty: Boolean;
begin
  Result := DataSet.IsEmpty;
end;

function TBrookTable.GetBookmark: TBookmark;
begin
  Result := DataSet.GetBookmark;
end;

procedure TBrookTable.GotoBookmark(ABookmark: TBookmark);
begin
  DataSet.GotoBookmark(ABookmark);
end;

end.
