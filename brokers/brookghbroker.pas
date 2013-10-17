(*
  Brook Greyhound Broker unit.

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

unit BrookGhBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, BrookTable, BrookException, BrookMessages,
  BrookDBConsts, ghSQL, DB, DBConst, FPJSON, JSONParser;

type
  EBrookGhTable = class(EBrook);

  { TBrookGhDataBase }

  TBrookGhDataBase = class(TBrookDataBase)
  private
    FConn: TghSQLConnector;
  protected
    class function GetSQLLibClass: TghSQLLibClass; virtual; abstract;
    function GetConnected: Boolean; override;
    function GetDatabase: string; override;
    function GetHost: string; override;
    function GetPassword: string; override;
    function GetUser: string; override;
    procedure SetDatabase(AValue: string); override;
    procedure SetHost(AValue: string); override;
    procedure SetPassword(AValue: string); override;
    procedure SetUser(AValue: string); override;
    function GetConnection: TObject; override;
  public
    constructor Init; override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function InTransaction: Boolean; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
  published
    property Driver: string read GetDriver write SetDriver;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
  end;

  { TBrookGhTable }

  TBrookGhTable = class(TghSQLTable)
  private
    FDateAsString: Boolean;
    function GetAsJSON: TJSONStringType;
    function GetDataBase: TghSQLConnector;
    function GetDataSet: TDataSet;
    function GetFieldDefs: TFieldDefs;
    function GetFields: TFields;
    function GetName: string;
    function GetRow: TJSONObject;
    function GetRows: TJSONArray;
    procedure SetAsJSON(AValue: TJSONStringType);
    procedure SetDataBase(AValue: TghSQLConnector);
    procedure SetName(AValue: string);
  protected
    procedure CheckActive;
    procedure CheckJSONParam(AJSON: TJSONData);
  public
    function GetRows(out AJSON: TJSONArray): TBrookGhTable;
    function GetRow(out AJSON: TJSONObject): TBrookGhTable;
    function SetRows(AJSON: TJSONArray): TBrookGhTable;
    function SetRow(AJSON: TJSONObject): TBrookGhTable;
    function Bind(AJSON: TJSONObject): TBrookGhTable;
    function Apply: TBrookGhTable;
    function Undo: TBrookGhTable;
    function Close: TBrookGhTable;
    function Open: TBrookGhTable;
    function Post: TBrookGhTable;
    function Cancel: TBrookGhTable;
    function Delete: TBrookGhTable;
    function Refresh: TBrookGhTable;
    function First: TBrookGhTable;
    function Prior: TBrookGhTable;
    function Next: TBrookGhTable;
    function Last: TBrookGhTable;
    function Select(const AColumnNames: string): TBrookGhTable;
    function Where(const AConditions: string): TBrookGhTable; overload;
    function Where(const AConditions: string;
      AArgs: array of const): TBrookGhTable; overload;
    function OrderBy(const AColumnNames: string): TBrookGhTable;
    function Append(AJSON: TJSONObject): TBrookGhTable; overload;
    function Insert(AJSON: TJSONObject): TBrookGhTable; overload;
    function Edit(AJSON: TJSONObject): TBrookGhTable; overload;
    function Locate(AJSON: TJSONObject;
      const AOptions: TLocateOptions = []): Boolean; overload;
    function Locate(const AKeyFields: string; const AKeyValues: Variant;
      const AOptions: TLocateOptions = []): Boolean; overload;
    function Find(AJSON: TJSONObject;
      const AOptions: TLocateOptions = []): TBrookGhTable;
    function Get(const AKeyValue: Variant): TBrookGhTable; overload;
    function Get(const AKeyName: string;
      const AKeyValue: Variant): TBrookGhTable; overload;
    function Field(const AName: string): TField;
    function Param(const AName: string): TParam;
    function FieldDef(const AName: string): TFieldDef;
    function Conditions(AJSON: TJSONObject): TBrookGhTable;
    function Count: Int64;
    function Position: Int64;
    property Name: string read GetName write SetName;
    property DataSet: TDataSet read GetDataSet;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property DataBase: TghSQLConnector read GetDataBase write SetDataBase;
    property DateAsString: Boolean read FDateAsString write FDateAsString;
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    property Rows: TJSONArray read GetRows;
    property Row: TJSONObject read GetRow;
    property Fields: TFields read GetFields;
  end;

implementation

{ TBrookGhDataBase }

constructor TBrookGhDataBase.Init;
begin
  inherited Init;
  FConn := TghSQLConnector.Create(GetSQLLibClass);
end;

destructor TBrookGhDataBase.Destroy;
begin
  FreeAndNil(FConn);
  inherited Destroy;
end;

function TBrookGhDataBase.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

function TBrookGhDataBase.GetDatabase: string;
begin
  Result := FConn.Database;
end;

function TBrookGhDataBase.GetHost: string;
begin
  Result := FConn.Host;
end;

function TBrookGhDataBase.GetPassword: string;
begin
  Result := FConn.Password;
end;

function TBrookGhDataBase.GetUser: string;
begin
  Result := FConn.User;
end;

procedure TBrookGhDataBase.SetDatabase(AValue: string);
begin
  FConn.Database := AValue;
end;

procedure TBrookGhDataBase.SetHost(AValue: string);
begin
  FConn.Host := AValue;
end;

procedure TBrookGhDataBase.SetPassword(AValue: string);
begin
  FConn.Password := AValue;
end;

procedure TBrookGhDataBase.SetUser(AValue: string);
begin
  FConn.User := AValue;
end;

function TBrookGhDataBase.GetConnection: TObject;
begin
  Result := FConn;
end;

procedure TBrookGhDataBase.Connect;
begin
  FConn.Connect;
end;

procedure TBrookGhDataBase.Disconnect;
begin
  FConn.Disconnect;
end;

function TBrookGhDataBase.InTransaction: Boolean;
begin
  Result := FConn.InTransaction;
end;

procedure TBrookGhDataBase.StartTransaction;
begin
  FConn.StartTransaction;
end;

procedure TBrookGhDataBase.Commit;
begin
  FConn.Commit;
end;

procedure TBrookGhDataBase.Rollback;
begin
  FConn.Rollback;
end;

{ TBrookGhTable }

function TBrookGhTable.GetAsJSON: TJSONStringType;
var
  VJSON: TJSONArray = nil;
begin
  GetRows(VJSON);
  try
    Result := VJSON.AsJSON;
  finally
    VJSON.Free;
  end;
end;

function TBrookGhTable.GetDataBase: TghSQLConnector;
begin
  Result := Connector;
end;

function TBrookGhTable.GetDataSet: TDataSet;
begin
  CheckData;
  Result := FData;
end;

function TBrookGhTable.GetFieldDefs: TFieldDefs;
begin
  Result := DataSet.FieldDefs;
end;

function TBrookGhTable.GetFields: TFields;
begin
  Result := DataSet.Fields;
end;

function TBrookGhTable.GetName: string;
begin
  Result := TableName;
end;

function TBrookGhTable.GetRow: TJSONObject;
begin
  GetRow(Result);
end;

function TBrookGhTable.GetRows: TJSONArray;
begin
  GetRows(Result);
end;

procedure TBrookGhTable.SetAsJSON(AValue: TJSONStringType);
var
  I: Integer;
  VArray: TJSONArray;
  VParser: TJSONParser;
begin
  VParser := TJSONParser.Create(AValue);
  try
    VArray := VParser.Parse as TJSONArray;
    for I := 0 to Pred(VArray.Count) do
      Append(VArray.Objects[I]);
  finally
    VArray.Free;
    VParser.Free;
  end;
end;

procedure TBrookGhTable.SetDataBase(AValue: TghSQLConnector);
begin
  Connector := AValue;
end;

procedure TBrookGhTable.SetName(AValue: string);
begin
  TableName := AValue;
end;

procedure TBrookGhTable.CheckActive;
begin
  if not Active then
    inherited Open;
end;

procedure TBrookGhTable.CheckJSONParam(AJSON: TJSONData);
begin
  if not Assigned(AJSON) then
    raise EBrookGhTable.Create(Self, SBrookNilJSONParamError);
end;

function TBrookGhTable.GetRows(out AJSON: TJSONArray): TBrookGhTable;
var
  VBookMark: TBookMark;
begin
  Result := Self;
  AJSON := TJSONArray.Create;
  CheckActive;
  if DataSet.RecordCount = 0 then
    Exit;
  DataSet.DisableControls;
  VBookMark := DataSet.GetBookmark;
  try
    TBrookQuery.DataSetToJSON(DataSet, AJSON, FDateAsString);
    DataSet.GotoBookmark(VBookMark);
  finally
    DataSet.FreeBookmark(VBookmark);
    DataSet.EnableControls;
  end;
end;

function TBrookGhTable.GetRow(out AJSON: TJSONObject): TBrookGhTable;
begin
  Result := Self;
  AJSON := TJSONObject.Create;
  CheckActive;
  if DataSet.RecordCount <> 0 then
    TBrookQuery.FieldsToJSON(DataSet.Fields, AJSON, FDateAsString);
end;

function TBrookGhTable.SetRows(AJSON: TJSONArray): TBrookGhTable;
var
  I: Integer;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  for I := 0 to Pred(AJSON.Count) do
  begin
    DataSet.Append;
    TBrookQuery.JSONToFields(AJSON.Objects[I], DataSet.Fields, FDateAsString);
    DataSet.Post;
  end;
end;

function TBrookGhTable.SetRow(AJSON: TJSONObject): TBrookGhTable;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookGhTable.Bind(AJSON: TJSONObject): TBrookGhTable;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TBrookQuery.JSONToParams(AJSON, Params, DataSet.FieldDefs,
    FDateAsString, True);
end;

function TBrookGhTable.Apply: TBrookGhTable;
begin
  Result := Self;
  Commit;
end;

function TBrookGhTable.Undo: TBrookGhTable;
begin
  Result := Self;
  Rollback;
end;

function TBrookGhTable.Close: TBrookGhTable;
begin
  Result := Self;
  inherited Close;
end;

function TBrookGhTable.Open: TBrookGhTable;
begin
  Result := Self;
  inherited Open;
end;

function TBrookGhTable.Post: TBrookGhTable;
begin
  Result := Self;
  inherited Post;
end;

function TBrookGhTable.Cancel: TBrookGhTable;
begin
  Result := Self;
  inherited Cancel;
end;

function TBrookGhTable.Delete: TBrookGhTable;
begin
  Result := Self;
  inherited Delete;
end;

function TBrookGhTable.Refresh: TBrookGhTable;
begin
  Result := Self;
  inherited Refresh;
end;

function TBrookGhTable.First: TBrookGhTable;
begin
  Result := Self;
  inherited First;
end;

function TBrookGhTable.Prior: TBrookGhTable;
begin
  Result := Self;
  inherited Prior;
end;

function TBrookGhTable.Next: TBrookGhTable;
begin
  Result := Self;
  inherited Next;
end;

function TBrookGhTable.Last: TBrookGhTable;
begin
  Result := Self;
  inherited Last;
end;

function TBrookGhTable.Select(const AColumnNames: string): TBrookGhTable;
begin
  Result := Self;
  inherited Select(AColumnNames);
end;

function TBrookGhTable.Where(const AConditions: string): TBrookGhTable;
begin
  Result := Self;
  inherited Where(AConditions);
end;

function TBrookGhTable.Where(const AConditions: string;
  AArgs: array of const): TBrookGhTable;
begin
  Result := Self;
  inherited Where(AConditions, AArgs);
end;

function TBrookGhTable.OrderBy(const AColumnNames: string): TBrookGhTable;
begin
  Result := Self;
  inherited OrderBy(AColumnNames);
end;

function TBrookGhTable.Append(AJSON: TJSONObject): TBrookGhTable;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  inherited Append;
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookGhTable.Insert(AJSON: TJSONObject): TBrookGhTable;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  inherited Insert;
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookGhTable.Edit(AJSON: TJSONObject): TBrookGhTable;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  inherited Edit;
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookGhTable.Locate(AJSON: TJSONObject;
  const AOptions: TLocateOptions): Boolean;
begin
  CheckJSONParam(AJSON);
  Result := TBrookQuery.Locate(DataSet, AJSON, AOptions);
end;

function TBrookGhTable.Locate(const AKeyFields: string;
  const AKeyValues: Variant; const AOptions: TLocateOptions): Boolean;
begin
  Result := DataSet.Locate(AKeyFields, AKeyValues, AOptions);
end;

function TBrookGhTable.Find(AJSON: TJSONObject;
  const AOptions: TLocateOptions): TBrookGhTable;
begin
  Result := Open;
  Locate(AJSON, AOptions);
end;

function TBrookGhTable.Get(const AKeyValue: Variant): TBrookGhTable;
begin
  Result := Open;
  Locate(BROOK_DEFAULT_KEY_NAME, AKeyValue);
end;

function TBrookGhTable.Get(const AKeyName: string;
  const AKeyValue: Variant): TBrookGhTable;
begin
  Result := Open;
  Locate(AKeyName, AKeyValue);
end;

function TBrookGhTable.Field(const AName: string): TField;
begin
  Result := DataSet.FindField(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SFieldNotFound, [AName], DataSet);
end;

function TBrookGhTable.Param(const AName: string): TParam;
begin
  Result := Params.FindParam(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SParameterNotFound, [AName], DataSet);
end;

function TBrookGhTable.FieldDef(const AName: string): TFieldDef;
begin
  Result := DataSet.FieldDefs.Find(AName);
end;

function TBrookGhTable.Conditions(AJSON: TJSONObject): TBrookGhTable;
var
  VConditions: string;
begin
  Result := Self;
  TBrookTable.GetConditions(AJSON, VConditions);
  inherited Where(VConditions);
end;

function TBrookGhTable.Count: Int64;
begin
  Result := DataSet.RecordCount;
end;

function TBrookGhTable.Position: Int64;
begin
  Result := DataSet.RecNo;
end;

end.

