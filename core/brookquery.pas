(*
  Brook Query unit.

  Copyright (C) 2012 Silvio Clecio.

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

unit BrookQuery;

{$i brook.inc}

interface

uses
  BrookClasses, BrookDataBase, BrookException, BrookMessages, BrookConsts,
  BrookDBConsts, DB, SQLdb, FPJSON, JSONParser, SysUtils, Classes, Variants;

type
  { Handles exceptions for @link(TBrookQuery). }
  EBrookQuery = class(EBrook);

  { Is a metaclass for @link(TBrookQuery) class. }
  TBrookQueryClass = class of TBrookQuery;

  { Handles database queries. }
  TBrookQuery = class(TBrookObject)
  private
    FDateAsString: Boolean;
    function GetActive: Boolean;
    function GetAsJSON: TJSONStringType;
    function GetBOF: Boolean;
    function GetEOF: Boolean;
    function GetFieldDefs: TFieldDefs;
    function GetModified: Boolean;
    function GetRow: TJSONObject;
    function GetRows: TJSONArray;
    function GetState: TDataSetState;
    procedure SetActive(AValue: Boolean);
    procedure SetAsJSON(AValue: TJSONStringType);
  protected
    function GetFields: TFields; virtual; abstract;
    function GetParams: TParams; virtual; abstract;
    function GetSQL: TStrings; virtual; abstract;
    function GetDataSet: TDataSet; virtual; abstract;
    function GetDataBase: TBrookDataBase; virtual; abstract;
    function GetDataSource: TDataSource; virtual; abstract;
    procedure SetDataBase(AValue: TBrookDataBase); virtual; abstract;
    procedure SetDataSource(AValue: TDataSource); virtual; abstract;
    procedure CheckJSONParam(AJSON: TJSONData);
    procedure CheckActive;
  public
    { Creates an instance of a @link(TBrookQuery) class. }
    constructor Init(ADataBase: TBrookDataBase); virtual; abstract;
    { Frees an instance of @link(TBrookQuery) class. }
    destructor Destroy; override;
    { Creates an instance of a @link(TBrookQuery) class. }
    class function Create(ADataBase: TBrookDataBase): TBrookQuery;
    { Initializes the class broker. }
    class procedure InitBrokerClass;
    { Registers the broker class. }
    class function BrokerClass: TBrookQueryClass; virtual;
    { Returns a JSON type according to a field class. }
    class function GetJSONType(const AFieldClass: TFieldClass): ShortString;
    { Returns a JSON type according to a field type. }
    class function GetJSONType(const AField: TField): ShortString;
    { Set the content of a JSON object to fields. }
    class procedure JSONToFields(AJSON: TJSONObject; AFields: TFields;
      const ADateAsString: Boolean);
    { Set the content of fields to a JSON object. }
    class procedure FieldsToJSON(AFields: TFields; AJSON: TJSONObject;
      const ADateAsString: Boolean);
    { Set the content of a dataset to a JSON array. }
    class procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
      const ADateAsString: Boolean);
    { Set the content of a JSON object to params. }
    class procedure JSONToParams(AJSON: TJSONObject; AParams: TParams;
      AFieldDefs: TFieldDefs; const ADateAsString, AAutoCreateParams: Boolean);
    { Set the content of fieldsdefs to a JSON object. }
    class procedure FieldDefsToSchema(AFieldDefs: TFieldDefs;
      ASchema: TJSONObject); overload;
    { Set the content of fieldsdefs to a JSON string. }
    class procedure FieldDefsToSchema(AFieldDefs: TFieldDefs;
      out ASchema: TJSONStringType); overload;
    { Get the attributes of a JSON object and converts into a variant. }
    class procedure GetJSONAttributes(AJSON: TJSONObject; out ANames: string;
      out AValues: Variant; const ADelimiter: Char = SC);
    { Returns a JSON object with the query columns. }
    procedure GetSchema(out ASchema: TJSONObject); overload;
    { Returns a JSON string with the query columns. }
    function GetSchema: TJSONStringType; overload;
    { Locates a register from a JSON object. }
    class function Locate(ADataSet: TDataSet; const AJSON: TJSONObject;
      const AOptions: TLocateOptions): Boolean; overload;
    { Get all the rows of a query in a JSON array. }
    function GetRows(out AJSON: TJSONArray): TBrookQuery;
    { Get the current register in a JSON object. }
    function GetRow(out AJSON: TJSONObject): TBrookQuery;
    { Inserts registers from a JSON array. }
    function SetRows(AJSON: TJSONArray): TBrookQuery;
    { Inserts one register from a JSON object. }
    function SetRow(AJSON: TJSONObject): TBrookQuery;
    { Binds a JSON object to the parameters. }
    function Bind(AJSON: TJSONObject): TBrookQuery;
    { Applies all the changes stored in the buffer. }
    function ApplyUpdates: TBrookQuery; virtual; abstract;
    { Cancels all the updates stored in the buffer. }
    function CancelUpdates: TBrookQuery; virtual; abstract;
    { Applies the updates stored in the buffer and commits the transaction. }
    function Apply(const ARetaining: Boolean = False): TBrookQuery; virtual; abstract;
    { Undoes the updates stored in the buffer and rollbacks the transaction. }
    function Undo(const ARetaining: Boolean = False): TBrookQuery; virtual; abstract;
    { Commits the transaction. }
    function Commit(const ARetaining: Boolean = False): TBrookQuery; virtual; abstract;
    { Rollbacks the transaction. }
    function Rollback(const ARetaining: Boolean = False): TBrookQuery; virtual; abstract;
    { Adds a JSON object to the end of registers. }
    function Append(AJSON: TJSONObject): TBrookQuery;
    { Inserts a JSON object in the current position. }
    function Insert(AJSON: TJSONObject): TBrookQuery;
    { Edits the current register by means of a JSON object. }
    function Edit(AJSON: TJSONObject): TBrookQuery;
    { Cancels editions in the query. }
    function Cancel: TBrookQuery;
    { Deletes the current register. }
    function Delete: TBrookQuery;
    { Opens the query. }
    function Open: TBrookQuery;
    { Closes the query. }
    function Close: TBrookQuery;
    { Refreshes the query. }
    function Refresh: TBrookQuery;
    { Goes to the query first register. }
    function First: TBrookQuery;
    { Goes to the query previous register. }
    function Prior: TBrookQuery;
    { Goes to the query next register. }
    function Next: TBrookQuery;
    { Goes to the query last register. }
    function Last: TBrookQuery;
    { Applies editions to the query. }
    function Post: TBrookQuery;
    { Executes the query. }
    function Execute: TBrookQuery; virtual; abstract;
    { Get the number of changed registers.}
    function RowsAffected: TRowsCount; virtual; abstract;
    { Locates a register by means of JSON object. }
    function Locate(const AJSON: TJSONObject;
      const AOptions: TLocateOptions = []): Boolean; overload;
    { Locates a register passing a key and a value. }
    function Locate(const AKeyFields: string; const AKeyValues: Variant;
      const AOptions: TLocateOptions = []): Boolean; overload;
    { Get a query parameter. }
    function Param(const AName: string): TParam; virtual; abstract;
    { Get a query field. }
    function Field(const AName: string): TField; virtual; abstract;
    { Get a query fielddef. }
    function FieldDef(const AName: string): TFieldDef; virtual; abstract;
    { Get the number of registers. }
    function Count: Int64;
    { Get the position of the current register. }
    function Position: Int64;
    { Creates a bookmark. }
    function GetBookmark: TBookmark;
    { Goes to a bookmark. }
    procedure GotoBookmark(ABookmark: TBookmark);
    { Is the SQL statement. }
    property SQL: TStrings read GetSQL;
    { Are the query fields. }
    property Fields: TFields read GetFields;
    { Are the query parameters. }
    property Params: TParams read GetParams;
    { Checks if the cursor is at the first register. }
    property BOF: Boolean read GetBOF;
    { Checks if the cursor is at the last register. }
    property EOF: Boolean read GetEOF;
    { Returns or creates a bookmark. }
    property Bookmark: TBookmark read GetBookmark write GotoBookmark;
    { Returns @code(True) if the content of the query has changed. }
    property Modified: Boolean read GetModified;
    { Get the current query state. }
    property State: TDataSetState read GetState;
    { Checks if the query is active. }
    property Active: Boolean read GetActive write SetActive;
    { Get the dataset of the query. }
    property DataSet: TDataSet read GetDataSet;
    { Get the fieldefs of the query. }
    property FieldDefs: TFieldDefs read GetFieldDefs;
    { Get or set a master query datasource. }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    { Get or set a query database. }
    property DataBase: TBrookDataBase read GetDataBase write SetDataBase;
    { Enables the date saving as string. }
    property DateAsString: Boolean read FDateAsString write FDateAsString;
    { Returns a JSON string with the query registers. }
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    { Returns a JSON array with the query registers. }
    property Rows: TJSONArray read GetRows;
    { Returns the current register in a JSON object. }
    property Row: TJSONObject read GetRow;
  end;

implementation

var
  _BrookQueryBrokerClass: TBrookQueryClass = nil;

class function TBrookQuery.Create(ADataBase: TBrookDataBase): TBrookQuery;
begin
  Result := TBrookQuery.BrokerClass.Init(ADataBase);
  if Assigned(ADataBase) then
    ADataBase.AddObject(Result);
end;

destructor TBrookQuery.Destroy;
begin
  DataSet.Free;
  inherited Destroy;
end;

class procedure TBrookQuery.InitBrokerClass;
begin
  _BrookQueryBrokerClass := Self;
end;

class function TBrookQuery.GetJSONType(
  const AFieldClass: TFieldClass): ShortString;
begin
  Result := BROOK_FT_NULL;
  if (AFieldClass = TStringField) or (AFieldClass = TBinaryField) or
    (AFieldClass = TBlobField) or (AFieldClass = TVariantField) then
    Result := BROOK_FT_STRING;
  if (AFieldClass = TLongintField) or (AFieldClass = TLargeintField) then
    Result := BROOK_FT_INT;
  if (AFieldClass = TFloatField) or (AFieldClass = TBCDField) or
    (AFieldClass = TFMTBCDField) then
    Result := BROOK_FT_FLOAT;
  if AFieldClass = TBooleanField then
    Result := BROOK_FT_BOOLEAN;
  if AFieldClass = TDateTimeField then
    Result := BROOK_FT_DATE;
end;

class function TBrookQuery.GetJSONType(const AField: TField): ShortString;
begin
  Result := BROOK_FT_NULL;
  if (AField is TStringField) or (AField is TBinaryField) or
    (AField is TBlobField) or (AField is TVariantField) then
    Result := BROOK_FT_STRING;
  if (AField is TLongintField) or (AField is TLargeintField) then
    Result := BROOK_FT_INT;
  if (AField is TFloatField) or (AField is TBCDField) or
    (AField is TFMTBCDField) then
    Result := BROOK_FT_FLOAT;
  if AField is TBooleanField then
    Result := BROOK_FT_BOOLEAN;
  if AField is TDateTimeField then
    Result := BROOK_FT_DATE;
end;

class procedure TBrookQuery.JSONToFields(AJSON: TJSONObject; AFields: TFields;
  const ADateAsString: Boolean);
var
  I, J: Integer;
  VName: string;
  VField: TField;
  VData: TJSONData;
begin
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    for J := 0 to Pred(AFields.Count) do
    begin
      VField := AFields[J];
      if CompareText(VName, VField.FieldName) = 0 then
        Break;
      VField := nil;
    end;
    if not Assigned(VField) then
      Continue;
    if not VField.Visible then
      Continue;
    VData := AJSON.Items[I];
    VField.Clear;
    if VData.IsNull then
      Exit;
    if (VField is TStringField) or (VField is TBinaryField) or
      (VField is TBlobField) or (VField is TVariantField) then
      VField.AsString := VData.AsString;
    if (VField is TLongintField) or (VField is TLargeintField) then
      VField.AsInteger := VData.AsInteger;
    if (VField is TFloatField) or (VField is TBCDField) or
      (VField is TFMTBCDField) then
      VField.AsFloat := VData.AsFloat;
    if VField is TBooleanField then
      VField.AsBoolean := VData.AsBoolean;
    if VField is TDateTimeField then
      if ADateAsString then
        VField.AsDateTime := StrToDateTime(VData.AsString)
      else
        VField.AsDateTime := VData.AsFloat;
  end;
end;

class procedure TBrookQuery.FieldsToJSON(AFields: TFields; AJSON: TJSONObject;
  const ADateAsString: Boolean);
var
  I: Integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
begin
  for I := 0 to Pred(AFields.Count) do
  begin
    VField := AFields[I];
    if not VField.Visible then
      Continue;
    VFieldType := TBrookQuery.GetJSONType(VField);
    VFieldName := VField.FieldName;
    if (VFieldType = BROOK_FT_NULL) or VField.IsNull then
    begin
      AJSON.Add(VFieldName);
      Continue;
    end;
    if VFieldType = BROOK_FT_STRING then
      AJSON.Add(VFieldName, VField.AsString);
    if VFieldType = BROOK_FT_BOOLEAN then
      AJSON.Add(VFieldName, VField.AsBoolean);
    if VFieldType = BROOK_FT_DATE then
      if ADateAsString then
        AJSON.Add(VFieldName, VField.AsString)
      else
        AJSON.Add(VFieldName, VField.AsFloat);
    if VFieldType = BROOK_FT_FLOAT then
      AJSON.Add(VFieldName, VField.AsFloat);
    if VFieldType = BROOK_FT_INT then
      AJSON.Add(VFieldName, VField.AsInteger);
  end;
end;

class procedure TBrookQuery.DataSetToJSON(ADataSet: TDataSet;
  AJSON: TJSONArray; const ADateAsString: Boolean);
var
  VJSON: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VJSON := TJSONObject.Create;
    TBrookQuery.FieldsToJSON(ADataSet.Fields, VJSON, ADateAsString);
    AJSON.Add(VJSON);
    ADataSet.Next;
  end;
end;

class procedure TBrookQuery.JSONToParams(AJSON: TJSONObject; AParams: TParams;
  AFieldDefs: TFieldDefs; const ADateAsString, AAutoCreateParams: Boolean);
var
  I, J: Integer;
  VName: string;
  VParam: TParam;
  VData: TJSONData;
  VFieldClass: TFieldClass;
begin
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    for J := 0 to Pred(AParams.Count) do
    begin
      VParam := AParams[J];
      if CompareText(VName, VParam.Name) = 0 then
        Break;
      VParam := nil;
    end;
    if not Assigned(VParam) then
      if AAutoCreateParams then
      begin
        VParam := TParam.Create(AParams);
        VParam.Name := VName;
      end
      else
        Continue;
    VData := AJSON.Items[I];
    VParam.Clear;
    if VData.IsNull then
      Exit;
    VFieldClass := AFieldDefs.Find(VName).FieldClass;
    if (VFieldClass = TStringField) or (VFieldClass = TBinaryField) or
      (VFieldClass = TBlobField) or (VFieldClass = TVariantField) then
      VParam.AsString := VData.AsString;
    if (VFieldClass = TLongintField) or (VFieldClass = TLargeintField) then
      VParam.AsInteger := VData.AsInteger;
    if (VFieldClass = TFloatField) or (VFieldClass = TBCDField) or
      (VFieldClass = TFMTBCDField) then
      VParam.AsFloat := VData.AsFloat;
    if VFieldClass = TBooleanField then
      VParam.AsBoolean := VData.AsBoolean;
    if VFieldClass = TDateTimeField then
      if ADateAsString then
        VParam.AsDateTime := StrToDateTime(VData.AsString)
      else
        VParam.AsDateTime := VData.AsFloat;
  end;
end;

class procedure TBrookQuery.FieldDefsToSchema(AFieldDefs: TFieldDefs;
  ASchema: TJSONObject);
var
  I: Integer;
  VArray: TJSONArray;
  VObject: TJSONObject;
  VFieldDef: TFieldDef;
  VFieldType: ShortString;
begin
  VArray := TJSONArray.Create;
  ASchema.Add('fields', VArray);
  for I := 0 to Pred(AFieldDefs.Count) do
  begin
    VFieldDef := AFieldDefs[I];
    VObject := TJSONObject.Create(['name', VFieldDef.Name]);
    VArray.Add(VObject);
    VFieldType := TBrookQuery.GetJSONType(VFieldDef.FieldClass);
    VObject.Strings['type'] := VFieldType;
    if VFieldType = BROOK_FT_STRING then
      VObject.Integers['maxlen'] := VFieldDef.Size;
    if VFieldDef.Required then
      VObject.Booleans['required'] := True;
    if VFieldDef.Precision <> -1 then
      VObject.Integers['precision'] := VFieldDef.Precision;
    if faHiddenCol in VFieldDef.Attributes then
      VObject.Booleans['hidden'] := True;
  end;
end;

class procedure TBrookQuery.FieldDefsToSchema(AFieldDefs: TFieldDefs; out
  ASchema: TJSONStringType);
var
  I, C: Integer;
  VFieldDef: TFieldDef;
  VFieldType: ShortString;
begin
  C := AFieldDefs.Count;
  if C = 0 then
  begin
    ASchema := '{}';
    Exit;
  end;
  for I := 0 to Pred(C) do
  begin
    VFieldDef := AFieldDefs[I];
    VFieldType := TBrookQuery.GetJSONType(VFieldDef.FieldClass);
    ASchema += '{ "name": "' + VFieldDef.Name + '"';
    ASchema += ', "type": "' + VFieldType + '"';
    if VFieldType = BROOK_FT_STRING then
      ASchema += ', "maxlen": ' + IntToStr(VFieldDef.Size);
    if VFieldDef.Required then
      ASchema += ', "required": true';
    if VFieldDef.Precision <> -1 then
      ASchema += ', "precision": ' + IntToStr(VFieldDef.Precision);
    if faHiddenCol in VFieldDef.Attributes then
      ASchema += ', "hidden": true';
    ASchema += ' }, ';
  end;
  SetLength(ASchema, Length(ASchema) - 2);
end;

class procedure TBrookQuery.GetJSONAttributes(AJSON: TJSONObject; out
  ANames: string; out AValues: Variant; const ADelimiter: Char);
var
  I, C: Integer;
begin
  ANames := ES;
  C := AJSON.Count;
  AValues := VarArrayCreate([0, Pred(C)], varVariant);
  for I := 0 to Pred(C) do
  begin
    ANames += AJSON.Names[I] + ADelimiter;
    AValues[I] := AJSON.Items[I].Value;
  end;
  SetLength(ANames, Length(ANames) - 1);
end;

procedure TBrookQuery.GetSchema(out ASchema: TJSONObject);
begin
  ASchema := TJSONObject.Create;
  TBrookQuery.FieldDefsToSchema(DataSet.FieldDefs, ASchema);
end;

function TBrookQuery.GetSchema: TJSONStringType;
begin
  TBrookQuery.FieldDefsToSchema(DataSet.FieldDefs, Result);
end;

function TBrookQuery.GetAsJSON: TJSONStringType;
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

function TBrookQuery.GetActive: Boolean;
begin
  Result := DataSet.Active;
end;

function TBrookQuery.GetBOF: Boolean;
begin
  Result := DataSet.BOF;
end;

function TBrookQuery.GetBookmark: TBookmark;
begin
  Result := DataSet.GetBookmark;
end;

procedure TBrookQuery.GotoBookmark(ABookmark: TBookmark);
begin
  DataSet.GotoBookmark(ABookmark);
end;

function TBrookQuery.GetEOF: Boolean;
begin
  Result := DataSet.EOF;
end;

function TBrookQuery.GetFieldDefs: TFieldDefs;
begin
  Result := DataSet.FieldDefs;
end;

function TBrookQuery.GetModified: Boolean;
begin
  Result := DataSet.Modified;
end;

function TBrookQuery.GetRow: TJSONObject;
begin
  GetRow(Result);
end;

function TBrookQuery.GetRows: TJSONArray;
begin
  GetRows(Result);
end;

function TBrookQuery.GetState: TDataSetState;
begin
  Result := DataSet.State;
end;

procedure TBrookQuery.SetActive(AValue: Boolean);
begin
  DataSet.Active := AValue;
end;

procedure TBrookQuery.SetAsJSON(AValue: TJSONStringType);
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

procedure TBrookQuery.CheckJSONParam(AJSON: TJSONData);
begin
  if not Assigned(AJSON) then
    raise EBrookQuery.Create(Self, SBrookNilJSONParamError);
end;

procedure TBrookQuery.CheckActive;
begin
  if not DataSet.Active then
    DataSet.Open;
end;

class function TBrookQuery.BrokerClass: TBrookQueryClass;
begin
  if not Assigned(_BrookQueryBrokerClass) then
    raise EBrookQuery.Create(Self, SBrookNoQueryBrokerClassRegisteredError);
  Result := _BrookQueryBrokerClass;
end;

function TBrookQuery.GetRows(out AJSON: TJSONArray): TBrookQuery;
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

function TBrookQuery.GetRow(out AJSON: TJSONObject): TBrookQuery;
begin
  Result := Self;
  AJSON := TJSONObject.Create;
  CheckActive;
  if DataSet.RecordCount <> 0 then
    TBrookQuery.FieldsToJSON(DataSet.Fields, AJSON, FDateAsString);
end;

function TBrookQuery.SetRows(AJSON: TJSONArray): TBrookQuery;
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

function TBrookQuery.SetRow(AJSON: TJSONObject): TBrookQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookQuery.Bind(AJSON: TJSONObject): TBrookQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TBrookQuery.JSONToParams(AJSON, Params, DataSet.FieldDefs,
    FDateAsString, False);
end;

function TBrookQuery.Append(AJSON: TJSONObject): TBrookQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  DataSet.Append;
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookQuery.Insert(AJSON: TJSONObject): TBrookQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  DataSet.Insert;
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookQuery.Edit(AJSON: TJSONObject): TBrookQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  DataSet.Edit;
  TBrookQuery.JSONToFields(AJSON, DataSet.Fields, FDateAsString);
end;

function TBrookQuery.Cancel: TBrookQuery;
begin
  Result := Self;
  DataSet.Cancel;
end;

function TBrookQuery.Delete: TBrookQuery;
begin
  Result := Self;
  DataSet.Delete;
end;

function TBrookQuery.Open: TBrookQuery;
begin
  Result := Self;
  DataSet.Open;
end;

function TBrookQuery.Close: TBrookQuery;
begin
  Result := Self;
  DataSet.Close;
end;

function TBrookQuery.Refresh: TBrookQuery;
begin
  Result := Self;
  DataSet.Refresh;
end;

function TBrookQuery.First: TBrookQuery;
begin
  Result := Self;
  DataSet.First;
end;

function TBrookQuery.Prior: TBrookQuery;
begin
  Result := Self;
  DataSet.Prior;
end;

function TBrookQuery.Next: TBrookQuery;
begin
  Result := Self;
  DataSet.Next;
end;

function TBrookQuery.Last: TBrookQuery;
begin
  Result := Self;
  DataSet.Last;
end;

function TBrookQuery.Post: TBrookQuery;
begin
  Result := Self;
  DataSet.Post;
end;

function TBrookQuery.Locate(const AJSON: TJSONObject;
  const AOptions: TLocateOptions): Boolean;
begin
  CheckJSONParam(AJSON);
  Result := TBrookQuery.Locate(DataSet, AJSON, AOptions);
end;

function TBrookQuery.Locate(const AKeyFields: string;
  const AKeyValues: Variant; const AOptions: TLocateOptions): Boolean;
begin
  Result := DataSet.Locate(AKeyFields, AKeyValues, AOptions);
end;

function TBrookQuery.Count: Int64;
begin
  Result := DataSet.RecordCount;
end;

function TBrookQuery.Position: Int64;
begin
  Result := DataSet.RecNo;
end;

class function TBrookQuery.Locate(ADataSet: TDataSet; const AJSON: TJSONObject;
  const AOptions: TLocateOptions): Boolean;
var
  VKeyFields: string;
  VKeyValues: Variant;
begin
  TBrookQuery.GetJSONAttributes(AJSON, VKeyFields, VKeyValues);
  Result := ADataSet.Locate(VKeyFields, VKeyValues, AOptions);
end;

end.
