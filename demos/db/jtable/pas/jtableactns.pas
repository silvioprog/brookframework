unit jTableActns;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, BrookDBAction, BrookDBConsts, FPJSON;

type

  { TjTableCustomAction }

  TjTableCustomAction = class(TBrookDBAction)
  public
    procedure Post; override;
    procedure Posting(AData: TJSONObject); virtual;
    procedure Writting(AData: TJSONObject); virtual;
  end;

  { TjTableListAction }

  TjTableListAction = class(TjTableCustomAction)
  public
    procedure Posting(AData: TJSONObject); override;
  end;

  { TjTableCreateAction }

  TjTableCreateAction = class(TjTableCustomAction)
  public
    procedure Posting(AData: TJSONObject); override;
  end;

  { TjTableUpdateAction }

  TjTableUpdateAction = class(TjTableCustomAction)
  public
    procedure Posting({%H-}AData: TJSONObject); override;
  end;

  { TjTableDeleteAction }

  TjTableDeleteAction = class(TjTableCustomAction)
  public
    procedure Posting({%H-}AData: TJSONObject); override;
  end;

implementation

function PgGetNextVal(ADatabase: TBrookDataBase;
  const ASequenceName: string): Integer;
var
  VQuery: TBrookQuery;
begin
  VQuery := TBrookQuery.Create(nil);
  try
    VQuery.DataBase := ADatabase;
    VQuery.SQL.Text := 'select nextval(''' + ASequenceName + ''')';
    VQuery.Open;
    Result := VQuery.Fields[0].AsInteger;
  finally
    VQuery.Free;
  end;
end;

{ TjTableCustomAction }

procedure TjTableCustomAction.Post;
var
  VData: TJSONObject;
begin
  VData := TJSONObject.Create;
  try
    Posting(VData);
    Writting(VData);
  finally
    VData.Free;
  end;
end;

procedure TjTableCustomAction.Posting(AData: TJSONObject);
begin
  AData.Add('Result', 'OK');
end;

procedure TjTableCustomAction.Writting(AData: TJSONObject);
begin
  Write(AData.AsJSON);
end;

{ TjTableListAction }

procedure TjTableListAction.Posting(AData: TJSONObject);
begin
  inherited Posting(AData);
  Table.Select('count(*) as count').Open;
  AData.Add('TotalRecordCount', Table.Field('count').AsInteger);
  Table.Clear.OrderBy(Params['jtSorting'].AsString +
    ' limit :jtPageSize offset :jtStartIndex');
  if Values.Count > 0 then
    Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values)
  else
    Table.Prepare;
  Table.Param('jtPageSize').AsInteger := Params['jtPageSize'].AsInteger;
  Table.Param('jtStartIndex').AsInteger := Params['jtStartIndex'].AsInteger;
  AData.Add('Records', Table.Open.Rows);
end;

{ TjTableCreateAction }

procedure TjTableCreateAction.Posting(AData: TJSONObject);
begin
  inherited Posting(AData);
  Fields.Int64s[BROOK_DEFAULT_KEY_NAME] := PgGetNextVal(Table.DataBase,
    Table.Name + '_' + BROOK_DEFAULT_KEY_NAME + '_seq');
  Table.Insert(Fields).ApplyUpdates;
  AData.Add('Record', Table.Row);
  Table.Apply;
end;

{ TjTableUpdateAction }

procedure TjTableUpdateAction.Posting(AData: TJSONObject);
begin
  inherited Posting(AData);
  if Table.Open.Locate(BROOK_DEFAULT_KEY_NAME,
    Fields[BROOK_DEFAULT_KEY_NAME].AsInt64) then
    Table.Edit(Fields).Apply
end;

{ TjTableDeleteAction }

procedure TjTableDeleteAction.Posting(AData: TJSONObject);
begin
  inherited Posting(AData);
  if Table.Open.Locate(BROOK_DEFAULT_KEY_NAME,
    Fields[BROOK_DEFAULT_KEY_NAME].AsInt64) then
    Table.Delete.Apply;
end;

end.
