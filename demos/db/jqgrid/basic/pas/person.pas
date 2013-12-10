unit Person;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookConsts, BrookDBConsts, SysUtils, Math, FPJSON, DB;

type

  { TPersonAction }

  TPersonAction = class(TBrookDBAction)
  public
    procedure Post; override;
    procedure Get; override;
  end;

implementation

procedure FieldsToJSON(AFields: TFields; AJSON: TJSONArray);
var
  I: Integer;
  VField: TField;
begin
  for I := 0 to Pred(AFields.Count) do
  begin
    VField := AFields[I];
    if not VField.Visible then
      Continue;
    if VField.IsNull then
      AJSON.Add
    else
      AJSON.Add(VField.AsString);
  end;
end;

function SearchOps(const AOps: string): string;
begin
  case aops of
    'eq': Result := '='; // equal
    'ne': Result := '<>'; // not equal
    'lt': Result := '<'; // less than
    'le': Result := '<='; // less than or equal
    'gt': Result := '>'; // greater than
    'ge': Result := '>='; // greater than or equal
    'bw': Result := 'like'; // begins with
    'bn': Result := 'not like'; // doesn't begin with
    'in': Result := 'like'; // is in
    'ni': Result := 'not like'; // is not in
    'ew': Result := 'like'; // ends with
    'en': Result := 'not like'; // doesn't end with
    'cn': Result := 'like'; // contains
    'nc': Result := 'not like'; //doesn't contain
  end;
end;

function GetWhereClause(ACol, AOper, AVal: string): string;
begin
  case AOper of
    'bw', 'bn': AVal += PT;
    'ew', 'en': AVal := PT + AVal;
    'cn', 'nc', 'in', 'ni': AVal := PT + AVal + PT;
  end;
  Result := ACol + SP + SearchOps(AOper) + SP + QuotedStr(AVal);
end;

{ TPersonAction }

procedure TPersonAction.Post;
var
  VId: Int64;
  VOper: string;
begin
  VOper := Fields['oper'].AsString;
  Fields.Delete('oper');
  case VOper of
    'add':
      begin
        Fields.Delete(BROOK_DEFAULT_KEY_NAME);
        Table.Insert(Fields).Apply;
      end;
    'edit', 'del':
      begin
        VId := Fields[BROOK_DEFAULT_KEY_NAME].AsInt64;
        if Table.Open.Locate(BROOK_DEFAULT_KEY_NAME, VId) then
        begin
          Fields.Delete(BROOK_DEFAULT_KEY_NAME);
          if VOper = 'edit' then
            Table.Edit(Fields).Apply
          else
            Table.Delete.Apply;
        end;
      end;
  end;
end;

procedure TPersonAction.Get;
var
  VOrderBy: string;
  VData: TJSONObject;
  VRecord, VRecords: TJSONArray;
  Vsidx, Vsord, VRows, VPage, VCount, VTotal: Integer;
begin
  VData := TJSONObject.Create;
  VRecord := TJSONArray.Create;
  VRecords := TJSONArray.Create;
  try
    VCount := Table.Select('count(*) as count').Open.Field('count').AsInteger;
    VRows := Params['rows'].AsInteger;
    VPage := Params['page'].AsInteger;
    if VCount > 0 then
      VTotal := Ceil(VCount / VRows)
    else
      VTotal := 0;
    VData.Add('records', VCount);
    Vsidx := Params.IndexOfName('sidx');
    if Vsidx > 0 then
    begin
      VOrderBy := Params.Items[Vsidx].AsString;
      Vsord := Params.IndexOfName('sord');
      if (Vsord > 0) and (VOrderBy <> ES) then
        VOrderBy += SP + Params.Items[Vsord].AsString
      else
        VOrderBy := BROOK_DEFAULT_KEY_NAME;
    end
    else
      VOrderBy := BROOK_DEFAULT_KEY_NAME;
    Table.Clear.OrderBy(VOrderBy + ' limit :rows offset :offset');
    if Params['_search'].AsBoolean then
      Table.Where(getWhereClause(Params['searchField'].AsString,
        Params['searchOper'].AsString, Params['searchString'].AsString));
    if Values.Count > 0 then
      Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values)
    else
      Table.Prepare;
    Table.Param('rows').AsInteger := VRows;
    Table.Param('offset').AsInteger := (VPage - 1) * VRows;
    VData.Add('page', VPage);
    VData.Add('total', VTotal);
    Table.Open.First;
    while not Table.EOF do
    begin
      VRecord.Clear;
      FieldsToJSON(Table.Fields, VRecord);
      VRecords.Add(TJSONObject.Create([BROOK_DEFAULT_KEY_NAME,
        Table.Field(BROOK_DEFAULT_KEY_NAME).AsString, 'cell', VRecord.Clone]));
      Table.Next;
    end;
    VData.Add('rows', VRecords.Clone);
    Write(VData.AsJSON);
  finally
    VRecord.Free;
    VRecords.Free;
    VData.Free;
  end;
end;

initialization
  TPersonAction.Register('person', '/person');

end.
