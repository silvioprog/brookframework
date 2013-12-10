unit Person;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, BrookDBAction, BrookUtils, BrookDBConsts, FPJSON;

type

  { TPersonAction }

  TPersonAction = class(TBrookDBAction)
  public
    procedure Post; override;
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

{ TPersonAction }

procedure TPersonAction.Post;
var
  VId: Int64;
  VAction: string;
  VData: TJSONObject;
begin
  VData := TJSONObject.Create;
  try
    VData.Add('Result', 'OK');
    VAction := Params['action'].AsString;
    case VAction of
      'list':
        begin
          Table.Select('count(*) as count').Open;
          VData.Add('TotalRecordCount', Table.Field('count').AsInteger);
          Table.Clear.OrderBy(Params['jtSorting'].AsString +
            ' limit :jtPageSize offset :jtStartIndex');
          if Values.Count > 0 then
            Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values)
          else
            Table.Prepare;
          Table.Param('jtPageSize').AsInteger := Params['jtPageSize'].AsInteger;
          Table.Param('jtStartIndex').AsInteger := Params['jtStartIndex'].AsInteger;
          VData.Add('Records', Table.Open.Rows);
        end;
      'create':
        begin
          VData.Add('Result', 'OK');
          Fields.Int64s[BROOK_DEFAULT_KEY_NAME] := PgGetNextVal(Table.DataBase,
            Table.Name + '_' + BROOK_DEFAULT_KEY_NAME + '_seq');
          Table.Insert(Fields).Apply;
          VData.Add('Record', Table.Row);
        end;
      'update', 'delete':
        begin
          VId := Fields[BROOK_DEFAULT_KEY_NAME].AsInt64;
          if Table.Open.Locate(BROOK_DEFAULT_KEY_NAME, VId) then
          begin
            if VAction = 'update' then
              Table.Edit(Fields).Apply
            else
              Table.Delete.Apply;
          end;
        end;
    end;
    Write(VData.AsJSON);
  finally
    VData.Free;
  end;
end;

initialization
  TPersonAction.Register('person', '/person', rmPost);

end.
