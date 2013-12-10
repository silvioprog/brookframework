unit jqGridRESTActions;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, BrookRESTActions, BrookJSONHelper, BrookConsts,
  BrookHTTPConsts, BrookDBConsts, Classes, SysUtils, HTTPDefs, Math, FPJSON;

type

  { TjqGridRetrieveAction }

  TjqGridRetrieveAction = class(TBrookRetrieveAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

  { TjqGridCreateAction }

  TjqGridCreateAction = class(TBrookCreateAction)
  private
    FId: Int64;
  protected
    procedure InternalInsert; override;
    procedure CheckId; virtual;
  public
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    property Id: Int64 read FId write FId;
  end;

  { TjqGridUpdateAction }

  TjqGridUpdateAction = class(TBrookUpdateAction)
  public
    procedure Request(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TjqGridDestroyAction }

  TjqGridDestroyAction = class(TBrookDestroyAction)
  end;

implementation

procedure RemoveField(AFields: TStrings; const AName: string);
var
  VIdx: Integer;
begin
  VIdx := AFields.IndexOfName(AName);
  if VIdx > 0 then
    AFields.Delete(VIdx);
end;

function GetConditions(AJSON: TJSONObject): string;
var
  I: Integer;
  VName: string;
begin
  Result := ES;
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    if AJSON.Items[I].IsNull then
      Result += VName + SP + EQ + SP + NS + SP + BROOK_SQL_AND_TOKEN + SP
    else
      Result += VName + BROOK_SQL_EQ_PARAM_TOKEN + VName + SP +
        BROOK_SQL_AND_TOKEN + SP;
  end;
  SetLength(Result, Length(Result) - Length(SP + BROOK_SQL_AND_TOKEN + SP));
end;

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

{ TjqGridRetrieveAction }

procedure TjqGridRetrieveAction.Request(ARequest: TRequest; AResponse: TResponse);
var
  VOrderBy: string;
  VData: TJSONObject;
  VRows, VPage, VCount, VTotal: Integer;
begin
  if Params.Exists('rows') and Params.Exists('page') then
  begin
    VData := TJSONObject.Create;
    try
      VCount := Table.Select('count(*) as count').Open.Field('count').AsInteger;
      VRows := Params['rows'].AsInteger;
      VPage := Params['page'].AsInteger;
      if VCount > 0 then
      	VTotal := Ceil(VCount / VRows)
      else
      	VTotal := 0;
      VData.Add('records', VCount);
      if Params.Exists('sidx') then
      begin
        VOrderBy := Params['sidx'].AsString;
        if Params.Exists('sord') and (VOrderBy <> '') then
          VOrderBy += SP + Params['sord'].AsString
        else
          VOrderBy := BROOK_DEFAULT_KEY_NAME;
      end
      else
        VOrderBy := BROOK_DEFAULT_KEY_NAME;
      Table.Clear.OrderBy(VOrderBy + ' limit :rows offset :offset');
      if Values.Count > 0 then
        Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values)
      else
        Table.Prepare;
      Table.Param('rows').AsInteger := VRows;
      Table.Param('offset').AsInteger := (VPage - 1) * VRows;
      VData.Add('page', VPage);
      VData.Add('total', VTotal);
      VData.Add('rows', Table.Rows);
      Write(VData.AsJSON);
    finally
      VData.Free;
    end;
  end;
end;

{ TjqGridCreateAction }

procedure TjqGridCreateAction.InternalInsert;
begin
  CheckId;
  inherited InternalInsert;
end;

procedure TjqGridCreateAction.CheckId;
begin
  FId := PgGetNextVal(Table.DataBase, Table.Name + '_' +
    BROOK_DEFAULT_KEY_NAME + '_seq');
  Fields.Integers[BROOK_DEFAULT_KEY_NAME] := FId;
end;

procedure TjqGridCreateAction.Request(ARequest: TRequest; AResponse: TResponse);
begin
  RemoveField(ARequest.ContentFields, BROOK_DEFAULT_KEY_NAME);
  if Execute then
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_CREATED;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_CREATED;
    AResponse.Content := Format('{ "%s": %d }', [BROOK_DEFAULT_KEY_NAME, FId]);
  end;
end;

{ TjqGridUpdateAction }

procedure TjqGridUpdateAction.Request(ARequest: TRequest; AResponse: TResponse);
begin
  RemoveField(ARequest.ContentFields, BROOK_DEFAULT_KEY_NAME);
  inherited Request(ARequest, AResponse);
end;

end.
