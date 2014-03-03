unit dbutils;

{$mode objfpc}{$H+}

interface

uses
  dSQLdbBroker, PQConnection, SysUtils, TypInfo, FPJSON;

function con: TdSQLdbConnector;
function pgNextSeq(const ASeqName: string): Int64;
function pgCount(const ATableName: string): Int64;
procedure objToJSON(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AJson: TJSONObject);

implementation

var
  _con: TdSQLdbConnector = nil;

function con: TdSQLdbConnector;
begin
  if not Assigned(_con) then
  begin
    _con := TdSQLdbConnector.Create(nil);
    _con.Logger.Active := True;
    _con.Logger.FileName := 'OUTPUT.LOG';
    _con.Driver := 'postgresql';
    _con.Host := '127.0.0.1';
    _con.Database := 'postgres';
    _con.User := 'postgres';
    _con.Password := 'postgres';
  end;
  Result := _con;
end;

function pgNextSeq(const ASeqName: string): Int64;
var
  q: TdSQLdbQuery;
begin
  q := TdSQLdbQuery.Create(con);
  try
    q.SQL.Text := 'select nextval(''' + ASeqName + ''')';
    q.Open;
    Result := q.Fields[0].AsInteger;
  finally
    q.Free;
  end;
end;

function pgCount(const ATableName: string): Int64;
var
  q: TdSQLdbQuery;
begin
  q := TdSQLdbQuery.Create(con);
  try
    q.SQL.Text := 'select count(*) from ' + ATableName;
    q.Open;
    Result := q.Fields[0].AsInteger;
  finally
    q.Free;
  end;
end;

procedure objToJSON(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AJson: TJSONObject);
var
  I: Integer;
  PI: PPropInfo;
begin
  if not Assigned(APropList) then
    raise Exception.Create('APropList must not be nil.');
  if not Assigned(AObject) then
    raise Exception.Create('AObject must not be nil.');
  if not Assigned(AJson) then
    raise Exception.Create('AJson must not be nil.');
  for I := 0 to Pred(APropCount) do
  begin
    PI := APropList^[I];
    case PI^.PropType^.Kind of
      tkAString: AJson.Add(PI^.Name, GetStrProp(AObject, PI));
      tkChar: AJson.Add(PI^.Name, Char(GetOrdProp(AObject, PI)));
      tkInteger: AJson.Add(PI^.Name, GetOrdProp(AObject, PI));
      tkInt64, tkQWord: AJson.Add(PI^.Name, GetInt64Prop(AObject, PI));
      tkBool: AJson.Add(PI^.Name, GetOrdProp(AObject, PI) <> 0);
      tkFloat: AJson.Add(PI^.Name, GetFloatProp(AObject, PI));
      tkEnumeration: AJson.Add(PI^.Name, GetEnumProp(AObject, PI));
      tkSet: AJson.Add(PI^.Name, GetSetProp(AObject, PI, False));
    end;
  end;
end;

finalization
  FreeAndNil(_con);

end.

