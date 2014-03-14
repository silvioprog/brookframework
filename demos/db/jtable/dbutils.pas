unit dbutils;

{$mode objfpc}{$H+}

interface

uses
  dSQLdbBroker, dUtils, PQConnection, SysUtils;

function con: TdSQLdbConnector;
function pgNextSeq(const ASeqName: string): Int64;
function pgCount(const ATableName: string): Int64; overload;
function pgCount(const ATableName, AWhere: string; AParams: TObject): Int64; overload;

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

function pgCount(const ATableName, AWhere: string; AParams: TObject): Int64;
var
  q: TdSQLdbQuery;
begin
  q := TdSQLdbQuery.Create(con);
  try
    q.SQL.Add('select count(*) from ' + ATableName);
    if AWhere <> '' then
    begin
      q.SQL.Add('where ' + AWhere);
      dUtils.dSetParams(AParams, q.Params);
    end;
    q.Open;
    Result := q.Fields[0].AsInteger;
  finally
    q.Free;
  end;
end;

finalization
  FreeAndNil(_con);

end.

