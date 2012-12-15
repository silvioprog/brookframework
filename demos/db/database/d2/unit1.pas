unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookDataBase, BrookUtils, BrookSQLdbBroker, BrookZeosBroker,
  SQLite3Conn, SysUtils;

type
  TTest = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TTest.Get;
var
  db: TBrookDataBase;
begin
  BrookSettings.Configuration :=
    'library=sqldb;driver=sqlite3;database=db.sqlite3';
  db := TBrookDataBase.Create;
  db.Connect;
  WriteLn('Connected as %s.', [db.Connection.ClassName]);
  FreeAndNil(db);

  BrookSettings.Configuration :=
    'library=zeos;driver=sqlite-3;database=db.sqlite3';
  db := TBrookDataBase.Create;
  db.Connect;
  WriteLn('Connected as %s.', [db.Connection.ClassName]);
end;

initialization
  TTest.Register('*');

end.
