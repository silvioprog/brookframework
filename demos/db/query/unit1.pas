unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookQuery, BrookDataBase, FPJSON, SysUtils;

type
  TTest = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TTest.Get;
var
  q: TBrookQuery;
  o: TJSONObject;
begin
  q := TBrookQuery.Create(TBrookDataBase.Create);

  q.SQL.Text := 'drop table t1';
  q.Execute;
  q.SQL.Clear;
  q.SQL.Add('create table t1 (');
  q.SQL.Add(' id integer constraint pk_t1 primary key autoincrement not null,');
  q.SQL.Add(' dummy varchar(50) not null);');
  q.Execute.Commit;

  q.SQL.Text := 'select * from t1';
  q.Open;

  WriteLn('Appending records ...');
  o := TJSONObject.Create(['dummy', 'Dummy string 1']);
  q.Append(o);
  WriteLn(o);
  FreeAndNil(o);
  o := TJSONObject.Create(['dummy', 'Dummy string 2']);
  q.Append(o).Apply;
  WriteLn(o);
  FreeAndNil(o);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Showing inserted records ...');
  WriteLn(q.AsJSON);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Editing records ...');
  o := TJSONObject.Create(['id', 1, 'dummy', 'Dummy string 1 - Edited']);
  q.Locate('id', 1);
  q.Edit(o);
  WriteLn(o);
  FreeAndNil(o);
  o := TJSONObject.Create(['id', 2, 'dummy', 'Dummy string 2 - Edited']);
  q.Locate('id', 2);
  q.Edit(o).Apply;
  WriteLn(o);
  FreeAndNil(o);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Showing edited records ...');
  WriteLn(q.AsJSON);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Deleting records ...');
  q.First;
  while not q.EOF do
    q.Delete;
  q.Apply;
  WriteLn('Done.');

  WriteLn;

  WriteLn('Final result data:');
  WriteLn(q.AsJSON);
end;

initialization
  TTest.Register('*');

end.
