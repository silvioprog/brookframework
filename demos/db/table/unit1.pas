unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookQuery, BrookTable, BrookDataBase, FPJSON, SysUtils;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
var
  t: TBrookTable;
  q: TBrookQuery;
  o: TJSONObject;
  d: TBrookDataBase;
begin
  d := TBrookDataBase.Create;
  q := TBrookQuery.Create(d);

  q.SQL.Text := 'drop table t1';
  q.Execute;
  q.SQL.Clear;
  q.SQL.Add('create table t1 (');
  q.SQL.Add(' id integer constraint pk_t1 primary key autoincrement not null,');
  q.SQL.Add(' dummy varchar(50) not null);');
  q.Execute.Commit;

  t := TBrookTable.Create(d, 't1');

  WriteLn('Appending records ...');
  o := TJSONObject.Create(['dummy', 'Dummy string 1']);
  t.Append(o);
  WriteLn(o);
  FreeAndNil(o);
  o := TJSONObject.Create(['dummy', 'Dummy string 2']);
  t.Append(o).Apply;
  WriteLn(o);
  FreeAndNil(o);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Showing inserted records ...');
  WriteLn(t.AsJSON);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Editing records ...');
  o := TJSONObject.Create(['id', 1, 'dummy', 'Dummy string 1 - Edited']);
  t.Locate('id', 1);
  t.Edit(o);
  WriteLn(o);
  FreeAndNil(o);
  o := TJSONObject.Create(['id', 2, 'dummy', 'Dummy string 2 - Edited']);
  t.Locate('id', 2);
  t.Edit(o).Apply;
  WriteLn(o);
  FreeAndNil(o);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Showing edited records ...');
  WriteLn(t.AsJSON);
  WriteLn('Done.');

  WriteLn;

  WriteLn('Deleting records ...');
  t.First;
  while not t.EOF do
    t.Delete;
  t.Apply;
  WriteLn('Done.');

  WriteLn;

  WriteLn('Final result data:');
  WriteLn(t.AsJSON);
end;

initialization
  TMyAction.Register('*');

end.
