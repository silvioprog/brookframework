unit testbrookdbaction;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookSQLdbBroker, BrookDBAction, BrookRouter, BrookUtils,
  fpcunit, testregistry, sqlite3conn,Dialogs;

type
  TAction1 = class(TBrookDBAction)
  end;

  TAction2 = class(TAction1)
  end;

  TAction3 = class(TAction1)
  end;

  TTestBrookDBAction = class(TTestCase)
  published
    procedure TestRegister;
    procedure TestSetTableName;
    procedure TestGetTableName;
    procedure TestSetIgnoredFields;
    procedure TestGetIgnoredFields;
  end;

implementation

procedure TTestBrookDBAction.TestRegister;
var
  VAct: TBrookDBAction;
  VRoutes: TBrookRoutes;
  VActClass: TBrookDBActionClass;
begin
  VRoutes := TBrookRouter.Service.Routes;
  TAction1.Register('test', '/action1', rmGet, True);
  TAction2.Register('test', '/action2', rmPost);
  AssertEquals(2, VRoutes.List.Count);
  AssertEquals('TAction1', VRoutes.Items[0]^.ActionClass.ClassName);
  AssertEquals(True, VRoutes.Items[0]^.Default);
  AssertEquals('/action1', VRoutes.Items[0]^.Pattern);
  AssertEquals(True, BrookSettings.Mapped);
  AssertTrue('Invalid method', VRoutes.Items[0]^.Method = rmGet);
  VActClass := TBrookDBActionClass(VRoutes.Items[0]^.ActionClass);
  AssertEquals('test', VActClass.GetTableName);
  TAction3.Register('person', '/action3', 'id,lastupdate');
  AssertEquals('id,lastupdate', TAction3.GetIgnoredFields);
  BrookSettings.Configuration :=
    'library=sqldb;driver=sqlite3;database=testdb2.sqlite3';
  TBrookDataBases.Service.FreeCurrent;
  VAct := TAction3.Create;
  try
    VAct.SetIgnoredFields('name');
    VAct.Table.Open;
    AssertEquals(True, VAct.Table.Field('id').Visible);
    AssertEquals(False, VAct.Table.Field('name').Visible);
  finally
    VAct.Free;
  end;
end;

procedure TTestBrookDBAction.TestSetTableName;
begin
  TAction1.SetTableName('test');
  AssertEquals('test', TAction1.GetTableName);
end;

procedure TTestBrookDBAction.TestGetTableName;
begin
  TAction1.SetTableName('test');
  AssertEquals('test', TAction1.GetTableName);
end;

procedure TTestBrookDBAction.TestSetIgnoredFields;
begin
  TAction1.SetIgnoredFields('id,lastupdate');
  AssertEquals('id,lastupdate', TAction1.GetIgnoredFields);
end;

procedure TTestBrookDBAction.TestGetIgnoredFields;
begin
  TAction1.SetIgnoredFields('id,lastupdate');
  AssertEquals('id,lastupdate', TAction1.GetIgnoredFields);
end;

initialization
  RegisterTest(TTestBrookDBAction);

end.

