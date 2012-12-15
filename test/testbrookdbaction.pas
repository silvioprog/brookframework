unit testbrookdbaction;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookRouter, BrookUtils, fpcunit, testregistry;

type
  TAction1 = class(TBrookDBAction)
  end;

  TAction2 = class(TAction1)
  end;

  TTestBrookDBAction = class(TTestCase)
  published
    procedure TestRegister;
    procedure TestSetTableName;
    procedure TestGetTableName;
  end;

implementation

procedure TTestBrookDBAction.TestRegister;
var
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

initialization
  RegisterTest(TTestBrookDBAction);

end.

