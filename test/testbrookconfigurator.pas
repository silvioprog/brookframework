unit testbrookconfigurator;

{$mode objfpc}{$H+}

interface

uses
  BrookConfigurator, BrookUtils, fpcunit, testregistry;

type
  TTarget1 = class
  private
    FDatabase: string;
    FDriver: string;
    FHost: string;
    FPassword: string;
    FUser: string;
  published
    property Driver: string read FDriver write FDriver;
    property Database: string read FDatabase write FDatabase;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Host: string read FHost write FHost;
  end;

  TTarget2 = class(TTarget1)
  end;

  TTestBrookConfigurator = class(TTestCase)
  published
    procedure TestConfigure;
  end;

implementation

procedure TTestBrookConfigurator.TestConfigure;
var
  VTarget1: TTarget1;
  VTarget2: TTarget2;
  VCfg: TBrookConfigurator;
begin
  BrookSettings.Configuration := '';
  VCfg := TBrookConfigurator.Create('testdb.cfg');
  VTarget1 := TTarget1.Create;
  VTarget2 := TTarget2.Create;
  try
    VCfg.Target := VTarget1;
    VCfg.Configure;
    AssertEquals('pg2', VTarget1.Driver);
    AssertEquals('db2', VTarget1.Database);
    AssertEquals('usr2', VTarget1.User);
    AssertEquals('pass2', VTarget1.Password);
    AssertEquals('localhost2', VTarget1.Host);
    VTarget1.Driver := '';
    VTarget1.Database := '';
    VTarget1.User := '';
    VTarget1.Password := '';
    VTarget1.Host := '';
    VCfg.Configuration :=
      'driver=pg;database=db;user=usr;password=pass;host=localhost';
    VCfg.Configure;
    AssertEquals('pg', VTarget1.Driver);
    AssertEquals('db', VTarget1.Database);
    AssertEquals('usr', VTarget1.User);
    AssertEquals('pass', VTarget1.Password);
    AssertEquals('localhost', VTarget1.Host);
    VCfg.ClassChecking := True;
    VCfg.Configuration := 'ttarget2.driver=pg;ttarget2.database=db;' +
      'ttarget2.user=usr;ttarget2.password=pass;ttarget2.host=localhost';
    VCfg.Target := VTarget1;
    VTarget1.Driver := '';
    VTarget1.Database := '';
    VTarget1.User := '';
    VTarget1.Password := '';
    VTarget1.Host := '';
    VCfg.Configure;
    AssertEquals('', VTarget1.Driver);
    AssertEquals('', VTarget1.Database);
    AssertEquals('', VTarget1.User);
    AssertEquals('', VTarget1.Password);
    AssertEquals('', VTarget1.Host);
    VCfg.Target := VTarget2;
    VCfg.Configure;
    AssertEquals('pg', VTarget2.Driver);
    AssertEquals('db', VTarget2.Database);
    AssertEquals('usr', VTarget2.User);
    AssertEquals('pass', VTarget2.Password);
    AssertEquals('localhost', VTarget2.Host);
  finally
    VTarget1.Free;
    VTarget2.Free;
    VCfg.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookConfigurator);

end.

