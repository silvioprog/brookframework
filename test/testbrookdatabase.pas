unit testbrookdatabase;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookUtils, fpcunit, testregistry;

type
  TDatabaseBroker = class(TBrookDataBase)
  private
    FConnection: TObject;
    FDatabase: string;
    FDriver: string;
    FHost: string;
    FPassword: string;
    FPort: Integer;
    FUser: string;
  protected
    function GetDatabase: string; override;
    function GetHost: string; override;
    function GetPassword: string; override;
    function GetDriver: string; override;
    function GetUser: string; override;
    procedure SetDatabase(AValue: string); override;
    procedure SetHost(AValue: string); override;
    procedure SetPassword(AValue: string); override;
    procedure SetDriver(AValue: string); override;
    procedure SetUser(AValue: string); override;
    function GetPort: Integer; override;
    procedure SetPort(AValue: Integer); override;
    function GetConnection: TObject; override;
  public
    class function GetLibrary: string; override;
  published
    property Driver: string read GetDriver write SetDriver;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property Connection: TObject read GetConnection;
  end;

  TTestBrookDatabase = class(TTestCase)
  published
    procedure TestConfiguration;
    procedure TestCount;
    procedure TestFind;
    procedure TestItemByLibrary;
    procedure TestCurrent;
    procedure TestItems;
  end;

implementation

{ TDatabaseBroker }

function TDatabaseBroker.GetDatabase: string;
begin
  Result := FDatabase;
end;

function TDatabaseBroker.GetHost: string;
begin
  Result := FHost;
end;

function TDatabaseBroker.GetPassword: string;
begin
  Result := FPassword;
end;

function TDatabaseBroker.GetDriver: string;
begin
  Result := FDriver;
end;

function TDatabaseBroker.GetUser: string;
begin
  Result := FUser;
end;

procedure TDatabaseBroker.SetDatabase(AValue: string);
begin
  FDatabase := AValue;
end;

procedure TDatabaseBroker.SetHost(AValue: string);
begin
  FHost := AValue;
end;

procedure TDatabaseBroker.SetPassword(AValue: string);
begin
  FPassword := AValue;
end;

procedure TDatabaseBroker.SetDriver(AValue: string);
begin
  FDriver := AValue;
end;

procedure TDatabaseBroker.SetUser(AValue: string);
begin
  FUser := AValue;
end;

function TDatabaseBroker.GetPort: Integer;
begin
  Result := FPort;
end;

procedure TDatabaseBroker.SetPort(AValue: Integer);
begin
  FPort := AValue;
end;

function TDatabaseBroker.GetConnection: TObject;
begin
  Result := FConnection;
end;

class function TDatabaseBroker.GetLibrary: string;
begin
  Result := 'dbtest';
end;

{ TTestBrookDatabase }

procedure TTestBrookDatabase.TestConfiguration;
var
  VDb: TBrookDataBase;
  VDbs: TBrookDataBases;
begin
  VDbs := TBrookDataBases.Service;
  BrookSettings.Configuration := 'library=dbtest;driver=pg1;' +
    'database=db1;user=usr1;password=pass1;host=localhost1';
  VDbs.FreeCurrent;
  VDb := TDatabaseBroker.Create;
  VDbs.Configurator.Configure;
  AssertEquals('dbtest', VDb.GetLibrary);
  AssertEquals('pg1', VDb.Driver);
  AssertEquals('db1', VDb.Database);
  AssertEquals('usr1', VDb.User);
  AssertEquals('pass1', VDb.Password);
  AssertEquals('localhost1', VDb.Host);
  VDb.Driver := '';
  VDb.Database := '';
  VDb.User := '';
  VDb.Password := '';
  VDb.Host := '';
  BrookSettings.Configuration := 'testdb.cfg';
  VDbs.Configurator.Load.Configure;
  AssertEquals('dbtest', VDb.GetLibrary);
  AssertEquals('pg2', VDb.Driver);
  AssertEquals('db2', VDb.Database);
  AssertEquals('usr2', VDb.User);
  AssertEquals('pass2', VDb.Password);
  AssertEquals('localhost2', VDb.Host);
end;

procedure TTestBrookDatabase.TestCount;
begin
  AssertEquals(2, TBrookDataBases.Service.Count);
end;

procedure TTestBrookDatabase.TestFind;
begin
  AssertEquals(TDatabaseBroker, TBrookDataBases.Service.Find('dbtest'));
end;

procedure TTestBrookDatabase.TestItemByLibrary;
begin
  AssertEquals(TDatabaseBroker, TBrookDataBases.Service.ItemByLibrary('dbtest'));
end;

procedure TTestBrookDatabase.TestCurrent;
begin
  AssertEquals(True, TBrookDataBases.Service.Current is TDatabaseBroker);
end;

procedure TTestBrookDatabase.TestItems;
begin
  AssertEquals(TDatabaseBroker, TBrookDataBases.Service.Items[0]);
end;

initialization
  TDatabaseBroker.Register;
  RegisterTest(TTestBrookDatabase);

finalization
  TDatabaseBroker.Unregister;

end.

