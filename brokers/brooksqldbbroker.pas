(*
  Brook SQLdb Broker unit.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/brookframework

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookSQLdbBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, SQLdb, DB, Classes, SysUtils, DBConst;

type

  { TBrookSQLdbQuery }

  TBrookSQLdbQuery = class(TBrookQuery)
  private
    FQuery: TSQLQuery;
    FDataBase: TBrookDataBase;
  protected
    function GetFields: TFields; override;
    function GetParams: TParams; override;
    function GetSQL: TStrings; override;
    function GetDataSet: TDataSet; override;
    function GetDataSource: TDataSource; override;
    function GetDataBase: TBrookDataBase; override;
    procedure SetDataBase(AValue: TBrookDataBase); override;
    procedure SetDataSource(AValue: TDataSource); override;
  public
    constructor Init(ADataBase: TBrookDataBase); override;
    destructor Destroy; override;
    function RowsAffected: TRowsCount; override;
    function Execute: TBrookQuery; override;
    function Param(const AName: string): TParam; override;
    function Field(const AName: string): TField; override;
    function FieldDef(const AName: string): TFieldDef; override;
    function ApplyUpdates: TBrookQuery; override;
    function CancelUpdates: TBrookQuery; override;
    function Apply(const ARetaining: Boolean = False): TBrookQuery; override;
    function Undo(const ARetaining: Boolean = False): TBrookQuery; override;
    function Commit(const ARetaining: Boolean = False): TBrookQuery; override;
    function Rollback(const ARetaining: Boolean = False): TBrookQuery; override;
  end;

  { TBrookSQLdbDataBase }

  TBrookSQLdbDataBase = class(TBrookDataBase)
  private
    FConn: TSQLConnector;
  protected
    function GetConnected: Boolean; override;
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
    constructor Init; override;
    destructor Destroy; override;
    class function GetLibrary: string; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function InTransaction: Boolean; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    property Connection: TObject read GetConnection;
  published
    property Driver: string read GetDriver write SetDriver;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
  end;

implementation

{ TBrookSQLdbQuery }

constructor TBrookSQLdbQuery.Init(ADataBase: TBrookDataBase);
begin
  FQuery := TSQLQuery.Create(nil);
  SetDataBase(ADataBase);
end;

destructor TBrookSQLdbQuery.Destroy;
begin
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function TBrookSQLdbQuery.RowsAffected: TRowsCount;
begin
  Result := FQuery.RowsAffected;
end;

function TBrookSQLdbQuery.Execute: TBrookQuery;
begin
  Result := Self;
  FQuery.ExecSQL;
end;

function TBrookSQLdbQuery.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.FindParam(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SParameterNotFound, [AName], FQuery);
end;

function TBrookSQLdbQuery.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FindField(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SFieldNotFound, [AName], FQuery);
end;

function TBrookSQLdbQuery.FieldDef(const AName: string): TFieldDef;
begin
  Result := FQuery.FieldDefs.Find(AName);
end;

function TBrookSQLdbQuery.ApplyUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.ApplyUpdates(0);
end;

function TBrookSQLdbQuery.CancelUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TBrookSQLdbQuery.Apply(const ARetaining: Boolean): TBrookQuery;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  try
    FQuery.ApplyUpdates(0);
    if ARetaining then
      VTrans.CommitRetaining
    else
      VTrans.Commit;
  except
    if ARetaining then
      VTrans.RollbackRetaining
    else
      VTrans.Rollback;
    raise;
  end;
end;

function TBrookSQLdbQuery.Undo(const ARetaining: Boolean): TBrookQuery;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  FQuery.CancelUpdates;
  if ARetaining then
    VTrans.RollbackRetaining
  else
    VTrans.Rollback;
end;

function TBrookSQLdbQuery.Commit(const ARetaining: Boolean): TBrookQuery;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  try
    if ARetaining then
      VTrans.CommitRetaining
    else
      VTrans.Commit;
  except
    if ARetaining then
      VTrans.RollbackRetaining
    else
      VTrans.Rollback;
    raise;
  end;
end;

function TBrookSQLdbQuery.Rollback(const ARetaining: Boolean): TBrookQuery;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  if ARetaining then
    VTrans.RollbackRetaining
  else
    VTrans.Rollback;
end;

function TBrookSQLdbQuery.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TBrookSQLdbQuery.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TBrookSQLdbQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TBrookSQLdbQuery.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TBrookSQLdbQuery.GetDataSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TBrookSQLdbQuery.GetDataBase: TBrookDataBase;
begin
  Result := FDataBase;
end;

procedure TBrookSQLdbQuery.SetDataBase(AValue: TBrookDataBase);
begin
  FDataBase := AValue;
  if Assigned(AValue) and Assigned(AValue.Connection) then
    FQuery.DataBase := TSQLConnector(AValue.Connection)
  else
    FQuery.DataBase := nil;
end;

procedure TBrookSQLdbQuery.SetDataSource(AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

{ TBrookSQLdbDataBase }

constructor TBrookSQLdbDataBase.Init;
begin
  inherited Init;
  FConn := TSQLConnector.Create(nil);
  FConn.Transaction := TSQLTransaction.Create(FConn);
end;

destructor TBrookSQLdbDataBase.Destroy;
begin
  FreeAndNil(FConn);
  inherited Destroy;
end;

function TBrookSQLdbDataBase.GetPort: Integer;
begin
  Result := StrToIntDef(FConn.Params.Values['port'], 0);
end;

function TBrookSQLdbDataBase.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

function TBrookSQLdbDataBase.GetDatabase: string;
begin
  Result := FConn.DatabaseName;
end;

function TBrookSQLdbDataBase.GetHost: string;
begin
  Result := FConn.HostName;
end;

function TBrookSQLdbDataBase.GetPassword: string;
begin
  Result := FConn.Password;
end;

function TBrookSQLdbDataBase.GetDriver: string;
begin
  Result := FConn.ConnectorType;
end;

function TBrookSQLdbDataBase.GetUser: string;
begin
  Result := FConn.UserName;
end;

procedure TBrookSQLdbDataBase.SetPort(AValue: Integer);
begin
  FConn.Params.Values['port'] := IntToStr(AValue);
end;

procedure TBrookSQLdbDataBase.SetDatabase(AValue: string);
begin
  FConn.DatabaseName := AValue;
end;

procedure TBrookSQLdbDataBase.SetHost(AValue: string);
begin
  FConn.HostName := AValue;
end;

procedure TBrookSQLdbDataBase.SetPassword(AValue: string);
begin
  FConn.Password := AValue;
end;

procedure TBrookSQLdbDataBase.SetDriver(AValue: string);
begin
  FConn.ConnectorType := AValue;
end;

procedure TBrookSQLdbDataBase.SetUser(AValue: string);
begin
  FConn.UserName := AValue;
end;

function TBrookSQLdbDataBase.GetConnection: TObject;
begin
  Result := FConn;
end;

class function TBrookSQLdbDataBase.GetLibrary: string;
begin
  Result := 'SQLdb';
end;

procedure TBrookSQLdbDataBase.StartTransaction;
begin
  FConn.Transaction.StartTransaction;
end;

procedure TBrookSQLdbDataBase.Commit;
begin
  FConn.Transaction.Commit;
end;

procedure TBrookSQLdbDataBase.Rollback;
begin
  FConn.Transaction.Rollback;
end;

procedure TBrookSQLdbDataBase.Connect;
begin
  FConn.Connected := True;
end;

procedure TBrookSQLdbDataBase.Disconnect;
begin
  FConn.Connected := False;
end;

function TBrookSQLdbDataBase.InTransaction: Boolean;
begin
  Result := FConn.Transaction.Active;
end;

initialization
  TBrookSQLdbDataBase.Register;
  TBrookSQLdbQuery.InitBrokerClass;

end.
