(*
  Brook Zeos Broker unit.

  Copyright (C) 2012 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookZeosBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, ZConnection, ZDataset, DB, SQLdb, Classes, DBConst;

type

  { TBrookZeosQuery }

  TBrookZeosQuery = class(TBrookQuery)
  private
    FQuery: TZQuery;
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
    function Execute: TBrookQuery; override;
    function RowsAffected: TRowsCount; override;
    function Param(const AName: string): TParam; override;
    function Field(const AName: string): TField; override;
    function FieldDef(const AName: string): TFieldDef; override;
    function ApplyUpdates: TBrookQuery; override;
    function CancelUpdates: TBrookQuery; override;
    function Apply({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
    function Undo({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
    function Commit({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
    function Rollback({%H-}const ARetaining: Boolean = False): TBrookQuery; override;
  end;

  { TBrookZeosDataBase }

  TBrookZeosDataBase = class(TBrookDataBase)
  private
    FConn: TZConnection;
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

{ TBrookZeosQuery }

constructor TBrookZeosQuery.Init(ADataBase: TBrookDataBase);
begin
  FQuery := TZQuery.Create(nil);
  SetDataBase(ADataBase);
  FQuery.CachedUpdates := True;
end;

function TBrookZeosQuery.Execute: TBrookQuery;
begin
  Result := Self;
  FQuery.ExecSQL;
end;

function TBrookZeosQuery.RowsAffected: TRowsCount;
begin
  Result := FQuery.RowsAffected;
end;

function TBrookZeosQuery.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.FindParam(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SParameterNotFound, [AName], FQuery);
end;

function TBrookZeosQuery.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FindField(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SFieldNotFound, [AName], FQuery);
end;

function TBrookZeosQuery.FieldDef(const AName: string): TFieldDef;
begin
  Result := FQuery.FieldDefs.Find(AName);
end;

function TBrookZeosQuery.ApplyUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.ApplyUpdates;
end;

function TBrookZeosQuery.CancelUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TBrookZeosQuery.Apply(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  try
    FQuery.ApplyUpdates;
    FQuery.Connection.Commit;
  except
    FQuery.CancelUpdates;
    FQuery.Connection.Rollback;
    raise;
  end;
end;

function TBrookZeosQuery.Undo(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  FQuery.CancelUpdates;
  FQuery.Connection.Rollback;
end;

function TBrookZeosQuery.Commit(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  try
    FQuery.Connection.Commit;
  except
    FQuery.Connection.Rollback;
    raise;
  end;
end;

function TBrookZeosQuery.Rollback(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  FQuery.Connection.Rollback;
end;

function TBrookZeosQuery.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TBrookZeosQuery.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TBrookZeosQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TBrookZeosQuery.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TBrookZeosQuery.GetDataSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TBrookZeosQuery.GetDataBase: TBrookDataBase;
begin
  Result := FDataBase;
end;

procedure TBrookZeosQuery.SetDataBase(AValue: TBrookDataBase);
begin
  FDataBase := AValue;
  if Assigned(AValue) and Assigned(AValue.Connection) then
    FQuery.Connection := TZConnection(AValue.Connection)
  else
    FQuery.Connection := nil;
end;

procedure TBrookZeosQuery.SetDataSource(AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

{ TBrookZeosDataBase }

constructor TBrookZeosDataBase.Init;
begin
  inherited Init;
  FConn := TZConnection.Create(nil);
  FConn.AutoCommit := False;
end;

function TBrookZeosDataBase.GetPort: Integer;
begin
  Result := FConn.Port;
end;

function TBrookZeosDataBase.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

function TBrookZeosDataBase.GetDatabase: string;
begin
  Result := FConn.Database;
end;

function TBrookZeosDataBase.GetHost: string;
begin
  Result := FConn.HostName;
end;

function TBrookZeosDataBase.GetPassword: string;
begin
  Result := FConn.Password;
end;

function TBrookZeosDataBase.GetDriver: string;
begin
  Result := FConn.Protocol;
end;

function TBrookZeosDataBase.GetUser: string;
begin
  Result := FConn.User;
end;

procedure TBrookZeosDataBase.SetPort(AValue: Integer);
begin
  FConn.Port := AValue;
end;

procedure TBrookZeosDataBase.SetDatabase(AValue: string);
begin
  FConn.Database := AValue;
end;

procedure TBrookZeosDataBase.SetHost(AValue: string);
begin
  FConn.HostName := AValue;
end;

procedure TBrookZeosDataBase.SetPassword(AValue: string);
begin
  FConn.Password := AValue;
end;

procedure TBrookZeosDataBase.SetDriver(AValue: string);
begin
  FConn.Protocol := AValue;
end;

procedure TBrookZeosDataBase.SetUser(AValue: string);
begin
  FConn.User := AValue;
end;

function TBrookZeosDataBase.GetConnection: TObject;
begin
  Result := FConn;
end;

class function TBrookZeosDataBase.GetLibrary: string;
begin
  Result := 'Zeos';
end;

procedure TBrookZeosDataBase.StartTransaction;
begin
  FConn.StartTransaction;
end;

procedure TBrookZeosDataBase.Commit;
begin
  FConn.Commit;
end;

procedure TBrookZeosDataBase.Rollback;
begin
  FConn.Rollback;
end;

function TBrookZeosDataBase.InTransaction: Boolean;
begin
  Result := FConn.InTransaction;
end;

procedure TBrookZeosDataBase.Connect;
begin
  FConn.Connected := True;
end;

procedure TBrookZeosDataBase.Disconnect;
begin
  FConn.Connected := False;
end;

initialization
  TBrookZeosDataBase.Register;
  TBrookZeosQuery.InitBrokerClass;

finalization
  TBrookDataBases.Service.Current.Disconnect; // avoids error on finalizing

end.
