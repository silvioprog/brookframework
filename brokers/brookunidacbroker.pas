(*
  Brook UniDAC Broker unit.

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

unit BrookUniDACBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, Uni, DB, SQLdb, Classes, DBConst;

type

  { TBrookUniDACQuery }

  TBrookUniDACQuery = class(TBrookQuery)
  private
    FQuery: TUniQuery;
    FDataBase: TBrookDataBase;
  protected
    function GetFields: TFields; override;
    function GetParams: TParams; override;
    function GetSQL: TStrings; override;
    function GetDataSet: TDataSet; override;
    function GetDataSource: TDataSource; override;
    function GetDataBase: TBrookDataBase; override;
    procedure SetDataBase(AValue: TBrookDataBase); override;
    procedure SetDataSource({%H-}AValue: TDataSource); override;
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
    function Commit(const ARetaining: Boolean = False): TBrookQuery; override;
    function Rollback(const ARetaining: Boolean = False): TBrookQuery; override;
  end;

  { TBrookUniDACDataBase }

  TBrookUniDACDataBase = class(TBrookDataBase)
  private
    FConn: TUniConnection;
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

{ TBrookUniDACQuery }

constructor TBrookUniDACQuery.Init(ADataBase: TBrookDataBase);
begin
  FQuery := TUniQuery.Create(nil);
  SetDataBase(ADataBase);
  FQuery.CachedUpdates := True;
end;

function TBrookUniDACQuery.Execute: TBrookQuery;
begin
  Result := Self;
  FQuery.ExecSQL;
end;

function TBrookUniDACQuery.RowsAffected: TRowsCount;
begin
  Result := FQuery.RowsAffected;
end;

function TBrookUniDACQuery.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.FindParam(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SParameterNotFound, [AName], FQuery);
end;

function TBrookUniDACQuery.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FindField(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SFieldNotFound, [AName], FQuery);
end;

function TBrookUniDACQuery.FieldDef(const AName: string): TFieldDef;
begin
  Result := FQuery.FieldDefs.Find(AName);
end;

function TBrookUniDACQuery.ApplyUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.ApplyUpdates;
end;

function TBrookUniDACQuery.CancelUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TBrookUniDACQuery.Apply(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  try
    FQuery.ApplyUpdates;
  except
    FQuery.CancelUpdates;
    raise;
  end;
end;

function TBrookUniDACQuery.Undo(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TBrookUniDACQuery.Commit(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  try
    if ARetaining then
      FQuery.Connection.CommitRetaining
    else
      FQuery.Connection.Commit;
  except
    if ARetaining then
      FQuery.Connection.RollbackRetaining
    else
      FQuery.Connection.Rollback;
    raise;
  end;
end;

function TBrookUniDACQuery.Rollback(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  if ARetaining then
    FQuery.Connection.RollbackRetaining
  else
    FQuery.Connection.Rollback;
end;

function TBrookUniDACQuery.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TBrookUniDACQuery.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TBrookUniDACQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TBrookUniDACQuery.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TBrookUniDACQuery.GetDataSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TBrookUniDACQuery.GetDataBase: TBrookDataBase;
begin
  Result := FDataBase;
end;

procedure TBrookUniDACQuery.SetDataBase(AValue: TBrookDataBase);
begin
  FDataBase := AValue;
  if Assigned(AValue) and Assigned(AValue.Connection) then
    FQuery.Connection := TUniConnection(AValue.Connection)
  else
    FQuery.Connection := nil;
end;

procedure TBrookUniDACQuery.SetDataSource(AValue: TDataSource);
begin
  raise EBrookQuery.Create(Self,
    'Unsupported method: TBrookUniDACQuery.SetDataSource');
end;

{ TBrookUniDACDataBase }

constructor TBrookUniDACDataBase.Init;
begin
  inherited Init;
  FConn := TUniConnection.Create(nil);
  FConn.LoginPrompt := False;
end;

function TBrookUniDACDataBase.GetPort: Integer;
begin
  Result := FConn.Port;
end;

function TBrookUniDACDataBase.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

function TBrookUniDACDataBase.GetDatabase: string;
begin
  Result := FConn.Database;
end;

function TBrookUniDACDataBase.GetHost: string;
begin
  Result := FConn.Server;
end;

function TBrookUniDACDataBase.GetPassword: string;
begin
  Result := FConn.Password;
end;

function TBrookUniDACDataBase.GetDriver: string;
begin
  Result := FConn.ProviderName;
end;

function TBrookUniDACDataBase.GetUser: string;
begin
  Result := FConn.Username;
end;

procedure TBrookUniDACDataBase.SetPort(AValue: Integer);
begin
  FConn.Port := AValue;
end;

procedure TBrookUniDACDataBase.SetDatabase(AValue: string);
begin
  FConn.Database := AValue;
end;

procedure TBrookUniDACDataBase.SetHost(AValue: string);
begin
  FConn.Server := AValue;
end;

procedure TBrookUniDACDataBase.SetPassword(AValue: string);
begin
  FConn.Password := AValue;
end;

procedure TBrookUniDACDataBase.SetDriver(AValue: string);
begin
  FConn.ProviderName := AValue;
end;

procedure TBrookUniDACDataBase.SetUser(AValue: string);
begin
  FConn.Username := AValue;
end;

function TBrookUniDACDataBase.GetConnection: TObject;
begin
  Result := FConn;
end;

class function TBrookUniDACDataBase.GetLibrary: string;
begin
  Result := 'UniDAC';
end;

procedure TBrookUniDACDataBase.StartTransaction;
begin
  FConn.StartTransaction;
end;

procedure TBrookUniDACDataBase.Commit;
begin
  FConn.Commit;
end;

procedure TBrookUniDACDataBase.Rollback;
begin
  FConn.Rollback;
end;

function TBrookUniDACDataBase.InTransaction: Boolean;
begin
  Result := FConn.InTransaction;
end;

procedure TBrookUniDACDataBase.Connect;
begin
  FConn.Connected := True;
end;

procedure TBrookUniDACDataBase.Disconnect;
begin
  FConn.Connected := False;
end;

initialization
  TBrookUniDACDataBase.Register;
  TBrookUniDACQuery.InitBrokerClass;

end.
