(*
  Brook ADS Broker unit.

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

unit BrookADSBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookQuery, adscnnct, adstable, adsset, DB, SQLdb, Classes,
  DBConst;

type

  { TBrookADSQuery }

  TBrookADSQuery = class(TBrookQuery)
  private
    FQuery: TAdsQuery;
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

  { TBrookADSDataBase }

  TBrookADSDataBase = class(TBrookDataBase)
  private
    FConn: TAdsConnection;
  protected
    function GetConnected: Boolean; override;
    function GetDatabase: string; override;
    function GetHost: string; override;
    function GetPassword: string; override;
    function GetDriver: string; override;
    function GetUser: string; override;
    procedure SetDatabase(AValue: string); override;
    procedure SetHost({%H-}AValue: string); override;
    procedure SetPassword(AValue: string); override;
    procedure SetDriver({%H-}AValue: string); override;
    procedure SetUser(AValue: string); override;
    function GetPort: Integer; override;
    procedure SetPort({%H-}AValue: Integer); override;
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

{ TBrookADSQuery }

constructor TBrookADSQuery.Init(ADataBase: TBrookDataBase);
begin
  FQuery := TAdsQuery.Create(nil);
  SetDataBase(ADataBase);
  FQuery.RequestLive := True;
end;

function TBrookADSQuery.Execute: TBrookQuery;
begin
  Result := Self;
  FQuery.ExecSQL;
end;

function TBrookADSQuery.RowsAffected: TRowsCount;
begin
  Result := FQuery.RowsAffected;
end;

function TBrookADSQuery.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.FindParam(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SParameterNotFound, [AName], FQuery);
end;

function TBrookADSQuery.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FindField(AName);
  if not Assigned(Result) then
    DatabaseErrorFmt(SFieldNotFound, [AName], FQuery);
end;

function TBrookADSQuery.FieldDef(const AName: string): TFieldDef;
begin
  Result := FQuery.FieldDefs.Find(AName);
end;

function TBrookADSQuery.ApplyUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.ApplyUpdates;
end;

function TBrookADSQuery.CancelUpdates: TBrookQuery;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TBrookADSQuery.Apply(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  try
    FQuery.Post;
  except
    FQuery.Cancel;
    raise;
  end;
end;

function TBrookADSQuery.Undo(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  FQuery.Cancel;
end;

function TBrookADSQuery.Commit(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  try
    FQuery.AdsConnection.Commit;
  except
    FQuery.AdsConnection.Rollback;
    raise;
  end;
end;

function TBrookADSQuery.Rollback(const ARetaining: Boolean): TBrookQuery;
begin
  Result := Self;
  FQuery.AdsConnection.Rollback;
end;

function TBrookADSQuery.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TBrookADSQuery.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TBrookADSQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TBrookADSQuery.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TBrookADSQuery.GetDataSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TBrookADSQuery.GetDataBase: TBrookDataBase;
begin
  Result := FDataBase;
end;

procedure TBrookADSQuery.SetDataBase(AValue: TBrookDataBase);
begin
  FDataBase := AValue;
  if Assigned(AValue) and Assigned(AValue.Connection) then
    FQuery.AdsConnection := TAdsConnection(AValue.Connection)
  else
    FQuery.AdsConnection := nil;
end;

procedure TBrookADSQuery.SetDataSource(AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

{ TBrookADSDataBase }

constructor TBrookADSDataBase.Init;
begin
  inherited Init;
  FConn := TAdsConnection.Create(nil);
  FConn.AdsServerTypes := [stADS_LOCAL];
end;

function TBrookADSDataBase.GetPort: Integer;
begin
  Result := 0;
end;

function TBrookADSDataBase.GetConnected: Boolean;
begin
  Result := FConn.IsConnected;
end;

function TBrookADSDataBase.GetDatabase: string;
begin
  Result := FConn.ConnectPath;
end;

function TBrookADSDataBase.GetHost: string;
begin
  Result := '';
end;

function TBrookADSDataBase.GetPassword: string;
begin
  Result := FConn.Password;
end;

function TBrookADSDataBase.GetDriver: string;
begin
  Result := '';
end;

function TBrookADSDataBase.GetUser: string;
begin
  Result := FConn.Username;
end;

procedure TBrookADSDataBase.SetPort(AValue: Integer);
begin
end;

procedure TBrookADSDataBase.SetDatabase(AValue: string);
begin
  FConn.ConnectPath := AValue;
end;

procedure TBrookADSDataBase.SetHost(AValue: string);
begin
end;

procedure TBrookADSDataBase.SetPassword(AValue: string);
begin
  FConn.Password := AValue;
end;

procedure TBrookADSDataBase.SetDriver(AValue: string);
begin
end;

procedure TBrookADSDataBase.SetUser(AValue: string);
begin
  FConn.Username := AValue;
end;

function TBrookADSDataBase.GetConnection: TObject;
begin
  Result := FConn;
end;

class function TBrookADSDataBase.GetLibrary: string;
begin
  Result := 'ADS';
end;

procedure TBrookADSDataBase.StartTransaction;
begin
  FConn.BeginTransaction;
end;

procedure TBrookADSDataBase.Commit;
begin
  FConn.Commit;
end;

procedure TBrookADSDataBase.Rollback;
begin
  FConn.Rollback;
end;

function TBrookADSDataBase.InTransaction: Boolean;
begin
  Result := FConn.TransactionActive;
end;

procedure TBrookADSDataBase.Connect;
begin
  FConn.IsConnected := True;
end;

procedure TBrookADSDataBase.Disconnect;
begin
  FConn.IsConnected := False;
end;

initialization
  TBrookADSDataBase.Register;
  TBrookADSQuery.InitBrokerClass;

end.
