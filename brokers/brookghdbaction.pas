(*
  Brook Greyhound DB Action unit.

  Copyright (C) 2013 Silvio Clecio.

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

unit BrookGhDBAction;

{$mode objfpc}{$H+}

interface

uses
  BrookDataBase, BrookDBAction, BrookGhBroker, ghSQL, SysUtils;

type
  EBrookDBAction = BrookDBAction.EBrookDBAction;

  TBrookDBActionClass = BrookDBAction.TBrookDBAction;

  TBrookDBAction = class(TBrookCustomDBAction)
  private
    FLinks: TghSQLTableList;
    FRelations: TghSQLTableList;
    FTable: TBrookGhTable;
    function GetDataBase: TghSQLConnector;
    function GetTable: TBrookGhTable;
    procedure SetDataBase(AValue: TghSQLConnector);
    procedure SetTable(AValue: TBrookGhTable);
  protected
    function CreateTable: TghSQLTable; virtual;
  public
    constructor Create; override;
    property DataBase: TghSQLConnector read GetDataBase write SetDataBase;
    property Table: TBrookGhTable read GetTable write SetTable;
    property Links: TghSQLTableList read FLinks;
    property Relations: TghSQLTableList read FRelations;
  end;

implementation

{ TBrookDBAction }

constructor TBrookDBAction.Create;
begin
  inherited Create;
  FTable := TBrookGhTable(CreateTable);
  FLinks := FTable.Links;
  FRelations := FTable.Relations;
end;

function TBrookDBAction.CreateTable: TghSQLTable;
begin
  if Assigned(TBrookDataBases.Service.Current) then
    Result := TghSQLConnector(
      TBrookDataBases.Service.Current.Connection).Tables[GetTableName]
  else
    Result := TghSQLConnector(
      TBrookDataBase.Create.Connection).Tables[GetTableName];
end;

function TBrookDBAction.GetDataBase: TghSQLConnector;
begin
  Result := FTable.Connector;
end;

function TBrookDBAction.GetTable: TBrookGhTable;
begin
  Result := FTable;
end;

procedure TBrookDBAction.SetDataBase(AValue: TghSQLConnector);
begin
  FTable.Connector := AValue;
end;

procedure TBrookDBAction.SetTable(AValue: TBrookGhTable);
begin
  FreeAndNil(FTable);
  FTable := AValue;
end;

end.

