(*
  Brook Greyhound SQLdb Broker unit.

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

unit BrookGhSQLdbBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookGhBroker, ghSQL, ghSQLdbLib, SysUtils;

type
  TBrookGhSQLdbDataBase = class(TBrookGhDataBase)
  protected
    class function GetSQLLibClass: TghSQLLibClass; override;
    function GetDriver: string; override;
    procedure SetDriver(AValue: string); override;
    function GetPort: Integer; override;
    procedure SetPort(AValue: Integer); override;
  public
    class function GetLibrary: string; override;
  end;

implementation

class function TBrookGhSQLdbDataBase.GetSQLLibClass: TghSQLLibClass;
begin
  Result := TghSQLdbLib;
end;

function TBrookGhSQLdbDataBase.GetDriver: string;
begin
  Result := TghSQLdbLib(TghSQLConnector(
    Connection).Lib).Connection.ConnectorType;
end;

procedure TBrookGhSQLdbDataBase.SetDriver(AValue: string);
begin
  TghSQLdbLib(TghSQLConnector(
    Connection).Lib).Connection.ConnectorType := AValue;
end;

function TBrookGhSQLdbDataBase.GetPort: Integer;
begin
  Result := StrToInt(TghSQLdbLib(TghSQLConnector(
    Connection).Lib).Connection.Params.Values['port']);
end;

procedure TBrookGhSQLdbDataBase.SetPort(AValue: Integer);
begin
  TghSQLdbLib(TghSQLConnector(
    Connection).Lib).Connection.Params.Values['port'] := IntToStr(AValue);
end;

class function TBrookGhSQLdbDataBase.GetLibrary: string;
begin
  Result := 'Greyhound-SQLdb';
end;

initialization
  TBrookGhSQLdbDataBase.Register;

end.

