(*
  Brook Greyhound Zeos Broker unit.

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

unit BrookGhZeosBroker;

{$mode objfpc}{$H+}

interface

uses
  BrookGhBroker, gh_SQL, gh_ZeosLib;

type
  TBrookGhZeosDataBase = class(TBrookGhDataBase)
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

class function TBrookGhZeosDataBase.GetSQLLibClass: TghSQLLibClass;
begin
  Result := TghZeosLib;
end;

function TBrookGhZeosDataBase.GetDriver: string;
begin
  Result := TghZeosLib(TghSQLConnector(Connection).Lib).Connection.Protocol;
end;

procedure TBrookGhZeosDataBase.SetDriver(AValue: string);
begin
  TghZeosLib(TghSQLConnector(Connection).Lib).Connection.Protocol := AValue;
end;

function TBrookGhZeosDataBase.GetPort: Integer;
begin
  Result := TghZeosLib(TghSQLConnector(Connection).Lib).Connection.Port;
end;

procedure TBrookGhZeosDataBase.SetPort(AValue: Integer);
begin
  TghZeosLib(TghSQLConnector(Connection).Lib).Connection.Port := AValue;
end;

class function TBrookGhZeosDataBase.GetLibrary: string;
begin
  Result := 'Greyhound-Zeos';
end;

initialization
  TBrookGhZeosDataBase.Register;

end.

