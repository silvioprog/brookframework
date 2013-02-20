(*
  Brook DB Action unit.

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

unit BrookDBAction;

{$i brook.inc}

interface

uses
  BrookAction, BrookException, BrookDataBase, BrookTable, BrookUtils,
  BrookMessages, BrookConsts, Classes, SysUtils;

type
  { Handles exceptions for @link(TBrookDBAction). }
  EBrookDBAction = class(EBrook);

  { Is a metaclass for @link(TBrookDBAction) class. }
  TBrookDBActionClass = class of TBrookDBAction;

  { Custom class for manages HTTP requests and responses when data persistance
    is required. }
  TBrookCustomDBAction = class(TBrookAction)
  strict private
    class var _BrookTableNames: TStrings;
  protected
    class procedure CheckTableName(const ATableName: string);
    class function TableNames: TStrings;
    class procedure InitTableNames;
    class procedure DoneTableNames;
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const ADefault: Boolean = False); overload;
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod;
      const ADefault: Boolean = False); overload;
    { Defines the table name. }
    class procedure SetTableName(const ATableName: string);
    { Returns the table name. }
    class function GetTableName: string;
  end;

  { Manages HTTP requests and responses when data persistance is required. }
  TBrookDBAction = class(TBrookCustomDBAction)
  private
    FTable: TBrookTable;
    function GetDataBase: TBrookDataBase;
    function GetTable: TBrookTable;
    procedure SetDataBase(AValue: TBrookDataBase);
    procedure SetTable(AValue: TBrookTable);
  protected
    function CreateTable: TBrookTable; virtual;
  public
    { Creates an instance of @code(TBrookDBAction). }
    constructor Create; override;
    { Defines the current database or return it. }
    property DataBase: TBrookDataBase read GetDataBase write SetDataBase;
    { Get de table linked to the action. }
    property Table: TBrookTable read GetTable write SetTable;
  end;

implementation

{ TBrookCustomDBAction }

class procedure TBrookCustomDBAction.CheckTableName(const ATableName: string);
begin
  if ATableName = ES then
    raise EBrookDBAction.Create(Self, SBrookEmptyTableNameError);
end;

class function TBrookCustomDBAction.TableNames: TStrings;
begin
  if not Assigned(_BrookTableNames) then
    TBrookCustomDBAction.InitTableNames;
  Result := TBrookCustomDBAction._BrookTableNames;
end;

class procedure TBrookCustomDBAction.InitTableNames;
begin
  _BrookTableNames := TStringList.Create;
end;

class procedure TBrookCustomDBAction.DoneTableNames;
begin
  FreeAndNil(TBrookCustomDBAction._BrookTableNames);
end;

class procedure TBrookCustomDBAction.SetTableName(const ATableName: string);
begin
  CheckTableName(ATableName);
  TBrookCustomDBAction.TableNames.Add(ClassName + EQ + ATableName);
end;

class function TBrookCustomDBAction.GetTableName: string;
begin
  Result := TBrookCustomDBAction.TableNames.Values[ClassName];
  CheckTableName(Result);
end;

class procedure TBrookCustomDBAction.Register(const ATableName,
  APattern: string; const ADefault: Boolean);
begin
  Register(ATableName, APattern, rmAll, ADefault);
end;

class procedure TBrookCustomDBAction.Register(const ATableName,
  APattern: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean);
begin
  inherited Register(APattern, AMethod, ADefault);
  Self.SetTableName(ATableName);
end;

{ TBrookDBAction }

constructor TBrookDBAction.Create;
begin
  inherited Create;
  FTable := CreateTable;
end;

function TBrookDBAction.CreateTable: TBrookTable;
begin
  Result := TBrookTable.Create;
end;

function TBrookDBAction.GetDataBase: TBrookDataBase;
begin
  Result := FTable.DataBase;
end;

function TBrookDBAction.GetTable: TBrookTable;
begin
  Result := FTable;
  FTable.Name := GetTableName;
end;

procedure TBrookDBAction.SetDataBase(AValue: TBrookDataBase);
begin
  FTable.DataBase := AValue;
end;

procedure TBrookDBAction.SetTable(AValue: TBrookTable);
begin
  FreeAndNil(FTable);
  FTable := AValue;
end;

finalization
  TBrookCustomDBAction.DoneTableNames;

end.
