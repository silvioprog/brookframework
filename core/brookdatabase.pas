(*
  Brook DataBase unit.

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

unit BrookDataBase;

{$i brook.inc}

interface

uses
  BrookClasses, BrookException, BrookConsts, BrookDBConsts, BrookMessages,
  BrookConfigurator, Classes, SysUtils;

type
  TBrookDataBases = class;

  { Handles exceptions for @link(TBrookDataBase). }
  EBrookDataBase = class(EBrook);

  { Handles exceptions for @link(TBrookDataBases). }
  EBrookDataBases = class(EBrook);

  { Is a metaclass for @link(TBrookDataBase) class. }
  TBrookDataBaseClass = class of TBrookDataBase;

  { Is a metaclass for @link(TBrookDataBases) class. }
  TBrookDataBasesClass = class of TBrookDataBases;

  { Offers general abstract features for databases handling. }
  TBrookDataBase = class(TBrookObject)
  private
    FObjects: TFPList;
  protected
    function GetConnection: TObject; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    function GetDatabase: string; virtual; abstract;
    function GetHost: string; virtual; abstract;
    function GetPassword: string; virtual; abstract;
    function GetDriver: string; virtual; abstract;
    function GetUser: string; virtual; abstract;
    procedure SetDatabase(AValue: string); virtual; abstract;
    procedure SetHost(AValue: string); virtual; abstract;
    procedure SetPassword(AValue: string); virtual; abstract;
    procedure SetDriver(AValue: string); virtual; abstract;
    procedure SetUser(AValue: string); virtual; abstract;
    function GetPort: Integer; virtual; abstract;
    procedure SetPort(AValue: Integer); virtual; abstract;
    procedure FreeObjects;
    property Objects: TFPList read FObjects;
  public
    { Creates an instance of a @link(TBrookDataBase) class. }
    constructor Init; virtual;
    { Frees an instance of @link(TBrookDataBase) class. }
    destructor Destroy; override;
    { Creates an instance of a @link(TBrookDataBase) class. }
    class function Create: TBrookDataBase;
    { Register the broker class. }
    class procedure Register;
    { Unregister the broker class. }
    class procedure Unregister;
    { Adds objects that will be automatically freed when the database object is
      destroyed. }
    procedure AddObject(AObject: TObject);
    { Removes objects that would be automatically freed when the database object
      is destroyed. }
    procedure RemoveObject(AObject: TObject);
    { Get the broker library name, exemple: SQLdb, Zeos, UniDAC etc. }
    class function GetLibrary: string; virtual; abstract;
    { Connects to the data base. }
    procedure Connect; virtual; abstract;
    { Disconnects from the data base. }
    procedure Disconnect; virtual; abstract;
    { Checks if a transaction is active. }
    function InTransaction: Boolean; virtual; abstract;
    { Starts a transaction. }
    procedure StartTransaction; virtual; abstract;
    { Commits a transaction. }
    procedure Commit; virtual; abstract;
    { Rollbacks a transaction. }
    procedure Rollback; virtual; abstract;
    { Checks if the database is connected. }
    property Connected: Boolean read GetConnected;
    { Defines the driver name. In Zeos driver is know as "Protocol"; in Unidac,
      "Provider", etc. }
    property Driver: string read GetDriver write SetDriver;
    { Defines the database name or the database path. }
    property Database: string read GetDatabase write SetDatabase;
    { Defines the database user. }
    property User: string read GetUser write SetUser;
    { Defines the database password. }
    property Password: string read GetPassword write SetPassword;
    { Defines the database host. }
    property Host: string read GetHost write SetHost;
    { Defines the database port. }
    property Port: Integer read GetPort write SetPort;
    { Is the instance of the database broker. }
    property Connection: TObject read GetConnection;
  end;

  { Registers and configure database classes. }
  TBrookDataBases = class(TBrookObject)
  private
    FConfigurator: TBrookConfigurator;
    FList: TFPList;
    FCurrent: TBrookDataBase;
    function GetItems(const AIndex: Integer): TBrookDataBaseClass;
    procedure SetItems(const AIndex: Integer; AValue: TBrookDataBaseClass);
  public
    { Creates an instance of a @link(TBrookDataBases) class. }
    constructor Create; virtual;
    { Frees an instance of @link(TBrookDataBases) class. }
    destructor Destroy; override;
    { Registers the service provided by this class. }
    class procedure RegisterService;
    { Unregisters the service  provided by this class. }
    class procedure UnregisterService;
    { Return a instance of this class. }
    class function Service: TBrookDataBases;
    { Creates an database item. }
    function CreateDataBase: TBrookDataBase;
    { Frees the current database item. }
    procedure FreeCurrent;
    { Returns the number of registered databases. }
    function Count: Integer;
    { Finds a database item by its library name. }
    function Find(const ALibrary: string): TBrookDataBaseClass;
    { Returns a database item by its library name. }
    function ItemByLibrary(const ALibrary: string): TBrookDataBaseClass;
    { Adds a database item. }
    procedure Add(AClass: TBrookDataBaseClass);
    { Removes a database item. }
    procedure Remove(AClass: TBrookDataBaseClass);
    { The current database item. }
    property Current: TBrookDataBase read FCurrent write FCurrent;
    { The list of database items. }
    property Items[const AIndex: Integer]: TBrookDataBaseClass read GetItems
      write SetItems;
    { A configurator for database items. }
    property Configurator: TBrookConfigurator read FConfigurator;
  end;

implementation

var
  _BrookDataBaseLibrary: string = ES;
  _BrookDataBasesService: TBrookDataBases = nil;
  _BrookDataBasesServiceClass: TBrookDataBasesClass = nil;

{ TBrookDataBase }

constructor TBrookDataBase.Init;
begin
  FObjects := TFPList.Create;
end;

destructor TBrookDataBase.Destroy;
begin
  FreeObjects;
  FObjects.Free;
  inherited Destroy;
end;

class function TBrookDataBase.Create: TBrookDataBase;
var
  VDbs: TBrookDataBases;
begin
  VDbs := TBrookDataBases.Service;
  Result := VDbs.CreateDataBase;
  VDbs.Configurator.Configure;
end;

procedure TBrookDataBase.AddObject(AObject: TObject);
begin
  FObjects.Add(AObject);
end;

procedure TBrookDataBase.RemoveObject(AObject: TObject);
begin
  FObjects.Remove(AObject);
end;

procedure TBrookDataBase.FreeObjects;
var
  P: Pointer;
begin
  for P in FObjects do
    TObject(P).Free;
end;

class procedure TBrookDataBase.Register;
begin
  TBrookDataBases.Service.Add(Self);
end;

class procedure TBrookDataBase.Unregister;
begin
  TBrookDataBases.Service.Remove(Self);
end;

{ TBrookDataBases }

constructor TBrookDataBases.Create;
begin
  inherited Create;
  FList := TFPList.Create;
  FConfigurator := TBrookConfigurator.Create;
  FConfigurator.AutoLoaded := False;
end;

destructor TBrookDataBases.Destroy;
begin
  FList.Free;
  FConfigurator.Free;
  FreeAndNil(FCurrent);
  inherited Destroy;
end;

function TBrookDataBases.CreateDataBase: TBrookDataBase;
begin
  _BrookDataBaseLibrary := FConfigurator.Load.Values[BROOK_DEFAULT_LIBRARY_PARAM];
  FCurrent := ItemByLibrary(_BrookDataBaseLibrary).Init;
  FConfigurator.Target := FCurrent;
  Result := FCurrent;
end;

procedure TBrookDataBases.FreeCurrent;
begin
  FreeAndNil(FCurrent);
end;

function TBrookDataBases.GetItems(
  const AIndex: Integer): TBrookDataBaseClass;
begin
  Result := TBrookDataBaseClass(FList.Items[AIndex]);
end;

procedure TBrookDataBases.SetItems(const AIndex: Integer;
  AValue: TBrookDataBaseClass);
begin
  FList.Items[AIndex] := AValue;
end;

class procedure TBrookDataBases.RegisterService;
begin
  if Assigned(_BrookDataBasesServiceClass) then
    raise EBrookDataBases.Create(Self, SBrookDataBaseAlreadyRegisteredError);
  _BrookDataBasesServiceClass := Self;
end;

class procedure TBrookDataBases.UnregisterService;
begin
  FreeAndNil(_BrookDataBasesService);
  _BrookDataBasesServiceClass := nil;
end;

class function TBrookDataBases.Service: TBrookDataBases;
begin
  if not Assigned(_BrookDataBasesService) then
  begin
    if not Assigned(_BrookDataBasesServiceClass) then
      raise EBrookDataBases.Create(Self, SBrookNoDataBaseRegisteredError);
    _BrookDataBasesService := _BrookDataBasesServiceClass.Create;
  end;
  Result := _BrookDataBasesService;
end;

function TBrookDataBases.Count: Integer;
begin
  Result := FList.Count;
end;

function TBrookDataBases.Find(const ALibrary: string): TBrookDataBaseClass;
var
  I: Integer;
begin
  for I := 0 to Pred(FList.Count) do
  begin
    Result := Items[I];
    if SameText(Result.GetLibrary, ALibrary) then
      Exit;
  end;
  Result := nil;
end;

function TBrookDataBases.ItemByLibrary(
  const ALibrary: string): TBrookDataBaseClass;
begin
  if ALibrary = ES then
    raise EBrookDataBases.Create(Self, SBrookEmptyLibraryNameError);
  Result := Find(ALibrary);
  if not Assigned(Result) then
    raise EBrookDataBases.CreateFmt(Self, SBrookItemNotFoundError, [ALibrary]);
end;

procedure TBrookDataBases.Add(AClass: TBrookDataBaseClass);
begin
  FList.Add(AClass);
end;

procedure TBrookDataBases.Remove(AClass: TBrookDataBaseClass);
begin
  FList.Remove(AClass);
end;

initialization
  TBrookDataBases.RegisterService;

finalization
  TBrookDataBases.UnregisterService;

end.
