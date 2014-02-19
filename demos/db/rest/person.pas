unit person;

{$mode objfpc}{$H+}

interface

uses
  objectstorage, dbutils, SysUtils;

type
  EPerson = class(Exception);

  { TPerson }

  TPerson = class
  private
    FId: Int64;
    FName: string;
  public
    procedure Validate;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

  { TPersonStorage }

  TPersonStorage = class(TObjectStorage)
  public
    constructor Create; reintroduce;
  end;

implementation

{ TPerson }

procedure TPerson.Validate;
begin
  if Trim(FName) = '' then
    raise EPerson.Create('Name must not be empty.');
end;

{ TPersonStorage }

constructor TPersonStorage.Create;
begin
  inherited Create(dbutils.Connection);
end;

end.

