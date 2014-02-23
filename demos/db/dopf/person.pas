unit person;

{$mode objfpc}{$H+}

interface

uses
  dbutils, dOPF, SysUtils;

type
  EPerson = class(Exception);

  { TPerson }

  TPerson = class
  private
    FId: Int64;
    FName: string;
  public
    procedure Validate;
    procedure Save;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

implementation

{ TPerson }

procedure TPerson.Validate;
begin
  if Trim(FName) = '' then
    raise EPerson.Create('Name must not be empty.');
end;

procedure TPerson.Save;
var
  q: TdQuery;
begin
  Validate;
  q := TdQuery.Create(dbutils.Connection);
  try
    q.SQL.Text := 'insert into person (name) values (:name)';
    q.SetParams(Self).Execute.Apply;
  finally
    q.Free;
  end;
end;

end.

