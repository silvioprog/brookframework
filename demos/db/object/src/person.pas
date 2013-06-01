unit Person;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  EPerson = class(Exception);

  TPerson = class
  private
    FActive: Boolean;
    FAge: Byte;
    FLastUpdate: TDateTime;
    FName: string;
    FWeight: Double;
  public
    constructor Create;
    procedure Validate;
    procedure AssertData(const AExpr: Boolean; const AMsg: string);
  published
    property Active: Boolean read FActive write FActive default True;
    property Name: string read FName write FName;
    property Age: Byte read FAge write FAge;
    property Weight: Double read FWeight write FWeight;
    property LastUpdate: TDateTime read FLastUpdate write FLastUpdate;
  end;

implementation

constructor TPerson.Create;
begin
  FActive := True;
  FLastUpdate := Now;
end;

procedure TPerson.Validate;
begin
  AssertData(FName <> '', 'Name must not be empty.');
  AssertData(FAge <> 0, 'Age must not be zero.');
  AssertData(FWeight <> 0, 'Weight must not be zero.');
end;

procedure TPerson.AssertData(const AExpr: Boolean; const AMsg: string);
begin
  if not AExpr then
    raise EPerson.CreateFmt('%s: %s', [ClassName, AMsg]);
end;

end.

