unit people;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  EPeople = class(Exception);

  TPeople = class
  private
    Factive: Boolean;
    Fage: Byte;
    Flastupdate: TDateTime;
    Fname: string;
    Fweight: Double;
  public
    constructor Create;
    procedure Validate;
    procedure AssertData(const AExpr: Boolean; const AMsg: string);
  published
    property active: Boolean read Factive write Factive default True;
    property name: string read Fname write Fname;
    property age: Byte read Fage write Fage;
    property weight: Double read Fweight write Fweight;
    property lastupdate: TDateTime read Flastupdate write Flastupdate;
  end;

implementation

constructor TPeople.Create;
begin
  Factive := True;
  Flastupdate := Now;
end;

procedure TPeople.Validate;
begin
  AssertData(Fname <> '', 'Name must not be empty.');
  AssertData(Fage <> 0, 'Age must not be zero.');
  AssertData(Fweight <> 0, 'Weight must not be zero.');
end;

procedure TPeople.AssertData(const AExpr: Boolean; const AMsg: string);
begin
  if not AExpr then
    raise EPeople.CreateFmt('%s: %s', [ClassName, AMsg]);
end;

end.

