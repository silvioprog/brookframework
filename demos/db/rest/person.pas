unit person;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

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

implementation

{ TPerson }

procedure TPerson.Validate;
begin
  if Trim(FName) = '' then
    raise EPerson.Create('Field "Name" must not be empty.');
end;

end.

