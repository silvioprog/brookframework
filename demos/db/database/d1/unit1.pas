unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookDataBase;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
var
  db: TBrookDataBase;
begin
  db := TBrookDataBase.Create;
  db.Connect;
  if db.Connected then
    Write('Connected.')
  else
    Write('Disconnected.');
end;

initialization
  TMyAction.Register('*');

end.
