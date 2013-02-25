unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction;

type
  TMyAction = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
begin
  Write(Table.Open.AsJSON);
end;

initialization
  TMyAction.Register('person', '*', 'id,lastupdate');

end.
