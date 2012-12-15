unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TAction1 = class(TBrookAction)
  public
    procedure Get; override;
  end;

  TAction2 = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TAction1.Get;
begin
  Write('<a href="%s">Call Action2</a>', [UrlFor(TAction2, ['foo', '1'])]);
end;

procedure TAction2.Get;
begin
  Write('ID: %d, name: %s', [Values['id'].AsInteger, Values['name'].AsString]);
end;

initialization
  TAction2.Register('/user/:name/:id');
  TAction1.Register('*');

end.
