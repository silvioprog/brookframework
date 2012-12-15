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
  Write('<a href="%s">Call Action2</a>', [UrlFor(TAction2)]);
end;

procedure TAction2.Get;
begin
  Write('Action2');
end;

initialization
  TAction2.Register('/i/m/action/2');
  TAction1.Register('*');

end.
