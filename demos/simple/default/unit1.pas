unit Unit1;

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

{ TAction1 }

procedure TAction1.Get;
begin
  Write('TAction1 - Default.');
end;

{ TAction2 }

procedure TAction2.Get;
begin
  Write('TAction2');
end;

initialization
  TAction1.Register('*', True);
  TAction2.Register('/action2');

end.
