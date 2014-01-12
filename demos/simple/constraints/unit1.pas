unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookConstraints;

type

  { TMyConstraint }

  TMyConstraint = class(TBrookConstraint)
  public
    procedure Execute; override;
  end;

  { TMyAction }

  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TMyConstraint }

procedure TMyConstraint.Execute;
begin
  if Action.Params.Count = 0 then
    Stop('Please click <a href="%s">here</a>.', [Action.UrlFor(TMyAction) +
      '?q=abc']);
end;

{ TMyAction }

procedure TMyAction.Get;
begin
  Write('Params: %s', [Params.AsJSON]);
end;

initialization
  TMyAction.Register('*');
  TMyConstraint.Register(TMyAction);

end.
