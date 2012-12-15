unit default;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TTest = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TTest.Get;
begin
  Write('Default action');
end;

initialization
  TTest.Register('*', True);

end.
