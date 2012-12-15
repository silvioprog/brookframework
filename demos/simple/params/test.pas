unit test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
var
  I: Integer;
begin
  for I := 0 to Pred(Params.Count) do
    WriteLn(Params.Names[I] + ': ' + Params.Items[I].AsString);
end;

initialization
  // call http://localhost/cgi-bin/cgi1?q1=abc&q2=123
  TMyAction.Register('*');

end.
