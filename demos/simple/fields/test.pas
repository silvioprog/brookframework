unit test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
    procedure Post; override;
  end;

implementation

procedure TMyAction.Get;
begin
  Render('form.html');
end;

procedure TMyAction.Post;
var
  I: Integer;
begin
  for I := 0 to Pred(Fields.Count) do
    WriteLn(Fields.Names[I] + ': ' + Fields.Items[I].AsString);
end;

initialization
  TMyAction.Register('*');

end.
