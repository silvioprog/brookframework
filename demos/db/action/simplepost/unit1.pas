unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction;

type
  TMyAction = class(TBrookDBAction)
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
begin
  Write(Table.Insert(Fields).Apply.Open.AsJSON);
end;

initialization
  TMyAction.Register('person', '*');

end.
