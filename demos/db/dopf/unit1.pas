unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, person;

type

  { TPersonAction }

  TPersonAction = class(specialize TBrookEntityAction<TPerson>)
  public
    procedure Post; override;
  end;

implementation

procedure TPersonAction.Post;
begin
  Entity.Save;
  Write('Saved: %s', [Entity.Name]);
end;

initialization
  TPersonAction.Register('*');

end.
