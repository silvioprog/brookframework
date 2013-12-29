unit Person;

{$mode objfpc}{$H+}

interface

uses
  jTableActns, BrookUtils;

type

  { TPersonListAction }

  TPersonListAction = class(TjTableListAction)
  end;

  { TPersonCreateAction }

  TPersonCreateAction = class(TjTableCreateAction)
  public
    procedure Post; override;
  end;

  { TPersonUpdateAction }

  TPersonUpdateAction = class(TjTableUpdateAction)
  public
    procedure Post; override;
  end;

  { TPersonDeleteAction }

  TPersonDeleteAction = class(TjTableDeleteAction)
  public
    procedure Post; override;
  end;

implementation

{ TPersonCreateAction }

procedure TPersonCreateAction.Post;
begin
  Stop('This is just a demo, so can''t be changed.');
end;

{ TPersonUpdateAction }

procedure TPersonUpdateAction.Post;
begin
  Stop('This is just a demo, so can''t be changed.');
end;

{ TPersonDeleteAction }

procedure TPersonDeleteAction.Post;
begin
  Stop('This is just a demo, so can''t be changed.');
end;

initialization
  TPersonListAction.Register('person', '/personlist', rmPost);
  TPersonCreateAction.Register('person', '/personcreate', rmPost);
  TPersonUpdateAction.Register('person', '/personupdate', rmPost);
  TPersonDeleteAction.Register('person', '/persondelete', rmPost);

end.
