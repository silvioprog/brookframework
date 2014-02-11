unit Person;

{$mode objfpc}{$H+}

interface

uses
  jTableActns, BrookUtils;

type

  { TPersonListAction }

  TPersonListAction = class(TjTableListAction)
  public
    constructor Create; override;
  end;

  { TPersonCreateAction }

  TPersonCreateAction = class(TjTableCreateAction)
  public
    constructor Create; override;
    procedure Post; override;
  end;

  { TPersonUpdateAction }

  TPersonUpdateAction = class(TjTableUpdateAction)
  public
    constructor Create; override;
    procedure Post; override;
  end;

  { TPersonDeleteAction }

  TPersonDeleteAction = class(TjTableDeleteAction)
  public
    constructor Create; override;
    procedure Post; override;
  end;

implementation

{ TPersonListAction }

constructor TPersonListAction.Create;
begin
  inherited Create;
  Table.Name := 'person';
end;

{ TPersonCreateAction }

constructor TPersonCreateAction.Create;
begin
  inherited Create;
  Table.Name := 'person';
end;

procedure TPersonCreateAction.Post;
begin
  Stop('This is just a demo, so can''t be changed.');
end;

{ TPersonUpdateAction }

constructor TPersonUpdateAction.Create;
begin
  inherited Create;
  Table.Name := 'person';
end;

procedure TPersonUpdateAction.Post;
begin
  Stop('This is just a demo, so can''t be changed.');
end;

{ TPersonDeleteAction }

constructor TPersonDeleteAction.Create;
begin
  inherited Create;
  Table.Name := 'person';
end;

procedure TPersonDeleteAction.Post;
begin
  Stop('This is just a demo, so can''t be changed.');
end;

initialization
  TPersonListAction.Register('/personlist', rmPost);
  TPersonCreateAction.Register('/personcreate', rmPost);
  TPersonUpdateAction.Register('/personupdate', rmPost);
  TPersonDeleteAction.Register('/persondelete', rmPost);

end.
