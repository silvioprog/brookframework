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
  end;

  { TPersonUpdateAction }

  TPersonUpdateAction = class(TjTableUpdateAction)
  end;

  { TPersonDeleteAction }

  TPersonDeleteAction = class(TjTableDeleteAction)
  end;

implementation

initialization
  TPersonListAction.Register('person', '/personlist', rmPost);
  TPersonCreateAction.Register('person', '/personcreate', rmPost);
  TPersonUpdateAction.Register('person', '/personupdate', rmPost);
  TPersonDeleteAction.Register('person', '/persondelete', rmPost);

end.
