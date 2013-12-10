unit Person;

{$mode objfpc}{$H+}

interface

uses
  jqGridRESTActions;

type

  TPersonRetrieveAction = class(TjqGridRetrieveAction)
  end;

  TPersonCreateAction = class(TjqGridCreateAction)
  end;

  TPersonUpdateAction = class(TjqGridUpdateAction)
  end;

  TPersonDestroyAction = class(TjqGridDestroyAction)
  end;

implementation

initialization
  TPersonRetrieveAction.Register('person', '/person');
  TPersonCreateAction.Register('person', '/person');
  TPersonUpdateAction.Register('person', '/person/:id');
  TPersonDestroyAction.Register('person', '/person/:id');

end.
