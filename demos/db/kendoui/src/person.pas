unit Person;

{$mode objfpc}{$H+}

interface

uses
  BrookUtils, KendoRESTActions;

type
  TPersonRetrieveAction = class(TKendoRetrieveAction)
  end;

  TPersonCreateAction = class(TKendoCreateAction)
  end;

  TPersonUpdateAction = class(TKendoUpdateAction)
  end;

  TPersonDestroyAction = class(TKendoDestroyAction)
  end;

implementation

initialization
  TPersonRetrieveAction.Register('person', '/person', rmGet);
  TPersonCreateAction.Register('person', '/person', rmPost);
  TPersonUpdateAction.Register('person', '/person/:id', rmPut);
  TPersonDestroyAction.Register('person', '/person/:id', rmDelete);

end.
