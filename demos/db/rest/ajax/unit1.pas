unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookRESTActions, BrookUtils;

type
  TPersonRetrieve = class(TBrookRetrieveAction)
  end;

  TPersonShow = class(TBrookShowAction)
  end;

  TPersonCreate = class(TBrookCreateAction)
  end;

  TPersonUpdate = class(TBrookUpdateAction)
  end;

  TPersonDestroy = class(TBrookDestroyAction)
  end;

implementation

initialization
  TPersonRetrieve.Register('person', '/person', rmGet);
  TPersonShow.Register('person', '/person/:id', rmGet);
  TPersonCreate.Register('person', '/person', rmPost);
  TPersonUpdate.Register('person', '/person/:id', rmPut);
  TPersonDestroy.Register('person', '/person/:id', rmDelete);

end.
