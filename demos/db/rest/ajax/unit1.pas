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
  TPersonRetrieve.Register('person', '/person');
  TPersonShow.Register('person', '/person/:id');
  TPersonCreate.Register('person', '/person');
  TPersonUpdate.Register('person', '/person/:id');
  TPersonDestroy.Register('person', '/person/:id');

end.
