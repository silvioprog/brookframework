unit Contacts;

{$mode objfpc}{$H+}

interface

uses
  BrookRESTActions, BrookUtils;

type
  TContactsOptions = class(TBrookOptionsAction)
  end;

  TContactsRetrieve = class(TBrookRetrieveAction)
  end;

  TContactsShow = class(TBrookShowAction)
  end;

  TContactsCreate = class(TBrookCreateAction)
  end;

  TContactsUpdate = class(TBrookUpdateAction)
  end;

  TContactsDestroy = class(TBrookDestroyAction)
  end;

implementation

initialization
  TContactsOptions.Register('contacts', '/contacts', rmOptions);
  TContactsRetrieve.Register('contacts', '/contacts', rmGet);
  TContactsShow.Register('contacts', '/contacts/:id', rmGet);
  TContactsCreate.Register('contacts', '/contacts', rmPost);
  TContactsUpdate.Register('contacts', '/contacts/:id', rmPut);
  TContactsDestroy.Register('contacts', '/contacts/:id', rmDelete);

end.
