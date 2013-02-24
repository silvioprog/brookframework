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
  TContactsOptions.Register('contacts', '/contacts');
  TContactsRetrieve.Register('contacts', '/contacts');
  TContactsShow.Register('contacts', '/contacts/:id');
  TContactsCreate.Register('contacts', '/contacts');
  TContactsUpdate.Register('contacts', '/contacts/:id');
  TContactsDestroy.Register('contacts', '/contacts/:id');

end.
