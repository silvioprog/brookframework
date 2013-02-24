unit Phones;

{$mode objfpc}{$H+}

interface

uses
  BrookRESTActions, BrookUtils;

type
  TPhonesOptions = class(TBrookOptionsAction)
  end;

  TPhonesRetrieve = class(TBrookRetrieveAction)
  end;

  TPhonesShow = class(TBrookShowAction)
  end;

  TPhonesCreate = class(TBrookCreateAction)
  end;

  TPhonesUpdate = class(TBrookUpdateAction)
  end;

  TPhonesDestroy = class(TBrookDestroyAction)
  end;

implementation

initialization
  TPhonesOptions.Register('phones', '/contacts/phones');
  TPhonesRetrieve.Register('phones', '/contacts/:contactid/phones');
  TPhonesShow.Register('phones', '/contacts/:contactid/phones/:id');
  TPhonesCreate.Register('phones', '/contacts/:contactid/phones');
  TPhonesUpdate.Register('phones', '/contacts/:contactid/phones/:id');
  TPhonesDestroy.Register('phones', '/contacts/:contactid/phones/:id');

end.
