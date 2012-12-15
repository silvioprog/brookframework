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
  TPhonesOptions.Register('phones', '/contacts/phones', rmOptions);
  TPhonesRetrieve.Register('phones', '/contacts/:contactid/phones', rmGet);
  TPhonesShow.Register('phones', '/contacts/:contactid/phones/:id', rmGet);
  TPhonesCreate.Register('phones', '/contacts/:contactid/phones', rmPost);
  TPhonesUpdate.Register('phones', '/contacts/:contactid/phones/:id', rmPut);
  TPhonesDestroy.Register('phones', '/contacts/:contactid/phones/:id', rmDelete);

end.
