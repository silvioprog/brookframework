unit personactns;

{$mode objfpc}{$H+}

interface

uses
  jTableActns, dbutils, personobjs;

type

  { TPersonOpf }

  TPersonOpf = class(specialize TjTableGOpf<TPerson>)
  public
    constructor Create; overload;
  end;

  { TPersonListAction }

  TPersonListAction = class(specialize TjTableGListAction<TPersonOpf, TPerson>)
  end;

  { TPersonCreateAction }

  TPersonCreateAction = class(specialize TjTableGCreateAction<TPersonOpf, TPerson>)
  end;

  { TPersonUpdateAction }

  TPersonUpdateAction = class(specialize TjTableGUpdateAction<TPersonOpf, TPerson>)
  end;

  { TPersonDeleteAction }

  TPersonDeleteAction = class(specialize TjTableGDeleteAction<TPersonOpf, TPerson>)
  end;

implementation

{ TPersonOpf }

constructor TPersonOpf.Create;
begin
  inherited Create(dbutils.con, 'person');
end;

initialization
  TPersonListAction.Register('/personlist');
  TPersonCreateAction.Register('/personcreate');
  TPersonUpdateAction.Register('/personupdate');
  TPersonDeleteAction.Register('/persondelete');

end.
