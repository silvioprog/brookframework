unit KendoRESTActions;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookRESTActions, FPJSON;

type
  TKendoRetrieveAction = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

  TKendoCreateAction = class(TBrookCreateAction)
  end;

  TKendoUpdateAction = class(TBrookUpdateAction)
  end;

  TKendoDestroyAction = class(TBrookDestroyAction)
  end;

implementation

procedure TKendoRetrieveAction.Get;
var
  VData: TJSONObject;
begin
  VData := TJSONObject.Create;
  try
    Table.Select('count(*) as items').Open;
    VData.Add('count', Table.Field('items').AsInteger);
    Table.Clear.OrderBy('id limit :limit offset :offset').Prepare;
    Table.Param('limit').AsInteger := Params['take'].AsInteger;
    Table.Param('offset').AsInteger := Params['skip'].AsInteger;
    VData.Add('items', Table.Open.Rows);
    Write(VData.AsJSON);
  finally
    VData.Free;
  end;
end;

end.

