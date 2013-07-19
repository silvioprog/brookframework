unit KendoRESTActions;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookRESTActions, BrookRequestHelper, BrookJSONHelper,
  BrookDBConsts, HTTPDefs, FPJSON;

type
  TKendoRetrieveAction = class(TBrookDBAction)
  public
    procedure Request(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TKendoCreateAction = class(TBrookCreateAction)
  end;

  TKendoUpdateAction = class(TBrookUpdateAction)
  end;

  TKendoDestroyAction = class(TBrookDestroyAction)
  end;

implementation

procedure TKendoRetrieveAction.Request(ARequest: TRequest; AResponse: TResponse);
var
  VData: TJSONObject;
begin
  if ARequest.IsGet and Params.Exists('take') and Params.Exists('skip') then
  begin
    VData := TJSONObject.Create;
    try
      Table.Select('count(*) as items').Open;
      VData.Add('count', Table.Field('items').AsInteger);
      Table.Clear.OrderBy(BROOK_DEFAULT_KEY_NAME +
        ' limit :take offset :skip');
      if Values.Count > 0 then
        Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values)
      else
        Table.Prepare;
      Table.Param('take').AsInteger := Params['take'].AsInteger;
      Table.Param('skip').AsInteger := Params['skip'].AsInteger;
      VData.Add('items', Table.Open.Rows);
      Write(VData.AsJSON);
    finally
      VData.Free;
    end;
  end
  else
    inherited Request(ARequest, AResponse);
end;

end.

