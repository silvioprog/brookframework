unit person;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookRequestHelper, FPJSON, HTTPDefs, RUtils;

type
  TPerson = class(TBrookDBAction)
  public
    procedure FillFields(ARequest: TRequest); override;
    procedure Request(ARequest: TRequest;{%H-}AResponse: TResponse); override;
  end;

const
  DB_SUCESS = '{ "success": true }';

implementation

procedure TPerson.FillFields(ARequest: TRequest);
var
  I: Integer;
begin
  for I := 0 to Pred(ARequest.ContentFields.Count) do
    Fields.Add(ARequest.ContentFields.Names[I],
      StripHTMLMarkup(ARequest.ContentFields.ValueFromIndex[I]));
end;

procedure TPerson.Request(ARequest: TRequest; AResponse: TResponse);
var
  data: TJSONObject;
  rows, page: Integer;
begin
  case ARequest.Path[0] of
    'list':
      begin
        data := TJSONObject.Create;
        try
          Table.Select('count(*) as items').Open;
          data.Add('total', Table.Field('items').AsInteger);
          Table.Clear.OrderBy('name limit :limit offset :offset').Prepare;
          rows := Fields['rows'].AsInteger;
          page := Fields['page'].AsInteger;
          Table.Param('limit').AsInteger := rows;
          Table.Param('offset').AsInteger := (page - 1) * rows;
          data.Add('rows', Table.Open.Rows);
          Write(data.AsJSON);
          Exit;
        finally
          data.Free;
        end;
      end;
    'add': Table.Insert(Fields).Apply;
    'edit': Table.Get(ARequest.Path[1]).Edit(Fields).Apply;
    'delete': Table.Find(Fields).Delete.Apply;
  else
    Error('Not found.');
  end;
  Write(DB_SUCESS);
end;

initialization
  TPerson.Register('person', '*');

end.
