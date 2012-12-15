unit Person;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookUtils, BrookDBUtils, BrookActionHelper, MyDBAction,
  JTemplate, DB, SysUtils;

type

  { TIndex }

  TIndex = class(TBrookAction)
  public
    procedure Get; override;
  end;

  { TPersonList }

  TPersonList = class(TMyAction)
  public
    function GridCallback(ADataSet: TDataSet;
      const AWritingType: TBrookHTMLTableWritingState;
      const APosition, AMax: Integer; var AData: string): string; override;
    procedure Get; override;
  end;

  { TPersonNew }

  TPersonNew = class(TMyAction)
  public
    procedure Get; override;
  end;

  { TPersonCreate }

  TPersonCreate = class(TMyAction)
  public
    procedure Post; override;
  end;

  { TPersonShow }

  TPersonShow = class(TMyAction)
  public
    procedure Get; override;
  end;

  { TPersonEdit }

  TPersonEdit = class(TMyAction)
  public
    procedure Get; override;
  end;

  { TPersonUpdate }

  TPersonUpdate = class(TMyAction)
  public
    procedure Post; override;
  end;

  { TPersonDelete }

  TPersonDelete = class(TMyAction)
  public
    procedure Get; override;
  end;

  { TPersonDestroy }

  TPersonDestroy = class(TMyAction)
  public
    procedure Post; override;
  end;

implementation

{ TIndex }

procedure TIndex.Get;
begin
  Redirect(UrlFor(TPersonList));
end;

{ TPersonList }

function TPersonList.GridCallback(ADataSet: TDataSet;
  const AWritingType: TBrookHTMLTableWritingState; const APosition,
  AMax: Integer; var AData: string): string;
const
  TD = #10+#9+#9+#9+#9+
    '<td width="50" style="text-align: center;" >%s</td>';
var
  VId: string;
begin
  Result := AData;
  if (AWritingType = wtBodyTD) and (APosition = AMax) then
  begin
    VId := ADataSet.Fields[0].AsString;
    Result += Format(TD, [LinkTo('Show', TPersonShow, [VId], 'btn btn-small')]);
    Result += Format(TD, [LinkTo('Edit', TPersonEdit, [VId], 'btn btn-small')]);
    Result += Format(TD, [LinkTo('Delete', TPersonDelete, [VId], 'btn btn-small')]);
  end;
end;

procedure TPersonList.Get;
begin
  Load('index');
  Add('grid', GridTo(Table.Open.DataSet));
  Add('new', LinkTo('New', TPersonNew, 'btn btn-large'));
  Display;
end;

{ TPersonNew }

procedure TPersonNew.Get;
begin
  Load('new');
  Add('form', FormTo(Table.Select('name').Open.FieldDefs, TPersonCreate));
  Add('index', LinkTo('Index', TIndex, 'btn'));
  Display;
end;

{ TPersonCreate }

procedure TPersonCreate.Post;
begin
  Redirect(UrlFor(TPersonShow,
    [Table.Insert(Fields).Apply.Close.Open.Last.Field('id').AsString]), 302);
end;

{ TPersonShow }

procedure TPersonShow.Get;
begin
  Load('show');
  Add('name', Table.Find(Values).Field('name').AsString);
  Add('index', LinkTo('Index', TPersonList, 'btn'));
  Display;
end;

{ TPersonEdit }

procedure TPersonEdit.Get;
begin
  Load('edit');
  Add('action', UrlFor(TPersonUpdate, Values));
  Add('name', StrToHtml(Table.Find(Values).Field('name').AsString));
  Add('index', LinkTo('Index', TIndex, 'btn'));
  Display;
end;

{ TPersonUpdate }

procedure TPersonUpdate.Post;
begin
  Table.Find(Values).Edit(Fields).Apply;
  Redirect(UrlFor(TPersonList), 302);
end;

{ TPersonDelete }

procedure TPersonDelete.Get;
begin
  Load('delete');
  Add('name', Table.Find(Values).Field('name').AsString);
  Add('button', ButtonTo('Yes, delete!', TPersonDestroy, Values));
  Add('index', LinkTo('Index', TIndex, 'btn'));
  Display;
end;

{ TPersonDestroy }

procedure TPersonDestroy.Post;
begin
  Table.Find(Values).Delete.Apply;
  Redirect(UrlFor(TPersonList), 302);
end;

initialization
  TIndex.Register('/', rmGet);
  TPersonList.Register('person', '/person', rmGet);
  TPersonNew.Register('person', '/person/new', rmGet);
  TPersonCreate.Register('person', '/person', rmPost);
  TPersonShow.Register('person', '/person/:id', rmGet);
  TPersonEdit.Register('person', '/person/:id/edit', rmGet);
  TPersonUpdate.Register('person', '/person/:id', rmPost);
  TPersonDelete.Register('person', '/person/:id/delete', rmGet);
  TPersonDestroy.Register('person', '/person/:id/destroy', rmPost);

end.
