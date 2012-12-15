unit Person;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookDBAction, BrookActionHelper, BrookUtils, BrookDBUtils,
  JTemplate, DB, SysUtils;

type

  { TIndex }

  TIndex = class(TBrookAction)
  public
    procedure Get; override;
  end;

  { TPersonList }

  TPersonList = class(TBrookDBAction)
  public
    function GridCallback(ADataSet: TDataSet;
      const AWritingType: TBrookHTMLTableWritingState;
      const APosition, AMax: Integer; var AData: string): string;
    procedure Get; override;
  end;

  { TPersonNew }

  TPersonNew = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

  { TPersonCreate }

  TPersonCreate = class(TBrookDBAction)
  public
    procedure Post; override;
  end;

  { TPersonShow }

  TPersonShow = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

  { TPersonEdit }

  TPersonEdit = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

  { TPersonUpdate }

  TPersonUpdate = class(TBrookDBAction)
  public
    procedure Post; override;
  end;

  { TPersonDelete }

  TPersonDelete = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

  { TPersonDestroy }

  TPersonDestroy = class(TBrookDBAction)
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
var
  VTemplate: TJTemplate;
begin
  VTemplate := TJTemplate.Create;
  try
    VTemplate.HTMLSupports := False;
    VTemplate.LoadFromFile('index.html');
    VTemplate.Fields.Add('grid', BrookDataSetToHTML5Table(Table.Open.DataSet,
      [], '', '', 'table table-bordered table-hover', 1, [], @GridCallback));
    VTemplate.Fields.Add('new', LinkTo('New', TPersonNew, 'btn btn-large'));
    VTemplate.Replace;
    Write(VTemplate.Content);
  finally
    VTemplate.Free;
  end;
end;

{ TPersonNew }

procedure TPersonNew.Get;
var
  VTemplate: TJTemplate;
begin
  VTemplate := TJTemplate.Create;
  try
    VTemplate.HTMLSupports := False;
    VTemplate.LoadFromFile('new.html');
    VTemplate.Fields.Add('form', BrookFieldDefsToHTMLForm(
      Table.Select('name').Open.FieldDefs, UrlFor(TPersonCreate), 'post',
      False, 'form'));
    VTemplate.Fields.Add('index', LinkTo('Index', TIndex, 'btn'));
    VTemplate.Replace;
    Write(VTemplate.Content);
  finally
    VTemplate.Free;
  end;
end;

{ TPersonCreate }

procedure TPersonCreate.Post;
begin
  Redirect(UrlFor(TPersonShow,
    [Table.Insert(Fields).Apply.Open.Last.Field('id').AsString]), 302);
end;

{ TPersonShow }

procedure TPersonShow.Get;
var
  VTemplate: TJTemplate;
begin
  VTemplate := TJTemplate.Create;
  try
    VTemplate.HTMLSupports := False;
    VTemplate.LoadFromFile('show.html');
    VTemplate.Fields.Add('name', Table.Find(Values).Field('name').AsString);
    VTemplate.Fields.Add('index', LinkTo('Index', TPersonList, 'btn'));
    VTemplate.Replace;
    Write(VTemplate.Content);
  finally
    VTemplate.Free;
  end;
end;

{ TPersonEdit }

procedure TPersonEdit.Get;
var
  VTemplate: TJTemplate;
begin
  VTemplate := TJTemplate.Create;
  try
    VTemplate.HTMLSupports := False;
    VTemplate.LoadFromFile('edit.html');
    VTemplate.Fields.Add('action', UrlFor(TPersonUpdate, Values));
    VTemplate.Fields.Add('name', StrToHtml(Table.Find(Values).Field('name').AsString));
    VTemplate.Fields.Add('index', LinkTo('Index', TIndex, 'btn'));
    VTemplate.Replace;
    Write(VTemplate.Content);
  finally
    VTemplate.Free;
  end;
end;

{ TPersonUpdate }

procedure TPersonUpdate.Post;
begin
  Table.Find(Values).Edit(Fields).Apply;
  Redirect(UrlFor(TPersonList), 302);
end;

{ TPersonDelete }

procedure TPersonDelete.Get;
var
  VTemplate: TJTemplate;
begin
  VTemplate := TJTemplate.Create;
  try
    VTemplate.HTMLSupports := False;
    VTemplate.LoadFromFile('delete.html');
    VTemplate.Fields.Add('name', Table.Find(Values).Field('name').AsString);
    VTemplate.Fields.Add('button', ButtonTo('Yes, delete!', TPersonDestroy, Values));
    VTemplate.Fields.Add('index', LinkTo('Index', TIndex, 'btn'));
    VTemplate.Replace;
    Write(VTemplate.Content);
  finally
    VTemplate.Free;
  end;
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
