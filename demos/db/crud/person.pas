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

const
  tpl_dir = 'C:\repository\git\brookframework\demos\db\crud\';

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
    Result += Format(TD, [LinkTo('Show', TPersonShow, [VId])]);
    Result += Format(TD, [LinkTo('Edit', TPersonEdit, [VId])]);
    Result += Format(TD, [LinkTo('Delete', TPersonDelete, [VId])]);
  end;
end;

procedure TPersonList.Get;
var
  VTemplate: TJTemplateStream;
begin
  VTemplate := TJTemplateStream.Create;
  try
    VTemplate.Parser.HtmlSupports := False;
    VTemplate.LoadFromFile(tpl_dir + 'index.html');
    VTemplate.Parser.Fields.Add('grid', BrookDataSetToHTML5Table(
      Table.Open.DataSet, [], '', '', '', 1, [],
      @GridCallback));
    VTemplate.Parser.Fields.Add('new', LinkTo('New', TPersonNew));
    VTemplate.Parser.Replace;
    Write(VTemplate.Parser.Content);
  finally
    VTemplate.Free;
  end;
end;

{ TPersonNew }

procedure TPersonNew.Get;
var
  VTemplate: TJTemplateStream;
begin
  VTemplate := TJTemplateStream.Create;
  try
    VTemplate.Parser.HtmlSupports := False;
    VTemplate.LoadFromFile(tpl_dir + 'new.html');
    VTemplate.Parser.Fields.Add('form', BrookFieldDefsToHTMLForm(
      Table.Select('name').Open.FieldDefs, UrlFor(TPersonCreate), 'post',
      False, ''));
    VTemplate.Parser.Fields.Add('index', LinkTo('Index', TIndex));
    VTemplate.Parser.Replace;
    Write(VTemplate.Parser.Content);
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
  VTemplate: TJTemplateStream;
begin
  VTemplate := TJTemplateStream.Create;
  try
    VTemplate.Parser.HtmlSupports := False;
    VTemplate.LoadFromFile(tpl_dir + 'show.html');
    VTemplate.Parser.Fields.Add('name',
      Table.Find(Values).Field('name').AsString);
    VTemplate.Parser.Fields.Add('index', LinkTo('Index', TPersonList));
    VTemplate.Parser.Replace;
    Write(VTemplate.Parser.Content);
  finally
    VTemplate.Free;
  end;
end;

{ TPersonEdit }

procedure TPersonEdit.Get;
var
  VTemplate: TJTemplateStream;
begin
  VTemplate := TJTemplateStream.Create;
  try
    VTemplate.Parser.HtmlSupports := False;
    VTemplate.LoadFromFile(tpl_dir + 'edit.html');
    VTemplate.Parser.Fields.Add('action', UrlFor(TPersonUpdate, Values));
    VTemplate.Parser.Fields.Add('name',
      StrToHtml(Table.Find(Values).Field('name').AsString));
    VTemplate.Parser.Fields.Add('index', LinkTo('Index', TIndex));
    VTemplate.Parser.Replace;
    Write(VTemplate.Parser.Content);
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
  VTemplate: TJTemplateStream;
begin
  VTemplate := TJTemplateStream.Create;
  try
    VTemplate.Parser.HtmlSupports := False;
    VTemplate.LoadFromFile(tpl_dir + 'delete.html');
    VTemplate.Parser.Fields.Add('name',
      Table.Find(Values).Field('name').AsString);
    VTemplate.Parser.Fields.Add('button',
      ButtonTo('Yes, delete!', TPersonDestroy, Values));
    VTemplate.Parser.Fields.Add('index', LinkTo('Index', TIndex));
    VTemplate.Parser.Replace;
    Write(VTemplate.Parser.Content);
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
