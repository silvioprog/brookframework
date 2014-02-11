unit Person;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookTable, BrookHttpDefs, BrookActionHelper, BrookUtils,
  BrookDBUtils, JTemplate, DB, SysUtils;

type

  { TCustomDBAction }

  TCustomDBAction = class(TBrookAction)
  private
    FTable: TBrookTable;
  public
    constructor Create(ARequest: TBrookRequest;
      AResponse: TBrookResponse); overload; override;
    destructor Destroy; override;
    property Table: TBrookTable read FTable;
  end;

  { TIndex }

  TIndex = class(TBrookAction)
  public
    procedure Get; override;
  end;

  { TPersonList }

  TPersonList = class(TCustomDBAction)
  public
    function GridCallback(ADataSet: TDataSet;
      const AWritingType: TBrookHTMLTableWritingState;
      const APosition, AMax: Integer; var AData: string): string;
    procedure Get; override;
  end;

  { TPersonNew }

  TPersonNew = class(TCustomDBAction)
  public
    procedure Get; override;
  end;

  { TPersonCreate }

  TPersonCreate = class(TCustomDBAction)
  public
    procedure Post; override;
  end;

  { TPersonShow }

  TPersonShow = class(TCustomDBAction)
  public
    procedure Get; override;
  end;

  { TPersonEdit }

  TPersonEdit = class(TCustomDBAction)
  public
    procedure Get; override;
  end;

  { TPersonUpdate }

  TPersonUpdate = class(TCustomDBAction)
  public
    procedure Post; override;
  end;

  { TPersonDelete }

  TPersonDelete = class(TCustomDBAction)
  public
    procedure Get; override;
  end;

  { TPersonDestroy }

  TPersonDestroy = class(TCustomDBAction)
  public
    procedure Post; override;
  end;

const
  tpl_dir = 'C:\repository\git\brookframework\demos\db\crud\';

implementation

{ TCustomDBAction }

constructor TCustomDBAction.Create(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  inherited Create(ARequest, AResponse);
  FTable := TBrookTable.Create('person');
end;

destructor TCustomDBAction.Destroy;
begin
  FTable.Free;
  inherited Destroy;
end;

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
  TPersonList.Register('/person', rmGet);
  TPersonNew.Register('/person/new', rmGet);
  TPersonCreate.Register('/person', rmPost);
  TPersonShow.Register('/person/:id', rmGet);
  TPersonEdit.Register('/person/:id/edit', rmGet);
  TPersonUpdate.Register('/person/:id', rmPost);
  TPersonDelete.Register('/person/:id/delete', rmGet);
  TPersonDestroy.Register('/person/:id/destroy', rmPost);

end.
