unit main;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction,BrookDBUtils,BrookHTTPConsts,BrookTable,JTemplate,HTTPDefs,DB,
  fpjson;

type

  { TActionView }

  TActionView = class(TBrookDBAction)
  private
    FTemplate: TJTemplate;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DoRequest(ARequest: TRequest; AResponse: TResponse); override;
    procedure LoadHtml(const AHtml: string);
    property Template: TJTemplate read FTemplate;
  end;

  { THome }

  THome = class(TActionView)
    function AddEditRemoveButton(ADataSet: TDataSet;
      const AWritingType: TBrookHTMLTableWritingState; const APosition,
      AMax: Integer; var AData: string): string;
  public
    procedure Get; override;
  end;

  { TPostAdd }

  TPostAdd = class(TActionView)
  public
    procedure Get; override;
    procedure Post; override;
  end;

  { TPostEdit }

  TPostEdit = class(TActionView)
  public
    procedure Get; override;
    procedure Post; override;
  end;

  { TPostRemove }

  TPostRemove = class(TActionView)
  public
    procedure Get; override;
  end;

implementation

uses
  SysUtils, Brokers, BrookConsts, BrookActionHelper;

{ TActionView }

constructor TActionView.Create;
begin
  inherited Create;
  FTemplate := TJTemplate.Create;
  FTemplate.HTMLSupports := False;
end;

destructor TActionView.Destroy;
begin
  FTemplate.Free;
  inherited Destroy;
end;

procedure TActionView.DoRequest(ARequest: TRequest; AResponse: TResponse);
begin
  inherited DoRequest(ARequest, AResponse);
  FTemplate.Replace;
  Write(FTemplate.Content);
end;

procedure TActionView.LoadHtml(const AHtml: string);
begin
  FTemplate.LoadFromFile(publichtml + AHtml + '.html');
end;

{ THome }

function THome.AddEditRemoveButton(ADataSet: TDataSet;
  const AWritingType: TBrookHTMLTableWritingState; const APosition,
  AMax: Integer; var AData: string): string;
var
  ID: String;
begin
  if (APosition = AMax) and (AWritingType in [wtHeadTD,wtBodyTD]) then begin
    ID := ADataSet.FieldByName('id').AsString;
    case AWritingType of
      wtHeadTD: Result := AData + LF + HT + HT + '<td>Action</td>';
      wtBodyTD: Result := AData + LF + HT + '<td>'
        + LF + HT + HT + Link('Edit','/post/edit/' + ID)
        + LF + HT + HT + Link('Remove','/post/remove/' + ID)
        + LF + HT + '</td>'
        + LF;
    end;
  end else
    Result := AData;
end;

procedure THome.Get;
begin
  LoadHtml('home');
  Template.Fields.Add('menu', UrlFor(TPostAdd, ['new']));
  Template.Fields.Add('post', BrookDataSetToHTMLTable(Table.Open.DataSet, [],
    'table table-bordered table-hover',1,[],@AddEditRemoveButton));
end;

{ TPostAdd }

procedure TPostAdd.Get;
begin
  LoadHtml('newpost');
  Template.Fields.Add('title','');
  Template.Fields.Add('author','');
  Template.Fields.Add('post','');
  Template.Fields.Add('action','/post/add');
end;

procedure TPostAdd.Post;
begin
  Table.Insert(Fields).Apply;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

{ TPostEdit }

procedure TPostEdit.Get;
var
  ID: TJSONStringType;
  Row: TBrookTable;
begin
  ID := Values['id'].AsString;
  Row := Table.Get('id',ID);
  if not Row.EOF then begin
    LoadHtml('newpost');
    Template.Fields.Add('title',Row['title'].AsString);
    Template.Fields.Add('author',Row['author'].AsString);
    Template.Fields.Add('post',Row['post'].AsString);
    Template.Fields.Add('action','/post/edit/' + ID);
  end else begin
    Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
  end;
end;

procedure TPostEdit.Post;
var
  ID: TJSONStringType;
  Row: TBrookTable;
begin
  ID := Values['id'].AsString;
  Row := Table.Get('id',ID);
  if not Row.EOF then begin
    Row.Edit(Fields).Apply;
  end;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

{ TPostRemove }

procedure TPostRemove.Get;
var
  ID: TJSONStringType;
  Row: TBrookTable;
begin
  ID := Values['id'].AsString;
  Row := Table.Get('id',ID);
  if not Row.EOF then begin
    Row.Delete.Apply;
  end;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

initialization
  THome.Register('post', '*', True);
  TPostAdd.Register('post', '/post/add/');
  TPostEdit.Register('post', '/post/edit/:id');
  TPostRemove.Register('post', '/post/remove/:id');

end.
