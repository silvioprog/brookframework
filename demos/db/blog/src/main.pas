unit main;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookUtils, BrookDBUtils, BrookHTTPConsts, BrookTable,
  BrookConsts, BrookActionHelper, Brokers, JTemplate, SysUtils, HTTPDefs, DB,
  FPJSON;

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
    function GridCallback(ADataSet: TDataSet;
      const AWritingType: TBrookHTMLTableWritingState;
      const APosition, AMax: Integer; var AData: string): string;
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
  FTemplate.LoadFromFile(PUBLIC_HTML + AHtml + '.html');
end;

{ THome }

function THome.GridCallback(ADataSet: TDataSet;
  const AWritingType: TBrookHTMLTableWritingState;
  const APosition, AMax: Integer; var AData: string): string;
var
  ID: string;
begin
  if (APosition = AMax) and (AWritingType in [wtHeadTD, wtBodyTD]) then
  begin
    ID := ADataSet.FieldByName('id').AsString;
    case AWritingType of
      wtHeadTD: Result := AData + LF + HT + HT +
        '<td width="120" style="text-align: center;">Action</td>';
      wtBodyTD: Result := AData + LF + HT + '<td>' + LF + HT + HT +
        LinkTo('Edit', TPostEdit, [ID], 'btn btn-small') + LF + HT + HT +
        LinkTo('Remove', TPostRemove, [ID], 'btn btn-small') + LF + HT +
          '</td>' + LF;
    end;
  end
  else
    Result := AData;
end;

procedure THome.Get;
begin
  LoadHtml('home');
  Template.Fields.Add('menu', UrlFor(TPostAdd, ['new']));
  Template.Fields.Add('post', BrookDataSetToHTMLTable(Table.Open.DataSet, [],
    'table table-bordered table-hover', 1, [], @GridCallback));
end;

{ TPostAdd }

procedure TPostAdd.Get;
begin
  LoadHtml('newpost');
  Template.Fields.Add('title', ES);
  Template.Fields.Add('author', ES);
  Template.Fields.Add('post', ES);
  Template.Fields.Add('action', UrlFor(TPostAdd));
end;

procedure TPostAdd.Post;
begin
  Table.Insert(Fields).Apply;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

{ TPostEdit }

procedure TPostEdit.Get;
var
  Row: TJSONObject;
begin
  Table.Find(Values).GetRow(Row);
  try
    LoadHtml('newpost');
    BrookJSONCopy(Row, Template.Fields);
    Template.Fields.Add('action', UrlFor(TPostEdit, Row));
  finally
    FreeAndNil(Row);
  end;
end;

procedure TPostEdit.Post;
begin
  Table.Find(Values).Edit(Fields).Apply;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

{ TPostRemove }

procedure TPostRemove.Get;
begin
  Table.Find(Values).Delete.Apply;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

initialization
  THome.Register('post', '*', True);
  TPostAdd.Register('post', '/post/add/');
  TPostEdit.Register('post', '/post/edit/:id');
  TPostRemove.Register('post', '/post/remove/:id');

end.
