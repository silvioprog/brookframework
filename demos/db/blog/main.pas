unit main;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookDBAction, BrookUtils, BrookDBUtils, BrookHTTPConsts,
  BrookActionHelper, BrookConsts, JTemplate, RUtils, Brokers, SysUtils,
  HTTPDefs, DB, FPJSON;

type

  { TView }

  TView = class(TBrookDBAction)
  private
    FTemplate: TJTemplateStream;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FillFields(ARequest: TRequest); override;
    procedure DoRequest(ARequest: TRequest; AResponse: TResponse); override;
    function Link(const ACaption: string; AActionClass: TBrookActionClass;
      const AParams: array of string; const AClass: string = ES): string;
    procedure Add(const AName, AValue: string);
    procedure Load(const AFileName: string);
    property Template: TJTemplateStream read FTemplate;
  end;

  { THome }

  THome = class(TView)
  protected
    function GridCallback(ADataSet: TDataSet;
      const AWritingType: TBrookHTMLTableWritingState;
      const APosition, AMax: Integer; var AData: string): string;
  public
    procedure Get; override;
  end;

  { TPostAdd }

  TPostAdd = class(TView)
  public
    procedure Get; override;
    procedure Post; override;
  end;

  { TPostEdit }

  TPostEdit = class(TView)
  public
    procedure Get; override;
    procedure Post; override;
  end;

  { TPostRemove }

  TPostRemove = class(TView)
  public
    procedure Get; override;
  end;

implementation

{ TView }

constructor TView.Create;
begin
  inherited Create;
  FTemplate := TJTemplateStream.Create;
  FTemplate.Parser.HtmlSupports := False;
end;

destructor TView.Destroy;
begin
  FTemplate.Free;
  inherited Destroy;
end;

procedure TView.FillFields(ARequest: TRequest);
var
  I: Integer;
begin
  for I := 0 to Pred(ARequest.ContentFields.Count) do
    Fields.Add(ARequest.ContentFields.Names[I],
      StripHTMLMarkup(ARequest.ContentFields.ValueFromIndex[I]));
end;

procedure TView.DoRequest(ARequest: TRequest; AResponse: TResponse);
begin
  inherited DoRequest(ARequest, AResponse);
  FTemplate.Parser.Replace;
  Write(FTemplate.Parser.Content);
end;

function TView.Link(const ACaption: string; AActionClass: TBrookActionClass;
  const AParams: array of string; const AClass: string): string;
begin
  Result := '<a href="' + UrlFor(AActionClass, AParams) + '" class="' + AClass +
    '" role="button">' + ACaption + '</a>';
end;

procedure TView.Add(const AName, AValue: string);
begin
  Template.Parser.Fields.Add(AName, AValue);
end;

procedure TView.Load(const AFileName: string);
begin
  FTemplate.LoadFromFile(PUBLIC_HTML + AFileName + '.html');
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
        '<td width="140" style="text-align: center;">Action</td>';
      wtBodyTD: Result := AData + LF + HT + '<td>' + LF + HT + HT +
        LinkTo('Edit', TPostEdit, [ID], 'btn btn-sm btn-default') + LF + HT + HT +
        LinkTo('Remove', TPostRemove, [ID], 'btn btn-sm btn-default') + LF + HT +
          '</td>' + LF;
    end;
  end
  else
    Result := AData;
end;

procedure THome.Get;
begin
  Load('home');
  Add('menu', UrlFor(TPostAdd, ['new']));
  Add('post', BrookDataSetToHTMLTable(Table.Open.DataSet, [],
    'table table-bordered table-hover', 1, [], @GridCallback));
end;

{ TPostAdd }

procedure TPostAdd.Get;
begin
  Load('newpost');
  Add('title', ES);
  Add('author', ES);
  Add('post', ES);
  Add('action', UrlFor(TPostAdd));
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
    Load('newpost');
    BrookJSONCopy(Row, Template.Parser.Fields);
    Add('action', UrlFor(TPostEdit, Row));
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
  THome.Register('post', '/');
  TPostAdd.Register('post', '/post/add');
  TPostEdit.Register('post', '/post/edit/:id');
  TPostRemove.Register('post', '/post/remove/:id');

end.
