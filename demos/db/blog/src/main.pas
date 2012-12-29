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

  { TPost }

  TPost = class(TActionView)
  public
    procedure Get; override;
    procedure Post; override;
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
        + LF + HT + HT + Link('Edit','/post/?action=edit&id=' + ID)
        + LF + HT + HT + Link('Remove','/post/?action=remove&id=' + ID)
        + LF + HT + '</td>'
        + LF;
    end;
  end else
    Result := AData;
end;

procedure THome.Get;
begin
  LoadHtml('home');
  Template.Fields.Clear;
  Template.Fields.Add('menu', UrlFor(TPost, ['new']));
  Template.Fields.Add('post', BrookDataSetToHTMLTable(Table.Open.DataSet, [],
    'table table-bordered table-hover',1,[],@AddEditRemoveButton));
end;

{ TPost }

procedure TPost.Get;
var
  Action,ID: TJSONData;
  Row: TBrookTable;
begin
  Template.Fields.Clear;

  Action := Params.Find('action');
  ID := Params.Find('id');
  if Assigned(Action) and Assigned(ID) then begin
    case Action.AsString of
      'edit': begin
        Row := Table.Get('id',ID.AsString);
        Template.Fields.Add('title',Row['title'].AsString);
        Template.Fields.Add('author',Row['author'].AsString);
        Template.Fields.Add('post',Row['post'].AsString);
        Template.Fields.Add('action','/post/?action=edit&id=' + ID.AsString);
        LoadHtml('newpost');
      end;
      'remove': begin
        Table.Get('id',ID.AsString).Delete.Apply;
        Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
      end;
      else begin
        Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
      end;
    end;
  end else begin
    Template.Fields.Add('title','');
    Template.Fields.Add('author','');
    Template.Fields.Add('post','');
    Template.Fields.Add('action','/post/?action=append');
    LoadHtml('newpost');
  end;
end;

procedure TPost.Post;
var
  Action,ID: TJSONData;
begin
  Action := Params.Find('action');
  ID := Params.Find('id');
  if Assigned(Action) then begin
    case Action.AsString of
      'append': Table.Insert(Fields).Apply;
      'edit'  : if Assigned(ID) then
        Table.Get('id',ID.AsString).Edit(Fields).Apply;
    end;
  end;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

initialization
  THome.Register('post', '*', True);
  TPost.Register('post', '/post/');

end.
