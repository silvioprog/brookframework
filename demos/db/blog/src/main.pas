unit main;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookDBUtils, BrookHTTPConsts, JTemplate, HTTPDefs;

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
  FTemplate.LoadFromFile(AHtml + '.html');
end;

{ THome }

procedure THome.Get;
begin
  LoadHtml('home');
  Template.Fields.Add('menu', UrlFor(TPost, ['new']));
  Template.Fields.Add('post', BrookDataSetToHTMLTable(Table.Open.DataSet, [],
    'table table-bordered table-hover'));
end;

{ TPost }

procedure TPost.Get;
begin
  LoadHtml('newpost');
end;

procedure TPost.Post;
begin
  Table.Insert(Fields).Apply;
  Redirect(UrlFor(THome), BROOK_HTTP_STATUS_CODE_FOUND);
end;

initialization
  THome.Register('post', '*', True);
  TPost.Register('post', '/post/');

end.
