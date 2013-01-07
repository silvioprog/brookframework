unit Rss;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, EasyRSS;

type
  TRSSAction = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

implementation

procedure TRSSAction.Get;
var
  i: Integer;
  rss: TRSS;
  item: TRSSItem;
begin
  rss := TRSS.Create;
  try
    rss.Title := 'News';
    rss.Copyright := 'EasyRSS';
    Table.Open.First;
    for i := 0 to Pred(Table.Count) do
    begin
      item := rss.Add;
      item.Title := Table['title'].AsString;
      item.Description := Table['description'].AsString;
      item.PubDate := Table['pubdate'].AsString;
      item.Link := Table['link'].AsString;
      Table.Next;
    end;
    Write(rss.Content);
  finally
    rss.Free;
  end;
end;

initialization
  TRSSAction.Register('news', '*');

end.
