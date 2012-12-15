unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
begin
  Write('ID: %d, Name: %s', [Values['id'].AsInteger, Values['name'].AsString]);
end;

initialization
  // call http://localhost/cgi-bin/cgi1/test/foo/1
  TMyAction.Register('/test/:name/:id/');

end.
