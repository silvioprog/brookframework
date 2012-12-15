unit test;

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
  WriteLn('ID: %d', [Values['id'].AsInteger]);
  WriteLn('Name: %s', [Values['name'].AsString]);
end;

initialization
  // call http://localhost/cgi-bin/cgi1/login/foo/1
  TMyAction.Register('/login/:name/:id');

end.
