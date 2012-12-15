unit Login;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookConsts;

type
  TLogin = class(TBrookAction)
  public
    procedure Get; override;
  end;

  TWelcome = class(TBrookAction)
  public
    procedure Post; override;
  end;

implementation

{ TLogin }

procedure TLogin.Get;
begin
  Render('login.html', ['']);
end;

{ TWelcome }

procedure TWelcome.Post;
var
  VUser, VPass: string;
begin
  VUser := Fields['usertxt'].AsString;
  VPass := Fields['passtxt'].AsString;
  if (VUser <> ES) and (VPass <> ES) then
  begin
    Render('welcome.html', [VUser, VPass]);
    Exit;
  end;
  Render('login.html', ['<hr />Login fail!']);
end;

initialization
  TLogin.Register('/login');
  TWelcome.Register('/welcome');

end.
