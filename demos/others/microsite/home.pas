unit Home;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TDefault = class(TBrookAction)
  public
    procedure Get; override;
  end;

  THome = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TDefault }

procedure TDefault.Get;
begin
  Redirect(UrlFor(THome, []));
end;

{ THome }

procedure THome.Get;
begin
  Render('home.html');
end;

initialization
  TDefault.Register('*', True);
  THome.Register('/home/');

end.
