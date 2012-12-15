unit About;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TAbout = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TAbout.Get;
begin
  Render('about.html');
end;

initialization
  TAbout.Register('/about/');

end.
