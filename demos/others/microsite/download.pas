unit Download;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TDownload = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TDownload.Get;
begin
  Render('download.html');
end;

initialization
  TDownload.Register('/download/');

end.

