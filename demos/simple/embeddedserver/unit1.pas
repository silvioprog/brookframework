unit Unit1;

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
  Write('Your content here ...');
end;

initialization
  TMyAction.Register('*');

end.