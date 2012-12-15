unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookActionHelper;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
begin
 if Confirm('Click in "OK" or "Cancel" ...') then
    Write('The result is: OK')
  else
    Write('The result is: Cancel');

  //Alert('Hello world!');

  //Write(Prompt('What''s your name?', 'put your name here'));
end;

initialization
  TMyAction.Register('*');

end.
