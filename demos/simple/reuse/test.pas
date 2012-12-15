unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type
  TAction1 = class(TBrookAction)
  public
    procedure Get; override;
    function Text: string;
  end;

  TAction2 = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TAction1.Get;
begin
end;

function TAction1.Text: string;
begin
  Result := 'Hello world!';
end;

procedure TAction2.Get;
var
  VAct: TAction1;
begin
  VAct := TAction1.Create;
  try
    Write(VAct.Text);
  finally
    VAct.Free;
  end;
end;

initialization
  TAction2.Register('*');

end.
