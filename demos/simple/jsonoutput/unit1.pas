unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction;

type

  { TMyAction }

  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

{ TMyAction }

procedure TMyAction.Get;
begin
  HttpResponse.ContentType := 'application/json';
  Write('{"BrookIsFun":true}');
end;

initialization
  TMyAction.Register('*');

end.
