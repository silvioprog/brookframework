unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookHTTPUtils, BrookFCLHTTPClientBroker;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
begin
  Write('Content from: http://brookframework.org/humans.txt');
  Write('');
  Write(BrookHttpRequest('http://brookframework.org/humans.txt').Content);
end;

initialization
  TMyAction.Register('*');

end.
