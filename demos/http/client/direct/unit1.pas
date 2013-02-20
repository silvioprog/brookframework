unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookHTTPClient, BrookFCLHTTPClientBroker;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
var
  VClient: TBrookHTTPClient;
begin
  VClient := TBrookHTTPClient.Create('fclweb');
  try
    Write('Content from: http://brookframework.org/humans.txt');
    Write('');
    Write(VClient.Request('http://brookframework.org/humans.txt').Content);
  finally
    VClient.Free;
  end;
end;

initialization
  TMyAction.Register('*');

end.
