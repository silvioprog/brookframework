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
  VHttp: TBrookHTTPDef = nil;
begin
  VClient := TBrookHTTPClient.Create('fclweb');
  try
    VClient.Prepare(VHttp);
    VHttp.Method := 'GET';
    VHttp.Url := 'http://brookframework.org/humans.txt';
    VClient.Request(VHttp);
    VHttp.Document.Position := 0;
    Write(VHttp.Document);
  finally
    VHttp.Free;
    VClient.Free;
  end;
end;

initialization
  TMyAction.Register('*');

end.
