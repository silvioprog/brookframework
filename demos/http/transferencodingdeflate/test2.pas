unit test2;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, RUtils, HTTPDefs;

type
  TTest2Action = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TTest2Action.Get;
begin
  GetResponse.SetCustomHeader(fieldContentEncoding, 'deflate');
  Write(ZCompressStr('Hello world! (compressed content)'));
end;

initialization
  TTest2Action.Register('/test2');

end.
