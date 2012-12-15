unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, RUtils, HTTPDefs;

type
  TMyAction = class(TBrookAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

implementation

procedure TMyAction.Request(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.SetCustomHeader(fieldContentEncoding, 'deflate');
  Write(ZCompressStr('Hello world! (compressed content)'));
end;

initialization
  TMyAction.Register('*');

end.
