unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookRequestHelper, HTTPDefs, Classes;

type
  TMyAction = class(TBrookAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

implementation

procedure TMyAction.Request(ARequest: TRequest; AResponse: TResponse);
var
  S: TStrings;
begin
  ARequest.Variables(S);
  try
    Write(S.Text);
  finally
    S.Free;
  end;
end;

initialization
  TMyAction.Register('*');

end.
