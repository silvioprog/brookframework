unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookResponseHelper, BrookHTTPConsts, HTTPDefs;

type
  TTest = class(TBrookAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

implementation

procedure TTest.Request(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Status := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
end;

initialization
  TTest.Register('*');

end.
