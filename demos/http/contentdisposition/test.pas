// Please see: http://www.ietf.org/rfc/rfc2183.txt

unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookHTTPConsts, BrookResponseHelper, HTTPDefs;

type
  TMyAction = class(TBrookAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

implementation

procedure TMyAction.Request(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.AddContentDisposition(BROOK_HTTP_CONTENT_TYPE_IMAGE_PNG, 'img.png',
    BROOK_HTTP_CONTENT_DISPOSITION_INLINE);
end;

initialization
  TMyAction.Register('*');

end.
