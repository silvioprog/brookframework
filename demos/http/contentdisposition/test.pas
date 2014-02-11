// Please see: http://www.ietf.org/rfc/rfc2183.txt

unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookHTTPConsts, BrookResponseHelper;

type
  TMyAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TMyAction.Get;
begin
  GetResponse.AddContentDisposition(BROOK_HTTP_CONTENT_TYPE_IMAGE_PNG,
    'C:\repository\git\brookframework\demos\http\contentdisposition\img.png',
    BROOK_HTTP_CONTENT_DISPOSITION_INLINE);
end;

initialization
  TMyAction.Register('*');

end.
