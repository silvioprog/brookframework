// Please see: http://en.wikipedia.org/wiki/HTTP_compression

unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookConsts, BrookHTTPUtils, HTTPDefs, ZStream, Classes;

const
  CONTENT =
    '<!DOCTYPE HTML>' + LF +
    '<html lang="en-US">' + LF +
    '<head>' + LF +
    '	<meta charset="UTF-8">' + LF +
    '	<title>Deflate</title>' + LF +
    '</head>' + LF +
    '<body>' + LF +
    '	Hello world!' + LF +
    '</body>' + LF +
    '</html>';

type
  TMyAction = class(TBrookAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

implementation

procedure TMyAction.Request(ARequest: TRequest; AResponse: TResponse);
var
  S: string;
  VDeflateStream: TCompressionStream;
begin
  if aeDeflate in BrookGetAcceptEncodingSet(AResponse.HTTPAcceptEncoding) then
  begin
    AResponse.ContentStream := TMemoryStream.Create;
    VDeflateStream := TCompressionStream.Create(
      clMax, AResponse.ContentStream, True);
    try
      S := CONTENT;
      VDeflateStream.Write(Pointer(S)^, Length(S));
    finally
      AResponse.ContentStream.Free;
      VDeflateStream.Free;
    end;
  end
  else
    Write(CONTENT);
end;

initialization
  TMyAction.Register('*');

end.
