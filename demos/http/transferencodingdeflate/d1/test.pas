// Please see: http://en.wikipedia.org/wiki/HTTP_compression

unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookConsts, BrookHTTPUtils, HTTPDefs, ZStream, Classes, SysUtils;

const
  CONTENT =
    '<!DOCTYPE HTML>' + LF +
    '<html lang="en-US">' + LF +
    '<head>' + LF +
    '	<meta charset="UTF-8">' + LF +
    '	<title>Deflate</title>' + LF +
    '</head>' + LF +
    '<body>' + LF +
    '	Hello world! (%s)' + LF +
    '</body>' + LF +
    '</html>';

type
  TMyAction = class(TBrookAction)
  public
    procedure Request(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

procedure TMyAction.Request(ARequest: TRequest; AResponse: TResponse);
var
  S: string;
  VOutput: TMemoryStream;
begin
  if aeDeflate in BrookGetAcceptEncodingSet(ARequest.AcceptEncoding) then
  begin
    VOutput := TMemoryStream.Create;
    try
      with TCompressionStream.Create(clMax, VOutput, True) do
      try
        S := Format(CONTENT, ['Compressed']);
        Write(Pointer(S)^, Length(S));
      finally
        Free;
      end;
      VOutput.Seek(0, 0);
      AResponse.SetCustomHeader(fieldContentEncoding, 'deflate');
      AResponse.ContentStream := VOutput;
      AResponse.SendContent;
    finally
      VOutput.Free;
    end;
  end
  else
    Write(CONTENT, ['Uncompressed']);
end;

initialization
  TMyAction.Register('*');

end.
