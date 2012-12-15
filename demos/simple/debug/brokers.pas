unit Brokers;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF BROOK_DEBUG}
  HeapTrc, SysUtils, BrookHTTPConsts, BrookConsts, BrookUtils,
{$ENDIF}
  BrookFCLCGIBroker;

implementation

{$IFDEF BROOK_DEBUG}
const
  HEAP_FN = 'HEAP.TXT';
  HTML_TPL = '<html><head><title>%s</title><style>body{margin:0;padding:30px;font:12px/1.5 Helvetica,Arial,Verdana,sans-serif;}h1{margin:0;font-size:48px;font-weight:normal;line-height:48px;}strong{display:inline-block;width:65px;}</style></head><body><h1>%s</h1><br />%s</body></html>';

var
  TmpDir: string;

function HTML(const ATitle, AError, ABody: string): string;
begin
  Result := Format(HTML_TPL, [ATitle, AError, ABody]);
end;

initialization
  TmpDir := GetTempDir(True);
  if TmpDir = ES then
    TmpDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  DeleteFile(TmpDir + HEAP_FN);
  SetHeapTraceOutput(TmpDir + HEAP_FN);
  BrookSettings.Charset := BROOK_HTTP_CHARSET_UTF_8;
  BrookSettings.Page404 := HTML('Page not found', '404 - Page not found',
    'Click <a href="%s">here</a> to go to home page ...');
  BrookSettings.Page500 := HTML('Internal server error',
    '500 - Internal server error', '%s');
{$ENDIF}

end.
