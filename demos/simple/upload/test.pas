unit Test;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookConsts, BrookHTTPConsts, BrookUtils, HTTPDefs, SysUtils;

type
  TTest = class(TBrookAction)
  public
    procedure Request({%H-}ARequest: TRequest;
      {%H-}AResponse: TResponse); override;
  end;

const
  RESULT =
    'All files:' + BR + LF + '%s' + BR + BR + LF +
    'All files saved in:' + BR + LF + '%s';

implementation

procedure TTest.Request(ARequest: TRequest; AResponse: TResponse);
var
  I: Integer;
  VFiles, VSep: string;
  VFormItem: TUploadedFile;
begin
  case ARequest.Method of
    BROOK_HTTP_REQUEST_METHOD_GET: Write({$i head.inc}, [{$i form.inc}]);
    BROOK_HTTP_REQUEST_METHOD_POST:
      begin
        VFiles := '';
        VSep := '||';
        for I := 0 to Pred(ARequest.Files.Count) do
        begin
          VFormItem := ARequest.Files[I];
          if VFormItem.FileName <> '' then
            VFiles += VFormItem.FileName + VSep;
        end;
        Write({$i head.inc},
          [Format(RESULT, [VFiles, BrookSettings.DirectoryForUploads])]);
      end;
  end;
end;

initialization
  TTest.Register('*');

end.
