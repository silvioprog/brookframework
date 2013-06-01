unit JSONRTTIUtils;

{$mode objfpc}{$H+}

interface

uses
  fpjsonrtti, fpjson, sysutils;

function ObjectToJSON(AObject: TObject): TJSONObject;
procedure JSONToObject(AJSON: TJSONObject; AObject: TObject);

implementation

function ObjectToJSON(AObject: TObject): TJSONObject;
var
  VStreamer: TJSONStreamer;
begin
  VStreamer := TJSONStreamer.Create(nil);
  try
    try
      Result := VStreamer.ObjectToJSON(AObject);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    VStreamer.Free;
  end;
end;

procedure JSONToObject(AJSON: TJSONObject; AObject: TObject);
var
  VDeStreamer: TJSONDeStreamer;
begin
  VDeStreamer := TJSONDeStreamer.Create(nil);
  try
    VDeStreamer.CaseInsensitive := True;
    VDeStreamer.JSONToObject(AJSON, AObject);
  finally
    VDeStreamer.Free;
  end;
end;

end.

