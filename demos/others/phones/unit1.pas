unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, FPJSON, RUtils;

type
  TPerson = class(TBrookAction)
  public
    procedure Get; override;
    procedure Post; override;
  end;

implementation

procedure TPerson.Get;
begin
  Render('form.html');
end;

procedure TPerson.Post;
var
  i: Integer;
  phones: TJSONArray;
begin
  GetJSONArray(Fields['phones'].AsString, phones);
  try
    WriteLn('Name: %s', [Fields['name'].AsString]);
    for i := 0 to Pred(phones.Count) do
      WriteLn('Phone %d: %s', [i + 1, phones[i].AsString]);
  finally
    phones.Free;
  end;
end;

initialization
  TPerson.Register('*');

end.
