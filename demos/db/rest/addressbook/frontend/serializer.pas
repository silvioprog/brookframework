unit Serializer;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, FPJSON;

procedure Serialize(AComponent: TComponent; AJSON: TJSONObject;
  const AInputPrefix: ShortString = 'input_');
procedure Unserialize(AJSON: TJSONObject; AComponent: TComponent;
  const AInputPrefix: ShortString = 'input_');

implementation

procedure Serialize(AComponent: TComponent; AJSON: TJSONObject;
  const AInputPrefix: ShortString);
var
  N: string;
  L: Integer;
  I: TComponent;
begin
  L := Length(AInputPrefix);
  AJSON.Clear;
  for I in AComponent do
    if I is TCustomEdit then
    begin
      N := I.Name;
      if SameText(Copy(N, 1, L), AInputPrefix) then
      begin
        Delete(N, 1, L);
        AJSON.Add(N, TCustomEdit(I).Text);
      end;
    end;
end;

procedure Unserialize(AJSON: TJSONObject; AComponent: TComponent;
  const AInputPrefix: ShortString);
var
  I, L: Integer;
  C: TComponent;
begin
  L := Length(AInputPrefix);
  for I := 0 to Pred(AJSON.Count) do
    for C in AComponent do
      if (C is TCustomEdit) and SameText(Copy(C.Name, 1, L), AInputPrefix) and
        SameText(AJSON.Names[I], Copy(C.Name, L + 1, MaxInt)) then
        TCustomEdit(C).Text := AJSON.Items[I].AsString;
end;

end.

