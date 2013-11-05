unit Actn;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookUtils, SysUtils, FPJSON, Person;

type
  TPersonAction = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

implementation

procedure TPersonAction.Get;
var
  VPerson: TPerson;
  VData: TJSONObject = nil;
begin
  VPerson := TPerson.Create;
  try
    if Table.Open.Empty then
    begin
      VPerson.Name := 'Roger Waters';
      VPerson.Age := 70;
      VPerson.Weight := 6.3;
      VPerson.LastUpdate := Now;
      VPerson.Validate;
      VData := BrookObjectToJson(VPerson);
      Table.Insert(VData).Apply;
      Write('Please use F5 to show the inserted object ...');
    end
    else
    begin
      Table.First.GetRow(VData);
      BrookJsonToObject(VData, VPerson);
      Write('Active: %s, Name: %s, Age: %d, Weight: %f, LastUpdate: %s', [
        BoolToStr(VPerson.Active, 'Y', 'N'), VPerson.Name, VPerson.Age,
        VPerson.Weight, FormatDateTime('yyyy/mm/dd hh:nn:ss',
        VPerson.LastUpdate)]);
    end;
  finally
    VData.Free;
    VPerson.Free;
  end;
end;

initialization
  TPersonAction.Register('person', '*');

end.
