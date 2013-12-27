unit Actn;

{$mode objfpc}{$H+}

interface

uses
  BrookDBAction, BrookUtils, SysUtils, FPJSON, people;

type
  TPeopleAction = class(TBrookDBAction)
  public
    procedure Get; override;
  end;

implementation

procedure TPeopleAction.Get;
var
  VPeople: TPeople;
  VData: TJSONObject = nil;
begin
  VPeople := TPeople.Create;
  try
    if Table.Open.Empty then
    begin
      VPeople.Name := 'Roger Waters';
      VPeople.Age := 70;
      VPeople.Weight := 6.3;
      VPeople.LastUpdate := Now;
      VPeople.Validate;
      VData := BrookObjectToJson(VPeople);
      Table.Insert(VData).Apply;
      Write('{ "info": "Please use F5 to show the inserted object ..." }');
    end
    else
    begin
      Table.First.GetRow(VData);
      BrookJsonToObject(VData, VPeople);
      Write('{ "active": "%s", "name": "%s", "age": %d, "weight": %e, ' +
        '"lastupdate": "%s" }', [BoolToStr(VPeople.Active, 'Y', 'N'),
        VPeople.Name, VPeople.Age, VPeople.Weight,
        FormatDateTime('yyyy/mm/dd hh:nn:ss', VPeople.LastUpdate)]);
    end;
  finally
    VData.Free;
    VPeople.Free;
  end;
end;

initialization
  DefaultFormatSettings.DecimalSeparator := '.';
  TPeopleAction.Register('people', '*');

end.
