(*
  Brook JSON Helper unit.

  Copyright (C) 2013 Silvio Clecio.

  http://brookframework.org

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookJSONHelper;

{$i brook.inc}

interface

uses
  BrookConsts, FPJSON, SysUtils, MD5, SHA1;

type
  { Adds features to the @code(TJSONData) class. }
  TBrookJSONDataHelper = class helper for TJSONData
  private
    function GetAsChar: Char;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsLowerJS: string;
    function GetAsLowerStr: string;
    function GetAsMD5: string;
    function GetAsQuotedStr: string;
    function GetAsSHA1: string;
    function GetAsSmallInt: SmallInt;
    function GetAsTime: TTime;
    function GetAsTrimJS: string;
    function GetAsTrimStr: string;
    function GetAsUpperJS: string;
    function GetAsUpperStr: string;
    function GetIsBlank: Boolean;
    function GetIsEmpty: Boolean;
    procedure SetAsChar(AValue: Char);
    procedure SetAsDate(AValue: TDate);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsLowerJS(AValue: string);
    procedure SetAsLowerStr(AValue: string);
    procedure SetAsMD5(AValue: string);
    procedure SetAsQuotedStr(AValue: string);
    procedure SetAsSHA1(AValue: string);
    procedure SetAsSmallInt(AValue: SmallInt);
    procedure SetAsTime(AValue: TTime);
    procedure SetAsTrimJS(AValue: string);
    procedure SetAsTrimStr(AValue: string);
    procedure SetAsUpperJS(AValue: string);
    procedure SetAsUpperStr(AValue: string);
  public
    { Get or set the JSONData as a Char. }
    property AsChar: Char read GetAsChar write SetAsChar;
    { Get or set the JSONData as a SmallInt. }
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    { Get or set the JSONData as a TTime. }
    property AsTime: TTime read GetAsTime write SetAsTime;
    { Get or set the JSONData as a TDate. }
    property AsDate: TDate read GetAsDate write SetAsDate;
    { Get or set the JSONData as a TDateTime. }
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    { Get or set the JSONData as a trimmed string. }
    property AsTrimStr: string read GetAsTrimStr write SetAsTrimStr;
    { Get or set the JSONData as a trimmed JSON string. }
    property AsTrimJS: string read GetAsTrimJS write SetAsTrimJS;
    { Get or set the JSONData as a lower string. }
    property AsLowerStr: string read GetAsLowerStr write SetAsLowerStr;
    { Get or set the JSONData as a lower JSON string. }
    property AsLowerJS: string read GetAsLowerJS write SetAsLowerJS;
    { Get or set the JSONData as a upper string. }
    property AsUpperStr: string read GetAsUpperStr write SetAsUpperStr;
    { Get or set the JSONData as a upper JSON string. }
    property AsUpperJS: string read GetAsUpperJS write SetAsUpperJS;
    { Get or set the JSONData as a quoted string. }
    property AsQuotedStr: string read GetAsQuotedStr write SetAsQuotedStr;
    { Get or set the JSONData as MD5 string. }
    property AsMD5: string read GetAsMD5 write SetAsMD5;
    { Get or set the JSONData as SHA1 string. }
    property AsSHA1: string read GetAsSHA1 write SetAsSHA1;
    { Get if the JSONData is an empty string. }
    property IsEmpty: Boolean read GetIsEmpty;
    { Get if the JSONData is a blank string. }
    property IsBlank: Boolean read GetIsBlank;
  end;

  { Adds features to the @code(TJSONObject) class. }
  TBrookJSONObjectHelper = class helper for TJSONObject
  private
    function GetDates(AName: string): TTime;
    function GetDateTimes(AName: string): TTime;
    function GetTimes(AName: string): TTime;
    procedure SetDates(AName: string; AValue: TTime);
    procedure SetDateTimes(AName: string; AValue: TTime);
    procedure SetTimes(AName: string; AValue: TTime);
  public
    { Checks if a name exists. }
    function Exists(const AName: string): Boolean;
    { Get or set a value as TTime. }
    property Times[AName: string]: TTime read GetTimes write SetTimes;
    { Get or set a value as TDate. }
    property Dates[AName: string]: TTime read GetDates write SetDates;
    { Get or set a value as TDateTime. }
    property DateTimes[AName: string]: TTime read GetDateTimes write SetDateTimes;
  end;

implementation

{ TBrookJSONDataHelper }

function TBrookJSONDataHelper.GetAsChar: Char;
begin
  Result := PChar(AsString)^;
end;

function TBrookJSONDataHelper.GetAsDate: TDate;
begin
  Result := Trunc(AsFloat);
end;

function TBrookJSONDataHelper.GetAsDateTime: TDateTime;
begin
  Result := AsFloat;
end;

function TBrookJSONDataHelper.GetAsLowerJS: string;
begin
  Result := LowerCase(AsJSON);
end;

function TBrookJSONDataHelper.GetAsLowerStr: string;
begin
  Result := LowerCase(AsString);
end;

function TBrookJSONDataHelper.GetAsQuotedStr: string;
begin
  Result := AnsiQuotedStr(AsString, AP);
end;

function TBrookJSONDataHelper.GetAsSmallInt: SmallInt;
begin
  Result := AsInteger;
end;

function TBrookJSONDataHelper.GetAsTime: TTime;
begin
  Result := Frac(AsFloat);
end;

function TBrookJSONDataHelper.GetAsTrimJS: string;
begin
  Result := Trim(AsJSON);
end;

function TBrookJSONDataHelper.GetAsTrimStr: string;
begin
  Result := Trim(AsString);
end;

function TBrookJSONDataHelper.GetAsUpperJS: string;
begin
  Result := UpperCase(AsJSON);
end;

function TBrookJSONDataHelper.GetAsUpperStr: string;
begin
  Result := UpperCase(AsString);
end;

function TBrookJSONDataHelper.GetIsBlank: Boolean;
begin
  Result := Trim(AsString) = '';
end;

function TBrookJSONDataHelper.GetIsEmpty: Boolean;
begin
  Result := AsString = '';
end;

function TBrookJSONDataHelper.GetAsMD5: string;
begin
  Result := MD5Print(MD5String(AsString));
end;

function TBrookJSONDataHelper.GetAsSHA1: string;
begin
  Result := SHA1Print(SHA1String(AsString));
end;

procedure TBrookJSONDataHelper.SetAsChar(AValue: Char);
begin
  AsString := AValue;
end;

procedure TBrookJSONDataHelper.SetAsDate(AValue: TDate);
begin
  AsFloat := Trunc(AValue);
end;

procedure TBrookJSONDataHelper.SetAsDateTime(AValue: TDateTime);
begin
  AsFloat := AValue;
end;

procedure TBrookJSONDataHelper.SetAsLowerJS(AValue: string);
begin
  AsString := StringToJSONString(LowerCase(AValue));
end;

procedure TBrookJSONDataHelper.SetAsLowerStr(AValue: string);
begin
  AsString := LowerCase(AValue);
end;

procedure TBrookJSONDataHelper.SetAsQuotedStr(AValue: string);
begin
  AsString := AnsiQuotedStr(AValue, AP);
end;

procedure TBrookJSONDataHelper.SetAsSmallInt(AValue: SmallInt);
begin
  AsInteger := AValue;
end;

procedure TBrookJSONDataHelper.SetAsTime(AValue: TTime);
begin
  AsFloat := Frac(AValue);
end;

procedure TBrookJSONDataHelper.SetAsTrimJS(AValue: string);
begin
  AsString := StringToJSONString(Trim(AValue));
end;

procedure TBrookJSONDataHelper.SetAsTrimStr(AValue: string);
begin
  AsString := Trim(AValue);
end;

procedure TBrookJSONDataHelper.SetAsUpperJS(AValue: string);
begin
  AsString := StringToJSONString(UpperCase(AValue));
end;

procedure TBrookJSONDataHelper.SetAsUpperStr(AValue: string);
begin
  AsString := UpperCase(AValue);
end;

procedure TBrookJSONDataHelper.SetAsMD5(AValue: string);
begin
  AsString := MD5Print(MD5String(AValue));
end;

procedure TBrookJSONDataHelper.SetAsSHA1(AValue: string);
begin
  AsString := SHA1Print(SHA1String(AValue));
end;

{ TBrookJSONObjectHelper }

function TBrookJSONObjectHelper.GetDates(AName: string): TTime;
begin
  Result := Trunc(Floats[AName]);
end;

function TBrookJSONObjectHelper.GetDateTimes(AName: string): TTime;
begin
  Result := Floats[AName];
end;

function TBrookJSONObjectHelper.GetTimes(AName: string): TTime;
begin
  Result := Frac(Floats[AName]);
end;

procedure TBrookJSONObjectHelper.SetDates(AName: string; AValue: TTime);
begin
  Floats[AName] := Trunc(AValue);
end;

procedure TBrookJSONObjectHelper.SetDateTimes(AName: string; AValue: TTime);
begin
  Floats[AName] := AValue;
end;

procedure TBrookJSONObjectHelper.SetTimes(AName: string; AValue: TTime);
begin
  Floats[AName] := Frac(AValue);
end;

function TBrookJSONObjectHelper.Exists(const AName: string): Boolean;
begin
  Result := IndexOfName(AName, True) <> -1;
end;

end.

