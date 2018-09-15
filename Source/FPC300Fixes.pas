unit FPC300Fixes;

{$MODE DELPHI}

interface

uses
  SysUtils;

type
  TArray<T> = array of T;

  { TIntegerHelper }

  TIntegerHelper = record helper for Integer
  public
    function ToString: string; inline;
  end;

  { TStringHelper }

  TStringHelper = record helper for string
  private
    function GetLength: SizeInt;
  public
    function IsEmpty: Boolean; inline;
    function StartsWith(const AValue: string): Boolean; overload; inline;
    function StartsWith(const AValue: string;
      AIgnoreCase: Boolean): Boolean; overload;
    function EndsWith(const AValue: string): Boolean; overload; inline;
    function EndsWith(const AValue: string;
      AIgnoreCase: Boolean): Boolean; overload;
    property Length: SizeInt read GetLength;
  end;

function SameStr(const S1, S2: string): Boolean; inline;

implementation

function SameStr(const S1, S2: string): Boolean;
begin
  Result := CompareStr(S1, S2) = 0;
end;

{ TIntegerHelper }

function TIntegerHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ TStringHelper }

function TStringHelper.GetLength: SizeInt;
begin
  Result := System.Length(Self);
end;

function TStringHelper.IsEmpty: Boolean;
begin
  Result := Length = 0;
end;

function TStringHelper.StartsWith(const AValue: string): Boolean;
begin
  Result := StartsWith(AValue, False);
end;

function TStringHelper.StartsWith(const AValue: string;
  AIgnoreCase: Boolean): Boolean;
var
  L: SizeInt;
  S: string;
begin
  L := System.Length(AValue);
  Result := L <= 0;
  if not Result then
  begin
    S := System.Copy(Self, 1, L);
    Result := System.Length(S) = L;
    if Result then
      if AIgnoreCase then
        Result := SameText(S, aValue)
      else
        Result := SameStr(S, AValue);
  end;
end;

function TStringHelper.EndsWith(const AValue: string): Boolean;
begin
  Result := EndsWith(AValue, False);
end;

function TStringHelper.EndsWith(const AValue: string;
  AIgnoreCase: Boolean): Boolean;
var
  L: SizeInt;
  S: string;
begin
  L := System.Length(AVAlue);
  Result := L = 0;
  if not Result then
  begin
    S := System.Copy(Self, Length - L + 1, L);
    Result := System.Length(S) = L;
    if Result then
      if AIgnoreCase then
        Result := CompareText(S, AValue) = 0
      else
        Result := S = AValue;
  end;
end;

end.
