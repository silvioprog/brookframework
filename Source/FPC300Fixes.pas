unit FPC300Fixes;

{$MODE DELPHI}

interface

uses
  SysUtils;

type

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
    property Length: SizeInt read GetLength;
  end;

implementation

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

end.
