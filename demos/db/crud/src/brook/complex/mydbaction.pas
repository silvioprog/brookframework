unit MyDBAction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookDBAction, BrookDBUtils, JTemplate, RUtils, DB, FPJSON,
  HTTPDefs, SysUtils;

type

  { TMyAction }

  TMyAction = class(TBrookDBAction)
  private
    FTemplate: TJTemplate;
  protected
    function GridCallback({%H-}ADataSet: TDataSet;
      {%H-}const AWritingType: TBrookHTMLTableWritingState;
      {%H-}const APosition,{%H-}AMax: Integer;
      {%H-}var AData: string): string; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FillFields(ARequest: TRequest); override;
    procedure Load(const AFileName: TFileName);
    procedure Display;
    function Add(const AName: TJSONStringType; AValue: TJSONData): Integer; overload;
    function Add(const AName: TJSONStringType; AValue: Boolean): Integer; overload;
    function Add(const AName: TJSONStringType; AValue: TJSONFloat): Integer; overload;
    function Add(const AName, AValue: TJSONStringType): Integer; overload;
    function Add(const AName: TJSONStringType; AValue: Integer): Integer; overload;
    function Add(const AName: TJSONStringType; AValue: Int64): Integer; overload;
    function Add(const AName: TJSONStringType): Integer; overload;
    function Add(const AName: TJSONStringType; AValue: TJSONArray): Integer; overload;
    function FormTo(AFieldsDef: TFieldDefs;
      AActionClass: TBrookActionClass): string;
    function GridTo(ADataSet: TDataSet): string;
    property Template: TJTemplate read FTemplate;
  end;

implementation

constructor TMyAction.Create;
begin
  inherited Create;
  FTemplate := TJTemplate.Create;
  FTemplate.HTMLSupports := False;
end;

destructor TMyAction.Destroy;
begin
  FTemplate.Free;
  inherited Destroy;
end;

procedure TMyAction.FillFields(ARequest: TRequest);
var
  I: Integer;
begin
  for I := 0 to Pred(ARequest.ContentFields.Count) do
    Fields.Add(ARequest.ContentFields.Names[I],
      StripHTMLMarkup(ARequest.ContentFields.ValueFromIndex[I]));
end;

function TMyAction.GridCallback(ADataSet: TDataSet;
  const AWritingType: TBrookHTMLTableWritingState;
  const APosition, AMax: Integer; var AData: string): string;
begin
  Result := AData;
end;

procedure TMyAction.Load(const AFileName: TFileName);
begin
  FTemplate.LoadFromFile(AFileName + '.html');
end;

procedure TMyAction.Display;
begin
  FTemplate.Replace;
  Write(FTemplate.Content);
end;

function TMyAction.Add(const AName: TJSONStringType;
  AValue: TJSONData): Integer;
begin
  Result := FTemplate.Fields.Add(AName, AValue);
end;

function TMyAction.Add(const AName: TJSONStringType;
  AValue: Boolean): Integer;
begin
  Result := FTemplate.Fields.Add(AName, AValue);
end;

function TMyAction.Add(const AName: TJSONStringType;
  AValue: TJSONFloat): Integer;
begin
  Result := FTemplate.Fields.Add(AName, AValue);
end;

function TMyAction.Add(const AName, AValue: TJSONStringType): Integer;
begin
  Result := FTemplate.Fields.Add(AName, AValue);
end;

function TMyAction.Add(const AName: TJSONStringType;
  AValue: Integer): Integer;
begin
  Result := FTemplate.Fields.Add(AName, AValue);
end;

function TMyAction.Add(const AName: TJSONStringType;
  AValue: Int64): Integer;
begin
  Result := FTemplate.Fields.Add(AName, AValue);
end;

function TMyAction.Add(const AName: TJSONStringType): Integer;
begin
  Result := FTemplate.Fields.Add(AName);
end;

function TMyAction.Add(const AName: TJSONStringType;
  AValue: TJSONArray): Integer;
begin
  Result := FTemplate.Fields.Add(AName, AValue);
end;

function TMyAction.FormTo(AFieldsDef: TFieldDefs;
  AActionClass: TBrookActionClass): string;
begin
  Result := BrookFieldDefsToHTMLForm(AFieldsDef,
    UrlFor(AActionClass), 'post', False, 'form');
end;

function TMyAction.GridTo(ADataSet: TDataSet): string;
begin
  Result := BrookDataSetToHTML5Table(ADataSet, [], '', '',
    'table table-bordered table-hover', 1, [], @GridCallback);
end;

end.

