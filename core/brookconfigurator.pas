(*
  Brook framework, Configurator Class

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookConfigurator;

{$i brook.inc}

interface

uses
  BrookClasses, BrookUtils, BrookException, BrookMessages, BrookConsts, Classes,
  SysUtils, TypInfo;

type
  { Handles exceptions for @link(TBrookConfigurator). }
  EBrookConfigurator = class(EBrook);

  { Is a metaclass for @link(TBrookConfigurator) class. }
  TBrookConfiguratorClass = class of TBrookConfigurator;

  { Configures objects by means of string or file. }
  TBrookConfigurator = class(TBrookObject)
  private
    FAutoLoaded: Boolean;
    FCfg: TStrings;
    FConfiguration: string;
    FTarget: TObject;
    FClassChecking: Boolean;
    function GetValues(AName: string): string;
  public
    { Creates an instance of a @link(TBrookConfigurator) class. }
    constructor Create;
    { Creates an instance of a @link(TBrookConfigurator) class passing the
      configuration in its parameter. }
    constructor Create(const AConfiguration: string); reintroduce;
    { Frees an instance of @link(TBrookConfigurator) class. }
    destructor Destroy; override;
    { Loads the configuration file to the memory. }
    function Load: TBrookConfigurator; virtual;
    { Clears the internal configuration. }
    procedure Clear;
    { Configures the object informed in the Target property. }
    procedure Configure;
    { Defines if the configuration is read automatically. }
    property AutoLoaded: Boolean read FAutoLoaded write FAutoLoaded;
    { Defines the configuration using one of the following formats. Exemple:

        @code(cfg.Configuration := 'param1=value1;param2=value2;param3=value2';)

        or:

        @code(cfg.Configuration := 'cgi1.cfg';)

        This option requires the following file format:

@longCode(
  param1=value1
  param2=value2
  param3=value2
)

      NOTE: If a configuration is passed to @code(BrookSettings.Configuration),
      the data informed in this property will be disregarded.
    }
    property Configuration: string read FConfiguration write FConfiguration;
    { Defines the object to be configured. }
    property Target: TObject read FTarget write FTarget;
    { Devolve o valor de um parâmetro. }
    property Values[AName: string]: string read GetValues; default;
    { Enables the class name checking, i.e., if the class of the configuring
      object is a 'TObject1', the following string will be informed as
      configuration:

      @code('tobject1.param1=value1;tobject1.param2=value2;tobject1.param3=value3')

      This feature is useful when more then one object will be configured in
      one or more configurator objects. }
    property ClassChecking: Boolean read FClassChecking write FClassChecking;
  end;

implementation

constructor TBrookConfigurator.Create;
begin
  inherited Create;
  FCfg := TStringList.Create;
  FCfg.Delimiter := SC;
  FCfg.StrictDelimiter := True;
  FAutoLoaded := True;
end;

constructor TBrookConfigurator.Create(const AConfiguration: string);
begin
  Create;
  FConfiguration := AConfiguration;
  Load;
end;

destructor TBrookConfigurator.Destroy;
begin
  Target := nil;
  FCfg.Free;
  inherited Destroy;
end;

function TBrookConfigurator.Load: TBrookConfigurator;
begin
  Result := Self;
  if (FConfiguration = ES) and (BrookSettings.Configuration <> ES) then
    FConfiguration := BrookSettings.Configuration;
  if Trim(FConfiguration) = ES then
  begin
    FCfg.Clear;
    Exit;
  end;
  if (Pos(SC, FConfiguration) <> 0) or (Pos(EQ, FConfiguration) <> 0) then
    FCfg.DelimitedText := FConfiguration
  else
  begin
    if not FileExists(FConfiguration) then
      raise EBrookConfigurator.CreateFmt(Self,
        SBrookCfgFileNotFoundError, [FConfiguration]);
    FCfg.LoadFromFile(FConfiguration);
  end;
end;

procedure TBrookConfigurator.Clear;
begin
  FCfg.Clear;
end;

function TBrookConfigurator.GetValues(AName: string): string;
begin
  Result := FCfg.Values[AName];
end;

procedure TBrookConfigurator.Configure;
var
  I: Integer;
  VValue, VPropName, VToken, VClassName: string;
begin
  if not Assigned(FTarget) then
    Exit;
  if FAutoLoaded then
    Load;
  if FClassChecking then
    for I := 0 to Pred(FCfg.Count) do
    begin
      VClassName := FCfg.Names[I];
      Delete(VClassName, Pos(DT, VClassName), MaxInt);
      VPropName := FCfg.Names[I];
      Delete(VPropName, 1, Pos(DT, VPropName));
      VToken := Copy(VPropName, 1, 1);
      if (VToken = PO) or (VToken = ES) then
        Continue;
      VValue := FCfg.Values[VClassName + DT + VPropName];
      if SameText(VClassName, FTarget.ClassName) and
        IsPublishedProp(FTarget, VPropName) then
          SetPropValue(FTarget, VPropName, VValue);
    end
  else
    for I := 0 to Pred(FCfg.Count) do
    begin
      VPropName := FCfg.Names[I];
      VToken := Copy(VPropName, 1, 1);
      if (VToken = PO) or (VToken = ES) then
        Continue;
      VValue := FCfg.Values[VPropName];
      if IsPublishedProp(FTarget, VPropName) then
        SetPropValue(FTarget, VPropName, VValue);
    end;
end;

end.

