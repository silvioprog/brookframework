unit testbrookconfigurator;

{$mode objfpc}{$H+}

interface

uses
  BrookConfigurator, BrookUtils, BrookConsts, fpcunit, testregistry, sysutils,
  typinfo;

type
  TMyEnum = (enum1, enum2, enum3);

  TMySet = set of TMyEnum;

  { TTarget1 }

  TTarget1 = class
  private
    FMyBoolean: Boolean;
    FMyChar: Char;
    FMyCurrency: Currency;
    FMyDateTime: TDateTime;
    FMyEnum: TMyEnum;
    FMyFloat: Double;
    FMyInt64: Int64;
    FMyInteger: Integer;
    FMySet: TMySet;
    FMyString: string;
  published
    property MyChar: Char read FMyChar write FMyChar;
    property MyString: string read FMyString write FMyString;
    property MyInteger: Integer read FMyInteger write FMyInteger;
    property MyInt64: Int64 read FMyInt64 write FMyInt64;
    property MyFloat: Double read FMyFloat write FMyFloat;
    property MyCurrency: Currency read FMyCurrency write FMyCurrency;
    property MyBoolean: Boolean read FMyBoolean write FMyBoolean;
    property MyDateTime: TDateTime read FMyDateTime write FMyDateTime;
    property MyEnum: TMyEnum read FMyEnum write FMyEnum;
    property MySet: TMySet read FMySet write FMySet;
  end;

  { TTarget2 }

  TTarget2 = class(TTarget1)
  end;

  { TTestBrookConfigurator }

  TTestBrookConfigurator = class(TTestCase)
  public
    constructor Create; override;
  published
    procedure TestConfigureFromString;
    procedure TestConfigureFromFile;
    procedure TestClassChecking;
  end;

implementation

{ TTestBrookConfigurator }

constructor TTestBrookConfigurator.Create;
begin
  inherited Create;
  BrookSettings.Configuration := '';
  with DefaultFormatSettings do
  begin
    DecimalSeparator := ',';
    ThousandSeparator := '.';
    ShortDateFormat := 'yyyy/mm/dd';
    ShortTimeFormat := 'hh:nn:ss';
  end;
end;

procedure TTestBrookConfigurator.TestConfigureFromString;
var
  VTarget1: TTarget1;
  VCfg: TBrookConfigurator;
begin
  VCfg := TBrookConfigurator.Create(
    'mychar=A;mystring=ABC;myinteger=123;myint64=456;myfloat=12,3;' +
    'mycurrency=45,6;myboolean=true;mydatetime=2014/02/01 11:59:01;' +
    'myenum=enum2;myset=[enum1, enum3]');
  VTarget1 := TTarget1.Create;
  try
    VCfg.Target := VTarget1;
    VCfg.Configure;
    AssertEquals('A', VTarget1.MyChar);
    AssertEquals('ABC', VTarget1.MyString);
    AssertEquals(123, VTarget1.MyInteger);
    AssertEquals(456, VTarget1.MyInt64);
    AssertEquals(12.3, VTarget1.MyFloat);
    AssertEquals(45.6, VTarget1.MyCurrency);
    AssertEquals(True, VTarget1.MyBoolean);
    AssertEquals(StrToDateTime('2014/02/01 11:59:01'), VTarget1.MyDateTime);
    AssertEquals('enum2', GetEnumProp(VTarget1, 'MyEnum'));
    AssertEquals('enum1,enum3', GetSetProp(VTarget1, 'MySet'));
  finally
    VTarget1.Free;
    VCfg.Free;
  end;
end;

procedure TTestBrookConfigurator.TestConfigureFromFile;
var
  VTarget1: TTarget1;
  VCfg: TBrookConfigurator;
begin
  BrookSettings.Configuration := '';
  VCfg := TBrookConfigurator.Create(ExtractFilePath(ParamStr(0)) + 'test.cfg');
  VTarget1 := TTarget1.Create;
  try
    VCfg.Target := VTarget1;
    VCfg.Configure;
    AssertEquals('A', VTarget1.MyChar);
    AssertEquals('ABC', VTarget1.MyString);
    AssertEquals(123, VTarget1.MyInteger);
    AssertEquals(456, VTarget1.MyInt64);
    AssertEquals(12.3, VTarget1.MyFloat);
    AssertEquals(45.6, VTarget1.MyCurrency);
    AssertEquals(True, VTarget1.MyBoolean);
    AssertEquals(StrToDateTime('2014/02/01 11:59:01'), VTarget1.MyDateTime);
    AssertEquals('enum2', GetEnumProp(VTarget1, 'MyEnum'));
    AssertEquals('enum1,enum3', GetSetProp(VTarget1, 'MySet'));
  finally
    VTarget1.Free;
    VCfg.Free;
  end;
end;

procedure TTestBrookConfigurator.TestClassChecking;
var
  VTarget1: TTarget1;
  VTarget2: TTarget2;
  VCfg: TBrookConfigurator;
begin
  VCfg := TBrookConfigurator.Create(
    'ttarget2.mychar=A;ttarget2.mystring=ABC;ttarget2.myinteger=123;' +
    'ttarget2.myint64=456;ttarget2.myfloat=12,3;ttarget2.mycurrency=45,6;' +
    'ttarget2.myboolean=true;ttarget2.mydatetime=2014/02/01 11:59:01;' +
    'ttarget2.myenum=enum2;ttarget2.myset=[enum1, enum3]');
  VTarget1 := TTarget1.Create;
  VTarget2 := TTarget2.Create;
  try
    VCfg.ClassChecking := True;
    VCfg.Target := VTarget1;
    VCfg.Configure;
    AssertEquals(#0, VTarget1.MyChar);
    AssertEquals('', VTarget1.MyString);
    AssertEquals(0, VTarget1.MyInteger);
    AssertEquals(0, VTarget1.MyInt64);
    AssertEquals(0, VTarget1.MyFloat);
    AssertEquals(0, VTarget1.MyCurrency);
    AssertEquals(False, VTarget1.MyBoolean);
    AssertEquals(NullDateTime, VTarget1.MyDateTime);
    AssertEquals('enum1', GetEnumProp(VTarget1, 'MyEnum'));
    AssertEquals('', GetSetProp(VTarget1, 'MySet'));
    VCfg.Target := VTarget2;
    VCfg.Configure;
    AssertEquals('A', VTarget2.MyChar);
    AssertEquals('ABC', VTarget2.MyString);
    AssertEquals(123, VTarget2.MyInteger);
    AssertEquals(456, VTarget2.MyInt64);
    AssertEquals(12.3, VTarget2.MyFloat);
    AssertEquals(45.6, VTarget2.MyCurrency);
    AssertEquals(True, VTarget2.MyBoolean);
    AssertEquals(StrToDateTime('2014/02/01 11:59:01'), VTarget2.MyDateTime);
    AssertEquals('enum2', GetEnumProp(VTarget2, 'MyEnum'));
    AssertEquals('enum1,enum3', GetSetProp(VTarget2, 'MySet'));
  finally
    VTarget1.Free;
    VTarget2.Free;
    VCfg.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookConfigurator);

end.

