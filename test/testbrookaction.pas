unit testbrookaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, BrookRouter, BrookUtils, BrookConsts, fpcunit, testregistry,
  HTTPDefs, fpjson, sysutils, Classes;

type
  TAction1 = class(TBrookAction)
  public
    procedure Get; override;
  end;

  TAction2 = class(TAction1)
  end;

  TTestBrookAction = class(TTestCase)
  private
    procedure ClearRoutes(R: TBrookRoutes);
  published
    procedure TestFillFields;
    procedure TestFillParams;
    procedure TestFillValues;
    procedure TestRegister;
    procedure TestUrlFor;
    procedure TestWrite;
  end;

implementation

const
  SOME_SPECIAL_CHARS = '"!@#$%¨&*()_+''¹²³£¢¬§-=`{^}:?<>´[~],.;/ªº°ãñáàâç';

{ TAction1 }

procedure TAction1.Get;
var
  VArray: TJSONArray;
  VObject: TJSONObject;
  VStrings: TStringList;
begin
  VArray := TJSONArray.Create(['abc', 123, 1.23, True]);
  VObject := TJSONObject.Create(['field1', 'abc', 'field2', 123, 'field3', 1.23,
    'field4', True]);
  VStrings := TStringList.Create;
  try
    VStrings.Add('abc');
    VStrings.Add('123');
    Write('abc');
    Write(123);
    Write(1.23);
    Write(True);
    Write(VArray);
    Write(VObject);
    Write(VStrings);
    WriteLn('abc');
    WriteLn(123);
    WriteLn(1.23);
    WriteLn(True);
    WriteLn(VArray);
    WriteLn(VObject);
    WriteLn(VStrings);
  finally
    VArray.Free;
    VObject.Free;
    VStrings.Free;
  end;
end;

{ TTestBrookAction }

procedure TTestBrookAction.ClearRoutes(R: TBrookRoutes);
var
  P: PBrookRoute;
begin
  for P in R.List do
    Dispose(P);
  R.List.Clear;
end;

procedure TTestBrookAction.TestFillFields;
var
  VRec: TRequest;
  VAct: TBrookAction;
begin
  VAct := TBrookAction.Create;
  VRec := TRequest.Create;
  try
    VRec.ContentFields.Add('field1=abc');
    VRec.ContentFields.Add('field2=123');
    VRec.ContentFields.Add('field2=123'); // same field
    VRec.ContentFields.Add('field3=' + SOME_SPECIAL_CHARS);
    VAct.FillFields(VRec);
    AssertEquals(4, VAct.Fields.Count);
    AssertEquals('abc', VAct.Fields['field1'].AsString);
    AssertEquals(123, VAct.Fields['field2'].AsInteger);
    AssertEquals(SOME_SPECIAL_CHARS, VAct.Fields['field3'].AsString);
  finally
    VRec.Free;
    VAct.Free;
  end;
end;

procedure TTestBrookAction.TestFillParams;
var
  VRec: TRequest;
  VAct: TBrookAction;
begin
  VAct := TBrookAction.Create;
  VRec := TRequest.Create;
  try
    VRec.QueryFields.Add('param1=abc');
    VRec.QueryFields.Add('param2=123');
    VRec.QueryFields.Add('param2=123'); // same param
    VRec.QueryFields.Add('param3=' + SOME_SPECIAL_CHARS);
    VAct.FillParams(VRec);
    AssertEquals(4, VAct.Params.Count);
    AssertEquals('abc', VAct.Params['param1'].AsString);
    AssertEquals(123, VAct.Params['param2'].AsInteger);
    AssertEquals(SOME_SPECIAL_CHARS, VAct.Params['param3'].AsString);
  finally
    VRec.Free;
    VAct.Free;
  end;
end;

procedure TTestBrookAction.TestFillValues;
var
  VRec: TRequest;
  VAct: TBrookAction;
  VNames, VValues: TBrookArrayOfString;
begin
  VAct := TBrookAction.Create;
  VRec := TRequest.Create;
  try
    SetLength(VNames, 4);
    VNames[0] := 'value1';
    VNames[1] := 'value2';
    VNames[2] := 'value2'; // same name
    VNames[3] := 'value3';
    SetLength(VValues, 4);
    VValues[0] := 'abc';
    VValues[1] := '123';
    VValues[2] := '123'; // same name
    VValues[3] := SOME_SPECIAL_CHARS;
    VAct.FillValues(VNames, VValues);
    AssertEquals(4, VAct.Values.Count);
    AssertEquals('abc', VAct.Values['value1'].AsString);
    AssertEquals(123, VAct.Values['value2'].AsInteger);
    AssertEquals(SOME_SPECIAL_CHARS, VAct.Values['value3'].AsString);
  finally
    VRec.Free;
    VAct.Free;
  end;
end;

procedure TTestBrookAction.TestRegister;
var
  VRoutes: TBrookRoutes;
begin
  VRoutes := TBrookRouter.Service.Routes;
  ClearRoutes(VRoutes);
  TAction1.Register('/action1', rmGet, True);
  TAction2.Register('/action2', rmPost);
  AssertEquals(2, VRoutes.List.Count);
  AssertEquals('TAction1', VRoutes.Items[0]^.ActionClass.ClassName);
  AssertEquals(True, VRoutes.Items[0]^.Default);
  AssertEquals('/action1', VRoutes.Items[0]^.Pattern);
  AssertEquals(True, BrookSettings.Mapped);
  AssertTrue('Invalid method', VRoutes.Items[0]^.Method = rmGet);
end;

procedure TTestBrookAction.TestUrlFor;
var
  VAct1: TAction1;
  VAct2: TAction2;
  VRoutes: TBrookRoutes;
begin
  VAct1 := TAction1.Create;
  VAct2 := TAction2.Create;
  try
    VRoutes := TBrookRouter.Service.Routes;
    ClearRoutes(VRoutes);
    TAction1.Register('/action1', rmGet, True);
    TAction2.Register('/action2/:val', rmGet);
    AssertEquals('/action1', VAct1.UrlFor(TAction1));
    AssertEquals('/action2/abc', VAct2.UrlFor(TAction2, ['abc']));
  finally
    VAct1.Free;
    VAct2.Free;
  end;
end;

procedure TTestBrookAction.TestWrite;
var
  S: string;
  D: double;
  VAct: TAction1;
  VRes: TResponse;
  VReq: TRequest;
  VRoutes: TBrookRoutes;
begin
  VReq := TRequest.Create;
{$WARNINGS OFF}
  VRes := TResponse.Create(VReq);
{$WARNINGS ON}
  VAct := TAction1.Create;
  try
    VRoutes := TBrookRouter.Service.Routes;
    ClearRoutes(VRoutes);
    VReq.Method := 'GET';
    VAct.DoRequest(VReq, VRes);
    AssertEquals(30, VRes.Contents.Count);
    AssertEquals('abc', VRes.Contents.ValueFromIndex[0]);
    AssertEquals(IntToStr(123), VRes.Contents.ValueFromIndex[1]);
    AssertEquals(FloatToStr(1.23), VRes.Contents.ValueFromIndex[2]);
    AssertEquals(BoolToStr(True), VRes.Contents.ValueFromIndex[3]);
    AssertEquals('abc', VRes.Contents.ValueFromIndex[4]);
    AssertEquals(IntToStr(123), VRes.Contents.ValueFromIndex[5]);
    D := 1.23;
    Str(D, S);
    if (S <> '') and (S[1] = ' ') then
      Delete(S, 1, 1);
    AssertEquals(S, VRes.Contents.ValueFromIndex[6]);
    AssertEquals('abc', VRes.Contents.ValueFromIndex[8]);
    AssertEquals(IntToStr(123), VRes.Contents.ValueFromIndex[9]);
    AssertEquals(S, VRes.Contents.ValueFromIndex[10]);
    AssertEquals(True, StrToBool(VRes.Contents.ValueFromIndex[11]));
    AssertEquals('abc', VRes.Contents.ValueFromIndex[12]);
    AssertEquals(IntToStr(123), VRes.Contents.ValueFromIndex[13]);
    AssertEquals('abc' + BR, VRes.Contents.ValueFromIndex[14]);
  finally
    VAct.Free;
    VRes.Free;
    VReq.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookAction);

end.

