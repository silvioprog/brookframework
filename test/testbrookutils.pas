unit testbrookutils;

{$mode objfpc}{$H+}

interface

uses
  BrookUtils, BrookConsts, fpcunit, testregistry, sysutils, Classes, fpjson;

type
  TTestBrookUtils = class(TTestCase)
  published
    procedure TestExtractPathLevels;
    procedure TestGetPathLevel;
    procedure TestGetPathLevels;
    procedure TestMacthMethod;
    procedure TestDeleteFiles;
    procedure TestFileDate;
    procedure TestFileSetDate;
    procedure TestJSONCopy;
  end;

implementation

procedure TTestBrookUtils.TestExtractPathLevels;
var
  S, R, VLvl: string;
  VEndDelim: Boolean = False;
begin
  S := '/a/b/c/';
  R := '';
  VLvl := '';
  BrookExtractPathLevels(S, R, VLvl, VEndDelim);
  AssertEquals('a', R);
  AssertEquals('a', VLvl);
  AssertEquals(True, VEndDelim);
  BrookExtractPathLevels(S, R, VLvl, VEndDelim);
  AssertEquals('a/b', R);
  AssertEquals('b', VLvl);
  BrookExtractPathLevels(S, R, VLvl, VEndDelim);
  AssertEquals('a/b/c', R);
  AssertEquals('c', VLvl);
end;

procedure TTestBrookUtils.TestGetPathLevel;
begin
  AssertEquals('b', BrookGetPathLevel('/a/b/c/', 1));
end;

procedure TTestBrookUtils.TestGetPathLevels;
begin
  AssertEquals('b/c/', BrookGetPathLevels('/a/b/c/', 1));
end;

procedure TTestBrookUtils.TestMacthMethod;
begin
  AssertEquals(True, BrookMacthMethod(rmPost, 'POST'));
end;

procedure TTestBrookUtils.TestDeleteFiles;
var
  I: Integer;
  VFiles: TStringList;
  VFileName, VPath: string;
begin
  VFiles := TStringList.Create;
  try
    VPath := ExtractFilePath(ParamStr(0));
    for I := 1 to 10 do
    begin
      VFileName := VPath + 'prefix_' + IntToStr(I);
      VFiles.Add(VFileName);
      VFiles.SaveToFile(VFileName);
    end;
    BrookDeleteFiles(VPath, NullDate, ES, 'prefix_');
    for I := 0 to Pred(VFiles.Count) do
      AssertEquals(False, FileExists(VFiles[I]));
  finally
    VFiles.Free;
  end;
end;

procedure TTestBrookUtils.TestFileDate;
var
  VFiles: TStringList;
  VFileName, VPath: string;
begin
  VFiles := TStringList.Create;
  try
    VPath := ExtractFilePath(ParamStr(0));
    VFileName := VPath + 'file';
    VFiles.SaveToFile(VFileName);
    AssertEquals(Trunc(Now), Trunc(BrookFileDate(VFileName)));
    DeleteFile(VFileName);
  finally
    VFiles.Free;
  end;
end;

procedure TTestBrookUtils.TestFileSetDate;
var
  VFiles: TStringList;
  VFileName, VPath: string;
begin
  VFiles := TStringList.Create;
  try
    VPath := ExtractFilePath(ParamStr(0));
    VFileName := VPath + 'file';
    VFiles.SaveToFile(VFileName);
    BrookFileSetDate(VFileName, Now - 1);
    AssertEquals(Trunc(Now - 1), Trunc(BrookFileDate(VFileName)));
    DeleteFile(VFileName);
  finally
    VFiles.Free;
  end;
end;

procedure TTestBrookUtils.TestJSONCopy;
var
  J1, J2: TJSONObject;
begin
  J1 := TJSONObject.Create(['a', 'b']);
  J2 := TJSONObject.Create;
  try
    BrookJSONCopy(J1, J2);
    AssertEquals(J2.AsJSON, J1.AsJSON);
  finally
    J1.Free;
    J2.Free;
  end;
end;

initialization
  RegisterTest(TTestBrookUtils);

end.

