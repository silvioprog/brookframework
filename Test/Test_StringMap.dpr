program Test_StringMap;

{$I Tests.inc}

uses
  libbrook,
  Marshalling,
  BrookStringMap;

type
  TLocalStringMap = class(TBrookStringMap)
  public
    procedure LocalDestroy;
  end;

procedure TLocalStringMap.LocalDestroy;
begin
  inherited Destroy;
  { checks if the handle was really freed and 'nilified'. }
  Assert(not Assigned(Handle));
  bk_strmap_cleanup(Handle);
end;

procedure Test_StringMapNameValue;
var
  VPair: TBrookStringPair;
begin
  VPair := TBrookStringPair.Create('', '');
  Assert(VPair.Name = '');
  Assert(VPair.Value = '');
  VPair := TBrookStringPair.Create('abc', '123');
  Assert(VPair.Name = 'abc');
  Assert(VPair.Value = '123');
end;

procedure Test_StringMapOwnsHandle;
var
  M: TMarshaller;
  Vhandle: Pbk_strmap;
  VMap: TBrookStringMap;
begin
  Vhandle := nil;
  bk_strmap_add(@Vhandle, M.ToCString('abc'), M.ToCString('123'));
  VMap := TBrookStringMap.Create(Vhandle);
  try
    Assert(Assigned(VMap.Handle));
    Assert(VMap.Handle = Vhandle);
  finally
    VMap.Destroy;
    bk_strmap_cleanup(@Vhandle);
    Assert(not Assigned(Vhandle));
  end;
  VMap := TLocalStringMap.Create(nil);
  try
    VMap.Add('abc', '123');
    Assert(Assigned(VMap.Handle));
  finally
    TLocalStringMap(VMap).LocalDestroy;
  end;
end;

procedure Test_StringMapHandle(AMap: TBrookStringMap);
var
  VMap: TBrookStringMap;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  Assert(Assigned(AMap.Handle));
  VMap := TBrookStringMap.Create(AMap.Handle);
  try
    Assert(VMap.Handle = AMap.Handle);
  finally
    VMap.Free;
  end;
  VMap := TBrookStringMap.Create(nil);
  try
    VMap.Add('abc', '123');
    Assert(Assigned(VMap.Handle));
    Assert(VMap.Handle <> AMap.Handle);
  finally
    VMap.Free;
  end;
end;

procedure Test_StringMapAdd(AMap: TBrookStringMap; const AName, AVal: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add('', AVal);
  Assert(AMap.Count = 1);
  AMap.Clear;
  AMap.Add(AName, '');
  Assert(AMap.Count = 1);

  AMap.Clear;
  AMap.Add(AName, AVal);
  AMap.Add(AName, AVal);
  Assert(AMap.Count = 2);
end;

procedure Test_StringMapAddOrSet(AMap: TBrookStringMap; const AName,
  AVal: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.AddOrSet('', AVal);
  Assert(AMap.Count = 1);
  AMap.Clear;
  AMap.AddOrSet(AName, '');
  Assert(AMap.Count = 1);

  AMap.Clear;
  AMap.AddOrSet(AName, AVal);
  AMap.AddOrSet(AName, AVal);
  Assert(AMap.Count = 1);
end;

procedure Test_StringMapFind(AMap: TBrookStringMap; const AName, AVal: string);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AVal);
  Assert(AMap.Count = 1);
  Assert(not AMap.Find('', VPair));
  Assert(VPair.Name = '');
  Assert(VPair.Value = '');
  Assert(not AMap.Find('xxx', VPair));
  Assert(VPair.Name = '');
  Assert(VPair.Value = '');
  Assert(not AMap.Find('yyy', VPair));
  Assert(VPair.Name = '');
  Assert(VPair.Value = '');

  AMap.Add('', '');
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 4);
  Assert(AMap.Find(AName, VPair));
  Assert((VPair.Name = AName) and (VPair.Value = AVal));
  Assert(AMap.Find('', VPair));
  Assert((VPair.Name = '') and (VPair.Value = ''));
  Assert(AMap.Find('xxx', VPair));
  Assert((VPair.Name = 'xxx') and (VPair.Value = 'yyy'));
  Assert(AMap.Find('yyy', VPair));
  Assert((VPair.Name = 'yyy') and (VPair.Value = 'xxx'));
end;

const
  NAME = 'abç';
  VAL = 'déf';
var
  VMap: TBrookStringMap;
begin
  Test_StringMapNameValue;
  Test_StringMapOwnsHandle;
  VMap := TBrookStringMap.Create(nil);
  try
    Test_StringMapHandle(VMap);
    Test_StringMapAdd(VMap, NAME, VAL);
    Test_StringMapAddOrSet(VMap, NAME, VAL);
    Test_StringMapFind(VMap, NAME, VAL);
  finally
    VMap.Free;
  end;
end.
