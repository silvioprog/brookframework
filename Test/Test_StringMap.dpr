(*    _____   _____    _____   _____   _   __
 *   |  _  \ |  _  \  /  _  \ /  _  \ | | / /
 *   | |_) | | |_) |  | | | | | | | | | |/ /
 *   |  _ <  |  _ <   | | | | | | | | |   (
 *   | |_) | | | \ \  | |_| | | |_| | | |\ \
 *   |_____/ |_|  \_\ \_____/ \_____/ |_| \_\
 *
 *   –– a small library which helps you write quickly REST APIs.
 *
 * Copyright (c) 2012-2018 Silvio Clecio <silvioprog@gmail.com>
 *
 * This file is part of Brook library.
 *
 * Brook library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Brook library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Brook library.  If not, see <http://www.gnu.org/licenses/>.
 *)

program Test_StringMap;

{$I Tests.inc}

{$IFDEF FPC}
 {$WARN 5024 OFF}
{$ENDIF}

uses
  SysConst,
  SysUtils,
  libbrook,
  Marshalling,
  BrookStringMap;

type
  TLocalStringMap = class(TBrookStringMap)
  private
    FOperation: TBrookStringMapOperation;
  protected
    procedure DoChange(AOperation: TBrookStringMapOperation); override;
  public
    procedure LocalDestroy;
    property Operation: TBrookStringMapOperation read FOperation;
  end;

procedure TLocalStringMap.DoChange(AOperation: TBrookStringMapOperation);
begin
  FOperation := AOperation;
  inherited DoChange(AOperation);
end;

procedure TLocalStringMap.LocalDestroy;
begin
  inherited Destroy;
  BkCheckLibrary;
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
  BkCheckLibrary;
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

procedure Test_StringMapOnChange;
var
  VMap: TLocalStringMap;
begin
  VMap := TLocalStringMap.Create(nil);
  try
    Assert(VMap.Operation = bkmoNone);
    VMap.Add('abc', '123');
    Assert(VMap.Operation = bkmoAdd);
    VMap.AddOrSet('def', '456');
    Assert(VMap.Operation = bkmoAddOrSet);
    VMap.Remove('abc');
    Assert(VMap.Operation = bkmoRemove);
    VMap.Clear;
    Assert(VMap.Operation = bkmoNone);
  finally
    VMap.Free;
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

procedure Test_StringMapAdd(AMap: TBrookStringMap; const AName, AValue: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add('', AValue);
  Assert(AMap.Count = 1);
  AMap.Clear;
  AMap.Add(AName, '');
  Assert(AMap.Count = 1);

  AMap.Clear;
  AMap.Add(AName, AValue);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 2);
end;

procedure Test_StringMapAddOrSet(AMap: TBrookStringMap; const AName,
  AValue: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.AddOrSet('', AValue);
  Assert(AMap.Count = 1);
  AMap.Clear;
  AMap.AddOrSet(AName, '');
  Assert(AMap.Count = 1);

  AMap.Clear;
  AMap.AddOrSet(AName, AValue);
  AMap.AddOrSet(AName, AValue);
  Assert(AMap.Count = 1);
end;

procedure Test_StringMapFind(AMap: TBrookStringMap; const AName,
  AValue: string);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
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
  Assert((VPair.Name = AName) and (VPair.Value = AValue));
  Assert(AMap.Find('', VPair));
  Assert((VPair.Name = '') and (VPair.Value = ''));
  Assert(AMap.Find('xxx', VPair));
  Assert((VPair.Name = 'xxx') and (VPair.Value = 'yyy'));
  Assert(AMap.Find('yyy', VPair));
  Assert((VPair.Name = 'yyy') and (VPair.Value = 'xxx'));
end;

procedure Test_StringMapRemove(AMap: TBrookStringMap; const AName,
  AValue: string);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  AMap.Remove('');
  Assert(AMap.Count = 1);
  AMap.Remove('xxx');
  Assert(AMap.Count = 1);
  AMap.Remove('yyy');
  Assert(AMap.Count = 1);

  AMap.Add('', '');
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 4);

  Assert(AMap.Find(AName, VPair));
  AMap.Remove(AName);
  Assert(AMap.Count = 3);
  Assert(not AMap.Find(AName, VPair));

  Assert(AMap.Find('', VPair));
  AMap.Remove('');
  Assert(AMap.Count = 2);
  Assert(not AMap.Find('', VPair));

  Assert(AMap.Find('xxx', VPair));
  AMap.Remove('xxx');
  Assert(AMap.Count = 1);
  Assert(not AMap.Find('xxx', VPair));

  Assert(AMap.Find('yyy', VPair));
  AMap.Remove('yyy');
  Assert(AMap.Count = 0);
  Assert(not AMap.Find('yyy', VPair));
end;

function StringMapIterateEmpty(AData: Pointer;
  APair: TBrookStringPair): Integer;
begin
  Result := 0;
end;

function StringMapIterate123(AData: Pointer; APair: TBrookStringPair): Integer;
begin
  Result := 123;
end;

function StringMapIterateConcat(AData: Pointer;
  APair: TBrookStringPair): Integer;
var
  S: PString absolute AData;
begin
  S^ := Concat(S^, APair.Name, APair.Value);
  Result := 0;
end;

procedure Test_StringMapIterate(AMap: TBrookStringMap);
var
  S: string;
  OK: Boolean;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');

  AMap.Iterate(StringMapIterateEmpty, nil);
  OK := False;
  try
    AMap.Iterate(StringMapIterate123, nil);
  except
    on E: EOSError do
      OK := EOSError(E).ErrorCode = 123;
  end;
  Assert(OK);

  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S = 'abc123def456');
end;

function StringMapSortEmpty(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
var
  S: PString absolute AData;
begin
  S^ := Concat(S^, S^);
  Result := 0;
end;

function StringMapSortNameDesc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairB.Name, APairA.Name);
end;

function StringMapSortNameAsc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairA.Name, APairB.Name);
end;

function StringMapSortValueDesc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairB.Value, APairA.Value);
end;

function StringMapSortValueAsc(AData: Pointer; APairA,
  APairB: TBrookStringPair): Integer;
begin
  Result := CompareStr(APairA.Value, APairB.Value);
end;

procedure Test_StringMapSort(AMap: TBrookStringMap);
var
  S: string;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');

  S := 'abc';
  AMap.Sort(StringMapSortEmpty, @S);
  Assert(S = 'abcabc');

  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S = 'abc123def456');
  AMap.Sort(StringMapSortNameDesc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S = 'def456abc123');

  AMap.Sort(StringMapSortNameAsc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S = 'abc123def456');

  AMap.Sort(StringMapSortValueDesc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S = 'def456abc123');

  AMap.Sort(StringMapSortValueAsc, nil);
  S := '';
  AMap.Iterate(StringMapIterateConcat, @S);
  Assert(S = 'abc123def456');
end;

procedure Test_StringMapCount(AMap: TBrookStringMap; const AName,
  AValue: string);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  AMap.Add('xxx', 'yyy');
  Assert(AMap.Count = 2);
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 3);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 4);
  AMap.Remove(AName);
  Assert(AMap.Count = 3);
  AMap.Clear;
  Assert(AMap.Count = 0);
end;

procedure Test_StringMapTryValue(AMap: TBrookStringMap; const AName,
  AValue: string);
var
  S: string;
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add(AName, AValue);
  Assert(AMap.Count = 1);
  Assert(not AMap.TryValue('', S));
  Assert(S = '');

  Assert(not AMap.TryValue('xxx', S));
  Assert(S = '');
  Assert(not AMap.TryValue('yyy', S));
  Assert(S = '');

  AMap.Add('', '');
  AMap.Add('xxx', 'yyy');
  AMap.Add('yyy', 'xxx');
  Assert(AMap.Count = 4);
  Assert(AMap.TryValue(AName, S));
  Assert(S = AValue);
  Assert(AMap.TryValue('', S));
  Assert(S = '');
  Assert(AMap.TryValue('xxx', S));
  Assert(S = 'yyy');
  Assert(AMap.TryValue('yyy', S));
  Assert(S = 'xxx');
end;

procedure Test_StringMapFirst(AMap: TBrookStringMap);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  AMap.First(VPair);
  Assert((VPair.Name = 'abc') and (VPair.Value = '123'));
  AMap.Next(VPair);
  AMap.Next(VPair);
  Assert((VPair.Name = 'xxx') and (VPair.Value = 'yyy'));
  AMap.First(VPair);
  Assert((VPair.Name = 'abc') and (VPair.Value = '123'));
end;

procedure Test_StringMapValues(AMap: TBrookStringMap);
begin
  AMap.Clear;
  Assert(AMap.Values['abc'] = '');
  AMap.Values['abc'] := '123';
  Assert(AMap.Values['abc'] = '123');
  Assert(AMap.Values['def'] = '');
  AMap.Values['def'] := '456';
  Assert(AMap.Values['def'] = '456');
  Assert(AMap.Values['xxx'] = '');
  AMap.Values['xxx'] := 'yyy';
  Assert(AMap.Values['xxx'] = 'yyy');
  Assert(AMap.Count = 3);
  AMap.Values['xxx'] := 'yyy';
  Assert(AMap.Count = 3);
end;

procedure Test_StringMapEOF(AMap: TBrookStringMap);
var
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  AMap.First(VPair);
  Assert(not AMap.EOF);
  AMap.Next(VPair);
  Assert(not AMap.EOF);
  AMap.Next(VPair);
  Assert(not AMap.EOF);
  AMap.Next(VPair);
  Assert(AMap.EOF);
  AMap.Next(VPair);
  Assert(AMap.EOF);
end;

procedure Test_StringMapNext(AMap: TBrookStringMap);
var
  S: string;
  VPair: TBrookStringPair;
begin
  AMap.Clear;
  VPair := Default(TBrookStringPair);
  Assert(AMap.Next(VPair));
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  S := '';
  AMap.First(VPair);
  while not AMap.EOF do
  begin
    S := Concat(S, VPair.Name, VPair.Value);
    AMap.Next(VPair);
  end;
  Assert(S = 'abc123def456xxxyyy');
end;

procedure Test_StringMapEnumerator(AMap: TBrookStringMap);
var
  I: Byte;
  S: string;
  P: TBrookStringPair;
begin
  AMap.Clear;
  for I := 1 to 3 do
  begin
    S := I.ToString;
    AMap.Add(Concat('name', S), Concat('value', S));
  end;
  I := 0;
  for P in AMap do
  begin
    S := Succ(I).ToString;
    Assert((P.Name = Concat('name', S)) and (P.Value = Concat('value', S)));
    Inc(I);
  end;
  Assert(I = 3);
end;

procedure Test_StringMapClear(AMap: TBrookStringMap);
begin
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Add('abc', '123');
  AMap.Add('def', '456');
  AMap.Add('xxx', 'yyy');
  Assert(AMap.Count = 3);
  AMap.Clear;
  Assert(AMap.Count = 0);
  AMap.Clear;
  Assert(AMap.Count = 0);
end;

const
  NAME = 'abç';
  VAL = 'déf';
var
  VMap: TBrookStringMap;
begin
  Test_StringMapNameValue;
  Test_StringMapOwnsHandle;
  Test_StringMapOnChange;
  VMap := TBrookStringMap.Create(nil);
  try
    Test_StringMapHandle(VMap);
    Test_StringMapAdd(VMap, NAME, VAL);
    Test_StringMapAddOrSet(VMap, NAME, VAL);
    Test_StringMapFind(VMap, NAME, VAL);
    Test_StringMapRemove(VMap, NAME, VAL);
    Test_StringMapIterate(VMap);
    Test_StringMapSort(VMap);
    Test_StringMapCount(VMap, NAME, VAL);
    Test_StringMapTryValue(VMap, NAME, VAL);
    Test_StringMapFirst(VMap);
    Test_StringMapValues(VMap);
    Test_StringMapEOF(VMap);
    Test_StringMapNext(VMap);
    Test_StringMapEnumerator(VMap);
    Test_StringMapClear(VMap);
  finally
    VMap.Free;
  end;
end.
