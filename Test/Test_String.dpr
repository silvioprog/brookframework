program Test_String;

{$I Tests.inc}

uses
  SysUtils,
  BrookString;

procedure Test_StringWriteBytes(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
var
  OK: Boolean;
begin
  OK := False;
  try
    Assert(AStr.WriteBytes(nil, ALen) = 0);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);
  OK := False;
  try
    Assert(AStr.WriteBytes(AVal, 0) = 0);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);

  AStr.Clear;
  Assert(AStr.WriteBytes(AVal, ALen) = ALen);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringReadBytes(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
var
  OK: Boolean;
  VRes: TBytes;
begin
  OK := False;
  try
    Assert(AStr.ReadBytes(nil, ALen) = 0);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);
  OK := False;
  try
    Assert(AStr.ReadBytes(AVal, 0) = 0);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);

  SetLength(VRes, 15 * SizeOf(Byte));

  AStr.Clear;
  Assert(AStr.ReadBytes(VRes, ALen) = 0);

  AStr.WriteBytes(AVal, ALen);
  Assert(AStr.ReadBytes(VRes, SizeOf(Byte)) = SizeOf(Byte));
  Assert(CompareMem(@AVal[0], @VRes[0], SizeOf(Byte)));

  Assert(AStr.ReadBytes(VRes, ALen * 2) = ALen);

  Assert(AStr.ReadBytes(VRes, ALen) = ALen);
  Assert(CompareMem(@VRes[0], @AVal[0], ALen));
  Assert(vres[ALen] = 0);
end;

procedure Test_StringWrite(AStr: TBrookString; const AVal: string;
  ALen: NativeUInt);
var
  OK: Boolean;
begin
  OK := False;
  try
    AStr.Write('', TEncoding.UTF8);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);
  OK := False;
  try
    AStr.Write(AVal, nil);
  except
    on E: Exception do
      OK := E.ClassType = EArgumentNilException;
  end;
  Assert(OK);

  AStr.Clear;
  AStr.Write(AVal, TEncoding.UTF8);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringRead(AStr: TBrookString; const AVal: string);
var
  OK: Boolean;
begin
  OK := False;
  try
    AStr.Read(nil);
  except
    on E: Exception do
      OK := E.ClassType = EArgumentNilException;
  end;
  Assert(OK);

  AStr.Clear;
  Assert(AStr.Read.IsEmpty);
  Assert(AStr.Read(TEncoding.UTF8).IsEmpty);

  AStr.Write(AVal);
  Assert(AStr.Read.Equals(AVal));
  Assert(AStr.Read(TEncoding.UTF8).Equals(AVal));
end;

procedure Test_StrincContent(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(Length(AStr.Content) = 0);
  AStr.WriteBytes(AVal, ALen);
  Assert(CompareMem(@AStr.Content[0], @AVal[0], ALen));
end;

procedure Test_StringLength(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(AStr.Length = 0);

  AStr.WriteBytes(AVal, ALen);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringClear(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(AStr.Length = 0);
  AStr.WriteBytes(AVal, ALen);
  Assert(AStr.Length > 0);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringText(AStr: TBrookString; const AVal: string);
begin
  AStr.Clear;
  Assert(AStr.Text.IsEmpty);

  AStr.Text := AVal;
  Assert(AStr.Text = AVal);
end;

const
  VAL = 'abc123def456';
  LEN: NativeUInt = Length(VAL);
var
  VValB: TBytes;
  VStr: TBrookString;
begin
  VValB := TEncoding.UTF8.GetBytes(VAL);
  VStr := TBrookString.Create(nil);
  try
    Assert(Assigned(VStr.Handle));
    Test_StringWriteBytes(VStr, VValB, LEN);
    Test_StringReadBytes(VStr, VValB, LEN);
    Test_StringWrite(VStr, VAL, LEN);
    Test_StringRead(VStr, VAL);
    Test_StrincContent(VStr, VValB, LEN);
    Test_StringLength(VStr, VValB, LEN);
    Test_StringClear(VStr, VValB, LEN);
    Test_StringText(VStr, VAL);
  finally
    VStr.Free;
  end;
end.