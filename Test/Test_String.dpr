program Test_String;

{$I Tests.inc}

uses
  SysUtils,
  libbrook,
  BrookString;

type
  TLocalString = class(TBrookString)
  public
    procedure LocalDestroy;
  end;

procedure TLocalString.LocalDestroy;
begin
  inherited Destroy;
  { checks if the handle was really freed and 'nilified'. }
  Assert(not Assigned(Handle));
  Assert(bk_str_clear(Handle) <> 0);
end;

procedure Test_StringHandle(AStr: TBrookString);
var
  VStr: TBrookString;
begin
  Assert(Assigned(AStr.Handle));
  VStr := TBrookString.Create(AStr.Handle);
  try
    Assert(VStr.Handle = AStr.Handle);
  finally
    VStr.Free;
  end;
  VStr := TBrookString.Create(nil);
  try
    Assert(Assigned(VStr.Handle));
    Assert(VStr.Handle <> AStr.Handle);
  finally
    VStr.Free;
  end;
end;

procedure Test_StringOwnsHandle;
var
  Vhandle: Pbk_str;
  VStr: TBrookString;
begin
  Vhandle := bk_str_new;
  Assert(Assigned(Vhandle));
  VStr := TBrookString.Create(Vhandle);
  try
    Assert(Assigned(VStr.Handle));
    Assert(VStr.Handle = Vhandle);
  finally
    VStr.Destroy;
    bk_str_free(Vhandle);
  end;
  VStr := TLocalString.Create(nil);
  try
    Assert(Assigned(VStr.Handle));
  finally
    TLocalString(VStr).LocalDestroy;
  end;
end;

procedure Test_StringCopyBytes(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
var
  OK: Boolean;
begin
  OK := False;
  try
    Assert(AStr.CopyBytes(nil, ALen) = 0);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);
  OK := False;
  try
    Assert(AStr.CopyBytes(AVal, 0) = 0);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);

  AStr.Clear;
  Assert(AStr.CopyBytes(AVal, ALen) = ALen);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringCopy(AStr: TBrookString; const AVal: string;
  ALen: NativeUInt);
var
  OK: Boolean;
begin
  OK := False;
  try
    AStr.Copy('', TEncoding.UTF8);
  except
    on E: Exception do
      OK := E.ClassType = EOSError;
  end;
  Assert(OK);
  OK := False;
  try
    AStr.Copy(AVal, nil);
  except
    on E: Exception do
      OK := E.ClassType = EArgumentNilException;
  end;
  Assert(OK);

  AStr.Clear;
  AStr.Copy(AVal, TEncoding.UTF8);
  Assert(AStr.Length = ALen);
end;

procedure Test_StrincContent(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(Length(AStr.Content) = 0);
  AStr.CopyBytes(AVal, ALen);
  Assert(CompareMem(@AStr.Content[0], @AVal[0], ALen));
end;

procedure Test_StringLength(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(AStr.Length = 0);

  AStr.CopyBytes(AVal, ALen);
  Assert(AStr.Length = ALen);
end;

procedure Test_StringClear(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  AStr.Clear;
  Assert(AStr.Length = 0);
  AStr.CopyBytes(AVal, ALen);
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

procedure Test_StringExtra(AStr: TBrookString);
var
  VStr: TBrookString;
begin
  AStr.Clear;
  AStr.Copy('abc');
  Assert(AStr.Text = 'abc');
  VStr := TBrookString.Create(AStr.Handle);
  try
    VStr.Copy('123');
  finally
    VStr.Free;;
  end;
  Assert(AStr.Text = 'abc123');
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
    Test_StringHandle(VStr);
    Test_StringOwnsHandle;
    Test_StringCopyBytes(VStr, VValB, LEN);
    Test_StringCopy(VStr, VAL, LEN);
    Test_StrincContent(VStr, VValB, LEN);
    Test_StringLength(VStr, VValB, LEN);
    Test_StringClear(VStr, VValB, LEN);
    Test_StringText(VStr, VAL);
    Test_StringExtra(VStr);
  finally
    VStr.Destroy;
  end;
end.
