program Test_libbrook;

{$I Tests.inc}

uses
  SysUtils,
  libbrook;

procedure Test_LoadLibrary;
begin
  BkUnloadLibrary;
  Assert(BkLoadLibrary('') = NilHandle);
  Assert(BkLoadLibrary('abc') = NilHandle);
  Assert(BkLoadLibrary(BK_LIB_NAME) <> NilHandle);
end;

procedure Test_UnloadLibrary;
begin
  Assert(BkLoadLibrary(BK_LIB_NAME) <> NilHandle);
  Assert(BkUnloadLibrary = NilHandle);
end;

procedure Test_CheckLibrary;
var
  OK: Boolean;
begin
  OK := False;
  try
    BkUnloadLibrary;
    BkCheckLibrary;
  except
    on E: Exception do
      OK := (E.ClassType = EBkLibraryNotLoaded) and (E.Message =
        Format(SBkLibraryNotLoaded, [BK_LIB_NAME]));
  end;
  Assert(OK);
end;

begin
  Test_LoadLibrary;
  Test_UnloadLibrary;
  Test_CheckLibrary;
end.
