program Test_Utils;

{$I Tests.inc}

uses
  SysUtils,
  BrookUtils;

procedure Test_Version;
begin
  Assert(BrookVersion > 0);
  ASSERT(BrookVersionStr <> '');
end;

procedure Test_Memory;
var
  Vbuf: Pointer;
begin
  Vbuf := BrookAlloc(10);
  Assert(Assigned(Vbuf));
  BrookFree(Vbuf);
end;

begin
  Test_Version;
  Test_Memory;
end.