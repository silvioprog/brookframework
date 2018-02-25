program Test_String;

{$I Tests.inc}

uses
  SysUtils,
  BrookString;

procedure Test_StringWriteBytes(AStr: TBrookString; const AVal: TBytes;
  ALen: NativeUInt);
begin
  Assert(AStr.WriteBytes(nil, ALen) = 0);
  Assert(AStr.WriteBytes(AVal, 0) = 0);

  AStr.Clear;
  Assert(AStr.WriteBytes(AVal, ALen) = ALen);
  Assert(AStr.Length = ALen);
end;

(*static inline void test_str_read(struct bk_str *str, const char *val, size_t len) {
    char res[16 * sizeof(char)];
    size_t res_len;

    ASSERT(bk_str_read(NULL, res, &res_len) == -EINVAL);
    ASSERT(bk_str_read(str, NULL, &res_len) == -EINVAL);
    ASSERT(bk_str_read(str, res, NULL) == -EINVAL);
    ASSERT(bk_str_read(str, res, (size_t * ) 0) == -EINVAL);

    bk_str_clear(str);
    res_len = 10;
	memcpy(res, val, len);
	res[len] = '\0';
    ASSERT(strlen(res) == len);
    ASSERT(bk_str_read(str, res, &res_len) == 0);
    ASSERT(res_len == 0);
    ASSERT(strlen(res) == 0);

    bk_str_write(str, val, len);
    res_len = sizeof(char);
    ASSERT(bk_str_read(str, res, &res_len) == 0);
    ASSERT(res_len == sizeof(char));
    ASSERT(memcmp(val, res, sizeof(char)) == 0);

    res_len = len + len;
    ASSERT(bk_str_read(str, res, &res_len) == 0);
    ASSERT(res_len == len);

    res_len = len;
    ASSERT(bk_str_read(str, res, &res_len) == 0);
    ASSERT(res_len == len);
    ASSERT(strcmp(res, val) == 0);
    ASSERT(res[res_len] == '\0');
}

static inline void test_str_printf_va(struct bk_str *str, const char *fmt, va_list ap) {
    ASSERT(bk_str_printf_va(NULL, fmt, ap) == -EINVAL);
    ASSERT(bk_str_printf_va(str, NULL, ap) == -EINVAL);
    ASSERT(bk_str_printf_va(str, fmt, NULL) == -EINVAL);

    bk_str_clear(str);
    bk_str_printf_va(str, fmt, ap);
    ASSERT(strcmp(bk_str_content(str), "abc123def456") == 0);

    /* the `test_str_printf_va()` is called and tested by `test_str_printf()`. */
}

static void test_str_printf(struct bk_str *str, const char *fmt, ...) {
    va_list ap;

    ASSERT(bk_str_printf(NULL, fmt) == -EINVAL);
    ASSERT(bk_str_printf(str, NULL) == -EINVAL);

    bk_str_clear(str);
    ASSERT(bk_str_printf(str, "") == 0);
    ASSERT(strlen(bk_str_content(str)) == 0);
    ASSERT(bk_str_printf(str, "%s%d", "abc", 123) == 0);
    ASSERT(strcmp(bk_str_content(str), "abc123") == 0);

    va_start(ap, fmt);
    test_str_printf_va(str, fmt, ap);
    va_end(ap);
}

static inline void test_str_content(struct bk_str *str, const char *val, size_t len) {
    ASSERT(!bk_str_content(NULL));

    bk_str_clear(str);
    ASSERT(strlen(bk_str_content(str)) == 0);
    bk_str_write(str, val, len);
    ASSERT(strcmp(bk_str_content(str), val) == 0);
}

static inline void test_str_length(struct bk_str *str, const char *val, size_t len) {
    size_t res_len;

    ASSERT(bk_str_length(NULL, &res_len) == -EINVAL);
    ASSERT(bk_str_length(str, NULL) == -EINVAL);

    bk_str_clear(str);
    res_len = 10;
    ASSERT(bk_str_length(str, &res_len) == 0);
    ASSERT(res_len == 0);

    bk_str_write(str, val, len);
    ASSERT(bk_str_length(str, &res_len) == 0);
    ASSERT(res_len == len);
}

static inline void test_str_clear(struct bk_str *str, const char *val, size_t len) {
    size_t tmp_len;

    ASSERT(bk_str_clear(NULL) == -EINVAL);

    bk_str_clear(str);
    bk_str_write(str, val, len);
    ASSERT(bk_str_length(str, &tmp_len) == 0);
    ASSERT(tmp_len > 0);
    bk_str_clear(str);
    ASSERT(bk_str_length(str, &tmp_len) == 0);
    ASSERT(tmp_len == 0);
}
*)

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
(*test_str_read(str, val, len);
test_str_printf(str, "%s%d%s%d", "abc", 123, "def", 456);
/* test_str_printf_va()` is already called by `test_str_printf`. */
test_str_content(str, val, len);
test_str_clear(str, val, len);
test_str_length(str, val, len);*)
  finally
    VStr.Free;
  end;
end.
