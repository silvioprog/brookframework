unit BrookStringMap;

{$I Brook.inc}

interface

uses
{$IF DEFINED(MSWINDOWS)}
  Windows,
{$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
  BaseUnix,
{$ELSEIF DEFINED(POSIX)}
  Posix.Errno,
{$ENDIF}
  SysUtils,
  libbrook,
  Marshalling,
  BrookHandledClasses;

const
  BROOK_STRMAP_MAX_BUF: NativeUInt = 4096; // 4 kB

type
  TBrookStringMap = class;

  TBrookStringPair = record
  private
    FName: string;
    FValue: string;
  public
    constructor Create(const AName, AValue: string);
    property Name: string read FName;
    property Value: string read FValue;
  end;

  TBrookStringMapEnumerator = class
  private
    FMap: TBrookStringMap;
    FCurr: TBrookStringPair;
    FBOF: Boolean;
  public
    constructor Create(AMap: TBrookStringMap);
    function GetCurrent: TBrookStringPair;
    function MoveNext: Boolean;
    property Current: TBrookStringPair read GetCurrent;
  end;

  TBrookStringMapIterator = function(AData: Pointer;
    APair: TBrookStringPair): Integer;

  TBrookStringMapComparator = function(AData: Pointer;
    APairA, APairB: TBrookStringPair): Integer;

  TBrookStringMap = class(TBrookHandledPersistent)
  private
    Fpair: Pbk_strmap;
    Fmap: Pbk_strmap;
    FOwnsHandle: Boolean;
    function GetCount: Integer;
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
  protected
    class function DoIterate(Acls: Pcvoid;
      Apair: Pbk_strmap): cint; cdecl; static;
    class function DoSort(Acls: Pcvoid; Apair_a: Pbk_strmap;
      Apair_b: Pbk_strmap): cint; cdecl; static;
    class function CreatePair(
      Apair: Pbk_strmap): TBrookStringPair; static; inline;
    function GetHandle: Pointer; override;
    procedure SetHandle(AHandle: Pointer); override;
    function GetOwnsHandle: Boolean; override;
    procedure SetOwnsHandle(AValue: Boolean); override;
    function IsEOF: Boolean; virtual;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    function GetEnumerator: TBrookStringMapEnumerator;
    procedure Add(const AName, AValue: string); virtual;
    procedure AddOrSet(const AName, AValue: string); virtual;
    procedure Remove(const AName: string); virtual;
    procedure Clear; virtual;
    function Find(const AName: string;
      out APair: TBrookStringPair): Boolean; virtual;
    function TryValue(const AName: string;
      out AValue: string): Boolean; virtual;
    function First(out APair: TBrookStringPair): Boolean; virtual;
    function Next(out APair: TBrookStringPair): Boolean; virtual;
    function Iterate(AIterator: TBrookStringMapIterator;
      AData: Pointer): Boolean; virtual;
    procedure Sort(AComparator: TBrookStringMapComparator;
      AData: Pointer); virtual;
    property Count: Integer read GetCount;
    property Values[const AName: string]: string read GetValue
      write SetValue; default;
    property EOF: Boolean read IsEOF;
  end;

implementation

{$IFNDEF POSIX}
const
  ENOENT =
 {$IF DEFINED(MSWINDOWS)}
    ERROR_FILE_NOT_FOUND
 {$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
    ESysENOENT
 {$ENDIF};
{$ENDIF}

{ TBrookStringPair }

constructor TBrookStringPair.Create(const AName, AValue: string);
begin
  FName := AName;
  FValue := AValue;
end;

{ TBrookStringMapEnumerator }

constructor TBrookStringMapEnumerator.Create(AMap: TBrookStringMap);
begin
  inherited Create;
  FMap := AMap;
  FMap.First(FCurr);
  FBOF := True;
end;

function TBrookStringMapEnumerator.GetCurrent: TBrookStringPair;
begin
  Result := FCurr;
end;

function TBrookStringMapEnumerator.MoveNext: Boolean;
begin
  if FBOF then
    FBOF := False
  else
    FMap.Next(FCurr);
  Result := not FMap.EOF;
end;

{ TBrookStringMap }

constructor TBrookStringMap.Create(AHandle: Pointer);
begin
  inherited Create;
  FOwnsHandle := not Assigned(AHandle);
  if not FOwnsHandle then
    Fmap := AHandle;
end;

destructor TBrookStringMap.Destroy;
begin
  try
    if FOwnsHandle then
      Clear;
  finally
    inherited Destroy;
  end;
end;

function TBrookStringMap.GetEnumerator: TBrookStringMapEnumerator;
begin
  Result := TBrookStringMapEnumerator.Create(Self);
end;

class function TBrookStringMap.DoIterate(Acls: Pcvoid; Apair: Pbk_strmap): cint;
var
  M: PMethod absolute Acls;
begin
  if not Assigned(M.Code) then
    Exit(-1);
  Result := TBrookStringMapIterator(M.Code)(M.Data, CreatePair(Apair));
end;

class function TBrookStringMap.DoSort(Acls: Pcvoid; Apair_a: Pbk_strmap;
  Apair_b: Pbk_strmap): cint;
var
  M: PMethod absolute Acls;
begin
  if not Assigned(M.Code) then
    Exit(0);
  Result := TBrookStringMapComparator(M.Code)(M.Data, CreatePair(Apair_a),
    CreatePair(Apair_b));
end;

class function TBrookStringMap.CreatePair(Apair: Pbk_strmap): TBrookStringPair;
var
  N, V: TBytes;
  NL, VL: csize_t;
begin
  SetLength(N, BROOK_STRMAP_MAX_BUF);
  NL := BROOK_STRMAP_MAX_BUF;
  CheckOSError(bk_strmap_readname(Apair, @N[0], @NL));
  SetLength(V, BROOK_STRMAP_MAX_BUF);
  VL := BROOK_STRMAP_MAX_BUF;
  CheckOSError(bk_strmap_readval(Apair, @V[0], @VL));
  Result := TBrookStringPair.Create(TMarshal.ToString(@N[0], NL),
    TMarshal.ToString(@V[0], VL));
end;

function TBrookStringMap.GetCount: Integer;
begin
  if not Assigned(Fmap) then
    Exit(0);
  BkCheckLibrary;
  CheckOSError(bk_strmap_count(Fmap, @Result));
end;

function TBrookStringMap.GetValue(const AName: string): string;
begin
  if not TryValue(AName, Result) then
    Result := '';
end;

procedure TBrookStringMap.SetValue(const AName, AValue: string);
begin
  AddOrSet(AName, AValue);
end;

function TBrookStringMap.GetHandle: Pointer;
begin
  Result := Fmap;
end;

procedure TBrookStringMap.SetHandle(AHandle: Pointer);
begin
  Fmap := AHandle;
end;

function TBrookStringMap.GetOwnsHandle: Boolean;
begin
  Result := FOwnsHandle;
end;

procedure TBrookStringMap.SetOwnsHandle(AValue: Boolean);
begin
  FOwnsHandle := AValue;
end;

function TBrookStringMap.IsEOF: Boolean;
begin
  Result := not Assigned(Fpair);
end;

procedure TBrookStringMap.Add(const AName, AValue: string);
var
  N, V: Pcchar;
  M: TMarshaller;
begin
  BkCheckLibrary;
  N := M.ToCString(AName);
  V := M.ToCString(AValue);
  CheckOSError(bk_strmap_add(@Fmap, N, Length(N), V, Length(V)));
end;

procedure TBrookStringMap.AddOrSet(const AName, AValue: string);
var
  N, V: Pcchar;
  M: TMarshaller;
begin
  BkCheckLibrary;
  N := M.ToCString(AName);
  V := M.ToCString(AValue);
  CheckOSError(bk_strmap_set(@Fmap, N, Length(N), V, Length(V)));
end;

procedure TBrookStringMap.Remove(const AName: string);
var
  R: cint;
  N: Pcchar;
  M: TMarshaller;
begin
  if not Assigned(Fmap) then
    Exit;
  BkCheckLibrary;
  N := M.ToCString(AName);
  R := bk_strmap_rm(@Fmap, N, Length(N));
  if (R <> 0) and (R <> -ENOENT) then
    CheckOSError(R);
end;

procedure TBrookStringMap.Clear;
begin
  if not Assigned(Fmap) then
    Exit;
  BkCheckLibrary;
  bk_strmap_cleanup(@Fmap);
  Fmap := nil;
end;

function TBrookStringMap.Find(const AName: string;
  out APair: TBrookStringPair): Boolean;
var
  R: cint;
  N: Pcchar;
  V: TBytes;
  L: csize_t;
  P: Pbk_strmap;
  M: TMarshaller;
begin
  if not Assigned(Fmap) then
    Exit(False);
  BkCheckLibrary;
  N := M.ToCString(AName);
  R := bk_strmap_find(Fmap, N, Length(N), @P);
  Result := R = 0;
  if Result then
  begin
    SetLength(V, BROOK_STRMAP_MAX_BUF);
    L := BROOK_STRMAP_MAX_BUF;
    CheckOSError(bk_strmap_readval(P, @V[0], @L));
    APair := TBrookStringPair.Create(AName, TMarshal.ToString(@V[0], L));
  end
  else
    if R <> -ENOENT then
      CheckOSError(R);
end;

function TBrookStringMap.TryValue(const AName: string;
  out AValue: string): Boolean;
var
  P: TBrookStringPair;
begin
  Result := Find(AName, P);
  if Result then
    AValue := P.Value;
end;

function TBrookStringMap.First(out APair: TBrookStringPair): Boolean;
begin
  Fpair := Fmap;
  Result := Assigned(Fpair);
  if Result then
  begin
    BkCheckLibrary;
    APair := CreatePair(Fpair);
  end;
end;

function TBrookStringMap.Next(out APair: TBrookStringPair): Boolean;
var
  R: cint;
begin
  if not Assigned(Fpair) then
    Exit(False);
  BkCheckLibrary;
  R := bk_strmap_next(@Fpair);
  CheckOSError(R);
  Result := R = 0;
  if Result and Assigned(Fpair) then
    APair := CreatePair(Fpair);
end;

function TBrookStringMap.Iterate(AIterator: TBrookStringMapIterator;
  AData: Pointer): Boolean;
var
  R: cint;
  M: TMethod;
begin
  if not Assigned(Fmap) then
    Exit;
  BkCheckLibrary;
  M.Code := @AIterator;
  M.Data := AData;
  R := bk_strmap_iter(Fmap, {$IFNDEF VER3_0}@{$ENDIF}DoIterate, @M);
  if R <> -1 then
    CheckOSError(R);
  Result := R = 0;
end;

procedure TBrookStringMap.Sort(AComparator: TBrookStringMapComparator;
  AData: Pointer);
var
  M: TMethod;
begin
  if not Assigned(Fmap) then
    Exit;
  BkCheckLibrary;
  M.Code := @AComparator;
  M.Data := AData;
  CheckOSError(bk_strmap_sort(@Fmap, {$IFNDEF VER3_0}@{$ENDIF}DoSort, @M));
end;

end.
