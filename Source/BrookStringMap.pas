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
  BROOK_STRMAP_MAX_VAL: NativeUInt = 4096; // 4 kB

type
  TBrookStringPair = record
  private
    FName: string;
    FValue: string;
  public
    constructor Create(const AName, AValue: string);
    property Name: string read FName;
    property Value: string read FValue;
  end;

  TBrookStringMap = class(TBrookHandledPersistent)
  private
    Fmap: Pbk_strmap;
    FOwnsHandle: Boolean;
  protected
    function GetHandle: Pointer; override;
    procedure SetHandle(AHandle: Pointer); override;
    function GetOwnsHandle: Boolean; override;
    procedure SetOwnsHandle(AValue: Boolean); override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    procedure Add(const AName, AValue: string); virtual;
    function Find(const AName: string;
      out APair: TBrookStringPair): Boolean; virtual;
    function TryValue(const AName: string;
      out AValue: string): Boolean; virtual;
  end;

implementation

{ TBrookStringPair }

constructor TBrookStringPair.Create(const AName, AValue: string);
begin
  FName := AName;
  FValue := AValue;
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
  if FOwnsHandle then
  begin
    BkCheckLibrary;
    bk_strmap_cleanup(@Fmap);
    Fmap := nil;
  end;
  inherited Destroy;
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

function TBrookStringMap.Find(const AName: string;
  out APair: TBrookStringPair): Boolean;
{$IFNDEF POSIX}
const
  ENOENT =
 {$IF DEFINED(MSWINDOWS)}
    ERROR_FILE_NOT_FOUND
 {$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
    ESysENOENT
 {$ENDIF};
{$ENDIF}
var
  R: cint;
  N: Pcchar;
  V: TBytes;
  L: csize_t;
  P: Pbk_strmap;
  M: TMarshaller;
begin
  BkCheckLibrary;
  if not Assigned(Fmap) then // avoids -EINVAL
    Exit(False);
  N := M.ToCString(AName);
  R := bk_strmap_find(Fmap, N, Length(N), @P);
  Result := R = 0;
  if Result then
  begin
    SetLength(V, BROOK_STRMAP_MAX_VAL);
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

end.
