unit BrookStringMap;

{$I Brook.inc}

interface

uses
{$IFDEF FPC}
  BaseUnix,
{$ENDIF}
  SysUtils,
  libbrook,
  Marshalling,
  BrookHandledClasses;

const
  BROOK_STRMAP_MAX_VAL = 4096; // 4 kB

type
  TBrookStringMap = class(TBrookHandledPersistent)
  private
    Fmap: Pbk_strmap;
    FOwnsHandle: Boolean;
  protected
    function GetHandle: Pointer; override;
    function GetOwnsHandle: Boolean; override;
    procedure SetOwnsHandle(AValue: Boolean); override;
  public
    constructor Create(AHandle: Pointer); virtual;
    destructor Destroy; override;
    procedure Add(const AName, AValue: string); virtual;
    function Find(const AName: string; out AValue: string): Boolean; virtual;
  end;

implementation

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
  N := M.ToCString(AName);
  V := M.ToCString(AValue);
  BkCheckLibrary;
  CheckOSError(bk_strmap_add(@Fmap, N, Length(N), V, Length(V)));
end;

function TBrookStringMap.Find(const AName: string; out AValue: string): Boolean;
var
  R: cint;
  N: Pcchar;
  VL: csize_t;
  M: TMarshaller;
  V: array[0..BROOK_STRMAP_MAX_VAL - SizeOf(Byte)] of Byte;
begin
  VL := BROOK_STRMAP_MAX_VAL;
  N := M.ToCString(AName);
  BkCheckLibrary;
  R := bk_strmap_find(Fmap, N, Length(N), @V[0], @VL);
  Result := R = 0;
  if Result then
    AValue := TMarshal.ToString(@V[0], VL)
  else
    if R <> -ESysENOENT then
      CheckOSError(R);
end;

end.
