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

{ String map used to represent HTML fields, query-string parameters and more. }

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

type
  TBrookStringMap = class;

  { Identifies the kind of operation in the map.

    @value(bkmoNone None operation or map cleaned.)
    @value(bkmoAdd Pair added to the map.)
    @value(bkmoAddOrSet Pair added or set to the map.)
    @value(bkmoRemove Pair removed from the map.) }
  TBrookStringMapOperation = (bkmoNone, bkmoAdd, bkmoAddOrSet, bkmoRemove);

  { Event to notify a change in the map.

    @param(ASender Event caller.)
    @param(AOperation Operation kind.) }
  TBrookStringMapChangeEvent = procedure(ASender: TObject;
    AOperation: TBrookStringMapOperation) of object;

  { Pair item of @link(TBrookStringMap). }
  TBrookStringPair = record
  private
    FName: string;
    FValue: string;
  public
    { Initializes a variable of @link(TBrookStringPair).

      @param(AName[in] Name of the pair.)
      @param(AValue[in] Value of the pair.)}
    constructor Create(const AName, AValue: string);
    { Name of the pair. }
    property Name: string read FName;
    { Value of the pair. }
    property Value: string read FValue;
  end;

  { Enumerator used to iterate the map @link(TBrookStringMap). }
  TBrookStringMapEnumerator = class
  private
    FMap: TBrookStringMap;
    FCurr: TBrookStringPair;
    FBOF: Boolean;
  public
    { Creates an instance of @link(TBrookStringMapEnumerator).

      @param(AMap[in] Pairs map.) }
    constructor Create(AMap: TBrookStringMap);
    { Gets the current pair.

      @return(Current pair.)  }
    function GetCurrent: TBrookStringPair;
    { Moves to the next pair.

      @return(@True when move next reachs the EOF.) }
    function MoveNext: Boolean;
    { Same to @link(GetCurrent). }
    property Current: TBrookStringPair read GetCurrent;
  end;

  { Function signature used by @link(TBrookStringMap.Iterate).

    @param(AData[in,out] User-defined data.)
    @param(APair[out] Current iterated pair.)}
  TBrookStringMapIterator = function(AData: Pointer;
    APair: TBrookStringPair): Integer;

  { Function signature used by @link(TBrookStringMap.Sort).

    @param(AData[in,out] User-defined data.)
    @param(APairA[out] Current left pair (A).)
    @param(APairB[out] Current right pair (B).)}
  TBrookStringMapComparator = function(AData: Pointer;
    APairA, APairB: TBrookStringPair): Integer;

  { String map class and its related methods. }
  TBrookStringMap = class(TBrookHandledPersistent)
  private
    Fnext: Pbk_strmap;
    Fmap: Pbk_strmap;
    FOnChange: TBrookStringMapChangeEvent;
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
    procedure DoChange(AOperation: TBrookStringMapOperation); virtual;
  public
    { Creates an instance of @link(TBrookStringMap).

      @param(AHandle[in] String map handle.)}
    constructor Create(AHandle: Pointer); virtual;
    { Frees an instance of @link(TBrookStringMap). }
    destructor Destroy; override;
    { Gets an instance of @link(TBrookStringMapEnumerator). }
    function GetEnumerator: TBrookStringMapEnumerator;
    { Adds a pair of strings to the map.

      @param(AName[in] Name of the pair.)
      @param(AValue[in] Value of the pair.) }
    procedure Add(const AName, AValue: string); virtual;
    { Adds or sets a pair of strings to the map.

      @param(AName[in] Name of the pair.)
      @param(AValue[in] Value of the pair.) }
    procedure AddOrSet(const AName, AValue: string); virtual;
    { Removes a pair by its name.

      @param(AName[in] Name of the pair.) }
    procedure Remove(const AName: string); virtual;
    { Cleans the entire map. }
    procedure Clear; virtual;
    { Finds a pair by its name.

      @param(AName[in] Name of the pair.)
      @param(APair[out] Reference to store found pair.) }
    function Find(const AName: string;
      out APair: TBrookStringPair): Boolean; virtual;
    { Tries to find a mapped value by its name.

      @param(AName[in] Name of the pair.)
      @param(AValue[out] Reference to store found value.) }
    function TryValue(const AName: string;
      out AValue: string): Boolean; virtual;
    { Retrieves the first pair in the map.

      @param(APair[out] First pair returned.) }
    function First(out APair: TBrookStringPair): Boolean; virtual;
    { Retrieves the next pair in the map.

      @param(APair[out] Next pair returned.) }
    function Next(out APair: TBrookStringPair): Boolean; virtual;
    { Iterates over pairs map.

      @param(AIterator[in] Function to iterate the pairs.)
      @param(AData[in,out] User-specified value.) }
    procedure Iterate(AIterator: TBrookStringMapIterator;
      AData: Pointer); virtual;
    { Sorts the pairs map.

      @param(AComparator[in] Function to sort the pairs.)
      @param(AData[in,out] User-specified value.) }
    procedure Sort(AComparator: TBrookStringMapComparator;
      AData: Pointer); virtual;
    { Counts the total pairs present in the map. }
    property Count: Integer read GetCount;
    { Adds or gets the pair value. }
    property Values[const AName: string]: string read GetValue
      write SetValue; default;
    { Indicates the end of map. }
    property EOF: Boolean read IsEOF;
  published
    { Notifies a change in the map. }
    property OnChange: TBrookStringMapChangeEvent read FOnChange write FOnChange;
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
begin
  BkCheckLibrary;
  Result := TBrookStringPair.Create(TMarshal.ToString(bk_strmap_name(Apair)),
    TMarshal.ToString(bk_strmap_val(Apair)));
end;

function TBrookStringMap.GetCount: Integer;
begin
  if not Assigned(Fmap) then
    Exit(0);
  BkCheckLibrary;
  Result := bk_strmap_count(Fmap);
  if Result < 0 then
    CheckOSError(Result);
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
  if AHandle = Fmap then
    Exit;
  Clear;
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
  Result := not Assigned(Fnext);
end;

procedure TBrookStringMap.DoChange(AOperation: TBrookStringMapOperation);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AOperation);
end;

procedure TBrookStringMap.Add(const AName, AValue: string);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  CheckOSError(bk_strmap_add(@Fmap, M.ToCString(AName), M.ToCString(AValue)));
  DoChange(bkmoAdd);
end;

procedure TBrookStringMap.AddOrSet(const AName, AValue: string);
var
  M: TMarshaller;
begin
  BkCheckLibrary;
  CheckOSError(bk_strmap_set(@Fmap, M.ToCString(AName), M.ToCString(AValue)));
  DoChange(bkmoAddOrSet);
end;

procedure TBrookStringMap.Remove(const AName: string);
var
  R: cint;
  M: TMarshaller;
begin
  if not Assigned(Fmap) then
    Exit;
  BkCheckLibrary;
  R := bk_strmap_rm(@Fmap, M.ToCString(AName));
  if (R <> 0) and (R <> -ENOENT) then
    CheckOSError(R);
  DoChange(bkmoRemove);
end;

procedure TBrookStringMap.Clear;
begin
  if not Assigned(Fmap) then
    Exit;
  BkCheckLibrary;
  bk_strmap_cleanup(@Fmap);
  DoChange(bkmoNone);
end;

function TBrookStringMap.Find(const AName: string;
  out APair: TBrookStringPair): Boolean;
var
  R: cint;
  P: Pbk_strmap;
  M: TMarshaller;
begin
  if not Assigned(Fmap) then
    Exit(False);
  BkCheckLibrary;
  R := bk_strmap_find(Fmap, M.ToCString(AName), @P);
  Result := R = 0;
  if Result then
    APair := TBrookStringPair.Create(AName, TMarshal.ToString(bk_strmap_val(P)))
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
  Fnext := Fmap;
  Result := Assigned(Fnext);
  if Result then
  begin
    BkCheckLibrary;
    APair := CreatePair(Fnext);
  end;
end;

function TBrookStringMap.Next(out APair: TBrookStringPair): Boolean;
var
  R: cint;
begin
  if not Assigned(@Fnext) then
    Exit(False);
  BkCheckLibrary;
  R := bk_strmap_next(@Fnext);
  CheckOSError(R);
  Result := R = 0;
  if Result and Assigned(Fnext) then
    APair := CreatePair(Fnext);
end;

procedure TBrookStringMap.Iterate(AIterator: TBrookStringMapIterator;
  AData: Pointer);
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