unit objectstorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, sqldb, db, typinfo;

type
  EObjectStorage = class(Exception);

  { TObjectStorage }

  TObjectStorage = class(TObject)
  private
    FConnection: TSQLConnection;
    FTableName: ShortString;
    FTransaction: TSQLTransaction;
  protected
    function CreateQuery: TSQLQuery;
    function BuildUpdateSQL(AObject: TObject;
      const ASQLType: TUpdateKind): string; virtual;
    procedure BindParams(AObject: TObject; AParams: TParams); virtual;
  public
    constructor Create(AConnection: TSQLConnection); virtual;
    destructor Destroy; override;
    function Save: TObjectStorage;
    function Cancel: TObjectStorage;
    procedure Execute(AObject: TObject; const ASQLType: TUpdateKind);
    function Add(AObject: TObject): TObjectStorage;
    function Modify(AObject: TObject): TObjectStorage;
    function Delete(AObject: TObject): TObjectStorage;
    function List(AObjects: TFPList; AObjectClass: TClass;
      const ASQL: string = ''): TObjectStorage;
    property Connection: TSQLConnection read FConnection write FConnection;
    property TableName: ShortString read FTableName write FTableName;
  end;

implementation

{ TObjectStorage }

constructor TObjectStorage.Create(AConnection: TSQLConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
end;

destructor TObjectStorage.Destroy;
begin
  FTransaction.Free;
  inherited Destroy;
end;

function TObjectStorage.CreateQuery: TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.DataBase := FConnection;
  Result.Transaction := FTransaction;
end;

function TObjectStorage.BuildUpdateSQL(AObject: TObject;
  const ASQLType: TUpdateKind): string;
var
  N: string;
  C, I: Integer;
  FS: string = '';
  PS: string = '';
  PL: PPropList = nil;
begin
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      strlower(@FTableName);
      case ASQLType of
        ukInsert:
          begin
            for I := 0 to Pred(C) do
            begin
              N := PL^[I]^.Name;
              if ShortCompareText(N, 'id') = 0 then
                Continue;
              N += ', ';
              FS += N;
              PS += ':' + N;
            end;
            SetLength(FS, Length(FS) - 2);
            SetLength(PS, Length(PS) - 2);
            strlower(PChar(FS));
            strlower(PChar(PS));
            Result := 'insert into ' + FTableName + ' (' + FS + ') ' +
              'values (' + PS + ')';
          end;
        ukModify:
          begin
            PS := '';
            for I := 0 to Pred(C) do
            begin
              N := PL^[I]^.Name;
              if ShortCompareText(N, 'id') = 0 then
                Continue;
              PS += N + ' = :' + N + ' and';
            end;
            SetLength(PS, Length(PS) - 4);
            strlower(PChar(PS));
            Result := 'update ' + FTableName + ' set ' + PS + ' where id = :id';
          end;
        ukDelete: Result := 'delete from ' + FTableName + ' where id = :id';
      end;
    finally
      FreeMem(PL);
    end
  else
    Result := '';
end;

procedure TObjectStorage.BindParams(AObject: TObject; AParams: TParams);
var
  C, I: Integer;
  P: TParam;
  PI: PPropInfo;
  PL: PPropList = nil;
begin
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      for I := 0 to Pred(C) do
      begin
        PI := PL^[I];
        P := AParams.FindParam(PI^.Name);
        if not Assigned(P) then
          Continue;
        case PI^.PropType^.Kind of
          tkAString: P.AsString := GetStrProp(AObject, PI);
          tkChar: PChar(P.AsString)^ := Char(GetOrdProp(AObject, PI));
          tkInteger: P.AsInteger := GetOrdProp(AObject, PI);
          tkInt64, tkQWord: P.AsLargeInt := GetInt64Prop(AObject, PI);
          tkBool: P.AsBoolean := GetOrdProp(AObject, PI) <> 0;
          tkFloat: P.AsFloat := GetFloatProp(AObject, PI);
          tkEnumeration: P.AsString := GetEnumProp(AObject, PI);
          tkSet: P.AsString := GetSetProp(AObject, PI, False);
        end;
      end;
    finally
      FreeMem(PL);
    end;
end;

function TObjectStorage.Save: TObjectStorage;
begin
  Result := Self;
  try
    FTransaction.Commit;
  except
    FTransaction.Rollback;
    raise;
  end;
end;

function TObjectStorage.Cancel: TObjectStorage;
begin
  Result := Self;
  FTransaction.Rollback;
end;

procedure TObjectStorage.Execute(AObject: TObject; const ASQLType: TUpdateKind);
var
  Q: TSQLQuery;
begin
  if not Assigned(AObject) then
    raise EObjectStorage.Create('"AObject" must not be nil.');
  if AObject.ClassType = TObject then
    Exit;
  Q := CreateQuery;
  try
    Q.SQL.Text := BuildUpdateSQL(AObject, ASQLType);
    BindParams(AObject, Q.Params);
    { TODO: Log SQL }
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

function TObjectStorage.Add(AObject: TObject): TObjectStorage;
begin
  Result := Self;
  Execute(AObject, ukInsert);
end;

function TObjectStorage.Modify(AObject: TObject): TObjectStorage;
begin
  Result := Self;
  Execute(AObject, ukModify);
end;

function TObjectStorage.Delete(AObject: TObject): TObjectStorage;
begin
  Result := Self;
  Execute(AObject, ukDelete);
end;

function TObjectStorage.List(AObjects: TFPList; AObjectClass: TClass;
  const ASQL: string): TObjectStorage;
var
  I: Integer;
  F: TField;
  O: TObject;
  Q: TSQLQuery;
  PI: PPropInfo;
begin
  Result := Self;
  if not Assigned(AObjects) then
    raise EObjectStorage.Create('"AObjects" must not be nil.');
  if not Assigned(AObjectClass) then
    raise EObjectStorage.Create('"AObjectClass" must not be nil.');
  Q := CreateQuery;
  try
    if Trim(ASQL) = '' then
      Q.SQL.Text := 'select * from ' + FTableName + ' order by id';
    Q.Open;
    while not Q.EOF do
    begin
      O := AObjectClass.Create;
      for I := 0 to Pred(Q.Fields.Count) do
      begin
        F := Q.Fields[I];
        PI := GetPropInfo(PTypeInfo(O.ClassInfo), F.FieldName);
        if not Assigned(PI) then
          Continue;
        case F.DataType of
          ftFixedWideChar, ftWideString, ftFixedChar,
            ftString: SetStrProp(O, PI, F.AsString);
          ftSmallInt, ftInteger, ftAutoInc,
            ftWord: SetOrdProp(O, PI, F.AsInteger);
          ftLargeInt: SetInt64Prop(O, PI, F.AsLargeInt);
          ftFloat: SetFloatProp(O, PI, F.AsFloat);
          ftBoolean: SetOrdProp(O, PI, Ord(F.AsBoolean));
          ftDate, ftTime, ftDateTime: SetFloatProp(O, PI, F.AsDateTime);
        end;
      end;
      AObjects.Add(O);
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

end.

