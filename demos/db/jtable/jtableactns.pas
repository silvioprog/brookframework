unit jTableActns;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, dOpf, dSQLdbBroker, dbutils, Classes, FPJSON;

type

  { TjTableGOpf }

  generic TjTableGOpf<T> = class(specialize TdGOpf<TdSQLdbConnector, TdSQLdbQuery, T>)
  public
    constructor Create; overload; virtual;
    procedure BuildSelect(AEntities: TEntities; AParamsStr: TStrings;
      AParams: TObject);
  end;

  { TjTableGAction }

  generic TjTableGAction<T1, T2> = class(specialize TBrookGAction<T2>)
  private
    FOpf: T1;
    FData: TJSONObject;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure Post; override;
    procedure WriteData; virtual;
    property Data: TJSONObject read FData;
    property Opf: T1 read FOpf;
  end;

  { TjTableGListAction }

  generic TjTableGListAction<T1, T2> = class(specialize TjTableGAction<T1, T2>)
  private
    FjtPageSize: Integer;
    FjtSorting: string;
    FjtStartIndex: Integer;
  public
    procedure WriteData; override;
  published
    property jtSorting: string read FjtSorting write FjtSorting;
    property jtPageSize: Integer read FjtPageSize write FjtPageSize;
    property jtStartIndex: Integer read FjtStartIndex write FjtStartIndex;
  end;

  { TjTableGCreateAction }

  generic TjTableGCreateAction<T1, T2> = class(specialize TjTableGAction<T1, T2>)
  public
    procedure WriteData; override;
  end;

  { TjTableGUpdateAction }

  generic TjTableGUpdateAction<T1, T2> = class(specialize TjTableGAction<T1, T2>)
  public
    procedure WriteData; override;
  end;

  { TjTableGDeleteAction }

  generic TjTableGDeleteAction<T1, T2> = class(specialize TjTableGAction<T1, T2>)
  public
    procedure WriteData; override;
  end;

implementation

{ TjTableGOpf }

constructor TjTableGOpf.Create;
begin
  inherited Create(dbutils.con, '');
end;

procedure TjTableGOpf.BuildSelect(AEntities: TEntities; AParamsStr: TStrings;
  AParams: TObject);
var
  VSql: string;
  VFields: string = '';
  VjtSortingIdx, VjtStartIndexIdx, VjtPageSizeIdx: Integer;
begin
  VjtSortingIdx := AParamsStr.IndexOfName('jtSorting');
  VjtPageSizeIdx := AParamsStr.IndexOfName('jtPageSize');
  VjtStartIndexIdx := AParamsStr.IndexOfName('jtStartIndex');
  GetFieldNames(VFields);
  VSql := 'select ' + VFields + ' from ' + Table.Name;
  if VjtSortingIdx > -1 then
    VSql += ' order by ' + AParamsStr.ValueFromIndex[VjtSortingIdx];
  if (VjtPageSizeIdx > -1) and (VjtStartIndexIdx > -1) then
  begin
    VSql += ' limit :jtPageSize offset :jtStartIndex';
    List(AEntities, VSql, AParams)
  end
  else
    List(AEntities);
end;

{ TjTableGAction }

constructor TjTableGAction.Create;
begin
  inherited Create;
  FData := TJSONObject.Create;
  FOpf := T1.Create;
end;

destructor TjTableGAction.Destroy;
begin
  FData.Free;
  FOpf.Free;
  inherited Destroy;
end;

procedure TjTableGAction.Post;
begin
  WriteData;
end;

procedure TjTableGAction.WriteData;
begin
  Data.Add('Result', 'OK');
  Write(FData.AsJSON);
end;

{ TjTableGListAction }

{$NOTES OFF}
procedure TjTableGListAction.WriteData;
var
  I: Integer;
  VEntity: TObject;
  VEntities: TObject;
  VArray: TJSONArray;
  VObject: TJSONObject;
begin
  VEntities := T1.TEntities.Create;
  try
    GetParams(Self);
    Opf.BuildSelect(T1.TEntities(VEntities), Params, Self);
    VArray := TJSONArray.Create;
    for I := 0 to Pred(T1.TEntities(VEntities).Count) do
    begin
      VObject := TJSONObject.Create;
      VEntity := T1.TEntities(VEntities).Items[I];
      dbutils.objToJSON(Opf.Table.PropList, Opf.Table.PropCount, VEntity,
        VObject);
      VArray.Add(VObject);
    end;
    Data.Add('TotalRecordCount', pgCount(Opf.Table.Name));
    Data.Add('Records', VArray);
    inherited WriteData;
  finally
    VEntities.Free;
  end;
end;
{$NOTES ON}

{ TjTableGCreateAction }

procedure TjTableGCreateAction.WriteData;
var
  VObject: TJSONObject;
begin
  VObject := TJSONObject.Create;
  if Entity.Id < 1 then
    Entity.Id := pgNextSeq(Opf.Table.Name + '_id_seq');
  Opf.Add(Entity, False);
  Opf.Apply;
  dbutils.objToJSON(Opf.Table.PropList, Opf.Table.PropCount, Entity, VObject);
  Data.Add('Record', VObject);
  inherited WriteData;
end;

{ TjTableGUpdateAction }

procedure TjTableGUpdateAction.WriteData;
var
  VObject: TJSONObject;
begin
  VObject := TJSONObject.Create;
  Opf.Modify(Entity, False);
  Opf.Apply;
  dbutils.objToJSON(Opf.Table.PropList, Opf.Table.PropCount, Entity, VObject);
  Data.Add('Record', VObject);
  inherited WriteData;
end;

{ TjTableGDeleteAction }

procedure TjTableGDeleteAction.WriteData;
begin
  Opf.Remove(Entity);
  Opf.Apply;
  inherited WriteData;
end;

end.

