unit personactns;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, dOpf, dSQLdbBroker, fpjson, dbutils, personobjs;

type

  { TPersonOpf }

  TPersonOpf = specialize TdGSQLdbOpf<TPerson>;

  { TPersonOpfAction }

  TPersonOpfAction = class(specialize TBrookGAction<TPerson>)
  private
    FData: TJSONObject;
    FOpf: TPersonOpf;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    property Data: TJSONObject read FData;
    property Opf: TPersonOpf read FOpf;
  end;

  { TPersonCreateAction }

  TPersonCreateAction = class(TPersonOpfAction)
  public
    procedure Post; override;
  end;

  { TPersonUpdateAction }

  TPersonUpdateAction = class(TPersonOpfAction)
  public
    procedure Post; override;
  end;

  { TPersonDeleteAction }

  TPersonDeleteAction = class(TPersonOpfAction)
  public
    procedure Post; override;
  end;

  { TPersonListAction }

  TPersonListAction = class(TPersonOpfAction)
  private
    FjtPageSize: Integer;
    FjtSorting: string;
    FjtStartIndex: Integer;
  public
    procedure Post; override;
  published
    property jtSorting: string read FjtSorting write FjtSorting;
    property jtPageSize: Integer read FjtPageSize write FjtPageSize;
    property jtStartIndex: Integer read FjtStartIndex write FjtStartIndex;
  end;

implementation

{ TPersonAction }

constructor TPersonOpfAction.Create;
begin
  inherited Create;
  FOpf := TPersonOpf.Create(dbutils.con, 'person');
  FData := TJSONObject.Create;
end;

destructor TPersonOpfAction.Destroy;
begin
  FData.Free;
  FOpf.Free;
  inherited Destroy;
end;

{ TPersonCreateAction }

procedure TPersonCreateAction.Post;
var
  VObject: TJSONObject;
begin
  Entity.Validate;
  VObject := TJSONObject.Create;
  Entity.Id := pgNextSeq(FOpf.Table.Name + '_id_seq');
  FOpf.Add(Entity);
  FOpf.Apply;
  dbutils.objToJSON(FOpf.Table.PropList, FOpf.Table.PropCount, Entity, VObject);
  FData.Add('Result', 'OK');
  FData.Add('Record', VObject);
  Write(FData.AsJSON);
end;

{ TPersonUpdateAction }

procedure TPersonUpdateAction.Post;
var
  VObject: TJSONObject;
begin
  Entity.Validate;
  VObject := TJSONObject.Create;
  Opf.Modify(Entity);
  Opf.Apply;
  dbutils.objToJSON(FOpf.Table.PropList, FOpf.Table.PropCount, Entity, VObject);
  FData.Add('Result', 'OK');
  FData.Add('Record', VObject);
  Write(FData.AsJSON);
end;

{ TPersonDeleteAction }

procedure TPersonDeleteAction.Post;
begin
  Opf.Remove(Entity);
  Opf.Apply;
  FData.Add('Result', 'OK');
  Write(FData.AsJSON);
end;

{ TPersonListAction }

procedure TPersonListAction.Post;
var
  VSql: string;
  VEntity: TPerson;
  VArray: TJSONArray;
  VFields: string = '';
  VObject: TJSONObject;
  VEntities: TPersonOpf.TEntities;
  VjtSortingIdx, VjtPageSizeIdx, VjtStartIndexIdx: Integer;
begin
  VArray := TJSONArray.Create;
  VEntities := TPersonOpf.TEntities.Create;
  try
    GetParams(Self);
    VjtSortingIdx := Params.IndexOfName('jtSorting');
    VjtPageSizeIdx := Params.IndexOfName('jtPageSize');
    VjtStartIndexIdx := Params.IndexOfName('jtStartIndex');
    FOpf.GetFieldNames(VFields);
    VSql := 'select ' + VFields + ' from ' + FOpf.Table.Name;
    if VjtSortingIdx > -1 then
      VSql += ' order by ' + Params.ValueFromIndex[VjtSortingIdx];
    if (VjtPageSizeIdx > -1) and (VjtStartIndexIdx > -1) then
    begin
      VSql += ' limit :jtPageSize offset :jtStartIndex';
      FOpf.List(VEntities, VSql, Self)
    end
    else
      FOpf.List(VEntities);
    for VEntity in VEntities do
    begin
      VObject := TJSONObject.Create;
      dbutils.objToJSON(FOpf.Table.PropList, FOpf.Table.PropCount, VEntity,
        VObject);
      VArray.Add(VObject);
    end;
    FData.Add('TotalRecordCount', pgCount(FOpf.Table.Name));
    FData.Add('Result', 'OK');
    FData.Add('Records', VArray);
    Write(FData.AsJSON);
  finally
    VEntities.Free;
  end;
end;

initialization
  TPersonCreateAction.Register('/personcreate');
  TPersonUpdateAction.Register('/personupdate');
  TPersonDeleteAction.Register('/persondelete');
  TPersonListAction.Register('/personlist');

end.
