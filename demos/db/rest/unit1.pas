unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, person, Classes;

type

  { TPersonAction }

  TPersonAction = class(specialize TBrookEntityAction<TPerson>)
  private
    FStorage: TPersonStorage;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure Get; override;
    procedure Post; override;
    procedure Put; override;
    procedure Delete; override;
    property Storage: TPersonStorage read FStorage;
  end;

implementation

{ TPersonAction }

constructor TPersonAction.Create;
begin
  inherited Create;
  FStorage := TPersonStorage.Create;
  FStorage.TableName := 'person';
end;

destructor TPersonAction.Destroy;
begin
  FStorage.Free;
  inherited Destroy;
end;

procedure TPersonAction.Get;
var
  I: Pointer;
  VPerson: TPerson absolute I;
  VPersons: TFPList;
begin
  VPersons := TFPList.Create;
  try
    FStorage.List(VPersons, TPerson);
    for I in VPersons do
      Write('Id: %d, Name: %s<br />', [VPerson.Id, VPerson.Name]);
  finally
    VPersons.Free;
  end;
end;

procedure TPersonAction.Post;
begin
  Entity.Validate;
  FStorage.Add(Entity).Store;
end;

procedure TPersonAction.Put;
begin
  Entity.Validate;
  FStorage.Modify(Entity).Store;
end;

procedure TPersonAction.Delete;
begin
  FStorage.Delete(Entity).Store;
end;

initialization
  TPersonAction.Register('*');

end.
