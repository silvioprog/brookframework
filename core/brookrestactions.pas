(*
  Brook REST Actions unit.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/brookframework

  All contributors:
  Plase see the file CONTRIBUTORS.txt, included in this
  distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit BrookRESTActions;

{$i brook.inc}

interface

uses
  BrookDBAction, BrookUtils, BrookHTTPConsts, HTTPDefs, FPJSON, SysUtils;

type

  { Is a metaclass for @link(TBrookCustomRESTAction) class. }
  TBrookCustomRESTActionClass = class of TBrookCustomRESTAction;

    { Is a metaclass for @link(TBrookCustomCRUDAction) class. }
  TBrookCustomCRUDActionClass = class of TBrookCustomCRUDAction;

  { Offers abstract methods for REST actions. }
  TBrookCustomRESTAction = class(TBrookDBAction)
  public
    { Is triggered by the inherited class. }
    function Execute: Boolean; virtual; abstract;
  end;

  { Offers abstract methods for CRUD actions. }
  TBrookCustomCRUDAction = class(TBrookCustomRESTAction)
  private
    FAutoApply: Boolean;
    FAfterOpen: TBrookDBActionNotifyEvent;
    FAfterInsert: TBrookDBActionNotifyEvent;
    FAfterEdit: TBrookDBActionNotifyEvent;
    FAfterDelete: TBrookDBActionNotifyEvent;
    FAfterApply: TBrookDBActionNotifyEvent;
    FBeforeOpen: TBrookDBActionNotifyEvent;
    FBeforeInsert: TBrookDBActionNotifyEvent;
    FBeforeApply: TBrookDBActionNotifyEvent;
    FBeforeEdit: TBrookDBActionNotifyEvent;
    FBeforeDelete: TBrookDBActionNotifyEvent;
  protected
    procedure InternalOpen; virtual; abstract;
    procedure InternalInsert; virtual; abstract;
    procedure InternalEdit; virtual; abstract;
    procedure InternalDelete; virtual; abstract;
    procedure InternalApply(const AUpdating: Boolean); virtual; abstract;
  public
    { Creates an instance of @code(TBrookCustomCRUDAction). }
    constructor Create; override;
    { Indicates whether to apply the Dataset changes automatically. }
    property AutoApply: Boolean read FAutoApply write FAutoApply;
    { Is triggered after open dataset. }
    property AfterOpen: TBrookDBActionNotifyEvent read FAfterOpen
      write FAfterOpen;
    { Is triggered after insert a record. }
    property AfterInsert: TBrookDBActionNotifyEvent read FAfterInsert
      write FAfterInsert;
    { Is triggered after edit a record. }
    property AfterEdit: TBrookDBActionNotifyEvent read FAfterEdit
      write FAfterEdit;
    { Is triggered after delete a record. }
    property AfterDelete: TBrookDBActionNotifyEvent read FAfterDelete
      write FAfterDelete;
    { Is triggered after apply changes. }
    property AfterApply: TBrookDBActionNotifyEvent read FAfterApply
      write FAfterApply;
    { Is triggered before open dataset. }
    property BeforeOpen: TBrookDBActionNotifyEvent read FBeforeOpen
      write FBeforeOpen;
    { Is triggered before insert a record. }
    property BeforeInsert: TBrookDBActionNotifyEvent read FBeforeInsert
      write FBeforeInsert;
    { Is triggered before edit a record. }
    property BeforeEdit: TBrookDBActionNotifyEvent read FBeforeEdit
      write FBeforeEdit;
    { Is triggered before delete a record. }
    property BeforeDelete: TBrookDBActionNotifyEvent read FBeforeDelete
      write FBeforeDelete;
    { Is triggered before apply updates. }
    property BeforeApply: TBrookDBActionNotifyEvent read FBeforeApply
      write FBeforeApply;
  end;

  { Displays the schema of the resource. }
  TBrookOptionsAction = class(TBrookCustomCRUDAction)
  protected
    procedure InternalOpen; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalApply({%H-}const AUpdating: Boolean); override;
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmOptions;
      const ADefault: Boolean = False); overload;
    { Registers an action linking the request to a database table and defining
      the fields that will be ignored for persistance purposes. }
    class procedure Register(const ATableName, APattern, AIgnoredFields: string;
      const AMethod: TBrookRequestMethod = rmOptions;
      const ADefault: Boolean = False); overload;
    { Is triggered by a OPTIONS HTTP request method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    (* Executes the action. If there are schema in the resource, they are
      returned, if not, it returns a @code('{ "error": "No schema." }') JSON. *)
    function Execute: Boolean; override;
  end;

  { Displays all the contents of the resource. }
  TBrookRetrieveAction = class(TBrookCustomCRUDAction)
  protected
    procedure InternalOpen; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalApply({%H-}const AUpdating: Boolean); override;
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmGet;
      const ADefault: Boolean = False); overload;
    { Registers an action linking the request to a database table and defining
      the fields that will be ignored for persistance purposes. }
    class procedure Register(const ATableName, APattern, AIgnoredFields: string;
      const AMethod: TBrookRequestMethod = rmGet;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If there are contents in the resource, they are
      returned, if not, it returns a 404 status code. }
    function Execute: Boolean; override;
  end;

  { Displays the content of a specific resource. }
  TBrookShowAction = class(TBrookCustomCRUDAction)
  protected
    procedure InternalOpen; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalApply({%H-}const AUpdating: Boolean); override;
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmGet;
      const ADefault: Boolean = False); overload;
    { Registers an action linking the request to a database table and defining
      the fields that will be ignored for persistance purposes. }
    class procedure Register(const ATableName, APattern, AIgnoredFields: string;
      const AMethod: TBrookRequestMethod = rmGet;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If there are contents in the resource, they are
      returned, if not, it returns a 404 status code. }
    function Execute: Boolean; override;
  end;

  { Creates a new resource. }
  TBrookCreateAction = class(TBrookCustomCRUDAction)
  protected
    procedure InternalOpen; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalApply({%H-}const AUpdating: Boolean); override;
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmPost;
      const ADefault: Boolean = False); overload;
    { Registers an action linking the request to a database table and defining
      the fields that will be ignored for persistance purposes. }
    class procedure Register(const ATableName, APattern, AIgnoredFields: string;
      const AMethod: TBrookRequestMethod = rmPost;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. The 201 status code is always returned. }
    function Execute: Boolean; override;
  end;

  { Updates a specific resource. }
  TBrookUpdateAction = class(TBrookCustomCRUDAction)
  protected
    procedure InternalOpen; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalApply({%H-}const AUpdating: Boolean); override;
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmPut;
      const ADefault: Boolean = False); overload;
    { Registers an action linking the request to a database table and defining
      the fields that will be ignored for persistance purposes. }
    class procedure Register(const ATableName, APattern, AIgnoredFields: string;
      const AMethod: TBrookRequestMethod = rmPut;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If the edition is successful, the 204 status code is
      returned, if not, the code is 201. }
    function Execute: Boolean; override;
  end;

  { Destroy a specific resource. }
  TBrookDestroyAction = class(TBrookCustomCRUDAction)
  protected
    procedure InternalOpen; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalApply({%H-}const AUpdating: Boolean); override;
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmDelete;
      const ADefault: Boolean = False); overload;
    { Registers an action linking the request to a database table and defining
      the fields that will be ignored for persistance purposes. }
    class procedure Register(const ATableName, APattern, AIgnoredFields: string;
      const AMethod: TBrookRequestMethod = rmDelete;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If the deletion is successful, the 204 status code is
      returned, if not, the code is 404. }
    function Execute: Boolean; override;
  end;

implementation

{ TBrookCustomCRUDAction }

constructor TBrookCustomCRUDAction.Create;
begin
  inherited Create;
  FAutoApply := True;
end;

{ TBrookOptionsAction }

procedure TBrookOptionsAction.InternalOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
  Table.Open;
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TBrookOptionsAction.InternalInsert;
begin
end;

procedure TBrookOptionsAction.InternalEdit;
begin
end;

procedure TBrookOptionsAction.InternalDelete;
begin
end;

procedure TBrookOptionsAction.InternalApply(const AUpdating: Boolean);
begin
end;

class procedure TBrookOptionsAction.Register(const ATableName,
  APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

class procedure TBrookOptionsAction.Register(const ATableName, APattern,
  AIgnoredFields: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AIgnoredFields, AMethod, ADefault);
end;

procedure TBrookOptionsAction.Request(ARequest: TRequest; AResponse: TResponse);
begin
  if Execute then
    Write(Table.GetSchema)
  else
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
  end;
end;

function TBrookOptionsAction.Execute: Boolean;
begin
  InternalOpen;
  Result := Table.FieldDefs.Count > 0;
end;

{ TBrookRetrieveAction }

procedure TBrookRetrieveAction.InternalOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
  Table.Open;
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TBrookRetrieveAction.InternalInsert;
begin
end;

procedure TBrookRetrieveAction.InternalEdit;
begin
end;

procedure TBrookRetrieveAction.InternalDelete;
begin
end;

procedure TBrookRetrieveAction.InternalApply(const AUpdating: Boolean);
begin
end;

class procedure TBrookRetrieveAction.Register(const ATableName,
  APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

class procedure TBrookRetrieveAction.Register(const ATableName, APattern,
  AIgnoredFields: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AIgnoredFields, AMethod, ADefault);
end;

procedure TBrookRetrieveAction.Request(ARequest: TRequest;
  AResponse: TResponse);
begin
  if Execute then
    Write(Table.AsJSON)
  else
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
  end;
end;

function TBrookRetrieveAction.Execute: Boolean;
begin
  if Values.Count > 0 then
    Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values);
  InternalOpen;
  Result := not Table.Empty;
end;

{ TBrookShowAction }

procedure TBrookShowAction.InternalOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
  Table.Open;
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TBrookShowAction.InternalInsert;
begin
end;

procedure TBrookShowAction.InternalEdit;
begin
end;

procedure TBrookShowAction.InternalDelete;
begin
end;

procedure TBrookShowAction.InternalApply(const AUpdating: Boolean);
begin
end;

class procedure TBrookShowAction.Register(const ATableName, APattern: string;
  const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

class procedure TBrookShowAction.Register(const ATableName, APattern,
  AIgnoredFields: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AIgnoredFields, AMethod, ADefault);
end;

procedure TBrookShowAction.Request(ARequest: TRequest;
  AResponse: TResponse);
var
  VRow: TJSONObject;
begin
  if Execute then
  begin
    Table.GetRow(VRow);
    try
      Write(VRow.AsJSON);
    finally
      FreeAndNil(VRow);
    end;
  end
  else
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
  end;
end;

function TBrookShowAction.Execute: Boolean;
var
  VCount: Integer;
begin
  VCount := Values.Count;
  Result := VCount > 0;
  if not Result then
    Exit;
  if VCount > 1 then
    Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values);
  InternalOpen;
  Result := Table.Locate(Values);
end;

{ TBrookCreateAction }

procedure TBrookCreateAction.InternalOpen;
begin
end;

procedure TBrookCreateAction.InternalInsert;
begin
  if Assigned(FBeforeInsert) then
    FBeforeInsert(Self);
  Table.Insert(Fields);
  if Assigned(FAfterInsert) then
    FAfterInsert(Self);
end;

procedure TBrookCreateAction.InternalEdit;
begin
end;

procedure TBrookCreateAction.InternalDelete;
begin
end;

procedure TBrookCreateAction.InternalApply(const AUpdating: Boolean);
begin
  if Assigned(FBeforeApply) then
    FBeforeApply(Self);
  Table.Apply;
  if Assigned(FAfterApply) then
    FAfterApply(Self);
end;

class procedure TBrookCreateAction.Register(const ATableName, APattern: string;
  const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

class procedure TBrookCreateAction.Register(const ATableName, APattern,
  AIgnoredFields: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AIgnoredFields, AMethod, ADefault);
end;

procedure TBrookCreateAction.Request(ARequest: TRequest;
  AResponse: TResponse);
begin
  if Execute then
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_CREATED;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_CREATED;
  end
  else
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_NO_CONTENT;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NO_CONTENT;
  end;
end;

function TBrookCreateAction.Execute: Boolean;
begin
  BrookJSONCopy(Values, Fields);
  InternalInsert;
  if FAutoApply then
    InternalApply(True);
  Result := Fields.Count > 0;
end;

{ TBrookUpdateAction }

procedure TBrookUpdateAction.InternalOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
  Table.Open;
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TBrookUpdateAction.InternalInsert;
begin
  if Assigned(FBeforeInsert) then
    FBeforeInsert(Self);
  Table.Insert(Fields);
  if Assigned(FAfterInsert) then
    FAfterInsert(Self);
end;

procedure TBrookUpdateAction.InternalEdit;
begin
  if Assigned(FBeforeEdit) then
    FBeforeEdit(Self);
  Table.Edit(Fields);
  if Assigned(FAfterEdit) then
    FAfterEdit(Self);
end;

procedure TBrookUpdateAction.InternalDelete;
begin
end;

procedure TBrookUpdateAction.InternalApply(const AUpdating: Boolean);
begin
  if Assigned(FBeforeApply) then
    FBeforeApply(Self);
  Table.Apply;
  if Assigned(FAfterApply) then
    FAfterApply(Self);
end;

class procedure TBrookUpdateAction.Register(const ATableName, APattern: string;
  const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

class procedure TBrookUpdateAction.Register(const ATableName, APattern,
  AIgnoredFields: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AIgnoredFields, AMethod, ADefault);
end;

procedure TBrookUpdateAction.Request(ARequest: TRequest;
  AResponse: TResponse);
begin
  if Execute then
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_NO_CONTENT;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NO_CONTENT;
  end
  else
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_CREATED;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_CREATED;
  end;
end;

function TBrookUpdateAction.Execute: Boolean;
var
  VCount: Integer;
begin
  VCount := Values.Count;
  if VCount = 1 then
  begin
    InternalOpen;
    Result := Table.Locate(Values)
  end
  else
  begin
    Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values);
    InternalOpen;
    Result := (VCount > 1) and Table.Locate(Values);
  end;
  BrookJSONCopy(Values, Fields);
  if Result then
  begin
    InternalEdit;
    if FAutoApply then
      InternalApply(True);
  end
  else
  begin
    InternalInsert;
    if FAutoApply then
      InternalApply(False);
  end;
end;

{ TBrookDestroyAction }

procedure TBrookDestroyAction.InternalOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
  Table.Open;
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TBrookDestroyAction.InternalInsert;
begin
end;

procedure TBrookDestroyAction.InternalEdit;
begin
end;

procedure TBrookDestroyAction.InternalDelete;
begin
  if Assigned(FBeforeDelete) then
    FBeforeDelete(Self);
  Table.Delete;
  if Assigned(FAfterDelete) then
    FAfterDelete(Self);
end;

procedure TBrookDestroyAction.InternalApply(const AUpdating: Boolean);
begin
  if Assigned(FBeforeApply) then
    FBeforeApply(Self);
  Table.Apply;
  if Assigned(FAfterApply) then
    FAfterApply(Self);
end;

class procedure TBrookDestroyAction.Register(const ATableName,
  APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

class procedure TBrookDestroyAction.Register(const ATableName, APattern,
  AIgnoredFields: string; const AMethod: TBrookRequestMethod;
  const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AIgnoredFields, AMethod, ADefault);
end;

procedure TBrookDestroyAction.Request(ARequest: TRequest;
  AResponse: TResponse);
begin
  if Execute then
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_NO_CONTENT;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NO_CONTENT;
  end
  else
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_NOT_FOUND;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_NOT_FOUND;
  end;
end;

function TBrookDestroyAction.Execute: Boolean;
var
  VCount: Integer;
begin
  VCount := Values.Count;
  if VCount = 1 then
  begin
    InternalOpen;
    Result := Table.Locate(Values)
  end
  else
  begin
    Table.CreateFields(Values).Conditions(Values).Prepare.Bind(Values);
    InternalOpen;
    Result := (VCount > 1) and Table.Locate(Values);
  end;
  if Result then
  begin
    InternalDelete;
    if FAutoApply then
      InternalApply(False);
  end;
end;

end.

