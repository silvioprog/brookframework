(*
  Brook REST Actions unit.

  Copyright (C) 2012 Silvio Clecio.

  http://brookframework.org

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
  { Displays the schema of the resource. }
  TBrookOptionsAction = class(TBrookDBAction)
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmOptions;
      const ADefault: Boolean = False); overload;
    { Is triggered by a OPTIONS HTTP request method. }
    procedure Options; override;
    (* Executes the action. If there are schema in the resource, they are
      returned, if not, it returns a @code('{ "error": "No schema." }') JSON. *)
    function Execute: Boolean; virtual;
  end;

  { Displays all the contents of the resource. }
  TBrookRetrieveAction = class(TBrookDBAction)
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmGet;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If there are contents in the resource, they are
      returned, if not, it returns a 404 status code. }
    function Execute: Boolean; virtual;
  end;

  { Displays the content of a specific resource. }
  TBrookShowAction = class(TBrookDBAction)
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmGet;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If there are contents in the resource, they are
      returned, if not, it returns a 404 status code. }
    function Execute: Boolean; virtual;
  end;

  { Creates a new resource. }
  TBrookCreateAction = class(TBrookDBAction)
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmPost;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. The 201 status code is always returned. }
    function Execute: Boolean; virtual;
  end;

  { Updates a specific resource. }
  TBrookUpdateAction = class(TBrookDBAction)
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmPut;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If the edition is successful, the 204 status code is
      returned, if not, the code is 201. }
    function Execute: Boolean; virtual;
  end;

  { Destroy a specific resource. }
  TBrookDestroyAction = class(TBrookDBAction)
  public
    { Registers an action linking the request to a database table. }
    class procedure Register(const ATableName, APattern: string;
      const AMethod: TBrookRequestMethod = rmDelete;
      const ADefault: Boolean = False); overload;
    { Is triggered by a request of any HTTP method. }
    procedure Request({%H-}ARequest: TRequest; AResponse: TResponse); override;
    { Executes the action. If the deletion is successful, the 204 status code is
      returned, if not, the code is 404. }
    function Execute: Boolean; virtual;
  end;

implementation

{ TBrookOptionsAction }

class procedure TBrookOptionsAction.Register(const ATableName,
  APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

procedure TBrookOptionsAction.Options;
begin
  if Execute then
    Write(Table.GetSchema)
  else
    Write('{ "error": "No schema." }');
end;

function TBrookOptionsAction.Execute: Boolean;
begin
  Result := not Table.Open.Empty;
end;

{ TBrookRetrieveAction }

class procedure TBrookRetrieveAction.Register(const ATableName,
  APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
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
    Result := not Table.CreateFields(Values).Conditions(
      Values).Prepare.Bind(Values).Open.Empty
  else
    Result := not Table.Open.Empty;
end;

{ TBrookShowAction }

class procedure TBrookShowAction.Register(const ATableName, APattern: string;
  const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
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
  if VCount = 1 then
    Result := Table.Open.Locate(Values)
  else
    Result := Table.CreateFields(Values).Conditions(
      Values).Prepare.Bind(Values).Open.Locate(Values);
end;

{ TBrookCreateAction }

class procedure TBrookCreateAction.Register(const ATableName, APattern: string;
  const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
end;

procedure TBrookCreateAction.Request(ARequest: TRequest;
  AResponse: TResponse);
begin
  if Execute then
  begin
    AResponse.Code := BROOK_HTTP_STATUS_CODE_CREATED;
    AResponse.CodeText := BROOK_HTTP_REASON_PHRASE_CREATED;
  end;
end;

function TBrookCreateAction.Execute: Boolean;
begin
  BrookJSONCopy(Values, Fields);
  Table.Insert(Fields).Apply;
  Result := True;
end;

{ TBrookUpdateAction }

class procedure TBrookUpdateAction.Register(const ATableName, APattern: string;
  const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
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
  VLocate: Boolean;
begin
  VCount := Values.Count;
  if VCount = 1 then
    VLocate := Table.Open.Locate(Values)
  else
    VLocate := (VCount > 1) and Table.CreateFields(Values).Conditions(
      Values).Prepare.Bind(Values).Open.Locate(Values);
  BrookJSONCopy(Values, Fields);
  if VLocate then
    Table.Edit(Fields).Apply
  else
    Table.Insert(Fields).Apply;
  Result := True;
end;

{ TBrookDestroyAction }

class procedure TBrookDestroyAction.Register(const ATableName,
  APattern: string; const AMethod: TBrookRequestMethod; const ADefault: Boolean);
begin
  inherited Register(ATableName, APattern, AMethod, ADefault);
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
    Result := Table.Open.Locate(Values)
  else
    Result := (VCount > 1) and Table.CreateFields(Values).Conditions(
      Values).Prepare.Bind(Values).Open.Locate(Values);
  if Result then
    Table.Delete.Apply;
end;

end.

