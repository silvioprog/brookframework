unit frmCustomEdit;

{$mode objfpc}{$H+}

interface

uses
  BrookHTTPClient, BrookFCLHTTPClientBroker, BrookHTTPUtils, BrookUtils, Forms,
  ExtCtrls, Buttons, SysUtils, Controls, Dialogs, Grids, LJGridUtils, FPJSON;

type
  TfrCustomEdit = class(TForm)
    btSubmit: TBitBtn;
    btCancel: TBitBtn;
    pnClient: TPanel;
    pnBottom: TPanel;
  protected
    class function ProcessRequest(const AHttpResult: TBrookHTTPResult): Boolean;
    class function FillPattern(const APattern: string;
      AParams: TJSONObject): string;
  public
    class procedure Refresh(AGrid: TCustomStringGrid; const AUrl: string);
    class procedure Refresh(AGrid: TCustomStringGrid; const APattern: string;
      const AArgs: TJSONObject);
    class function Add(const AUrl: string): Boolean;
    class function Add(const APattern: string;
      const AArgs: TJSONObject): Boolean;
    class function Edit(const APattern: string;
      const AArgs: TJSONObject): Boolean;
    class function Delete(const APattern, AMsg: string;
      const AArgs: TJSONObject): Boolean;
    class function Execute(var AData: TJSONObject): Boolean;
    class function Question(const ACaption, AMsg: string): Boolean;
  end;

implementation

{$R *.lfm}

uses
  Serializer;

class function TfrCustomEdit.ProcessRequest(
  const AHttpResult: TBrookHTTPResult): Boolean;
begin
  Result := (AHttpResult.StatusCode = 200) or (AHttpResult.StatusCode = 201) or
    (AHttpResult.StatusCode = 204) or (AHttpResult.StatusCode = 404);
  if not Result then
    ShowMessageFmt('ERROR: Text: %s; code: %d.',
      [AHttpResult.ReasonPhrase, AHttpResult.StatusCode]);
end;

class function TfrCustomEdit.FillPattern(const APattern: string;
  AParams: TJSONObject): string;
var
  I: Integer;
  VName, VValue: string;
begin
  Result := APattern;
  for I := 0 to Pred(AParams.Count) do
  begin
    VName := ':' + AParams.Names[I];
    VValue := AParams.Items[I].AsString;
    Result := StringReplace(Result, VName, VValue, [rfIgnoreCase]);
  end;
end;

class procedure TfrCustomEdit.Refresh(AGrid: TCustomStringGrid;
  const AUrl: string);
var
  VData: TJSONArray = nil;
begin
  ClearGrid(AGrid);
  ProcessRequest(BrookHttpRequest(AUrl, VData));
  LoadJSON(AGrid, VData, False, False);
  FreeAndNil(VData);
end;

class procedure TfrCustomEdit.Refresh(AGrid: TCustomStringGrid;
  const APattern: string; const AArgs: TJSONObject);
var
  VData: TJSONArray = nil;
begin
  ClearGrid(AGrid);
  if AArgs.Count = 0 then
    Exit;
  ProcessRequest(BrookHttpRequest(TfrCustomEdit.FillPattern(
    APattern, AArgs), VData));
  LoadJSON(AGrid, VData, False, False);
  FreeAndNil(VData);
end;

class function TfrCustomEdit.Add(const AUrl: string): Boolean;
var
  VData: TJSONObject = nil;
begin
  Result := Self.Execute(VData);
  if not Result then
    Exit;
  Result := ProcessRequest(BrookHttpRequest(VData, AUrl));
  if not Result then
    Exit;
  FreeAndNil(VData);
end;

class function TfrCustomEdit.Add(const APattern: string;
  const AArgs: TJSONObject): Boolean;
var
  VData: TJSONObject = nil;
begin
  Result := Self.Execute(VData);
  if not Result then
    Exit;
  Result := ProcessRequest(BrookHttpRequest(VData,
    TfrCustomEdit.FillPattern(APattern, AArgs)));
  if not Result then
    Exit;
  FreeAndNil(VData);
end;

class function TfrCustomEdit.Edit(const APattern: string;
  const AArgs: TJSONObject): Boolean;
var
  VRow: TJSONObject;
begin
  VRow := AArgs.Clone as TJSONObject;
  try
    Result := Self.Execute(VRow) and ProcessRequest(BrookHttpRequest(VRow,
      TfrCustomEdit.FillPattern(APattern, AArgs), rmPut));
  finally
    VRow.Free;
  end;
end;

class function TfrCustomEdit.Delete(const APattern, AMsg: string;
  const AArgs: TJSONObject): Boolean;
begin
  Result := Self.Question('Deleting', AMsg) and
    ProcessRequest(BrookHttpRequest(TfrCustomEdit.FillPattern(APattern, AArgs),
      rmDelete));
end;

class function TfrCustomEdit.Execute(var AData: TJSONObject): Boolean;
var
  VForm: TCustomForm;
begin
  VForm := Self.Create(nil);
  try
    if Assigned(AData) then
      Unserialize(AData, VForm);
    Result := VForm.ShowModal = mrOK;
    if not Result then
      Exit;
    if not Assigned(AData) then
      AData := TJSONObject.Create;
    Serialize(VForm, AData);
  finally
    VForm.Free;
  end;
end;

class function TfrCustomEdit.Question(const ACaption, AMsg: string): Boolean;
begin
  Result := MessageDlg(ACaption, AMsg, mtConfirmation, mbYesNo, 0) = mrYes;
end;

end.

